(ns kosatsu.methods.anchor
  "錠 — the durability plane. Makes 高札's append-only record externally checkable.

  ## What this adds that the log alone does not

  `kosatsu.methods.kotoba` already gives a content-addressed commit DAG: each
  transaction's CID covers its datoms and its predecessor, so the chain detects
  tampering *by anyone holding the log*. What it cannot do is prove to a third
  party that a given head existed at a given time. Someone who controls the file
  can rewrite the whole chain from genesis and hand out a consistent lie.

  That gap matters more here than in most records. Gate G4 says a delisting is a
  NEW datom carrying `:lifted-at` and that nothing is overwritten. Today that is
  a promise. An anchored checkpoint turns it into something an outsider can
  falsify: if the record ever moves under a previously anchored head, the
  commitment no longer opens.

  ## What it does NOT do

  It does not reimplement anchoring. `kotoba-lang/kotobase-anchor-fevm` already
  owns chain-neutral checkpoint receipts, and its default provider is a signed
  append-only transparency log needing no chain, no gas and no RPC. A second
  anchor format would be a second answer to the question the anchor plane exists
  to answer — the same reason 高札 does not author designations of its own.

  It also does not publish anything. Every function here returns a PLAN. The
  network adapter that consumes the effect lives outside this namespace and
  outside this repository, and G8 still governs whether it may run.

  ## The disclosure boundary

  A checkpoint carries `{database-id, epoch, logical-checkpoint-root}` and
  nothing else. The root is a CID — opaque. No subject label, no authority id,
  no datom crosses it. This is not incidental: G5 (subject-dignity / no-doxxing)
  would be violated by an anchor that leaked who is designated, and an anchor is
  the one artefact here designed to be permanent and public."
  (:require [clojure.string :as str]
            [kosatsu.methods.kotoba :as k]
            [kotobase.anchor.checkpoint :as cp]))

(def database-id-prefix "kosatsu")

;; ── chain verification ────────────────────────────────────────────────────────

(defn verify-chain
  "Recompute the commit DAG from the datoms and report what was checked.

      {:ok? bool :checked n :broken-at i|nil :reason kw|nil}

  An EMPTY log is `{:ok? false :checked 0 :reason :empty-log}`, not intact. This
  is the distinction the workspace keeps paying for: a verifier that returns the
  same value for `checked nothing` and `checked everything and it was fine`
  reports a pass it never measured. Anchoring an unverified chain would notarise
  whatever the file happened to contain."
  [txs]
  (if (empty? txs)
    {:ok? false :checked 0 :broken-at nil :reason :empty-log}
    (loop [i 0 prev ""]
      (if (>= i (count txs))
        {:ok? true :checked (count txs) :broken-at nil :reason nil}
        (let [tx     (nth txs i)
              datoms (get tx ":tx/datoms")
              stated (get tx ":tx/prev")
              cid    (get tx ":tx/cid")]
          (cond
            (not= prev stated)
            {:ok? false :checked i :broken-at i :reason :prev-mismatch}

            (not= cid (k/tx-cid datoms stated))
            {:ok? false :checked i :broken-at i :reason :cid-mismatch}

            :else (recur (inc i) cid)))))))

(defn head
  "The CID this checkpoint would commit to, or nil when there is nothing to
  commit. nil rather than \"\" so a caller cannot pass an empty root into a
  payload and have it look like a value."
  [txs]
  (when (seq txs) (get (last txs) ":tx/cid")))

;; ── G10: sourcing honesty survives into the anchor ────────────────────────────

(def ^:private representative-values #{":representative" :representative})
(def ^:private authoritative-values #{":authoritative" :authoritative})

(defn sourcing-modes
  "Which sourcing modes appear across every datom in the log.

  G10 requires `:representative` vs `:authoritative` to be declared on every
  datom. That declaration has to survive into the anchor, because from outside a
  commitment over synthetic seed data and one over real designations are the
  same 32 bytes. `#{}` means the log declared neither, which is its own answer
  and is treated as unknown rather than as authoritative."
  [txs]
  (into #{}
        (comp (mapcat #(get % ":tx/datoms"))
              (keep (fn [d]
                      (let [a (str (nth d 2 "")) v (nth d 3 nil)]
                        (when (str/includes? a "sourcing")
                          (cond (representative-values v) :representative
                                (authoritative-values v)  :authoritative
                                :else                     :unknown))))))
        txs))

(defn database-id
  "`kosatsu` plus the sourcing mode, so the mode is inside the commitment rather
  than alongside it. A receipt that has been separated from its context still
  says which kind of record it covers."
  [txs]
  (let [modes (sourcing-modes txs)]
    (str database-id-prefix "/"
         (cond
           (empty? modes)               "unsourced"
           (= #{:representative} modes)  "representative"
           (= #{:authoritative} modes)   "authoritative"
           :else                         "mixed"))))

;; ── checkpoint + plan ─────────────────────────────────────────────────────────

(defn checkpoint
  "Build the checkpoint value for a verified log. Throws on an unverified chain.

  Throwing rather than returning nil: a caller that ignores a nil here anchors
  nothing and reports success, which is the failure this whole namespace exists
  to make impossible."
  [txs]
  (let [v (verify-chain txs)]
    (when-not (:ok? v)
      (throw (ex-info "refusing to checkpoint an unverified chain"
                      {:kosatsu.anchor/refusal (:reason v)
                       :verification v})))
    {:database-id             (database-id txs)
     :epoch                   (count txs)
     :logical-checkpoint-root (head txs)}))

(defn plan
  "A submission plan for `kotobase.anchor.checkpoint`.

  `provider` is REQUIRED and has no default. `:transparency` is the chainless
  signed log; `:evm` notarises to a public chain. A defaulted destination is a
  destination nobody chose, and one of these two is irreversible.

  **G10-anchor.** `:evm` is refused for a log that is not wholly
  `:authoritative`. A public-chain artefact is permanent and carries no room to
  say `this was synthetic` — notarising the representative seed there would
  leave a durable, externally-cited record implying 高札 had captured real
  designations. The transparency log is fine for either, because it is ours to
  annotate and to retire."
  [txs {:keys [provider digest-fn network contract-address]}]
  (when-not (contains? cp/providers provider)
    (throw (ex-info "anchor provider must be named explicitly"
                    {:kosatsu.anchor/refusal :provider-not-chosen
                     :known cp/providers :got provider})))
  (let [ck    (checkpoint txs)
        modes (sourcing-modes txs)]
    (when (and (= :evm provider) (not= #{:authoritative} modes))
      (throw (ex-info "refusing to notarise non-authoritative records to a public chain"
                      {:kosatsu.anchor/refusal :representative-to-public-chain
                       :sourcing-modes modes
                       :database-id (:database-id ck)})))
    (cp/submission-plan ck (cond-> {:provider provider :digest-fn digest-fn}
                             (= :evm provider)
                             (assoc :network network :contract-address contract-address)))))

(defn discloses-nothing?
  "Does this plan carry any datom content?

  Checked rather than asserted. The payload shape is owned upstream, so a change
  there could start carrying more than a root without anything here noticing —
  and the thing that would leak is exactly what G5 forbids. `terms` are the
  strings that must not appear: subject labels, authority ids, program names."
  [plan terms]
  (let [s (pr-str (:effect plan))]
    (not-any? #(str/includes? s (str %)) terms)))

;; ── clj-only: read the log ────────────────────────────────────────────────────

#?(:clj
   (defn plan-from-log
     "Read the append-only log at `log-path` and plan a checkpoint over it."
     [log-path opts]
     (plan (k/read-log log-path) opts)))

#?(:clj
   (defn verify-log
     "Verify the chain on disk without planning anything. The read-only entry
     point — use it in a gate."
     [log-path]
     (verify-chain (k/read-log log-path))))
