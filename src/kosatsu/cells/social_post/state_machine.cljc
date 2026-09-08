(ns kosatsu.cells.social-post.state-machine
  "Phase state machine for the 高札 (kosatsu) social_post cell — the G2/G7/G8/G9 publication membrane.
  1:1 port of src/kosatsu/cells/social_post/state_machine.cljc (ADR-2606072000).

  A divergence finding enters; it is DRAFTED into a dry-run post ONLY if:
    G3 — ≥2 primary-source citations are present;
    G9 — the post is a mirror (isMirror), opening with the competing-claim disclaimer;
    G7 — server-held-key is false (the member signs, the server never does);
    G8 — the status is dry-run (a 'published' request REFUSES — live needs Council Lv6+ + operator);
    G2 — non-adjudicating (the body reports who-designated-whom + disagreement, never a verdict).
  Self-contained. Conventions: dataclass PostState → a plain map with the SAME string field keys the
  Python `cs.__dict__` round-trips; phase enum value identities stay strings."
  (:require [kotoba.lang.text :as str]))

(def disclaimer "[mirror · competing-claim — non-adjudicating; a designation is asserter-relative]")

;; ── PostPhase (enum — Python value identities preserved) ──
(def post-phases {:init "init" :drafted "drafted" :refused "refused"})
(def phase-init    (:init post-phases))
(def phase-drafted (:drafted post-phases))
(def phase-refused (:refused post-phases))

;; ── PostState (dataclass → plain map, string keys + field defaults) ──
(def state-defaults
  {"phase"            phase-init
   "subject"          ""
   "sources"          []
   "requested_status" "dry-run"
   "server_held_key"  false
   "payload"          {}
   "refusal"          ""})

(defn- cell-state [state]
  (merge state-defaults (get state "cell_state" {})))

(defn- lstrip-colon [s] (str/replace (str s) #"^:+" ""))

(defn transition-to-drafted [state]
  (let [cs (cell-state state)
        cs (assoc cs
                  "subject" (get state "subject" (get cs "subject"))
                  "sources" (get state "sources" (get cs "sources"))
                  "requested_status" (lstrip-colon (get state "requested_status" (get cs "requested_status")))
                  "server_held_key" (boolean (get state "server_held_key" (get cs "server_held_key"))))
        refuse (fn [msg] {"cell_state" (assoc cs "refusal" msg "phase" phase-refused)})]
    (cond
      (< (count (get cs "sources")) 2)
      (refuse "G3: a post needs ≥2 primary-source citations")

      (get cs "server_held_key")
      (refuse "G7/no-server-key: server-held-key must be false (ADR-2605231525)")

      (not= (get cs "requested_status") "dry-run")
      (refuse "G8: only dry-run posts at R0; live publication is Council Lv6+ + operator gated")

      :else
      (let [payload {":post/subject" (get cs "subject")
                     ":post/body" (str disclaimer " " (get cs "subject"))
                     ":post/status" ":dry-run"
                     ":post/is-mirror" true
                     ":post/non-adjudicating-notice" true
                     ":post/server-held-key" false
                     ":post/sources" (get cs "sources")}]
        {"cell_state" (assoc cs "payload" payload "refusal" "" "phase" phase-drafted)}))))

(defn solve
  "R0 scaffold: social_post drafts dry-run only; live publication is Council Lv6+ + operator +
  member-sig gated (G8)."
  [_input-state]
  (throw (ex-info "kosatsu R0 scaffold: social_post drafts dry-run only; live publication is Council Lv6+ + operator + member-sig gated (G8)."
                  {:gate "G8"})))
