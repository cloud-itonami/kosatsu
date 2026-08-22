(ns kosatsu.methods.test-anchor
  "Every refusal in kosatsu.methods.anchor is exercised in BOTH directions: a
  case that passes it, and one thing broken so that the NAMED refusal fires.
  A guard that has only been seen to allow is not evidence that it guards."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [kosatsu.methods.anchor :as anchor]
            [kosatsu.methods.edn :as edn]
            [kosatsu.methods.kotoba :as kotoba]
            [kosatsu.methods.weave :as weave]))

(defn- seed-path [] "data/seed-designation-graph.kotoba.edn")

(defn- digest [s] (str "d" (kotoba/*sha256-hex* (str s))))

(defn- seed-datoms []
  (kotoba/graph-datoms (weave/weave (edn/load-edn (seed-path)))))

(defn- chain-of
  "Build a real n-transaction chain over `ds`."
  [ds n]
  (loop [i 0 prev "" out []]
    (if (= i n)
      out
      (let [tx (kotoba/make-tx ds :tx-id (inc i) :as-of (+ 20260822 i) :prev-cid prev)]
        (recur (inc i) (get tx ":tx/cid") (conj out tx))))))

(defn- chain [n] (chain-of (seed-datoms) n))

(defn- authoritative
  "The same chain with every sourcing datom flipped to :authoritative, so the
  public-chain guard can be tested from the allowing side too. Rebuilt through
  make-tx rather than edited in place — an edited tx would fail verification for
  the wrong reason and the test would pass while proving nothing."
  [n]
  (chain-of (mapv (fn [d]
                    (if (str/includes? (str (nth d 2 "")) "sourcing")
                      (assoc d 3 ":authoritative")
                      d))
                  (seed-datoms))
            n))

;; ── chain verification ────────────────────────────────────────────────────────

(deftest test-verify-chain-accepts-a-real-chain
  (testing "a chain built by make-tx verifies, or every negative below proves nothing"
    (let [v (anchor/verify-chain (chain 3))]
      (is (:ok? v))
      (is (= 3 (:checked v)))
      (is (nil? (:broken-at v))))))

(deftest test-empty-log-is-not-intact
  (testing "checked-zero must not read as verified-intact"
    (let [v (anchor/verify-chain [])]
      (is (false? (:ok? v)))
      (is (= 0 (:checked v)))
      (is (= :empty-log (:reason v))))))

(deftest test-a-tampered-datom-is-caught-at-its-own-index
  (let [c   (chain 3)
        bad (assoc-in (vec c) [1 ":tx/datoms" 0 3] "tampered")
        v   (anchor/verify-chain bad)]
    (is (false? (:ok? v)))
    (is (= :cid-mismatch (:reason v)))
    (is (= 1 (:broken-at v)) "the break is reported where it is, not where it was noticed")))

(deftest test-a-rewritten-history-breaks-the-prev-linkage
  (testing "dropping a transaction from the middle is what a quiet rewrite looks like"
    (let [c   (chain 3)
          bad [(nth c 0) (nth c 2)]
          v   (anchor/verify-chain bad)]
      (is (false? (:ok? v)))
      (is (= :prev-mismatch (:reason v)))
      (is (= 1 (:broken-at v))))))

(deftest test-head-is-nil-not-empty-string
  (is (nil? (anchor/head [])))
  (is (= (get (last (chain 2)) ":tx/cid") (anchor/head (chain 2)))))

;; ── G10 sourcing honesty ──────────────────────────────────────────────────────

(deftest test-sourcing-modes-reads-the-seed-as-representative
  (is (= #{:representative} (anchor/sourcing-modes (chain 1))))
  (is (= #{:authoritative} (anchor/sourcing-modes (authoritative 1))))
  (testing "a log that declares no sourcing at all is empty, not authoritative"
    (let [tx (kotoba/make-tx [[":db/add" "e" ":authority/label" "x"]] :tx-id 1 :as-of 1 :prev-cid "")]
      (is (= #{} (anchor/sourcing-modes [tx]))))))

(deftest test-the-sourcing-mode-is-inside-the-database-id
  (testing "a receipt separated from its context still says what kind of record it covers"
    (is (= "kosatsu/representative" (anchor/database-id (chain 1))))
    (is (= "kosatsu/authoritative" (anchor/database-id (authoritative 1))))
    (let [tx (kotoba/make-tx [[":db/add" "e" ":authority/label" "x"]] :tx-id 1 :as-of 1 :prev-cid "")]
      (is (= "kosatsu/unsourced" (anchor/database-id [tx]))))))

;; ── checkpoint ────────────────────────────────────────────────────────────────

(deftest test-checkpoint-refuses-an-unverified-chain
  (let [bad (assoc-in (vec (chain 2)) [0 ":tx/datoms" 0 3] "tampered")]
    (is (thrown? #?(:clj Exception :cljs js/Error) (anchor/checkpoint bad)))
    (is (thrown? #?(:clj Exception :cljs js/Error) (anchor/checkpoint [])))))

(deftest test-checkpoint-commits-to-the-head
  (let [c  (chain 4)
        ck (anchor/checkpoint c)]
    (is (= 4 (:epoch ck)))
    (is (= (anchor/head c) (:logical-checkpoint-root ck)))
    (is (= "kosatsu/representative" (:database-id ck)))))

;; ── plan ──────────────────────────────────────────────────────────────────────

(deftest test-the-provider-must-be-chosen
  (testing "a defaulted destination is a destination nobody chose"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (anchor/plan (chain 1) {:digest-fn digest})))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (anchor/plan (chain 1) {:provider :carrier-pigeon :digest-fn digest})))))

(deftest test-the-chainless-transparency-plan-is-produced
  (let [p (anchor/plan (chain 2) {:provider :transparency :digest-fn digest})]
    (is (= :checkpoint/append (get-in p [:effect :effect/type])))
    (is (= :transparency (get-in p [:effect :provider])))
    (is (string? (get-in p [:effect :idempotency-key])))
    (is (= :pending (get-in p [:state :status])))))

(deftest test-representative-records-are-refused-a-public-chain
  (testing "a public-chain artefact is permanent and cannot say 'this was synthetic'"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (anchor/plan (chain 1) {:provider :evm :digest-fn digest
                                         :network "calibration"
                                         :contract-address "0xabc"}))))
  (testing "and the SAME call is allowed once the record is authoritative"
    (let [p (anchor/plan (authoritative 1) {:provider :evm :digest-fn digest
                                            :network "calibration"
                                            :contract-address "0xabc"})]
      (is (= :evm/submit-checkpoint (get-in p [:effect :effect/type])))
      (is (= "calibration" (get-in p [:effect :network]))))))

(deftest test-the-idempotency-key-follows-the-record
  (let [opts {:provider :transparency :digest-fn digest}
        a (anchor/plan (chain 2) opts)
        b (anchor/plan (chain 2) opts)
        c (anchor/plan (chain 3) opts)]
    (is (= (get-in a [:effect :idempotency-key]) (get-in b [:effect :idempotency-key]))
        "re-planning the same log must not produce a second submission")
    (is (not= (get-in a [:effect :idempotency-key]) (get-in c [:effect :idempotency-key]))
        "a grown log is a different checkpoint")))

;; ── the disclosure boundary ───────────────────────────────────────────────────

(deftest test-no-record-content-crosses-into-the-plan
  (testing "G5 — the anchor is the one artefact designed to be permanent and public"
    (let [p (anchor/plan (chain 1) {:provider :transparency :digest-fn digest})
          leaked ["US Treasury OFAC" "us-ofac" "designated entity" "subj-alpha"
                  "vessel V-1" "eu-council" "un-sc"]]
      (is (anchor/discloses-nothing? p leaked)
          (str "the effect payload leaked record content: " (pr-str (:effect p)))))))

(deftest test-the-disclosure-check-can-fail
  (testing "a checker that has never failed is not a checker"
    (let [p (anchor/plan (chain 1) {:provider :transparency :digest-fn digest})]
      (is (false? (anchor/discloses-nothing? p ["kosatsu"]))
          "database-id is deliberately in the payload, so this term must be found"))))

;; ── the whole point ───────────────────────────────────────────────────────────

(deftest test-an-anchored-head-detects-a-later-rewrite
  (testing "G4 says nothing is overwritten. This is what makes that falsifiable."
    (let [anchored  (anchor/checkpoint (chain 3))
          rewritten (loop [i 0 prev "" out []]
                      (if (= i 3)
                        out
                        (let [ds (if (= i 1)
                                   [[":db/add" "us-ofac" ":authority/label" "rewritten"]]
                                   (seed-datoms))
                              tx (kotoba/make-tx ds :tx-id (inc i) :as-of 20260822 :prev-cid prev)]
                          (recur (inc i) (get tx ":tx/cid") (conj out tx)))))]
      (is (:ok? (anchor/verify-chain rewritten))
          "the rewritten chain is internally consistent — that is the attack")
      (is (not= (:logical-checkpoint-root anchored)
                (:logical-checkpoint-root (anchor/checkpoint rewritten)))
          "but it cannot reach the anchored head"))))
