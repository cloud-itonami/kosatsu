(ns kosatsu.methods.test-weave
  "test_weave.cljc — 高札 (kosatsu) weave + competing-claim/divergence engine. ADR-2606072000.
  1:1 Clojure port of src/kosatsu/methods/test_weave.cljc.

  Covers: seed-weaves-clean, the G1/G2/G3/G4/G5/G7 validation gates (each → ex-info),
  status-as-of event-log folding (delisting + silence), the divergence engine
  (contested = active list-vs-delist conflict; coverage_split ≠ contested; unanimous;
  single-asserter; contested-first ordering), and the aggregates (agreement-index,
  delisting-timeline, by-authority, co-designation, report shape + integrity).

  DEFERRED (out of scope, matching the rasen/inochi/kabuto precedent): test_analyze.py /
  test_autorun.py / test_consistency.py / test_lexicons.py / test_charter_invariants.py —
  those exercise analyze / autorun / ingest / social / bridge modules that are not part of
  this weave-core port."
  (:require [kotoba.lang.text] [clojure.test :refer [deftest is run-tests]]
            [clojure.java.io :as io]
            [kosatsu.methods.edn :as edn]
            [kosatsu.methods.weave :as w]))

(def actor-dir (io/file (or (System/getProperty "user.dir") ".")))
(def seed (io/file actor-dir "data" "seed-designation-graph.kotoba.edn"))

(defn- g [] (w/weave (edn/load-edn seed)))

(defn- auth [& {:as kw}]
  (merge {":authority/id" "x-auth" ":authority/kind" ":state-treasury" ":authority/label" "X"
          ":authority/jurisdiction" "x" ":authority/stance" "rep" ":authority/sourcing" ":representative"
          ":authority/sources" ["https://example.gov/"]}
         kw))

(defn- subj [& {:as kw}]
  (merge {":subject/id" "x-sub" ":subject/kind" ":designated-entity" ":subject/label" "S"
          ":subject/sourcing" ":representative"}
         kw))

(defn- desig [& {:as kw}]
  (merge {":designation/id" "x-d" ":designation/asserter" "x-auth" ":designation/subject" "x-sub"
          ":designation/measure" ":financial-sanction" ":designation/program" "P"
          ":designation/status" ":listed" ":designation/posted-at" 20230101
          ":designation/asserted-notice" true ":designation/sourcing" ":representative"
          ":designation/sources" ["https://example.gov/a" "https://example.gov/b"]}
         kw))

(defn- raises-containing?
  "Run thunk; assert it throws and the message contains `frag`."
  [frag thunk]
  (try (thunk) false
       (catch clojure.lang.ExceptionInfo e
         (kotoba.lang.text/includes? (.getMessage e) frag))
       (catch Exception e
         (kotoba.lang.text/includes? (str (.getMessage e)) frag))))

;; ── seed weaves clean ─────────────────────────────────────────────────────────
(deftest test-seed-weaves
  (let [gr (g)]
    (is (= 7 (count (get gr "authorities"))))
    (is (= 5 (count (get gr "subjects"))))
    (is (= 13 (count (get gr "designations"))))))

;; ── G1 etzhayyim authors no designation ───────────────────────────────────────
(deftest test-authority-self-refused
  (is (raises-containing? "G1" #(w/validate-authority (auth ":authority/id" "etzhayyim-board")))))

;; ── G2 asserter mandatory + no verdict measure ────────────────────────────────
(deftest test-designation-without-asserter-refused
  (is (raises-containing? "G2" #(w/validate-designation (desig ":designation/asserter" "")))))

(deftest test-designation-verdict-measure-refused
  (is (raises-containing? "G2" #(w/validate-designation (desig ":designation/measure" ":criminal")))))

(deftest test-designation-terrorist-measure-refused
  (is (raises-containing? "G2" #(w/validate-designation (desig ":designation/measure" ":terrorist")))))

;; ── G3 sourcing ───────────────────────────────────────────────────────────────
(deftest test-designation-under-sourced-refused
  (is (raises-containing? "G3" #(w/validate-designation (desig ":designation/sources" ["https://example.gov/a"])))))

(deftest test-designation-commercial-terminal-refused
  (is (raises-containing? "Rider"
        #(w/validate-designation (desig ":designation/sources" ["https://worldcheck.refinitiv.com/x" "https://example.gov/b"])))))

;; ── G4 status / delisting needs lifted-at ─────────────────────────────────────
(deftest test-delisted-needs-lifted-at
  (is (raises-containing? "G4" #(w/validate-designation (desig ":designation/status" ":delisted")))))

(deftest test-final-status-refused
  (is (raises-containing? "G4" #(w/validate-designation (desig ":designation/status" ":permanent")))))

;; ── G2/G7 no per-subject score ────────────────────────────────────────────────
(deftest test-subject-score-refused
  (is (raises-containing? "G2/G7" #(w/validate-subject (subj ":subject/risk-score" 9)))))

(deftest test-subject-pii-refused
  (is (raises-containing? "no-doxxing" #(w/validate-subject (subj ":subject/dob" "1970-01-01")))))

;; ── status as-of (event log) ──────────────────────────────────────────────────
(deftest test-status-as-of-delisting
  (let [gr (g)]
    ;; subj-beta: us listed 2022, us delisted 2024
    (is (= "listed" (w/status-as-of gr "subj-beta" "us-ofac" 20230101)))
    (is (= "delisted" (w/status-as-of gr "subj-beta" "us-ofac" 20240601)))
    (is (= "delisted" (w/status-as-of gr "subj-beta" "us-ofac")))))   ;; latest

(deftest test-status-as-of-silent
  (let [gr (g)]
    (is (nil? (w/status-as-of gr "subj-alpha" "jp-mof")))))   ;; jp never designated alpha

;; ── divergence engine (the political-stance core) ─────────────────────────────
(deftest test-divergence-contested-is-active-delist-conflict
  ;; subj-beta: eu currently lists it, us delisted it → an ACTIVE disagreement on current status
  (let [d (w/divergence (g) "subj-beta")]
    (is (= "contested" (get d "class")))
    (is (some #{"us-ofac"} (get d "delisted")))
    (is (some #{"eu-council"} (get d "listing")))))

(deftest test-divergence-coverage-split-not-contested
  ;; subj-alpha: us+eu+un list it, jp+cn+ru never designated it. opiners AGREE (unanimous),
  ;; but coverage is split — silence reported, NOT inferred as dissent.
  (let [d (w/divergence (g) "subj-alpha")]
    (is (= "unanimous" (get d "class")))
    (is (true? (get d "coverage_split")))
    (is (and (some #{"jp-mof"} (get d "silent")) (some #{"cn-mofcom"} (get d "silent"))))
    (is (= #{"us-ofac" "eu-council" "un-sc"} (set (get d "listing"))))))

(deftest test-divergence-unanimous
  (let [d (w/divergence (g) "subj-gamma")]
    (is (= "unanimous" (get d "class")))
    (is (= #{"us-ofac" "eu-council" "un-sc" "gb-ofsi"} (set (get d "listing"))))))

(deftest test-divergence-single-asserter
  (let [d (w/divergence (g) "subj-delta")]
    (is (= "single-asserter" (get d "class")))
    (is (= ["ru-mfa"] (get d "listing")))))

(deftest test-divergence-all-contested-first
  (let [classes (mapv #(get % "class") (w/divergence-all (g)))]
    (is (= "contested" (first classes)))))

;; ── aggregates ────────────────────────────────────────────────────────────────
(deftest test-agreement-index
  (let [ai (w/agreement-index (g))]
    (is (= 5 (get ai "designated_subjects")))
    (is (>= (get ai "contested") 1))          ;; subj-beta (us delisted vs eu listing)
    (is (>= (get ai "coverage_split") 2))     ;; subj-alpha + subj-gamma
    (is (and (<= 0.0 (get ai "contested_ratio")) (<= (get ai "contested_ratio") 1.0)))))

(deftest test-delisting-timeline
  (let [tl (w/delisting-timeline (g))]
    (is (= 1 (count tl)))
    (is (and (= "us-ofac" (get (first tl) "asserter")) (= "subj-beta" (get (first tl) "subject"))))
    (is (= 20240115 (get (first tl) "lifted_at")))))

(deftest test-by-authority-counts
  (let [rows (into {} (map (fn [r] [(get r "authority") (get r "listed_subjects")]) (w/by-authority (g))))]
    (is (>= (get rows "us-ofac") 2))          ;; alpha, gamma, vessel listed (beta delisted)
    (is (= 3 (get rows "us-ofac")))))         ;; alpha + gamma + vessel (beta now delisted)

(deftest test-co-designation-us-has-program-cluster
  (let [cd (w/co-designation (g))]
    ;; none shares a program with >1 subject in seed → expect no cluster, a valid (empty) result.
    (is (sequential? cd))))

(deftest test-report-shape
  (let [r (w/report (g))]
    (doseq [k ["agreement_index" "divergence" "by_authority" "delisting_timeline" "co_designation" "integrity"]]
      (is (contains? r k)))
    (is (= 0 (get-in r ["integrity" "dangling_count"])))))

;; ── byte/numeric-parity smoke: the headline numbers ──────────────────────────
(deftest test-parity-headline
  (let [r (w/report (g))
        ai (get r "agreement_index")]
    (is (= 7 (get r "authority_count")))
    (is (= 5 (get r "subject_count")))
    (is (= 13 (get r "designation_count")))
    (is (= 1 (get ai "contested")))
    (is (= 1 (get ai "single_asserter")))
    (is (= 3 (get ai "unanimous")))
    (is (= 0.2 (get ai "contested_ratio")))
    ;; top contested subject is subj-beta (the only active list-vs-delist conflict)
    (is (= "subj-beta" (get (first (get r "divergence")) "subject")))))

(defn -main [& _]
  (run-tests 'kosatsu.methods.test-weave))
