#!/usr/bin/env bb
;; New babashka test for methods/ingest.clj (no test_ingest.py existed; fresh coverage).
(ns kosatsu.methods.test-ingest
  "Tests for the 高札 (kosatsu) offline designation normalizer (methods/ingest.clj).
  ADR-2606072003.

  Pins against `python3 ingest.py` on synthetic records:

    normalize-designation — fields + keyword-prefixed values (measure/status/sourcing),
                            program default, asserted-notice=true, lifted-at conditional.
    normalize-authority   — fields + kind colon-prefix, label default, sourcing default.
    normalize-subject     — fields + kind colon-prefix, jurisdiction default, sourcing default.
    validate-* RAISE      — G1 etzhayyim-asserter, G2 verdict-measure, G3 under-sourced;
                            raises MUST propagate (not caught by ingest layer).
    ingest-file           — temp JSON fixture; counts + specific normalized record.

  Run as part of the standalone bb test suite."
  (:require [kosatsu.methods.ingest :as ingest]
            [clojure.test :refer [deftest is testing run-tests]]
            [clojure.java.io :as io]
            [cheshire.core :as json]))

;; ── helpers ───────────────────────────────────────────────────────────────────

(defn- raises? [thunk]
  (try (thunk) false
       (catch clojure.lang.ExceptionInfo _ true)
       (catch Exception _ true)))

(defn- raises-containing? [frag thunk]
  (try (thunk) false
       (catch clojure.lang.ExceptionInfo e
         (clojure.string/includes? (.getMessage e) frag))
       (catch Exception e
         (clojure.string/includes? (str (.getMessage e)) frag))))

;; ── fixtures ──────────────────────────────────────────────────────────────────

(defn- good-designation-rec []
  {"id"        "desg-001"
   "asserter"  "us-ofac"
   "subject"   "subj-alpha"
   "measure"   "asset-freeze"    ; no leading colon — normalize must add it
   "program"   "SDN"
   "status"    "listed"          ; no leading colon — normalize must add it
   "posted_at" 20230101
   "sourcing"  "authoritative"   ; no leading colon
   "sources"   ["https://home.treasury.gov/a" "https://home.treasury.gov/b"]})

(defn- good-authority-rec []
  {"id"           "us-ofac"
   "kind"         "state-treasury"  ; no leading colon — normalize must add it
   "label"        "OFAC"
   "jurisdiction" "us"
   "stance"       "primary sanctions + AML"
   "sourcing"     "authoritative"
   "sources"      ["https://home.treasury.gov/"]})

(defn- good-subject-rec []
  {"id"           "subj-test"
   "kind"         "designated-entity"  ; no leading colon
   "label"        "Test Corp"
   "jurisdiction" "ru"
   "sourcing"     "representative"})

;; ── normalize-designation ─────────────────────────────────────────────────────

(deftest test-normalize-designation-fields
  ;; py parity: normalize_designation(good-designation-rec) =
  ;;   {':designation/id': 'desg-001', ':designation/asserter': 'us-ofac',
  ;;    ':designation/subject': 'subj-alpha', ':designation/measure': ':asset-freeze',
  ;;    ':designation/program': 'SDN', ':designation/status': ':listed',
  ;;    ':designation/posted-at': 20230101, ':designation/asserted-notice': True,
  ;;    ':designation/sourcing': ':authoritative',
  ;;    ':designation/sources': ['https://home.treasury.gov/a', 'https://home.treasury.gov/b']}
  (let [d (ingest/normalize-designation (good-designation-rec))]
    (testing "id"
      (is (= "desg-001" (get d ":designation/id"))))
    (testing "asserter"
      (is (= "us-ofac" (get d ":designation/asserter"))))
    (testing "subject"
      (is (= "subj-alpha" (get d ":designation/subject"))))
    (testing "measure colon-prefixed"
      (is (= ":asset-freeze" (get d ":designation/measure"))))
    (testing "program"
      (is (= "SDN" (get d ":designation/program"))))
    (testing "status colon-prefixed"
      (is (= ":listed" (get d ":designation/status"))))
    (testing "posted-at as long"
      (is (= 20230101 (get d ":designation/posted-at"))))
    (testing "asserted-notice is true"
      (is (true? (get d ":designation/asserted-notice"))))
    (testing "sourcing colon-prefixed"
      (is (= ":authoritative" (get d ":designation/sourcing"))))
    (testing "sources vector"
      (is (= ["https://home.treasury.gov/a" "https://home.treasury.gov/b"]
             (get d ":designation/sources"))))
    (testing "no lifted-at when absent"
      (is (not (contains? d ":designation/lifted-at"))))))

(deftest test-normalize-designation-defaults
  ;; py parity: no program/status/sourcing → defaults (unspecified), :listed, :representative
  (let [rec {"id"        "desg-003"
             "asserter"  "us-ofac"
             "subject"   "subj-alpha"
             "measure"   "travel-restriction"
             "posted_at" 20240101
             "sources"   ["https://home.treasury.gov/a" "https://home.treasury.gov/b"]}
        d (ingest/normalize-designation rec)]
    (is (= "(unspecified)" (get d ":designation/program")))
    (is (= ":listed" (get d ":designation/status")))
    (is (= ":representative" (get d ":designation/sourcing")))
    (is (= ":travel-restriction" (get d ":designation/measure")))))

(deftest test-normalize-designation-already-colon-prefixed
  ;; py: rec['measure'] already starts with ':' → passes through unchanged
  (let [rec (assoc (good-designation-rec) "measure" ":financial-sanction" "status" ":listed")
        d (ingest/normalize-designation rec)]
    (is (= ":financial-sanction" (get d ":designation/measure")))
    (is (= ":listed" (get d ":designation/status")))))

(deftest test-normalize-designation-lifted-at
  ;; py parity: delisted rec with lifted_at → lifted-at present
  ;; normalize_designation({'id':'desg-002','asserter':'eu-council','subject':'subj-beta',
  ;;   'measure':':financial-sanction','status':':delisted','posted_at':20220601,
  ;;   'lifted_at':20231201,'sourcing':':authoritative',
  ;;   'sources':['https://eur-lex.europa.eu/a','https://eur-lex.europa.eu/b']})
  ;;   → ':designation/lifted-at': 20231201
  (let [rec {"id"        "desg-002"
             "asserter"  "eu-council"
             "subject"   "subj-beta"
             "measure"   ":financial-sanction"
             "status"    ":delisted"
             "posted_at" 20220601
             "lifted_at" 20231201
             "sourcing"  ":authoritative"
             "sources"   ["https://eur-lex.europa.eu/a" "https://eur-lex.europa.eu/b"]}
        d (ingest/normalize-designation rec)]
    (is (= 20231201 (get d ":designation/lifted-at")))
    (is (= ":delisted" (get d ":designation/status")))))

;; ── normalize-authority ───────────────────────────────────────────────────────

(deftest test-normalize-authority-fields
  ;; py parity: normalize_authority(good-authority-rec) =
  ;;   {':authority/id': 'us-ofac', ':authority/kind': ':state-treasury',
  ;;    ':authority/label': 'OFAC', ':authority/jurisdiction': 'us',
  ;;    ':authority/stance': 'primary sanctions + AML', ':authority/sourcing': ':authoritative',
  ;;    ':authority/sources': ['https://home.treasury.gov/']}
  (let [a (ingest/normalize-authority (good-authority-rec))]
    (testing "id"
      (is (= "us-ofac" (get a ":authority/id"))))
    (testing "kind colon-prefixed"
      (is (= ":state-treasury" (get a ":authority/kind"))))
    (testing "label"
      (is (= "OFAC" (get a ":authority/label"))))
    (testing "jurisdiction"
      (is (= "us" (get a ":authority/jurisdiction"))))
    (testing "stance"
      (is (= "primary sanctions + AML" (get a ":authority/stance"))))
    (testing "sourcing colon-prefixed"
      (is (= ":authoritative" (get a ":authority/sourcing"))))
    (testing "sources"
      (is (= ["https://home.treasury.gov/"] (get a ":authority/sources"))))))

(deftest test-normalize-authority-label-defaults-to-id
  ;; py: rec.get('label', rec['id']) — if no label, fallback is the id
  (let [rec (dissoc (good-authority-rec) "label")
        a (ingest/normalize-authority rec)]
    (is (= "us-ofac" (get a ":authority/label")))))

(deftest test-normalize-authority-jurisdiction-defaults
  ;; py: rec.get('jurisdiction', '?') → '?' when absent
  (let [rec (dissoc (good-authority-rec) "jurisdiction")
        a (ingest/normalize-authority rec)]
    (is (= "?" (get a ":authority/jurisdiction")))))

;; ── normalize-subject ─────────────────────────────────────────────────────────

(deftest test-normalize-subject-fields
  ;; py parity: normalize_subject(good-subject-rec) =
  ;;   {':subject/id': 'subj-test', ':subject/kind': ':designated-entity',
  ;;    ':subject/label': 'Test Corp', ':subject/jurisdiction': 'ru',
  ;;    ':subject/sourcing': ':representative'}
  (let [s (ingest/normalize-subject (good-subject-rec))]
    (testing "id"
      (is (= "subj-test" (get s ":subject/id"))))
    (testing "kind colon-prefixed"
      (is (= ":designated-entity" (get s ":subject/kind"))))
    (testing "label"
      (is (= "Test Corp" (get s ":subject/label"))))
    (testing "jurisdiction"
      (is (= "ru" (get s ":subject/jurisdiction"))))
    (testing "sourcing"
      (is (= ":representative" (get s ":subject/sourcing"))))))

(deftest test-normalize-subject-label-defaults-to-id
  ;; py: rec.get('label', rec['id'])
  (let [rec (dissoc (good-subject-rec) "label")
        s (ingest/normalize-subject rec)]
    (is (= "subj-test" (get s ":subject/label")))))

(deftest test-normalize-subject-jurisdiction-default
  ;; py: rec.get('jurisdiction', '(rep)')
  (let [rec (dissoc (good-subject-rec) "jurisdiction")
        s (ingest/normalize-subject rec)]
    (is (= "(rep)" (get s ":subject/jurisdiction")))))

;; ── validator RAISE propagation ───────────────────────────────────────────────

(deftest test-g1-etzhayyim-asserter-raises
  ;; G1: authority id contains 'etzhayyim' → validate-authority throws → propagates through normalize-authority
  ;; py: G1: authority 'etzhayyim' resolves to etzhayyim — UNREPRESENTABLE...
  (let [bad-rec (assoc (good-authority-rec)
                       "id" "etzhayyim"
                       "kind" "state-treasury")]
    (is (raises-containing? "G1"
          #(ingest/normalize-authority bad-rec)))))

(deftest test-g2-verdict-measure-raises
  ;; G2: measure 'criminal' is a verdict/label — unrepresentable
  ;; py: G2: measure 'criminal' is a verdict/label — unrepresentable...
  (let [bad-rec (assoc (good-designation-rec) "measure" "criminal")]
    (is (raises-containing? "G2"
          #(ingest/normalize-designation bad-rec)))))

(deftest test-g3-undersourced-designation-raises
  ;; G3: designation needs >=2 sources; only 1 → throws
  (let [bad-rec (assoc (good-designation-rec) "sources" ["https://home.treasury.gov/a"])]
    (is (raises? #(ingest/normalize-designation bad-rec)))))

(deftest test-g3-authority-no-sources-raises
  ;; G3: authority needs >=1 sources
  (let [bad-rec (assoc (good-authority-rec) "sources" [])]
    (is (raises? #(ingest/normalize-authority bad-rec)))))

(deftest test-g5-bad-subject-kind-raises
  ;; G5: subject kind not in closed vocab
  (let [bad-rec (assoc (good-subject-rec) "kind" "secret-entity")]
    (is (raises-containing? "G5"
          #(ingest/normalize-subject bad-rec)))))

;; ── ingest-file ───────────────────────────────────────────────────────────────

(deftest test-ingest-file-counts
  ;; py parity: ingest_file(tmp) → authorities=1, subjects=1, designations=2
  (let [data {"authorities"  [{"id" "us-ofac" "kind" "state-treasury" "label" "OFAC"
                               "jurisdiction" "us" "stance" "primary"
                               "sourcing" "authoritative"
                               "sources" ["https://home.treasury.gov/"]}]
              "subjects"     [{"id" "e-corp" "kind" "designated-entity"
                               "label" "E Corp" "sourcing" "representative"}]
              "designations" [{"id" "d1" "asserter" "us-ofac" "subject" "e-corp"
                               "measure" "asset-freeze" "status" "listed"
                               "posted_at" 20230101 "sourcing" "authoritative"
                               "sources" ["https://home.treasury.gov/a" "https://home.treasury.gov/b"]}
                              {"id" "d2" "asserter" "us-ofac" "subject" "e-corp"
                               "measure" "financial-sanction" "status" "listed"
                               "posted_at" 20230201 "sourcing" "authoritative"
                               "sources" ["https://home.treasury.gov/a" "https://home.treasury.gov/b"]}]}
        tmp (java.io.File/createTempFile "kosatsu-ingest-test" ".json")]
    (try
      (spit tmp (json/generate-string data))
      (let [out (ingest/ingest-file (str tmp))]
        (testing "authority count"
          (is (= 1 (count (get out "authorities")))))
        (testing "subject count"
          (is (= 1 (count (get out "subjects")))))
        (testing "designation count"
          (is (= 2 (count (get out "designations"))))))
      (finally
        (.delete tmp)))))

(deftest test-ingest-file-first-authority
  ;; py parity: result['authorities'][0] =
  ;;   {':authority/id': 'us-ofac', ':authority/kind': ':state-treasury',
  ;;    ':authority/label': 'OFAC', ':authority/jurisdiction': 'us',
  ;;    ':authority/stance': 'primary', ':authority/sourcing': ':authoritative',
  ;;    ':authority/sources': ['https://home.treasury.gov/']}
  (let [data {"authorities"  [{"id" "us-ofac" "kind" "state-treasury" "label" "OFAC"
                               "jurisdiction" "us" "stance" "primary"
                               "sourcing" "authoritative"
                               "sources" ["https://home.treasury.gov/"]}]
              "subjects"     [{"id" "e-corp" "kind" "designated-entity"
                               "label" "E Corp" "sourcing" "representative"}]
              "designations" [{"id" "d1" "asserter" "us-ofac" "subject" "e-corp"
                               "measure" "asset-freeze" "status" "listed"
                               "posted_at" 20230101 "sourcing" "authoritative"
                               "sources" ["https://home.treasury.gov/a" "https://home.treasury.gov/b"]}]}
        tmp (java.io.File/createTempFile "kosatsu-ingest-test2" ".json")]
    (try
      (spit tmp (json/generate-string data))
      (let [out (ingest/ingest-file (str tmp))
            a   (first (get out "authorities"))]
        (is (= "us-ofac"         (get a ":authority/id")))
        (is (= ":state-treasury" (get a ":authority/kind")))
        (is (= "OFAC"            (get a ":authority/label")))
        (is (= "us"              (get a ":authority/jurisdiction")))
        (is (= "primary"         (get a ":authority/stance")))
        (is (= ":authoritative"  (get a ":authority/sourcing")))
        (is (= ["https://home.treasury.gov/"] (get a ":authority/sources"))))
      (finally
        (.delete tmp)))))

(deftest test-ingest-file-first-designation
  ;; py parity: result['designations'][0] =
  ;;   {':designation/id': 'd1', ':designation/asserter': 'us-ofac',
  ;;    ':designation/subject': 'e-corp', ':designation/measure': ':asset-freeze',
  ;;    ':designation/program': '(unspecified)', ':designation/status': ':listed',
  ;;    ':designation/posted-at': 20230101, ':designation/asserted-notice': True,
  ;;    ':designation/sourcing': ':authoritative',
  ;;    ':designation/sources': ['https://home.treasury.gov/a', 'https://home.treasury.gov/b']}
  (let [data {"authorities"  [{"id" "us-ofac" "kind" "state-treasury" "label" "OFAC"
                               "jurisdiction" "us" "stance" "primary"
                               "sourcing" "authoritative"
                               "sources" ["https://home.treasury.gov/"]}]
              "subjects"     [{"id" "e-corp" "kind" "designated-entity"
                               "label" "E Corp" "sourcing" "representative"}]
              "designations" [{"id" "d1" "asserter" "us-ofac" "subject" "e-corp"
                               "measure" "asset-freeze" "status" "listed"
                               "posted_at" 20230101 "sourcing" "authoritative"
                               "sources" ["https://home.treasury.gov/a" "https://home.treasury.gov/b"]}]}
        tmp (java.io.File/createTempFile "kosatsu-ingest-test3" ".json")]
    (try
      (spit tmp (json/generate-string data))
      (let [out (ingest/ingest-file (str tmp))
            d   (first (get out "designations"))]
        (is (= "d1"                (get d ":designation/id")))
        (is (= "us-ofac"           (get d ":designation/asserter")))
        (is (= "e-corp"            (get d ":designation/subject")))
        (is (= ":asset-freeze"     (get d ":designation/measure")))
        (is (= "(unspecified)"     (get d ":designation/program")))
        (is (= ":listed"           (get d ":designation/status")))
        (is (= 20230101            (get d ":designation/posted-at")))
        (is (true?                 (get d ":designation/asserted-notice")))
        (is (= ":authoritative"    (get d ":designation/sourcing")))
        (is (= ["https://home.treasury.gov/a" "https://home.treasury.gov/b"]
               (get d ":designation/sources"))))
      (finally
        (.delete tmp)))))

(deftest test-ingest-file-invalid-record-raises
  ;; A bad designation (verdict measure) in the JSON file causes ingest-file to raise.
  (let [data {"authorities"  [{"id" "us-ofac" "kind" "state-treasury" "label" "OFAC"
                               "jurisdiction" "us" "stance" "primary"
                               "sourcing" "authoritative"
                               "sources" ["https://home.treasury.gov/"]}]
              "subjects"     [{"id" "e-corp" "kind" "designated-entity"
                               "label" "E Corp" "sourcing" "representative"}]
              "designations" [{"id" "bad-d" "asserter" "us-ofac" "subject" "e-corp"
                               "measure" "criminal"   ; G2 verdict measure
                               "status" "listed"
                               "posted_at" 20230101
                               "sourcing" "authoritative"
                               "sources" ["https://home.treasury.gov/a" "https://home.treasury.gov/b"]}]}
        tmp (java.io.File/createTempFile "kosatsu-ingest-bad" ".json")]
    (try
      (spit tmp (json/generate-string data))
      (is (raises-containing? "G2"
            #(ingest/ingest-file (str tmp))))
      (finally
        (.delete tmp)))))

;; ── entry point ───────────────────────────────────────────────────────────────

(when (= *file* (System/getProperty "babashka.file"))
  (let [{:keys [fail error]} (clojure.test/run-tests 'kosatsu.methods.test-ingest)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
