(ns kosatsu.methods.test-charter-invariants
  "test_charter_invariants.py — 高札 (kosatsu) structural-invariant drift-lock. ADR-2606072000.
  1:1 Clojure port of src/kosatsu/methods/test_charter_invariants.cljc (stdlib harness → clojure.test).

  Parses the THREE homes of each invariant (ontology :db/allowed/enum vectors · lexicon
  :const/:enum · the seed values) and asserts they agree and carry no representable charter
  violation. The Python module imports VERDICT_TOKENS/SELF_TOKENS/PII_FORBIDDEN_SUBJECT_ATTRS from
  weave — here those are weave/verdict-tokens, weave/self-tokens, weave/pii-forbidden-subject-attrs.

  NOTE on token shapes: in the ontology/lexicons keywords are kept as ':ns/name' strings (the
  kosatsu.methods.edn reader convention); we lstrip the leading colon to compare against the bare
  tokens, exactly as the Python `k.lstrip(':')` does."
  (:require [clojure.test :refer [deftest is run-tests]]
            [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            [kosatsu.methods.edn :as edn]
            [kosatsu.methods.weave :as weave]))

(def ^:private actor-dir (io/file (or (System/getProperty "user.dir") ".")))
(def ^:private ont-path (io/file actor-dir "contracts" "ontology" "crime-sanctions-ontology.kotoba.edn"))
(def ^:private lexdir (io/file actor-dir "contracts" "lexicons"))
(def ^:private seed-path (io/file actor-dir "data" "seed-designation-graph.kotoba.edn"))

(defn- ont [] (edn/load-edn ont-path))

(defn- lstrip-colon [^String s]
  (loop [i 0] (if (and (< i (count s)) (= \: (nth s i))) (recur (inc i)) (subs s i))))

(defn- datom [ident]
  (or (some (fn [d] (when (= (get d ":db/ident") ident) d)) (get (ont) ":schema"))
      (throw (ex-info (str "no schema datom " ident) {}))))

(defn- lex [name] (edn/load-edn (io/file lexdir (str name ".edn"))))
(defn- props [name] (get-in (lex name) [":defs" ":main" ":record" ":properties"]))

;; ── ontology closed-vocab invariants ────────────────────────────────────────────
(deftest test-ont-authority-kinds-no-self
  (let [kinds (set (map lstrip-colon (get (ont) ":ontology/authority-kinds")))]
    (doseq [tok weave/self-tokens]
      (is (not (contains? kinds tok)) (str "G1: etzhayyim/self " tok " must not be an authority kind")))))

(deftest test-ont-measure-kinds-no-verdict
  (let [kinds (set (map lstrip-colon (get (ont) ":ontology/measure-kinds")))]
    (doseq [tok weave/verdict-tokens]
      (is (not (contains? kinds tok)) (str "G2/G3: verdict " tok " must not be a measure kind")))))

(deftest test-ont-status-no-final-or-verdict
  (let [statuses (set (map lstrip-colon (get (ont) ":ontology/designation-status")))]
    (is (= #{"listed" "delisted"} statuses))
    (doseq [tok ["guilty" "convicted" "permanent" "final"]]
      (is (not (contains? statuses tok)) (str "G4: " tok " must not be a status (非終末論)")))))

(deftest test-ont-post-status-dry-run-only
  (is (= ["dry-run"] (mapv lstrip-colon (get (ont) ":ontology/post-statuses")))))

(deftest test-ont-divergence-classes
  (is (= #{"unanimous" "contested" "single-asserter"}
         (set (map lstrip-colon (get (ont) ":ontology/divergence-classes"))))))

;; ── ontology schema :db/allowed invariants ──────────────────────────────────────
(deftest test-schema-authority-kind-no-self
  (let [allowed (set (map lstrip-colon (get (datom ":authority/kind") ":db/allowed")))]
    (doseq [tok weave/self-tokens]
      (is (not (contains? allowed tok))))))

(deftest test-schema-measure-no-verdict
  (let [allowed (set (map lstrip-colon (get (datom ":designation/measure") ":db/allowed")))]
    (doseq [tok weave/verdict-tokens]
      (is (not (contains? allowed tok))))))

(deftest test-schema-status-listed-delisted-only
  (is (= ["listed" "delisted"] (mapv lstrip-colon (get (datom ":designation/status") ":db/allowed")))))

(deftest test-schema-asserted-notice-true-only
  (is (= [true] (get (datom ":designation/asserted-notice") ":db/allowed"))))

(deftest test-schema-no-subject-score-attr
  (let [idents (set (map #(get % ":db/ident") (get (ont) ":schema")))]
    (doseq [bad [":subject/risk-score" ":subject/guilt" ":subject/threat-level" ":subject/rank"]]
      (is (not (contains? idents bad)) (str "G2/G7: " bad " must not exist (we never rate a subject)")))))

(deftest test-schema-no-self-designation-attr
  (let [idents (set (map #(get % ":db/ident") (get (ont) ":schema")))]
    (doseq [bad [":designation/our-verdict" ":our-designation" ":verdict"]]
      (is (not (contains? idents bad)) (str "G1: " bad " must not exist (etzhayyim authors no designation)")))))

(deftest test-schema-post-status-dry-run-only
  (is (= ["dry-run"] (mapv lstrip-colon (get (datom ":post/status") ":db/allowed")))))

(deftest test-schema-post-server-key-false-only
  (is (= [false] (get (datom ":post/server-held-key") ":db/allowed"))))

(deftest test-schema-post-is-mirror-true-only
  (is (= [true] (get (datom ":post/is-mirror") ":db/allowed"))))

;; ── lexicon :enum/:const invariants ─────────────────────────────────────────────
(deftest test-lex-authority-kind-no-self
  (let [enum (set (get-in (props "assertingAuthority") [":kind" ":enum"]))]
    (doseq [tok weave/self-tokens]
      (is (not (contains? enum tok))))))

(deftest test-lex-measure-no-verdict
  (let [enum (set (get-in (props "designationNotice") [":measure" ":enum"]))]
    (doseq [tok weave/verdict-tokens]
      (is (not (contains? enum tok))))))

(deftest test-lex-designation-asserter-required
  (let [req (get-in (lex "designationNotice") [":defs" ":main" ":record" ":required"])]
    (is (some #{"asserter"} req) "G2: asserter must be required on a designation")))

(deftest test-lex-designation-asserted-notice-const-true
  (is (true? (get-in (props "designationNotice") [":assertedNotice" ":const"]))))

(deftest test-lex-designation-status-enum
  (is (= #{"listed" "delisted"} (set (get-in (props "designationNotice") [":status" ":enum"])))))

(deftest test-lex-designation-sources-min-two
  (is (= 2 (get-in (props "designationNotice") [":sources" ":minLength"]))))

(deftest test-lex-view-non-adjudicating-const-true
  (is (true? (get-in (props "competingClaimView") [":nonAdjudicatingNotice" ":const"]))))

(deftest test-lex-post-status-const-dry-run
  (is (= "dry-run" (get-in (props "networkPost") [":status" ":const"]))))

(deftest test-lex-post-is-mirror-const-true
  (is (true? (get-in (props "networkPost") [":isMirror" ":const"]))))

(deftest test-lex-post-server-key-const-false
  (is (false? (get-in (props "networkPost") [":serverHeldKey" ":const"]))))

;; ── seed value invariants ───────────────────────────────────────────────────────
(deftest test-seed-authorities-not-self
  (let [seed (edn/load-edn seed-path)]
    (doseq [a (get seed ":authorities")]
      (let [aid (str/lower-case (get a ":authority/id"))]
        (doseq [tok weave/self-tokens]
          (is (not (str/includes? aid tok)) (str "G1: authority " aid " resolves to self")))
        (is (not (str/blank? (str/trim (get a ":authority/stance"))))
            (str "G6: authority " aid " must declare a stance"))))))

(deftest test-seed-subjects-no-score-or-pii
  (is (seq weave/pii-forbidden-subject-attrs))
  (let [seed (edn/load-edn seed-path)]
    (doseq [s (get seed ":subjects")]
      (is (and (not (contains? s ":subject/risk-score")) (not (contains? s ":subject/guilt"))))
      (doseq [key (keys s)]
        (is (not (contains? weave/pii-forbidden-subject-attrs
                            (str/lower-case (last (str/split (lstrip-colon key) #"/" -1)))))
            (str key))))))

(deftest test-seed-designations-attributed-factual-sourced
  (let [seed (edn/load-edn seed-path)
        measures (set (map lstrip-colon (get (ont) ":ontology/measure-kinds")))]
    (doseq [d (get seed ":designations")]
      (is (not (str/blank? (str/trim (str (get d ":designation/asserter")))))
          (str "G2: " (get d ":designation/id") " needs an asserter"))
      (is (true? (get d ":designation/asserted-notice")))
      (is (contains? measures (lstrip-colon (get d ":designation/measure"))))
      (is (>= (count (get d ":designation/sources")) 2) (str (get d ":designation/id"))))))

(deftest test-seed-delisted-carries-lifted-at
  (let [seed (edn/load-edn seed-path)]
    (doseq [d (get seed ":designations")]
      (when (= "delisted" (lstrip-colon (get d ":designation/status")))
        (is (contains? d ":designation/lifted-at")
            (str "G4: " (get d ":designation/id") " delisted needs :lifted-at"))))))

;; ── lexicon ⊆ ontology drift-lock (BOTH directions) ─────────────────────────────
(deftest test-measure-lex-eq-ontology
  (let [enum (set (get-in (props "designationNotice") [":measure" ":enum"]))
        vocab (set (map lstrip-colon (get (ont) ":ontology/measure-kinds")))]
    (is (= enum vocab))))

(deftest test-authority-kind-lex-eq-ontology
  (let [enum (set (get-in (props "assertingAuthority") [":kind" ":enum"]))
        vocab (set (map lstrip-colon (get (ont) ":ontology/authority-kinds")))]
    (is (= enum vocab))))

(deftest test-subject-kind-lex-eq-ontology
  (let [enum (set (get-in (props "subjectEntity") [":kind" ":enum"]))
        vocab (set (map lstrip-colon (get (ont) ":ontology/subject-kinds")))]
    (is (= enum vocab))))

(deftest test-post-status-const-matches-ontology
  (let [statuses (set (map lstrip-colon (get (ont) ":ontology/post-statuses")))]
    (is (= #{(get-in (props "networkPost") [":status" ":const"])} statuses))))

#?(:clj (defn -main [& _] (run-tests 'kosatsu.methods.test-charter-invariants)))
