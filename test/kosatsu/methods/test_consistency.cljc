(ns kosatsu.methods.test-consistency
  "test_consistency.py — 高札 (kosatsu) seed/ontology consistency + integrity. ADR-2606072000.
  1:1 Clojure port of src/kosatsu/methods/test_consistency.cljc (stdlib harness → clojure.test)."
  (:require [clojure.test :refer [deftest is run-tests]]
            [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            [kosatsu.methods.edn :as edn]
            [kosatsu.methods.weave :as weave]))

;; Paths resolve from the standalone repository root.
;;   parents[3] (ROOT)        = up 4 → root
(def ^:private actor-dir (io/file (or (System/getProperty "user.dir") ".")))
(def ^:private ont (io/file actor-dir "contracts" "ontology" "crime-sanctions-ontology.kotoba.edn"))
(def ^:private seed (io/file actor-dir "data" "seed-designation-graph.kotoba.edn"))

(defn- lstrip-colon [^String s]
  (loop [i 0] (if (and (< i (count s)) (= \: (nth s i))) (recur (inc i)) (subs s i))))

(deftest test-seed-no-dangling-refs
  (let [g (weave/weave (edn/load-edn seed))]
    (is (= 0 (get (weave/check-integrity g) "dangling_count")))))

(deftest test-seed-subject-kinds-in-ontology
  (let [o (edn/load-edn ont)
        kinds (set (map lstrip-colon (get o ":ontology/subject-kinds")))]
    (doseq [s (get (edn/load-edn seed) ":subjects")]
      (is (contains? kinds (lstrip-colon (get s ":subject/kind")))))))

(deftest test-seed-authority-kinds-in-ontology
  (let [o (edn/load-edn ont)
        kinds (set (map lstrip-colon (get o ":ontology/authority-kinds")))]
    (doseq [a (get (edn/load-edn seed) ":authorities")]
      (is (contains? kinds (lstrip-colon (get a ":authority/kind")))))))

(deftest test-every-designation-resolves
  (let [g (weave/weave (edn/load-edn seed))
        auth (set (keys (get g "authorities")))
        subj (set (keys (get g "subjects")))]
    (doseq [d (get g "designations")]
      (is (contains? auth (get d ":designation/asserter")))
      (is (contains? subj (get d ":designation/subject"))))))

(deftest test-every-subject-has-a-designation
  (let [g (weave/weave (edn/load-edn seed))
        designated (set (map #(get % ":designation/subject") (get g "designations")))]
    (doseq [sid (keys (get g "subjects"))]
      (is (contains? designated sid)
          (str "subject " sid " has no designation (G5: subjects exist only as targets)")))))

#?(:clj (defn -main [& _] (run-tests 'kosatsu.methods.test-consistency)))
