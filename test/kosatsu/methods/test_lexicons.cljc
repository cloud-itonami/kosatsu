(ns kosatsu.methods.test-lexicons
  "test_lexicons.py — 高札 (kosatsu) lexicon well-formedness + NSID parity. ADR-2606072000.
  1:1 Clojure port of src/kosatsu/methods/test_lexicons.cljc (stdlib harness → clojure.test)."
  (:require [clojure.test :refer [deftest is run-tests]]
            #?(:clj [clojure.java.io :as io])
            [kosatsu.methods.edn :as edn]))

(def ^:private actor-dir (io/file (or (System/getProperty "user.dir") ".")))
(def ^:private lexdir (io/file actor-dir "contracts" "lexicons"))
(def ^:private expected
  ["assertingAuthority" "subjectEntity" "designationNotice"
   "competingClaimView" "delistingEvent" "networkPost"])

(defn- lex [name] (edn/load-edn (io/file lexdir (str name ".edn"))))

(deftest test-all-lexicons-present
  (doseq [name expected]
    (is (.exists (io/file lexdir (str name ".edn"))) (str "missing lexicon " name))))

(deftest test-lexicons-well-formed
  (doseq [name expected]
    (let [lx (lex name)
          rec (get-in lx [":defs" ":main" ":record"])]
      (is (= 1 (get lx ":lexicon")))
      (is (= (str "com.etzhayyim.kosatsu." name) (get lx ":id")))
      (is (= "object" (get rec ":type")))
      (is (and (sequential? (get rec ":required")) (seq (get rec ":required"))))
      (is (and (map? (get rec ":properties")) (seq (get rec ":properties")))))))

(deftest test-required-keys-are-properties
  (doseq [name expected]
    (let [rec (get-in (lex name) [":defs" ":main" ":record"])
          props (set (keys (get rec ":properties")))]
      (doseq [r (get rec ":required")]
        (is (contains? props (str ":" r)) (str name ": required " r " not in properties"))))))

#?(:clj (defn -main [& _] (run-tests 'kosatsu.methods.test-lexicons)))
