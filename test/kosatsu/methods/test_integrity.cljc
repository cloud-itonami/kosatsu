#!/usr/bin/env bb
;; kosatsu 高札 — validation of the designation referential-integrity check.
;; Run as part of `bb test`.
(ns kosatsu.methods.test-integrity
  "Validation of weave.cljc's referential-integrity check — check-integrity / assert-integrity,
  which guard the kosatsu invariant that every sanctions/crime DESIGNATION's asserter (the
  authority making the claim) and subject (who it is about) resolves to a real node in the graph.
  assert-integrity was ISOLATED (no test). A dangling ref means a competing-claim observation
  attributed to — or about — a non-existent entity, which would silently corrupt the divergence
  computation; this pins that the check finds every dangling asserter/subject and that
  assert-integrity refuses such a graph."
  (:require [kosatsu.methods.weave :as w]
            [clojure.test :refer [deftest is run-tests]]))

(def ^:private clean-g
  {"authorities" {"OFAC" {} "EU" {}}
   "subjects"    {"S1" {} "S2" {}}
   "designations" [{":designation/id" "D1" ":designation/asserter" "OFAC" ":designation/subject" "S1"}
                   {":designation/id" "D2" ":designation/asserter" "EU"   ":designation/subject" "S2"}]})

(defn- with-designations [g ds] (assoc g "designations" ds))

(deftest a-fully-resolved-graph-has-no-dangling-refs
  (is (= 0 (get (w/check-integrity clean-g) "dangling_count")))
  (is (= [] (get (w/check-integrity clean-g) "dangling")))
  (is (nil? (w/assert-integrity clean-g)) "assert-integrity passes (returns nil) on a clean graph"))

(deftest a-dangling-asserter-is-detected-and-refused
  (let [g (with-designations clean-g [{":designation/id" "D3" ":designation/asserter" "GHOST"
                                       ":designation/subject" "S1"}])
        rep (w/check-integrity g)
        d (first (get rep "dangling"))]
    (is (= 1 (get rep "dangling_count")))
    (is (= "asserter" (get d "field")) "the unresolved field is the asserter")
    (is (= "GHOST" (get d "ref")) "and the dangling ref is reported")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"integrity.*dangling" (w/assert-integrity g)))))

(deftest a-dangling-subject-is-detected-and-refused
  (let [g (with-designations clean-g [{":designation/id" "D4" ":designation/asserter" "OFAC"
                                       ":designation/subject" "NOBODY"}])
        d (first (get (w/check-integrity g) "dangling"))]
    (is (= "subject" (get d "field")))
    (is (= "NOBODY" (get d "ref")))
    (is (thrown? clojure.lang.ExceptionInfo (w/assert-integrity g)))))

(deftest both-ends-dangling-counts-two-and-only-dangling-ones-count
  ;; one designation with BOTH a bad asserter and a bad subject → 2 dangling
  (let [g2 (with-designations clean-g [{":designation/id" "D5" ":designation/asserter" "X"
                                        ":designation/subject" "Y"}])]
    (is (= 2 (get (w/check-integrity g2) "dangling_count"))))
  ;; a mix: one resolved + one dangling → exactly 1 counted (the resolved one is not flagged)
  (let [gm (with-designations clean-g [{":designation/id" "D1" ":designation/asserter" "OFAC" ":designation/subject" "S1"}
                                       {":designation/id" "D6" ":designation/asserter" "OFAC" ":designation/subject" "GONE"}])]
    (is (= 1 (get (w/check-integrity gm) "dangling_count")) "only the unresolved designation is flagged")))

#?(:clj
   (when (= *file* (System/getProperty "babashka.file"))
     (let [{:keys [fail error]} (run-tests 'kosatsu.methods.test-integrity)]
       (System/exit (if (zero? (+ fail error)) 0 1)))))
