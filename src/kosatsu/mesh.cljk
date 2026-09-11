;; mesh.clj — kosatsu 高札 KOTOBA Mesh entry component (Clojure / kotoba-clj).
;;
;; The mesh-hosting face of actor:kosatsu (crime/sanctions competing-claim
;; observatory). Compiled by kotoba-clj into a kotoba:kais WASM component, placed
;; by the KOTOBA Mesh lattice. Kotoba-native slice: observe asserter→subject
;; designation edges as ATTRIBUTED Datom assertions, derive designation divergence
;; via Datalog → TRANSPARENCY. The full as-of/divergence analysis stays in methods.
;;
;; Posture: etzhayyim authors NO designation and NO verdict; every designation is
;; an attributed event. Map-not-target, non-adjudicating, no per-subject score.
;; host-imports: kqe-assert! / kqe-query → kotoba:kais/kqe (needs cap/kqe)
(ns kosatsu)

(defn run [ctx]
  ;; observe — attributed designation events (asserter designates subject).
  (kqe-assert! "kosatsu" "ofac" "designates" "subject-x")
  (kqe-assert! "kosatsu" "eu-council" "designates" "subject-x")
  (kqe-assert! "kosatsu" "un-sc" "designates" "subject-y")
  ;; derive — designation divergence/coverage → transparency map (Datalog).
  (kqe-query "transparency(?s) :- designates(?s)."))

(defn on-kse [topic payload]
  ;; KSE-topic trigger (observatory on-kse pattern, ADR-2606230001 §4).
  (kqe-query "transparency(?s) :- designates(?s)."))
