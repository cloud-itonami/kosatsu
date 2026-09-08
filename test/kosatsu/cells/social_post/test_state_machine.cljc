(ns kosatsu.cells.social-post.test-state-machine
  "高札 (kosatsu) cell scaffolds + publication membrane tests. 1:1 port of test/kosatsu/cells/social_post/test_state_machine.cljc
  (ADR-2606072000). G8 every cell .solve() raises at R0; the social_post drafting membrane enforces
  G3 (≥2 sources) / G7 (no-server-key) / G8 (dry-run only) / G9 (mirror disclaimer)."
  (:require [clojure.test :refer [deftest is]]
            [kotoba.lang.text :as str]
            [kosatsu.cells.social-post.state-machine :as sp]
            [kosatsu.cells.designation-ingest.cell :as di]
            [kosatsu.cells.competing-claim-weave.cell :as ccw]))

;; ── G8: every cell .solve() raises at R0 ──
(deftest test-designation-ingest-solve-raises
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G8" (di/solve {}))))

(deftest test-competing-claim-weave-solve-raises
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G8" (ccw/solve {}))))

(deftest test-social-post-solve-raises
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G8" (sp/solve {}))))

;; ── publication membrane (G2/G3/G7/G8/G9) ──
(deftest test-post-drafts-when-clean
  (let [out (sp/transition-to-drafted {"subject" "subj-alpha contested"
                                       "sources" ["https://ofac.treasury.gov/" "https://www.sanctionsmap.eu/"]})
        cs (get out "cell_state")]
    (is (= sp/phase-drafted (get cs "phase")))
    (is (= ":dry-run" (get-in cs ["payload" ":post/status"])))
    (is (= true (get-in cs ["payload" ":post/is-mirror"])))
    (is (= false (get-in cs ["payload" ":post/server-held-key"])))
    (is (str/starts-with? (get-in cs ["payload" ":post/body"]) "[mirror"))))

(deftest test-post-refuses-under-sourced
  (let [out (sp/transition-to-drafted {"subject" "x" "sources" ["https://ofac.treasury.gov/"]})]
    (is (= sp/phase-refused (get-in out ["cell_state" "phase"])))
    (is (str/includes? (get-in out ["cell_state" "refusal"]) "G3"))))

(deftest test-post-refuses-published-status
  (let [out (sp/transition-to-drafted {"subject" "x" "requested_status" "published"
                                       "sources" ["https://ofac.treasury.gov/" "https://www.sanctionsmap.eu/"]})]
    (is (= sp/phase-refused (get-in out ["cell_state" "phase"])))
    (is (str/includes? (get-in out ["cell_state" "refusal"]) "G8"))))

(deftest test-post-refuses-server-key
  (let [out (sp/transition-to-drafted {"subject" "x" "server_held_key" true
                                       "sources" ["https://ofac.treasury.gov/" "https://www.sanctionsmap.eu/"]})]
    (is (= sp/phase-refused (get-in out ["cell_state" "phase"])))
    (is (str/includes? (get-in out ["cell_state" "refusal"]) "G7"))))
