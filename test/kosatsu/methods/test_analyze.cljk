(ns kosatsu.methods.test-analyze
  "test_analyze.cljc — 高札 (kosatsu) analyze dry-run driver. ADR-2606072000.
  1:1 Clojure port of the analyze-relevant assertions in src/kosatsu/methods/test_analyze.cljc.

  Covers the two assertions that exercise the analyze DRIVER:
    - test-analyze-renders-md : render(g) is ascii-safe-headered, mirror-disclaimered,
      and carries the Divergence + Delisting-timeline sections.
    - test-analyze-main-writes-file : -main writes methods/out/intel-report.md, the file
      exists, and its text contains \"kosatsu\".

  DEFERRED (out of scope of the analyze port — they exercise UNPORTED modules):
    test_social_posts_are_dry_run_mirror / test_social_has_contested_post  → need `social`
    test_ingest_live_refused / test_ingest_offline_normalizes_and_validates /
      test_ingest_rejects_verdict_measure                                  → need `ingest`
    test_bridge_join_keys_advisory / test_bridge_tsumugi_en_edges          → need `bridge`
  (bridge / ingest / social are not part of this analyze-driver port, matching the
  rasen/inochi/kabuto precedent.)"
  (:require [clojure.test :refer [deftest is run-tests]]
            [kotoba.lang.text :as str]
            [clojure.java.io :as io]
            [kosatsu.methods.edn :as edn]
            [kosatsu.methods.weave :as w]
            [kosatsu.methods.analyze :as analyze]))

(def actor-dir (io/file (or (System/getProperty "user.dir") ".")))
(def seed (io/file actor-dir "data" "seed-designation-graph.kotoba.edn"))

(defn- g [] (w/weave (edn/load-edn seed)))

;; ── analyze ───────────────────────────────────────────────────────────────────
(deftest test-analyze-renders-md
  (let [md (analyze/render (g))]
    (is (not (str/includes? md "競")) "sanity: ascii-safe headers")
    (is (str/includes? md "Mirror, not a verdict"))
    (is (str/includes? md "Divergence"))
    (is (str/includes? md "Delisting timeline"))))

(deftest test-analyze-main-writes-file
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                          "kosatsu-analyze-" (make-array java.nio.file.attribute.FileAttribute 0)))
        path (io/file (analyze/main tmp-dir))]
    (try
      (is (.exists path))
      (is (str/includes? (slurp path) "kosatsu"))
      (finally
        (.delete path)
        (.delete (.getParentFile path))
        (.delete tmp-dir)))))

#?(:clj (defn -main [& _] (run-tests 'kosatsu.methods.test-analyze)))
