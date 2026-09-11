(ns kosatsu.methods.test-autorun
  "test_autorun.py — kosatsu autonomous competing-claim heartbeat + kotoba Datom-log invariants.
  ADR-2606072000. 1:1 Clojure port of src/kosatsu/methods/test_autorun.cljc (stdlib harness → clojure.test).

  Guards the autonomy + persistence + neutral-competing-claim contract for the fleet:
    - the loop persists one content-addressed tx per heartbeat to an append-only log;
    - the log is a verifiable commit-DAG (every CID recomputes; tamper is detected);
    - determinism / resume-safe: persisted datoms are canonically ordered → CID reproducible
      across processes regardless of report's set-iteration order;
    - it is append-only; derived :kosatsu.div/* signals are flagged :kosatsu.div/derived;
    - every designation is an ATTRIBUTED event carrying a non-etzhayyim :designation/asserter;
    - no verdict / no per-subject score; the per-subject divergence :kosatsu.div/class is one of
      {contested | unanimous | single-asserter};
    - it does NO external I/O (offline seed, local persist — G7/G8 stay gated)."
  (:require [clojure.test :refer [deftest is run-tests]]
            [kotoba.lang.text :as str]
            [clojure.set :as set]
            #?(:clj [clojure.java.io :as io])
            [kosatsu.methods.autorun :as autorun]
            [kosatsu.methods.kotoba :as kotoba]))

#?(:clj
   (defn- tmp-log []
     (let [f (java.io.File/createTempFile "kosatsu" ".datoms.kotoba.edn")]
       (.delete f)
       f)))

#?(:clj
   (defn- json-key [v]
     ;; mirror json.dumps(d, ensure_ascii=False, sort_keys=True) for the canonical-order check
     (cond
       (string? v)     (str "\"" v "\"")
       (boolean? v)    (if v "true" "false")
       (nil? v)        "null"
       (integer? v)    (str v)
       (number? v)     (str v)
       (sequential? v) (str "[" (str/join ", " (map json-key v)) "]")
       :else (str v))))

(deftest test-heartbeat-persists
  (let [log (tmp-log)]
    (try
      (let [res (autorun/run-autonomous 3 autorun/seed-default log)]
        (is (= 3 (get res "log_length")) "one tx per heartbeat")
        (is (every? #(> (get % "datoms") 0) (get res "beats")) "every heartbeat persisted datoms")
        (is (get-in res ["chain" "ok"]) "commit-DAG verifies (chain OK)")
        (is (str/starts-with? (get res "head_cid") "b") "head CID is content-addressed"))
      (finally (.delete log)))))

(deftest test-canonical-order-deterministic
  (let [log (tmp-log)]
    (try
      (autorun/run-cycle 1 autorun/seed-default log)
      (let [datoms (get (first (kotoba/read-log log)) ":tx/datoms")
            keyed (mapv json-key datoms)]
        (is (= keyed (vec (sort keyed)))
            "persisted datoms are in canonical sorted order (cross-process deterministic)"))
      (finally (.delete log)))))

(deftest test-deterministic-resume-safe
  (let [a (tmp-log) b (tmp-log)]
    (try
      (let [ra (autorun/run-autonomous 3 autorun/seed-default a)
            rb (autorun/run-autonomous 3 autorun/seed-default b)]
        (is (= (mapv #(get % "cid") (get ra "beats")) (mapv #(get % "cid") (get rb "beats")))
            "same cycles → same CIDs (deterministic / resume-safe)"))
      (finally (.delete a) (.delete b)))))

(deftest test-append-only-and-tamper
  (let [log (tmp-log)]
    (try
      (autorun/run-cycle 1 autorun/seed-default log)
      (let [first* (kotoba/read-log log)]
        (autorun/run-cycle 2 autorun/seed-default log)
        (let [second* (kotoba/read-log log)]
          (is (= (count second*) (inc (count first*))) "second heartbeat appends, does not rewrite")
          (is (= (get (nth second* 1) ":tx/prev") (get (nth first* 0) ":tx/cid"))
              "tx 2 links tx 1's CID (commit-DAG)")
          (let [lines (str/split-lines (slurp log))
                lines (loop [i 0, out []]
                        (if (>= i (count lines))
                          out
                          (let [ln (nth lines i)]
                            (if (str/includes? ln ":tx/id 1 ")
                              (into (conj out (str/replace-first ln ":kosatsu.div/derived true"
                                                                ":kosatsu.div/derived false"))
                                    (subvec (vec lines) (inc i)))
                              (recur (inc i) (conj out ln))))))]
            (spit log (str (str/join "\n" lines) "\n"))
            (let [v (kotoba/verify-chain log)]
              (is (and (not (get v "ok")) (= 0 (get v "broken_at")))
                  "tampering an earlier tx breaks the chain")))))
      (finally (.delete log)))))

(deftest test-every-designation-attributed
  ;; etzhayyim authors NO designation: every designation event carries a non-etzhayyim asserter.
  (let [log (tmp-log)]
    (try
      (autorun/run-cycle 1 autorun/seed-default log)
      (let [datoms (get (first (kotoba/read-log log)) ":tx/datoms")
            desig-ents (set (keep (fn [d] (when (str/starts-with? (str (nth d 2)) ":designation/")
                                            (nth d 1)))
                                  datoms))]
        (is (> (count desig-ents) 0) "designation events persisted")
        (doseq [e desig-ents]
          (let [asserters (keep (fn [d] (when (and (= (nth d 1) e) (= (nth d 2) ":designation/asserter"))
                                          (nth d 3)))
                                datoms)]
            (is (= 1 (count asserters)) (str "designation " e " carries exactly one :asserter"))
            (is (and (seq asserters) (not (str/includes? (str/lower (str (first asserters))) "etzhayyim")))
                (str "designation " e " asserter is NOT etzhayyim (etzhayyim authors no designation)")))))
      (finally (.delete log)))))

(deftest test-no-score-no-verdict-neutral-class
  (let [log (tmp-log)]
    (try
      (autorun/run-cycle 1 autorun/seed-default log)
      (let [datoms (get (first (kotoba/read-log log)) ":tx/datoms")
            attrs (set (map #(str/lower (str (nth % 2))) datoms))]
        (doseq [tok ["score" "rank" "verdict" "guilt" "legitimacy" "true-crime" "trustworthiness"]]
          (is (not (some #(str/includes? % tok) attrs))
              (str "no `" tok "` attr in the log (no verdict / no score)")))
        (let [classes (set (keep (fn [d] (when (= (nth d 2) ":kosatsu.div/class") (nth d 3))) datoms))]
          (is (and (seq classes) (set/subset? classes #{":contested" ":unanimous" ":single-asserter"}))
              (str "divergence class ∈ {contested,unanimous,single-asserter} (neutral fact), got " classes)))
        (let [ops (set (map #(nth % 0) datoms))]
          (is (= #{":db/add"} ops) "every datom is append-only :db/add (no :db/retract)")))
      (finally (.delete log)))))

(def ^:private methods-dir (io/file (or (System/getProperty "user.dir") ".") "src" "kosatsu" "methods"))

(deftest test-no-external-io
  (let [src (str (slurp (io/file methods-dir "autorun.cljc"))
                 (slurp (io/file methods-dir "kotoba.cljc")))]
    (doseq [banned ["clj-http" "http-kit" "java.net.Socket" "java.net.URL" "slurp \"http"]]
      (is (not (str/includes? src banned))
          (str "autorun/kotoba does no external network I/O (no `" banned "`)")))))

#?(:clj (defn -main [& _] (run-tests 'kosatsu.methods.test-autorun)))
