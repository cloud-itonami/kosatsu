(ns kosatsu.methods._t
  "_t.py — tiny standalone test harness (no pytest needed) — shared by kosatsu test_*.py.
  1:1 Clojure port of `src/kosatsu/methods/_t.cljc`.

  NOTE: the .cljc test ports (test_consistency.cljc / test_lexicons.cljc /
  test_charter_invariants.cljc / test_autorun.cljc) use clojure.test directly rather than this
  harness — `run`/`expect-raises` are kept here only for parity with the Python `_t.run` /
  `_t.expect_raises` API. Pure (the Python sys.exit / traceback printing is host I/O, kept behind
  #?(:clj …); cljs callers get the {:passed :failed} return value)."
  (:require [kotoba.lang.text :as str]))

(defn run
  "Run a seq of [name fn] cases. A case passes if it returns without throwing; failures are printed
  and (on :clj) the process exits non-zero. Returns {:passed n :failed n}. Mirrors _t.run."
  [suite cases]
  (let [{:keys [passed failed]}
        (reduce
         (fn [acc [name fn]]
           (try
             (fn)
             (update acc :passed inc)
             (catch #?(:clj Exception :cljs :default) e
               #?(:clj (do (println (str "  FAIL " name))
                           (.printStackTrace ^Throwable e)))
               (update acc :failed inc))))
         {:passed 0 :failed 0}
         cases)
        total (+ passed failed)]
    (println (str "[" suite "] " passed "/" total " passed"))
    #?(:clj (when (pos? failed) (System/exit 1)))
    {:passed passed :failed failed}))

(defn expect-raises
  "Run thunk; assert it throws. If `contains` is non-empty, the message must contain it. Mirrors
  _t.expect_raises."
  [thunk & {:keys [contains] :or {contains ""}}]
  (let [outcome
        (try (thunk) ::no-throw
             (catch #?(:clj Exception :cljs :default) e
               (let [msg (str #?(:clj (.getMessage ^Throwable e) :cljs (.-message e)))]
                 (if (and (seq contains) (not (str/includes? msg contains)))
                   (throw (ex-info (str "raised but missing " (pr-str contains) ": " msg) {}))
                   ::ok))))]
    (when (= outcome ::no-throw)
      (throw (ex-info "expected an exception, none raised" {})))
    nil))
