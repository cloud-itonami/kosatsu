(ns kosatsu.methods._edn
  "_edn.py — minimal EDN reader (subset: [] {} :kw \"str\" num bool nil). ADR-2606072000.

  The faithful 1:1 port of `src/kosatsu/methods/_edn.cljc` ALREADY LIVES in the established sibling
  `kosatsu.methods.edn` (file edn.cljc) — that is the module the whole actor (weave / kotoba /
  autorun / the tests) requires, and it is byte-for-byte the same reader (keywords kept as
  \":ns/name\" STRINGS). This file exists only to honor the snake_case filename convention
  (_edn.py → _edn.cljc); it re-exports the canonical reader rather than re-inlining it, so there is
  a single source of truth and no drift. Require `kosatsu.methods.edn` directly in new code."
  (:require [kosatsu.methods.edn :as edn]))

(def parse-edn edn/parse-edn)
#?(:clj (def load-edn edn/load-edn))
