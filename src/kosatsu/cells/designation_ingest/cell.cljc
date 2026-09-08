(ns kosatsu.cells.designation-ingest.cell
  "LangGraph Pregel wrapper for 高札 (kosatsu) designation_ingest — R0 scaffold.
  1:1 port of src/kosatsu/cells/designation_ingest/cell.cljc (ADR-2606072000). Normalizes a public authority's
  designation export into validated :designation/* datoms. .solve() raises at R0 (G8): live ingest
  of a real sanctions list is Council Lv6+ + operator + member-signature gated; offline normalization
  runs via methods/ingest."
  (:require [kotoba.lang.text]))

(defn solve
  [_input-state]
  (throw (ex-info "kosatsu R0 scaffold: designation_ingest normalizes offline; live list ingest (OFAC/EU/UN/UK-OFSI/JP-MOF/Interpol) is Council Lv6+ + operator + member-sig gated (G8)."
                  {:gate "G8"})))
