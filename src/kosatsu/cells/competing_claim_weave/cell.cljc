(ns kosatsu.cells.competing-claim-weave.cell
  "LangGraph Pregel wrapper for 高札 (kosatsu) competing_claim_weave — R0 scaffold.
  1:1 port of src/kosatsu/cells/competing_claim_weave/cell.cljc (ADR-2606072000). Weaves attributed designation
  events into the kotoba Datom log + computes divergence offline. .solve() raises at R0 (G8): the
  live Datom-log weave is Council Lv6+ + operator gated."
  (:require [kotoba.lang.text]))

(defn solve
  [_input-state]
  (throw (ex-info "kosatsu R0 scaffold: competing_claim_weave runs offline; live Datom-log weave is Council Lv6+ + operator gated (G8)."
                  {:gate "G8"})))
