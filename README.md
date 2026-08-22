# kosatsu (高札)

Standalone Etzhayyim observatory for attributed crime and sanctions designations,
delistings, and competing claims. It preserves authority attribution and event
history, exposes disagreement, and never issues a verdict or score.

EDN metadata, ontology, lexicons, and seed data are canonical. External lexicon
and DID JSON is isolated under `wire/`. Python twins, Go/TinyGo, shell runners,
and legacy JSON-LD manifests are prohibited.

## Running the suite

```bash
clojure -M:test     # 173 tests / 729 assertions  (measured 2026-08-22)
clojure -M:lint
bb test             # legacy path; the workspace retired bb as a script host
```

> **The `:test` alias was running 1 namespace of 13 until 2026-08-22.** The
> cognitect runner's default pattern is `#".*-test$"` and twelve of this repo's
> namespaces are named `kosatsu.methods.test-*`, so the suite ran
> `kosatsu.murakumo-test` alone — **31 assertions of 729** — and reported
> "0 failures". The alias now passes `-d test -r ".*"`.
>
> Fixing it surfaced two breaks that only `bb test` had been hiding, both
> because babashka is more permissive than the JVM: `cheshire` was missing from
> the test classpath (bb bundles it), and three tests called a `defn-` var
> directly (SCI does not enforce privacy). Both are fixed. **A green that
> covered 4% of the suite is the same defect the charter gates guard against —
> a check that did not run returning the value of a check that passed.**

## The durability plane — `kosatsu.methods.anchor` (錠)

`kosatsu.methods.kotoba` gives a content-addressed commit DAG: every
transaction's CID covers its datoms and its predecessor, so tampering is
detectable **by anyone holding the log**. What that cannot do is prove to a
third party that a given head existed at a given time — whoever controls the
file can rewrite the chain from genesis and hand out a consistent lie.

That gap matters here more than in most records. **G4** says a delisting is a
NEW datom carrying `:lifted-at` and that nothing is overwritten. Today that is a
promise. An anchored checkpoint makes it falsifiable: if the record ever moves
under a previously anchored head, the commitment no longer opens.

```clojure
(require '[kosatsu.methods.anchor :as anchor])

(anchor/verify-log "…/kosatsu.kotoba.log")
;=> {:ok? true :checked 12 :broken-at nil :reason nil}

(anchor/plan-from-log "…/kosatsu.kotoba.log"
                      {:provider :transparency :digest-fn sha256-hex})
;=> {:state {…:status :pending} :effect {:effect/type :checkpoint/append …}}
```

**It does not reimplement anchoring.** [`kotoba-lang/kotobase-anchor-fevm`][a]
already owns chain-neutral checkpoint receipts, and its default provider is a
signed append-only transparency log needing no chain, no gas and no RPC. A
second anchor format would be a second answer to the question the anchor plane
exists to answer — the same reason 高札 authors no designation of its own.

**It publishes nothing.** Every function returns a *plan*. The adapter that
consumes the effect lives outside this repository, and **G8** still governs
whether it may run.

[a]: https://github.com/kotoba-lang/kotobase-anchor-fevm

### Four refusals, each tested in both directions

| refusal | why |
|---|---|
| an **empty** log is `{:ok? false :checked 0 :reason :empty-log}` | checked-zero must not read as verified-intact |
| `checkpoint` **throws** on an unverified chain | a caller that ignores a nil anchors nothing and reports success |
| `provider` is **required**, no default | a defaulted destination is a destination nobody chose, and one of the two is irreversible |
| **G10-anchor**: `:evm` is refused unless the log is wholly `:authoritative` | a public-chain artefact is permanent and has no room to say *this was synthetic* |

That last one is the charter reaching into the anchor. From outside, a
commitment over the `:representative` seed and one over real designations are
the same 32 bytes. Notarising the seed to a public chain would leave a durable,
externally-citable record implying 高札 had captured real designations. The
sourcing mode is therefore carried **inside** the commitment — `database-id` is
`kosatsu/representative`, `kosatsu/authoritative`, `kosatsu/mixed` or
`kosatsu/unsourced` — so a receipt separated from its context still says which
kind of record it covers.

### The disclosure boundary

A checkpoint carries `{database-id, epoch, logical-checkpoint-root}` and nothing
else; the root is an opaque CID. No subject label, no authority id, no datom
crosses it. **G5** (subject-dignity / no-doxxing) would be violated by an anchor
that leaked who is designated, and the anchor is the one artefact here designed
to be permanent and public. `anchor/discloses-nothing?` checks this rather than
assuming it, because the payload shape is owned upstream and could grow without
anything here noticing.

### Current state

R0. The committed seed is `:representative` (synthetic ids and generic labels),
so **today every plan this produces is `kosatsu/representative` and the public
chain is closed to it by G10-anchor** — that is the design working, not a
limitation to route around. Live full-universe ingest (OFAC SDN / EU
consolidated / UN SC / UK OFSI / JP-MOF / Interpol) remains **G8-gated**:
Council Lv6+ plus operator plus member signature. No adapter is deployed and
nothing has been anchored.

### Known pre-existing debt

`clojure -M:lint` reports **13 errors across 8 files** (`src/kosatsu/mesh.clj`,
`ingest.cljc`, `weave.cljc` and five test namespaces). All predate the anchor
plane; the two new files contribute none. Recorded rather than silently carried.
