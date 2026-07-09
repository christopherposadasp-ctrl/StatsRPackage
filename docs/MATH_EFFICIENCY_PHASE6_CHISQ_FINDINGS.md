# Phase 6 Chi-Square Findings

Audit baseline: `codex/math-efficiency-audit` after Phase 5 findings commit `a09f0a0`.

Phase 6 audited the exported chi-square and categorical helpers:

- `chisq_gof_probs()`
- `chisq_gof_dist()`
- `chisq_table()`
- `table_props()`

Production code was not changed. The audited formulas, degrees-of-freedom adjustments, rejection regions, table calculations, and proportion summaries matched the intended behavior.

## Independent Checks

Temporary independent R probes passed for:

- `chisq_gof_probs()` expected counts, Pearson contributions, residuals, observed proportions, df, p-value, critical value, upper-tail rejection region, explicit labels, generated labels, and equal-probability defaults;
- `chisq_gof_dist()` grouped normal GOF with `params_estimated = TRUE`, raw fixed-parameter exponential GOF with generated quantile bins, raw Poisson GOF with estimated lambda and right-tail combining, sparse adjacent combining for continuous classes, and raw uniform GOF with estimated bounds;
- `chisq_table()` expected counts, contributions, Pearson residuals, standardized residuals, df, p-value, critical value, homogeneity-vs-independence interpretation, and ignored Yates correction outside 2 x 2 tables;
- `table_props()` row, column, and overall proportions, totals, dimnames, invisible return behavior, and `digits` as print-only formatting.

No mathematical defects were found.

## Tests Added

Focused tests were added in `StatsPackage/tests/testthat/test-chisq-functions.R` for:

- explicit labels, stored observed proportions, critical values, and rejection-region shape in `chisq_gof_probs()`;
- generated default one-way GOF labels;
- raw fixed-parameter exponential binning and expected counts;
- grouped normal `params_estimated` df adjustment;
- sparse adjacent combining for continuous GOF classes;
- raw Poisson estimated lambda and df adjustment;
- ignored Yates correction outside 2 x 2 tables;
- independence and homogeneity statistic equivalence;
- `table_props()` totals and print-only `digits` behavior.

These tests preserve the existing public API and add no production-code length.

## Policy Status

The Phase 6 audit preserved:

- exported function names, signatures, defaults, and result classes;
- upper-tail chi-square p-values via `pchisq(..., lower.tail = FALSE)`;
- critical values via `qchisq(1 - alpha, df)`;
- unrounded stored computational fields and `digits` as print-only formatting;
- `quiet = TRUE` suppressing printed output;
- Yates correction only for 2 x 2 contingency tables;
- independence and homogeneity differing only by interpretation text;
- row, column, and overall table proportions matching `prop.table()`.

## Non-Blocking Observation

Named one-way observed vectors are currently converted to generated numeric labels after integer-count validation in `chisq_gof_probs()`. Explicit `labels` work correctly, and generated labels are stable. This is not a mathematical correctness issue, so Phase 6 did not change production behavior.

## Validation

Commands run:

```r
devtools::test("StatsPackage", filter = "chisq")
devtools::test("StatsPackage")
```

Reported results:

- Targeted chi-square tests: `FAIL 0 | WARN 0 | SKIP 0 | PASS 91`
- Full test suite: `FAIL 0 | WARN 0 | SKIP 0 | PASS 603`

Local process note: both test commands reported clean test output, but the Rscript process returned a nonzero exit after reporting results. This is consistent with the previously recorded local R exit anomaly and is not counted as a test failure.

## Follow-Up Risks

No Phase 6 mathematical blockers remain. Later phases should continue treating local `devtools::check()` as non-blocking until the known `Rcmd.exe` / Rscript exit anomaly is resolved in a clean environment.
