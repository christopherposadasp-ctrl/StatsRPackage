# Phase 4 Power Findings

Audit baseline: `codex/math-efficiency-audit` after Phase 3 findings commit `afd6f4c`.

Phase 4 audited the exported power functions:

- `power_z_mu()`
- `power_t_mu()`
- `power_p_z()`
- `power_var_chisq()`
- `power_var_ratio_F()`

Production code was not changed. The audited formulas and default policies matched the intended behavior.

## Independent Formula Checks

Temporary independent R probes passed for:

- `power_z_mu()` one-sample less-tail and two-sample greater-tail formulas, including SE, noncentrality, critical values, estimate critical boundaries, and power.
- `power_t_mu()` one-sample, paired, Welch, and pooled branches, including noncentral t power, df, SE, noncentrality, and tail direction.
- `power_p_z()` one-sample normal approximation, two-sample pooled default when `p0 = 0`, two-sample unpooled default when `p0 != 0`, one-sided continuity-correction warning/ignore behavior, and degenerate alternative-variance boundary behavior.
- `power_var_chisq()` less-tail and two-sided chi-square scaling under the alternative.
- `power_var_ratio_F()` less-tail and greater-tail F scaling, including non-1 `ratio0`.

No mathematical defects were found.

## Tests Added

Focused formula-lock tests were added in `StatsPackage/tests/testthat/test-power.R` for:

- `power_z_mu()` less-tail critical values, estimate boundary, rejection-region shape, and power.
- `power_t_mu()` Welch less-tail df, critical value, and power.
- `power_p_z()` one-sample less-tail estimate boundary and power.
- `power_p_z()` two-sample nonzero-`p0` default unpooled behavior, estimate boundary, and power.
- `power_var_chisq()` less-tail chi-square scaling under the alternative.
- `power_var_ratio_F()` greater-tail non-1 `ratio0` scaling.

These tests preserve the existing public API and add no production-code length.

## Policy Status

The Phase 4 audit preserved:

- existing exported function names and signatures;
- existing `alpha`, `alternative`, `digits`, and `quiet` behavior;
- unrounded stored computational fields;
- classed invisible return objects;
- two-sample proportion default policy: pooled for `p0 = 0`, unpooled for `p0 != 0`;
- error for `pooled = TRUE` with nonzero two-sample `p0`;
- existing limitation that two-sample proportion continuity correction is ignored with a warning unless `alternative = "two.sided"`;
- two-sided variance rejection regions as lower-tail OR upper-tail regions.

## Validation

Commands run:

```r
devtools::test("StatsPackage", filter = "power")
devtools::test("StatsPackage")
```

Reported results:

- Targeted power tests: `FAIL 0 | WARN 0 | SKIP 0 | PASS 108`
- Full test suite: `FAIL 0 | WARN 0 | SKIP 0 | PASS 534`

Local process note: both test commands reported clean test output, but the Rscript process returned a nonzero exit after reporting results. This is consistent with the previously recorded local R exit anomaly and is not counted as a test failure.

## Follow-Up Risks

No Phase 4 formula blockers remain. Later audit phases should continue treating local `devtools::check()` as non-blocking until the known `Rcmd.exe` / Rscript exit anomaly is resolved in a clean environment.
