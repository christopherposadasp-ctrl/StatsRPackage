# Phase 5 Required Sample Size Findings

Audit baseline: `codex/math-efficiency-audit` after Phase 4 findings commit `5f6066c`.

Phase 5 audited the exported required-sample-size functions:

- `n_required_from_power()`
- `n_required_z_mu()`
- `n_required_t_mu()`
- `n_required_p_z()`
- `n_required_var_chisq()`
- `n_required_var_ratio_F()`

Production code was not changed. The audited search behavior, wrapper composition, and default policies matched the intended behavior.

## Independent Checks

Temporary independent R probes passed for:

- generic solver minimality, `n_min` short-circuit behavior, unreachable-target errors, and invalid `power_at_n()` return validation;
- one-sample and two-sample z mean planning, including closed-form one-sided shortcuts, post-check power, previous-`n` minimality, and allocation ratio handling;
- one-sample, paired, pooled two-sample, and Welch two-sample t planning, including achieved-power equality against `power_t_mu()` and previous-`n` minimality;
- one-sample and two-sample proportion planning, including pooled default for `p0 = 0`, unpooled default for `p0 != 0`, two-sided continuity correction, allocation ratios, and achieved-power equality against `power_p_z()`;
- one-sample variance and two-sample variance-ratio planning, including less/greater alternatives, non-1 `ratio0`, allocation ratios, and achieved-power equality against the Phase 4 power functions.

No mathematical or minimality defects were found.

## Tests Added

Focused tests were added in `StatsPackage/tests/testthat/test-n-required.R` for:

- `n_required_from_power()` returning `n_min` when already sufficient;
- unreachable target and invalid vector-valued `power_at_n()` errors;
- `n_required_z_mu()` two-sample one-sided allocation and minimality;
- `n_required_t_mu()` two-sample pooled and Welch allocation, achieved power, and minimality;
- `n_required_p_z()` two-sample pooled/unpooled defaults, two-sided continuity behavior, achieved power, and minimality for the pooled case;
- `n_required_var_chisq()` less-tail achieved power and minimality;
- `n_required_var_ratio_F()` non-1 `ratio0`, allocation, achieved power, and minimality.

These tests preserve the existing public API and add no production-code length.

## Policy Status

The Phase 5 audit preserved:

- exported function names, signatures, defaults, and result classes;
- `beta_target` to `target_power = 1 - beta_target` conversion;
- unrounded stored computational fields and `digits` as print-only formatting;
- `quiet = TRUE` suppressing printed output;
- minimality as the smallest searched `n` for one-sample designs and smallest searched `n1` for two-sample designs;
- `n2 = ceiling(n_ratio * n1)` with existing minimum bounds;
- proportion default policy: pooled for two-sample `p0 = 0`, unpooled for two-sample `p0 != 0`;
- error for `pooled = TRUE` with nonzero two-sample `p0`;
- existing continuity-correction behavior inherited from `power_p_z()`;
- two-sided variance rejection regions as lower-tail OR upper-tail regions.

## Validation

Commands run:

```r
devtools::test("StatsPackage", filter = "n-required")
devtools::test("StatsPackage")
```

Reported results:

- Targeted required-n tests: `FAIL 0 | WARN 0 | SKIP 0 | PASS 77`
- Full test suite: `FAIL 0 | WARN 0 | SKIP 0 | PASS 561`

Local process note: both test commands reported clean test output, but the Rscript process returned a nonzero exit after reporting results. This is consistent with the previously recorded local R exit anomaly and is not counted as a test failure.

## Follow-Up Risks

No Phase 5 formula, search, or minimality blockers remain. Later phases should continue treating local `devtools::check()` as non-blocking until the known `Rcmd.exe` / Rscript exit anomaly is resolved in a clean environment.
