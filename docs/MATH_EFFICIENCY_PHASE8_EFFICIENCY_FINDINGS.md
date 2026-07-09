# Phase 8 Code Efficiency And Simplification Findings

Audit baseline: `codex/math-efficiency-audit` after Phase 7 findings commit `569e9d4`.

Phase 8 reviewed code efficiency, simplification opportunities, solver behavior, and numerical stability in:

- `StatsPackage/R/n-required-functions.R`
- `StatsPackage/R/power-functions.R`
- `StatsPackage/R/power-helpers.R`
- `StatsPackage/R/hypothesis-test-functions.R`

One production change was made and committed separately as `61ef4f0` (`Improve upper-tail probability stability`). It preserves public APIs, defaults, result field names, result classes, and printed-output templates. The only intentional behavior change is improved numerical precision for extreme upper-tail probabilities that are mathematically nonzero.

## Duplication Review

Repeated validation, result construction, and printing logic remains across hypothesis-test, power, and required-sample-size families. No broad helper extraction was made because:

- existing helpers already cover shared result construction and common validation where the codebase relies on them;
- the remaining repeated print code is family-specific and tied to stable public text;
- extracting more helpers would likely increase production code length or risk printed-output drift without improving mathematical correctness or efficiency.

No simplification opportunity was identified that clearly reduced production code length while preserving behavior.

## Search And Solver Review

The required-sample-size solver review found:

- `.nreq_find_min_n()` validates `power_at_n`, `target_power`, `n_min`, and `n_max` before search;
- power evaluations are cached in an environment keyed by integer `n`;
- the search brackets by doubling, then uses binary search;
- the previous-`n` check verifies minimality after the returned candidate is found;
- invalid, non-finite, vector-valued, or out-of-range `power_at_n()` returns error before result construction;
- unreachable targets within `n_max` error cleanly.

The one-sided closed-form shortcuts in `n_required_z_mu()` already use post-check correction and backward search for minimality. No change was made because removing repeated power calls could alter `n_evaluations` semantics or weaken the minimality evidence.

## Numerical Stability Change

Temporary probes confirmed that `1 - pnorm(9)` underflows to `0`, while `pnorm(9, lower.tail = FALSE)` returns the correct nonzero upper-tail probability.

The following upper-tail calculations were changed to explicit `lower.tail = FALSE` calls:

- greater-tail p-values in `z_test_mu()` and `t_test_mu()`;
- normal upper-tail helpers used by `power_z_mu()` and `power_p_z()`;
- noncentral-t upper-tail power terms in `power_t_mu()`;
- chi-square upper-tail power terms in `power_var_chisq()`;
- F upper-tail power terms in `power_var_ratio_F()`.

This is a numerical-stability correction, not an API or default-policy change.

## Tests Added Or Updated

Focused tests were added or aligned in:

- `StatsPackage/tests/testthat/test-hypothesis-tests.R`
- `StatsPackage/tests/testthat/test-power.R`

Coverage now includes:

- an extreme greater-tail `z_test_mu()` p-value that remains nonzero;
- an extreme greater-tail `power_z_mu()` value that remains nonzero;
- hand-formula expectations using explicit upper-tail probability calls for affected hypothesis and power branches.

No generated documentation, README, vignette, or cheat-sheet source files were changed.

## Validation

Commands run:

```r
devtools::test("StatsPackage", filter = "hypothesis")
devtools::test("StatsPackage", filter = "power")
devtools::test("StatsPackage")
```

Reported results:

- Targeted hypothesis tests: `FAIL 0 | WARN 0 | SKIP 0 | PASS 130`
- Targeted power tests: `FAIL 0 | WARN 0 | SKIP 0 | PASS 110`
- Full test suite: `FAIL 0 | WARN 0 | SKIP 0 | PASS 628`

The cheat-sheet regression was also run because return values changed for extreme tail cases:

```powershell
Rscript scripts/qa_cheatsheet_audit.R
```

Reported result:

- Cheat-sheet audit: `Issues found: 0`

Local process note: the first targeted test attempt loaded a stale `rlang` from `C:/Users/posad/Rlibs`; tests were rerun with `--vanilla` and the isolated audit library. The successful test and cheat-sheet commands reported clean results, but Rscript returned a nonzero exit after reporting output. This matches the previously recorded local Rscript exit anomaly and is not counted as a test failure.

## Follow-Up Risks

No Phase 8 efficiency blocker remains. The main remaining operational risk is still the local R / Rscript / `Rcmd.exe` exit anomaly; `devtools::check()` should remain non-blocking locally until verified in a clean environment.
