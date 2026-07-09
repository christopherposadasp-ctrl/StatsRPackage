# Phase 1 Findings: API and Default-Policy Lock

Date: 2026-05-08

Branch: `codex/math-efficiency-audit`

Baseline: StatsPackage `1.1.0`, after the `pi_mu()` release and pkgdown
workflow fix.

## Summary

Phase 1 found no API, signature, result-contract, or documented default-policy
blockers that need production-code changes before the formula-level audit.

No package R source files, tests, generated `.Rd` files, release tags, or
public APIs were changed during this phase.

## API and Documentation Alignment

- Exported function count from `StatsPackage/NAMESPACE`: 29.
- `docs/API_INDEX.md` has no missing or extra exported functions.
- Generated `.Rd` aliases cover all 29 exported functions.
- `README.md`, `StatsPackage/vignettes/getting-started.Rmd`, and
  `2.CheatsheetV8.r` each reference all 29 exported functions by name.
- `pi_mu()` remains the newest exported API from `StatsPackage/NEWS.md`
  and should receive extra scrutiny in the formula-level audit.

## Signature Inventory

| Function | Source file | Arguments |
| --- | --- | --- |
| `chisq_gof_probs()` | `chisq-functions.R` | `observed, p, labels, min_expected, alpha, digits, quiet` |
| `chisq_gof_dist()` | `chisq-functions.R` | `x, observed, dist, k, breaks, params, estimate, params_estimated, min_expected, alpha, digits, quiet` |
| `chisq_table()` | `chisq-functions.R` | `observed, type, correct, min_expected, alpha, digits, quiet` |
| `table_props()` | `chisq-functions.R` | `observed, margin, digits, quiet` |
| `ci_mu()` | `ci-functions.R` | `xbar, n, s, sigma, conf.level, paired, side, method, digits, quiet` |
| `ci_p()` | `ci-functions.R` | `x, n, conf.level, exact_1s, digits, quiet` |
| `ci_var()` | `ci-functions.R` | `s, n, conf.level, digits, quiet` |
| `ci_lambda_exp()` | `ci-functions.R` | `Sum, n, conf.level, digits, quiet` |
| `z_test_mu()` | `hypothesis-test-functions.R` | `xbar, mu0, sigma, n, alpha, alternative, digits, quiet, s` |
| `t_test_mu()` | `hypothesis-test-functions.R` | `xbar, mu0, s, n, alpha, alternative, var.equal, digits, quiet, paired` |
| `p_test()` | `hypothesis-test-functions.R` | `x, n, p0, alpha, alternative, digits, quiet, check_npq, pooled, continuity` |
| `var_test_chisq()` | `hypothesis-test-functions.R` | `s, n, sigma0, ratio0, alpha, alternative, digits, quiet` |
| `n_required_from_power()` | `n-required-functions.R` | `power_at_n, target_power, n_min, n_max, digits, quiet` |
| `n_required_z_mu()` | `n-required-functions.R` | `mu_a, mu0, sigma, alpha, beta_target, alternative, n_min, n_max, n_ratio, digits, quiet` |
| `n_required_t_mu()` | `n-required-functions.R` | `mu_a, mu0, sigma_true, alpha, beta_target, alternative, n_min, n_max, paired, method, n_ratio, digits, quiet` |
| `n_required_p_z()` | `n-required-functions.R` | `p_a, p0, alpha, beta_target, alternative, n_min, n_max, pooled, continuity, n_ratio, digits, quiet` |
| `n_required_var_chisq()` | `n-required-functions.R` | `sigma_a, sigma0, alpha, beta_target, alternative, n_min, n_max, digits, quiet` |
| `n_required_var_ratio_F()` | `n-required-functions.R` | `sigma_a, ratio0, alpha, beta_target, alternative, n_min, n_max, n_ratio, digits, quiet` |
| `power_z_mu()` | `power-functions.R` | `mu_a, mu0, sigma, n, alpha, alternative, digits, quiet` |
| `power_t_mu()` | `power-functions.R` | `mu_a, mu0, sigma_true, n, alpha, alternative, paired, method, digits, quiet` |
| `power_p_z()` | `power-functions.R` | `p_a, p0, n, alpha, alternative, digits, quiet, pooled, continuity` |
| `power_var_chisq()` | `power-functions.R` | `sigma_a, sigma0, n, alpha, alternative, digits, quiet` |
| `power_var_ratio_F()` | `power-functions.R` | `sigma_a, ratio0, n, alpha, alternative, digits, quiet` |
| `pi_mu()` | `prediction-functions.R` | `xbar, n, s, sigma, conf.level, side, digits, quiet` |
| `skew()` | `shape-functions.R` | `x, method, na_rm, digits, quiet` |
| `kurt()` | `shape-functions.R` | `x, method, excess, na_rm, digits, quiet` |
| `n_width_mu_z()` | `width-functions.R` | `w, sigma, conf.level, digits, quiet` |
| `n_width_p_wald()` | `width-functions.R` | `w, conf.level, p, worst_case, digits, quiet` |
| `n_width_mu_t()` | `width-functions.R` | `w, s, conf.level, n_start, max_iter, digits, quiet` |

## Result Object Contract

A representative runtime probe checked all 29 exported functions. Each
representative call:

- returned invisibly;
- returned a classed list;
- suppressed output with `quiet = TRUE`.

Existing tests already include display-only `digits` checks across the exported
families, including confidence intervals, prediction intervals, hypothesis
tests, power, required sample size, width planning, and shape helpers.

No additional contract tests were added in Phase 1 because the existing tests
and representative runtime probe were sufficient to lock the public contract
before formula-level review.

## Default-Policy Status

The default-policy commitments in `docs/DEFAULT_POLICIES.md` are represented in
implementation and tests:

- `p_test()` one-sample branch calls `stats::binom.test()`.
- Two-sample proportion defaults are resolved in `.ht_resolve_two_prop_options()`:
  pooled by default when `p0 = 0`, unpooled by default when `p0 != 0`, and
  `pooled = TRUE` with nonzero `p0` errors.
- Continuity correction remains off by default.
- `var_test_chisq()` stores and prints two-sided chi-square/F rejection regions
  as lower-tail OR upper-tail critical regions.
- `pi_mu()` is implemented as a prediction interval for one future observation:
  z-based with known `sigma`, t-based with sample `s`, and `df = n - 1` for the
  t branch.

No Phase 1 policy mismatches were found.

## Test Results

Targeted Phase 1 test filters reported clean test results:

- `devtools::test("StatsPackage", filter = "prediction")`:
  25 passed, 0 failed, 0 warnings, 0 skipped.
- `devtools::test("StatsPackage", filter = "hypothesis")`:
  80 passed, 0 failed, 0 warnings, 0 skipped.
- `devtools::test("StatsPackage", filter = "chisq")`:
  49 passed, 0 failed, 0 warnings, 0 skipped.
- `devtools::test("StatsPackage", filter = "power|n-required|width|ci|shape")`:
  252 passed, 0 failed, 0 warnings, 0 skipped.

The full test suite also reported clean results:

- `devtools::test("StatsPackage")`:
  406 passed, 0 failed, 0 warnings, 0 skipped.

The known local Windows R process-exit anomaly persisted after each
`devtools::test()` run: the test output completed successfully, then the R
process exited with status `-1073741569`. This remains consistent with the
Phase 0 environment issue and is not treated as a package-code finding.

`devtools::check()` remains environment-blocked locally and was not used as a
Phase 1 blocker. The cheat-sheet regression was not rerun in Phase 1 because no
printed output, default behavior, or return fields were changed.

## Phase 1 Conclusion

The package is ready to proceed to formula-level audit work. The next audit
target should be `pi_mu()` because it is the newest exported API and directly
encodes the newest default-policy commitment.
