# Phase 2 Findings: Confidence Intervals and Width Planning

Date: 2026-05-08

Branch: `codex/math-efficiency-audit`

Starting point: `dff678f` (`Record pi_mu formula audit findings`)

## Summary

The remaining Phase 2 confidence-interval and width-planning families were
audited after the completed `pi_mu()` review.

No mathematical defects were found in production code. No package R source
files, generated documentation, public API signatures, release tags, or
user-facing docs were changed.

Focused tests were added for under-covered confidence-interval formulas.

## Confidence-Interval Checks

Independent hand-calculation checks confirmed:

- `ci_mu()` one-sample z intervals use `SE = sigma / sqrt(n)` and z critical
  values;
- `ci_mu()` one-sample t intervals use `SE = s / sqrt(n)`, `df = n - 1`, and t
  critical values;
- `ci_mu()` paired t intervals use supplied summary differences, `s`, `n`, and
  `df = n - 1`;
- `ci_mu()` two-sample z intervals use
  `sqrt(sigma1^2 / n1 + sigma2^2 / n2)`;
- `ci_mu()` Welch intervals use the Satterthwaite degrees-of-freedom formula;
- `ci_mu()` pooled intervals use pooled variance, `df = n1 + n2 - 2`, and the
  pooled standard error;
- `ci_mu()` one-sided intervals use alpha-tail finite bounds with the opposite
  bound set to infinity;
- `ci_p()` one-sample exact intervals match `stats::binom.test()`;
- `ci_p()` retained Wald intervals match the normal-approximation formulas used
  by the implementation;
- `ci_var()` one-sample intervals use chi-square inversion for `sigma^2` and
  derive the SD interval by square root;
- `ci_var()` two-sample intervals use F inversion for `sigma1^2 / sigma2^2`;
- `ci_lambda_exp()` uses chi-square quantiles with `df = 2n`.

The independent confidence-interval formula audit passed.

## Width-Planning Checks

Independent checks confirmed:

- `n_width_mu_z()` uses total width `w`:
  `n = ceiling((2 * z * sigma / w)^2)`;
- `n_width_p_wald()` uses total width `w` and the planning value `p`;
- `n_width_p_wald()` worst-case mode uses `p = 0.5`;
- `n_width_mu_t()` returns the minimal valid `n` by direct comparison against a
  brute-force search;
- achieved full-width and `width_at_n_minus_1` fields support minimality
  checks.

The independent width formula and minimality audit passed.

## Test Changes

Added focused formula-lock tests in
`StatsPackage -1.0/tests/testthat/test-ci.R` for:

- one-sample and paired `ci_mu()` t intervals;
- one-sided `ci_mu()` finite-bound formulas;
- two-sample `ci_mu()` z, Welch, and pooled formulas;
- one-sample and two-sample `ci_p()` Wald formulas;
- two-sample `ci_var()` F-ratio interval formulas.

No width tests were added because the existing width tests already covered the
audited formulas, achieved-width fields, and minimality indicators.

No production code changes were made.

## Validation

- Independent confidence-interval formula audit script:
  passed.
- Independent width formula/minimality audit script:
  passed.
- `devtools::test("StatsPackage -1.0", filter = "ci")`:
  85 passed, 0 failed, 0 warnings, 0 skipped.
- `devtools::test("StatsPackage -1.0", filter = "width")`:
  50 passed, 0 failed, 0 warnings, 0 skipped.
- `devtools::test("StatsPackage -1.0")`:
  463 passed, 0 failed, 0 warnings, 0 skipped.

The known local Windows R process-exit anomaly persisted after the
`devtools::test()` output completed successfully, with exit status
`-1073741569`. This is consistent with earlier audit notes and is not treated
as a confidence-interval or width-planning finding.

The cheat-sheet regression was not rerun because this audit did not change
printed output, defaults, return fields, production code, or user-facing
examples.

## Conclusion

The Phase 2 confidence-interval and width-planning families are mathematically
correct for the audited 1.1.0 scope. Phase 2 can be considered complete, and
the audit can proceed to hypothesis-test families.
