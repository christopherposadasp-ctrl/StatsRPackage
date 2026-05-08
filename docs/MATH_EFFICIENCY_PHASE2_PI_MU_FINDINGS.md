# Phase 2.4 Findings: `pi_mu()` Formula-Level Audit

Date: 2026-05-08

Branch: `codex/math-efficiency-audit`

Starting point: `dbbc554` (`Record phase 1 audit findings`)

## Summary

`pi_mu()` was audited as the newest exported API and the owner of the
prediction-interval default policy.

No mathematical defect was found in the production implementation. No package
R source files, generated documentation, public API signatures, or release tags
were changed.

One focused test block was added to lock previously under-tested one-sided
finite-bound formulas.

## Mathematical Checks

Independent hand-calculation checks confirmed:

- known-`sigma` prediction intervals use
  `SE_pred = sigma * sqrt(1 + 1 / n)`;
- sample-`s` prediction intervals use
  `SE_pred = s * sqrt(1 + 1 / n)`;
- the t branch uses `df = n - 1`;
- two-sided intervals use `alpha / 2` tail areas;
- one-sided intervals use `alpha` tail areas;
- `side = "lower"` returns `[xbar - margin, Inf]`;
- `side = "upper"` returns `[-Inf, xbar + margin]`.

The independent formula probe passed for six cases:

- z, two-sided;
- z, lower one-sided;
- z, upper one-sided with `n = 1`;
- t, two-sided;
- t, lower one-sided;
- t, upper one-sided with `n = 2`.

## Public Contract Checks

The implementation preserves the Phase 1 public contract:

- exactly one of `s` or `sigma` is required;
- the known-`sigma` branch permits `n >= 1`;
- the sample-`s` branch requires `n >= 2`;
- stored computational fields remain unrounded;
- returned objects are invisible classed lists with classes
  `pi_mu_result`, `prediction_result`, and `list`;
- printed output labels the result as `PI`, not `CI`.

## Documentation Alignment

The implementation remains aligned with:

- `docs/DEFAULT_POLICIES.md`;
- `StatsPackage -1.0/man/prediction_interval_functions.Rd`;
- `README.md`;
- `StatsPackage -1.0/vignettes/getting-started.Rmd`;
- `2.CheatsheetV8.r`.

No documentation changes were needed because public behavior and documented
policy did not change.

## Test Changes

Added a focused test in
`StatsPackage -1.0/tests/testthat/test-prediction-intervals.R`:

- verifies one-sided z and t intervals use the one-sided alpha-tail critical
  value;
- verifies finite bounds, `se_pred`, `crit`, `margin`, and `df`;
- locks known-`sigma` behavior at `n = 1`;
- locks sample-`s` behavior at `n = 2`.

No production code changes were made.

## Validation

- Independent `pi_mu()` formula audit script:
  passed 6 z/t two-sided and one-sided cases.
- `devtools::test("StatsPackage -1.0", filter = "prediction")`:
  35 passed, 0 failed, 0 warnings, 0 skipped.
- `devtools::test("StatsPackage -1.0")`:
  416 passed, 0 failed, 0 warnings, 0 skipped.

The known local Windows R process-exit anomaly persisted after the
`devtools::test()` output completed successfully, with exit status
`-1073741569`. This is consistent with the Phase 0 and Phase 1 environment
notes and is not treated as a `pi_mu()` finding.

The cheat-sheet regression was not rerun because this audit did not change
printed output, defaults, return fields, or production code.

## Conclusion

`pi_mu()` is mathematically correct for its current 1.1.0 scope. The function
is ready to remain unchanged while the audit proceeds to the next formula
family.
