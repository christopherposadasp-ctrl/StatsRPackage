# Mathematical Correctness And Efficiency Audit Final Report

Final closeout date: 2026-05-08

Branch: `codex/math-efficiency-audit`

Package version: `1.1.0`

Audit baseline: StatsPackage `1.1.0` on `main`, after the `pi_mu()` release and pkgdown workflow fix.

Phase 9 starting commit: `292dab2` (`Record efficiency audit findings`)

Audit change range before this report: `a8ba040..292dab2`

Published release tag checked and not altered: `1.1.0` at `e64d2f70390febdea21146bc868b4c6934b211ac`

## Summary

The mathematical correctness and code efficiency audit is complete through Phase 9 closeout.

No mathematical correctness defects were found in the audited formulas for prediction intervals, confidence intervals, hypothesis tests, power, required sample size, chi-square procedures, table proportions, or shape helpers.

One production-code change was made during Phase 8: upper-tail probability calculations now use explicit `lower.tail = FALSE` calls instead of upper-tail subtraction where that improves numerical precision. This preserves public APIs, defaults, result fields, result classes, and printed-output templates. The intended behavior change is limited to mathematically nonzero extreme upper-tail probabilities that could previously underflow to zero.

## Completed Phases

- Phase 0: baseline setup and QA recording.
- Phase 1: exported API, signatures, result contracts, and default-policy lock.
- Phase 2.4: `pi_mu()` formula-level audit.
- Phase 2 continuation: confidence intervals and width planning.
- Phase 3: hypothesis-test families.
- Phase 4: power families.
- Phase 5: required sample size for target power.
- Phase 6: chi-square and categorical procedures.
- Phase 7: descriptive shape helpers.
- Phase 8: code efficiency, simplification, and numerical stability.
- Phase 9: final regression, documentation alignment, and closeout.

## Public API And Documentation Status

Phase 9 reconfirmed:

- exported function count from `StatsPackage -1.0/NAMESPACE`: `29`;
- `docs/API_INDEX.md` covers all exported functions;
- generated `.Rd` aliases cover all exported functions;
- `2.CheatsheetV8.r` references all exported functions;
- `README.md` links to the API index, default policies, vignette, and cheat-sheet QA instead of enumerating every export;
- `StatsPackage -1.0/vignettes/getting-started.Rmd` remains a representative getting-started workflow and includes `pi_mu()`;
- `pi_mu()` remains present in exports and user-facing docs.

No README, vignette, generated `.Rd`, API index, default-policy, or cheat-sheet source changes were needed in Phase 9.

## Default-Policy Status

Runtime spot checks passed for:

- `pi_mu()` z prediction interval standard error;
- `pi_mu()` t prediction interval degrees of freedom;
- one-sample `p_test()` matching `stats::binom.test()`;
- two-sample `p_test()` pooled default when `p0 = 0`;
- two-sample `p_test()` unpooled default when `p0 != 0`;
- `pooled = TRUE` with nonzero two-sample `p0` erroring;
- chi-square and F two-sided rejection regions stored as lower-tail OR upper-tail regions;
- `n_required_from_power()` minimal returned integer;
- `skew()` and `kurt()` estimator field consistency.

The policies in `docs/DEFAULT_POLICIES.md` remain aligned with implementation and tests.

## Production Changes

Only one production change was made during the audit:

- Commit `61ef4f0` (`Improve upper-tail probability stability`)

Files changed in that commit:

- `StatsPackage -1.0/R/hypothesis-test-functions.R`
- `StatsPackage -1.0/R/power-functions.R`
- `StatsPackage -1.0/R/power-helpers.R`

The change replaced upper-tail subtraction with explicit upper-tail probability calls in affected hypothesis-test and power-function paths. No exported names, signatures, defaults, result fields, printed summaries, generated docs, or release tags changed.

## Test Coverage Added

Focused tests were added during the audit for:

- one-sided `pi_mu()` z and t prediction interval formulas;
- confidence interval z/t/Wald/F-ratio formula branches;
- hypothesis-test critical values, estimate-critical boundaries, exact binomial alternatives, continuity correction, and variance-ratio null behavior;
- power-function tail, critical-value, default-policy, and variance-scaling formulas;
- required-sample-size solver minimality, allocation, achieved-power, and error behavior;
- chi-square goodness-of-fit, distribution-fit, contingency-table, and table-proportion behavior;
- shape-helper estimators, interpretation labels, return classes, and validation;
- Phase 8 upper-tail numerical-stability behavior.

No broad abstraction or cosmetic code expansion was added outside tests.

## Final QA Results

Final full test suite:

```r
devtools::test("StatsPackage -1.0")
```

Reported result:

- `FAIL 0 | WARN 0 | SKIP 0 | PASS 628`

Final cheat-sheet regression:

```powershell
Rscript scripts/qa_cheatsheet_audit.R
```

Reported result:

- `Issues found: 0`

Final default-policy spot check:

- all 9 runtime checks passed.

Local process note: the successful `devtools::test()`, cheat-sheet audit, and policy spot-check commands still returned nonzero process exits after reporting clean output. This matches the local Rscript exit anomaly recorded throughout the audit.

## Non-Blocking Check Result

The optional final `devtools::check("StatsPackage -1.0", document = FALSE, manual = FALSE)` retry remains environment-blocked locally.

Observed failure:

- location: `R CMD build`, while rebuilding `vignettes/getting-started.Rmd`;
- status: `Rcmd.exe` exit status `-1073741569`;
- vignette rebuild text shows `getting-started.Rmd` finished rebuilding before the subprocess failure surfaced.

This is consistent with the Phase 0 local Windows R / Rscript / `Rcmd.exe` anomaly. It is not counted as a package-code failure unless reproduced in CI or another clean R environment.

## Final Recommendation

The audit branch is ready for human review. Before merging release-facing changes, run `R CMD check` or `devtools::check()` in CI or another clean R environment to rule out the local Windows subprocess anomaly.

Subject to clean-environment package check confirmation, the audited package is mathematically ready for merge.
