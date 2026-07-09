# Phase 10 Efficiency Baseline And Static Inventory

Baseline date: 2026-05-08

Branch: `codex/code-efficiency-audit`

Starting commit: `e240ae9` (`Merge pull request #13 from codex/update-audit-plan-efficiency-roadmap`)

Package version: `1.1.0`

Exported function count: `29`

Published release tag checked and not altered: `1.1.0` at `e64d2f70390febdea21146bc868b4c6934b211ac`

## Summary

Phase 10 is a read-only baseline for the follow-up code-efficiency audit. No
production R code, tests, generated docs, README, vignette, cheat sheet, release
tags, or public API behavior were changed.

The static inventory found plausible efficiency and simplification candidates,
but no immediate efficiency blocker that should bypass the planned Phase 11
profiling step.

## Baseline QA

Full test suite command:

```powershell
$env:R_LIBS_USER="$env:LOCALAPPDATA\StatsPackageAudit\Rlib-4.6.0"
& "C:\Program Files (x86)\R\R-4.6.0\bin\x64\Rscript.exe" --vanilla -e ".libPaths(Sys.getenv('R_LIBS_USER')); devtools::test('StatsPackage')"
```

Reported result:

- `FAIL 0 | WARN 0 | SKIP 0 | PASS 628`

Cheat-sheet audit command:

```powershell
$env:R_LIBS_USER="$env:LOCALAPPDATA\StatsPackageAudit\Rlib-4.6.0"
& "C:\Program Files (x86)\R\R-4.6.0\bin\x64\Rscript.exe" --vanilla -e ".libPaths(Sys.getenv('R_LIBS_USER')); source('scripts/qa_cheatsheet_audit.R')"
```

Reported result:

- `Issues found: 0`

Local process note: both commands returned a nonzero process status after
reporting clean output. This matches the local Rscript / `Rcmd.exe` anomaly
recorded in the mathematical audit and is not counted as a package-code failure.
Local `devtools::check()` was not rerun in Phase 10.

## Files Reviewed

Primary review targets:

- `StatsPackage/R/n-required-functions.R`
- `StatsPackage/R/power-functions.R`
- `StatsPackage/R/hypothesis-test-functions.R`
- `StatsPackage/R/chisq-functions.R`

Helper files reviewed for existing support and possible local simplification:

- `StatsPackage/R/ht-helpers.R`
- `StatsPackage/R/power-helpers.R`
- `StatsPackage/R/chisq-helpers.R.R`
- `StatsPackage/R/ci-helpers.R`
- `StatsPackage/R/width-helpers.R`

Lightweight static metrics:

| File | Approx. lines | Function definitions | `cat()` calls | Search loops |
| --- | ---: | ---: | ---: | ---: |
| `n-required-functions.R` | 1983 | 14 | 77 | 6 `while` |
| `power-functions.R` | 1056 | 5 | 67 | 0 |
| `hypothesis-test-functions.R` | 931 | 4 | 67 | 0 |
| `chisq-functions.R` | 696 | 4 | 29 | 0 |
| `ht-helpers.R` | 265 | 17 | 0 | 0 |
| `power-helpers.R` | 91 | 8 | 0 | 0 |
| `chisq-helpers.R.R` | 263 | 15 | 0 | 2 `for`, 1 `repeat` |

## Static Inventory Findings

### Required sample size family

`n-required-functions.R` is the largest reviewed file and the main candidate
for Phase 11 profiling.

Observed patterns:

- `.nreq_find_min_n()` already uses a good structure: validation, cached power
  evaluations, doubling bracket search, binary search, and previous-`n`
  minimality verification.
- Required-n wrappers repeatedly define small `power_at_n()` closures, call
  `.nreq_find_min_n()`, then call the corresponding `power_*()` function again
  to build the returned result fields.
- Two-sample wrappers repeatedly compute `n2 = ceiling(n_ratio * n1)` with
  family-specific lower bounds.
- One-sided `n_required_z_mu()` branches use closed-form starts plus forward
  correction and backward minimality checks. These are correct but contain the
  only additional wrapper-level search loops outside `.nreq_find_min_n()`.
- Printed summaries repeat the same target-power, target-beta, required-n, and
  achieved-power lines across branches.

Candidate hotspots:

- hard target-power searches near `n_max`;
- two-sample searches with non-1 `n_ratio`;
- one-sided z closed-form correction loops;
- repeated final `power_*()` calls after solver completion.

Constraints for later phases:

- Preserve returned `n`, `n1`, `n2`, `achieved_power`, `achieved_beta`,
  `n_is_minimal`, `n_evaluations`, and allocation semantics.
- Do not weaken minimality guarantees for speed.

### Power and hypothesis-test families

`power-functions.R` and `hypothesis-test-functions.R` have repeated
alternative-tail and critical-region branches.

Observed patterns:

- z, t, proportion, chi-square, and F branches each repeat the same broad shape:
  validate inputs, compute SE/statistic or noncentrality, choose tail-specific
  critical values, build a classed result, then print a family-specific summary.
- `.ht_build_region()` and `.ht_build_region_nonsymmetric()` already centralize
  rejection-region text, limiting the value of broader helper extraction.
- `power-helpers.R` already centralizes normal lower, upper, and two-sided
  probability calculations and preserves the Phase 8 upper-tail stability fix.
- Noncentral t power branches are repeated for paired, one-sample, and
  two-sample cases in `power_t_mu()`.
- Printing code accounts for most visible repetition, but printed text is a
  public contract and should not be touched without a strong reason.

Candidate hotspots:

- high-volume repeated calls to `power_z_mu()`, `power_t_mu()`, `power_p_z()`,
  `power_var_chisq()`, and `power_var_ratio_F()` through required-n wrappers;
- repeated noncentral t branch logic in `power_t_mu()`;
- result construction overhead only if profiling shows high-volume direct calls
  spend meaningful time there.

Constraints for later phases:

- Preserve result fields, class names, printed-output templates, and default
  policy behavior.
- Tail-helper consolidation is only worth implementing if it reduces production
  code length or measured repeated-computation cost.

### Chi-square and categorical family

`chisq-functions.R` and `chisq-helpers.R.R` are the main non-required-n
profiling candidates.

Observed patterns:

- `chisq_gof_dist()` has the broadest branch surface: raw vs grouped inputs,
  continuous vs Poisson distributions, estimated vs fixed parameters, optional
  breaks, and expected-count combining.
- Raw continuous GOF creates bins with `cut()` and `table()`, then computes
  expected counts with CDF differences.
- `.combine_sparse_adjacent()` appends to vectors and lists inside a loop. This
  is simple and readable, but may allocate repeatedly for many classes.
- `.combine_pois_right_tail()` uses a `repeat` loop and recomputes independent
  Poisson expected counts while moving the tail boundary.
- Printed observed/expected tables repeat construction in multiple functions.

Candidate hotspots:

- `chisq_gof_dist()` on large raw continuous inputs with omitted `breaks`;
- sparse continuous GOF where many adjacent classes combine;
- Poisson GOF with a long right tail and small expected-count threshold.

Constraints for later phases:

- Preserve labels, `group_map`, initial observed/expected fields, combined
  flags, notes, warnings, degrees of freedom, and expected-count values.

## Rejected Cosmetic-Only Refactors

These should not be pursued without profiling evidence or a behavior-preserving
net reduction in production code:

- broad print-helper extraction for existing `cat()` blocks;
- helper extraction that only moves branch-specific result-list fields around;
- renaming or reorganizing helper files;
- whitespace-only indentation cleanup in previously audited functions;
- changing printed text, aliases, or field names to make code shorter.

## Candidate List For Phase 11 Profiling

Recommended profiling order:

1. Required-n searches:
   - `n_required_t_mu()` one-sample and two-sample Welch with high target power;
   - `n_required_p_z()` two-sample with `continuity = TRUE`;
   - `n_required_var_ratio_F()` with non-1 `n_ratio`;
   - `n_required_z_mu()` one-sided closed-form branches near rounding
     boundaries.
2. Power functions called directly in loops:
   - `power_t_mu()` paired, one-sample, Welch, and pooled paths;
   - `power_p_z()` one-sample, pooled two-sample, unpooled two-sample, and
     continuity-corrected two-sample paths.
3. Chi-square GOF:
   - `chisq_gof_dist()` raw normal/exponential/uniform inputs with omitted
     `breaks`;
   - `chisq_gof_dist()` grouped sparse continuous inputs;
   - `chisq_gof_dist()` Poisson inputs with long right tails.
4. High-volume object construction:
   - repeated `quiet = TRUE` calls for representative hypothesis-test and power
     functions to decide whether result-list construction is measurable.

Use base R profiling only: `system.time()`, `Rprof()`, and `summaryRprof()`.
Do not add profiling dependencies or commit scratch scripts.

## Phase 10 Conclusion

Phase 10 baseline setup and static inventory are complete. The package remains
at 29 exports, baseline QA reports no package failures, and no release tags were
altered.

The next phase should be profiling only. Production edits should wait until
Phase 12 and should be limited to changes supported by Phase 11 timing evidence
or clear net simplification without public-contract drift.
