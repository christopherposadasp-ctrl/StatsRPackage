# Mathematical Correctness and Efficiency Audit Baseline

Baseline date: 2026-05-08

Branch: `codex/math-efficiency-audit`

Baseline commit: `a8ba040` (`Add mathematical correctness and efficiency audit plan`)

Audit baseline: StatsPackage 1.1.0 on main, after the `pi_mu()` release and
pkgdown workflow fix.

## Package State

- Package version: `1.1.0`
- Published release tag checked but not altered: `1.1.0`
- `1.1.0` tag ref observed during baseline setup:
  `e64d2f70390febdea21146bc868b4c6934b211ac`
- Exported function count from `StatsPackage/NAMESPACE`: 29
- Newest exported API from `StatsPackage/NEWS.md`: `pi_mu()`
- `pi_mu()` should receive extra scrutiny during the audit because it is the
  newest prediction-interval API.

## Baseline QA

- `devtools::test("StatsPackage")`: 406 passing assertions by reported
  test output.
- `scripts/qa_cheatsheet_audit.R`: 154 cheat-sheet expressions evaluated with
  0 recorded issues.
- `devtools::check("StatsPackage")`: environment-blocked on this Windows
  workstation during `R CMD build` / vignette rebuild.

Observed `devtools::check()` blocker:

- R version used for the latest rerun:
  `R version 4.6.0 (2026-04-24 ucrt)`
- R library path used for the latest rerun:
  `C:/Users/posad/AppData/Local/StatsPackageAudit/Rlib-4.6.0`
- Failure location: `R CMD build`, while rebuilding
  `vignettes/getting-started.Rmd`
- Failure status: `Rcmd.exe` exit status `-1073741569`
- Additional note from `devtools::check()`: installed `roxygen2` version
  `8.0.0` does not match declared `RoxygenNote: 7.3.3`; check did not
  re-document the package.

The VS Code R configuration was updated from the stale `R-4.5.2` path to
`R-4.6.0`, and `jsonlite` loads under the configured R. The package check
blocker persisted after that fix, so it should be treated as a local
environment/subprocess issue unless reproduced in CI or on another clean
machine.

## Working Tree Notes

- Package code, tests, generated Rd files, and release tags were not edited
  during Phase 0.
- Current untracked file observed outside the audit baseline:
  `2.CheatsheetV9_Narrow.r`

## Baseline Conclusion

Phase 0 is sufficient to begin the source/API inventory and mathematical
hotspot triage. The full `R CMD check` result should be revalidated later in CI
or on another clean machine before release-facing audit changes are accepted.
