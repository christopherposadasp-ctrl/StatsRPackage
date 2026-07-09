# QA and Release Workflow

This workflow is the baseline quality process for StatsPackage.

## 1) Fast Development Cycle

Run from repository root:

```r
devtools::load_all("StatsPackage")
devtools::test("StatsPackage")
```

Use this loop during active development to catch regressions quickly.

## 2) Full Package Validation

Run before merging larger changes or preparing a release:

```r
devtools::document("StatsPackage")
devtools::check("StatsPackage")
```

Expectation:
- no test failures
- no check errors, warnings, or notes

## 3) Cheat-Sheet Regression Checks

### Direct run

```bash
Rscript 2.CheatsheetV8.r
```

Run both supported cheat sheets directly:

```bash
Rscript 2.CheatsheetV9_Narrow.r
```

These runs validate current example behavior and printed summaries.

### Structured audit run

```bash
Rscript scripts/qa_cheatsheet_audit.R
```

The audit runner:
- installs the exact local package source into an isolated temporary library
- runs both V8 and V9 cheat-sheet expressions in sequence
- logs full output
- writes a CSV of warnings/errors for triage
- exits unsuccessfully if any warning or error is recorded

Expected outputs:
- `qa/cheatsheet_audit_output.txt`
- `qa/cheatsheet_audit_issues.csv`

### Release smoke test

```bash
Rscript scripts/qa_smoke_release.R
```

The smoke runner:
- unconditionally installs the exact local source into an isolated temporary library
- validates high-risk function families with fast structural checks
- confirms default-policy-sensitive behavior (for example nonzero-null proportion defaults)
- writes pass/fail artifacts for quick deployment triage

Expected outputs:
- `qa/smoke_test_output.txt`
- `qa/smoke_test_results.csv`

## 4) Triage Priorities

When issues occur, prioritize fixes in this order:
1. mathematical correctness
2. null/alternative interpretation and rejection-region correctness
3. API consistency and object shape
4. printed-output polish

## 5) Release Readiness Checklist

Before a tagged release:
1. `devtools::test("StatsPackage")` passes
2. `devtools::check("StatsPackage")` passes cleanly
3. cheat-sheet audit reports no warnings or errors for V8 and V9
4. defaults and documentation are aligned
5. version and release notes are updated

## 6) GitHub Actions Automation

Five workflows enforce this process in CI/CD:

- `.github/workflows/ci.yml`
  - runs on pushes and pull requests to `main`
  - matrix coverage:
    - `ubuntu-latest` with `R-release`
    - `ubuntu-latest` with `R-devel`
    - `windows-latest` with `R-release`
    - `macos-latest` with `R-release`
  - runs tests, a note-clean package check, and both cheat-sheet audits
  - uploads audit artifacts

- `.github/workflows/release.yml`
  - runs on pushed tags matching `*.*.*` (for example `1.1.1`)
  - verifies the tag exactly matches the package version
  - reruns tests, note-clean check, and audit
  - builds release artifacts in `dist/`
  - publishes a GitHub Release with built assets and audit outputs

- `.github/workflows/pkgdown.yml`
  - runs on pushes to `main` and manual dispatch
  - builds and deploys pkgdown site from `StatsPackage/`

- `.github/workflows/lint.yml`
  - runs `lintr` checks on package R source
  - intended as style/static-analysis guard

- `.github/workflows/coverage.yml`
  - computes test coverage with `covr`
  - enforces at least 80% line coverage in CI (`COVERAGE_THRESHOLD`)
  - uploads coverage summary artifacts

When coverage fails, inspect `qa/coverage-summary.txt` and the workflow's
Cobertura artifact, identify release-critical untested branches, and add focused
behavioral tests. Do not lower the threshold or exclude source merely to make
the gate pass.

## 7) Branch Protection

Protect `main` so merges require passing checks from the CI matrix.

- setup guide: `docs/BRANCH_PROTECTION.md`
- recommended solo-maintainer enforcement:
  - require pull requests
  - require conversation resolution
  - require linear history
  - disallow force pushes and deletions
- require one approval when a second maintainer or reviewer is consistently available


