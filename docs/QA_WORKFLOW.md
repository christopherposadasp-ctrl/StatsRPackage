# QA and Release Workflow

This workflow is the baseline quality process for StatsPackage.

## 1) Fast Development Cycle

Run from repository root:

```r
devtools::load_all("StatsPackage -1.0")
devtools::test("StatsPackage -1.0")
```

Use this loop during active development to catch regressions quickly.

## 2) Full Package Validation

Run before merging larger changes or preparing a release:

```r
devtools::document("StatsPackage -1.0")
devtools::check("StatsPackage -1.0")
```

Expectation:
- no test failures
- no check errors
- warnings and notes are reviewed intentionally

## 3) Cheat-Sheet Regression Checks

### Direct run

```bash
Rscript 2.CheatsheetV4.r
```

This is the fastest way to validate current example behavior and printed summaries.

### Structured audit run

```bash
Rscript scripts/qa_cheatsheet_audit.R
```

The audit runner:
- installs the package from `StatsPackage -1.0`
- runs cheat-sheet expressions in sequence
- logs full output
- writes a CSV of warnings/errors for triage

Expected outputs:
- `qa/cheatsheet_audit_output.txt`
- `qa/cheatsheet_audit_issues.csv`

## 4) Triage Priorities

When issues occur, prioritize fixes in this order:
1. mathematical correctness
2. null/alternative interpretation and rejection-region correctness
3. API consistency and object shape
4. printed-output polish

## 5) Release Readiness Checklist

Before a tagged release:
1. `devtools::test("StatsPackage -1.0")` passes
2. `devtools::check("StatsPackage -1.0")` passes cleanly
3. cheat-sheet audit reports no errors
4. defaults and documentation are aligned
5. version and release notes are updated

## 6) GitHub Actions Automation

Two workflows enforce this process in CI/CD:

- `.github/workflows/ci.yml`
  - runs on pushes and pull requests to `main`
  - matrix coverage:
    - `ubuntu-latest` with `R-release`
    - `ubuntu-latest` with `R-devel`
    - `windows-latest` with `R-release`
    - `macos-latest` with `R-release`
  - runs tests, package check, and cheat-sheet audit
  - uploads audit artifacts

- `.github/workflows/release.yml`
  - runs on pushed tags matching `*.*.*` (for example `0.1.0`)
  - reruns tests, check, and audit
  - builds release artifacts in `dist/`
  - publishes a GitHub Release with built assets and audit outputs

- `.github/workflows/pkgdown.yml`
  - runs on pushes to `main` and manual dispatch
  - builds and deploys pkgdown site from `StatsPackage -1.0/`

## 7) Branch Protection

Protect `main` so merges require passing checks from the CI matrix.

- setup guide: `docs/BRANCH_PROTECTION.md`
- recommended enforcement:
  - require pull requests
  - require at least one approval
  - require conversation resolution
  - require linear history
  - disallow force pushes and deletions
