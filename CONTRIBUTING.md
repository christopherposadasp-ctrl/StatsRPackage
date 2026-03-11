# Contributing to StatsRPackage

Thanks for contributing.

## Scope

This repository contains:
- the R package source in `StatsPackage -1.0/`
- QA scripts and documentation at repository root

## Development setup

Use R (release) and install development dependencies:

```r
install.packages(c("devtools", "testthat", "lintr", "covr", "pkgdown"))
```

From repository root:

```r
devtools::load_all("StatsPackage -1.0")
devtools::test("StatsPackage -1.0")
devtools::check("StatsPackage -1.0")
```

Optional regression run:

```r
source("2.CheatsheetV4.r")
```

## Branch and PR workflow

- branch from `main`
- keep changes scoped and focused
- open a pull request to `main`
- ensure required CI checks pass before requesting merge

PRs should include:
- a short problem statement
- summary of approach
- testing notes (tests/check/audit run)
- documentation updates when behavior changes

## Quality expectations

Required:
- no regressions in mathematical behavior
- tests pass
- package check passes
- docs and defaults stay aligned

When changing defaults or formulas, update:
- tests
- `2.CheatsheetV4.r`
- `docs/DEFAULT_POLICIES.md`
- changelog (`StatsPackage -1.0/NEWS.md`)

## Commit guidance

Use descriptive, imperative commit messages.

Examples:
- `Fix pooled proportion default for nonzero null`
- `Add chi-square sparse-cell test coverage`

## Release notes

For release-affecting changes, add a bullet under the current unreleased or next release section in:
- `StatsPackage -1.0/NEWS.md`

