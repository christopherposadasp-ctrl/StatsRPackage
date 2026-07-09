# Contributing to StatsRPackage

Thanks for contributing.

## Scope

This repository contains:
- the R package source in `StatsPackage/`
- QA scripts and documentation at repository root

## Development setup

Use R (release) and install development dependencies:

```r
install.packages(c("devtools", "testthat", "lintr", "covr", "pkgdown"))
```

From repository root:

```r
devtools::load_all("StatsPackage")
devtools::test("StatsPackage")
devtools::check("StatsPackage")
```

Optional regression run:

```r
source("2.CheatsheetV8.r")
source("2.CheatsheetV9_Narrow.r")
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
- package check passes without errors, warnings, or notes
- source files pass the configured `lintr` profile
- docs and defaults stay aligned

The lint profile keeps the established public dotted argument names (for
example, `conf.level`) and existing continuation indentation. It still enforces
the remaining default static checks, a 120-character line limit, trailing
whitespace/newline checks, and a 50-character object-name limit across source
and tests.

When changing defaults or formulas, update:
- tests
- `2.CheatsheetV8.r`
- `docs/DEFAULT_POLICIES.md`
- changelog (`StatsPackage/NEWS.md`)

## Commit guidance

Use descriptive, imperative commit messages.

Examples:
- `Fix pooled proportion default for nonzero null`
- `Add chi-square sparse-cell test coverage`

## Release notes

For release-affecting changes, add a bullet under the current unreleased or next release section in:
- `StatsPackage/NEWS.md`


