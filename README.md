# StatsRPackage

StatsRPackage is the repository for `StatsPackage`, an R package focused on introductory statistical inference and design.

[![CI](https://github.com/christopherposadasp-ctrl/StatsRPackage/actions/workflows/ci.yml/badge.svg)](https://github.com/christopherposadasp-ctrl/StatsRPackage/actions/workflows/ci.yml)
[![Release](https://github.com/christopherposadasp-ctrl/StatsRPackage/actions/workflows/release.yml/badge.svg)](https://github.com/christopherposadasp-ctrl/StatsRPackage/actions/workflows/release.yml)
[![Pkgdown](https://github.com/christopherposadasp-ctrl/StatsRPackage/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/christopherposadasp-ctrl/StatsRPackage/actions/workflows/pkgdown.yml)
[![Lint](https://github.com/christopherposadasp-ctrl/StatsRPackage/actions/workflows/lint.yml/badge.svg)](https://github.com/christopherposadasp-ctrl/StatsRPackage/actions/workflows/lint.yml)
[![Coverage](https://github.com/christopherposadasp-ctrl/StatsRPackage/actions/workflows/coverage.yml/badge.svg)](https://github.com/christopherposadasp-ctrl/StatsRPackage/actions/workflows/coverage.yml)

The project priorities are:
- mathematical correctness
- consistent APIs across function families
- reusable workflows beyond a single assignment
- regression-tested behavior

Current package version: `0.1.4` (post-`0.1.0` stabilization)

## Repository Layout

| Path | Purpose |
| --- | --- |
| `StatsPackage -1.0/` | R package source (`R/`, `man/`, `tests/`, `DESCRIPTION`) |
| `2.CheatsheetV7.r` | End-to-end usage and output regression script |
| `scripts/qa_cheatsheet_audit.R` | Structured cheat-sheet audit runner |
| `docs/` | Project brief and supporting documentation |

## Install and Load

Install from this local repository:

```r
devtools::install("StatsPackage -1.0")
library(StatsPackage)
```

Install from GitHub (canonical classroom deployment command, pinned to `0.1.4`):

```r
remotes::install_github(
  "christopherposadasp-ctrl/StatsRPackage",
  subdir = "StatsPackage -1.0",
  ref = "0.1.4"
)
library(StatsPackage)
```

## Quickstart

```r
devtools::load_all("StatsPackage -1.0")
devtools::test("StatsPackage -1.0")
source("2.CheatsheetV7.r")
```

## API and Defaults

- API index by function family: [docs/API_INDEX.md](docs/API_INDEX.md)
- Default behavior policies: [docs/DEFAULT_POLICIES.md](docs/DEFAULT_POLICIES.md)
- QA and release workflow: [docs/QA_WORKFLOW.md](docs/QA_WORKFLOW.md)
- Changelog: [StatsPackage -1.0/NEWS.md](StatsPackage%20-1.0/NEWS.md)

## Site and Vignettes

- Pkgdown site: [christopherposadasp-ctrl.github.io/StatsRPackage](https://christopherposadasp-ctrl.github.io/StatsRPackage/)
- Getting started vignette source: [getting-started.Rmd](StatsPackage%20-1.0/vignettes/getting-started.Rmd)

## Quality Gates

Minimum local quality checks:

```r
devtools::test("StatsPackage -1.0")
devtools::check("StatsPackage -1.0")
lintr::lint_dir("StatsPackage -1.0/R")
```

Cheat-sheet regression audit:

```bash
Rscript scripts/qa_cheatsheet_audit.R
```

Release smoke test:

```bash
Rscript scripts/qa_smoke_release.R
```

The package design assumes:
- `quiet = FALSE` prints a readable summary
- `quiet = TRUE` suppresses print output
- functions return invisible, classed result objects with computational details

## Documentation Map

- Project brief: [docs/StatsPackage_Project_Brief.txt](docs/StatsPackage_Project_Brief.txt)
- 1.0 scope: [docs/1.0_SCOPE.md](docs/1.0_SCOPE.md)
- Deployment checklist: [docs/DEPLOYMENT_CHECKLIST.md](docs/DEPLOYMENT_CHECKLIST.md)
- API index: [docs/API_INDEX.md](docs/API_INDEX.md)
- Default policies: [docs/DEFAULT_POLICIES.md](docs/DEFAULT_POLICIES.md)
- QA and release workflow: [docs/QA_WORKFLOW.md](docs/QA_WORKFLOW.md)
- Branch protection setup: [docs/BRANCH_PROTECTION.md](docs/BRANCH_PROTECTION.md)
- Contributing guide: [CONTRIBUTING.md](CONTRIBUTING.md)

