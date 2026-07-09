# StatsRPackage

StatsRPackage is the repository for `StatsPackage`, an R package focused on introductory statistical inference, prediction intervals, and design.

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

Current package version: `1.1.1` (stable API plus numerical and release hardening)

## Repository Layout

| Path | Purpose |
| --- | --- |
| `StatsPackage/` | R package source (`R/`, `man/`, `tests/`, `DESCRIPTION`) |
| `2.CheatsheetV8.r` | Full API usage and output regression script |
| `2.CheatsheetV9_Narrow.r` | Focused classroom workflow cheat sheet |
| `scripts/qa_cheatsheet_audit.R` | Structured cheat-sheet audit runner |
| `docs/` | Project brief and supporting documentation |

## Install and Load

Install from this local repository:

```r
devtools::install("StatsPackage")
library(StatsPackage)
```

Install from GitHub (canonical classroom deployment command, pinned to `1.1.1`):

```r
remotes::install_github(
  "christopherposadasp-ctrl/StatsRPackage",
  subdir = "StatsPackage",
  ref = "1.1.1"
)
library(StatsPackage)
```

## Quickstart

```r
devtools::load_all("StatsPackage")
devtools::test("StatsPackage")
source("2.CheatsheetV8.r")
# Or run the focused classroom workflows:
source("2.CheatsheetV9_Narrow.r")
```

## API and Defaults

- API index by function family: [docs/API_INDEX.md](docs/API_INDEX.md)
- Default behavior policies: [docs/DEFAULT_POLICIES.md](docs/DEFAULT_POLICIES.md)
- QA and release workflow: [docs/QA_WORKFLOW.md](docs/QA_WORKFLOW.md)
- Repository audit remediation: [docs/REPOSITORY_AUDIT_REMEDIATION.md](docs/REPOSITORY_AUDIT_REMEDIATION.md)
- Changelog: [StatsPackage/NEWS.md](StatsPackage/NEWS.md)
- Security policy: [SECURITY.md](SECURITY.md)
- Citation metadata: [CITATION.cff](CITATION.cff)

## Site and Vignettes

- Pkgdown site: [christopherposadasp-ctrl.github.io/StatsRPackage](https://christopherposadasp-ctrl.github.io/StatsRPackage/)
- Getting started vignette source: [getting-started.Rmd](StatsPackage/vignettes/getting-started.Rmd)

## Quality Gates

Minimum local quality checks:

```r
devtools::test("StatsPackage")
devtools::check("StatsPackage")
lintr::lint_dir("StatsPackage/R")
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


