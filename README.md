# StatsRPackage

StatsRPackage is the repository for `StatsPackage`, an R package focused on introductory statistical inference and design.

The project priorities are:
- mathematical correctness
- consistent APIs across function families
- reusable workflows beyond a single assignment
- regression-tested behavior

Current package version: `0.0.1.0000` (development)

## Repository Layout

| Path | Purpose |
| --- | --- |
| `StatsPackage -1.0/` | R package source (`R/`, `man/`, `tests/`, `DESCRIPTION`) |
| `2.CheatsheetV4.r` | End-to-end usage and output regression script |
| `scripts/qa_cheatsheet_audit.R` | Structured cheat-sheet audit runner |
| `docs/` | Project brief and supporting documentation |

## Install and Load

Install from this local repository:

```r
devtools::install("StatsPackage -1.0")
library(StatsPackage)
```

Install from GitHub:

```r
remotes::install_github(
  "christopherposadasp-ctrl/StatsRPackage",
  subdir = "StatsPackage -1.0"
)
library(StatsPackage)
```

## Quickstart

```r
devtools::load_all("StatsPackage -1.0")
devtools::test("StatsPackage -1.0")
source("2.CheatsheetV4.r")
```

## API and Defaults

- API index by function family: [docs/API_INDEX.md](docs/API_INDEX.md)
- Default behavior policies: [docs/DEFAULT_POLICIES.md](docs/DEFAULT_POLICIES.md)
- QA and release workflow: [docs/QA_WORKFLOW.md](docs/QA_WORKFLOW.md)

## Quality Gates

Minimum local quality checks:

```r
devtools::test("StatsPackage -1.0")
devtools::check("StatsPackage -1.0")
```

Cheat-sheet regression audit:

```bash
Rscript scripts/qa_cheatsheet_audit.R
```

The package design assumes:
- `quiet = FALSE` prints a readable summary
- `quiet = TRUE` suppresses print output
- functions return invisible, classed result objects with computational details

## Documentation Map

- Project brief: [docs/StatsPackage_Project_Brief.txt](docs/StatsPackage_Project_Brief.txt)
- API index: [docs/API_INDEX.md](docs/API_INDEX.md)
- Default policies: [docs/DEFAULT_POLICIES.md](docs/DEFAULT_POLICIES.md)
- QA and release workflow: [docs/QA_WORKFLOW.md](docs/QA_WORKFLOW.md)

