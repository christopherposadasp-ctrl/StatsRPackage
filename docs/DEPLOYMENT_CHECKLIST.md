# Deployment Checklist (Classroom)

Use this checklist when deploying `StatsPackage` to classmates.

Current deployment target: `0.1.3`

Canonical install command:

```r
remotes::install_github(
  "christopherposadasp-ctrl/StatsRPackage",
  subdir = "StatsPackage -1.0",
  ref = "0.1.3"
)
library(StatsPackage)
```

## 1) Preflight (Before Announcing)

1. Confirm latest `main` checks are green in GitHub Actions:
   - `CI`
   - `Lint`
   - `Coverage`
   - `Pkgdown`
2. Confirm release tag `0.1.3` exists and is published (not draft/prerelease).
3. Confirm README install snippet is pinned to `ref = "0.1.3"`.
4. Confirm no uncommitted release-critical local changes.

## 2) GitHub Click Path

1. Open [StatsRPackage Releases](https://github.com/christopherposadasp-ctrl/StatsRPackage/releases).
2. Open release `0.1.3`.
3. Verify:
   - tag: `0.1.3`
   - published status (not draft)
   - assets present
4. Open [repository README](https://github.com/christopherposadasp-ctrl/StatsRPackage#readme).
5. Copy the pinned install command from the `Install from GitHub` section.

## 3) Classroom Validation Snippet

Have at least one clean machine/session run:

```r
install.packages("remotes")
remotes::install_github(
  "christopherposadasp-ctrl/StatsRPackage",
  subdir = "StatsPackage -1.0",
  ref = "0.1.3"
)
library(StatsPackage)
ci_mu(xbar = 12.4, n = 15, s = 3.2, quiet = TRUE)
```

Then run the lightweight smoke suite from repository root:

```bash
Rscript scripts/qa_smoke_release.R
```

Expected:
- package installs and loads without error
- smoke checks pass across CI/test/default-policy-sensitive functions
- `qa/smoke_test_results.csv` is written with `PASS` statuses

## 4) Rollback / Hotfix Plan

If a deployment issue is found:

1. Stop sharing the broken command/message immediately.
2. Do not retag or rewrite `0.1.3`.
3. Create a hotfix commit on `main`.
4. Bump package version and notes for the next patch release (for example `0.1.4`).
5. Push new tag and let Release workflow publish it.
6. Update README pinned `ref` to the new patch tag.
7. Send corrected classroom install command.

## 5) Post-Deploy Monitoring (First 24 Hours)

1. Watch GitHub Issues for install/runtime reports.
2. If multiple students report the same failure, cut a patch release rather than giving one-off workaround steps.
