# Repository Audit Remediation

Audit date: 2026-07-09

Baseline: `main` at `1344894`

Release target: `1.1.1`

## Resolved Findings

| Area | Remediation |
| --- | --- |
| Release drift | Prepared `1.1.1` to publish the mathematical fixes already merged after `1.1.0`. |
| QA false positives | Both QA runners install the exact local source in isolated libraries; the cheat-sheet audit exits unsuccessfully on every recorded warning or error. |
| Package check notes | Excluded development-only `.lintr` and `_pkgdown.yml` files from source packages and made CI fail on notes. |
| API examples | Added fast, deterministic examples covering all 29 exported functions and regenerated all manuals. |
| Cheat sheets | Repaired V8 text encoding and added V9 to the structured audit and release artifacts. |
| Workflow security | Pinned every action to an immutable commit, disabled persisted checkout credentials, and added weekly Dependabot updates. |
| Release integrity | Added a release gate requiring the pushed tag to equal the `DESCRIPTION` version. |
| Coverage | Raised the enforced line-coverage threshold from 65% to 80%. |
| Lint | Removed the test exclusion and restored default linters except the documented public-name and continuation-indentation exceptions. |
| Layout | Renamed the package directory to `StatsPackage/` and `chisq-helpers.R.R` to `chisq-helpers.R`; updated all repository references. |
| Governance | Aligned branch-protection guidance with a solo-maintainer workflow and retained mandatory status checks. |
| Project policy | Added `SECURITY.md` and `CITATION.cff`. |

## Local Verification

- package tests: all contexts passed with zero failures
- runnable documentation examples: all 29 exports executed
- lintr: zero findings
- line coverage: 80.4247% (2,083 of 2,590 measured lines)
- cheat-sheet audit: V8 and V9, zero warnings or errors
- release smoke test: 7 passed, 0 failed
- source build without vignette rebuilding: passed; development-only files were absent from the tarball

The local Windows R 4.6.0 installation still exits with status
`-1073741569` after successfully rebuilding the vignette or finishing commands
that load selected development packages. This occurs after successful work and
is treated as a workstation/toolchain defect. The hosted Linux, Windows, and
macOS package-check matrix is the release authority for the complete vignette
build and note-clean `R CMD check`.

## Hosted Validation

Pending pull-request CI. Release publication and repository housekeeping occur
only after all required checks pass.
