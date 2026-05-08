# Mathematical Correctness And Code Efficiency Audit Plan

This document is the living audit roadmap for `StatsPackage`, an R package for
introductory statistical inference and design workflows.

## Current State

The first mathematical correctness and stability audit is complete and merged.

- Merged PR: `#12` (`Mathematical correctness and efficiency audit`)
- Merge commit on `main`: `9b647bb`
- Final audit branch commit before merge: `14d73d1`
- Package version: `1.1.0`
- Published release tag checked and not altered: `1.1.0`
- Exported function count: `29`
- Final local full test output: `FAIL 0 | WARN 0 | SKIP 0 | PASS 628`
- Final cheat-sheet audit output: `Issues found: 0`
- Clean GitHub Actions result for PR `#12`: `CI`, `Coverage`, and `Lint` all
  passed
- `devtools::check()` passed in clean CI on:
  - `ubuntu-latest / R-release`
  - `ubuntu-latest / R-devel`
  - `windows-latest / R-release`
  - `macos-latest / R-release`
- Local Windows `Rcmd.exe` / Rscript exit status `-1073741569` remains a local
  environment anomaly because it did not reproduce in CI.

The completed audit found no mathematical formula defects. One production-code
change was made: upper-tail probability calculations now use explicit
`lower.tail = FALSE` calls where that improves extreme-tail numerical
stability.

## Completed Audit Goals

The completed Phase 0-9 audit established and preserved:

- the 29-function public API;
- exported function names, argument names, defaults, result classes, and result
  fields;
- `quiet`, `digits`, `alpha`, and `conf.level` conventions;
- unrounded stored computational values;
- printed-output templates, except for mathematically equivalent values caused
  by the intentional upper-tail stability fix;
- documented default policies in `docs/DEFAULT_POLICIES.md`;
- user-facing API coverage in `docs/API_INDEX.md`, generated `.Rd` aliases,
  the cheat sheet, README links, and the getting-started vignette;
- release tag integrity for `1.1.0`.

Completed findings are recorded in:

- `docs/MATH_EFFICIENCY_AUDIT_BASELINE.md`
- `docs/MATH_EFFICIENCY_PHASE1_FINDINGS.md`
- `docs/MATH_EFFICIENCY_PHASE2_PI_MU_FINDINGS.md`
- `docs/MATH_EFFICIENCY_PHASE2_CONFIDENCE_WIDTH_FINDINGS.md`
- `docs/MATH_EFFICIENCY_PHASE3_HYPOTHESIS_FINDINGS.md`
- `docs/MATH_EFFICIENCY_PHASE4_POWER_FINDINGS.md`
- `docs/MATH_EFFICIENCY_PHASE5_N_REQUIRED_FINDINGS.md`
- `docs/MATH_EFFICIENCY_PHASE6_CHISQ_FINDINGS.md`
- `docs/MATH_EFFICIENCY_PHASE7_SHAPE_FINDINGS.md`
- `docs/MATH_EFFICIENCY_PHASE8_EFFICIENCY_FINDINGS.md`
- `docs/MATH_EFFICIENCY_AUDIT_FINAL_REPORT.md`

## Standing Constraints

These constraints apply to any follow-up audit work:

- Work on a non-`main` branch.
- Do not retag, rewrite, or otherwise alter the published `1.1.0` release.
- Preserve public API behavior unless a confirmed mathematical or numerical
  correctness issue requires a change.
- Preserve printed summaries and invisible classed return objects.
- Outside tests, production code should not grow unless the change fixes a
  confirmed mathematical, numerical-stability, runtime, or memory issue.
- Prefer local simplification, removal of duplication, and clearer existing
  logic over broad new abstraction.
- Do not make formatting-only or style-only refactors.
- Do not update generated `.Rd`, README, vignette, or cheat sheet unless public
  behavior or documentation becomes inaccurate.

## Completed Phase Summary

### Phase 0: Baseline And Audit Setup

Completed. Recorded package version, release tag, exported API count, newest
API risk (`pi_mu()`), baseline tests, cheat-sheet audit, and local
`devtools::check()` environment blocker.

### Phase 1: API And Default-Policy Lock

Completed. Confirmed the 29 exported functions, signatures, result-object
contracts, `quiet` and `digits` behavior, and default-policy commitments.

### Phase 2: Prediction, Confidence, And Width Families

Completed. Audited `pi_mu()`, `ci_mu()`, `ci_p()`, `ci_var()`,
`ci_lambda_exp()`, `n_width_mu_z()`, `n_width_mu_t()`, and
`n_width_p_wald()`. No production formula defects were found. Focused formula
tests were added.

### Phase 3: Hypothesis Test Families

Completed. Audited `z_test_mu()`, `t_test_mu()`, `p_test()`, and
`var_test_chisq()`. No production formula defects were found. Focused tests
were added for under-covered tails, critical values, continuity correction, and
variance-ratio behavior.

### Phase 4: Power Families

Completed. Audited `power_z_mu()`, `power_t_mu()`, `power_p_z()`,
`power_var_chisq()`, and `power_var_ratio_F()`. No production formula defects
were found. Focused formula tests were added.

### Phase 5: Required Sample Size For Target Power

Completed. Audited the generic solver and required-n wrappers. Minimality,
allocation ratios, unreachable-target errors, and achieved-power fields matched
the intended contracts. Focused tests were added.

### Phase 6: Chi-Square And Categorical Procedures

Completed. Audited `chisq_gof_probs()`, `chisq_gof_dist()`, `chisq_table()`,
and `table_props()`. No production formula defects were found. Focused tests
were added. A non-blocking label-handling observation was recorded for named
one-way observed vectors in `chisq_gof_probs()`.

### Phase 7: Shape Helpers

Completed. Audited `skew()` and `kurt()` estimators, validation, result
contracts, and interpretation labels. No production formula defects were found.
Focused tests were added.

### Phase 8: Initial Efficiency And Numerical Stability Pass

Completed. Broad helper extraction was rejected as likely to increase code
length or risk output drift. One safe numerical-stability fix was made:
upper-tail probabilities now use `lower.tail = FALSE` where appropriate.

### Phase 9: Final Regression, Documentation Alignment, And Closeout

Completed. Confirmed final API/docs/default-policy alignment, final local QA,
clean CI across OS/R matrix, and final report.

## Follow-Up Code Efficiency Audit

The mathematical audit was deliberately broad and conservative. The code
efficiency portion received only one focused phase. The follow-up work below is
intended to audit efficiency and maintainability more deliberately without
reopening mathematical behavior that is already locked.

## Phase 10: Efficiency Baseline And Static Inventory

### 10.1 Branch And Baseline

- Start from `main` at or after merge commit `9b647bb`.
- Create a non-`main` branch, recommended:
  `codex/code-efficiency-audit`.
- Confirm exported function count remains `29`.
- Run and record:
  - `devtools::test("StatsPackage -1.0")`
  - `Rscript scripts/qa_cheatsheet_audit.R`
- Treat local `devtools::check()` as non-blocking unless the local Rscript
  anomaly has been fixed.

### 10.2 Static Inventory

Review production code for:

- repeated validation logic;
- repeated critical-value and tail-selection logic;
- repeated result construction;
- repeated printed-output construction;
- repeated sample-size search patterns;
- unnecessary recomputation inside loops;
- branch complexity that can be simplified locally.

Primary review targets:

- `StatsPackage -1.0/R/n-required-functions.R`
- `StatsPackage -1.0/R/power-functions.R`
- `StatsPackage -1.0/R/hypothesis-test-functions.R`
- `StatsPackage -1.0/R/chisq-functions.R`

Secondary targets:

- helper files that already support simplification;
- confidence/prediction/shape files only if the static inventory finds clear
  duplication or recomputation.

### 10.3 Inventory Deliverable

Create `docs/MATH_EFFICIENCY_PHASE10_EFFICIENCY_BASELINE.md` with:

- reviewed files;
- repeated patterns found;
- candidate hotspots;
- rejected cosmetic-only refactors;
- baseline QA results;
- a candidate list for profiling.

No production edits should occur in Phase 10.

## Phase 11: Runtime Profiling And Hotspot Triage

### 11.1 Profiling Rules

- Use temporary local scripts or direct R commands; do not commit profiling
  scratch files.
- Prefer base R tools:
  - `system.time()`
  - `Rprof()`
  - `summaryRprof()`
- Do not add package dependencies such as `bench` or `microbenchmark`.
- Profile `quiet = TRUE` calls unless printed output performance is the target.

### 11.2 Representative Workloads

Profile:

- required-n searches with harder targets and larger `n_max`;
- two-sample required-n functions with non-1 allocation ratios;
- `power_*()` functions called repeatedly through required-n wrappers;
- `chisq_gof_dist()` raw continuous data with omitted `breaks`;
- `chisq_gof_dist()` sparse class combining;
- high-volume classed result construction for common functions.

### 11.3 Profiling Deliverable

Create `docs/MATH_EFFICIENCY_PHASE11_PROFILING_FINDINGS.md` with:

- workloads used;
- before-change timings;
- observed hotspots;
- candidates approved for implementation;
- candidates rejected because risk exceeds likely benefit.

No production edits should occur in Phase 11 unless profiling exposes a clear
bug or severe performance issue.

## Phase 12: Targeted Efficiency And Simplification Changes

### 12.1 Decision Rules

Implement a production change only if it satisfies at least one condition:

- reduces production line count while preserving behavior;
- removes meaningful duplication across multiple branches;
- avoids repeated expensive computation shown by profiling;
- improves memory behavior for representative workloads;
- improves numerical stability without changing public contracts.

Reject changes that:

- only reformat code;
- increase code length without measured benefit;
- alter printed-output templates;
- alter public result fields or class names;
- change required-n minimality, `n_evaluations`, or allocation semantics;
- introduce broad helper abstraction without net simplification.

### 12.2 Candidate Areas

Investigate, in order:

1. Required-n solver and wrappers:
   - avoid repeated achieved-power calls only if `n_evaluations` semantics are
     preserved or intentionally documented;
   - preserve minimality guarantees.
2. Power and hypothesis tails:
   - consolidate repeated tail or critical-value logic only if production code
     gets shorter or clearer without output drift.
3. Chi-square distribution GOF:
   - review binning and sparse-combining loops for avoidable recomputation;
   - preserve labels, degrees of freedom, expected counts, warnings, and
     result fields.
4. Printing paths:
   - leave unchanged unless an exact local simplification removes duplication
     without changing text.

### 12.3 Testing During Implementation

After each production change, run the narrowest affected test slice first:

- `devtools::test("StatsPackage -1.0", filter = "n-required")`
- `devtools::test("StatsPackage -1.0", filter = "power")`
- `devtools::test("StatsPackage -1.0", filter = "hypothesis")`
- `devtools::test("StatsPackage -1.0", filter = "chisq")`

Then run:

- `devtools::test("StatsPackage -1.0")`
- `Rscript scripts/qa_cheatsheet_audit.R`

If a change claims performance improvement, record before/after timing in the
findings document.

### 12.4 Commit Rules

- Commit production/test changes before findings docs.
- Keep unrelated docs and generated files untouched.
- If no safe simplification is found, make no production-code changes and
  record the reason.

## Phase 13: Efficiency Audit Closeout

### 13.1 Final Findings

Create `docs/MATH_EFFICIENCY_PHASE13_CODE_EFFICIENCY_FINAL_REPORT.md` with:

- files reviewed;
- changes made;
- candidates rejected;
- before/after timing evidence;
- line-count or complexity impact where relevant;
- QA results;
- remaining risks.

### 13.2 Final QA

Run:

- `devtools::test("StatsPackage -1.0")`
- `Rscript scripts/qa_cheatsheet_audit.R`

Open a PR and require clean GitHub Actions before merging. Clean CI should
include `devtools::check()` on the existing matrix.

### 13.3 Acceptance Criteria

The efficiency follow-up is complete when:

- public API behavior is unchanged;
- final full tests pass;
- cheat-sheet audit reports `Issues found: 0`;
- GitHub Actions `CI`, `Coverage`, and `Lint` pass;
- all accepted performance claims have recorded evidence;
- all rejected candidates are documented clearly.

## Updated Recommended Sequence

The completed mathematical audit is closed. The next deliberate code
efficiency audit should proceed as:

1. Phase 10: efficiency baseline and static inventory.
2. Phase 11: runtime profiling and hotspot triage.
3. Phase 12: targeted efficiency and simplification changes.
4. Phase 13: efficiency audit closeout and clean CI confirmation.
