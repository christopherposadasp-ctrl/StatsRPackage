# Mathematical Correctness and Code Efficiency Audit Plan

This plan is for a future audit of `StatsPackage`, an R package for
introductory statistical inference and design workflows.

Audit baseline: StatsPackage 1.1.0 on main, after the `pi_mu()` release and
pkgdown workflow fix.

The audit focus is:

- mathematical correctness
- code efficiency
- simplification where behavior can be preserved

The audit must not change public API behavior unless a mathematical
correctness issue requires it.

## Audit Constraints

- Preserve exported function names, argument meanings, defaults, and result
  object conventions unless a correctness issue requires a change.
- Outside of testing files, code should not grow longer unless it is
  mathematically necessary.
- Prefer simplification, removal of duplication, and clearer existing logic
  over adding new abstraction.
- Keep printed summaries and invisible classed return objects consistent with
  current package conventions.
- Preserve `quiet`, `digits`, `alpha`, and `conf.level` behavior across
  families.
- Do not treat formatting-only preferences as audit defects unless they affect
  correctness, API consistency, or meaningful maintainability.
- Run the audit on a non-`main` branch.
- Do not retag, rewrite, or otherwise alter the published `1.1.0` release.

## Phase 0: Baseline And Audit Setup

### 0.1 Establish The Baseline

- Record current package version from `StatsPackage -1.0/DESCRIPTION`.
- Review current `NEWS.md` to identify recent or high-risk changes.
- Treat `pi_mu()` as a recent/high-risk exported API because it was added in
  the `1.1.0` release.
- Confirm the exported API from `NAMESPACE` and `docs/API_INDEX.md`.
- Confirm default-policy commitments from `docs/DEFAULT_POLICIES.md`.

### 0.2 Capture Baseline QA

- Run the fast test suite with `devtools::test("StatsPackage -1.0")`.
- Run the package check if the audit will touch broad behavior.
- Run the cheat-sheet regression script or structured audit runner when output
  behavior may be affected.
- Save notable baseline warnings, failures, or environment issues before
  making changes.

### 0.3 Define Reference Cases

- Build a small set of hand-checkable cases for each statistical family.
- Include boundary cases, one-sided cases, two-sided cases, and degenerate or
  near-degenerate inputs where the public API currently supports them.
- Prefer independent checks from base R functions, textbook formulas, or direct
  distribution calculations.

## Phase 1: API And Default-Policy Lock

### 1.1 Public API Inventory

- Verify exported functions match the documented API index.
- Verify each exported function has stable argument names and meanings.
- Identify any undocumented public behavior used by the cheat sheet.

### 1.2 Result Object Contract

- Confirm each function returns an invisible, classed list result.
- Confirm computational fields are unrounded.
- Confirm printed output uses `digits` only for display.
- Confirm `quiet = TRUE` suppresses printing.

### 1.3 Default Policy Review

- Preserve one-sample exact binomial behavior in `p_test()`.
- Preserve two-sample proportion defaults:
  - `p0 = 0`: pooled by default
  - `p0 != 0`: unpooled by default
  - continuity correction off by default
  - `pooled = TRUE` with nonzero `p0` errors
- Preserve lower-tail OR upper-tail two-sided rejection regions for chi-square
  and F variance tests.
- Preserve `pi_mu()` as a prediction interval for one future observation, not
  a confidence interval for `mu`.

## Phase 2: Confidence, Prediction, And Width Families

### 2.1 Mean Confidence Intervals: `ci_mu()`

- Check one-sample z interval formulas with known `sigma`.
- Check one-sample t interval formulas with sample `s`.
- Check paired t intervals on summary statistics for differences.
- Check two-sample z intervals with known sigmas.
- Check Welch t interval degrees of freedom and standard error.
- Check pooled t interval pooled variance, degrees of freedom, and standard
  error.
- Check one-sided lower and upper interval bounds.

### 2.2 Proportion Confidence Intervals: `ci_p()`

- Check one-sample exact Clopper-Pearson interval behavior.
- Check optional one-sample Wald behavior if retained.
- Check two-sample Wald interval for `p1 - p2`.
- Review warning behavior for small success or failure counts.

### 2.3 Variance And Exponential Confidence Intervals

- Check one-sample variance and SD chi-square intervals in `ci_var()`.
- Check two-sample F interval for `sigma1^2 / sigma2^2`.
- Check zero-variance edge handling.
- Check exponential rate interval in `ci_lambda_exp()` using chi-square
  quantiles with `df = 2n`.

### 2.4 Prediction Intervals: `pi_mu()`

- Check z prediction interval with known `sigma`.
- Check t prediction interval with sample `s` and `df = n - 1`.
- Verify standard error uses `sqrt(1 + 1 / n)`.
- Verify one-sided prediction intervals behave consistently with `ci_mu()`.
- Confirm two-sample, paired, pooled, Welch, and regression prediction
  intervals remain out of scope.

### 2.5 Width Planning

- Check `n_width_mu_z()` uses total width `w`, not half-width.
- Check `n_width_mu_t()` iteration, post-check search, convergence flags, and
  minimality.
- Check `n_width_p_wald()` worst-case and planning-proportion paths.
- Verify achieved width fields and minimality indicators.

## Phase 3: Hypothesis Test Families

### 3.1 Mean Tests

- Check `z_test_mu()` one-sample and two-sample statistics, p-values, critical
  values, and rejection regions.
- Check `t_test_mu()` one-sample, paired, Welch, and pooled branches.
- Verify one-sided alternatives use the correct tail.
- Verify estimate-critical boundaries match statistic-critical boundaries.

### 3.2 Proportion Tests

- Check `p_test()` one-sample exact binomial branch against `binom.test()`.
- Check two-sample z tests with pooled and unpooled standard errors.
- Check nonzero null difference behavior.
- Check continuity correction formulas for less, greater, and two-sided
  alternatives.
- Verify adequacy warnings do not alter computations.

### 3.3 Variance Tests

- Check one-sample chi-square variance statistic and p-values.
- Check two-sample F variance-ratio statistic and p-values.
- Verify two-sided p-values and rejection regions are tail-based.
- Confirm `ratio0` and `sigma0` are accepted or ignored only in documented
  branches.

## Phase 4: Power Families

### 4.1 Mean Power

- Check `power_z_mu()` against direct normal calculations.
- Check `power_t_mu()` one-sample and paired noncentral t calculations.
- Check pooled two-sample t power with equal-variance assumptions.
- Check Welch two-sample t power approximation and degrees of freedom.

### 4.2 Proportion Power

- Check one-sample normal approximation against direct distribution formulas.
- Check two-sample pooled and unpooled branches.
- Check nonzero `p0` behavior.
- Check continuity correction support and any documented limitations.
- Verify boundary probabilities and zero-variance cases are handled
  intentionally.

### 4.3 Variance Power

- Check `power_var_chisq()` using chi-square scaling under the alternative.
- Check `power_var_ratio_F()` using F scaling under the alternative.
- Verify one-sided direction checks and two-sided tail calculations.

## Phase 5: Required Sample Size For Target Power

### 5.1 Generic Solver

- Audit `.nreq_find_min_n()` bracketing and binary search.
- Confirm `n_required_from_power()` assumes nondecreasing power and returns the
  smallest integer in range.
- Check behavior when the target is not reachable within `n_max`.
- Check cached evaluations do not affect correctness.

### 5.2 Mean Required-n Functions

- Check `n_required_z_mu()` closed-form one-sided shortcuts and post-checks.
- Check two-sided z design search.
- Check `n_required_t_mu()` one-sample, paired, Welch, and pooled branches.
- Verify `n_ratio = n2 / n1` is applied consistently.

### 5.3 Proportion Required-n Functions

- Check one-sample and two-sample searches compose correctly with
  `power_p_z()`.
- Preserve pooled/unpooled defaults from `DEFAULT_POLICIES.md`.
- Check continuity correction behavior in required-n planning.
- Verify approximation adequacy warnings at the returned sample size.

### 5.4 Variance Required-n Functions

- Check `n_required_var_chisq()` for one-sample variance planning.
- Check `n_required_var_ratio_F()` for two-sample variance-ratio planning.
- Verify one-sided alternatives enforce mathematically compatible directions.
- Confirm returned `n`, `n1`, and `n2` are minimal within the search contract.

## Phase 6: Chi-square And Categorical Procedures

### 6.1 Goodness-of-fit To Probabilities

- Check `chisq_gof_probs()` expected counts, contributions, residuals, degrees
  of freedom, p-values, and critical values.
- Verify equal-probability default when `p = NULL`.
- Check category label handling.

### 6.2 Goodness-of-fit To Distributions

- Check raw and grouped input branches for `pois`, `norm`, `exp`, and `unif`.
- Check parameter estimation and degrees-of-freedom adjustment.
- Check raw-data bin creation when `breaks` is omitted.
- Check sparse-class combining for continuous distributions.
- Check Poisson right-tail combining.
- Confirm expected counts remain positive and warnings are appropriate.

### 6.3 Contingency Tables

- Check `chisq_table()` expected counts, contributions, residuals, standardized
  residuals, degrees of freedom, p-values, and critical values.
- Check Yates correction only for 2 by 2 tables.
- Confirm independence and homogeneity differ only in interpretation, not the
  statistic.

### 6.4 Table Proportions

- Check `table_props()` row, column, and overall margins.
- Verify row and column totals are preserved in returned objects.

## Phase 7: Shape Helpers

### 7.1 Skewness

- Check `skew()` moment estimator.
- Check adjusted estimator and minimum sample size.
- Check missing-value handling through `na_rm`.
- Check identical-observation error behavior.

### 7.2 Kurtosis

- Check Pearson and excess kurtosis paths.
- Check moment and adjusted estimators.
- Check minimum sample size for adjusted kurtosis.
- Check interpretation labels around normal-reference values.

## Phase 8: Code Efficiency And Simplification

### 8.1 Duplication Review

- Identify repeated validation, tail-selection, and result-construction logic.
- Simplify only when the resulting code is shorter or meaningfully clearer.
- Avoid new abstractions unless they remove real complexity.

### 8.2 Search And Solver Efficiency

- Review required-n searches for unnecessary repeated power calculations.
- Confirm any caching or closed-form shortcuts preserve exact returned results.
- Avoid performance changes that weaken minimality guarantees.

### 8.3 Numerical Stability

- Look for avoidable cancellation, zero-division risks, and tail-probability
  precision issues.
- Prefer base R distribution functions with explicit tail arguments where that
  improves clarity or stability.

## Phase 9: Regression, Documentation Alignment, And Closure

### 9.1 Regression Testing

- Add or update tests only for audited behavior, mathematical fixes, or
  important default-policy guarantees.
- Include tests for bug fixes before or alongside implementation changes.
- Keep tests focused but broad enough to cover public behavior touched by the
  audit.

### 9.2 Cheat-sheet And Output Checks

- Run `Rscript 2.CheatsheetV8.r` after changes that affect printed output or
  example behavior.
- Run `Rscript scripts/qa_cheatsheet_audit.R` for release-level confidence.
- Inspect output changes intentionally; do not accept drift silently.

### 9.3 Documentation Alignment

- Check `README.md` for install instructions, version references, API
  examples, and default-behavior statements.
- Check `StatsPackage -1.0/vignettes/getting-started.Rmd` for user-facing
  examples and explanations.
- Check generated `.Rd` files under `StatsPackage -1.0/man/` when exported
  behavior, arguments, return fields, or examples change.
- Confirm `docs/API_INDEX.md`, `docs/DEFAULT_POLICIES.md`, `NEWS.md`, the
  cheat sheet, README, vignette, and generated `.Rd` docs all describe the
  same public behavior.

### 9.4 Package Checks

- Run `devtools::test("StatsPackage -1.0")`.
- Run `devtools::check("StatsPackage -1.0")` for broad or release-facing
  changes.
- Run lint only after code edits that may affect style or readability.

### 9.5 Audit Closeout

- Summarize mathematical issues found and how they were resolved.
- Summarize efficiency changes separately from correctness fixes.
- List any intentionally deferred risks.
- Confirm whether public API behavior changed and why.
- Confirm whether default policies remained intact.

## Recommended Audit Sequence

1. API and default-policy lock.
2. Confidence, prediction, and width families.
3. Hypothesis tests.
4. Power functions.
5. Required sample-size solvers.
6. Chi-square and categorical procedures.
7. Shape helpers.
8. Efficiency and simplification pass.
9. Regression and closeout.
