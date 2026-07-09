# Changelog

## StatsPackage 1.1.1

- Improve numerical precision for extreme upper-tail probabilities in
  hypothesis-test and power calculations.
- Fix sparse-class combining for high-lambda estimated Poisson
  goodness-of-fit tests so valid inputs retain positive degrees of
  freedom.
- Add focused mathematical regression tests and raise the enforced
  coverage threshold to 80%.
- Add runnable examples for all exported functions and validate both
  supported cheat sheets in release QA.
- Normalize the package source directory and helper-file naming.
- Harden release automation, package checks, and GitHub Actions
  dependencies.

## StatsPackage 1.1.0

- Add
  [`pi_mu()`](https://christopherposadasp-ctrl.github.io/StatsRPackage/reference/prediction_interval_functions.md)
  for one-sample prediction intervals for a future observation using
  summary statistics.
- Support z-based prediction intervals when `sigma` is known and t-based
  prediction intervals when only the sample SD `s` is available.
- Keep prediction intervals separate from confidence intervals by
  returning `prediction_result` objects with `pi` bounds.

## StatsPackage 1.0.0

- Declare the public API stable for the package’s intended introductory
  inference workflows.
- Finalize the 1.0 freeze gate with aligned documentation, cheat-sheet
  regression validation, and release QA automation.
- Keep exported functions, defaults, and return-object conventions fixed
  as the 1.0 baseline unless a correctness issue requires a future
  patch.

## StatsPackage 0.1.4

- Add `dist = "unif"` to
  [`chisq_gof_dist()`](https://christopherposadasp-ctrl.github.io/StatsRPackage/reference/chisq_functions.md)
  for continuous uniform GOF.
- Support raw-data uniform GOF with `estimate = TRUE` using `min(x)` and
  `max(x)`.
- Support grouped-count uniform GOF with fixed `min`/`max` (or
  `lower`/`upper`) parameters.
- Add regression tests and cheat-sheet coverage for the uniform GOF
  workflow.

## StatsPackage 0.1.3

- Allow
  [`chisq_gof_dist()`](https://christopherposadasp-ctrl.github.io/StatsRPackage/reference/chisq_functions.md)
  to use `k = 3` for exponential raw-data GOF when the degrees of
  freedom remain positive.
- Keep stricter `k` requirements for continuous GOF settings that would
  otherwise yield invalid degrees of freedom.
- Add regression tests covering the new exponential `k = 3` path and the
  retained normal-model rejection at `k = 3`.

## StatsPackage 0.1.2

- Fix pkgdown metadata to include the deployed site URL in
  `DESCRIPTION`.
- Add ignore rules for generated pkgdown site artifacts.
- Keep release automation and package metadata aligned for the next tag.

## StatsPackage 0.1.1

- Align package version with post-`0.1.0` release maintenance.
- Add initial user vignette for core workflows.
- Add pkgdown configuration for generated reference and article site.
- Clarify release/QA automation and CI expectations in repository
  documentation.

## StatsPackage 0.1.0

- First tagged stable release of the package-quality toolkit.
- Confidence interval, hypothesis testing, power, and
  required-sample-size families available.
- Categorical-data functions added
  ([`chisq_gof_probs()`](https://christopherposadasp-ctrl.github.io/StatsRPackage/reference/chisq_functions.md),
  [`chisq_gof_dist()`](https://christopherposadasp-ctrl.github.io/StatsRPackage/reference/chisq_functions.md),
  [`chisq_table()`](https://christopherposadasp-ctrl.github.io/StatsRPackage/reference/chisq_functions.md),
  [`table_props()`](https://christopherposadasp-ctrl.github.io/StatsRPackage/reference/chisq_functions.md)).
- Regression-tested behavior and cheat-sheet-based QA workflow
  established.
