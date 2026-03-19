# StatsPackage 0.1.4

- Add `dist = "unif"` to `chisq_gof_dist()` for continuous uniform GOF.
- Support raw-data uniform GOF with `estimate = TRUE` using `min(x)` and `max(x)`.
- Support grouped-count uniform GOF with fixed `min`/`max` (or `lower`/`upper`) parameters.
- Add regression tests and cheat-sheet coverage for the uniform GOF workflow.

# StatsPackage 0.1.3

- Allow `chisq_gof_dist()` to use `k = 3` for exponential raw-data GOF when
  the degrees of freedom remain positive.
- Keep stricter `k` requirements for continuous GOF settings that would
  otherwise yield invalid degrees of freedom.
- Add regression tests covering the new exponential `k = 3` path and the
  retained normal-model rejection at `k = 3`.

# StatsPackage 0.1.2

- Fix pkgdown metadata to include the deployed site URL in `DESCRIPTION`.
- Add ignore rules for generated pkgdown site artifacts.
- Keep release automation and package metadata aligned for the next tag.

# StatsPackage 0.1.1

- Align package version with post-`0.1.0` release maintenance.
- Add initial user vignette for core workflows.
- Add pkgdown configuration for generated reference and article site.
- Clarify release/QA automation and CI expectations in repository documentation.

# StatsPackage 0.1.0

- First tagged stable release of the package-quality toolkit.
- Confidence interval, hypothesis testing, power, and required-sample-size families available.
- Categorical-data functions added (`chisq_gof_probs()`, `chisq_gof_dist()`, `chisq_table()`, `table_props()`).
- Regression-tested behavior and cheat-sheet-based QA workflow established.
