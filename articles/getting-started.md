# Getting Started with StatsPackage

`StatsPackage` is a reusable inference toolkit for confidence intervals,
hypothesis tests, power, sample size planning, and categorical-data
analysis.

## Load the package

``` r
library(StatsPackage)
```

## Confidence intervals

Use
[`ci_mu()`](https://christopherposadasp-ctrl.github.io/StatsRPackage/reference/ci_functions.md)
for one-sample or two-sample mean intervals:

``` r
ci_mu(xbar = 12.4, n = 15, s = 3.2, quiet = TRUE)
ci_mu(xbar = c(65, 63), n = c(40, 38), s = c(3.0, 2.5), method = "welch", quiet = TRUE)
```

Use
[`ci_p()`](https://christopherposadasp-ctrl.github.io/StatsRPackage/reference/ci_functions.md)
for one-sample exact intervals or two-sample difference intervals:

``` r
ci_p(x = 56, n = 100, quiet = TRUE)
ci_p(x = c(42, 30), n = c(100, 100), quiet = TRUE)
```

## Hypothesis testing

[`p_test()`](https://christopherposadasp-ctrl.github.io/StatsRPackage/reference/hypothesis_test_functions.md)
supports one-sample exact and two-sample z tests.

``` r
p_test(x = 21, n = 100, p0 = 0.20, quiet = TRUE)
p_test(x = c(35, 24), n = c(100, 100), p0 = 0, quiet = TRUE)
```

Variance-family tests use tail-based two-sided rejection regions:

``` r
var_test_chisq(s = 4.2, n = 12, sigma0 = 5, alternative = "two.sided", quiet = TRUE)
```

## Categorical data

``` r
obs <- c(18, 35, 31, 16)
chisq_gof_probs(observed = obs, p = c(0.2, 0.3, 0.3, 0.2), quiet = TRUE)
```

``` r
tab <- matrix(c(22, 18, 14, 26), nrow = 2, byrow = TRUE)
chisq_table(tab, type = "independence", quiet = TRUE)
table_props(tab, margin = "row", quiet = TRUE)
```

## Power and required sample size

``` r
power_z_mu(mu_a = 103, mu0 = 100, sigma = 10, n = 64, quiet = TRUE)
n_required_z_mu(mu_a = 103, mu0 = 100, sigma = 10, beta_target = 0.1, quiet = TRUE)
```

## Recommended QA workflow

For local validation before publishing:

``` r
devtools::test("StatsPackage -1.0")
devtools::check("StatsPackage -1.0")
source("2.CheatsheetV5.r")
```
