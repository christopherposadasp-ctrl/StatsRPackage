# Power functions

Computes power and type II error for common tests for means,
proportions, and variances.

## Usage

``` r
power_z_mu(
  mu_a,
  mu0,
  sigma,
  n,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  digits = 4,
  quiet = FALSE
)

power_t_mu(
  mu_a,
  mu0,
  sigma_true,
  n,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  paired = FALSE,
  method = c("welch", "pooled"),
  digits = 4,
  quiet = FALSE
)

power_p_z(
  p_a,
  p0,
  n,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  digits = 4,
  quiet = FALSE,
  pooled = NULL,
  continuity = FALSE
)

power_var_chisq(
  sigma_a,
  sigma0,
  n,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  digits = 4,
  quiet = FALSE
)

power_var_ratio_F(
  sigma_a,
  ratio0 = 1,
  n,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  digits = 4,
  quiet = FALSE
)
```

## Arguments

- mu_a:

  True mean value or vector of true means under the alternative.

- mu0:

  Null-hypothesis mean value or mean-difference value.

- sigma:

  Known population standard deviation input.

- n:

  Integer sample size or vector of sample sizes.

- alpha:

  Significance level in (0, 1).

- alternative:

  Alternative hypothesis direction: `"two.sided"`, `"less"`, or
  `"greater"`.

- digits:

  Integer number of decimal places used only for printed output.

- quiet:

  Logical; if `TRUE`, suppress printed output.

- sigma_true:

  True population standard deviation input used in t-test power
  calculations.

- paired:

  Logical; for `power_t_mu()`, whether the supplied summaries are for
  paired differences.

- method:

  Method for the two-sample sigma-unknown t-test power calculation.

- p_a:

  True proportion value or vector of true proportions under the
  alternative.

- p0:

  Null-hypothesis proportion value or difference in proportions.

- pooled:

  Logical; for two-sample proportion power, whether to use pooled
  standard errors.

- continuity:

  Logical; whether to apply the continuity correction where supported.

- sigma_a:

  True standard deviation value or vector under the alternative.

- sigma0:

  Null-hypothesis standard deviation for the chi-square variance test.

- ratio0:

  Null-hypothesis variance ratio for the F test.

## Details

These functions return classed result objects with unrounded stored
values and optional printed summaries controlled by `digits` and
`quiet`.

## Functions

- `power_z_mu()`: Power for z tests of means with known sigma.

- `power_t_mu()`: Power for t tests of means.

- `power_p_z()`: Power for z tests of proportions.

- `power_var_chisq()`: Power for one-sample chi-square tests of
  variance.

- `power_var_ratio_F()`: Power for two-sample F tests of a variance
  ratio.
