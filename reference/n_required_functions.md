# Sample-size functions for target power

Determines the smallest sample size that achieves a requested target
power for common hypothesis tests.

## Usage

``` r
n_required_from_power(
  power_at_n,
  target_power,
  n_min = 1L,
  n_max = 1000000L,
  digits = 4,
  quiet = FALSE
)

n_required_z_mu(
  mu_a,
  mu0,
  sigma,
  alpha = 0.05,
  beta_target = 0.1,
  alternative = c("two.sided", "less", "greater"),
  n_min = 1L,
  n_max = 1000000L,
  n_ratio = 1,
  digits = 4,
  quiet = FALSE
)

n_required_t_mu(
  mu_a,
  mu0,
  sigma_true,
  alpha = 0.05,
  beta_target = 0.1,
  alternative = c("two.sided", "less", "greater"),
  n_min = 2L,
  n_max = 1000000L,
  paired = FALSE,
  method = c("welch", "pooled"),
  n_ratio = 1,
  digits = 4,
  quiet = FALSE
)

n_required_p_z(
  p_a,
  p0,
  alpha = 0.05,
  beta_target = 0.1,
  alternative = c("two.sided", "less", "greater"),
  n_min = 5L,
  n_max = 10000000L,
  pooled = NULL,
  continuity = FALSE,
  n_ratio = 1,
  digits = 4,
  quiet = FALSE
)

n_required_var_chisq(
  sigma_a,
  sigma0,
  alpha = 0.05,
  beta_target = 0.1,
  alternative = c("two.sided", "less", "greater"),
  n_min = 2L,
  n_max = 1000000L,
  digits = 4,
  quiet = FALSE
)

n_required_var_ratio_F(
  sigma_a,
  ratio0 = 1,
  alpha = 0.05,
  beta_target = 0.1,
  alternative = c("two.sided", "less", "greater"),
  n_min = 2L,
  n_max = 1000000L,
  n_ratio = 1,
  digits = 4,
  quiet = FALSE
)
```

## Arguments

- power_at_n:

  Function returning power for a proposed integer sample size.

- target_power:

  Target power in (0, 1).

- n_min:

  Minimum integer sample size considered by the search.

- n_max:

  Maximum integer sample size considered by the search.

- digits:

  Integer number of decimal places used only for printed output.

- quiet:

  Logical; if `TRUE`, suppress printed output.

- mu_a:

  True mean value or vector of true means under the alternative.

- mu0:

  Null-hypothesis mean value or mean-difference value.

- sigma:

  Known population standard deviation input.

- alpha:

  Significance level in (0, 1).

- beta_target:

  Target type II error in (0, 1).

- alternative:

  Alternative hypothesis direction: `"two.sided"`, `"less"`, or
  `"greater"`.

- n_ratio:

  Allocation ratio `n2 / n1` for two-sample designs.

- sigma_true:

  True population standard deviation input used in t-test planning.

- paired:

  Logical; for `n_required_t_mu()`, whether the supplied summaries are
  for paired differences.

- method:

  Method for the two-sample sigma-unknown t-test planning calculation.

- p_a:

  True proportion value or vector of true proportions under the
  alternative.

- p0:

  Null-hypothesis proportion value or difference in proportions.

- pooled:

  Logical; for two-sample proportion planning, whether to use pooled
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

- `n_required_from_power()`: Generic integer sample-size solver based on
  a power function.

- `n_required_z_mu()`: Required sample size for z tests of means with
  known sigma.

- `n_required_t_mu()`: Required sample size for t tests of means.

- `n_required_p_z()`: Required sample size for z tests of proportions.

- `n_required_var_chisq()`: Required sample size for one-sample
  chi-square tests of variance.

- `n_required_var_ratio_F()`: Required sample size for two-sample F
  tests of a variance ratio.
