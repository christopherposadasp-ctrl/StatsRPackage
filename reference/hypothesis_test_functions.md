# Hypothesis-testing functions

Performs common one-sample and two-sample hypothesis tests for means,
proportions, and variances.

## Usage

``` r
z_test_mu(
  xbar,
  mu0,
  sigma = NULL,
  n,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  digits = 4,
  quiet = FALSE,
  s = NULL
)

t_test_mu(
  xbar,
  mu0,
  s,
  n,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  var.equal = FALSE,
  digits = 4,
  quiet = FALSE,
  paired = FALSE
)

p_test(
  x,
  n,
  p0,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  digits = 4,
  quiet = FALSE,
  check_npq = TRUE,
  pooled = NULL,
  continuity = FALSE
)

var_test_chisq(
  s,
  n,
  sigma0 = NULL,
  ratio0 = 1,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  digits = 4,
  quiet = FALSE
)
```

## Arguments

- xbar:

  Numeric vector of sample means or summary statistics.

- mu0:

  Null-hypothesis value for a mean or mean difference.

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

- s:

  Sample standard deviation input when sigma is unknown.

- var.equal:

  Logical; for `t_test_mu()`, whether to use the pooled-variance test.

- paired:

  Logical; for `t_test_mu()`, whether the supplied summaries are for
  paired differences.

- x:

  Count input for proportion and variance tests.

- p0:

  Null-hypothesis proportion value or difference in proportions.

- check_npq:

  Logical; whether to report normal-approximation adequacy checks for
  proportion tests.

- pooled:

  Logical; for two-sample proportion tests, whether to use pooled
  standard errors.

- continuity:

  Logical. If TRUE, apply a continuity correction in the two-sample z
  test for proportions. Ignored for one-sample exact tests.

- sigma0:

  Null-hypothesis standard deviation for the chi-square variance test.

- ratio0:

  Null-hypothesis variance ratio for the F test.

## Details

These functions return classed result objects with unrounded stored
values and optional printed summaries controlled by `digits` and
`quiet`.

## Functions

- `z_test_mu()`: Z test for a mean or difference in means with known
  sigma.

- `t_test_mu()`: t test for a mean or difference in means.

- `p_test()`: Hypothesis test for one or two proportions.

- `var_test_chisq()`: Chi-square or F test for variances.
