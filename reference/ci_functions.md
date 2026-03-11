# Confidence interval functions

Computes confidence intervals for means, proportions, variances, and the
exponential rate parameter.

## Usage

``` r
ci_mu(
  xbar,
  n,
  s = NULL,
  sigma = NULL,
  conf.level = 0.95,
  paired = FALSE,
  side = c("two.sided", "lower", "upper"),
  method = c("welch", "pooled", "z"),
  digits = 4,
  quiet = FALSE
)

ci_p(x, n, conf.level = 0.95, exact_1s = TRUE, digits = 4, quiet = FALSE)

ci_var(s, n, conf.level = 0.95, digits = 4, quiet = FALSE)

ci_lambda_exp(Sum, n, conf.level = 0.95, digits = 4, quiet = FALSE)
```

## Arguments

- xbar:

  Numeric vector of sample means or paired-difference mean summaries.

- n:

  Integer sample size or vector of sample sizes.

- s:

  Sample standard deviation input when sigma is unknown.

- sigma:

  Known population standard deviation input.

- conf.level:

  Confidence level in (0, 1).

- paired:

  Logical; for `ci_mu()`, whether the supplied summaries are for paired
  differences.

- side:

  Interval side specification: `"two.sided"`, `"lower"`, or `"upper"`.

- method:

  Method for the two-sample sigma-unknown mean interval.

- digits:

  Integer number of decimal places used only for printed output.

- quiet:

  Logical; if `TRUE`, suppress printed output.

- x:

  Count input for proportion, variance, or exponential-rate intervals.

- exact_1s:

  Logical; for `ci_p()`, whether to use exact one-sided bounds.

- Sum:

  Sum of observations for the exponential-rate interval.

## Details

These functions return classed result objects with unrounded stored
values and optional printed summaries controlled by `digits` and
`quiet`.

## Functions

- `ci_mu()`: Confidence interval for a mean or mean difference.

- `ci_p()`: Confidence interval for a proportion or difference in
  proportions.

- `ci_var()`: Confidence interval for a variance or variance ratio.

- `ci_lambda_exp()`: Confidence interval for an exponential rate
  parameter.
