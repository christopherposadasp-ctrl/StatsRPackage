# Prediction interval functions

Computes prediction intervals for future observations from introductory
inference settings.

## Usage

``` r
pi_mu(
  xbar,
  n,
  s = NULL,
  sigma = NULL,
  conf.level = 0.95,
  side = c("two.sided", "lower", "upper"),
  digits = 4,
  quiet = FALSE
)
```

## Arguments

- xbar:

  Single sample mean.

- n:

  Single integer sample size.

- s:

  Sample standard deviation when population sigma is unknown.

- sigma:

  Known population standard deviation.

- conf.level:

  Confidence level in (0, 1).

- side:

  Interval side specification: `"two.sided"`, `"lower"`, or `"upper"`.

- digits:

  Integer number of decimal places used only for printed output.

- quiet:

  Logical; if `TRUE`, suppress printed output.

## Details

These functions return classed result objects with unrounded stored
values and optional printed summaries controlled by `digits` and
`quiet`.

## Functions

- `pi_mu()`: Prediction interval for a future observation.

## Examples

``` r
pi_mu(xbar = 12.4, n = 15, s = 3.2, quiet = TRUE)
```
