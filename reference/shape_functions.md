# Descriptive shape functions

Computes skewness and kurtosis measures with optional small-sample
adjustments and plain-language interpretations.

## Usage

``` r
skew(
  x,
  method = c("moment", "adjusted"),
  na_rm = FALSE,
  digits = 4,
  quiet = FALSE
)

kurt(
  x,
  method = c("moment", "adjusted"),
  excess = FALSE,
  na_rm = FALSE,
  digits = 4,
  quiet = FALSE
)
```

## Arguments

- x:

  Numeric sample vector.

- method:

  Estimator choice for the shape measure.

- na_rm:

  Logical; if `TRUE`, remove missing values before computing the
  measure.

- digits:

  Integer number of decimal places used only for printed output.

- quiet:

  Logical; if `TRUE`, suppress printed output.

- excess:

  Logical; for `kurt()`, whether to report excess kurtosis instead of
  Pearson kurtosis.

## Details

These functions return classed result objects with unrounded stored
values and optional printed summaries controlled by `digits` and
`quiet`.

## Functions

- `skew()`: Sample skewness measure and interpretation.

- `kurt()`: Sample kurtosis measure and interpretation.
