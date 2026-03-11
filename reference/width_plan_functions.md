# Confidence-interval width planning functions

Determines the sample size required to achieve a target total confidence
interval width for selected interval procedures.

## Usage

``` r
n_width_mu_z(w, sigma, conf.level = 0.95, digits = 4, quiet = FALSE)

n_width_p_wald(
  w,
  conf.level = 0.95,
  p = 0.5,
  worst_case = TRUE,
  digits = 4,
  quiet = FALSE
)

n_width_mu_t(
  w,
  s,
  conf.level = 0.95,
  n_start = NULL,
  max_iter = 100,
  digits = 4,
  quiet = FALSE
)
```

## Arguments

- w:

  Target total confidence-interval width.

- sigma:

  Known population standard deviation input.

- conf.level:

  Confidence level in (0, 1).

- digits:

  Integer number of decimal places used only for printed output.

- quiet:

  Logical; if `TRUE`, suppress printed output.

- p:

  Planning value for a proportion.

- worst_case:

  Logical; if `TRUE`, use the worst-case proportion value.

- s:

  Sample standard deviation used for t-based width planning.

- n_start:

  Starting sample size for iterative width planning.

- max_iter:

  Maximum number of search iterations.

## Details

These functions return classed result objects with unrounded stored
values and optional printed summaries controlled by `digits` and
`quiet`.

## Functions

- `n_width_mu_z()`: Required sample size for a z interval for a mean.

- `n_width_p_wald()`: Required sample size for a Wald interval for a
  proportion.

- `n_width_mu_t()`: Required sample size for a t interval for a mean.
