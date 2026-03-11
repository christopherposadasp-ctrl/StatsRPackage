# Chapter 14 chi-square and contingency-table functions

`chisq_gof_probs()` performs a one-way chi-square goodness-of-fit test
to specified category probabilities.

## Usage

``` r
chisq_gof_probs(
  observed,
  p = NULL,
  labels = NULL,
  min_expected = 5,
  alpha = 0.05,
  digits = 4,
  quiet = FALSE
)

chisq_gof_dist(
  x = NULL,
  observed = NULL,
  dist = c("exp", "norm", "pois"),
  k = NULL,
  breaks = NULL,
  params = NULL,
  estimate = FALSE,
  params_estimated = FALSE,
  min_expected = 5,
  alpha = 0.05,
  digits = 4,
  quiet = FALSE
)

chisq_table(
  observed,
  type = c("independence", "homogeneity"),
  correct = FALSE,
  min_expected = 5,
  alpha = 0.05,
  digits = 4,
  quiet = FALSE
)

table_props(
  observed,
  margin = c("row", "col", "overall"),
  digits = 4,
  quiet = FALSE
)
```

## Arguments

- observed:

  Observed counts. For `chisq_gof_probs()` and grouped
  `chisq_gof_dist()` inputs, use a nonnegative integer vector. For
  `chisq_table()` and `table_props()`, use a nonnegative two-way table
  or matrix.

- p:

  Probabilities under the null for `chisq_gof_probs()`. If `NULL`, equal
  probabilities are assumed.

- labels:

  Optional category labels for one-way grouped inputs.

- min_expected:

  Minimum expected count threshold used for warnings and, where
  implemented, class combining.

- alpha:

  Significance level in (0, 1).

- digits:

  Integer number of decimal places used only for printed output.

- quiet:

  Logical; if `TRUE`, suppress printed output.

- x:

  Raw sample data for `chisq_gof_dist()`.

- dist:

  Distribution for `chisq_gof_dist()`: `"pois"`, `"norm"`, or `"exp"`.

- k:

  Number of classes used for raw-data GOF tests when `breaks` is not
  supplied.

- breaks:

  Class boundaries for grouped `chisq_gof_dist()` input.

- params:

  Named list of distribution parameters when parameters are not
  estimated from raw data.

- estimate:

  Logical; if `TRUE`, estimate parameters from raw data where
  applicable.

- params_estimated:

  Logical; for grouped GOF inputs, whether supplied parameters should
  count against the degrees of freedom.

- type:

  For `chisq_table()`, whether the interpretation is `"independence"` or
  `"homogeneity"`.

- correct:

  Logical; if `TRUE`, apply Yates correction in the 2 x 2 case for
  `chisq_table()`.

- margin:

  For `table_props()`, one of `"row"`, `"col"`, or `"overall"`.

## Value

For the chi-square test functions, a classed `htest_result` object with
statistic, df, p-value, critical value, rejection region, decision, and
diagnostic outputs such as observed counts, expected counts,
contributions, and residuals. For `chisq_table()`, standardized
residuals are also returned.

For `table_props()`, a classed `table_props_result` object is returned
with the original table and the requested proportions.

## Details

`chisq_gof_dist()` performs a chi-square goodness-of-fit test to a
distribution, supporting at least Poisson, normal, and exponential
models.

`chisq_table()` performs a chi-square test for independence or
homogeneity in an r x c contingency table.

`table_props()` computes descriptive row, column, or overall proportions
for a contingency table.

## Functions

- `chisq_gof_probs()`: One-way chi-square GOF test to specified
  probabilities.

- `chisq_gof_dist()`: Chi-square GOF test to a distribution.

- `chisq_table()`: Chi-square test for a contingency table.

- `table_props()`: Descriptive proportions for a contingency table.
