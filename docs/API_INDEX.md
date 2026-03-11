# StatsPackage API Index

This index lists the exported API grouped by function family.

For argument-level help in R, use `?function_name` after loading the package.

## Confidence Intervals

| Function | Summary |
| --- | --- |
| `ci_mu()` | One-sample and two-sample CIs for means (z, t, Welch, pooled, paired) |
| `ci_p()` | One-sample exact CI for proportion and two-sample Wald CI for difference |
| `ci_var()` | One-sample chi-square CI for variance/SD and two-sample F CI for variance ratio |
| `ci_lambda_exp()` | CI for exponential rate parameter using chi-square methods |

## Width and Precision Planning

| Function | Summary |
| --- | --- |
| `n_width_mu_z()` | Required sample size for target CI width for mean (known sigma) |
| `n_width_mu_t()` | Required sample size for target CI width for mean (unknown sigma, planning SD) |
| `n_width_p_wald()` | Required sample size for target CI width for proportion (Wald approximation) |

## Hypothesis Tests

| Function | Summary |
| --- | --- |
| `z_test_mu()` | z tests for one-sample and two-sample means with known sigma |
| `t_test_mu()` | t tests for one-sample, two-sample Welch/pooled, and paired means |
| `p_test()` | One-sample exact binomial test and two-sample z tests for proportions |
| `var_test_chisq()` | One-sample chi-square variance test and two-sample F variance-ratio test |

## Categorical Data (Chi-square)

| Function | Summary |
| --- | --- |
| `chisq_gof_probs()` | Goodness-of-fit test to specified category probabilities |
| `chisq_gof_dist()` | Goodness-of-fit test to distributions (`pois`, `norm`, `exp`) |
| `chisq_table()` | Chi-square test for independence/homogeneity in contingency tables |
| `table_props()` | Row/column/overall descriptive proportions for contingency tables |

## Power

| Function | Summary |
| --- | --- |
| `power_z_mu()` | Power for z tests of means |
| `power_t_mu()` | Power for t tests of means |
| `power_p_z()` | Power for z tests of proportions |
| `power_var_chisq()` | Power for one-sample chi-square variance tests |
| `power_var_ratio_F()` | Power for two-sample F variance-ratio tests |

## Required Sample Size for Target Power

| Function | Summary |
| --- | --- |
| `n_required_from_power()` | Generic solver for integer sample size from user-supplied power function |
| `n_required_z_mu()` | Required n for z tests of means |
| `n_required_t_mu()` | Required n for t tests of means |
| `n_required_p_z()` | Required n for z tests of proportions |
| `n_required_var_chisq()` | Required n for one-sample chi-square variance tests |
| `n_required_var_ratio_F()` | Required n for two-sample F variance-ratio tests |

## Shape

| Function | Summary |
| --- | --- |
| `skew()` | Sample skewness estimates |
| `kurt()` | Sample kurtosis/excess kurtosis estimates |

## Result Object Conventions

Most families follow these conventions:
- `quiet = FALSE` prints a readable summary
- `quiet = TRUE` suppresses printed output
- return values are invisible, classed objects with full computational fields
- result objects include decision-critical fields such as statistic, p-value or power/beta, and rejection region/critical values when relevant

