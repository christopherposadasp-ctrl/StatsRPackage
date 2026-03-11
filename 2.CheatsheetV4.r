#############################################################
# StatsPackage Cheat Sheet
pkg_dir <- if (dir.exists("StatsPackage")) {
  "StatsPackage"
} else if (dir.exists("StatsPackage -1.0")) {
  "StatsPackage -1.0"
} else {
  stop("Could not find package directory. Expected 'StatsPackage' or 'StatsPackage -1.0'.")
}

devtools::install(pkg_dir)
library(StatsPackage)
#############################################################
defs <- list(

  `1) Descriptive Statistics` = c(
    "Population (N): the full group of interest.",
    "Sample (n): a subset of the population, selected to draw inferences about the population.",
    "Parameter (μ, σ², p, θ): a numerical characteristic of the population distribution; fixed but unknown.",
    "Statistic (X̄, S², p̂, θ̂): a function of observable random variables in a sample and known constants.",
    "Data types: numerical can be discrete (counts) or continuous (measurements).",
    "Measures of location: mean, median, mode. Median is more robust to outliers than mean.",
    "Percentiles/quantiles: p-th percentile has p% of data at or below it; p-th quantile is the same idea on a 0–1 scale.",
    "Quartiles/IQR: Q1 = 25th percentile, Q3 = 75th percentile; IQR = Q3 − Q1.",
    "Histogram: for numerical (continuous) data with adjacent bins.",
    "Bar plot: for categorical data with separated bars."
  ),

  `2) Sampling Distributions & CLT` = c(
    "Simple Random Sample (SRS) / iid: X1,…,Xn are independent and identically distributed from the same population distribution (same μ, σ²).",
    "Other samples: stratified, cluster, systematic.",
    "Sampling distribution: the probability distribution of a statistic over repeated SRS of size n.",
    "Standard error (SE): the SD of a statistic (e.g., SE(X̄) = σ/√n). Estimated SE plugs in s for σ.",
    "Linear combination: a1X1 + ⋯ + anXn for constants ai (special cases: sum, mean).",
    "Normal algebra (closure): linear combinations of independent normal RVs are normal. If X ~ N(μ, σ²), then aX + b ~ N(aμ + b, a²σ²).",
    "CLT: for large n, sums/means of iid RVs are approximately normal, regardless of population shape (approx improves as n increases).",
    "Normal approximation to Binomial: if large-enough conditions hold, Bin(n,p) ≈ N(np, np(1-p)); continuity correction ±0.5."
  ),

  `3) Point Estimation` = c(
    "Parameter vs estimator vs estimate: θ is fixed (unknown). θ̂ is a statistic (random variable). An estimate is the realized numeric value after observing data.",
    "Estimator: a rule/formula that tells how to calculate an estimate from sample measurements.",
    "Unbiased / bias: θ̂ is unbiased if E[θ̂] = θ for all θ.",
    "Standard error (SE): SE(θ̂) = SD(θ̂). Estimated SE plugs in sample estimates for unknown parameters.",
    "Mean squared error (MSE): measures overall accuracy (variance + bias²). Lower MSE is better.",
    "Minimum Variance Unbiased Estimator (MVUE): unbiased estimator with minimum variance among unbiased estimators of θ.",
    "Method of Moments (MoM): solve for parameters by matching population moments E[X^k] to sample moments (1/n)∑X_i^k (not guaranteed unbiased).",
    "Likelihood function: the joint distribution viewed as a function of the unknown parameters given the known data.",
    "Maximum Likelihood Estimator (MLE): choose parameter(s) maximizing likelihood L(θ|x) (often maximize log-likelihood).",
    "Invariance of MLE: if θ̂ is MLE of θ and h is one-to-one, then h(θ̂) is MLE of h(θ).",
    "Memoryless property: only geometric and exponential distributions have it (past doesn’t matter given survival)."
  ),

  `4) Interval Estimation / Confidence Intervals` = c(
    "Confidence interval: a random interval [L(X), U(X)] such that P(L(X) ≤ θ ≤ U(X)) = 1 − α, where 1 − α is the confidence level.",
    "Correct interpretation: θ is fixed (unknown); the interval is random before sampling. After computing it, the realized interval is fixed and either contains θ or it doesn’t.",
    "Effect of n: increasing n generally makes the CI narrower (precision ↑) but does not change the nominal confidence level (coverage stays 1 − α if assumptions hold).",
    "CI vs prediction interval: a CI for μ is not about a future value X_(n+1). A prediction interval targets X_(n+1) and is typically wider.",
    "Pivotal quantity: a function of the sample data and θ, where θ is the only unknown quantity. Moreover, the probability distribution of the pivotal quantity is known and does not depend on θ."
  ),

  `5) Hypothesis Testing` = c(
    "Statistical hypothesis: a statement about the value of one or more population characteristics (e.g., parameters such as μ = μ0).",
    "Type I error (α): rejecting H0 when H0 is true.",
    "Type II error (β): failing to reject H0 when H0 is false (i.e., Ha is true).",
    "Test statistic: a function of the data (with known distribution if H0 is true) used to decide whether to reject H0.",
    "Rejection region: the set of test statistic values for which H0 will be rejected.",
    "P-value: the probability, assuming H0 is true, of observing a test statistic as extreme or more extreme than what was observed.",
    "Significance level (α): acceptable Type I error rate; reject H0 iff p-value < α.",
    "Power: P(reject H0 | a specified alternative Ha is true) = 1 − β (viewed as a function of the Ha parameter)."
  )
)

# 1) CONFIDENCE INTERVALS ----
#############################################################
#############################################################
##CI for Mu----
# ci_mu(
#   xbar,
#   n,
#   s = NULL,
#   sigma = NULL,
#   conf.level = 0.95,
#   paired = FALSE,
#   side = c("two.sided", "lower", "upper"),
#   method = c("welch", "pooled", "z"),
#   digits = 4,
#   quiet = FALSE
# )
#
# When to use method:
#   method = "welch"  -> two-sample means, sigma unknown, do NOT assume
#                         equal variances; safest default.
#   method = "pooled" -> two-sample means, sigma unknown, and you are
#                         willing to assume equal variances.
#   method = "z"      -> use when population sigma is known.
#
# Structure:
#   one-sample z : scalar xbar, scalar n, sigma
#   one-sample t : scalar xbar, scalar n, s
#   paired t     : summary stats for paired differences, paired = TRUE
#   two-sample   : xbar and n as length-2 vectors
#############################################################

# One-sample z CI
ci_mu(xbar = 2.25, n = 36, sigma = 1.5)

# One-sample t CI
ci_mu(xbar = 12.4, n = 15, s = 3.2)

# Two-sample Welch CI
ci_mu(xbar = c(65, 63), n = c(40, 38), s = c(3.0, 2.5), method = "welch")

# Two-sample pooled CI
ci_mu(xbar = c(81, 77), n = c(25, 25), s = c(10, 9), method = "pooled")

# Two-sample z CI (known sigmas)
ci_mu(xbar = c(105, 100), n = c(64, 49), sigma = c(12, 15), method = "z")

# Paired CI (summary stats of paired differences)
ci_mu(xbar = -1.4, n = 18, s = 2.2, paired = TRUE)

# One-sided lower CI
ci_mu(xbar = 5.2, n = 25, s = 1.8, side = "lower")

#############################################################
## CI for Proportion -----
# ci_p(
#   x,
#   n,
#   conf.level = 0.95,
#   exact_1s = TRUE,
#   digits = 4,
#   quiet = FALSE
# )
#
# Structure:
#   one-sample : x and n are scalars
#   two-sample : x and n are length-2 vectors
#############################################################

# One-sample CI for a proportion
ci_p(x = 56, n = 100)

# Two-sample CI for difference in proportions
ci_p(x = c(42, 30), n = c(100, 100))


#############################################################
##CI for Variance----
# ci_var(
#   s,
#   n,
#   conf.level = 0.95,
#   digits = 4,
#   quiet = FALSE
# )
#
# One-sample CI for variance / standard deviation
#############################################################
ci_var(s = 4.2, n = 12)

# Two-sample (ratio σ1^2/σ2^2)
ci_var(s = c(4.2, 3.1), n = c(12, 10), conf.level = 0.95)
#############################################################
##CI for lambda (Exponential)----
# ci_lambda_exp(
#   Sum,
#   n,
#   conf.level = 0.95,
#   digits = 4,
#   quiet = FALSE
# )
#
# For exponential data, Sum is the sum of sample observations.
#############################################################

ci_lambda_exp(Sum = 140, n = 50)

#############################################################
# 2) TARGET WIDTH & SAMPLE SIZE FOR TARGET CI  ----
#############################################################
#############################################################
##Mu target width Z----
# n_width_mu_z(
#   w,
#   sigma,
#   conf.level = 0.95,
#   digits = 4,
#   quiet = FALSE
# )
#
# w = TOTAL CI width, not half-width
#############################################################

n_width_mu_z(w = 2, sigma = 10)


#############################################################
## Mu target width T----
#   w,
#   s,
#   conf.level = 0.95,
#   n_start = NULL,
#   max_iter = 100,
#   digits = 4,
#   quiet = FALSE
# )
#
# Use when sigma is unknown and s is your planning SD.
# w = TOTAL CI width.
#############################################################

n_width_mu_t(w = 4, s = 15)


#############################################################
## Proportion width target width Wald ----
# n_width_p_wald(
#   w,
#   conf.level = 0.95,
#   p = 0.5,
#   worst_case = TRUE,
#   digits = 4,
#   quiet = FALSE
# )
#
# worst_case = TRUE  -> uses p = 0.5
# worst_case = FALSE -> uses the planning value p
# w = TOTAL CI width
#############################################################

# Worst-case planning
n_width_p_wald(w = 0.10, worst_case = TRUE)

# Planning with guessed p
n_width_p_wald(w = 0.10, p = 0.30, worst_case = FALSE)


#############################################################
# 3) HYPOTHESIS TESTS ----
#############################################################

#############################################################
# z_test_mu(
#   xbar,
#   mu0,
#   sigma = NULL,
#   n,
#   alpha = 0.05,
#   alternative = c("two.sided", "less", "greater"),
#   digits = 4,
#   quiet = FALSE,
#   s = NULL
# )
#
# Use when population sigma is known.
# s can be used as an alias for sigma.
#
# Structure:
#   one-sample : scalar xbar, scalar n
#   two-sample : length-2 xbar, length-2 n
#############################################################

# One-sample z test
z_test_mu(xbar = 101, mu0 = 100, sigma = 15, n = 50)

# Two-sample z test
z_test_mu(xbar = c(82, 77), mu0 = 0, sigma = c(12, 10), n = c(64, 49))

# One-sided example
z_test_mu(xbar = 30500, mu0 = 30000, sigma = 1500, n = 75,
          alpha = 0.01, alternative = "greater")

#############################################################
# t_test_mu(
#   xbar,
#   mu0,
#   s,
#   n,
#   alpha = 0.05,
#   alternative = c("two.sided", "less", "greater"),
#   var.equal = FALSE,
#   digits = 4,
#   quiet = FALSE,
#   paired = FALSE
# )
#
# Use guide:
#   var.equal = FALSE -> Welch two-sample t test (default)
#   var.equal = TRUE  -> pooled two-sample t test
#   paired = TRUE     -> paired t test on summary stats of differences
#############################################################

# One-sample t test
t_test_mu(xbar = 5.4, mu0 = 5, s = 1.2, n = 20)

# Two-sample Welch t test
t_test_mu(xbar = c(10, 8), mu0 = 0, s = c(3, 2.5), n = c(20, 18), var.equal = FALSE)

# Two-sample pooled t test
t_test_mu(xbar = c(10, 8), mu0 = 0, s = c(3, 3.1), n = c(20, 20), var.equal = TRUE)

# Paired t test
t_test_mu(xbar = -1.2, mu0 = 0, s = 2.4, n = 16, paired = TRUE)

#############################################################
# p_test(
#   x,
#   n,
#   p0,
#   alpha = 0.05,
#   alternative = c("two.sided", "less", "greater"),
#   digits = 4,
#   quiet = FALSE,
#   check_npq = TRUE,
#   pooled = NULL,
#   continuity = FALSE
# )

# Structure:
#   one-sample : x and n are scalars, p0 is the null proportion
#   two-sample : x and n are length-2 vectors, p0 is the null difference
#
# For two-sample tests:
#   pooled = NULL  -> default behavior
#   pooled = TRUE  -> explicitly pooled SE under H0 (usually for p0 = 0)
#   pooled = FALSE -> explicitly unpooled SE
#############################################################
# One-sample exact
p_test(x = 21, n = 100, p0 = 0.20)

# Two-sample equality test: default pooled, no continuity correction
p_test(x = c(35, 24), n = c(100, 100), p0 = 0)

# Two-sample equality test: explicit unpooled version
p_test(x = c(35, 24), n = c(100, 100), p0 = 0, pooled = FALSE)

# Two-sample nonzero null difference: default unpooled, no continuity correction
p_test(x = c(35, 24), n = c(100, 100), p0 = 0.05)

# Optional conservative version
p_test(x = c(35, 24), n = c(100, 100), p0 = 0, continuity = TRUE)
#############################################################
# var_test_chisq(
#   s,
#   n,
#   sigma0 = NULL,
#   ratio0 = 1,
#   alpha = 0.05,
#   alternative = c("two.sided", "less", "greater"),
#   digits = 4,
#   quiet = FALSE
# )
#
# Structure:
#   one-sample : scalar s, scalar n, sigma0
#   two-sample : length-2 s, length-2 n, ratio0
#############################################################

# One-sample chi-square variance test
var_test_chisq(s = 4.2, n = 12, sigma0 = 5)

# Two-sample F test for ratio of variances
var_test_chisq(s = c(4.2, 5.0), n = c(12, 10), ratio0 = 1)

var_test_chisq(s = 4.2, n = 12, sigma0 = 5, alternative = "two.sided")
var_test_chisq(s = c(4.2, 5.0), n = c(12, 10), ratio0 = 1, alternative = "two.sided")

#############################################################
## Categorical Data / Chi-Square ----
#############################################################

#############################################################
# chisq_gof_probs(
#   observed,
#   p = NULL,
#   labels = NULL,
#   min_expected = 5,
#   alpha = 0.05,
#   digits = 4,
#   quiet = FALSE
# )
#
# Use guide:
#   p = NULL -> tests equal category probabilities
#   p given  -> tests a specified multinomial model
#############################################################

# Equal-probability GOF (seasonal homicides)
chisq_gof_probs(
  observed = c(328, 334, 372, 327),
  labels = c("Winter", "Spring", "Summer", "Fall")
)

# Specified multinomial GOF (Mendel 9:3:3:1)
chisq_gof_probs(
  observed = c(315, 108, 101, 32),
  p = c(9, 3, 3, 1) / 16,
  labels = c("Round Yellow", "Round Green", "Wrinkled Yellow", "Wrinkled Green")
)

#############################################################
# chisq_gof_dist(
#   x = NULL,
#   observed = NULL,
#   dist = c("exp", "norm", "pois"),
#   k = NULL,
#   breaks = NULL,
#   params = NULL,
#   estimate = FALSE,
#   params_estimated = FALSE,
#   min_expected = 5,
#   alpha = 0.05,
#   digits = 4,
#   quiet = FALSE
# )
#
# Use guide:
#   x + estimate = TRUE      -> raw data GOF with parameter estimation
#   observed + breaks        -> grouped continuous-data GOF
#   observed + dist = "pois" -> grouped count-data GOF
#############################################################

# Exponential GOF from raw data
chisq_gof_dist(
  x = c(.10, .99, 1.14, 1.26, 3.24, .12, .26, .80,
        .79, 1.16, 1.76, .41, .59, .27, 2.22, .66,
        .71, 2.21, .68, .43, .11, .46, .69, .38,
        .91, .55, .81, 2.51, 2.77, .16, 1.11, .02,
        2.13, .19, 1.21, 1.13, 2.93, 2.14, .34, .44),
  dist = "exp",
  k = 5,
  estimate = TRUE
)

# Normal GOF from grouped counts
chisq_gof_dist(
  observed = c(12, 20, 23, 15, 13),
  dist = "norm",
  breaks = c(-Inf, .100, .150, .200, .250, Inf),
  params = list(mean = .173, sd = .066),
  params_estimated = TRUE
)

# Poisson GOF from grouped count frequencies
chisq_gof_dist(
  observed = c(1627, 421, 219, 130, 107, 51, 15, 22,
               8, 14, 5, 8, 5, 0, 3, 2),
  dist = "pois",
  estimate = TRUE
)

smoking_tab <- matrix(
  c(25, 10,
    24, 32,
    28, 17,
    19, 34),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(c("<16", "16-17", "18-20", ">20"), c("Male", "Female"))
)

spirituality_tab <- matrix(
  c(56, 162, 198, 211,
    56, 223, 243, 239,
    109, 164, 74, 28),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(c("N.S.", "S.S.", "G.D."), c("Very", "Moderate", "Slightly", "Not at all"))
)

#############################################################
# table_props(
#   observed,
#   margin = c("row", "col", "overall"),
#   digits = 4,
#   quiet = FALSE
# )
#
# Use guide:
#   margin = "row"     -> row proportions
#   margin = "col"     -> column proportions
#   margin = "overall" -> overall proportions
#############################################################

# Row proportions
 table_props(smoking_tab, margin = "row")

# Column proportions
 table_props(smoking_tab, margin = "col")

# Overall proportions
 table_props(smoking_tab, margin = "overall")

#############################################################
# chisq_table(
#   observed,
#   type = c("independence", "homogeneity"),
#   correct = FALSE,
#   min_expected = 5,
#   alpha = 0.05,
#   digits = 4,
#   quiet = FALSE
# )
#
# Use guide:
#   type = "independence" -> one sample classified by two factors
#   type = "homogeneity"  -> several groups compared on one categorical response
#############################################################

# Independence example
chisq_table(smoking_tab, type = "independence")

# Homogeneity example
chisq_table(spirituality_tab, type = "homogeneity")

#############################################################
# 4) POWER
#############################################################

#############################################################
# power_z_mu(
#   mu_a,
#   mu0,
#   sigma,
#   n,
#   alpha = 0.05,
#   alternative = c("two.sided", "less", "greater"),
#   digits = 4,
#   quiet = FALSE
# )
#
# Structure:
#   one-sample : scalar mu_a, sigma, n
#   two-sample : length-2 mu_a, sigma, n
#############################################################
# Default equality test planning: pooled, no continuity correction
power_p_z(p_a = c(0.20, 0.30), p0 = 0, n = c(150, 150))
n_required_p_z(p_a = c(0.20, 0.30), p0 = 0,
               alpha = 0.05, beta_target = 0.10)

# Explicit conservative version
power_p_z(p_a = c(0.20, 0.30), p0 = 0, n = c(150, 150), continuity = TRUE)
n_required_p_z(p_a = c(0.20, 0.30), p0 = 0,
               alpha = 0.05, beta_target = 0.10, continuity = TRUE)

# Nonzero null difference: default unpooled, no continuity correction
power_p_z(p_a = c(0.35, 0.24), p0 = 0.05, n = c(100, 100))
n_required_p_z(p_a = c(0.35, 0.24), p0 = 0.05,
               alpha = 0.05, beta_target = 0.10)
#############################################################
# power_t_mu(
#   mu_a,
#   mu0,
#   sigma_true,
#   n,
#   alpha = 0.05,
#   alternative = c("two.sided", "less", "greater"),
#   paired = FALSE,
#   method = c("welch", "pooled"),
#   digits = 4,
#   quiet = FALSE
# )
#
# Use guide:
#   method = "welch"  -> two-sample, unequal-variance approach
#   method = "pooled" -> two-sample, equal-variance assumption
#   paired = TRUE     -> paired t power on difference summaries
#############################################################

# One-sample t power
power_t_mu(mu_a = 5.4, mu0 = 5, sigma_true = 1.2, n = 20)

# Two-sample Welch t power
power_t_mu(mu_a = c(10, 8), mu0 = 0, sigma_true = c(3, 2.5), n = c(20, 18), method = "welch")

# Two-sample pooled t power
power_t_mu(mu_a = c(10, 8), mu0 = 0, sigma_true = c(3, 3), n = c(20, 20), method = "pooled")

# Paired t power
power_t_mu(mu_a = -1.1, mu0 = 0, sigma_true = 2.0, n = 16, paired = TRUE)


#############################################################
# power_p_z(
#   p_a,
#   p0,
#   n,
#   alpha = 0.05,
#   alternative = c("two.sided", "less", "greater"),
#   digits = 4,
#   quiet = FALSE,
#   pooled = NULL,
#   continuity = FALSE
# )
#
# Structure:
#   one-sample : scalar p_a, scalar n
#   two-sample : length-2 p_a, length-2 n, p0 = null difference
#############################################################

# One-sample power
power_p_z(p_a = 0.30, p0 = 0.25, n = 200)

# Two-sample power: default behavior under p0 = 0
power_p_z(p_a = c(0.20, 0.30), p0 = 0, n = c(150, 150))

# Two-sample power: explicitly pooled
power_p_z(p_a = c(0.20, 0.30), p0 = 0, n = c(150, 150), pooled = TRUE)

# Two-sample power: explicitly unpooled
power_p_z(p_a = c(0.20, 0.30), p0 = 0, n = c(150, 150), pooled = FALSE)

# Two-sample power without continuity correction
power_p_z(p_a = c(0.20, 0.30), p0 = 0, n = c(150, 150),
          pooled = TRUE, continuity = FALSE)
#############################################################
# power_var_chisq(
#   sigma_a,
#   sigma0,
#   n,
#   alpha = 0.05,
#   alternative = c("two.sided", "less", "greater"),
#   digits = 4,
#   quiet = FALSE
# )
#############################################################

power_var_chisq(sigma_a = 70, sigma0 = 60, n = 25)


#############################################################
# power_var_ratio_F(
#   sigma_a,
#   ratio0 = 1,
#   n,
#   alpha = 0.05,
#   alternative = c("two.sided", "less", "greater"),
#   digits = 4,
#   quiet = FALSE
# )
#############################################################

power_var_ratio_F(sigma_a = c(10, 15), ratio0 = 1, n = c(25, 25),
                  alternative = "two.sided")

#############################################################
# 5) REQUIRED SAMPLE SIZE FOR TARGET POWER
#############################################################

#############################################################
# n_required_from_power(
#   power_at_n,
#   target_power,
#   n_min = 1L,
#   n_max = 1e6L,
#   digits = 4,
#   quiet = FALSE
# )
#############################################################

toy_power <- function(nn) 1 - exp(-nn / 50)
n_required_from_power(power_at_n = toy_power, target_power = 0.80, n_min = 1, n_max = 10000)


#############################################################
# n_required_z_mu(
#   mu_a,
#   mu0,
#   sigma,
#   alpha = 0.05,
#   beta_target = 0.10,
#   alternative = c("two.sided", "less", "greater"),
#   n_min = 1L,
#   n_max = 1e6L,
#   n_ratio = 1,
#   digits = 4,
#   quiet = FALSE
# )
#
# Structure:
#   one-sample : scalar mu_a, sigma
#   two-sample : length-2 mu_a, sigma
#   n_ratio = n2 / n1 for two-sample use
#############################################################

# One-sample required n
n_required_z_mu(mu_a = 30500, mu0 = 30000, sigma = 1500,
                alpha = 0.01, beta_target = 0.05,
                alternative = "greater")

# Two-sample required n
n_required_z_mu(mu_a = c(65, 63), mu0 = 0, sigma = c(3, 2.5),
                alpha = 0.05, beta_target = 0.10,
                alternative = "two.sided", n_ratio = 1)


#############################################################
# n_required_t_mu(
#   mu_a,
#   mu0,
#   sigma_true,
#   alpha = 0.05,
#   beta_target = 0.10,
#   alternative = c("two.sided", "less", "greater"),
#   n_min = 2L,
#   n_max = 1e6L,
#   paired = FALSE,
#   method = c("welch", "pooled"),
#   n_ratio = 1,
#   digits = 4,
#   quiet = FALSE
# )
#
# Use guide:
#   method = "welch"  -> two-sample unequal-variance planning
#   method = "pooled" -> two-sample equal-variance planning
#   paired = TRUE     -> paired t design on differences
#############################################################

# One-sample required n
n_required_t_mu(mu_a = 2.0, mu0 = 3.0, sigma_true = 1.5,
                alpha = 0.05, beta_target = 0.10, alternative = "less")

# Two-sample Welch required n
n_required_t_mu(mu_a = c(10, 8), mu0 = 0, sigma_true = c(3, 2.5),
                alpha = 0.05, beta_target = 0.10,
                alternative = "two.sided", method = "welch", n_ratio = 1)

# Two-sample pooled required n
n_required_t_mu(mu_a = c(10, 8), mu0 = 0, sigma_true = c(3, 3),
                alpha = 0.05, beta_target = 0.10,
                alternative = "two.sided", method = "pooled", n_ratio = 1)

# Paired required n
n_required_t_mu(mu_a = -1.2, mu0 = 0, sigma_true = 2.3,
                alpha = 0.05, beta_target = 0.10,
                alternative = "two.sided", paired = TRUE)


#############################################################
# n_required_p_z(
#   p_a,
#   p0,
#   alpha = 0.05,
#   beta_target = 0.10,
#   alternative = c("two.sided", "less", "greater"),
#   n_min = 5L,
#   n_max = 1e7L,
#   pooled = NULL,
#   continuity = FALSE,
#   n_ratio = 1,
#   digits = 4,
#   quiet = FALSE
# )
#
# Structure:
#   one-sample : scalar p_a
#   two-sample : length-2 p_a, p0 = null difference
#   n_ratio = n2 / n1 for two-sample planning
#############################################################

# One-sample required n
n_required_p_z(p_a = 0.15, p0 = 0.10,
               alpha = 0.05, beta_target = 0.10,
               alternative = "greater")

# Two-sample required n: default behavior
n_required_p_z(p_a = c(0.20, 0.30), p0 = 0,
               alpha = 0.05, beta_target = 0.10,
               alternative = "two.sided", n_ratio = 1)

# Two-sample required n: explicitly pooled
n_required_p_z(p_a = c(0.20, 0.30), p0 = 0,
               alpha = 0.05, beta_target = 0.10,
               alternative = "two.sided", pooled = TRUE, n_ratio = 1)

# Two-sample required n: explicitly unpooled
n_required_p_z(p_a = c(0.20, 0.30), p0 = 0,
               alpha = 0.05, beta_target = 0.10,
               alternative = "two.sided", pooled = FALSE, n_ratio = 1)
#############################################################
# n_required_var_chisq(
#   sigma_a,
#   sigma0,
#   alpha = 0.05,
#   beta_target = 0.10,
#   alternative = c("two.sided", "less", "greater"),
#   n_min = 2L,
#   n_max = 1e6L,
#   digits = 4,
#   quiet = FALSE
# )
#############################################################

n_required_var_chisq(sigma_a = 70, sigma0 = 60,
                     alpha = 0.05, beta_target = 0.10,
                     alternative = "greater")


#############################################################
# n_required_var_ratio_F(
#   sigma_a,
#   ratio0 = 1,
#   alpha = 0.05,
#   beta_target = 0.10,
#   alternative = c("two.sided", "less", "greater"),
#   n_min = 2L,
#   n_max = 1e6L,
#   n_ratio = 1,
#   digits = 4,
#   quiet = FALSE
# )
#############################################################

n_required_var_ratio_F(sigma_a = c(23, 50), ratio0 = 1,
                       alpha = 0.05, beta_target = 0.10,
                       alternative = "two.sided", n_ratio = 1)


#############################################################
# 6) DESCRIPTIVE SHAPE
#############################################################

#############################################################
# skew(
#   x,
#   method = c("moment", "adjusted"),
#   na_rm = FALSE,
#   digits = 4,
#   quiet = FALSE
# )
#############################################################

skew(c(1, 1, 2, 2, 3, 9))
skew(c(1, 1, 2, 2, 3, 9), method = "adjusted")


#############################################################
# kurt(
#   x,
#   method = c("moment", "adjusted"),
#   excess = FALSE,
#   na_rm = FALSE,
#   digits = 4,
#   quiet = FALSE
# )
#############################################################

kurt(c(-10, 0, 0, 0, 0, 0, 10))
kurt(c(-10, 0, 0, 0, 0, 0, 10), excess = TRUE)
kurt(c(1, 1, 2, 2, 3, 9), method = "adjusted", excess = TRUE)
