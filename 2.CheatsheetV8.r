# setup/update only. After StatsPackage is installed once,
# you can delete or comment out this block.


required_version <- "0.1.4"

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

if (!requireNamespace("StatsPackage", quietly = TRUE) ||
    utils::packageVersion("StatsPackage") < required_version) {
  remotes::install_github(
    "christopherposadasp-ctrl/StatsRPackage",
    subdir = "StatsPackage -1.0",
    ref = required_version
  )
}
# you can delete block after you install

#############################################################
# StatsPackage Cheat Sheet
library(StatsPackage)
#############################################################
# Optional local validation:
# devtools::test(pkg = "StatsPackage -1.0")
defs <- list(

  `1) Descriptive Statistics` = c(
    "Population (N): the full group of interest.",
    "Sample (n): a subset of the population, selected to draw inferences about the population.",
    "Parameter (Î¼, ÏƒÂ², p, Î¸): a numerical characteristic of the population distribution; fixed but unknown.",
    "Statistic (XÌ„, SÂ², pÌ‚, Î¸Ì‚): a function of observable random variables in a sample and known constants.",
    "Data types: numerical can be discrete (counts) or continuous (measurements).",
    "Measures of location: mean, median, mode. Median is more robust to outliers than mean.",
    "Percentiles/quantiles: p-th percentile has p% of data at or below it; p-th quantile is the same idea on a 0â€“1 scale.",
    "Quartiles/IQR: Q1 = 25th percentile, Q3 = 75th percentile; IQR = Q3 âˆ’ Q1.",
# Data Types:
    "Discrete: Countable numeric values, usually whole-number counts.",
    "Continuous: Numeric values measured on a continuum; can take any value in an interval.",
    "Nominal: Categories with names/labels only; no natural order.",
    "Ordinal: Categories with a meaningful order/rank, but differences between levels are not numerically meaningful.",
    "Qualitative: Non-numeric, descriptive/categorical data.",
    "Quantitative: Numeric data representing counts or measurements.",
    "Histogram: for numerical (continuous) data with adjacent bins.",
    "Bar plot: for categorical data with separated bars."
  ),

  `2) Sampling Distributions & CLT` = c(
    "Simple Random Sample (SRS) / iid: X1,â€¦,Xn are independent and identically distributed from the same population distribution (same Î¼, ÏƒÂ²).",
    "Other samples: stratified, cluster, systematic.",
    "Sampling distribution: the probability distribution of a statistic over repeated SRS of size n.",
    "Standard error (SE): the SD of a statistic (e.g., SE(XÌ„) = Ïƒ/âˆšn). Estimated SE plugs in s for Ïƒ.",
    "Linear combination: a1X1 + â‹¯ + anXn for constants ai (special cases: sum, mean).",
    "Normal algebra (closure): linear combinations of independent normal RVs are normal. If X ~ N(Î¼, ÏƒÂ²), then aX + b ~ N(aÎ¼ + b, aÂ²ÏƒÂ²).",
    "Central limit theorem (CLT): for large n, sums/means of iid RVs are approximately normal, regardless of population shape (approx improves as n increases).",
    "Normal approximation to Binomial: if large-enough conditions hold, Bin(n,p) â‰ˆ N(np, np(1-p)); continuity correction Â±0.5."
  ),

  `3) Point Estimation` = c(
    "Parameter vs estimator vs estimate: Î¸ is fixed (unknown). Î¸Ì‚ is a statistic (random variable). An estimate is the realized numeric value after observing data.",
    "Estimator: a rule/formula that tells how to calculate an estimate from sample measurements.",
    "Unbiased / bias: Î¸Ì‚ is unbiased if E[Î¸Ì‚] = Î¸ for all Î¸.",
    "Standard error (SE): SE(Î¸Ì‚) = SD(Î¸Ì‚). Estimated SE plugs in sample estimates for unknown parameters.",
    "Mean squared error (MSE): measures overall accuracy (variance + biasÂ²). Lower MSE is better.",
    "Minimum Variance Unbiased Estimator (MVUE): unbiased estimator with minimum variance among unbiased estimators of Î¸.",
    "Method of Moments (MoM): solve for parameters by matching population moments E[X^k] to sample moments (1/n)âˆ‘X_i^k (not guaranteed unbiased).",
#   "Likelyhood:The likelihood function is the joint distribution viewed as a function of the unknown parameters given the known data." this is the same as Likelihood function
    "Likelihood function: the joint distribution viewed as a function of the unknown parameters given the known data.",
    "Maximum Likelihood Estimator (MLE): choose parameter(s) maximizing likelihood L(Î¸|x) (often maximize log-likelihood).",
    "Invariance of MLE: if Î¸Ì‚ is MLE of Î¸ and h is one-to-one, then h(Î¸Ì‚) is MLE of h(Î¸).",
    "Memoryless property: only geometric and exponential distributions have it (past doesnâ€™t matter given survival)."
  ),

  `4) Interval Estimation / Confidence Intervals` = c(
    "Confidence interval: a random interval [L(X), U(X)] such that P(L(X) â‰¤ Î¸ â‰¤ U(X)) = 1 âˆ’ Î±, where 1 âˆ’ Î± is the confidence level.",
    "Correct interpretation: Î¸ is fixed (unknown); the interval is random before sampling. After computing it, the realized interval is fixed and either contains Î¸ or it doesnâ€™t.",
    "Effect of n: increasing n generally makes the CI narrower (precision â†‘) but does not change the nominal confidence level (coverage stays 1 âˆ’ Î± if assumptions hold).",
    "CI vs prediction interval: a CI for Î¼ is not about a future value X_(n+1). A prediction interval targets X_(n+1) and is typically wider.",
    "Pivotal quantity: a function of the sample data and Î¸, where Î¸ is the only unknown quantity. Moreover, the probability distribution of the pivotal quantity is known and does not depend on Î¸."
  ),

  `5) Hypothesis Testing` = c(
    "Statistical hypothesis: a statement about the value of one or more population characteristics (e.g., parameters such as Î¼ = Î¼0).",
    "Type I error (Î±): rejecting H0 when H0 is true.",
    "Type II error (Î²): failing to reject H0 when H0 is false (i.e., Ha is true).",
    "Test statistic: a function of the data (with known distribution if H0 is true) used to decide whether to reject H0.",
    "Rejection region: the set of test statistic values for which H0 will be rejected.",
    "P-value: the probability, assuming H0 is true, of observing a test statistic as extreme or more extreme than what was observed.",
    "Significance level (Î±): acceptable Type I error rate; reject H0 iff p-value < Î±.",
    "Power: P(reject H0 | a specified alternative Ha is true) = 1 âˆ’ Î² (viewed as a function of the Ha parameter)."
  ),
    `6) Nonparametric Methods & Bootstrap` = c(
    "Bootstrap / bootstrapping: a resampling method that estimates the sampling distribution of a statistic by repeatedly sampling with replacement from the observed data.",
    "Bootstrap sample (x*): a sample of size n drawn with replacement from the original data.",
    "Bootstrap replication (Î¸Ì‚*): the value of the statistic computed from one bootstrap sample; repeating this many times approximates the sampling distribution.",
    "Nonparametric / distribution-free: methods that do not assume a specific population distribution; often based on ranks rather than raw values.",
    "Wilcoxon signed-rank test: a nonparametric alternative to the one-sample or paired t-test; tests whether the population median (or median difference) equals a hypothesized value.",
    "Wilcoxon rank-sum test (Mann-Whitney): a nonparametric alternative to the two-sample t-test; tests whether two independent populations have the same location."
  ),
  `7) Data Acquisition / Test Design` = c(
      "Anecdotal: Haphazard collection of individual cases.",
      "Observational: You collect the data, often retrospectively, but don't design who or what gets the treatment/intervention and who doesn't.",
      "Designed (controlled) experiment: Stebbing (1961): An experiment is \"deliberate observation under conditions deliberately arranged by the observer.\" An experiment involves an active imposition of a treatment or intervention.",
      "Experimental units (EUs): the objects on which the experiment is performed.",
      "Treatment (sometimes called an intervention): the experimental condition applied to the EUs, e.g., type of gas, dose of drug, training method, etc.",
      "Bias: A study is biased if it (usually unintentionally) systematically favors certain outcomes. When a selection system is biased, taking more samples does not help!"
    ),
#sd of a sample = 
    `8) Bias, Confounding, and DOE Controls` = c(
  #    "Selection bias (the catchall): A systematic tendency of the sampling procedure that results in the sample being systematically different than the target population.",
      "Non-response bias: Those sampled are not representative of the target population.",
      "Response bias (social desirability): People have a propensity to respond to what they think the experimenter wants to hear.",
      "Experimenter bias: A tendency for those administering the experiment to favor (perhaps unintentionally) some members of the target population.",
      "Hawthorne effect: People who know they are being experimented on act differently, i.e., the observer effect.",
      "Placebo effect: Placebos often do improve reported health conditions. A placebo is a simulated or otherwise medically ineffectual treatment for a disease or other medical condition intended to deceive the recipient.",
      "Goodhart's law: \"When a measure becomes a target, it ceases to be a good measure.\"",
      "Confounding (or lurking) variables: Variables that affect the response that if unaccounted for can cause misleading outcomes.",
      "Confound: Webster - \"to mistake one thing for another.\" Two factors are confounded if you can't tell which of them might have caused the change in the response.",
      "Simpson's paradox: a phenomenon in statistics in which a trend appears in several groups of data but disappears or even reverses when the groups are combined.",
      "Control group: a set of experimental units who are used to compare with the treatment/intervention groups, e.g., given a placebo or the current best practice.",
      "Block: a group of EUs that are thought to be similar in manners that will affect the response. The idea is to assign the EUs to treatments so that each treatment group is as similar as possible (to control for outside effects). Blocking is the first fundamental principle of DOE.",
      "Randomization: when the EUs are assigned to treatments (and the control group) randomly (within blocks). Randomization is used to control for biases and confounding variables beyond those accounted for in blocks. 2nd fundamental principle of DOE.",
      "Replication: taking multiple experiments under identical initial conditions - used to measure and reduce variability. 3rd fundamental principle of DOE.",
      "Blinding: An experiment is blind if those receiving the treatment do not know if they are getting the treatment or the control.",
      "Double blind: An experiment is double blind if those evaluating the condition do not know if the subjects (or EUs) are in the treatment or control group."
    )
)
pnorm(.5)
qnorm(.5)
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
#   quiet = FALSE)
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
ci_mu(xbar =375, n=25, sigma=20, conf.level = 0.90)

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

# One-sided upper CI
ci_mu(xbar = 5.2, n = 25, s = 1.8, side = "upper")

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
#   s,                 # sample SD(s): scalar (one-sample) or length-2 (two-sample)
#   n,                 # sample size(s) matching s
#   conf.level = 0.95, # confidence level
#   digits = 4,        # printed decimal places
#   quiet = FALSE      # FALSE prints interpretation, TRUE returns silently
# )
#
# Structure:
#   one-sample : scalar s and n -> CI for sigma^2 and sigma
#   two-sample : length-2 s and n -> CI for sigma1^2 / sigma2^2
#############################################################
ci_var(s = 4.2, n = 12)  # one-sample CI for variance and SD

# Two-sample (ratio sigma1^2 / sigma2^2)
ci_var(s = c(4.2, 3.1), n = c(12, 10), conf.level = 0.95)  # CI for sigma1^2 / sigma2^2
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
sum(7,3,4,1,10)
ci_lambda_exp(Sum = 25, n = 5, conf.level = 0.90)
1/.0788
1/.3661
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

n_width_mu_z(w = 20, sigma = 20, conf.level = 0.95)  # z-based width planning when sigma is known
# strong negative coralation means 
#############################################################
## Mu target width T----
# n_width_mu_t(
#   w,                 # target TOTAL CI width
#   s,                 # planning SD (pilot or prior estimate)
#   conf.level = 0.95, # confidence level for the future CI
#   n_start = NULL,    # optional starting n for the t-critical iteration
#   max_iter = 100,    # iteration cap for convergence
#   digits = 4,        # printed decimal places
#   quiet = FALSE      # FALSE prints interpretation, TRUE returns silently
# )
#
# Use when sigma is unknown and s is your planning SD.
# w = TOTAL CI width.
# n_start can be used to seed the iteration.
# max_iter controls how long the t-critical iteration is allowed to run.
#############################################################

n_width_mu_t(w = 4, s = 15)  # t-based width planning when sigma is unknown


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
n_width_p_wald(w = 0.10, worst_case = TRUE, conf.level = 0.95)

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
t_test_mu(xbar= 10.4, mu0 = 10, s = .5, n = 5, alpha = 0.01, alternative = "greater")
pt(1.78, df=4, lower.tail = FALSE)
# One-sample one-sided t test
t_test_mu(xbar = 5.4, mu0 = 5, s = 1.2, n = 20, alternative = "greater")

# Two-sample Welch t test
t_test_mu(xbar = c(10, 8), mu0 = 0, s = c(3, 2.5), n = c(20, 18), var.equal = FALSE)
t_test_mu(xbar = c(110, 120), mu0 = -4, s = c(5, 6), n = c(9, 7), var.equal = FALSE, alternative = "less", alpha = 0.05)
ci_mu(xbar = c(110, 120), n = c(9, 7), s = c(5, 6), method = "welch")
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
qnorm(.95, lower.tail = FALSE)
qnorm(0.05)
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

# One-sample one-sided exact test
p_test(x = 21, n = 100, p0 = 0.20, alternative = "greater")
p_test(x = 8, n = 10, p0 = 0.60, alternative = "greater")
# Two-sample equality test: default pooled, no continuity correction
p_test(x = c(35, 24), n = c(100, 100), p0 = 0)
p_test(x = c(344, 156), n = c(4000, 4000), p0 = 0, pooled = FALSE)
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
var_test_chisq(s = c(52.6, 84.2), n = c(8, 6), ratio0 = 1, alternative = "less", alpha = 0.1)
# One-sample one-sided variance test
var_test_chisq(s = 4.2, n = 12, sigma0 = 5, alternative = "greater")

# Two-sample one-sided F test
var_test_chisq(s = c(4.2, 5.0), n = c(12, 10), ratio0 = 1, alternative = "less")

#############################################################
# wilcox.test(
#   x,
#   y = NULL,
#   alternative = c("two.sided", "less", "greater"),
#   mu = 0,
#   paired = FALSE,
#   exact = NULL,   leave Null so R decides based on data size
#   correct = TRUE,  apply continuity correction for ties and discrete data
#   conf.int = FALSE,
#   conf.level = 0.95
# )
# Nonparametric location test.
#
# Use for:
#   one-sample : Wilcoxon signed-rank test:                x = sample vector, y = NULL, mu = hypothesized value
#   paired     : Wilcoxon signed-rank test on differences: x and y are matched samples, paired = TRUE
#   two-sample : Wilcoxon rank-sum / Mannâ€“Whitney test:    x and y are independent samples, paired = FALSE
#############################################################

x <- c(30.6, 30.1, 15.6, 26.7, 27.1)
y <- c(55.0, 55.7, 62.9, 45.5, 51.1)
# one-sample signed-rank
wilcox.test(x, mu = 30, alternative = "less")
# paired signed-rank
wilcox.test(x, y, paired = TRUE)
# two-sample rank-sum / Mannâ€“Whitney
wilcox.test(x, y)

x <- c(30.6, 30.1, 15.6, 26.7, 27.1, 25.4, 35.0, 30.8,
       31.9, 53.2, 12.5, 23.2, 8.8, 24.9, 30.2)

wilcox.test(x, mu = 30, alternative = "less", conf.int = TRUE, conf.level = 0.90)

grav <- c(54.7, 58.5, 66.8, 46.1, 52.3, 74.3, 92.5, 40.2,
          87.3, 74.8, 63.2, 68.5)

spec <- c(55.0, 55.7, 62.9, 45.5, 51.1, 75.4, 89.6, 38.4,
          86.8, 72.5, 62.3, 66.0)

wilcox.test(grav, spec, paired = TRUE, alternative = "two.sided")

adh1 <- c(229, 286, 245, 299, 250)
adh2 <- c(213, 179, 163, 247, 225)

wilcox.test(adh1, adh2, alternative = "greater")

Prior <- c(29,98,49,61,13,92,20,85,39,66)

Post <- c(33,91,37,49,5,80,9,74,31,59)
cor(Prior, Post)
wilcox.test(Prior, Post, paired = TRUE,  conf.level = 0.95, exact = FALSE, correct = TRUE)
wilcox.test(
  Prior,
  Post,
  paired = TRUE,
  alternative = "greater",
  exact = FALSE,
  correct = TRUE, conf.level = 0.95
)
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
  observed = c(7, 15, 8),               # observed counts in each category
  p = c(1/3, 1/3, 1/3),                   # null/theoretical category probabilities
  labels = c("0-1", "1-2",      # names printed with the output table
             "2-3"), alpha = 0.10, digits = 3
)
y
#############################################################
# chisq_gof_dist(
#   x = NULL,
#   observed = NULL,
#   dist = c("exp", "norm", "pois", "unif"),
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
#   dist = "unif"            -> continuous uniform GOF on [min, max]
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

# Uniform GOF from raw data (estimate min and max from x)
chisq_gof_dist(
  x = c(0.02, 0.05, 0.08, 0.15,
        0.28, 0.31, 0.33, 0.37, 0.41, 0.45,
        0.53, 0.56, 0.59, 0.63, 0.68, 0.72,
        0.76, 0.79, 0.83, 0.87, 0.89, 0.91, 0.95, 0.99),
  dist = "unif",
  k = 4,
  estimate = TRUE
)
x <- c(0.074, 0.375, 0.558, 0.559, 0.584, 0.768, 0.935, 1.077, 1.080, 1.182, 1.193, 1.193, 1.207, 1.234, 1.264, 1.278, 1.288, 1.479, 1.639, 1.806, 1.847, 1.904, 2.102, 2.170, 2.183, 2.256, 2.260, 2.275, 2.507, 2.903)
x
sqrt(.75)
y<- c(0.074, 0.375, 0.558, 0.559, 0.584, 0.768, 0.935, 1.077, 1.080, 1.182, 1.193, 1.193, 1.207, 1.234, 1.264, 1.278, 1.288, 1.479, 1.639, 1.806, 1.847, 1.904, 2.102, 2.170, 2.183, 2.256, 2.260, 2.275, 2.507, 2.903)
y
chisq_gof_dist(
  x = y,
  dist = "unif",
  breaks = c(0, 1, 2, 3),
  params = list(min = 0, max = 3)
)
# Poisson GOF from grouped count frequencies
chisq_gof_dist(
  observed = c(1627, 421, 219, 130, 107, 51, 15, 22,
               8, 14, 5, 8, 5, 0, 3, 2),
  dist = "pois",
  estimate = TRUE
)

x <- c(0.682,1.160,1.476,0.243,.122,1.318,0.872,.144,1.325,0.182,0.093,1.859,1.302,1.059, 
.191,1.669,2.000,0.053,1.283,1.634,0.840,.152,0.900,3.044,.264,0.872,2.599,0.584,1.053,1.025)
chisq_gof_dist(
  x = x,
  dist = "exp",
  k = 3,
  estimate = TRUE,
  alpha = 0.10
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

# Row / column / overall examples share the same table
smoking_tab <- matrix(
  c(25, 10,
    24, 32,
    28, 17,
    19, 34),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(c("<16", "16-17", "18-20", ">20"), c("Male", "Female"))
)
UAV_tab <- matrix(
  c(37, 23,
    11, 15),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(c("Success", "Failed"), c("AOR_1", "AOR_2"))
)
Class_tab <- matrix(
  c(187, 112, 186,
    117, 163, 526),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(c("survived", "Died"), c("1st Class", "2nd Class", "3rd Class"))
)
# Row proportions
table_props(Class_tab, margin = "overall")
table_props(UAV_tab, margin = "col")
# Column proportions
table_props(smoking_tab, margin = "col")
table_props(smoking_tab, margin = "row")

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
chisq_table(UAV_tab,type = "independence")

spirituality_tab <- matrix(
  c(56, 162, 198, 211,
    56, 223, 243, 239,
    109, 164, 74, 28),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(c("N.S.", "S.S.", "G.D."), c("Very", "Moderate", "Slightly", "Not at all"))
)
chisq_table(Class_tab,type = "independence", alpha = 0.01)
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

# One-sample z power
power_z_mu(mu_a = 10.4, mu0 = 10, sigma = .5, n = 5, alpha = 0.01, alternative = "greater")

# Two-sample z power (known sigmas)
power_z_mu(mu_a = c(82, 77), mu0 = 0, sigma = c(12, 10), n = c(64, 49),
           alternative = "two.sided")

# One-sample one-sided z power
power_z_mu(mu_a = 30500, mu0 = 30000, sigma = 1500, n = 75,
           alpha = 0.01, alternative = "greater")
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
power_t_mu(mu_a = 10.4, mu0 = 10, sigma_true = 1.2, n = 20)

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

# Two-sample power: default behavior under p0 = 0 (pooled, no continuity correction)
power_p_z(p_a = c(0.20, 0.30), p0 = 0, n = c(150, 150))

# Two-sample power: explicitly unpooled
power_p_z(p_a = c(0.20, 0.30), p0 = 0, n = c(150, 150), pooled = FALSE)

# Two-sample power with continuity correction.
# Appropriate when n is moderate and a more conservative normal approximation is preferred.
power_p_z(p_a = c(0.20, 0.30), p0 = 0, n = c(40, 40), continuity = TRUE)

# Nonzero null difference: default unpooled, no continuity correction
power_p_z(p_a = c(0.35, 0.24), p0 = 0.05, n = c(100, 100))

# Two-sample one-sided power
power_p_z(p_a = c(0.35, 0.24), p0 = 0, n = c(100, 100), alternative = "greater")
#############################################################
# power_var_chisq(
#   sigma_a,                                 # true SD under Ha
#   sigma0,                                  # SD under H0
#   n,                                       # one-sample n
#   alpha = 0.05,                            # Type I error rate
#   alternative = c("two.sided", "less", "greater"), # test direction
#   digits = 4,                              # printed decimal places
#   quiet = FALSE                            # FALSE prints interpretation
# )
# Use guide:
#   one-sample variance power under normality.
#   sigma_a = true SD under Ha; sigma0 = null SD in H0.
#   use "greater" when sigma_a > sigma0, "less" when sigma_a < sigma0.
#############################################################

power_var_chisq(sigma_a = 70, sigma0 = 60, n = 25)  # two-sided one-sample variance power

# One-sided variance power
power_var_chisq(sigma_a = 70, sigma0 = 60, n = 25, alternative = "greater")  # right-tail alternative


#############################################################
# power_var_ratio_F(
#   sigma_a,                                 # c(sigma1_a, sigma2_a) under Ha
#   ratio0 = 1,                              # null ratio sigma1^2 / sigma2^2
#   n,                                       # c(n1, n2)
#   alpha = 0.05,                            # Type I error rate
#   alternative = c("two.sided", "less", "greater"), # test direction
#   digits = 4,                              # printed decimal places
#   quiet = FALSE                            # FALSE prints interpretation
# )
# Use guide:
#   two-sample variance-ratio power under normality.
#   sigma_a = c(sigma1_a, sigma2_a); ratio0 is the null sigma1^2 / sigma2^2.
#   n = c(n1, n2); unbalanced n changes df and power.
#############################################################

power_var_ratio_F(sigma_a = c(10, 15), ratio0 = 1, n = c(25, 25),
                  alternative = "two.sided")  # two-sided F-ratio power

# One-sided F-ratio power
power_var_ratio_F(sigma_a = c(10, 15), ratio0 = 1, n = c(25, 25),
                  alternative = "less")  # left-tail alternative

#############################################################
# 5) REQUIRED SAMPLE SIZE FOR TARGET POWER
#############################################################

#############################################################
# n_required_from_power(
#   power_at_n,       # function: integer n -> scalar power in [0, 1]
#   target_power,     # desired minimum power
#   n_min = 1L,       # lower bound of integer search
#   n_max = 1e6L,     # upper bound of integer search
#   digits = 4,       # printed decimal places
#   quiet = FALSE     # FALSE prints interpretation
# )
# Use guide:
#   power_at_n must accept integer n and return one power value in [0, 1].
#   target_power is the desired minimum power.
#   returns the smallest n in [n_min, n_max] that meets target_power.
#############################################################

toy_power <- function(nn) 1 - exp(-nn / 50)  # monotone toy power curve for demonstration
n_required_from_power(power_at_n = toy_power, target_power = 0.80, n_min = 1, n_max = 10000)  # generic integer solver


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

n_required_z_mu(mu_a = 10.5, mu0 = 10, sigma = 0.6,
                alpha = 0.01, beta_target = 0.2,
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

# Two-sample required n: explicitly unpooled
n_required_p_z(p_a = c(0.20, 0.30), p0 = 0,
               alpha = 0.05, beta_target = 0.10,
               alternative = "two.sided", pooled = FALSE, n_ratio = 1)

# Two-sample required n with continuity correction.
# Appropriate when n is moderate and conservative planning is preferred.
n_required_p_z(p_a = c(0.20, 0.30), p0 = 0,
               alpha = 0.05, beta_target = 0.10,
               alternative = "two.sided", continuity = TRUE, n_ratio = 1)

# Two-sample required n for a nonzero null difference (defaults unpooled)
n_required_p_z(p_a = c(0.35, 0.24), p0 = 0.05,
               alpha = 0.05, beta_target = 0.10,
               alternative = "two.sided")
#############################################################
# n_required_var_chisq(
#   sigma_a,                                 # true SD under Ha
#   sigma0,                                  # SD under H0
#   alpha = 0.05,                            # Type I error rate
#   beta_target = 0.10,                      # target Type II error (1 - power)
#   alternative = c("two.sided", "less", "greater"), # test direction
#   n_min = 2L,                              # lower bound for feasible n
#   n_max = 1e6L,                            # upper search bound
#   digits = 4,                              # printed decimal places
#   quiet = FALSE                            # FALSE prints interpretation
# )
# Use guide:
#   one-sample required n for chi-square variance tests.
#   alternative = "greater" requires sigma_a > sigma0.
#   alternative = "less" requires sigma_a < sigma0.
#############################################################

n_required_var_chisq(sigma_a = 70, sigma0 = 60,
                     alpha = 0.05, beta_target = 0.10,
                     alternative = "greater")  # right-tail design target

# Required n for one-sided lower-variance alternative
n_required_var_chisq(sigma_a = 50, sigma0 = 60,
                     alpha = 0.05, beta_target = 0.10,
                     alternative = "less")  # left-tail design target


#############################################################
# n_required_var_ratio_F(
#   sigma_a,                                 # c(sigma1_a, sigma2_a) under Ha
#   ratio0 = 1,                              # null ratio sigma1^2 / sigma2^2
#   alpha = 0.05,                            # Type I error rate
#   beta_target = 0.10,                      # target Type II error (1 - power)
#   alternative = c("two.sided", "less", "greater"), # test direction
#   n_min = 2L,                              # lower bound for feasible n
#   n_max = 1e6L,                            # upper search bound
#   n_ratio = 1,                             # n2 / n1 planning ratio
#   digits = 4,                              # printed decimal places
#   quiet = FALSE                            # FALSE prints interpretation
# )
# Use guide:
#   two-sample required n for F-ratio tests.
#   sigma_a = c(sigma1_a, sigma2_a), ratio0 is the null sigma1^2 / sigma2^2.
#   n_ratio = n2 / n1 lets you plan unbalanced samples.
#   alternative = "less" requires true ratio < ratio0; "greater" requires > ratio0.
#############################################################

n_required_var_ratio_F(sigma_a = c(23, 50), ratio0 = 1,
                       alpha = 0.05, beta_target = 0.10,
                       alternative = "two.sided", n_ratio = 1)  # balanced two-sided design

# Required n for one-sided lower-ratio alternative with unbalanced design
n_required_var_ratio_F(sigma_a = c(20, 40), ratio0 = 1,
                       alpha = 0.05, beta_target = 0.10,
                       alternative = "less", n_ratio = 1.5)  # n2 = 1.5 * n1 planning


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


Super_Summary <- function(x, na_rm = TRUE, digits = 4) {
  if (!is.numeric(x)) {
    stop("Super_Summary(): x must be numeric.")
  }
  if (!is.logical(na_rm) || length(na_rm) != 1L || is.na(na_rm)) {
    stop("Super_Summary(): na_rm must be TRUE or FALSE.")
  }
  if (!is.numeric(digits) || length(digits) != 1L || !is.finite(digits) || digits < 0) {
    stop("Super_Summary(): digits must be a single nonnegative number.")
  }

  n_total <- length(x)
  n_missing <- sum(is.na(x))

  if (!na_rm && n_missing > 0L) {
    stop("Super_Summary(): x contains missing values; use na_rm = TRUE to remove them.")
  }

  x_used <- if (na_rm) x[!is.na(x)] else x
  n_used <- length(x_used)

  if (n_used == 0L) {
    stop("Super_Summary(): no non-missing observations are available.")
  }

  qu <- stats::quantile(x_used, probs = c(0.25, 0.50, 0.75), names = FALSE, type = 7)
  sd_x <- stats::sd(x_used)
  var_x <- stats::var(x_used)
  se_x <- if (is.finite(sd_x)) sd_x / sqrt(n_used) else NA_real_

  skew_x <- tryCatch(
    skew(x_used, na_rm = FALSE, quiet = TRUE)$estimate,
    error = function(e) NA_real_
  )
  kurt_x <- tryCatch(
    kurt(x_used, na_rm = FALSE, quiet = TRUE)$estimate,
    error = function(e) NA_real_
  )

  out <- list(
    counts = list(
      n_total = n_total,
      n = n_used,
      n_missing = n_missing,
      na_rm = na_rm
    ),
    location = c(
      mean = mean(x_used),
      median = stats::median(x_used)
    ),
    spread = c(
      sd = sd_x,
      var = var_x,
      se_mean = se_x,
      min = min(x_used),
      q1 = qu[1L],
      q3 = qu[3L],
      max = max(x_used),
      iqr = stats::IQR(x_used),
      range = max(x_used) - min(x_used)
    ),
    shape = c(
      skewness = skew_x,
      kurtosis = kurt_x
    ),
    other = c(
      sum = sum(x_used)
    )
  )

  fmt <- function(z) {
    if (is.na(z)) return("NA")
    if (is.nan(z)) return("NaN")
    if (is.infinite(z)) return(if (z > 0) "Inf" else "-Inf")
    formatC(z, format = "f", digits = as.integer(digits))
  }

  cat("\nSample summary statistics\n\n", sep = "")
  cat("Total length = ", out$counts$n_total, "\n", sep = "")
  cat("Used n = ", out$counts$n, "   Missing = ", out$counts$n_missing, "\n", sep = "")
  cat("na_rm = ", out$counts$na_rm, "\n\n", sep = "")

  cat("Location:\n", sep = "")
  cat(
    "mean = ", fmt(out$location["mean"]),
    "   median = ", fmt(out$location["median"]), "\n\n",
    sep = ""
  )

  cat("Spread:\n", sep = "")
  cat(
    "sd = ", fmt(out$spread["sd"]),
    "   var = ", fmt(out$spread["var"]),
    "   se_mean = ", fmt(out$spread["se_mean"]), "\n",
    sep = ""
  )
  cat(
    "min = ", fmt(out$spread["min"]),
    "   q1 = ", fmt(out$spread["q1"]),
    "   q3 = ", fmt(out$spread["q3"]),
    "   max = ", fmt(out$spread["max"]), "\n",
    sep = ""
  )
  cat(
    "IQR = ", fmt(out$spread["iqr"]),
    "   range = ", fmt(out$spread["range"]), "\n\n",
    sep = ""
  )

  cat("Shape:\n", sep = "")
  cat(
    "skewness = ", fmt(out$shape["skewness"]),
    "   kurtosis = ", fmt(out$shape["kurtosis"]), "\n\n",
    sep = ""
  )

  cat("Other:\n", sep = "")
  cat("sum = ", fmt(out$other["sum"]), "\n", sep = "")

  invisible(out)
}

x <- c(4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 18)
Super_Summary(x)

out <- chisq_gof_dist(
  x = seq(0.1, 3, length.out = 30),
  dist = "exp",
  k = 3,
  estimate = TRUE,
  quiet = TRUE
)

out$df
out$expected
#z.025 means
qnorm(0.025) 
qnorm(.975)
pnorm(1.96)-pnorm(-1.96)
qt(.99,30)
qf(.001,9,1)  

pchisq(15.987,10) - pchisq(2.588,10)
qf(.5, 30, 30) 

(((15/29)^2)*(58/15))+(((14/29)^2)*(58/14))+(((14/29)^2)*(15/29)*2*((58/14)+(29/15)))+(((15/29)^2)*(14/29)*2*((58/25)+(29/14)))
