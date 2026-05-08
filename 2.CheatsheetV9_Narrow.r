############################################################
# StatsPackage Narrow Cheat Sheet
#
# Purpose:
#   Install/import StatsPackage and demonstrate only the core
#   workflows needed for:
#   1. sample variance
#   2. one-sample t confidence interval for mu
#   3. Welch two-sample t test for mu1 - mu2
################################

# Setup/update block. Keep this if you are not sure the package is installed.
required_version <- "1.1.0"

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

if (!requireNamespace("StatsPackage", quietly = TRUE) ||
    utils::packageVersion("StatsPackage") < package_version(required_version)) {
  remotes::install_github(
    "christopherposadasp-ctrl/StatsRPackage",
    subdir = "StatsPackage -1.0",
    ref = required_version,
    upgrade = "never"
  )
}

library(StatsPackage)

############################################################
# Workflow 1: Sample Variance
#
# Formula:
#   s^2 = (1 / (n - 1)) * sum((Yi - Ybar)^2)
#
# Base R:
#   var(y) calculates sample variance using denominator n - 1.
#   sd(y) is sqrt(var(y)).
############################################################

y <- c(10.2, 9.8, 10.5, 11.1, 9.7, 10.4)  # Sample observations.

n <- length(y)                              # Sample size.
ybar <- mean(y)                             # Sample mean.
s2_manual <- sum((y - ybar)^2) / (n - 1)    # Sample variance formula.
s2_base <- var(y)                           # Base R sample variance.
s <- sd(y)                                  # Sample standard deviation.

cat("\nSample variance workflow\n")
cat("n =", n, "\n")
cat("Ybar =", round(ybar, 4), "\n")
cat("s^2 from formula =", round(s2_manual, 4), "\n")
cat("s^2 from var(y)  =", round(s2_base, 4), "\n")
cat("s from sd(y)     =", round(s, 4), "\n")

stopifnot(isTRUE(all.equal(s2_manual, s2_base)))

############################################################
# Workflow 2: One-Sample t Confidence Interval For mu
#
# Formula:
#   Ybar +/- t_(alpha/2, n-1) * s / sqrt(n)
#
# Use when:
#   one sample
#   sigma is unknown
#   sample SD s is used
#
# StatsPackage:
#   ci_mu(xbar = ..., n = ..., s = ...)
############################################################

ci_mu(
  xbar = ybar,        # sample mean
  n = n,              # sample size
  s = s,              # sample SD; use s because sigma is unknown
  conf.level = 0.95
)

# Optional base R cross-check when raw data are available.
# This also prints a hypothesis test, so leave it commented unless needed.
# t.test(y, conf.level = 0.95)

############################################################
# Workflow 3: One-Sample t Prediction Interval
#
# Formula:
#   Ybar +/- t_(alpha/2, n-1) * s * sqrt(1 + 1/n)
#
# Use when:
#   predicting one future observation from the same population/process
#   sigma is unknown
#   sample SD s is used
#
# StatsPackage:
#   pi_mu(xbar = ..., n = ..., s = ...)
#
# Important:
#   This is not a confidence interval for mu. It predicts a future value,
#   so it is wider than the confidence interval for the mean.
############################################################

pi_mu(
  xbar = ybar,        # sample mean
  n = n,              # sample size
  s = s,              # sample SD; use s because sigma is unknown
  conf.level = 0.95
)

############################################################
# Workflow 4: Welch Two-Sample t Test For mu1 - mu2
#
# Formula:
#   T = (Ybar1 - Ybar2 - Delta0) / sqrt(s1^2/n1 + s2^2/n2)
#
# Use when:
#   two independent samples
#   sigma1 and sigma2 are unknown
#   equal variances are NOT assumed
#
# StatsPackage:
#   t_test_mu(..., var.equal = FALSE)
#
# Key arguments:
#   xbar = c(mean group 1, mean group 2)
#   s    = c(SD group 1, SD group 2)
#   n    = c(n group 1, n group 2)
#   mu0  = null value for mu1 - mu2, usually 0
############################################################

group1 <- c(102, 98, 105, 110, 101, 99)
group2 <- c(95, 100, 96, 98, 97)

xbar_2 <- c(mean(group1), mean(group2))
s_2 <- c(sd(group1), sd(group2))
n_2 <- c(length(group1), length(group2))

t_test_mu(
  xbar = xbar_2,
  mu0 = 0,            # H0: mu1 - mu2 = 0
  s = s_2,
  n = n_2,
  var.equal = FALSE,  # Welch test; do not assume equal variances
  alternative = "two.sided",
  alpha = 0.05
)

# Matching Welch confidence interval for mu1 - mu2:
ci_mu(
  xbar = xbar_2,
  n = n_2,
  s = s_2,
  method = "welch",
  conf.level = 0.95
)

# Optional base R cross-check when raw data are available.
# t.test(group1, group2, var.equal = FALSE, conf.level = 0.95)
