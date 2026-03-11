#' Hypothesis-testing functions
#'
#' Performs common one-sample and two-sample hypothesis tests for means,
#' proportions, and variances.
#'
#' These functions return classed result objects with unrounded stored values
#' and optional printed summaries controlled by `digits` and `quiet`.
#'
#' @param xbar Numeric vector of sample means or summary statistics.
#' @param mu0 Null-hypothesis value for a mean or mean difference.
#' @param sigma Known population standard deviation input.
#' @param n Integer sample size or vector of sample sizes.
#' @param alpha Significance level in (0, 1).
#' @param alternative Alternative hypothesis direction: `"two.sided"`, `"less"`, or `"greater"`.
#' @param digits Integer number of decimal places used only for printed output.
#' @param quiet Logical; if `TRUE`, suppress printed output.
#' @param s Sample standard deviation input when sigma is unknown.
#' @param var.equal Logical; for `t_test_mu()`, whether to use the pooled-variance test.
#' @param paired Logical; for `t_test_mu()`, whether the supplied summaries are for paired differences.
#' @param x Count input for proportion and variance tests.
#' @param p0 Null-hypothesis proportion value or difference in proportions.
#' @param check_npq Logical; whether to report normal-approximation adequacy checks for proportion tests.
#' @param pooled Logical; for two-sample proportion tests, whether to use pooled standard errors.
#' @param sigma0 Null-hypothesis standard deviation for the chi-square variance test.
#' @param ratio0 Null-hypothesis variance ratio for the F test.
#' @name hypothesis_test_functions
NULL

# Hypothesis-testing functions ---------------------------------------------

#' @describeIn hypothesis_test_functions Z test for a mean or difference in means with known sigma.
#' @export
z_test_mu <- function(xbar, mu0, sigma = NULL, n, alpha = 0.05,
                      alternative = c("two.sided", "less", "greater"),
                      digits = 4, quiet = FALSE, s = NULL) {
  fun <- "z_test_mu()"

  alpha <- .ht_validate_alpha(alpha, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  alternative <- match.arg(alternative)
  mu0 <- .ht_validate_finite_scalar(mu0, "mu0", fun)

  if (!is.numeric(xbar) || length(xbar) == 0L || any(!is.finite(xbar))) {
    stop(fun, ": xbar must be a finite numeric vector.")
  }

  k <- length(xbar)
  if (!k %in% c(1L, 2L)) {
    stop(fun, ": xbar must have length 1 (one-sample) or 2 (two-sample).")
  }

  n <- .ht_validate_integer_vector(n, "n", fun, min_value = 1L)
  if (length(n) != k) {
    stop(fun, ": n must have the same length as xbar.")
  }

  sigma_supplied <- !is.null(sigma)
  s_supplied <- !is.null(s)

  if (sigma_supplied && s_supplied) {
    stop(fun, ": provide only one of sigma or s (alias), not both.")
  }
  if (!sigma_supplied && s_supplied) {
    sigma <- s
  }
  if (is.null(sigma)) {
    stop(fun, ": provide sigma (known population SD).")
  }
  if (!is.numeric(sigma) || length(sigma) == 0L || any(!is.finite(sigma))) {
    stop(fun, ": sigma must be finite numeric.")
  }

  if (k == 1L) {
    if (length(sigma) != 1L) {
      stop(fun, ": for the one-sample test, sigma must be length 1.")
    }
  } else {
    if (length(sigma) == 1L) {
      sigma <- rep(sigma, 2L)
    } else if (length(sigma) != 2L) {
      stop(fun, ": for the two-sample test, sigma must be length 1 or 2.")
    }
  }

  if (any(sigma <= 0)) {
    stop(fun, ": sigma must be > 0.")
  }

  if (k == 1L) {
    method <- "One-sample z test for mean (sigma known; summary stats)"
    parameter <- "mu"
    estimate_label <- "xbar"
    estimate <- xbar[1L]
    se <- sigma[1L] / sqrt(n[1L])

    assumptions <- c(
      "Observations are independent",
      "Population sigma is known",
      "Population is normal or the sample size is large"
    )

    sample_extras <- list(
      sample_type = "one_sample",
      xbar = xbar[1L],
      sigma = sigma[1L],
      n = n[1L]
    )
  } else {
    method <- "Two-sample z test for difference in means (sigmas known; summary stats)"
    parameter <- "mu1 - mu2"
    estimate_label <- "xbar1 - xbar2"
    estimate <- xbar[1L] - xbar[2L]
    se <- sqrt(sigma[1L]^2 / n[1L] + sigma[2L]^2 / n[2L])

    assumptions <- c(
      "The two samples are independent",
      "Population sigmas are known",
      "Each population is normal or the sample sizes are large"
    )

    sample_extras <- list(
      sample_type = "two_sample",
      xbar1 = xbar[1L],
      xbar2 = xbar[2L],
      sigma1 = sigma[1L],
      sigma2 = sigma[2L],
      n1 = n[1L],
      n2 = n[2L]
    )
  }

  z_stat <- (estimate - mu0) / se

  if (alternative == "two.sided") {
    zcrit <- stats::qnorm(1 - alpha / 2)
    p_val <- 2 * stats::pnorm(-abs(z_stat))
    crit_out <- c(lower = -zcrit, upper = zcrit)
    est_crit_out <- c(lower = mu0 - zcrit * se, upper = mu0 + zcrit * se)
  } else if (alternative == "less") {
    zcrit <- stats::qnorm(alpha)
    p_val <- stats::pnorm(z_stat)
    crit_out <- zcrit
    est_crit_out <- mu0 + zcrit * se
  } else {
    zcrit <- stats::qnorm(1 - alpha)
    p_val <- 1 - stats::pnorm(z_stat)
    crit_out <- zcrit
    est_crit_out <- mu0 + zcrit * se
  }

  reject_region <- .ht_build_region(
    stat_name = "Z",
    alternative = alternative,
    crit = crit_out,
    estimate_crit = est_crit_out,
    estimate_label = estimate_label
  )

  out <- do.call(
    .new_htest_result,
    c(
      list(
        class_name = "z_test_mu_result",
        method = method,
        parameter = parameter,
        alternative = alternative,
        alpha = alpha,
        null = mu0,
        estimate = estimate,
        statistic = z_stat,
        stat_name = "z",
        p_value = p_val,
        assumptions = assumptions,
        inputs = list(xbar = xbar, mu0 = mu0, sigma = sigma, n = n, alpha = alpha)
      ),
      list(
        se = se,
        crit = crit_out,
        crit_label = "z*",
        estimate_crit = est_crit_out,
        estimate_label = estimate_label,
        z_stat = z_stat,
        zcrit = crit_out,
        est_crit = est_crit_out,
        reject_region = reject_region
      ),
      sample_extras
    )
  )

  if (!quiet) {
    fmt <- function(x) .ci_format_num(x, digits)

    reject_region_print <- .ht_build_region(
      stat_name = "Z",
      alternative = alternative,
      crit = crit_out,
      estimate_crit = est_crit_out,
      estimate_label = estimate_label,
      formatter = fmt
    )

    cat("\n", out$method, "\n", sep = "")
    cat("Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ", fmt(out$null), "\n", sep = "")
    cat("alpha = ", fmt(out$alpha), "\n", sep = "")

    if (k == 1L) {
      cat("xbar = ", fmt(out$xbar), "  sigma = ", fmt(out$sigma), "  n = ", out$n, "\n", sep = "")
    } else {
      cat("xbar1 = ", fmt(out$xbar1), "  xbar2 = ", fmt(out$xbar2), "\n", sep = "")
      cat("sigma1 = ", fmt(out$sigma1), "  sigma2 = ", fmt(out$sigma2), "\n", sep = "")
      cat("n1 = ", out$n1, "  n2 = ", out$n2, "\n", sep = "")
    }

    cat("SE = ", fmt(out$se), "\n", sep = "")
    cat("Test statistic: z = ", fmt(out$z_stat), "\n", sep = "")
    cat("p-value = ", fmt(out$p_value), "\n", sep = "")
    cat("Rejection region: ", reject_region_print, "\n", sep = "")
    cat("Decision: ", out$decision, "\n", sep = "")
  }

  invisible(out)
}


#' @describeIn hypothesis_test_functions t test for a mean or difference in means.
#' @export
t_test_mu <- function(xbar, mu0, s, n, alpha = 0.05,
                      alternative = c("two.sided", "less", "greater"),
                      var.equal = FALSE, digits = 4, quiet = FALSE, paired = FALSE) {
  fun <- "t_test_mu()"
  var_equal_supplied <- !missing(var.equal)

  alpha <- .ht_validate_alpha(alpha, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  paired <- .ci_validate_flag(paired, "paired", fun)
  var.equal <- .ci_validate_flag(var.equal, "var.equal", fun)
  alternative <- match.arg(alternative)
  mu0 <- .ht_validate_finite_scalar(mu0, "mu0", fun)

  if (!is.numeric(xbar) || length(xbar) == 0L || any(!is.finite(xbar))) {
    stop(fun, ": xbar must be a finite numeric vector.")
  }
  if (!is.numeric(s) || length(s) == 0L || any(!is.finite(s))) {
    stop(fun, ": s must be a finite numeric vector.")
  }

  k <- length(xbar)
  if (!k %in% c(1L, 2L)) {
    stop(fun, ": xbar must have length 1 (one-sample/paired) or 2 (two-sample).")
  }
  if (paired && k != 1L) {
    stop(fun, ": paired = TRUE expects summary stats of paired differences as scalars.")
  }
  if (length(s) != k) {
    stop(fun, ": s must have the same length as xbar.")
  }

  n <- .ht_validate_integer_vector(n, "n", fun, min_value = 2L)
  if (length(n) != k) {
    stop(fun, ": n must have the same length as xbar.")
  }

  if (any(s < 0)) {
    stop(fun, ": s must be >= 0.")
  }

  if ((paired || k == 1L) && var_equal_supplied) {
    .ht_warn_ignored_arg(fun, "var.equal", "ignored for one-sample and paired t tests.")
  }

  if (paired || k == 1L) {
    estimate <- xbar[1L]
    se <- s[1L] / sqrt(n[1L])

    if (!is.finite(se) || se <= 0) {
      stop(fun, ": standard error is 0 or non-finite; the t test is not defined for these inputs.")
    }

    df <- n[1L] - 1L
    t_stat <- (estimate - mu0) / se

    if (paired) {
      method <- "Paired t test (one-sample on differences; summary stats)"
      parameter <- "mu_d"
      estimate_label <- "dbar"
      assumptions <- c(
        "Paired observations are reduced to differences",
        "Differences are independent",
        "Differences are approximately normal or the number of pairs is large"
      )
      sample_extras <- list(
        sample_type = "paired",
        paired = TRUE,
        var.equal = NA,
        dbar = xbar[1L],
        sd_diff = s[1L],
        n = n[1L]
      )
    } else {
      method <- "One-sample t test for mean (sigma unknown; summary stats)"
      parameter <- "mu"
      estimate_label <- "xbar"
      assumptions <- c(
        "Observations are independent",
        "Population is approximately normal or the sample size is large",
        "Population sigma is unknown"
      )
      sample_extras <- list(
        sample_type = "one_sample",
        paired = FALSE,
        var.equal = NA,
        xbar = xbar[1L],
        s = s[1L],
        n = n[1L]
      )
    }
  } else {
    estimate <- xbar[1L] - xbar[2L]
    estimate_label <- "xbar1 - xbar2"
    parameter <- "mu1 - mu2"

    if (var.equal) {
      method <- "Two-sample t test (pooled; equal variances; summary stats)"
      df <- n[1L] + n[2L] - 2L
      sp2 <- ((n[1L] - 1L) * s[1L]^2 + (n[2L] - 1L) * s[2L]^2) / df
      se <- sqrt(sp2 * (1 / n[1L] + 1 / n[2L]))

      if (!is.finite(se) || se <= 0) {
        stop(fun, ": pooled standard error is 0 or non-finite; the t test is not defined for these inputs.")
      }

      assumptions <- c(
        "The two samples are independent",
        "Each population is approximately normal or the sample sizes are large",
        "The population variances are equal"
      )

      sample_extras <- list(
        sample_type = "two_sample",
        paired = FALSE,
        var.equal = TRUE,
        xbar1 = xbar[1L],
        xbar2 = xbar[2L],
        s1 = s[1L],
        s2 = s[2L],
        n1 = n[1L],
        n2 = n[2L],
        sp2 = sp2
      )
    } else {
      method <- "Two-sample t test (Welch; unequal variances; summary stats)"
      se <- sqrt(s[1L]^2 / n[1L] + s[2L]^2 / n[2L])

      if (!is.finite(se) || se <= 0) {
        stop(fun, ": Welch standard error is 0 or non-finite; the t test is not defined for these inputs.")
      }

      df <- (s[1L]^2 / n[1L] + s[2L]^2 / n[2L])^2 /
        ((s[1L]^2 / n[1L])^2 / (n[1L] - 1L) + (s[2L]^2 / n[2L])^2 / (n[2L] - 1L))

      assumptions <- c(
        "The two samples are independent",
        "Each population is approximately normal or the sample sizes are large",
        "The population variances may differ"
      )

      sample_extras <- list(
        sample_type = "two_sample",
        paired = FALSE,
        var.equal = FALSE,
        xbar1 = xbar[1L],
        xbar2 = xbar[2L],
        s1 = s[1L],
        s2 = s[2L],
        n1 = n[1L],
        n2 = n[2L]
      )
    }

    t_stat <- (estimate - mu0) / se
  }

  if (alternative == "two.sided") {
    tcrit <- stats::qt(1 - alpha / 2, df = df)
    p_val <- 2 * stats::pt(-abs(t_stat), df = df)
    crit_out <- c(lower = -tcrit, upper = tcrit)
    est_crit_out <- c(lower = mu0 - tcrit * se, upper = mu0 + tcrit * se)
  } else if (alternative == "less") {
    tcrit <- stats::qt(alpha, df = df)
    p_val <- stats::pt(t_stat, df = df)
    crit_out <- tcrit
    est_crit_out <- mu0 + tcrit * se
  } else {
    tcrit <- stats::qt(1 - alpha, df = df)
    p_val <- 1 - stats::pt(t_stat, df = df)
    crit_out <- tcrit
    est_crit_out <- mu0 + tcrit * se
  }

  reject_region <- .ht_build_region(
    stat_name = "T",
    alternative = alternative,
    crit = crit_out,
    estimate_crit = est_crit_out,
    estimate_label = estimate_label
  )

  out <- do.call(
    .new_htest_result,
    c(
      list(
        class_name = "t_test_mu_result",
        method = method,
        parameter = parameter,
        alternative = alternative,
        alpha = alpha,
        null = mu0,
        estimate = estimate,
        statistic = t_stat,
        stat_name = "t",
        p_value = p_val,
        assumptions = assumptions,
        inputs = list(xbar = xbar, mu0 = mu0, s = s, n = n, alpha = alpha,
                      paired = paired, var.equal = if (k == 2L && !paired) var.equal else NULL)
      ),
      list(
        se = se,
        df = df,
        crit = crit_out,
        crit_label = "t*",
        estimate_crit = est_crit_out,
        estimate_label = estimate_label,
        t_stat = t_stat,
        tcrit = crit_out,
        est_crit = est_crit_out,
        reject_region = reject_region
      ),
      sample_extras
    )
  )

  if (!quiet) {
    fmt <- function(x) .ci_format_num(x, digits)

    reject_region_print <- .ht_build_region(
      stat_name = "T",
      alternative = alternative,
      crit = crit_out,
      estimate_crit = est_crit_out,
      estimate_label = estimate_label,
      formatter = fmt
    )

    cat("\n", out$method, "\n", sep = "")
    cat("Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ", fmt(out$null), "\n", sep = "")
    cat("alpha = ", fmt(out$alpha), "   df = ", fmt(out$df), "\n", sep = "")

    if (identical(out$sample_type, "paired")) {
      cat("dbar = ", fmt(out$dbar), "  sd(diff) = ", fmt(out$sd_diff), "  n = ", out$n, "\n", sep = "")
    } else if (identical(out$sample_type, "one_sample")) {
      cat("xbar = ", fmt(out$xbar), "  s = ", fmt(out$s), "  n = ", out$n, "\n", sep = "")
    } else {
      cat("xbar1 = ", fmt(out$xbar1), "  xbar2 = ", fmt(out$xbar2), "\n", sep = "")
      cat("s1 = ", fmt(out$s1), "  s2 = ", fmt(out$s2), "\n", sep = "")
      cat("n1 = ", out$n1, "  n2 = ", out$n2, "\n", sep = "")
      if (isTRUE(out$var.equal)) {
        cat("sp^2 = ", fmt(out$sp2), "\n", sep = "")
      }
    }

    cat("SE = ", fmt(out$se), "\n", sep = "")
    cat("Test statistic: t = ", fmt(out$t_stat), "\n", sep = "")
    cat("p-value = ", fmt(out$p_value), "\n", sep = "")
    cat("Rejection region: ", reject_region_print, "\n", sep = "")
    cat("Decision: ", out$decision, "\n", sep = "")
  }

  invisible(out)
}


#' @describeIn hypothesis_test_functions Hypothesis test for one or two proportions.
#' @param continuity Logical. If TRUE, apply a continuity correction in the
#'   two-sample z test for proportions. Ignored for one-sample exact tests.
#' @export
p_test <- function(x, n, p0,
                   alpha = 0.05,
                   alternative = c("two.sided", "less", "greater"),
                   digits = 4,
                   quiet = FALSE,
                   check_npq = TRUE,
                   pooled = NULL,
                   continuity = FALSE) {
  fun <- "p_test()"
  pooled_supplied <- !missing(pooled)
  continuity_supplied <- !missing(continuity)

  alpha <- .ht_validate_alpha(alpha, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  check_npq <- .ci_validate_flag(check_npq, "check_npq", fun)
  alternative <- match.arg(alternative)
  continuity <- .ci_validate_flag(continuity, "continuity", fun)
  pooled <- .ht_validate_optional_flag(pooled, "pooled", fun)

  x <- .ht_validate_integer_vector(x, "x", fun, min_value = 0L)
  n <- .ht_validate_integer_vector(n, "n", fun, min_value = 1L)

  k <- length(x)
  if (!k %in% c(1L, 2L)) {
    stop(fun, ": x must have length 1 (one-sample) or 2 (two-sample).")
  }
  if (length(n) != k) {
    stop(fun, ": n must have the same length as x.")
  }
  if (any(x > n)) {
    stop(fun, ": each x must satisfy 0 <= x <= n.")
  }

  fmt <- function(y) .ci_format_num(y, digits)

  if (k == 1L) {
    if (pooled_supplied && !is.null(pooled)) {
      .ht_warn_ignored_arg(fun, "pooled", "ignored in the one-sample exact binomial test.")
    }
    if (continuity_supplied) {
      .ht_warn_ignored_arg(fun, "continuity", "ignored in the one-sample exact binomial test.")
    }

    p0 <- .ht_validate_probability_closed(p0, "p0", fun)

    x1 <- x[1L]
    n1 <- n[1L]
    phat <- x1 / n1

    bt <- stats::binom.test(
      x = x1,
      n = n1,
      p = p0,
      alternative = alternative,
      conf.level = 1 - alpha
    )

    p_val <- bt$p.value
    conf_int <- stats::setNames(unname(bt$conf.int), c("lower", "upper"))

    reject_region <- .ht_build_region(
      stat_name = "X",
      alternative = alternative,
      crit = 0,
      exact = TRUE
    )

    out <- .new_htest_result(
      class_name = "p_test_result",
      method = "One-sample exact proportion test (binomial)",
      parameter = "p",
      alternative = alternative,
      alpha = alpha,
      null = p0,
      estimate = phat,
      statistic = x1,
      stat_name = "X",
      p_value = p_val,
      assumptions = c(
        "Binomial model with fixed n",
        "Exact test; no normal approximation is required"
      ),
      inputs = list(
        x = x1,
        n = n1,
        p0 = p0,
        alpha = alpha,
        alternative = alternative,
        check_npq = check_npq
      ),
      sample_type = "one_sample",
      x = x1,
      n = n1,
      phat = phat,
      conf_int = conf_int,
      np0 = n1 * p0,
      nq0 = n1 * (1 - p0),
      pooled = NULL,
      p_pool = NULL,
      continuity = FALSE,
      continuity_correction = FALSE,
      reject_region = reject_region,
      region = reject_region
    )

    if (!quiet) {
      reject_region_print <- .ht_build_region(
        stat_name = "X",
        alternative = alternative,
        crit = 0,
        exact = TRUE,
        formatter = fmt
      )

      cat("\n", out$method, "\n", sep = "")
      cat("Ha: p ", .ht_alt_symbol(alternative), " ", fmt(p0), "\n", sep = "")
      cat("alpha = ", fmt(alpha), "\n", sep = "")
      cat("phat = ", fmt(phat), "  (x = ", x1, ", n = ", n1, ")\n", sep = "")
      if (check_npq) {
        cat(
          "Check (not required for exact test): n*p0 = ", fmt(out$np0),
          "  n*(1-p0) = ", fmt(out$nq0), "\n",
          sep = ""
        )
      }
      cat(
        "Exact interval from binom.test (conf.level = ", fmt(1 - alpha), "): [",
        fmt(conf_int[1L]), ", ", fmt(conf_int[2L]), "]\n",
        sep = ""
      )
      cat("Test statistic: X = ", x1, " successes\n", sep = "")
      cat("p-value = ", sprintf("%.6f", p_val), "\n", sep = "")
      cat("Rejection rule: ", reject_region_print, "\n", sep = "")
      cat("Decision: ", out$decision, "\n", sep = "")
    }

    return(invisible(out))
  }

  # two-sample
  delta0 <- .ht_validate_difference_null(p0, "p0", fun)
  opts <- .ht_resolve_two_prop_options(delta0, pooled, continuity, fun)
  pooled <- opts$pooled
  continuity <- opts$continuity

  x1 <- x[1L]
  x2 <- x[2L]
  n1 <- n[1L]
  n2 <- n[2L]

  phat1 <- x1 / n1
  phat2 <- x2 / n2
  diff_hat <- phat1 - phat2

  if (pooled) {
    p_pool <- (x1 + x2) / (n1 + n2)
    se0 <- sqrt(p_pool * (1 - p_pool) * (1 / n1 + 1 / n2))
  } else {
    p_pool <- NA_real_
    se0 <- sqrt(phat1 * (1 - phat1) / n1 + phat2 * (1 - phat2) / n2)
  }

  if (!is.finite(se0) || se0 <= 0) {
    stop(fun, ": the two-sample z test is undefined because SE0 is zero or non-finite.")
  }

  adequacy <- .ht_prop_adequacy(
    pooled = pooled,
    x1 = x1, n1 = n1,
    x2 = x2, n2 = n2,
    p_pool = p_pool
  )
  adequacy_ok <- adequacy$ok
  adequacy_values <- adequacy$values

  if (check_npq) {
    .ht_warn_prop_adequacy(adequacy_ok, pooled, fun)
  }

  cc <- if (continuity) 0.5 * (1 / n1 + 1 / n2) else 0
  d <- diff_hat - delta0

  if (alternative == "less") {
    zcrit <- stats::qnorm(alpha)
    z_stat <- if (continuity) (d + cc) / se0 else d / se0
    p_val <- stats::pnorm(z_stat)

    zcrit_out <- zcrit
    est_crit_out <- delta0 + zcrit * se0 - cc
  } else if (alternative == "greater") {
    zcrit <- stats::qnorm(1 - alpha)
    z_stat <- if (continuity) (d - cc) / se0 else d / se0
    p_val <- stats::pnorm(z_stat, lower.tail = FALSE)

    zcrit_out <- zcrit
    est_crit_out <- delta0 + zcrit * se0 + cc
  } else {
    zcrit <- stats::qnorm(1 - alpha / 2)
    z_stat <- if (continuity) {
      sign(d) * max(abs(d) - cc, 0) / se0
    } else {
      d / se0
    }
    p_val <- min(1, 2 * stats::pnorm(abs(z_stat), lower.tail = FALSE))

    band <- zcrit * se0 + cc
    zcrit_out <- c(lower = -zcrit, upper = zcrit)
    est_crit_out <- c(lower = delta0 - band, upper = delta0 + band)
  }

  reject_region <- .ht_build_region(
    stat_name = "Z",
    alternative = alternative,
    crit = zcrit_out,
    estimate_crit = est_crit_out,
    estimate_label = "phat1 - phat2",
    corrected = continuity
  )

  method <- paste0(
    "Two-sample proportion z test (",
    .ht_two_prop_method_suffix(delta0, pooled, continuity),
    ")"
  )

  out <- .new_htest_result(
    class_name = "p_test_result",
    method = method,
    parameter = "p1 - p2",
    alternative = alternative,
    alpha = alpha,
    null = delta0,
    estimate = diff_hat,
    statistic = z_stat,
    stat_name = if (continuity) "z_cc" else "z",
    p_value = p_val,
    assumptions = c(
      "The two samples are independent",
      "Large-sample normal approximation is used"
    ),
    inputs = list(
      x = x,
      n = n,
      p0 = delta0,
      alpha = alpha,
      alternative = alternative,
      pooled = pooled,
      continuity = continuity,
      check_npq = check_npq
    ),
    sample_type = "two_sample",
    x1 = x1,
    n1 = n1,
    phat1 = phat1,
    x2 = x2,
    n2 = n2,
    phat2 = phat2,
    diff_hat = diff_hat,
    se0 = se0,
    z_stat = z_stat,
    zcrit = zcrit_out,
    crit = zcrit_out,
    crit_label = "z critical value",
    est_crit = est_crit_out,
    estimate_crit = est_crit_out,
    estimate_label = "phat1 - phat2",
    p_pool = p_pool,
    pooled = pooled,
    continuity = continuity,
    continuity_correction = continuity,
    cc = cc,
    adequacy_ok = adequacy_ok,
    approximation_ok = adequacy_ok,
    adequacy_values = adequacy_values,
    reject_region = reject_region,
    region = reject_region
  )

  if (!quiet) {
    stat_label <- if (continuity) "z_cc" else "z"

    reject_region_print <- .ht_build_region(
      stat_name = "Z",
      alternative = alternative,
      crit = zcrit_out,
      estimate_crit = est_crit_out,
      estimate_label = "phat1 - phat2",
      corrected = continuity,
      formatter = fmt
    )

    cat("\n", out$method, "\n", sep = "")
    cat("Ha: p1 - p2 ", .ht_alt_symbol(alternative), " ", fmt(delta0), "\n", sep = "")
    cat("alpha = ", fmt(alpha), "\n", sep = "")
    cat(
      "phat1 = ", fmt(phat1), "  phat2 = ", fmt(phat2),
      "  diff_hat = ", fmt(diff_hat), "\n",
      sep = ""
    )
    cat(
      "SE0 = ", fmt(se0),
      if (pooled) paste0("  (pooled p = ", fmt(p_pool), ")") else "  (unpooled)",
      "\n",
      sep = ""
    )
    if (continuity) {
      cat("cc = ", fmt(cc), "\n", sep = "")
    }

    if (check_npq) {
      if (pooled) {
        cat("Check (pooled expected counts):\n")
        cat("  n1*p = ", fmt(adequacy_values[["n1p"]]), "  n1*q = ", fmt(adequacy_values[["n1q"]]), "\n", sep = "")
        cat("  n2*p = ", fmt(adequacy_values[["n2p"]]), "  n2*q = ", fmt(adequacy_values[["n2q"]]), "\n", sep = "")
      } else {
        cat("Check (observed counts):\n")
        cat("  x1 = ", x1, "  n1-x1 = ", n1 - x1, "\n", sep = "")
        cat("  x2 = ", x2, "  n2-x2 = ", n2 - x2, "\n", sep = "")
      }
    }

    cat("Test statistic: ", stat_label, " = ", fmt(z_stat), "\n", sep = "")
    cat("p-value = ", sprintf("%.6f", p_val), "\n", sep = "")
    cat("Rejection region: ", reject_region_print, "\n", sep = "")
    cat("Decision: ", out$decision, "\n", sep = "")
  }

  invisible(out)
}
# Retains the legacy function name; the two-sample branch is an F test.
#' @describeIn hypothesis_test_functions Chi-square or F test for variances.
#' @export
var_test_chisq <- function(s, n, sigma0 = NULL, ratio0 = 1,
                           alpha = 0.05,
                           alternative = c("two.sided", "less", "greater"),
                           digits = 4, quiet = FALSE) {
  fun <- "var_test_chisq()"
  sigma0_supplied <- !missing(sigma0)
  ratio0_supplied <- !missing(ratio0)

  alpha <- .ht_validate_alpha(alpha, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  alternative <- match.arg(alternative)

  if (!is.numeric(s) || length(s) == 0L || any(!is.finite(s))) {
    stop(fun, ": s must be a finite numeric vector.")
  }

  k <- length(s)
  if (!k %in% c(1L, 2L)) {
    stop(fun, ": s must have length 1 (one-sample) or 2 (two-sample).")
  }

  n <- .ht_validate_integer_vector(n, "n", fun, min_value = 2L)
  if (length(n) != k) {
    stop(fun, ": n must have the same length as s.")
  }

  if (any(s < 0)) {
    stop(fun, ": s must be >= 0.")
  }

  if (k == 1L) {
    if (ratio0_supplied) {
      .ht_warn_ignored_arg(fun, "ratio0", "ignored in the one-sample chi-square test.")
    }

    sigma0 <- .ht_validate_positive_scalar(sigma0, "sigma0", fun)

    method <- "One-sample chi-square test for variance (normal population)"
    parameter <- "sigma^2"
    df <- n[1L] - 1L
    estimate <- s[1L]^2
    null_var <- sigma0^2
    stat <- df * estimate / null_var

    p_left <- stats::pchisq(stat, df = df)
    p_right <- stats::pchisq(stat, df = df, lower.tail = FALSE)

    if (alternative == "less") {
      p_val <- p_left
      crit_out <- stats::qchisq(alpha, df = df)
    } else if (alternative == "greater") {
      p_val <- p_right
      crit_out <- stats::qchisq(1 - alpha, df = df)
    } else {
      p_val <- min(1, 2 * min(p_left, p_right))
      crit_out <- c(
        lower = stats::qchisq(alpha / 2, df = df),
        upper = stats::qchisq(1 - alpha / 2, df = df)
      )
    }

    reject_region <- .ht_build_region_nonsymmetric(
      stat_name = "Chi^2",
      alternative = alternative,
      crit = crit_out
    )

    out <- .new_htest_result(
      class_name = "var_test_chisq_result",
      method = method,
      parameter = parameter,
      alternative = alternative,
      alpha = alpha,
      null = null_var,
      estimate = estimate,
      statistic = stat,
      stat_name = "chi^2",
      p_value = p_val,
      assumptions = c(
        "Observations are independent",
        "The population is normal"
      ),
      inputs = list(s = s[1L], n = n[1L], sigma0 = sigma0, alpha = alpha),
      sample_type = "one_sample",
      s = s[1L],
      n = n[1L],
      df = df,
      sigma0 = sigma0,
      sigma0_sq = null_var,
      s2 = estimate,
      crit = crit_out,
      crit_label = "chi-square critical value",
      chi_stat = stat,
      chi_crit = crit_out,
      reject_region = reject_region,
      region = reject_region
    )

    if (!quiet) {
      fmt <- function(y) .ci_format_num(y, digits)

      reject_region_print <- .ht_build_region_nonsymmetric(
        stat_name = "Chi^2",
        alternative = alternative,
        crit = crit_out,
        formatter = fmt
      )

      cat("\n", out$method, "\n", sep = "")
      cat(
        "Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ",
        fmt(out$null), "\n",
        sep = ""
      )
      cat("alpha = ", fmt(out$alpha), "   df = ", fmt(out$df), "\n", sep = "")
      cat("s = ", fmt(out$s), "  s^2 = ", fmt(out$s2), "\n", sep = "")
      cat("Test statistic: chi^2 = ", fmt(out$chi_stat), "\n", sep = "")
      cat("p-value = ", fmt(out$p_value), "\n", sep = "")
      cat("Rejection region: ", reject_region_print, "\n", sep = "")
      cat("Decision: ", out$decision, "\n", sep = "")
    }

    return(invisible(out))
  }

  if (sigma0_supplied && !is.null(sigma0)) {
    .ht_warn_ignored_arg(fun, "sigma0", "ignored in the two-sample F test.")
  }

  ratio0 <- .ht_validate_positive_scalar(ratio0, "ratio0", fun)

  s1 <- s[1L]
  s2 <- s[2L]

  if (s2 == 0) {
    stop(fun, ": the two-sample F test is undefined when s[2] = 0.")
  }

  df1 <- n[1L] - 1L
  df2 <- n[2L] - 1L
  ratio_hat <- s1^2 / s2^2
  stat <- ratio_hat / ratio0

  p_left <- stats::pf(stat, df1 = df1, df2 = df2)
  p_right <- stats::pf(stat, df1 = df1, df2 = df2, lower.tail = FALSE)

  if (alternative == "less") {
    p_val <- p_left
    crit_out <- stats::qf(alpha, df1 = df1, df2 = df2)
  } else if (alternative == "greater") {
    p_val <- p_right
    crit_out <- stats::qf(1 - alpha, df1 = df1, df2 = df2)
  } else {
    p_val <- min(1, 2 * min(p_left, p_right))
    crit_out <- c(
      lower = stats::qf(alpha / 2, df1 = df1, df2 = df2),
      upper = stats::qf(1 - alpha / 2, df1 = df1, df2 = df2)
    )
  }

  reject_region <- .ht_build_region_nonsymmetric(
    stat_name = "F",
    alternative = alternative,
    crit = crit_out
  )

  out <- .new_htest_result(
    class_name = "var_test_chisq_result",
    method = "Two-sample F test for ratio of variances (normal populations)",
    parameter = "sigma1^2 / sigma2^2",
    alternative = alternative,
    alpha = alpha,
    null = ratio0,
    estimate = ratio_hat,
    statistic = stat,
    stat_name = "F",
    p_value = p_val,
    assumptions = c(
      "The two samples are independent",
      "Both populations are normal"
    ),
    inputs = list(s = s, n = n, ratio0 = ratio0, alpha = alpha),
    sample_type = "two_sample",
    s1 = s1,
    n1 = n[1L],
    df1 = df1,
    s2 = s2,
    n2 = n[2L],
    df2 = df2,
    ratio0 = ratio0,
    ratio_hat = ratio_hat,
    crit = crit_out,
    crit_label = "F critical value",
    f_stat = stat,
    f_crit = crit_out,
    reject_region = reject_region,
    region = reject_region
  )

  if (!quiet) {
    fmt <- function(y) .ci_format_num(y, digits)

    reject_region_print <- .ht_build_region_nonsymmetric(
      stat_name = "F",
      alternative = alternative,
      crit = crit_out,
      formatter = fmt
    )

    cat("\n", out$method, "\n", sep = "")
    cat(
      "Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ",
      fmt(out$null), "\n",
      sep = ""
    )
    cat(
      "alpha = ", fmt(out$alpha),
      "   df1 = ", fmt(out$df1),
      "  df2 = ", fmt(out$df2),
      "\n",
      sep = ""
    )
    cat("ratio_hat = ", fmt(out$ratio_hat), "  F = ", fmt(out$f_stat), "\n", sep = "")
    cat("p-value = ", fmt(out$p_value), "\n", sep = "")
    cat("Rejection region: ", reject_region_print, "\n", sep = "")
    cat("Decision: ", out$decision, "\n", sep = "")
  }

  invisible(out)
}

