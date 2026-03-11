#' Power functions
#'
#' Computes power and type II error for common tests for means, proportions,
#' and variances.
#'
#' These functions return classed result objects with unrounded stored values
#' and optional printed summaries controlled by `digits` and `quiet`.
#'
#' @param mu_a True mean value or vector of true means under the alternative.
#' @param mu0 Null-hypothesis mean value or mean-difference value.
#' @param sigma Known population standard deviation input.
#' @param n Integer sample size or vector of sample sizes.
#' @param alpha Significance level in (0, 1).
#' @param alternative Alternative hypothesis direction: `"two.sided"`, `"less"`, or `"greater"`.
#' @param digits Integer number of decimal places used only for printed output.
#' @param quiet Logical; if `TRUE`, suppress printed output.
#' @param sigma_true True population standard deviation input used in t-test power calculations.
#' @param paired Logical; for `power_t_mu()`, whether the supplied summaries are for paired differences.
#' @param method Method for the two-sample sigma-unknown t-test power calculation.
#' @param p_a True proportion value or vector of true proportions under the alternative.
#' @param p0 Null-hypothesis proportion value or difference in proportions.
#' @param pooled Logical; for two-sample proportion power, whether to use pooled standard errors.
#' @param continuity Logical; whether to apply the continuity correction where supported.
#' @param sigma_a True standard deviation value or vector under the alternative.
#' @param sigma0 Null-hypothesis standard deviation for the chi-square variance test.
#' @param ratio0 Null-hypothesis variance ratio for the F test.
#' @name power_functions
NULL

# Power functions ----------------------------------------------------------

#' @describeIn power_functions Power for z tests of means with known sigma.
#' @export
power_z_mu <- function(mu_a, mu0, sigma, n, alpha = 0.05,
                       alternative = c("two.sided", "less", "greater"),
                       digits = 4, quiet = FALSE) {
  fun <- "power_z_mu()"

  alpha <- .ht_validate_alpha(alpha, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  alternative <- match.arg(alternative)
  mu0 <- .ht_validate_finite_scalar(mu0, "mu0", fun)

  if (!is.numeric(mu_a) || length(mu_a) == 0L || any(!is.finite(mu_a))) {
    stop(fun, ": mu_a must be a finite numeric vector.")
  }

  k <- length(mu_a)
  if (!k %in% c(1L, 2L)) {
    stop(fun, ": mu_a must have length 1 (one-sample) or 2 (two-sample).")
  }

  n <- .ht_validate_integer_vector(n, "n", fun, min_value = 1L)
  if (k == 2L && length(n) == 1L) {
    n <- rep(n, 2L)
  }
  if (length(n) != k) {
    stop(fun, ": n must have length 1 (recycled) or the same length as mu_a.")
  }

  sigma <- .power_validate_positive_numeric_vector(sigma, "sigma", fun)
  if (k == 2L && length(sigma) == 1L) {
    sigma <- rep(sigma, 2L)
  }
  if (length(sigma) != k) {
    stop(fun, ": sigma must have length 1 (recycled) or the same length as mu_a.")
  }

  if (k == 1L) {
    scenario <- "one_sample"
    method <- "Power for one-sample z test for mean (sigma known)"
    parameter <- "mu"
    true_value <- mu_a[1L]
    estimate_label <- "xbar"
    se <- sigma[1L] / sqrt(n[1L])
    ncp <- (true_value - mu0) / se

    if (alternative == "two.sided") {
      zcrit <- stats::qnorm(1 - alpha / 2)
      crit_out <- c(lower = -zcrit, upper = zcrit)
      est_crit_out <- c(lower = mu0 - zcrit * se, upper = mu0 + zcrit * se)
      power <- .power_prob_two_sided_normal(-zcrit, zcrit, mean = ncp, sd = 1)
    } else if (alternative == "less") {
      zcrit <- stats::qnorm(alpha)
      crit_out <- zcrit
      est_crit_out <- mu0 + zcrit * se
      power <- .power_prob_less_normal(zcrit, mean = ncp, sd = 1)
    } else {
      zcrit <- stats::qnorm(1 - alpha)
      crit_out <- zcrit
      est_crit_out <- mu0 + zcrit * se
      power <- .power_prob_greater_normal(zcrit, mean = ncp, sd = 1)
    }

    power <- .power_clip_prob(power)
    beta <- 1 - power

    reject_region <- .ht_build_region(
      stat_name = "Z",
      alternative = alternative,
      crit = crit_out,
      estimate_crit = est_crit_out,
      estimate_label = estimate_label
    )

    out <- .new_power_result(
      class_name = "power_z_mu_result",
      method = method,
      parameter = parameter,
      alternative = alternative,
      alpha = alpha,
      null = mu0,
      true_value = true_value,
      power = power,
      beta = beta,
      assumptions = c(
        "Population sigma is known",
        "Observations are independent",
        "Population is normal or the sample size is large"
      ),
      inputs = list(mu_a = mu_a[1L], mu0 = mu0, sigma = sigma[1L], n = n[1L], alpha = alpha),
      scenario = scenario,
      mu_a = mu_a[1L],
      sigma = sigma[1L],
      n = n[1L],
      se = se,
      ncp = ncp,
      crit = crit_out,
      crit_label = "z*",
      zcrit = crit_out,
      estimate_crit = est_crit_out,
      reject_region = reject_region,
      region = reject_region
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
      cat("mu_a = ", fmt(out$mu_a), "  mu0 = ", fmt(out$null),
          "  sigma = ", fmt(out$sigma), "  n = ", out$n, "\n", sep = "")
      cat("SE = ", fmt(out$se), "  ncp = ", fmt(out$ncp), "\n", sep = "")
      cat("Rejection region: ", reject_region_print, "\n", sep = "")
      cat("Power = ", fmt(out$power), "  Beta = ", fmt(out$beta), "\n", sep = "")
    }

    return(invisible(out))
  }

  scenario <- "two_sample"
  method <- "Power for two-sample z test for difference in means (sigmas known)"
  parameter <- "mu1 - mu2"
  true_value <- mu_a[1L] - mu_a[2L]
  estimate_label <- "xbar1 - xbar2"
  se <- sqrt(sigma[1L]^2 / n[1L] + sigma[2L]^2 / n[2L])
  ncp <- (true_value - mu0) / se

  if (alternative == "two.sided") {
    zcrit <- stats::qnorm(1 - alpha / 2)
    crit_out <- c(lower = -zcrit, upper = zcrit)
    est_crit_out <- c(lower = mu0 - zcrit * se, upper = mu0 + zcrit * se)
    power <- .power_prob_two_sided_normal(-zcrit, zcrit, mean = ncp, sd = 1)
  } else if (alternative == "less") {
    zcrit <- stats::qnorm(alpha)
    crit_out <- zcrit
    est_crit_out <- mu0 + zcrit * se
    power <- .power_prob_less_normal(zcrit, mean = ncp, sd = 1)
  } else {
    zcrit <- stats::qnorm(1 - alpha)
    crit_out <- zcrit
    est_crit_out <- mu0 + zcrit * se
    power <- .power_prob_greater_normal(zcrit, mean = ncp, sd = 1)
  }

  power <- .power_clip_prob(power)
  beta <- 1 - power

  reject_region <- .ht_build_region(
    stat_name = "Z",
    alternative = alternative,
    crit = crit_out,
    estimate_crit = est_crit_out,
    estimate_label = estimate_label
  )

  out <- .new_power_result(
    class_name = "power_z_mu_result",
    method = method,
    parameter = parameter,
    alternative = alternative,
    alpha = alpha,
    null = mu0,
    true_value = true_value,
    power = power,
    beta = beta,
    assumptions = c(
      "The two samples are independent",
      "Population sigmas are known",
      "Each population is normal or the sample sizes are large"
    ),
    inputs = list(mu_a = mu_a, mu0 = mu0, sigma = sigma, n = n, alpha = alpha),
    scenario = scenario,
    mu_a = mu_a,
    mu1_a = mu_a[1L],
    mu2_a = mu_a[2L],
    sigma = sigma,
    sigma1 = sigma[1L],
    sigma2 = sigma[2L],
    n = n,
    n1 = n[1L],
    n2 = n[2L],
    delta_a = true_value,
    se = se,
    ncp = ncp,
    crit = crit_out,
    crit_label = "z*",
    zcrit = crit_out,
    estimate_crit = est_crit_out,
    reject_region = reject_region,
    region = reject_region
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
    cat("mu1_a = ", fmt(out$mu1_a), "  mu2_a = ", fmt(out$mu2_a),
        "  delta0 = ", fmt(out$null), "\n", sep = "")
    cat("sigma1 = ", fmt(out$sigma1), "  sigma2 = ", fmt(out$sigma2),
        "  n1 = ", out$n1, "  n2 = ", out$n2, "\n", sep = "")
    cat("SE = ", fmt(out$se), "  ncp = ", fmt(out$ncp), "\n", sep = "")
    cat("Rejection region: ", reject_region_print, "\n", sep = "")
    cat("Power = ", fmt(out$power), "  Beta = ", fmt(out$beta), "\n", sep = "")
  }

  invisible(out)
}


#' @describeIn power_functions Power for t tests of means.
#' @export
power_t_mu <- function(mu_a, mu0, sigma_true, n, alpha = 0.05,
                       alternative = c("two.sided", "less", "greater"),
                       paired = FALSE,
                       method = c("welch", "pooled"),
                       digits = 4, quiet = FALSE) {
  fun <- "power_t_mu()"
  method_supplied <- !missing(method)

  alpha <- .ht_validate_alpha(alpha, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  paired <- .ci_validate_flag(paired, "paired", fun)
  alternative <- match.arg(alternative)
  mu0 <- .ht_validate_finite_scalar(mu0, "mu0", fun)

  if (!is.numeric(mu_a) || length(mu_a) == 0L || any(!is.finite(mu_a))) {
    stop(fun, ": mu_a must be a finite numeric vector.")
  }

  k <- length(mu_a)
  if (!k %in% c(1L, 2L)) {
    stop(fun, ": mu_a must have length 1 or 2.")
  }

  n <- .ht_validate_integer_vector(n, "n", fun, min_value = 2L)
  if (k == 2L && length(n) == 1L) {
    n <- rep(n, 2L)
  }
  if (length(n) != k) {
    stop(fun, ": n must have length 1 (recycled) or the same length as mu_a.")
  }

  if (paired) {
    if (k != 1L) {
      stop(fun, ": paired = TRUE requires scalar mu_a.")
    }
    if (method_supplied) {
      .ht_warn_ignored_arg(fun, "method", "ignored for paired t power.")
    }
    sigma_true <- .ht_validate_positive_scalar(sigma_true, "sigma_true", fun)

    scenario <- "paired"
    method_label <- "Power for paired t test (one-sample on differences; noncentral t)"
    parameter <- "mu_d"
    true_value <- mu_a[1L]
    df <- n[1L] - 1L
    se <- sigma_true / sqrt(n[1L])
    ncp <- (true_value - mu0) / se

    if (alternative == "two.sided") {
      tcrit <- stats::qt(1 - alpha / 2, df = df)
      crit_out <- c(lower = -tcrit, upper = tcrit)
      power <- stats::pt(-tcrit, df = df, ncp = ncp) +
        (1 - stats::pt(tcrit, df = df, ncp = ncp))
    } else if (alternative == "less") {
      tcrit <- stats::qt(alpha, df = df)
      crit_out <- tcrit
      power <- stats::pt(tcrit, df = df, ncp = ncp)
    } else {
      tcrit <- stats::qt(1 - alpha, df = df)
      crit_out <- tcrit
      power <- 1 - stats::pt(tcrit, df = df, ncp = ncp)
    }

    power <- .power_clip_prob(power)
    beta <- 1 - power

    reject_region <- .ht_build_region(
      stat_name = "T",
      alternative = alternative,
      crit = crit_out
    )

    out <- .new_power_result(
      class_name = "power_t_mu_result",
      method = method_label,
      parameter = parameter,
      alternative = alternative,
      alpha = alpha,
      null = mu0,
      true_value = true_value,
      power = power,
      beta = beta,
      assumptions = c(
        "Paired observations are reduced to differences",
        "Differences are independent",
        "Differences are approximately normal or the number of pairs is large",
        "Noncentral t power is used"
      ),
      inputs = list(mu_a = mu_a[1L], mu0 = mu0, sigma_true = sigma_true,
                    n = n[1L], alpha = alpha, paired = TRUE),
      scenario = scenario,
      sigma_true = sigma_true,
      n = n[1L],
      df = df,
      se = se,
      ncp = ncp,
      crit = crit_out,
      crit_label = "t*",
      tcrit = crit_out,
      reject_region = reject_region,
      region = reject_region
    )

    if (!quiet) {
      fmt <- function(x) .ci_format_num(x, digits)

      reject_region_print <- .ht_build_region(
        stat_name = "T",
        alternative = alternative,
        crit = crit_out,
        formatter = fmt
      )

      cat("\n", out$method, "\n", sep = "")
      cat("Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ", fmt(out$null), "\n", sep = "")
      cat("alpha = ", fmt(out$alpha), "   df = ", fmt(out$df), "\n", sep = "")
      cat("mu_d,a = ", fmt(out$true_value), "  mu_d,0 = ", fmt(out$null),
          "  sigma_true = ", fmt(out$sigma_true), "  n = ", out$n, "\n", sep = "")
      cat("SE = ", fmt(out$se), "  ncp = ", fmt(out$ncp), "\n", sep = "")
      cat("Rejection region: ", reject_region_print, "\n", sep = "")
      cat("Power = ", fmt(out$power), "  Beta = ", fmt(out$beta), "\n", sep = "")
    }

    return(invisible(out))
  }

  if (k == 1L) {
    if (method_supplied) {
      .ht_warn_ignored_arg(fun, "method", "ignored for one-sample t power.")
    }
    sigma_true <- .ht_validate_positive_scalar(sigma_true, "sigma_true", fun)

    scenario <- "one_sample"
    method_label <- "Power for one-sample t test for mean (noncentral t)"
    parameter <- "mu"
    true_value <- mu_a[1L]
    df <- n[1L] - 1L
    se <- sigma_true / sqrt(n[1L])
    ncp <- (true_value - mu0) / se

    if (alternative == "two.sided") {
      tcrit <- stats::qt(1 - alpha / 2, df = df)
      crit_out <- c(lower = -tcrit, upper = tcrit)
      power <- stats::pt(-tcrit, df = df, ncp = ncp) +
        (1 - stats::pt(tcrit, df = df, ncp = ncp))
    } else if (alternative == "less") {
      tcrit <- stats::qt(alpha, df = df)
      crit_out <- tcrit
      power <- stats::pt(tcrit, df = df, ncp = ncp)
    } else {
      tcrit <- stats::qt(1 - alpha, df = df)
      crit_out <- tcrit
      power <- 1 - stats::pt(tcrit, df = df, ncp = ncp)
    }

    power <- .power_clip_prob(power)
    beta <- 1 - power

    reject_region <- .ht_build_region(
      stat_name = "T",
      alternative = alternative,
      crit = crit_out
    )

    out <- .new_power_result(
      class_name = "power_t_mu_result",
      method = method_label,
      parameter = parameter,
      alternative = alternative,
      alpha = alpha,
      null = mu0,
      true_value = true_value,
      power = power,
      beta = beta,
      assumptions = c(
        "Observations are independent",
        "Population is approximately normal or the sample size is large",
        "Noncentral t power is used"
      ),
      inputs = list(mu_a = mu_a[1L], mu0 = mu0, sigma_true = sigma_true,
                    n = n[1L], alpha = alpha, paired = FALSE),
      scenario = scenario,
      sigma_true = sigma_true,
      n = n[1L],
      df = df,
      se = se,
      ncp = ncp,
      crit = crit_out,
      crit_label = "t*",
      tcrit = crit_out,
      reject_region = reject_region,
      region = reject_region
    )

    if (!quiet) {
      fmt <- function(x) .ci_format_num(x, digits)

      reject_region_print <- .ht_build_region(
        stat_name = "T",
        alternative = alternative,
        crit = crit_out,
        formatter = fmt
      )

      cat("\n", out$method, "\n", sep = "")
      cat("Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ", fmt(out$null), "\n", sep = "")
      cat("alpha = ", fmt(out$alpha), "   df = ", fmt(out$df), "\n", sep = "")
      cat("mu_a = ", fmt(out$true_value), "  mu0 = ", fmt(out$null),
          "  sigma_true = ", fmt(out$sigma_true), "  n = ", out$n, "\n", sep = "")
      cat("SE = ", fmt(out$se), "  ncp = ", fmt(out$ncp), "\n", sep = "")
      cat("Rejection region: ", reject_region_print, "\n", sep = "")
      cat("Power = ", fmt(out$power), "  Beta = ", fmt(out$beta), "\n", sep = "")
    }

    return(invisible(out))
  }

  method_type <- match.arg(method)
  scenario <- "two_sample"
  parameter <- "mu1 - mu2"
  true_value <- mu_a[1L] - mu_a[2L]
  n1 <- n[1L]
  n2 <- n[2L]

  sigma_true <- .power_validate_positive_numeric_vector(sigma_true, "sigma_true", fun)

  if (method_type == "pooled") {
    if (length(sigma_true) == 2L) {
      if (abs(sigma_true[1L] - sigma_true[2L]) > 1e-8) {
        stop(fun, ": method = 'pooled' assumes equal variances; provide scalar sigma_true or equal pair.")
      }
      sigma_true_used <- sigma_true[1L]
    } else if (length(sigma_true) == 1L) {
      sigma_true_used <- sigma_true[1L]
    } else {
      stop(fun, ": method = 'pooled' requires scalar sigma_true or a length-2 equal pair.")
    }

    method_label <- "Power for two-sample t test (pooled; equal variances; noncentral t)"
    df <- n1 + n2 - 2L
    se <- sigma_true_used * sqrt(1 / n1 + 1 / n2)
    ncp <- (true_value - mu0) / se
    sigma_true_out <- sigma_true_used
    assumptions <- c(
      "The two samples are independent",
      "The population variances are equal",
      "Each population is approximately normal or the sample sizes are large",
      "Noncentral t power is used"
    )
  } else {
    if (length(sigma_true) == 1L) {
      sigma_true <- rep(sigma_true, 2L)
    }
    if (length(sigma_true) != 2L) {
      stop(fun, ": method = 'welch' requires scalar sigma_true (recycled) or length-2 sigma_true.")
    }

    sigma1_true <- sigma_true[1L]
    sigma2_true <- sigma_true[2L]
    se <- sqrt(sigma1_true^2 / n1 + sigma2_true^2 / n2)
    df <- (sigma1_true^2 / n1 + sigma2_true^2 / n2)^2 /
      ((sigma1_true^2 / n1)^2 / (n1 - 1L) + (sigma2_true^2 / n2)^2 / (n2 - 1L))
    ncp <- (true_value - mu0) / se
    sigma_true_out <- sigma_true

    method_label <- "Power for two-sample t test (Welch; unequal variances; noncentral t approximation)"
    assumptions <- c(
      "The two samples are independent",
      "The population variances may differ",
      "Each population is approximately normal or the sample sizes are large",
      "A Welch noncentral t approximation is used"
    )
  }

  if (alternative == "two.sided") {
    tcrit <- stats::qt(1 - alpha / 2, df = df)
    crit_out <- c(lower = -tcrit, upper = tcrit)
    power <- stats::pt(-tcrit, df = df, ncp = ncp) +
      (1 - stats::pt(tcrit, df = df, ncp = ncp))
  } else if (alternative == "less") {
    tcrit <- stats::qt(alpha, df = df)
    crit_out <- tcrit
    power <- stats::pt(tcrit, df = df, ncp = ncp)
  } else {
    tcrit <- stats::qt(1 - alpha, df = df)
    crit_out <- tcrit
    power <- 1 - stats::pt(tcrit, df = df, ncp = ncp)
  }

  power <- .power_clip_prob(power)
  beta <- 1 - power

  reject_region <- .ht_build_region(
    stat_name = "T",
    alternative = alternative,
    crit = crit_out
  )

  out <- .new_power_result(
    class_name = "power_t_mu_result",
    method = method_label,
    parameter = parameter,
    alternative = alternative,
    alpha = alpha,
    null = mu0,
    true_value = true_value,
    power = power,
    beta = beta,
    assumptions = assumptions,
    inputs = list(mu_a = mu_a, mu0 = mu0, sigma_true = sigma_true,
                  n = n, alpha = alpha, method = method_type),
    scenario = scenario,
    method_type = method_type,
    mu_a = mu_a,
    mu1_a = mu_a[1L],
    mu2_a = mu_a[2L],
    sigma_true = sigma_true_out,
    n = n,
    n1 = n1,
    n2 = n2,
    delta_a = true_value,
    df = df,
    se = se,
    ncp = ncp,
    crit = crit_out,
    crit_label = "t*",
    tcrit = crit_out,
    reject_region = reject_region,
    region = reject_region
  )

  if (!quiet) {
    fmt <- function(x) .ci_format_num(x, digits)

    reject_region_print <- .ht_build_region(
      stat_name = "T",
      alternative = alternative,
      crit = crit_out,
      formatter = fmt
    )

    cat("\n", out$method, "\n", sep = "")
    cat("Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ", fmt(out$null), "\n", sep = "")
    cat("alpha = ", fmt(out$alpha), "   df = ", fmt(out$df), "\n", sep = "")
    cat("mu1_a = ", fmt(out$mu1_a), "  mu2_a = ", fmt(out$mu2_a),
        "  delta0 = ", fmt(out$null), "\n", sep = "")
    cat("n1 = ", out$n1, "  n2 = ", out$n2, "\n", sep = "")
    if (out$method_type == "pooled") {
      cat("sigma_true = ", fmt(out$sigma_true), "\n", sep = "")
    } else {
      cat("sigma1_true = ", fmt(out$sigma_true[1L]),
          "  sigma2_true = ", fmt(out$sigma_true[2L]), "\n", sep = "")
    }
    cat("SE = ", fmt(out$se), "  ncp = ", fmt(out$ncp), "\n", sep = "")
    cat("Rejection region: ", reject_region_print, "\n", sep = "")
    cat("Power = ", fmt(out$power), "  Beta = ", fmt(out$beta), "\n", sep = "")
  }

  invisible(out)
}


#' @describeIn power_functions Power for z tests of proportions.
#' @export
power_p_z <- function(p_a, p0, n,
                      alpha = 0.05,
                      alternative = c("two.sided", "less", "greater"),
                      digits = 4,
                      quiet = FALSE,
                      pooled = NULL,
                      continuity = FALSE) {
  fun <- "power_p_z()"
  pooled_supplied <- !missing(pooled)
  continuity_supplied <- !missing(continuity)

  alpha <- .ht_validate_alpha(alpha, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  alternative <- match.arg(alternative)
  continuity <- .ci_validate_flag(continuity, "continuity", fun)
  pooled <- .ht_validate_optional_flag(pooled, "pooled", fun)

  if (!is.numeric(p_a) || length(p_a) == 0L || any(!is.finite(p_a))) {
    stop(fun, ": p_a must be a finite numeric vector.")
  }
  if (any(p_a < 0 | p_a > 1)) {
    stop(fun, ": all values of p_a must be between 0 and 1.")
  }

  k <- length(p_a)
  if (!k %in% c(1L, 2L)) {
    stop(fun, ": p_a must have length 1 (one-sample) or 2 (two-sample).")
  }

  n <- .ht_validate_integer_vector(n, "n", fun, min_value = 1L)
  if (k == 2L && length(n) == 1L) {
    n <- rep(n, 2L)
  }
  if (length(n) != k) {
    stop(fun, ": n must have the same length as p_a.")
  }

  fmt <- function(y) .ci_format_num(y, digits)

  if (k == 1L) {
    if (pooled_supplied && !is.null(pooled)) {
      .ht_warn_ignored_arg(fun, "pooled", "ignored in the one-sample proportion z power calculation.")
    }
    if (continuity_supplied && continuity) {
      .ht_warn_ignored_arg(fun, "continuity", "ignored in the one-sample proportion z power calculation.")
    }

    p_a1 <- .ht_validate_probability_closed(p_a[1L], "p_a", fun)
    p0 <- .ht_validate_probability_closed(p0, "p0", fun)
    n1 <- n[1L]

    se0 <- sqrt(p0 * (1 - p0) / n1)
    sd_alt <- sqrt(p_a1 * (1 - p_a1) / n1)
    boundary_degenerate <- isTRUE(all.equal(sd_alt, 0))

    adequacy_ok <- all(c(
      n1 * p0,
      n1 * (1 - p0),
      n1 * p_a1,
      n1 * (1 - p_a1)
    ) >= 5)

    if (!adequacy_ok) {
      .power_warn_prop_approx(fun, scenario = "one_sample")
    }

    if (alternative == "less") {
      zcrit <- stats::qnorm(alpha)
      phat_crit <- p0 + zcrit * se0
      crit_out <- zcrit
      est_crit_out <- phat_crit
      power <- .power_prob_less_normal(phat_crit, mean = p_a1, sd = sd_alt)
    } else if (alternative == "greater") {
      zcrit <- stats::qnorm(1 - alpha)
      phat_crit <- p0 + zcrit * se0
      crit_out <- zcrit
      est_crit_out <- phat_crit
      power <- .power_prob_greater_normal(phat_crit, mean = p_a1, sd = sd_alt)
    } else {
      zcrit <- stats::qnorm(1 - alpha / 2)
      phat_lo <- p0 - zcrit * se0
      phat_hi <- p0 + zcrit * se0
      crit_out <- c(lower = -zcrit, upper = zcrit)
      est_crit_out <- c(lower = phat_lo, upper = phat_hi)
      power <- .power_prob_two_sided_normal(phat_lo, phat_hi, mean = p_a1, sd = sd_alt)
    }

    power <- .power_clip_prob(power)
    beta <- 1 - power

    reject_region <- .ht_build_region(
      stat_name = "Z",
      alternative = alternative,
      crit = crit_out,
      estimate_crit = est_crit_out,
      estimate_label = "phat",
      corrected = FALSE
    )

    out <- .new_power_result(
      class_name = "power_p_z_result",
      method = "Power for one-sample proportion z test (normal approximation)",
      parameter = "p",
      alternative = alternative,
      alpha = alpha,
      null = p0,
      true_value = p_a1,
      power = power,
      beta = beta,
      assumptions = c(
        "Large-sample normal approximation is used"
      ),
      inputs = list(
        p_a = p_a1,
        p0 = p0,
        n = n1,
        alpha = alpha,
        alternative = alternative
      ),
      scenario = "one_sample",
      p_a = p_a1,
      p0 = p0,
      n = n1,
      se0 = se0,
      sd_alt = sd_alt,
      sd_phat_a = sd_alt,
      boundary_degenerate = boundary_degenerate,
      approximation_ok = adequacy_ok,
      adequacy_ok = adequacy_ok,
      pooled = NULL,
      p_pool = NULL,
      continuity = FALSE,
      continuity_correction = FALSE,
      crit = crit_out,
      crit_label = "z critical value",
      zcrit = crit_out,
      estimate_crit = est_crit_out,
      reject_region = reject_region,
      region = reject_region
    )

    if (!quiet) {
      reject_region_print <- .ht_build_region(
        stat_name = "Z",
        alternative = alternative,
        crit = crit_out,
        estimate_crit = est_crit_out,
        estimate_label = "phat",
        corrected = FALSE,
        formatter = fmt
      )

      cat("\n", out$method, "\n", sep = "")
      cat("Ha: p ", .ht_alt_symbol(alternative), " ", fmt(p0), "\n", sep = "")
      cat("alpha = ", fmt(alpha), "\n", sep = "")
      cat("p_a = ", fmt(p_a1), "  p0 = ", fmt(p0), "  n = ", n1, "\n", sep = "")
      cat("SE0 = ", fmt(se0), "  SD(phat | alt) = ", fmt(out$sd_alt), "\n", sep = "")
      cat("Rejection region: ", reject_region_print, "\n", sep = "")
      cat("Power = ", sprintf("%.4f", power), "  Beta = ", sprintf("%.4f", beta), "\n", sep = "")
    }

    return(invisible(out))
  }

  # two-sample
  delta0 <- .ht_validate_difference_null(p0, "p0", fun)

  if (!is.null(pooled) && isTRUE(pooled) && !isTRUE(all.equal(delta0, 0))) {
    stop(fun, ": pooled = TRUE is only valid when p0 = 0 in the two-sample case.")
  }

  opts <- .ht_resolve_two_prop_options(delta0, pooled, continuity, fun)
  pooled <- opts$pooled
  continuity <- opts$continuity

  if (alternative != "two.sided" && continuity) {
    .ht_warn_ignored_arg(fun, "continuity", "ignored unless alternative = 'two.sided' in the two-sample proportion power calculation.")
    continuity <- FALSE
  }

  p1_a <- p_a[1L]
  p2_a <- p_a[2L]
  n1 <- n[1L]
  n2 <- n[2L]
  delta_a <- p1_a - p2_a

  sd_alt <- sqrt(p1_a * (1 - p1_a) / n1 + p2_a * (1 - p2_a) / n2)
  boundary_degenerate <- isTRUE(all.equal(sd_alt, 0))

  if (pooled) {
    p_pool <- (n1 * p1_a + n2 * p2_a) / (n1 + n2)
    se0 <- sqrt(p_pool * (1 - p_pool) * (1 / n1 + 1 / n2))
    adequacy_ok <- all(c(
      n1 * p_pool, n1 * (1 - p_pool),
      n2 * p_pool, n2 * (1 - p_pool),
      n1 * p1_a, n1 * (1 - p1_a),
      n2 * p2_a, n2 * (1 - p2_a)
    ) >= 5)
  } else {
    p_pool <- NA_real_
    se0 <- sd_alt
    adequacy_ok <- all(c(
      n1 * p1_a, n1 * (1 - p1_a),
      n2 * p2_a, n2 * (1 - p2_a)
    ) >= 5)
  }

  if (!adequacy_ok) {
    .power_warn_prop_approx(fun, scenario = "two_sample")
  }

  if (!is.finite(se0) || se0 <= 0) {
    stop(fun, ": the two-sample power calculation is undefined because SE0 is zero or non-finite.")
  }

  cc <- if (continuity) 0.5 * (1 / n1 + 1 / n2) else 0

  if (alternative == "less") {
    zcrit <- stats::qnorm(alpha)
    dcrit <- delta0 + zcrit * se0
    crit_out <- zcrit
    est_crit_out <- dcrit
    power <- .power_prob_less_normal(dcrit, mean = delta_a, sd = sd_alt)
  } else if (alternative == "greater") {
    zcrit <- stats::qnorm(1 - alpha)
    dcrit <- delta0 + zcrit * se0
    crit_out <- zcrit
    est_crit_out <- dcrit
    power <- .power_prob_greater_normal(dcrit, mean = delta_a, sd = sd_alt)
  } else {
    zcrit <- stats::qnorm(1 - alpha / 2)
    band <- zcrit * se0 + cc
    dlo <- delta0 - band
    dhi <- delta0 + band
    crit_out <- c(lower = -zcrit, upper = zcrit)
    est_crit_out <- c(lower = dlo, upper = dhi)
    power <- .power_prob_two_sided_normal(dlo, dhi, mean = delta_a, sd = sd_alt)
  }

  power <- .power_clip_prob(power)
  beta <- 1 - power

  reject_region <- .ht_build_region(
    stat_name = "Z",
    alternative = alternative,
    crit = crit_out,
    estimate_crit = est_crit_out,
    estimate_label = "diff_hat",
    corrected = continuity
  )

  out <- .new_power_result(
    class_name = "power_p_z_result",
    method = paste0(
      "Power for two-sample proportion z test (",
      .ht_two_prop_method_suffix(delta0, pooled, continuity),
      ")"
    ),
    parameter = "p1 - p2",
    alternative = alternative,
    alpha = alpha,
    null = delta0,
    true_value = delta_a,
    power = power,
    beta = beta,
    assumptions = c(
      "The two samples are independent",
      "Large-sample normal approximation is used"
    ),
    inputs = list(
      p_a = c(p1_a, p2_a),
      p0 = delta0,
      n = c(n1, n2),
      alpha = alpha,
      alternative = alternative,
      pooled = pooled,
      continuity = continuity
    ),
    scenario = "two_sample",
    p_a = c(p1_a, p2_a),
    p1_a = p1_a,
    p2_a = p2_a,
    delta_a = delta_a,
    delta0 = delta0,
    n = c(n1, n2),
    n1 = n1,
    n2 = n2,
    se0 = se0,
    sd_alt = sd_alt,
    sd_diff_a = sd_alt,
    p_pool = p_pool,
    pooled = pooled,
    continuity = continuity,
    continuity_correction = continuity,
    cc = cc,
    boundary_degenerate = boundary_degenerate,
    approximation_ok = adequacy_ok,
    adequacy_ok = adequacy_ok,
    crit = crit_out,
    crit_label = "z critical value",
    zcrit = crit_out,
    estimate_crit = est_crit_out,
    reject_region = reject_region,
    region = reject_region
  )

  if (!quiet) {
    reject_region_print <- .ht_build_region(
      stat_name = "Z",
      alternative = alternative,
      crit = crit_out,
      estimate_crit = est_crit_out,
      estimate_label = "diff_hat",
      corrected = continuity,
      formatter = fmt
    )

    cat("\n", out$method, "\n", sep = "")
    cat("Ha: p1 - p2 ", .ht_alt_symbol(alternative), " ", fmt(delta0), "\n", sep = "")
    cat("alpha = ", fmt(alpha), "\n", sep = "")
    cat(
      "p1_a = ", fmt(p1_a), "  p2_a = ", fmt(p2_a),
      "  delta0 = ", fmt(delta0), "\n",
      sep = ""
    )
    cat("n1 = ", n1, "  n2 = ", n2, "\n", sep = "")
    cat(
      "SE0 = ", fmt(se0),
      if (pooled) paste0("  (pooled p = ", fmt(p_pool), ")") else "  (unpooled)",
      "  SD(diff_hat | alt) = ", fmt(out$sd_alt), "\n",
      sep = ""
    )
    if (continuity) {
      cat("cc = ", fmt(cc), "\n", sep = "")
    }
    cat("Rejection region: ", reject_region_print, "\n", sep = "")
    cat("Power = ", sprintf("%.4f", power), "  Beta = ", sprintf("%.4f", beta), "\n", sep = "")
  }

  invisible(out)
}


#' @describeIn power_functions Power for one-sample chi-square tests of variance.
#' @export
power_var_chisq <- function(sigma_a, sigma0, n, alpha = 0.05,
                            alternative = c("two.sided", "less", "greater"),
                            digits = 4, quiet = FALSE) {
  fun <- "power_var_chisq()"

  alpha <- .ht_validate_alpha(alpha, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  alternative <- match.arg(alternative)
  sigma_a <- .ht_validate_positive_scalar(sigma_a, "sigma_a", fun)
  sigma0 <- .ht_validate_positive_scalar(sigma0, "sigma0", fun)
  n <- .ht_validate_integer_vector(n, "n", fun, min_value = 2L)

  if (length(n) != 1L) {
    stop(fun, ": n must be a single integer >= 2.")
  }

  df <- n[1L] - 1L
  k <- (sigma_a^2) / (sigma0^2)

  if (alternative == "two.sided") {
    chi_lo <- stats::qchisq(alpha / 2, df = df)
    chi_hi <- stats::qchisq(1 - alpha / 2, df = df)
    crit_out <- c(lower = chi_lo, upper = chi_hi)
    power <- stats::pchisq(chi_lo / k, df = df) +
      (1 - stats::pchisq(chi_hi / k, df = df))
  } else if (alternative == "less") {
    chi_crit <- stats::qchisq(alpha, df = df)
    crit_out <- chi_crit
    power <- stats::pchisq(chi_crit / k, df = df)
  } else {
    chi_crit <- stats::qchisq(1 - alpha, df = df)
    crit_out <- chi_crit
    power <- 1 - stats::pchisq(chi_crit / k, df = df)
  }

  power <- .power_clip_prob(power)
  beta <- 1 - power

  reject_region <- .ht_build_region_nonsymmetric(
  stat_name = "Chi^2",
  alternative = alternative,
  crit = crit_out
)

  out <- .new_power_result(
    class_name = "power_var_chisq_result",
    method = "Power for one-sample chi-square test for variance",
    parameter = "sigma^2",
    alternative = alternative,
    alpha = alpha,
    null = sigma0^2,
    true_value = sigma_a^2,
    power = power,
    beta = beta,
    assumptions = c(
      "Observations are independent",
      "The population is normal"
    ),
    inputs = list(sigma_a = sigma_a, sigma0 = sigma0, n = n[1L], alpha = alpha),
    scenario = "one_sample",
    sigma_a = sigma_a,
    sigma0 = sigma0,
    n = n[1L],
    df = df,
    k = k,
    crit = crit_out,
    crit_label = "chi-square critical value",
    chi_crit = crit_out,
    reject_region = reject_region,
    region = reject_region
  )

  if (!quiet) {
    fmt <- function(x) .ci_format_num(x, digits)

    reject_region_print <- .ht_build_region_nonsymmetric(
  stat_name = "Chi^2",
  alternative = alternative,
  crit = crit_out,
  formatter = fmt
)

    cat("\n", out$method, "\n", sep = "")
    cat("Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ", fmt(out$null), "\n", sep = "")
    cat("alpha = ", fmt(out$alpha), "   df = ", fmt(out$df), "\n", sep = "")
    cat("sigma_a = ", fmt(out$sigma_a), "  sigma0 = ", fmt(out$sigma0),
        "  k = ", fmt(out$k), "\n", sep = "")
    cat("Rejection region: ", reject_region_print, "\n", sep = "")
    cat("Power = ", fmt(out$power), "  Beta = ", fmt(out$beta), "\n", sep = "")
  }

  invisible(out)
}


#' @describeIn power_functions Power for two-sample F tests of a variance ratio.
#' @export
power_var_ratio_F <- function(sigma_a, ratio0 = 1, n, alpha = 0.05,
                              alternative = c("two.sided", "less", "greater"),
                              digits = 4, quiet = FALSE) {
  fun <- "power_var_ratio_F()"

  alpha <- .ht_validate_alpha(alpha, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  alternative <- match.arg(alternative)
  sigma_a <- .power_validate_positive_numeric_vector(sigma_a, "sigma_a", fun)

  if (length(sigma_a) != 2L) {
    stop(fun, ": sigma_a must be length 2: c(sigma1_a, sigma2_a).")
  }

  ratio0 <- .ht_validate_positive_scalar(ratio0, "ratio0", fun)
  n <- .ht_validate_integer_vector(n, "n", fun, min_value = 2L)

  if (length(n) != 2L) {
    stop(fun, ": n must be c(n1, n2) with both >= 2.")
  }

  sigma1_a <- sigma_a[1L]
  sigma2_a <- sigma_a[2L]
  n1 <- n[1L]
  n2 <- n[2L]
  df1 <- n1 - 1L
  df2 <- n2 - 1L
  ratio_a <- (sigma1_a^2) / (sigma2_a^2)
  k <- ratio_a / ratio0

  if (alternative == "two.sided") {
    flo <- stats::qf(alpha / 2, df1 = df1, df2 = df2)
    fhi <- stats::qf(1 - alpha / 2, df1 = df1, df2 = df2)
    crit_out <- c(lower = flo, upper = fhi)
    power <- stats::pf(flo / k, df1 = df1, df2 = df2) +
      (1 - stats::pf(fhi / k, df1 = df1, df2 = df2))
  } else if (alternative == "less") {
    fcrit <- stats::qf(alpha, df1 = df1, df2 = df2)
    crit_out <- fcrit
    power <- stats::pf(fcrit / k, df1 = df1, df2 = df2)
  } else {
    fcrit <- stats::qf(1 - alpha, df1 = df1, df2 = df2)
    crit_out <- fcrit
    power <- 1 - stats::pf(fcrit / k, df1 = df1, df2 = df2)
  }

  power <- .power_clip_prob(power)
  beta <- 1 - power

  reject_region <- .ht_build_region_nonsymmetric(
  stat_name = "F",
  alternative = alternative,
  crit = crit_out
)

  out <- .new_power_result(
    class_name = "power_var_ratio_F_result",
    method = "Power for two-sample F test of variance ratio",
    parameter = "sigma1^2 / sigma2^2",
    alternative = alternative,
    alpha = alpha,
    null = ratio0,
    true_value = ratio_a,
    power = power,
    beta = beta,
    assumptions = c(
      "The two samples are independent",
      "Both populations are normal"
    ),
    inputs = list(sigma_a = sigma_a, ratio0 = ratio0, n = n, alpha = alpha),
    scenario = "two_sample",
    sigma_a = sigma_a,
    sigma1_a = sigma1_a,
    sigma2_a = sigma2_a,
    n = n,
    n1 = n1,
    n2 = n2,
    df1 = df1,
    df2 = df2,
    ratio_a = ratio_a,
    ratio0 = ratio0,
    k = k,
    crit = crit_out,
    crit_label = "F critical value",
    f_crit = crit_out,
    reject_region = reject_region,
    region = reject_region
  )

  if (!quiet) {
    fmt <- function(x) .ci_format_num(x, digits)

    reject_region_print <- .ht_build_region_nonsymmetric(
  stat_name = "F",
  alternative = alternative,
  crit = crit_out,
  formatter = fmt
)

    cat("\n", out$method, "\n", sep = "")
    cat("Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ", fmt(out$null), "\n", sep = "")
    cat("alpha = ", fmt(out$alpha), "   df1 = ", fmt(out$df1),
        "  df2 = ", fmt(out$df2), "\n", sep = "")
    cat("ratio_a = ", fmt(out$ratio_a), "  ratio0 = ", fmt(out$ratio0),
        "  k = ", fmt(out$k), "\n", sep = "")
    cat("Rejection region: ", reject_region_print, "\n", sep = "")
    cat("Power = ", fmt(out$power), "  Beta = ", fmt(out$beta), "\n", sep = "")
  }

  invisible(out)
}
