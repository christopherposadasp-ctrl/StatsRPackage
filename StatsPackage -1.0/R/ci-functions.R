#' Confidence interval functions
#'
#' Computes confidence intervals for means, proportions, variances, and the
#' exponential rate parameter.
#'
#' These functions return classed result objects with unrounded stored values
#' and optional printed summaries controlled by `digits` and `quiet`.
#'
#' @param xbar Numeric vector of sample means or paired-difference mean summaries.
#' @param n Integer sample size or vector of sample sizes.
#' @param s Sample standard deviation input when sigma is unknown.
#' @param sigma Known population standard deviation input.
#' @param conf.level Confidence level in (0, 1).
#' @param paired Logical; for `ci_mu()`, whether the supplied summaries are for paired differences.
#' @param side Interval side specification: `"two.sided"`, `"lower"`, or `"upper"`.
#' @param method Method for the two-sample sigma-unknown mean interval.
#' @param digits Integer number of decimal places used only for printed output.
#' @param quiet Logical; if `TRUE`, suppress printed output.
#' @param x Count input for proportion, variance, or exponential-rate intervals.
#' @param exact_1s Logical; for `ci_p()`, whether to use exact one-sided bounds.
#' @param Sum Sum of observations for the exponential-rate interval.
#' @name ci_functions
NULL

# Confidence interval functions -------------------------------------------

#' @describeIn ci_functions Confidence interval for a mean or mean difference.
#' @export
ci_mu <- function(xbar, n,
                  s = NULL, sigma = NULL,
                  conf.level = 0.95,
                  paired = FALSE,
                  side = c("two.sided", "lower", "upper"),
                  method = c("welch", "pooled", "z"),
                  digits = 4, quiet = FALSE) {
  fun <- "ci_mu()"
  method_supplied <- !missing(method)

  .ci_validate_conf_level(conf.level, fun)
  digits <- .ci_validate_digits(digits, fun)
  paired <- .ci_validate_flag(paired, "paired", fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)

  side <- match.arg(side)
  method <- match.arg(method)

  alpha <- 1 - conf.level
  alpha_tail <- if (side == "two.sided") alpha / 2 else alpha

  crit_z <- function() stats::qnorm(1 - alpha_tail)
  crit_t <- function(df) stats::qt(1 - alpha_tail, df = df)

  mk_ci <- function(est, E) {
    if (side == "two.sided") {
      c(lower = est - E, upper = est + E)
    } else if (side == "lower") {
      c(lower = est - E, upper = Inf)
    } else {
      c(lower = -Inf, upper = est + E)
    }
  }

  if (!is.numeric(xbar) || length(xbar) == 0L || any(!is.finite(xbar))) {
    stop(fun, ": xbar must be a finite numeric vector.")
  }
  if (!is.numeric(n) || length(n) == 0L) {
    stop(fun, ": n must be numeric.")
  }

  k <- length(xbar)

  if (!k %in% c(1L, 2L)) {
    stop(fun, ": xbar must have length 1 (one-sample/paired) or 2 (two-sample).")
  }
  if (length(n) != k) {
    stop(fun, ": n must have the same length as xbar.")
  }
  if (any(!.ci_is_whole_number(n)) || any(n < 1)) {
    stop(fun, ": n must contain integer sample sizes >= 1.")
  }
  n <- as.integer(round(n))

  sigma_known <- !is.null(sigma)
  s_known <- !is.null(s)

  if (sigma_known && s_known) {
    stop(fun, ": provide only one of sigma= (known) or s= (sample SD), not both.")
  }
  if (!sigma_known && !s_known) {
    stop(fun, ": provide either sigma= (known) or s= (sample SD).")
  }

  if (paired && method_supplied) {
  warning(
    fun,
    ": method is ignored for paired intervals; paired intervals are t-based on the paired differences.",
    call. = FALSE
  )
} else if (k == 1L && method_supplied) {
  warning(
    fun,
    ": method is ignored for one-sample intervals; use sigma= for z intervals or s= for t intervals.",
    call. = FALSE
  )
} else if (k == 2L && sigma_known && method_supplied && method != "z") {
  warning(
    fun,
    ": for two-sample known-sigma intervals, the procedure is z-based; use method = 'z' or omit method.",
    call. = FALSE
  )
}

  if (paired) {
    if (k != 1L) {
      stop(fun, ": paired = TRUE expects scalar summaries for differences.")
    }
    if (!s_known) {
      stop(fun, ": paired = TRUE requires s = SD of the paired differences.")
    }
    if (!is.numeric(s) || length(s) != 1L || !is.finite(s) || s < 0) {
      stop(fun, ": for paired intervals, s must be a single finite number >= 0.")
    }
    if (n[1L] < 2L) {
      stop(fun, ": paired t intervals require n >= 2.")
    }

    df <- n[1L] - 1L
    se <- s / sqrt(n[1L])
    crit <- crit_t(df)
    E <- crit * se
    ci <- mk_ci(xbar[1L], E)

    out <- .new_ci_result(
      class_name = "ci_mu_result",
      method = "Paired CI for mean difference (t; summary stats on differences)",
      parameter = "mu_d",
      conf.level = conf.level,
      alpha = alpha,
      side = side,
      estimate = xbar[1L],
      ci = ci,
      assumptions = c(
        "Paired observations are reduced to differences",
        "Differences are independent",
        "Differences are approximately normal or the number of pairs is large"
      ),
      inputs = list(xbar = xbar[1L], n = n[1L], s = s, sigma = NULL, paired = TRUE),
      procedure = "paired_t",
      se = se,
      df = df,
      crit = crit,
      crit_label = "t*",
      margin = E
    )

    if (!quiet) {
      cat("\n", out$method, "\n", sep = "")
      cat("conf.level = ", .ci_format_num(out$conf.level, digits),
          "   df = ", .ci_format_num(out$df, digits), "\n", sep = "")
      cat("dbar = ", .ci_format_num(out$estimate, digits),
          "  s_d = ", .ci_format_num(s, digits),
          "  n = ", n[1L], "\n", sep = "")
      cat("SE = ", .ci_format_num(out$se, digits),
          "  ", out$crit_label, " = ", .ci_format_num(out$crit, digits),
          "  E = ", .ci_format_num(out$margin, digits), "\n", sep = "")
      cat("CI: ", .ci_format_ci(out$ci, digits), "\n", sep = "")
    }

    return(invisible(out))
  }

  if (k == 1L) {
    if (sigma_known) {
      if (!is.numeric(sigma) || length(sigma) != 1L || !is.finite(sigma) || sigma < 0) {
        stop(fun, ": for one-sample z intervals, sigma must be a single finite number >= 0.")
      }

      se <- sigma / sqrt(n[1L])
      crit <- crit_z()
      E <- crit * se
      ci <- mk_ci(xbar[1L], E)

      out <- .new_ci_result(
        class_name = "ci_mu_result",
        method = "One-sample CI for mu (z; sigma known; summary stats)",
        parameter = "mu",
        conf.level = conf.level,
        alpha = alpha,
        side = side,
        estimate = xbar[1L],
        ci = ci,
        assumptions = c(
          "Observations are independent",
          "Population sigma is known",
          "Population is normal or the sample size is large"
        ),
        inputs = list(xbar = xbar[1L], n = n[1L], s = NULL, sigma = sigma, paired = FALSE),
        procedure = "one_z_known_sigma",
        se = se,
        df = NA_real_,
        crit = crit,
        crit_label = "z*",
        margin = E
      )

      if (!quiet) {
        cat("\n", out$method, "\n", sep = "")
        cat("conf.level = ", .ci_format_num(out$conf.level, digits), "\n", sep = "")
        cat("xbar = ", .ci_format_num(out$estimate, digits),
            "  sigma = ", .ci_format_num(sigma, digits),
            "  n = ", n[1L], "\n", sep = "")
        cat("SE = ", .ci_format_num(out$se, digits),
            "  ", out$crit_label, " = ", .ci_format_num(out$crit, digits),
            "  E = ", .ci_format_num(out$margin, digits), "\n", sep = "")
        cat("CI: ", .ci_format_ci(out$ci, digits), "\n", sep = "")
      }

      return(invisible(out))
    }

    if (!is.numeric(s) || length(s) != 1L || !is.finite(s) || s < 0) {
      stop(fun, ": for one-sample t intervals, s must be a single finite number >= 0.")
    }
    if (n[1L] < 2L) {
      stop(fun, ": one-sample t intervals require n >= 2.")
    }

    df <- n[1L] - 1L
    se <- s / sqrt(n[1L])
    crit <- crit_t(df)
    E <- crit * se
    ci <- mk_ci(xbar[1L], E)

    out <- .new_ci_result(
      class_name = "ci_mu_result",
      method = "One-sample CI for mu (t; sigma unknown; summary stats)",
      parameter = "mu",
      conf.level = conf.level,
      alpha = alpha,
      side = side,
      estimate = xbar[1L],
      ci = ci,
      assumptions = c(
        "Observations are independent",
        "Population is approximately normal or the sample size is large",
        "Population sigma is unknown"
      ),
      inputs = list(xbar = xbar[1L], n = n[1L], s = s, sigma = NULL, paired = FALSE),
      procedure = "one_t",
      se = se,
      df = df,
      crit = crit,
      crit_label = "t*",
      margin = E
    )

    if (!quiet) {
      cat("\n", out$method, "\n", sep = "")
      cat("conf.level = ", .ci_format_num(out$conf.level, digits),
          "   df = ", .ci_format_num(out$df, digits), "\n", sep = "")
      cat("xbar = ", .ci_format_num(out$estimate, digits),
          "  s = ", .ci_format_num(s, digits),
          "  n = ", n[1L], "\n", sep = "")
      cat("SE = ", .ci_format_num(out$se, digits),
          "  ", out$crit_label, " = ", .ci_format_num(out$crit, digits),
          "  E = ", .ci_format_num(out$margin, digits), "\n", sep = "")
      cat("CI: ", .ci_format_ci(out$ci, digits), "\n", sep = "")
    }

    return(invisible(out))
  }

  xbar1 <- xbar[1L]
  xbar2 <- xbar[2L]
  n1 <- n[1L]
  n2 <- n[2L]
  diff_hat <- xbar1 - xbar2

  if (sigma_known) {
    if (!is.numeric(sigma) || length(sigma) != 2L || any(!is.finite(sigma)) || any(sigma < 0)) {
      stop(fun, ": for two-sample z intervals, sigma must be a length-2 finite vector with entries >= 0.")
    }

    se <- sqrt(sigma[1L]^2 / n1 + sigma[2L]^2 / n2)
    crit <- crit_z()
    E <- crit * se
    ci <- mk_ci(diff_hat, E)

    out <- .new_ci_result(
      class_name = "ci_mu_result",
      method = "Two-sample CI for mu1 - mu2 (z; sigmas known; summary stats)",
      parameter = "mu1 - mu2",
      conf.level = conf.level,
      alpha = alpha,
      side = side,
      estimate = diff_hat,
      ci = ci,
      assumptions = c(
        "The two samples are independent",
        "Population sigmas are known",
        "Each population is normal or the sample sizes are large"
      ),
      inputs = list(xbar = xbar, n = n, s = NULL, sigma = sigma, paired = FALSE),
      procedure = "two_z_known_sigma",
      se = se,
      df = NA_real_,
      crit = crit,
      crit_label = "z*",
      margin = E
    )

    if (!quiet) {
      cat("\n", out$method, "\n", sep = "")
      cat("conf.level = ", .ci_format_num(out$conf.level, digits), "\n", sep = "")
      cat("xbar1 - xbar2 = ", .ci_format_num(out$estimate, digits),
          "  (xbar1 = ", .ci_format_num(xbar1, digits),
          ", xbar2 = ", .ci_format_num(xbar2, digits), ")\n", sep = "")
      cat("sigma1 = ", .ci_format_num(sigma[1L], digits),
          "  sigma2 = ", .ci_format_num(sigma[2L], digits),
          "  n1 = ", n1,
          "  n2 = ", n2, "\n", sep = "")
      cat("SE = ", .ci_format_num(out$se, digits),
          "  ", out$crit_label, " = ", .ci_format_num(out$crit, digits),
          "  E = ", .ci_format_num(out$margin, digits), "\n", sep = "")
      cat("CI: ", .ci_format_ci(out$ci, digits), "\n", sep = "")
    }

    return(invisible(out))
  }

  if (!is.numeric(s) || length(s) != 2L || any(!is.finite(s)) || any(s < 0)) {
    stop(fun, ": for two-sample sigma-unknown intervals, s must be a length-2 finite vector with entries >= 0.")
  }
  if (n1 < 2L || n2 < 2L) {
    stop(fun, ": two-sample sigma-unknown intervals require n1 >= 2 and n2 >= 2.")
  }

  s1 <- s[1L]
  s2 <- s[2L]

  if (method == "z") {
    .ci_warn_z_unknown_sigma(c(n1, n2), fun)
    se <- sqrt(s1^2 / n1 + s2^2 / n2)
    crit <- crit_z()
    df <- NA_real_
    E <- crit * se
    ci <- mk_ci(diff_hat, E)
    method_label <- "Two-sample CI for mu1 - mu2 (z approx; sigmas unknown; summary stats)"
    crit_label <- "z*"
    procedure <- "two_z_unknown_sigma"
    assumptions <- c(
      "The two samples are independent",
      "Population sigmas are unknown",
      "A large-sample normal approximation is being used"
    )
  } else if (method == "pooled") {
    df <- n1 + n2 - 2L
    sp2 <- ((n1 - 1L) * s1^2 + (n2 - 1L) * s2^2) / df
    se <- sqrt(sp2 * (1 / n1 + 1 / n2))
    crit <- crit_t(df)
    E <- crit * se
    ci <- mk_ci(diff_hat, E)
    method_label <- "Two-sample CI for mu1 - mu2 (pooled t; equal variances; summary stats)"
    crit_label <- "t*"
    procedure <- "two_t_pooled"
    assumptions <- c(
      "The two samples are independent",
      "Each population is approximately normal or the sample sizes are large",
      "The population variances are equal"
    )
  } else {
    se <- sqrt(s1^2 / n1 + s2^2 / n2)

    if (se == 0) {
      df <- Inf
      crit <- stats::qnorm(1 - alpha_tail)
      crit_label <- "z*"
    } else {
      df <- (s1^2 / n1 + s2^2 / n2)^2 /
        ((s1^2 / n1)^2 / (n1 - 1L) + (s2^2 / n2)^2 / (n2 - 1L))
      crit <- crit_t(df)
      crit_label <- "t*"
    }

    E <- crit * se
    ci <- mk_ci(diff_hat, E)
    method_label <- "Two-sample CI for mu1 - mu2 (Welch t; unequal variances; summary stats)"
    procedure <- "two_t_welch"
    assumptions <- c(
      "The two samples are independent",
      "Each population is approximately normal or the sample sizes are large",
      "The population variances may differ"
    )
  }

  out <- .new_ci_result(
    class_name = "ci_mu_result",
    method = method_label,
    parameter = "mu1 - mu2",
    conf.level = conf.level,
    alpha = alpha,
    side = side,
    estimate = diff_hat,
    ci = ci,
    assumptions = assumptions,
    inputs = list(xbar = xbar, n = n, s = s, sigma = NULL, paired = FALSE, method = method),
    procedure = procedure,
    se = se,
    df = df,
    crit = crit,
    crit_label = crit_label,
    margin = E
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    if (is.na(out$df)) {
      cat("conf.level = ", .ci_format_num(out$conf.level, digits), "\n", sep = "")
    } else {
      cat("conf.level = ", .ci_format_num(out$conf.level, digits),
          "   df = ", .ci_format_num(out$df, digits), "\n", sep = "")
    }
    cat("xbar1 - xbar2 = ", .ci_format_num(out$estimate, digits),
        "  (xbar1 = ", .ci_format_num(xbar1, digits),
        ", xbar2 = ", .ci_format_num(xbar2, digits), ")\n", sep = "")
    cat("s1 = ", .ci_format_num(s1, digits),
        "  s2 = ", .ci_format_num(s2, digits),
        "  n1 = ", n1,
        "  n2 = ", n2, "\n", sep = "")
    cat("SE = ", .ci_format_num(out$se, digits),
        "  ", out$crit_label, " = ", .ci_format_num(out$crit, digits),
        "  E = ", .ci_format_num(out$margin, digits), "\n", sep = "")
    cat("CI: ", .ci_format_ci(out$ci, digits), "\n", sep = "")
  }

  invisible(out)
}


#' @describeIn ci_functions Confidence interval for a proportion or difference in proportions.
#' @export
ci_p <- function(x, n,
                 conf.level = 0.95,
                 exact_1s = TRUE,
                 digits = 4, quiet = FALSE) {
  fun <- "ci_p()"
  exact_1s_supplied <- !missing(exact_1s)

  .ci_validate_conf_level(conf.level, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  exact_1s <- .ci_validate_flag(exact_1s, "exact_1s", fun)

  alpha <- 1 - conf.level

  if (!is.numeric(x) || length(x) == 0L || any(!is.finite(x))) {
    stop(fun, ": x must be a finite numeric vector.")
  }
  if (!is.numeric(n) || length(n) == 0L || any(!is.finite(n))) {
    stop(fun, ": n must be a finite numeric vector.")
  }

  k <- length(x)

  if (!k %in% c(1L, 2L)) {
    stop(fun, ": x must have length 1 (one-sample) or 2 (two-sample).")
  }
  if (length(n) != k) {
    stop(fun, ": n must have the same length as x.")
  }
  if (any(!.ci_is_whole_number(x)) || any(!.ci_is_whole_number(n))) {
    stop(fun, ": x and n must contain integer counts.")
  }

  x <- as.integer(round(x))
  n <- as.integer(round(n))

  if (any(n <= 0L)) {
    stop(fun, ": n must contain integer sample sizes > 0.")
  }
  if (any(x < 0L) || any(x > n)) {
    stop(fun, ": require 0 <= x <= n componentwise.")
  }

  if (k == 2L && exact_1s_supplied) {
    warning(fun, ": exact_1s is ignored for the two-sample interval.", call. = FALSE)
  }

  if (k == 1L) {
    phat <- x[1L] / n[1L]

    if (exact_1s) {
      bt <- stats::binom.test(x = x[1L], n = n[1L], conf.level = conf.level)
      ci <- stats::setNames(unname(bt$conf.int), c("lower", "upper"))

      out <- .new_ci_result(
        class_name = "ci_p_result",
        method = "One-sample CI for p (exact Clopper-Pearson)",
        parameter = "p",
        conf.level = conf.level,
        alpha = alpha,
        side = "two.sided",
        estimate = phat,
        ci = ci,
        assumptions = c(
          "Observations are Bernoulli trials",
          "Trials are independent",
          "The success probability is constant across trials"
        ),
        inputs = list(x = x[1L], n = n[1L], exact_1s = TRUE),
        procedure = "one_exact",
        se = NA_real_,
        crit = NA_real_,
        margin = NA_real_,
        p1 = NA_real_,
        p2 = NA_real_
      )

      if (!quiet) {
        cat("\n", out$method, "\n", sep = "")
        cat("conf.level = ", .ci_format_num(out$conf.level, digits), "\n", sep = "")
        cat("phat = ", .ci_format_num(out$estimate, digits),
            "  (x = ", x[1L], ", n = ", n[1L], ")\n", sep = "")
        cat("CI: ", .ci_format_ci(out$ci, digits), "\n", sep = "")
      }

      return(invisible(out))
    }

    .ci_warn_prop_wald(x[1L], n[1L], fun)

    se <- sqrt(phat * (1 - phat) / n[1L])
    crit <- stats::qnorm(1 - alpha / 2)
    E <- crit * se
    ci <- c(lower = max(0, phat - E), upper = min(1, phat + E))

    out <- .new_ci_result(
      class_name = "ci_p_result",
      method = "One-sample CI for p (Wald normal approximation)",
      parameter = "p",
      conf.level = conf.level,
      alpha = alpha,
      side = "two.sided",
      estimate = phat,
      ci = ci,
      assumptions = c(
        "Observations are Bernoulli trials",
        "Trials are independent",
        "The success probability is constant across trials",
        "The normal approximation is adequate"
      ),
      inputs = list(x = x[1L], n = n[1L], exact_1s = FALSE),
      procedure = "one_wald",
      se = se,
      crit = crit,
      margin = E,
      p1 = NA_real_,
      p2 = NA_real_
    )

    if (!quiet) {
      cat("\n", out$method, "\n", sep = "")
      cat("conf.level = ", .ci_format_num(out$conf.level, digits), "\n", sep = "")
      cat("phat = ", .ci_format_num(out$estimate, digits),
          "  (x = ", x[1L], ", n = ", n[1L], ")\n", sep = "")
      cat("SE = ", .ci_format_num(out$se, digits),
          "  z* = ", .ci_format_num(out$crit, digits),
          "  E = ", .ci_format_num(out$margin, digits), "\n", sep = "")
      cat("CI: ", .ci_format_ci(out$ci, digits), "\n", sep = "")
    }

    return(invisible(out))
  }

  x1 <- x[1L]
  x2 <- x[2L]
  n1 <- n[1L]
  n2 <- n[2L]

  p1 <- x1 / n1
  p2 <- x2 / n2
  diff_hat <- p1 - p2

  .ci_warn_prop_wald(c(x1, x2), c(n1, n2), fun)

  se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  crit <- stats::qnorm(1 - alpha / 2)
  E <- crit * se
  ci <- c(lower = diff_hat - E, upper = diff_hat + E)

  out <- .new_ci_result(
    class_name = "ci_p_result",
    method = "Two-sample CI for p1 - p2 (Wald; unpooled normal approximation)",
    parameter = "p1 - p2",
    conf.level = conf.level,
    alpha = alpha,
    side = "two.sided",
    estimate = diff_hat,
    ci = ci,
    assumptions = c(
      "The two samples are independent",
      "Each sample consists of independent Bernoulli trials",
      "Each group's success probability is constant",
      "The normal approximation is adequate"
    ),
    inputs = list(x = x, n = n),
    procedure = "two_wald_unpooled",
    se = se,
    crit = crit,
    margin = E,
    p1 = p1,
    p2 = p2
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    cat("conf.level = ", .ci_format_num(out$conf.level, digits), "\n", sep = "")
    cat("phat1 = ", .ci_format_num(p1, digits),
        "  phat2 = ", .ci_format_num(p2, digits),
        "  diff_hat = ", .ci_format_num(out$estimate, digits), "\n", sep = "")
    cat("SE = ", .ci_format_num(out$se, digits),
        "  z* = ", .ci_format_num(out$crit, digits),
        "  E = ", .ci_format_num(out$margin, digits), "\n", sep = "")
    cat("CI: ", .ci_format_ci(out$ci, digits), "\n", sep = "")
  }

  invisible(out)
}


#' @describeIn ci_functions Confidence interval for a variance or variance ratio.
#' @export
ci_var <- function(s, n, conf.level = 0.95, digits = 4, quiet = FALSE) {
  fun <- "ci_var()"

  .ci_validate_conf_level(conf.level, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)

  alpha <- 1 - conf.level

  if (!is.numeric(s) || length(s) == 0L || any(!is.finite(s))) {
    stop(fun, ": s must be a finite numeric vector.")
  }
  if (!is.numeric(n) || length(n) == 0L) {
    stop(fun, ": n must be numeric.")
  }

  k <- length(s)

  if (!k %in% c(1L, 2L)) {
    stop(fun, ": s must have length 1 or 2.")
  }
  if (length(n) != k) {
    stop(fun, ": n must have the same length as s.")
  }
  if (any(s < 0)) {
    stop(fun, ": s must be >= 0.")
  }
  if (any(!.ci_is_whole_number(n)) || any(n <= 1)) {
    stop(fun, ": n must contain integer sample sizes > 1.")
  }

  n <- as.integer(round(n))

  if (k == 1L) {
    df <- n[1L] - 1L
    s2 <- s[1L]^2
    q <- stats::qchisq(c(1 - alpha / 2, alpha / 2), df = df)
    ci_s2 <- stats::setNames(df * s2 / q, c("lower", "upper"))
    ci_s <- sqrt(ci_s2)

    out <- .new_ci_result(
      class_name = "ci_var_result",
      method = "One-sample CI for variance and SD (chi-square; normal population)",
      parameter = "sigma^2",
      conf.level = conf.level,
      alpha = alpha,
      side = "two.sided",
      estimate = s2,
      ci = ci_s2,
      assumptions = c(
        "Observations are independent",
        "The population is normal"
      ),
      inputs = list(s = s[1L], n = n[1L]),
      procedure = "one_var_chisq",
      df = df,
      df1 = NA_real_,
      df2 = NA_real_,
      estimate_sd = s[1L],
      ci_sd = ci_s
    )

    if (!quiet) {
      cat("\n", out$method, "\n", sep = "")
      cat("conf.level = ", .ci_format_num(out$conf.level, digits),
          "   df = ", .ci_format_num(out$df, digits), "\n", sep = "")
      cat("s^2 = ", .ci_format_num(out$estimate, digits),
          "  s = ", .ci_format_num(out$estimate_sd, digits), "\n", sep = "")
      cat("CI for sigma^2: ", .ci_format_ci(out$ci, digits), "\n", sep = "")
      cat("CI for sigma:   ", .ci_format_ci(out$ci_sd, digits), "\n", sep = "")
    }

    return(invisible(out))
  }

  if (s[1L] == 0 && s[2L] == 0) {
    stop(fun, ": the ratio sigma1^2 / sigma2^2 is undefined when both sample SDs are zero.")
  }
  if (s[2L] == 0) {
    stop(fun, ": the ratio sigma1^2 / sigma2^2 is undefined/infinite when s[2] = 0.")
  }

  df1 <- n[1L] - 1L
  df2 <- n[2L] - 1L
  ratio_hat <- (s[1L]^2) / (s[2L]^2)
  q <- stats::qf(c(1 - alpha / 2, alpha / 2), df1 = df1, df2 = df2)
  ci_ratio <- stats::setNames(ratio_hat / q, c("lower", "upper"))

  out <- .new_ci_result(
    class_name = "ci_var_result",
    method = "Two-sample CI for ratio of variances (F; normal populations)",
    parameter = "sigma1^2 / sigma2^2",
    conf.level = conf.level,
    alpha = alpha,
    side = "two.sided",
    estimate = ratio_hat,
    ci = ci_ratio,
    assumptions = c(
      "The two samples are independent",
      "Both populations are normal"
    ),
    inputs = list(s = s, n = n),
    procedure = "two_var_ratio_f",
    df = NA_real_,
    df1 = df1,
    df2 = df2,
    estimate_sd = NA_real_,
    ci_sd = c(lower = NA_real_, upper = NA_real_)
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    cat("conf.level = ", .ci_format_num(out$conf.level, digits),
        "   df1 = ", .ci_format_num(out$df1, digits),
        "  df2 = ", .ci_format_num(out$df2, digits), "\n", sep = "")
    cat("ratio_hat = ", .ci_format_num(out$estimate, digits), "\n", sep = "")
    cat("CI for sigma1^2 / sigma2^2: ", .ci_format_ci(out$ci, digits), "\n", sep = "")
  }

  invisible(out)
}


#' @describeIn ci_functions Confidence interval for an exponential rate parameter.
#' @export
ci_lambda_exp <- function(Sum, n, conf.level = 0.95, digits = 4, quiet = FALSE) {
  fun <- "ci_lambda_exp()"

  .ci_validate_conf_level(conf.level, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)

  if (!is.numeric(Sum) || length(Sum) != 1L || !is.finite(Sum) || Sum <= 0) {
    stop(fun, ": Sum must be a single positive finite number.")
  }
  if (!is.numeric(n) ||
      length(n) != 1L ||
      !is.finite(n) ||
      !.ci_is_whole_number(n) ||
      n < 1) {
    stop(fun, ": n must be a single integer >= 1.")
  }

  n <- as.integer(round(n))
  alpha <- 1 - conf.level
  df <- 2L * n
  q <- stats::qchisq(c(alpha / 2, 1 - alpha / 2), df = df)
  ci <- stats::setNames(q / (2 * Sum), c("lower", "upper"))
  lambda_hat <- n / Sum

  out <- .new_ci_result(
    class_name = "ci_lambda_exp_result",
    method = "CI for exponential rate lambda (chi-square; summary stats)",
    parameter = "lambda",
    conf.level = conf.level,
    alpha = alpha,
    side = "two.sided",
    estimate = lambda_hat,
    ci = ci,
    assumptions = c(
      "Observations are independent",
      "Observations are identically distributed",
      "Each observation follows an exponential distribution with rate lambda"
    ),
    inputs = list(Sum = Sum, n = n),
    procedure = "exp_rate_chisq",
    df = df,
    chi_lo = q[1L],
    chi_hi = q[2L]
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    cat("conf.level = ", .ci_format_num(out$conf.level, digits),
        "   df = ", .ci_format_num(out$df, digits), "\n", sep = "")
    cat("Sum = ", .ci_format_num(Sum, digits),
        "  n = ", n,
        "  lambda_hat = ", .ci_format_num(out$estimate, digits), "\n", sep = "")
    cat("chi-square quantiles: lower = ", .ci_format_num(out$chi_lo, digits),
        "  upper = ", .ci_format_num(out$chi_hi, digits), "\n", sep = "")
    cat("CI: ", .ci_format_ci(out$ci, digits), "\n", sep = "")
  }

  invisible(out)
}
