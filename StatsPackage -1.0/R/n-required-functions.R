#' Sample-size functions for target power
#'
#' Determines the smallest sample size that achieves a requested target power
#' for common hypothesis tests.
#'
#' These functions return classed result objects with unrounded stored values
#' and optional printed summaries controlled by `digits` and `quiet`.
#'
#' @param power_at_n Function returning power for a proposed integer sample size.
#' @param target_power Target power in (0, 1).
#' @param n_min Minimum integer sample size considered by the search.
#' @param n_max Maximum integer sample size considered by the search.
#' @param digits Integer number of decimal places used only for printed output.
#' @param quiet Logical; if `TRUE`, suppress printed output.
#' @param mu_a True mean value or vector of true means under the alternative.
#' @param mu0 Null-hypothesis mean value or mean-difference value.
#' @param sigma Known population standard deviation input.
#' @param alpha Significance level in (0, 1).
#' @param beta_target Target type II error in (0, 1).
#' @param alternative Alternative hypothesis direction: `"two.sided"`, `"less"`, or `"greater"`.
#' @param n_ratio Allocation ratio `n2 / n1` for two-sample designs.
#' @param sigma_true True population standard deviation input used in t-test planning.
#' @param paired Logical; for `n_required_t_mu()`, whether the supplied summaries are for paired differences.
#' @param method Method for the two-sample sigma-unknown t-test planning calculation.
#' @param p_a True proportion value or vector of true proportions under the alternative.
#' @param p0 Null-hypothesis proportion value or difference in proportions.
#' @param pooled Logical; for two-sample proportion planning, whether to use pooled standard errors.
#' @param continuity Logical; whether to apply the continuity correction where supported.
#' @param sigma_a True standard deviation value or vector under the alternative.
#' @param sigma0 Null-hypothesis standard deviation for the chi-square variance test.
#' @param ratio0 Null-hypothesis variance ratio for the F test.
#' @name n_required_functions
NULL

# Sample-size planning results ---------------------------------------------

.new_n_required_result <- function(class_name,
                                   method,
                                   target_power,
                                   achieved_power,
                                   target_beta,
                                   achieved_beta,
                                   assumptions = character(),
                                   inputs = list(),
                                   ...) {
  out <- c(
    list(
      method = method,
      target_power = target_power,
      achieved_power = achieved_power,
      target_beta = target_beta,
      achieved_beta = achieved_beta,
      assumptions = assumptions,
      inputs = inputs
    ),
    list(...)
  )

  class(out) <- c(class_name, "sample_size_result", "list")
  out
}

.nreq_validate_open_probability <- function(x, arg, fun) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x) ||
      x <= 0 ||
      x >= 1) {
    stop(fun, ": ", arg, " must be a single number between 0 and 1.")
  }

  x
}

.nreq_validate_integer_scalar <- function(x, arg, fun, min_value = 1L) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x) ||
      !.ci_is_whole_number(x) ||
      x < min_value ||
      x > .Machine$integer.max) {
    stop(fun, ": ", arg, " must be a single integer >= ", min_value, ".")
  }

  as.integer(round(x))
}

.nreq_validate_function <- function(x, arg, fun) {
  if (!is.function(x)) {
    stop(fun, ": ", arg, " must be a function.")
  }

  x
}

.nreq_silence_warnings <- function(expr) {
  expr_sub <- substitute(expr)

  withCallingHandlers(
    eval.parent(expr_sub),
    warning = function(w) {
      tryInvokeRestart("muffleWarning")
    }
  )
}

.nreq_stop_if_null_equivalent <- function(true_value,
                                          null_value,
                                          alternative,
                                          alpha,
                                          target_power,
                                          fun) {
  if (alternative == "two.sided" &&
      isTRUE(all.equal(true_value, null_value)) &&
      target_power > alpha) {
    stop(
      fun,
      ": under the specified alternative, the true value equals the null value, so power cannot exceed alpha."
    )
  }
}

.nreq_eval_power <- function(power_at_n,
                             n,
                             fun,
                             suppress_warnings = FALSE) {
  value <- if (isTRUE(suppress_warnings)) {
    withCallingHandlers(
      power_at_n(n),
      warning = function(w) {
        tryInvokeRestart("muffleWarning")
      }
    )
  } else {
    power_at_n(n)
  }

  if (!is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      value < 0 ||
      value > 1) {
    stop(fun, ": power_at_n(", n, ") must return a single finite value in [0, 1].")
  }

  as.numeric(value)
}

.nreq_find_min_n <- function(power_at_n,
                             target_power,
                             n_min,
                             n_max,
                             fun = ".nreq_find_min_n()",
                             suppress_warnings = FALSE) {
  power_at_n <- .nreq_validate_function(power_at_n, "power_at_n", fun)
  target_power <- .nreq_validate_open_probability(target_power, "target_power", fun)
  n_min <- .nreq_validate_integer_scalar(n_min, "n_min", fun, min_value = 1L)
  n_max <- .nreq_validate_integer_scalar(n_max, "n_max", fun, min_value = 1L)

  if (n_min > n_max) {
    stop(fun, ": n_min must be <= n_max.")
  }

  cache <- new.env(parent = emptyenv())

  eval_power <- function(n) {
    key <- as.character(n)

    if (exists(key, envir = cache, inherits = FALSE)) {
      return(get(key, envir = cache, inherits = FALSE))
    }

    value <- .nreq_eval_power(
      power_at_n = power_at_n,
      n = n,
      fun = fun,
      suppress_warnings = suppress_warnings
    )

    assign(key, value, envir = cache)
    value
  }

  lo <- n_min
  p_lo <- eval_power(lo)

  if (p_lo >= target_power) {
    return(list(
      n = lo,
      achieved_power = p_lo,
      achieved_beta = 1 - p_lo,
      n_evaluations = length(ls(cache, all.names = TRUE)),
      n_is_minimal = TRUE,
      lower_bound = n_min,
      upper_bound = n_max,
      bracket_upper = lo
    ))
  }

  hi <- lo
  p_hi <- p_lo

  while (hi < n_max && p_hi < target_power) {
    next_hi <- as.integer(min(as.double(n_max), 2 * as.double(hi)))
    if (next_hi <= hi) {
      next_hi <- n_max
    }

    hi <- next_hi
    p_hi <- eval_power(hi)
  }

  if (p_hi < target_power) {
    stop(
      fun,
      ": target power was not reached within n_max. ",
      "Check the effect size and direction, whether the alternative differs from the null, or increase n_max."
    )
  }

  bracket_upper <- hi

  while ((lo + 1L) < hi) {
    mid <- as.integer((lo + hi) %/% 2L)
    p_mid <- eval_power(mid)

    if (p_mid >= target_power) {
      hi <- mid
      p_hi <- p_mid
    } else {
      lo <- mid
    }
  }

  n_is_minimal <- TRUE
  if (hi > n_min) {
    p_prev <- eval_power(hi - 1L)
    n_is_minimal <- (p_prev < target_power)
  }

  list(
    n = hi,
    achieved_power = p_hi,
    achieved_beta = 1 - p_hi,
    n_evaluations = length(ls(cache, all.names = TRUE)),
    n_is_minimal = n_is_minimal,
    lower_bound = n_min,
    upper_bound = n_max,
    bracket_upper = bracket_upper
  )
}


# Required n from a user-supplied power function ---------------------------

#' @describeIn n_required_functions Generic integer sample-size solver based on a power function.
#' @export
n_required_from_power <- function(power_at_n,
                                  target_power,
                                  n_min = 1L,
                                  n_max = 1e6L,
                                  digits = 4,
                                  quiet = FALSE) {
  fun <- "n_required_from_power()"

  power_at_n <- .nreq_validate_function(power_at_n, "power_at_n", fun)
  target_power <- .nreq_validate_open_probability(target_power, "target_power", fun)
  n_min <- .nreq_validate_integer_scalar(n_min, "n_min", fun, min_value = 1L)
  n_max <- .nreq_validate_integer_scalar(n_max, "n_max", fun, min_value = 1L)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)

  if (n_min > n_max) {
    stop(fun, ": n_min must be <= n_max.")
  }

  sol <- .nreq_find_min_n(
    power_at_n = power_at_n,
    target_power = target_power,
    n_min = n_min,
    n_max = n_max,
    fun = fun,
    suppress_warnings = FALSE
  )

  out <- .new_n_required_result(
    class_name = "n_required_from_power_result",
    method = "Required integer n from a user-supplied power function",
    target_power = target_power,
    achieved_power = sol$achieved_power,
    target_beta = 1 - target_power,
    achieved_beta = sol$achieved_beta,
    assumptions = c(
      "power_at_n(n) returns the power for integer n",
      "Power is nondecreasing in n over the search range"
    ),
    inputs = list(
      target_power = target_power,
      n_min = n_min,
      n_max = n_max
    ),
    solver = "integer bracketing + binary search",
    n = sol$n,
    n_is_minimal = sol$n_is_minimal,
    n_search_min = sol$lower_bound,
    n_search_max = sol$upper_bound,
    n_evaluations = sol$n_evaluations,
    bracket_upper = sol$bracket_upper
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    cat(
      "Target power = ", .ci_format_num(out$target_power, digits),
      "  Target beta = ", .ci_format_num(out$target_beta, digits),
      "\n",
      sep = ""
    )
    cat(
      "Search range: n in [", out$n_search_min, ", ", out$n_search_max, "]\n",
      sep = ""
    )
    cat("Required n = ", out$n, "\n", sep = "")
    cat(
      "Achieved power = ", .ci_format_num(out$achieved_power, digits),
      "  Achieved beta = ", .ci_format_num(out$achieved_beta, digits),
      "\n",
      sep = ""
    )
  }

  invisible(out)
}


# Required n for z tests ---------------------------------------------------

#' @describeIn n_required_functions Required sample size for z tests of means with known sigma.
#' @export
n_required_z_mu <- function(mu_a,
                            mu0,
                            sigma,
                            alpha = 0.05,
                            beta_target = 0.10,
                            alternative = c("two.sided", "less", "greater"),
                            n_min = 1L,
                            n_max = 1e6L,
                            n_ratio = 1,
                            digits = 4,
                            quiet = FALSE) {
  fun <- "n_required_z_mu()"
  n_ratio_supplied <- !missing(n_ratio)

  alpha <- .ht_validate_alpha(alpha, fun)
  beta_target <- .nreq_validate_open_probability(beta_target, "beta_target", fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  alternative <- match.arg(alternative)
  mu0 <- .ht_validate_finite_scalar(mu0, "mu0", fun)
  n_ratio <- .ht_validate_positive_scalar(n_ratio, "n_ratio", fun)

  if (!is.numeric(mu_a) || length(mu_a) == 0L || any(!is.finite(mu_a))) {
    stop(fun, ": mu_a must be a finite numeric vector.")
  }

  k <- length(mu_a)
  if (!k %in% c(1L, 2L)) {
    stop(fun, ": mu_a must have length 1 (one-sample) or 2 (two-sample).")
  }

  sigma <- .power_validate_positive_numeric_vector(sigma, "sigma", fun)
  if (k == 2L && length(sigma) == 1L) {
    sigma <- rep(sigma, 2L)
  }
  if (length(sigma) != k) {
    stop(fun, ": sigma must have length 1 (recycled for two-sample use) or the same length as mu_a.")
  }

  n_min <- .nreq_validate_integer_scalar(n_min, "n_min", fun, min_value = 1L)
  n_max <- .nreq_validate_integer_scalar(n_max, "n_max", fun, min_value = 1L)
  if (n_min > n_max) {
    stop(fun, ": n_min must be <= n_max.")
  }

  target_power <- 1 - beta_target

  if (k == 1L) {
    mu_a_scalar <- mu_a[1L]
    sigma_scalar <- sigma[1L]

    if (n_ratio_supplied) {
      .ht_warn_ignored_arg(fun, "n_ratio", "ignored for the one-sample z design.")
    }

    if (alternative == "less" && mu_a_scalar >= mu0) {
      stop(fun, ": for alternative = 'less', mu_a must be < mu0.")
    }
    if (alternative == "greater" && mu_a_scalar <= mu0) {
      stop(fun, ": for alternative = 'greater', mu_a must be > mu0.")
    }

    .nreq_stop_if_null_equivalent(
      true_value = mu_a_scalar,
      null_value = mu0,
      alternative = alternative,
      alpha = alpha,
      target_power = target_power,
      fun = fun
    )

    method_label <- "Required n for one-sample z test for mean (sigma known)"

    if (alternative %in% c("less", "greater")) {
      delta_abs <- abs(mu_a_scalar - mu0)
      z_alpha <- stats::qnorm(1 - alpha)
      z_power <- stats::qnorm(target_power)
      n_star <- (sigma_scalar * (z_alpha + z_power) / delta_abs)^2
      if (!is.finite(n_star) || n_star > n_max) {
        stop(
          fun,
          ": target power was not reached within n_max. ",
          "Check the effect size and direction, whether the alternative differs from the null, or increase n_max."
        )
      }

      n_req <- max(n_min, as.integer(ceiling(n_star)))

      if (n_req > n_max) {
        stop(
          fun,
          ": target power was not reached within n_max. ",
          "Check the effect size and direction, whether the alternative differs from the null, or increase n_max."
        )
      }

      chk <- power_z_mu(
        mu_a = mu_a_scalar,
        mu0 = mu0,
        sigma = sigma_scalar,
        n = n_req,
        alpha = alpha,
        alternative = alternative,
        quiet = TRUE
      )

      while (chk$power < target_power && n_req < n_max) {
        n_req <- n_req + 1L
        chk <- power_z_mu(
          mu_a = mu_a_scalar,
          mu0 = mu0,
          sigma = sigma_scalar,
          n = n_req,
          alpha = alpha,
          alternative = alternative,
          quiet = TRUE
        )
      }

      if (chk$power < target_power) {
        stop(
          fun,
          ": target power was not reached within n_max. ",
          "Check the effect size and direction, whether the alternative differs from the null, or increase n_max."
        )
      }

      while (n_req > n_min) {
        prev_req <- n_req - 1L
        prev_chk <- power_z_mu(
          mu_a = mu_a_scalar,
          mu0 = mu0,
          sigma = sigma_scalar,
          n = prev_req,
          alpha = alpha,
          alternative = alternative,
          quiet = TRUE
        )

        if (prev_chk$power >= target_power) {
          n_req <- prev_req
          chk <- prev_chk
        } else {
          break
        }
      }

      out <- .new_n_required_result(
        class_name = "n_required_z_mu_result",
        method = method_label,
        target_power = target_power,
        achieved_power = chk$power,
        target_beta = beta_target,
        achieved_beta = chk$beta,
        assumptions = chk$assumptions,
        inputs = list(
          mu_a = mu_a_scalar,
          mu0 = mu0,
          sigma = sigma_scalar,
          alpha = alpha,
          beta_target = beta_target,
          alternative = alternative,
          n_min = n_min,
          n_max = n_max
        ),
        solver = "closed form + integer rounding + power check",
        scenario = "one_sample",
        parameter = "mu",
        alternative = alternative,
        alpha = alpha,
        null = mu0,
        true_value = mu_a_scalar,
        mu_a = mu_a_scalar,
        sigma = sigma_scalar,
        n = n_req,
        n_star = n_star,
        n_is_minimal = TRUE,
        n_search_min = n_min,
        n_search_max = n_max,
        se = chk$se,
        ncp = chk$ncp,
        crit = chk$crit,
        crit_label = chk$crit_label,
        zcrit = chk$zcrit,
        estimate_crit = chk$estimate_crit,
        reject_region = chk$reject_region,
        region = chk$region
      )

      if (!quiet) {
        cat("\n", out$method, "\n", sep = "")
        cat(
          "Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ",
          .ci_format_num(out$null, digits), "\n",
          sep = ""
        )
        cat(
          "alpha = ", .ci_format_num(out$alpha, digits),
          "   target power = ", .ci_format_num(out$target_power, digits),
          "   target beta = ", .ci_format_num(out$target_beta, digits),
          "\n",
          sep = ""
        )
        cat(
          "mu_a = ", .ci_format_num(out$mu_a, digits),
          "  mu0 = ", .ci_format_num(out$null, digits),
          "  sigma = ", .ci_format_num(out$sigma, digits),
          "\n",
          sep = ""
        )
        cat(
          "n* = ", .ci_format_num(out$n_star, digits),
          "  ->  n = ", out$n,
          "\n",
          sep = ""
        )
        cat(
          "Check: power = ", .ci_format_num(out$achieved_power, digits),
          "  beta = ", .ci_format_num(out$achieved_beta, digits),
          "\n",
          sep = ""
        )
      }

      return(invisible(out))
    }

    power_at_n <- function(nn) {
      power_z_mu(
        mu_a = mu_a_scalar,
        mu0 = mu0,
        sigma = sigma_scalar,
        n = nn,
        alpha = alpha,
        alternative = alternative,
        quiet = TRUE
      )$power
    }

    sol <- .nreq_find_min_n(
      power_at_n = power_at_n,
      target_power = target_power,
      n_min = n_min,
      n_max = n_max,
      fun = fun,
      suppress_warnings = FALSE
    )

    chk <- power_z_mu(
      mu_a = mu_a_scalar,
      mu0 = mu0,
      sigma = sigma_scalar,
      n = sol$n,
      alpha = alpha,
      alternative = alternative,
      quiet = TRUE
    )

    out <- .new_n_required_result(
      class_name = "n_required_z_mu_result",
      method = method_label,
      target_power = target_power,
      achieved_power = chk$power,
      target_beta = beta_target,
      achieved_beta = chk$beta,
      assumptions = chk$assumptions,
      inputs = list(
        mu_a = mu_a_scalar,
        mu0 = mu0,
        sigma = sigma_scalar,
        alpha = alpha,
        beta_target = beta_target,
        alternative = alternative,
        n_min = n_min,
        n_max = n_max
      ),
      solver = "integer bracketing + binary search",
      scenario = "one_sample",
      parameter = "mu",
      alternative = alternative,
      alpha = alpha,
      null = mu0,
      true_value = mu_a_scalar,
      mu_a = mu_a_scalar,
      sigma = sigma_scalar,
      n = sol$n,
      n_is_minimal = sol$n_is_minimal,
      n_search_min = sol$lower_bound,
      n_search_max = sol$upper_bound,
      n_evaluations = sol$n_evaluations,
      bracket_upper = sol$bracket_upper,
      se = chk$se,
      ncp = chk$ncp,
      crit = chk$crit,
      crit_label = chk$crit_label,
      zcrit = chk$zcrit,
      estimate_crit = chk$estimate_crit,
      reject_region = chk$reject_region,
      region = chk$region
    )

    if (!quiet) {
      cat("\n", out$method, "\n", sep = "")
      cat(
        "Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ",
        .ci_format_num(out$null, digits), "\n",
        sep = ""
      )
      cat(
        "alpha = ", .ci_format_num(out$alpha, digits),
        "   target power = ", .ci_format_num(out$target_power, digits),
        "   target beta = ", .ci_format_num(out$target_beta, digits),
        "\n",
        sep = ""
      )
      cat(
        "mu_a = ", .ci_format_num(out$mu_a, digits),
        "  mu0 = ", .ci_format_num(out$null, digits),
        "  sigma = ", .ci_format_num(out$sigma, digits),
        "\n",
        sep = ""
      )
      cat("Required n = ", out$n, "\n", sep = "")
      cat(
        "Check: power = ", .ci_format_num(out$achieved_power, digits),
        "  beta = ", .ci_format_num(out$achieved_beta, digits),
        "\n",
        sep = ""
      )
    }

    return(invisible(out))
  }

  delta_a <- mu_a[1L] - mu_a[2L]
  sigma1 <- sigma[1L]
  sigma2 <- sigma[2L]

  if (alternative == "less" && delta_a >= mu0) {
    stop(fun, ": for alternative = 'less', mu1_a - mu2_a must be < mu0.")
  }
  if (alternative == "greater" && delta_a <= mu0) {
    stop(fun, ": for alternative = 'greater', mu1_a - mu2_a must be > mu0.")
  }

  .nreq_stop_if_null_equivalent(
    true_value = delta_a,
    null_value = mu0,
    alternative = alternative,
    alpha = alpha,
    target_power = target_power,
    fun = fun
  )

  method_label <- "Required n for two-sample z test for difference in means (sigmas known)"

  if (alternative %in% c("less", "greater")) {
    delta_abs <- abs(delta_a - mu0)
    z_alpha <- stats::qnorm(1 - alpha)
    z_power <- stats::qnorm(target_power)
    n1_star <- (sigma1^2 + sigma2^2 / n_ratio) * (z_alpha + z_power)^2 / delta_abs^2
    if (!is.finite(n1_star) || n1_star > n_max) {
      stop(
        fun,
        ": target power was not reached within n_max. ",
        "Check the effect size and direction, whether the alternative differs from the null, or increase n_max."
      )
    }

    n1_req <- max(n_min, as.integer(ceiling(n1_star)))

    if (n1_req > n_max) {
      stop(
        fun,
        ": target power was not reached within n_max. ",
        "Check the effect size and direction, whether the alternative differs from the null, or increase n_max."
      )
    }

    n2_req <- as.integer(max(1L, ceiling(n_ratio * n1_req)))
    chk <- power_z_mu(
      mu_a = mu_a,
      mu0 = mu0,
      sigma = c(sigma1, sigma2),
      n = c(n1_req, n2_req),
      alpha = alpha,
      alternative = alternative,
      quiet = TRUE
    )

    while (chk$power < target_power && n1_req < n_max) {
      n1_req <- n1_req + 1L
      n2_req <- as.integer(max(1L, ceiling(n_ratio * n1_req)))
      chk <- power_z_mu(
        mu_a = mu_a,
        mu0 = mu0,
        sigma = c(sigma1, sigma2),
        n = c(n1_req, n2_req),
        alpha = alpha,
        alternative = alternative,
        quiet = TRUE
      )
    }

    if (chk$power < target_power) {
      stop(
        fun,
        ": target power was not reached within n_max. ",
        "Check the effect size and direction, whether the alternative differs from the null, or increase n_max."
      )
    }

    while (n1_req > n_min) {
      prev_n1 <- n1_req - 1L
      prev_n2 <- as.integer(max(1L, ceiling(n_ratio * prev_n1)))
      prev_chk <- power_z_mu(
        mu_a = mu_a,
        mu0 = mu0,
        sigma = c(sigma1, sigma2),
        n = c(prev_n1, prev_n2),
        alpha = alpha,
        alternative = alternative,
        quiet = TRUE
      )

      if (prev_chk$power >= target_power) {
        n1_req <- prev_n1
        n2_req <- prev_n2
        chk <- prev_chk
      } else {
        break
      }
    }

    out <- .new_n_required_result(
      class_name = "n_required_z_mu_result",
      method = method_label,
      target_power = target_power,
      achieved_power = chk$power,
      target_beta = beta_target,
      achieved_beta = chk$beta,
      assumptions = chk$assumptions,
      inputs = list(
        mu_a = mu_a,
        mu0 = mu0,
        sigma = c(sigma1, sigma2),
        alpha = alpha,
        beta_target = beta_target,
        alternative = alternative,
        n_ratio = n_ratio,
        n_min = n_min,
        n_max = n_max
      ),
      solver = "closed form + integer rounding + power check",
      scenario = "two_sample",
      parameter = "mu1 - mu2",
      alternative = alternative,
      alpha = alpha,
      null = mu0,
      true_value = delta_a,
      mu_a = mu_a,
      mu1_a = mu_a[1L],
      mu2_a = mu_a[2L],
      sigma = c(sigma1, sigma2),
      sigma1 = sigma1,
      sigma2 = sigma2,
      n = c(n1_req, n2_req),
      n1 = n1_req,
      n2 = n2_req,
      n_ratio = n_ratio,
      n1_star = n1_star,
      delta_a = delta_a,
      n_is_minimal = TRUE,
      n_search_min = n_min,
      n_search_max = n_max,
      se = chk$se,
      ncp = chk$ncp,
      crit = chk$crit,
      crit_label = chk$crit_label,
      zcrit = chk$zcrit,
      estimate_crit = chk$estimate_crit,
      reject_region = chk$reject_region,
      region = chk$region
    )

    if (!quiet) {
      cat("\n", out$method, "\n", sep = "")
      cat(
        "Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ",
        .ci_format_num(out$null, digits), "\n",
        sep = ""
      )
      cat(
        "alpha = ", .ci_format_num(out$alpha, digits),
        "   target power = ", .ci_format_num(out$target_power, digits),
        "   target beta = ", .ci_format_num(out$target_beta, digits),
        "\n",
        sep = ""
      )
      cat(
        "mu1_a = ", .ci_format_num(out$mu1_a, digits),
        "  mu2_a = ", .ci_format_num(out$mu2_a, digits),
        "  delta0 = ", .ci_format_num(out$null, digits),
        "\n",
        sep = ""
      )
      cat(
        "sigma1 = ", .ci_format_num(out$sigma1, digits),
        "  sigma2 = ", .ci_format_num(out$sigma2, digits),
        "  n_ratio = ", .ci_format_num(out$n_ratio, digits),
        "\n",
        sep = ""
      )
      cat(
        "n1* = ", .ci_format_num(out$n1_star, digits),
        "  ->  n1 = ", out$n1,
        "  n2 = ", out$n2,
        "\n",
        sep = ""
      )
      cat(
        "Check: power = ", .ci_format_num(out$achieved_power, digits),
        "  beta = ", .ci_format_num(out$achieved_beta, digits),
        "\n",
        sep = ""
      )
    }

    return(invisible(out))
  }

  power_at_n1 <- function(nn1) {
    nn1 <- as.integer(nn1)
    nn2 <- as.integer(max(1L, ceiling(n_ratio * nn1)))

    power_z_mu(
      mu_a = mu_a,
      mu0 = mu0,
      sigma = c(sigma1, sigma2),
      n = c(nn1, nn2),
      alpha = alpha,
      alternative = alternative,
      quiet = TRUE
    )$power
  }

  sol <- .nreq_find_min_n(
    power_at_n = power_at_n1,
    target_power = target_power,
    n_min = n_min,
    n_max = n_max,
    fun = fun,
    suppress_warnings = FALSE
  )

  n1 <- sol$n
  n2 <- as.integer(max(1L, ceiling(n_ratio * n1)))
  chk <- power_z_mu(
    mu_a = mu_a,
    mu0 = mu0,
    sigma = c(sigma1, sigma2),
    n = c(n1, n2),
    alpha = alpha,
    alternative = alternative,
    quiet = TRUE
  )

  out <- .new_n_required_result(
    class_name = "n_required_z_mu_result",
    method = method_label,
    target_power = target_power,
    achieved_power = chk$power,
    target_beta = beta_target,
    achieved_beta = chk$beta,
    assumptions = chk$assumptions,
    inputs = list(
      mu_a = mu_a,
      mu0 = mu0,
      sigma = c(sigma1, sigma2),
      alpha = alpha,
      beta_target = beta_target,
      alternative = alternative,
      n_ratio = n_ratio,
      n_min = n_min,
      n_max = n_max
    ),
    solver = "integer bracketing + binary search",
    scenario = "two_sample",
    parameter = "mu1 - mu2",
    alternative = alternative,
    alpha = alpha,
    null = mu0,
    true_value = delta_a,
    mu_a = mu_a,
    mu1_a = mu_a[1L],
    mu2_a = mu_a[2L],
    sigma = c(sigma1, sigma2),
    sigma1 = sigma1,
    sigma2 = sigma2,
    n = c(n1, n2),
    n1 = n1,
    n2 = n2,
    n_ratio = n_ratio,
    delta_a = delta_a,
    n_is_minimal = sol$n_is_minimal,
    n_search_min = sol$lower_bound,
    n_search_max = sol$upper_bound,
    n_evaluations = sol$n_evaluations,
    bracket_upper = sol$bracket_upper,
    se = chk$se,
    ncp = chk$ncp,
    crit = chk$crit,
    crit_label = chk$crit_label,
    zcrit = chk$zcrit,
    estimate_crit = chk$estimate_crit,
    reject_region = chk$reject_region,
    region = chk$region
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    cat(
      "Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ",
      .ci_format_num(out$null, digits), "\n",
      sep = ""
    )
    cat(
      "alpha = ", .ci_format_num(out$alpha, digits),
      "   target power = ", .ci_format_num(out$target_power, digits),
      "   target beta = ", .ci_format_num(out$target_beta, digits),
      "\n",
      sep = ""
    )
    cat(
      "mu1_a = ", .ci_format_num(out$mu1_a, digits),
      "  mu2_a = ", .ci_format_num(out$mu2_a, digits),
      "  delta0 = ", .ci_format_num(out$null, digits),
      "\n",
      sep = ""
    )
    cat(
      "sigma1 = ", .ci_format_num(out$sigma1, digits),
      "  sigma2 = ", .ci_format_num(out$sigma2, digits),
      "  n_ratio = ", .ci_format_num(out$n_ratio, digits),
      "\n",
      sep = ""
    )
    cat("Required n1 = ", out$n1, "  n2 = ", out$n2, "\n", sep = "")
    cat(
      "Check: power = ", .ci_format_num(out$achieved_power, digits),
      "  beta = ", .ci_format_num(out$achieved_beta, digits),
      "\n",
      sep = ""
    )
  }

  invisible(out)
}


# Required n for t tests ---------------------------------------------------

#' @describeIn n_required_functions Required sample size for t tests of means.
#' @export
n_required_t_mu <- function(mu_a,
                            mu0,
                            sigma_true,
                            alpha = 0.05,
                            beta_target = 0.10,
                            alternative = c("two.sided", "less", "greater"),
                            n_min = 2L,
                            n_max = 1e6L,
                            paired = FALSE,
                            method = c("welch", "pooled"),
                            n_ratio = 1,
                            digits = 4,
                            quiet = FALSE) {
  fun <- "n_required_t_mu()"
  method_supplied <- !missing(method)
  n_ratio_supplied <- !missing(n_ratio)

  alpha <- .ht_validate_alpha(alpha, fun)
  beta_target <- .nreq_validate_open_probability(beta_target, "beta_target", fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  paired <- .ci_validate_flag(paired, "paired", fun)
  alternative <- match.arg(alternative)
  mu0 <- .ht_validate_finite_scalar(mu0, "mu0", fun)
  n_ratio <- .ht_validate_positive_scalar(n_ratio, "n_ratio", fun)

  if (!is.numeric(mu_a) || length(mu_a) == 0L || any(!is.finite(mu_a))) {
    stop(fun, ": mu_a must be a finite numeric vector.")
  }

  k <- length(mu_a)
  if (!k %in% c(1L, 2L)) {
    stop(fun, ": mu_a must have length 1 or 2.")
  }

  n_min <- .nreq_validate_integer_scalar(n_min, "n_min", fun, min_value = 2L)
  n_max <- .nreq_validate_integer_scalar(n_max, "n_max", fun, min_value = 2L)
  if (n_min > n_max) {
    stop(fun, ": n_min must be <= n_max.")
  }

  target_power <- 1 - beta_target

  if (paired && k != 1L) {
    stop(fun, ": paired = TRUE requires scalar mu_a.")
  }

  if (k == 1L) {
    mu_a_scalar <- mu_a[1L]
    sigma_true <- .ht_validate_positive_scalar(sigma_true, "sigma_true", fun)

    if (alternative == "less" && mu_a_scalar >= mu0) {
      stop(fun, ": for alternative = 'less', mu_a must be < mu0.")
    }
    if (alternative == "greater" && mu_a_scalar <= mu0) {
      stop(fun, ": for alternative = 'greater', mu_a must be > mu0.")
    }

    .nreq_stop_if_null_equivalent(
      true_value = mu_a_scalar,
      null_value = mu0,
      alternative = alternative,
      alpha = alpha,
      target_power = target_power,
      fun = fun
    )

    if (paired) {
      if (method_supplied) {
        .ht_warn_ignored_arg(fun, "method", "ignored for paired t designs.")
      }
      if (n_ratio_supplied) {
        .ht_warn_ignored_arg(fun, "n_ratio", "ignored for paired t designs.")
      }

      power_at_n <- function(nn) {
        power_t_mu(
          mu_a = mu_a_scalar,
          mu0 = mu0,
          sigma_true = sigma_true,
          n = nn,
          alpha = alpha,
          alternative = alternative,
          paired = TRUE,
          quiet = TRUE
        )$power
      }

      sol <- .nreq_find_min_n(
        power_at_n = power_at_n,
        target_power = target_power,
        n_min = n_min,
        n_max = n_max,
        fun = fun,
        suppress_warnings = FALSE
      )

      chk <- power_t_mu(
        mu_a = mu_a_scalar,
        mu0 = mu0,
        sigma_true = sigma_true,
        n = sol$n,
        alpha = alpha,
        alternative = alternative,
        paired = TRUE,
        quiet = TRUE
      )

      out <- .new_n_required_result(
        class_name = "n_required_t_mu_result",
        method = "Required n for paired t test (one-sample on differences)",
        target_power = target_power,
        achieved_power = chk$power,
        target_beta = beta_target,
        achieved_beta = chk$beta,
        assumptions = chk$assumptions,
        inputs = list(
          mu_a = mu_a_scalar,
          mu0 = mu0,
          sigma_true = sigma_true,
          alpha = alpha,
          beta_target = beta_target,
          alternative = alternative,
          paired = TRUE,
          n_min = n_min,
          n_max = n_max
        ),
        solver = "integer bracketing + binary search",
        scenario = "paired",
        parameter = "mu_d",
        alternative = alternative,
        alpha = alpha,
        null = mu0,
        true_value = mu_a_scalar,
        sigma_true = sigma_true,
        n = sol$n,
        n_is_minimal = sol$n_is_minimal,
        n_search_min = sol$lower_bound,
        n_search_max = sol$upper_bound,
        n_evaluations = sol$n_evaluations,
        bracket_upper = sol$bracket_upper,
        df = chk$df,
        se = chk$se,
        ncp = chk$ncp,
        crit = chk$crit,
        crit_label = chk$crit_label,
        tcrit = chk$tcrit,
        reject_region = chk$reject_region,
        region = chk$region
      )

      if (!quiet) {
        cat("\n", out$method, "\n", sep = "")
        cat(
          "Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ",
          .ci_format_num(out$null, digits), "\n",
          sep = ""
        )
        cat(
          "alpha = ", .ci_format_num(out$alpha, digits),
          "   target power = ", .ci_format_num(out$target_power, digits),
          "   target beta = ", .ci_format_num(out$target_beta, digits),
          "\n",
          sep = ""
        )
        cat(
          "mu_d,a = ", .ci_format_num(out$true_value, digits),
          "  mu_d,0 = ", .ci_format_num(out$null, digits),
          "  sigma_true = ", .ci_format_num(out$sigma_true, digits),
          "\n",
          sep = ""
        )
        cat("Required n = ", out$n, "\n", sep = "")
        cat(
          "Check: power = ", .ci_format_num(out$achieved_power, digits),
          "  beta = ", .ci_format_num(out$achieved_beta, digits),
          "\n",
          sep = ""
        )
      }

      return(invisible(out))
    }

    if (method_supplied) {
      .ht_warn_ignored_arg(fun, "method", "ignored for one-sample t designs.")
    }
    if (n_ratio_supplied) {
      .ht_warn_ignored_arg(fun, "n_ratio", "ignored for one-sample t designs.")
    }

    power_at_n <- function(nn) {
      power_t_mu(
        mu_a = mu_a_scalar,
        mu0 = mu0,
        sigma_true = sigma_true,
        n = nn,
        alpha = alpha,
        alternative = alternative,
        quiet = TRUE
      )$power
    }

    sol <- .nreq_find_min_n(
      power_at_n = power_at_n,
      target_power = target_power,
      n_min = n_min,
      n_max = n_max,
      fun = fun,
      suppress_warnings = FALSE
    )

    chk <- power_t_mu(
      mu_a = mu_a_scalar,
      mu0 = mu0,
      sigma_true = sigma_true,
      n = sol$n,
      alpha = alpha,
      alternative = alternative,
      quiet = TRUE
    )

    out <- .new_n_required_result(
      class_name = "n_required_t_mu_result",
      method = "Required n for one-sample t test for mean",
      target_power = target_power,
      achieved_power = chk$power,
      target_beta = beta_target,
      achieved_beta = chk$beta,
      assumptions = chk$assumptions,
      inputs = list(
        mu_a = mu_a_scalar,
        mu0 = mu0,
        sigma_true = sigma_true,
        alpha = alpha,
        beta_target = beta_target,
        alternative = alternative,
        paired = FALSE,
        n_min = n_min,
        n_max = n_max
      ),
      solver = "integer bracketing + binary search",
      scenario = "one_sample",
      parameter = "mu",
      alternative = alternative,
      alpha = alpha,
      null = mu0,
      true_value = mu_a_scalar,
      sigma_true = sigma_true,
      n = sol$n,
      n_is_minimal = sol$n_is_minimal,
      n_search_min = sol$lower_bound,
      n_search_max = sol$upper_bound,
      n_evaluations = sol$n_evaluations,
      bracket_upper = sol$bracket_upper,
      df = chk$df,
      se = chk$se,
      ncp = chk$ncp,
      crit = chk$crit,
      crit_label = chk$crit_label,
      tcrit = chk$tcrit,
      reject_region = chk$reject_region,
      region = chk$region
    )

    if (!quiet) {
      cat("\n", out$method, "\n", sep = "")
      cat(
        "Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ",
        .ci_format_num(out$null, digits), "\n",
        sep = ""
      )
      cat(
        "alpha = ", .ci_format_num(out$alpha, digits),
        "   target power = ", .ci_format_num(out$target_power, digits),
        "   target beta = ", .ci_format_num(out$target_beta, digits),
        "\n",
        sep = ""
      )
      cat(
        "mu_a = ", .ci_format_num(out$true_value, digits),
        "  mu0 = ", .ci_format_num(out$null, digits),
        "  sigma_true = ", .ci_format_num(out$sigma_true, digits),
        "\n",
        sep = ""
      )
      cat("Required n = ", out$n, "\n", sep = "")
      cat(
        "Check: power = ", .ci_format_num(out$achieved_power, digits),
        "  beta = ", .ci_format_num(out$achieved_beta, digits),
        "\n",
        sep = ""
      )
    }

    return(invisible(out))
  }

  if (paired) {
    stop(fun, ": paired = TRUE is not available for two-sample designs; supply the paired-difference mean as scalar mu_a.")
  }

  delta_a <- mu_a[1L] - mu_a[2L]
  if (alternative == "less" && delta_a >= mu0) {
    stop(fun, ": for alternative = 'less', mu1_a - mu2_a must be < mu0.")
  }
  if (alternative == "greater" && delta_a <= mu0) {
    stop(fun, ": for alternative = 'greater', mu1_a - mu2_a must be > mu0.")
  }

  .nreq_stop_if_null_equivalent(
    true_value = delta_a,
    null_value = mu0,
    alternative = alternative,
    alpha = alpha,
    target_power = target_power,
    fun = fun
  )

  method_type <- match.arg(method)
  sigma_true_checked <- .power_validate_positive_numeric_vector(sigma_true, "sigma_true", fun)

  if (method_type == "pooled") {
    if (length(sigma_true_checked) == 1L) {
      sigma_true_out <- sigma_true_checked[1L]
    } else if (length(sigma_true_checked) == 2L) {
      if (abs(sigma_true_checked[1L] - sigma_true_checked[2L]) > 1e-8) {
        stop(fun, ": method = 'pooled' assumes equal variances; provide scalar sigma_true or an equal pair.")
      }
      sigma_true_out <- sigma_true_checked[1L]
    } else {
      stop(fun, ": method = 'pooled' requires scalar sigma_true or a length-2 equal pair.")
    }
  } else {
    if (length(sigma_true_checked) == 1L) {
      sigma_true_out <- rep(sigma_true_checked[1L], 2L)
    } else if (length(sigma_true_checked) == 2L) {
      sigma_true_out <- sigma_true_checked
    } else {
      stop(fun, ": method = 'welch' requires scalar sigma_true (recycled) or length-2 sigma_true.")
    }
  }

  power_at_n1 <- function(nn1) {
    nn1 <- as.integer(nn1)
    nn2 <- as.integer(max(2L, ceiling(n_ratio * nn1)))

    power_t_mu(
      mu_a = mu_a,
      mu0 = mu0,
      sigma_true = sigma_true_out,
      n = c(nn1, nn2),
      alpha = alpha,
      alternative = alternative,
      method = method_type,
      quiet = TRUE
    )$power
  }

  sol <- .nreq_find_min_n(
    power_at_n = power_at_n1,
    target_power = target_power,
    n_min = n_min,
    n_max = n_max,
    fun = fun,
    suppress_warnings = FALSE
  )

  n1 <- sol$n
  n2 <- as.integer(max(2L, ceiling(n_ratio * n1)))
  chk <- power_t_mu(
    mu_a = mu_a,
    mu0 = mu0,
    sigma_true = sigma_true_out,
    n = c(n1, n2),
    alpha = alpha,
    alternative = alternative,
    method = method_type,
    quiet = TRUE
  )

  out <- .new_n_required_result(
    class_name = "n_required_t_mu_result",
    method = "Required n for two-sample t test for difference in means",
    target_power = target_power,
    achieved_power = chk$power,
    target_beta = beta_target,
    achieved_beta = chk$beta,
    assumptions = chk$assumptions,
    inputs = list(
      mu_a = mu_a,
      mu0 = mu0,
      sigma_true = sigma_true_out,
      alpha = alpha,
      beta_target = beta_target,
      alternative = alternative,
      method = method_type,
      n_ratio = n_ratio,
      n_min = n_min,
      n_max = n_max
    ),
    solver = "integer bracketing + binary search",
    scenario = "two_sample",
    parameter = "mu1 - mu2",
    alternative = alternative,
    alpha = alpha,
    null = mu0,
    true_value = delta_a,
    method_type = method_type,
    mu_a = mu_a,
    mu1_a = mu_a[1L],
    mu2_a = mu_a[2L],
    sigma_true = chk$sigma_true,
    n = c(n1, n2),
    n1 = n1,
    n2 = n2,
    n_ratio = n_ratio,
    delta_a = delta_a,
    n_is_minimal = sol$n_is_minimal,
    n_search_min = sol$lower_bound,
    n_search_max = sol$upper_bound,
    n_evaluations = sol$n_evaluations,
    bracket_upper = sol$bracket_upper,
    df = chk$df,
    se = chk$se,
    ncp = chk$ncp,
    crit = chk$crit,
    crit_label = chk$crit_label,
    tcrit = chk$tcrit,
    reject_region = chk$reject_region,
    region = chk$region
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    cat(
      "Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ",
      .ci_format_num(out$null, digits), "\n",
      sep = ""
    )
    cat(
      "alpha = ", .ci_format_num(out$alpha, digits),
      "   target power = ", .ci_format_num(out$target_power, digits),
      "   target beta = ", .ci_format_num(out$target_beta, digits),
      "\n",
      sep = ""
    )
    cat(
      "method = ", out$method_type,
      "  n_ratio = ", .ci_format_num(out$n_ratio, digits),
      "\n",
      sep = ""
    )
    cat(
      "mu1_a = ", .ci_format_num(out$mu1_a, digits),
      "  mu2_a = ", .ci_format_num(out$mu2_a, digits),
      "  delta0 = ", .ci_format_num(out$null, digits),
      "\n",
      sep = ""
    )
    cat("Required n1 = ", out$n1, "  n2 = ", out$n2, "\n", sep = "")
    cat(
      "Check: power = ", .ci_format_num(out$achieved_power, digits),
      "  beta = ", .ci_format_num(out$achieved_beta, digits),
      "\n",
      sep = ""
    )
  }

  invisible(out)
}


# Required n for proportion z tests ---------------------------------------

#' @describeIn n_required_functions Required sample size for z tests of proportions.
#' @export
n_required_p_z <- function(p_a, p0,
                           alpha = 0.05,
                           beta_target = 0.10,
                           alternative = c("two.sided", "less", "greater"),
                           n_min = 5L,
                           n_max = 1e7L,
                           pooled = NULL,
                           continuity = FALSE,
                           n_ratio = 1,
                           digits = 4,
                           quiet = FALSE) {
  fun <- "n_required_p_z()"
  pooled_supplied <- !missing(pooled)
  continuity_supplied <- !missing(continuity)
  n_ratio_supplied <- !missing(n_ratio)

  alpha <- .ht_validate_alpha(alpha, fun)
  beta_target <- .nreq_validate_open_probability(beta_target, "beta_target", fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  alternative <- match.arg(alternative)
  continuity <- .ci_validate_flag(continuity, "continuity", fun)
  pooled <- .ht_validate_optional_flag(pooled, "pooled", fun)
  n_ratio <- .ht_validate_positive_scalar(n_ratio, "n_ratio", fun)

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

  n_min <- .nreq_validate_integer_scalar(n_min, "n_min", fun, min_value = 1L)
  n_max <- .nreq_validate_integer_scalar(n_max, "n_max", fun, min_value = 1L)
  if (n_min > n_max) {
    stop(fun, ": n_min must be <= n_max.")
  }

  target_power <- 1 - beta_target
  fmt <- function(y) .ci_format_num(y, digits)

  if (k == 1L) {
    if (pooled_supplied && !is.null(pooled)) {
      .ht_warn_ignored_arg(fun, "pooled", "ignored in the one-sample case.")
    }
    if (continuity_supplied) {
      .ht_warn_ignored_arg(fun, "continuity", "ignored in the one-sample case.")
    }
    if (n_ratio_supplied) {
      .ht_warn_ignored_arg(fun, "n_ratio", "ignored in the one-sample case.")
    }

    p_a_scalar <- .ht_validate_probability_closed(p_a[1L], "p_a", fun)
    p0 <- .ht_validate_probability_closed(p0, "p0", fun)

    if (alternative == "less" && p_a_scalar >= p0) {
      stop(fun, ": for alternative = 'less', need p_a < p0.")
    }
    if (alternative == "greater" && p_a_scalar <= p0) {
      stop(fun, ": for alternative = 'greater', need p_a > p0.")
    }

    .nreq_stop_if_null_equivalent(
      true_value = p_a_scalar,
      null_value = p0,
      alternative = alternative,
      alpha = alpha,
      target_power = target_power,
      fun = fun
    )

    power_at_n <- function(nn) {
      power_p_z(
        p_a = p_a_scalar,
        p0 = p0,
        n = nn,
        alpha = alpha,
        alternative = alternative,
        quiet = TRUE
      )$power
    }

    sol <- .nreq_find_min_n(
      power_at_n = power_at_n,
      target_power = target_power,
      n_min = n_min,
      n_max = n_max,
      fun = fun,
      suppress_warnings = TRUE
    )

    chk <- suppressWarnings(
  power_p_z(
    p_a = p_a_scalar,
    p0 = p0,
    n = sol$n,
    alpha = alpha,
    alternative = alternative,
    quiet = TRUE
  )
)

    adequacy_ok <- all(c(
      sol$n * p0,
      sol$n * (1 - p0),
      sol$n * p_a_scalar,
      sol$n * (1 - p_a_scalar)
    ) >= 5)

    if (!adequacy_ok) {
      warning(
        fun,
        ": normal approximation may be unreliable because some expected counts at the required n are < 5.",
        call. = FALSE
      )
    }

    out <- .new_n_required_result(
      class_name = "n_required_p_z_result",
      method = "Required n for one-sample proportion z test (normal approximation)",
      target_power = target_power,
      achieved_power = chk$power,
      target_beta = beta_target,
      achieved_beta = chk$beta,
      assumptions = c(
        "Large-sample normal approximation is used",
        "Required n is the smallest integer in the search range meeting the target power"
      ),
      inputs = list(
        p_a = p_a_scalar,
        p0 = p0,
        alpha = alpha,
        beta_target = beta_target,
        alternative = alternative,
        n_min = n_min,
        n_max = n_max
      ),
      solver = "integer bracketing + binary search",
      scenario = "one_sample",
      parameter = "p",
      alternative = alternative,
      alpha = alpha,
      null = p0,
      true_value = p_a_scalar,
      p_a = p_a_scalar,
      n = sol$n,
      n_is_minimal = sol$n_is_minimal,
      n_search_min = sol$lower_bound,
      n_search_max = sol$upper_bound,
      n_evaluations = sol$n_evaluations,
      bracket_upper = sol$bracket_upper,
      pooled = NULL,
      p_pool = NULL,
      continuity = FALSE,
      continuity_correction = FALSE,
      adequacy_ok = adequacy_ok,
      approximation_ok = adequacy_ok,
      se0 = chk$se0,
      sd_phat_a = chk$sd_phat_a,
      reject_region = chk$region,
      region = chk$region
    )

    if (!quiet) {
      cat("\n", out$method, "\n", sep = "")
      cat("Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ", fmt(out$null), "\n", sep = "")
      cat(
        "alpha = ", fmt(out$alpha),
        "   target power = ", fmt(out$target_power),
        "   target beta = ", fmt(out$target_beta), "\n",
        sep = ""
      )
      cat("p_a = ", fmt(out$p_a), "  p0 = ", fmt(out$null), "\n", sep = "")
      cat("Required n = ", out$n, "\n", sep = "")
      cat(
        "Approximation adequate (all expected counts >= 5): ",
        if (out$approximation_ok) "yes" else "no", "\n",
        sep = ""
      )
      cat(
        "Check: power = ", fmt(out$achieved_power),
        "  beta = ", fmt(out$achieved_beta), "\n",
        sep = ""
      )
    }

    return(invisible(out))
  }

  # two-sample
  delta0 <- .ht_validate_difference_null(p0, "p0", fun)
  opts <- .ht_resolve_two_prop_options(delta0, pooled, continuity, fun)
  pooled <- opts$pooled
  continuity <- opts$continuity

  p1_a <- p_a[1L]
  p2_a <- p_a[2L]
  delta_a <- p1_a - p2_a

  if (alternative == "less" && delta_a >= delta0) {
    stop(fun, ": for alternative = 'less', need (p1_a - p2_a) < p0.")
  }
  if (alternative == "greater" && delta_a <= delta0) {
    stop(fun, ": for alternative = 'greater', need (p1_a - p2_a) > p0.")
  }

  .nreq_stop_if_null_equivalent(
    true_value = delta_a,
    null_value = delta0,
    alternative = alternative,
    alpha = alpha,
    target_power = target_power,
    fun = fun
  )

  power_at_n1 <- function(nn1) {
    nn1 <- as.integer(nn1)
    nn2 <- as.integer(max(1L, ceiling(n_ratio * nn1)))

    power_p_z(
      p_a = c(p1_a, p2_a),
      p0 = delta0,
      n = c(nn1, nn2),
      alpha = alpha,
      alternative = alternative,
      pooled = pooled,
      continuity = continuity,
      quiet = TRUE
    )$power
  }

  sol <- .nreq_find_min_n(
    power_at_n = power_at_n1,
    target_power = target_power,
    n_min = n_min,
    n_max = n_max,
    fun = fun,
    suppress_warnings = TRUE
  )

  n1 <- sol$n
  n2 <- as.integer(max(1L, ceiling(n_ratio * n1)))

  chk <- suppressWarnings(
  power_p_z(
    p_a = c(p1_a, p2_a),
    p0 = delta0,
    n = c(n1, n2),
    alpha = alpha,
    alternative = alternative,
    pooled = pooled,
    continuity = continuity,
    quiet = TRUE
  )
)

  if (pooled) {
    p_pool <- (n1 * p1_a + n2 * p2_a) / (n1 + n2)
    adequacy_ok <- all(c(
      n1 * p_pool,
      n1 * (1 - p_pool),
      n2 * p_pool,
      n2 * (1 - p_pool)
    ) >= 5)
  } else {
    p_pool <- NA_real_
    adequacy_ok <- all(c(
      n1 * p1_a,
      n1 * (1 - p1_a),
      n2 * p2_a,
      n2 * (1 - p2_a)
    ) >= 5)
  }

  if (!adequacy_ok) {
    .ht_warn_prop_adequacy(adequacy_ok, pooled, fun)
  }

  out <- .new_n_required_result(
    class_name = "n_required_p_z_result",
    method = paste0(
      "Required n for two-sample proportion z test (",
      .ht_two_prop_method_suffix(delta0, pooled, continuity),
      ")"
    ),
    target_power = target_power,
    achieved_power = chk$power,
    target_beta = beta_target,
    achieved_beta = chk$beta,
    assumptions = c(
      "The two samples are independent",
      "Large-sample normal approximation is used",
      "Required n1 is the smallest integer in the search range meeting the target power"
    ),
    inputs = list(
      p_a = c(p1_a, p2_a),
      p0 = delta0,
      alpha = alpha,
      beta_target = beta_target,
      alternative = alternative,
      pooled = pooled,
      continuity = continuity,
      n_ratio = n_ratio,
      n_min = n_min,
      n_max = n_max
    ),
    solver = "integer bracketing + binary search",
    scenario = "two_sample",
    parameter = "p1 - p2",
    alternative = alternative,
    alpha = alpha,
    null = delta0,
    true_value = delta_a,
    p_a = c(p1_a, p2_a),
    p1_a = p1_a,
    p2_a = p2_a,
    delta_a = delta_a,
    n = c(n1, n2),
    n1 = n1,
    n2 = n2,
    n_ratio = n_ratio,
    n_is_minimal = sol$n_is_minimal,
    n_search_min = sol$lower_bound,
    n_search_max = sol$upper_bound,
    n_evaluations = sol$n_evaluations,
    bracket_upper = sol$bracket_upper,
    pooled = pooled,
    p_pool = p_pool,
    continuity = continuity,
    continuity_correction = continuity,
    cc = chk$cc,
    adequacy_ok = adequacy_ok,
    approximation_ok = adequacy_ok,
    se0 = chk$se0,
    sd_diff_a = chk$sd_diff_a,
    reject_region = chk$region,
    region = chk$region
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    cat("Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ", fmt(out$null), "\n", sep = "")
    cat(
      "alpha = ", fmt(out$alpha),
      "   target power = ", fmt(out$target_power),
      "   target beta = ", fmt(out$target_beta), "\n",
      sep = ""
    )
    cat(
      "p1_a = ", fmt(out$p1_a),
      "  p2_a = ", fmt(out$p2_a),
      "  delta0 = ", fmt(out$null), "\n",
      sep = ""
    )
    cat(
      "n_ratio = ", fmt(out$n_ratio),
      "  pooled = ", out$pooled,
      "  continuity = ", out$continuity, "\n",
      sep = ""
    )
    cat("Required n1 = ", out$n1, "  n2 = ", out$n2, "\n", sep = "")
    cat(
      "Approximation adequate (all expected counts >= 5): ",
      if (out$approximation_ok) "yes" else "no", "\n",
      sep = ""
    )
    cat(
      "Check: power = ", fmt(out$achieved_power),
      "  beta = ", fmt(out$achieved_beta), "\n",
      sep = ""
    )
  }

  invisible(out)
}

# Required n for variance tests -------------------------------------------

#' @describeIn n_required_functions Required sample size for one-sample chi-square tests of variance.
#' @export
n_required_var_chisq <- function(sigma_a,
                                 sigma0,
                                 alpha = 0.05,
                                 beta_target = 0.10,
                                 alternative = c("two.sided", "less", "greater"),
                                 n_min = 2L,
                                 n_max = 1e6L,
                                 digits = 4,
                                 quiet = FALSE) {
  fun <- "n_required_var_chisq()"

  alpha <- .ht_validate_alpha(alpha, fun)
  beta_target <- .nreq_validate_open_probability(beta_target, "beta_target", fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  alternative <- match.arg(alternative)
  sigma_a <- .ht_validate_positive_scalar(sigma_a, "sigma_a", fun)
  sigma0 <- .ht_validate_positive_scalar(sigma0, "sigma0", fun)
  n_min <- .nreq_validate_integer_scalar(n_min, "n_min", fun, min_value = 2L)
  n_max <- .nreq_validate_integer_scalar(n_max, "n_max", fun, min_value = 2L)

  if (n_min > n_max) {
    stop(fun, ": n_min must be <= n_max.")
  }

  target_power <- 1 - beta_target

  if (alternative == "less" && sigma_a >= sigma0) {
    stop(fun, ": for alternative = 'less', sigma_a must be < sigma0.")
  }
  if (alternative == "greater" && sigma_a <= sigma0) {
    stop(fun, ": for alternative = 'greater', sigma_a must be > sigma0.")
  }

  .nreq_stop_if_null_equivalent(
    true_value = sigma_a,
    null_value = sigma0,
    alternative = alternative,
    alpha = alpha,
    target_power = target_power,
    fun = fun
  )

  power_at_n <- function(nn) {
    power_var_chisq(
      sigma_a = sigma_a,
      sigma0 = sigma0,
      n = nn,
      alpha = alpha,
      alternative = alternative,
      quiet = TRUE
    )$power
  }

  sol <- .nreq_find_min_n(
    power_at_n = power_at_n,
    target_power = target_power,
    n_min = n_min,
    n_max = n_max,
    fun = fun,
    suppress_warnings = FALSE
  )

  chk <- power_var_chisq(
    sigma_a = sigma_a,
    sigma0 = sigma0,
    n = sol$n,
    alpha = alpha,
    alternative = alternative,
    quiet = TRUE
  )

  out <- .new_n_required_result(
    class_name = "n_required_var_chisq_result",
    method = "Required n for one-sample chi-square test for variance",
    target_power = target_power,
    achieved_power = chk$power,
    target_beta = beta_target,
    achieved_beta = chk$beta,
    assumptions = chk$assumptions,
    inputs = list(
      sigma_a = sigma_a,
      sigma0 = sigma0,
      alpha = alpha,
      beta_target = beta_target,
      alternative = alternative,
      n_min = n_min,
      n_max = n_max
    ),
    solver = "integer bracketing + binary search",
    scenario = "one_sample",
    parameter = "sigma^2",
    alternative = alternative,
    alpha = alpha,
    null = sigma0^2,
    true_value = sigma_a^2,
    sigma_a = sigma_a,
    sigma0 = sigma0,
    n = sol$n,
    n_is_minimal = sol$n_is_minimal,
    n_search_min = sol$lower_bound,
    n_search_max = sol$upper_bound,
    n_evaluations = sol$n_evaluations,
    bracket_upper = sol$bracket_upper,
    df = chk$df,
    k = chk$k,
    crit = chk$crit,
    crit_label = chk$crit_label,
    chi_crit = chk$chi_crit,
    reject_region = chk$reject_region,
    region = chk$region
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    cat(
      "Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ",
      .ci_format_num(out$null, digits), "\n",
      sep = ""
    )
    cat(
      "alpha = ", .ci_format_num(out$alpha, digits),
      "   target power = ", .ci_format_num(out$target_power, digits),
      "   target beta = ", .ci_format_num(out$target_beta, digits),
      "\n",
      sep = ""
    )
    cat(
      "sigma_a = ", .ci_format_num(out$sigma_a, digits),
      "  sigma0 = ", .ci_format_num(out$sigma0, digits),
      "\n",
      sep = ""
    )
    cat("Required n = ", out$n, "\n", sep = "")
    cat(
      "Check: power = ", .ci_format_num(out$achieved_power, digits),
      "  beta = ", .ci_format_num(out$achieved_beta, digits),
      "\n",
      sep = ""
    )
  }

  invisible(out)
}

#' @describeIn n_required_functions Required sample size for two-sample F tests of a variance ratio.
#' @export
n_required_var_ratio_F <- function(sigma_a,
                                   ratio0 = 1,
                                   alpha = 0.05,
                                   beta_target = 0.10,
                                   alternative = c("two.sided", "less", "greater"),
                                   n_min = 2L,
                                   n_max = 1e6L,
                                   n_ratio = 1,
                                   digits = 4,
                                   quiet = FALSE) {
  fun <- "n_required_var_ratio_F()"

  alpha <- .ht_validate_alpha(alpha, fun)
  beta_target <- .nreq_validate_open_probability(beta_target, "beta_target", fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  alternative <- match.arg(alternative)
  sigma_a <- .power_validate_positive_numeric_vector(sigma_a, "sigma_a", fun)
  ratio0 <- .ht_validate_positive_scalar(ratio0, "ratio0", fun)
  n_ratio <- .ht_validate_positive_scalar(n_ratio, "n_ratio", fun)
  n_min <- .nreq_validate_integer_scalar(n_min, "n_min", fun, min_value = 2L)
  n_max <- .nreq_validate_integer_scalar(n_max, "n_max", fun, min_value = 2L)

  if (length(sigma_a) != 2L) {
    stop(fun, ": sigma_a must be length 2: c(sigma1_a, sigma2_a).")
  }
  if (n_min > n_max) {
    stop(fun, ": n_min must be <= n_max.")
  }

  target_power <- 1 - beta_target
  ratio_a <- (sigma_a[1L]^2) / (sigma_a[2L]^2)

  if (alternative == "less" && ratio_a >= ratio0) {
    stop(fun, ": for alternative = 'less', sigma1_a^2 / sigma2_a^2 must be < ratio0.")
  }
  if (alternative == "greater" && ratio_a <= ratio0) {
    stop(fun, ": for alternative = 'greater', sigma1_a^2 / sigma2_a^2 must be > ratio0.")
  }

  .nreq_stop_if_null_equivalent(
    true_value = ratio_a,
    null_value = ratio0,
    alternative = alternative,
    alpha = alpha,
    target_power = target_power,
    fun = fun
  )

  power_at_n1 <- function(nn1) {
    nn1 <- as.integer(nn1)
    nn2 <- as.integer(max(2L, ceiling(n_ratio * nn1)))

    power_var_ratio_F(
      sigma_a = sigma_a,
      ratio0 = ratio0,
      n = c(nn1, nn2),
      alpha = alpha,
      alternative = alternative,
      quiet = TRUE
    )$power
  }

  sol <- .nreq_find_min_n(
    power_at_n = power_at_n1,
    target_power = target_power,
    n_min = n_min,
    n_max = n_max,
    fun = fun,
    suppress_warnings = FALSE
  )

  n1 <- sol$n
  n2 <- as.integer(max(2L, ceiling(n_ratio * n1)))
  chk <- power_var_ratio_F(
    sigma_a = sigma_a,
    ratio0 = ratio0,
    n = c(n1, n2),
    alpha = alpha,
    alternative = alternative,
    quiet = TRUE
  )

  out <- .new_n_required_result(
    class_name = "n_required_var_ratio_F_result",
    method = "Required n for two-sample F test of variance ratio",
    target_power = target_power,
    achieved_power = chk$power,
    target_beta = beta_target,
    achieved_beta = chk$beta,
    assumptions = chk$assumptions,
    inputs = list(
      sigma_a = sigma_a,
      ratio0 = ratio0,
      alpha = alpha,
      beta_target = beta_target,
      alternative = alternative,
      n_ratio = n_ratio,
      n_min = n_min,
      n_max = n_max
    ),
    solver = "integer bracketing + binary search",
    scenario = "two_sample",
    parameter = "sigma1^2 / sigma2^2",
    alternative = alternative,
    alpha = alpha,
    null = ratio0,
    true_value = ratio_a,
    sigma_a = sigma_a,
    sigma1_a = sigma_a[1L],
    sigma2_a = sigma_a[2L],
    n = c(n1, n2),
    n1 = n1,
    n2 = n2,
    n_ratio = n_ratio,
    n_is_minimal = sol$n_is_minimal,
    n_search_min = sol$lower_bound,
    n_search_max = sol$upper_bound,
    n_evaluations = sol$n_evaluations,
    bracket_upper = sol$bracket_upper,
    df1 = chk$df1,
    df2 = chk$df2,
    ratio_a = chk$ratio_a,
    ratio0 = chk$ratio0,
    k = chk$k,
    crit = chk$crit,
    crit_label = chk$crit_label,
    f_crit = chk$f_crit,
    reject_region = chk$reject_region,
    region = chk$region
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    cat(
      "Ha: ", out$parameter, " ", .ht_alt_symbol(out$alternative), " ",
      .ci_format_num(out$null, digits), "\n",
      sep = ""
    )
    cat(
      "alpha = ", .ci_format_num(out$alpha, digits),
      "   target power = ", .ci_format_num(out$target_power, digits),
      "   target beta = ", .ci_format_num(out$target_beta, digits),
      "\n",
      sep = ""
    )
    cat(
      "sigma1_a = ", .ci_format_num(out$sigma1_a, digits),
      "  sigma2_a = ", .ci_format_num(out$sigma2_a, digits),
      "  n_ratio = ", .ci_format_num(out$n_ratio, digits),
      "\n",
      sep = ""
    )
    cat("Required n1 = ", out$n1, "  n2 = ", out$n2, "\n", sep = "")
    cat(
      "Check: power = ", .ci_format_num(out$achieved_power, digits),
      "  beta = ", .ci_format_num(out$achieved_beta, digits),
      "\n",
      sep = ""
    )
  }

  invisible(out)
}
