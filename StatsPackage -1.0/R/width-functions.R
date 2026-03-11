#' Confidence-interval width planning functions
#'
#' Determines the sample size required to achieve a target total confidence
#' interval width for selected interval procedures.
#'
#' These functions return classed result objects with unrounded stored values
#' and optional printed summaries controlled by `digits` and `quiet`.
#'
#' @param w Target total confidence-interval width.
#' @param sigma Known population standard deviation input.
#' @param conf.level Confidence level in (0, 1).
#' @param digits Integer number of decimal places used only for printed output.
#' @param quiet Logical; if `TRUE`, suppress printed output.
#' @param p Planning value for a proportion.
#' @param worst_case Logical; if `TRUE`, use the worst-case proportion value.
#' @param s Sample standard deviation used for t-based width planning.
#' @param n_start Starting sample size for iterative width planning.
#' @param max_iter Maximum number of search iterations.
#' @name width_plan_functions
NULL

# Width-planning functions -------------------------------------------------

#' @describeIn width_plan_functions Required sample size for a z interval for a mean.
#' @export
n_width_mu_z <- function(w, sigma, conf.level = 0.95, digits = 4, quiet = FALSE) {
  fun <- "n_width_mu_z()"
  tol <- 1e-12

  .ci_validate_conf_level(conf.level, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  w <- .width_validate_positive_scalar(w, "w", fun)
  sigma <- .width_validate_positive_scalar(sigma, "sigma", fun)

  alpha <- 1 - conf.level
  z <- stats::qnorm(1 - alpha / 2)

  n_star <- (2 * z * sigma / w)^2
  n <- ceiling(n_star)

  achieved_half_width <- z * sigma / sqrt(n)
  achieved_full_width <- 2 * achieved_half_width

  width_at_n_minus_1 <- if (n > 1) {
    2 * z * sigma / sqrt(n - 1)
  } else {
    NA_real_
  }

  meets_target <- achieved_full_width <= w + tol
  n_is_minimal <- if (n == 1) TRUE else width_at_n_minus_1 > w + tol

  out <- .new_width_result(
    class_name = "width_mu_z_result",
    method = "Width planning for mean CI (z; sigma known)",
    conf.level = conf.level,
    alpha = alpha,
    n = n,
    target_full_width = w,
    target_half_width = w / 2,
    achieved_full_width = achieved_full_width,
    achieved_half_width = achieved_half_width,
    assumptions = c(
      "Two-sided CI width planning",
      "Population sigma is known",
      "Population is normal or the sample size is large"
    ),
    inputs = list(
      w = w,
      sigma = sigma,
      conf.level = conf.level
    ),
    procedure = "one_mu_z_width",
    crit = z,
    crit_label = "z*",
    n_star = n_star,
    sigma = sigma,
    width_at_n_minus_1 = width_at_n_minus_1,
    meets_target = meets_target,
    n_is_minimal = n_is_minimal
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    cat(
      "conf.level = ", .ci_format_num(out$conf.level, digits),
      "   alpha = ", .ci_format_num(out$alpha, digits), "\n",
      sep = ""
    )
    cat(
      out$crit_label, " = ", .ci_format_num(out$crit, digits),
      "  sigma = ", .ci_format_num(out$sigma, digits), "\n",
      sep = ""
    )
    cat(
      "Target:   E = ", .ci_format_num(out$target_half_width, digits),
      "  | w = ", .ci_format_num(out$target_full_width, digits), "\n",
      sep = ""
    )
    cat(
      "Result n = ", format(out$n, scientific = FALSE, trim = TRUE),
      "  (n* = ", .ci_format_num(out$n_star, digits), ")\n",
      sep = ""
    )
    cat(
      "Achieved: E = ", .ci_format_num(out$achieved_half_width, digits),
      "  | w = ", .ci_format_num(out$achieved_full_width, digits), "\n",
      sep = ""
    )
  }

  invisible(out)
}


#' @describeIn width_plan_functions Required sample size for a Wald interval for a proportion.
#' @export
n_width_p_wald <- function(w, conf.level = 0.95, p = 0.5, worst_case = TRUE,
                           digits = 4, quiet = FALSE) {
  fun <- "n_width_p_wald()"
  tol <- 1e-12
  p_supplied <- !missing(p)

  .ci_validate_conf_level(conf.level, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  worst_case <- .ci_validate_flag(worst_case, "worst_case", fun)
  w <- .width_validate_positive_scalar(w, "w", fun)

  if (worst_case) {
    if (p_supplied) {
      .width_warn_p_ignored(fun)
    }
    p_used <- 0.5
  } else {
    p_used <- .width_validate_probability_scalar(p, "p", fun)
  }

  alpha <- 1 - conf.level
  z <- stats::qnorm(1 - alpha / 2)

  n_star <- ((2 * z)^2 * p_used * (1 - p_used)) / w^2
  n <- ceiling(n_star)

  achieved_half_width <- z * sqrt(p_used * (1 - p_used) / n)
  achieved_full_width <- 2 * achieved_half_width

  width_at_n_minus_1 <- if (n > 1) {
    2 * z * sqrt(p_used * (1 - p_used) / (n - 1))
  } else {
    NA_real_
  }

  meets_target <- achieved_full_width <= w + tol
  n_is_minimal <- if (n == 1) TRUE else width_at_n_minus_1 > w + tol
  approximation_ok <- (n * p_used >= 5) && (n * (1 - p_used) >= 5)

  .width_warn_wald_adequacy(n, p_used, fun)

  out <- .new_width_result(
    class_name = "width_p_wald_result",
    method = "Width planning for proportion CI (Wald normal approximation)",
    conf.level = conf.level,
    alpha = alpha,
    n = n,
    target_full_width = w,
    target_half_width = w / 2,
    achieved_full_width = achieved_full_width,
    achieved_half_width = achieved_half_width,
    assumptions = c(
      "Two-sided CI width planning",
      "Observations are Bernoulli trials",
      "Trials are independent",
      "The Wald normal approximation is being used"
    ),
    inputs = list(
      w = w,
      p = if (p_supplied) p else NULL,
      worst_case = worst_case,
      conf.level = conf.level
    ),
    procedure = "one_p_wald_width",
    crit = z,
    crit_label = "z*",
    n_star = n_star,
    p_used = p_used,
    worst_case = worst_case,
    approximation_ok = approximation_ok,
    width_at_n_minus_1 = width_at_n_minus_1,
    meets_target = meets_target,
    n_is_minimal = n_is_minimal
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    cat(
      "conf.level = ", .ci_format_num(out$conf.level, digits),
      "   alpha = ", .ci_format_num(out$alpha, digits), "\n",
      sep = ""
    )
    cat(
      out$crit_label, " = ", .ci_format_num(out$crit, digits),
      "  p_used = ", .ci_format_num(out$p_used, digits),
      if (out$worst_case) "  (worst-case)" else "",
      "\n",
      sep = ""
    )
    cat(
      "Target:   E = ", .ci_format_num(out$target_half_width, digits),
      "  | w = ", .ci_format_num(out$target_full_width, digits), "\n",
      sep = ""
    )
    cat(
      "Result n = ", format(out$n, scientific = FALSE, trim = TRUE),
      "  (n* = ", .ci_format_num(out$n_star, digits), ")\n",
      sep = ""
    )
    cat(
      "Achieved: E = ", .ci_format_num(out$achieved_half_width, digits),
      "  | w = ", .ci_format_num(out$achieved_full_width, digits), "\n",
      sep = ""
    )
  }

  invisible(out)
}


#' @describeIn width_plan_functions Required sample size for a t interval for a mean.
#' @export
n_width_mu_t <- function(w, s, conf.level = 0.95, n_start = NULL, max_iter = 100,
                         digits = 4, quiet = FALSE) {
  fun <- "n_width_mu_t()"
  tol <- 1e-12

  .ci_validate_conf_level(conf.level, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  w <- .width_validate_positive_scalar(w, "w", fun)
  s <- .width_validate_positive_scalar(s, "s", fun)
  max_iter <- .width_validate_integer_scalar(max_iter, "max_iter", fun, min_value = 1L)

  alpha <- 1 - conf.level
  z <- stats::qnorm(1 - alpha / 2)
  n_star_initial <- (2 * z * s / w)^2

  if (is.null(n_start)) {
    n_initial <- max(2, ceiling(n_star_initial))
  } else {
    n_initial <- .width_validate_integer_scalar(n_start, "n_start", fun, min_value = 2L)
  }

  width_for_n <- function(n_val) {
    if (n_val < 2) {
      stop(fun, ": internal error: n for t-based planning must be >= 2.")
    }
    tcrit_val <- stats::qt(1 - alpha / 2, df = n_val - 1)
    2 * tcrit_val * s / sqrt(n_val)
  }

  n_iter <- n_initial
  converged <- FALSE
  iterations <- 0L

  for (it in seq_len(max_iter)) {
    iterations <- it
    tcrit_iter <- stats::qt(1 - alpha / 2, df = n_iter - 1)
    n_new <- max(2, ceiling((2 * tcrit_iter * s / w)^2))

    if (n_new == n_iter) {
      converged <- TRUE
      break
    }

    n_iter <- n_new
  }

  if (!converged) {
    warning(
      fun,
      ": fixed-point iteration did not converge within max_iter; a post-check search was used to return a valid n.",
      call. = FALSE
    )
  }

  n_after_iteration <- n_iter
  postcheck_adjustments <- 0L

  while (width_for_n(n_after_iteration) > w + tol) {
    n_after_iteration <- n_after_iteration + 1
    postcheck_adjustments <- postcheck_adjustments + 1L
  }

  while (n_after_iteration > 2 && width_for_n(n_after_iteration - 1) <= w + tol) {
    n_after_iteration <- n_after_iteration - 1
    postcheck_adjustments <- postcheck_adjustments + 1L
  }

  n <- n_after_iteration
  df <- n - 1
  tcrit <- stats::qt(1 - alpha / 2, df = df)
  achieved_half_width <- tcrit * s / sqrt(n)
  achieved_full_width <- 2 * achieved_half_width

  width_at_n_minus_1 <- if (n > 2) width_for_n(n - 1) else NA_real_

  meets_target <- achieved_full_width <= w + tol
  n_is_minimal <- if (n == 2) TRUE else width_at_n_minus_1 > w + tol

  out <- .new_width_result(
    class_name = "width_mu_t_result",
    method = "Width planning for mean CI (t; sigma unknown; using guessed s)",
    conf.level = conf.level,
    alpha = alpha,
    n = n,
    target_full_width = w,
    target_half_width = w / 2,
    achieved_full_width = achieved_full_width,
    achieved_half_width = achieved_half_width,
    assumptions = c(
      "Two-sided CI width planning",
      "Population sigma is unknown",
      "A planning value s is used as the guessed SD",
      "Population is approximately normal or the sample size is large"
    ),
    inputs = list(
      w = w,
      s = s,
      n_start = n_start,
      max_iter = max_iter,
      conf.level = conf.level
    ),
    procedure = "one_mu_t_width",
    crit = tcrit,
    crit_label = "t*",
    df = df,
    s_guess = s,
    n_star_initial = n_star_initial,
    n_initial = n_initial,
    n_after_iteration = n_iter,
    iterations = iterations,
    converged = converged,
    postcheck_adjustments = postcheck_adjustments,
    width_at_n_minus_1 = width_at_n_minus_1,
    meets_target = meets_target,
    n_is_minimal = n_is_minimal
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    cat(
      "conf.level = ", .ci_format_num(out$conf.level, digits),
      "   alpha = ", .ci_format_num(out$alpha, digits),
      "   df = ", .ci_format_num(out$df, digits), "\n",
      sep = ""
    )
    cat(
      out$crit_label, " = ", .ci_format_num(out$crit, digits),
      "  s_guess = ", .ci_format_num(out$s_guess, digits),
      "  iterations = ", format(out$iterations, scientific = FALSE, trim = TRUE),
      "  converged = ", out$converged, "\n",
      sep = ""
    )
    cat(
      "Target:   E = ", .ci_format_num(out$target_half_width, digits),
      "  | w = ", .ci_format_num(out$target_full_width, digits), "\n",
      sep = ""
    )
    cat(
      "Result n = ", format(out$n, scientific = FALSE, trim = TRUE),
      "  (initial z-based n* = ", .ci_format_num(out$n_star_initial, digits), ")\n",
      sep = ""
    )
    cat(
      "Achieved: E = ", .ci_format_num(out$achieved_half_width, digits),
      "  | w = ", .ci_format_num(out$achieved_full_width, digits), "\n",
      sep = ""
    )
  }

  invisible(out)
}
