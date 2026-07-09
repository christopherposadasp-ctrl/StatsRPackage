#' Prediction interval functions
#'
#' Computes prediction intervals for future observations from introductory
#' inference settings.
#'
#' These functions return classed result objects with unrounded stored values
#' and optional printed summaries controlled by `digits` and `quiet`.
#'
#' @param xbar Single sample mean.
#' @param n Single integer sample size.
#' @param s Sample standard deviation when population sigma is unknown.
#' @param sigma Known population standard deviation.
#' @param conf.level Confidence level in (0, 1).
#' @param side Interval side specification: `"two.sided"`, `"lower"`, or `"upper"`.
#' @param digits Integer number of decimal places used only for printed output.
#' @param quiet Logical; if `TRUE`, suppress printed output.
#' @examples
#' pi_mu(xbar = 12.4, n = 15, s = 3.2, quiet = TRUE)
#' @name prediction_interval_functions
NULL

# Prediction interval functions ------------------------------------------

.new_prediction_result <- function(class_name,
                                   method,
                                   parameter,
                                   conf.level,
                                   alpha,
                                   side = "two.sided",
                                   estimate,
                                   pi,
                                   assumptions = character(),
                                   inputs = list(),
                                   ...) {
  out <- c(
    list(
      method = method,
      parameter = parameter,
      conf.level = conf.level,
      alpha = alpha,
      side = side,
      estimate = estimate,
      pi = pi,
      assumptions = assumptions,
      inputs = inputs
    ),
    list(...)
  )
  class(out) <- c(class_name, "prediction_result", "list")
  out
}

#' @describeIn prediction_interval_functions Prediction interval for a future observation.
#' @export
pi_mu <- function(xbar, n,
                  s = NULL, sigma = NULL,
                  conf.level = 0.95,
                  side = c("two.sided", "lower", "upper"),
                  digits = 4, quiet = FALSE) {
  fun <- "pi_mu()"

  .ci_validate_conf_level(conf.level, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  side <- match.arg(side)

  if (!is.numeric(xbar) || length(xbar) != 1L || !is.finite(xbar)) {
    stop(fun, ": xbar must be one finite numeric value.")
  }
  if (!is.numeric(n) || length(n) != 1L || !.ci_is_whole_number(n) || n < 1) {
    stop(fun, ": n must be one whole-number sample size >= 1.")
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

  alpha <- 1 - conf.level
  alpha_tail <- if (side == "two.sided") alpha / 2 else alpha

  mk_pi <- function(est, margin) {
    if (side == "two.sided") {
      c(lower = est - margin, upper = est + margin)
    } else if (side == "lower") {
      c(lower = est - margin, upper = Inf)
    } else {
      c(lower = -Inf, upper = est + margin)
    }
  }

  if (sigma_known) {
    if (!is.numeric(sigma) || length(sigma) != 1L || !is.finite(sigma) || sigma < 0) {
      stop(fun, ": sigma must be one finite numeric value >= 0.")
    }

    se_pred <- sigma * sqrt(1 + 1 / n)
    crit <- stats::qnorm(1 - alpha_tail)
    margin <- crit * se_pred
    pi <- mk_pi(xbar, margin)

    out <- .new_prediction_result(
      class_name = "pi_mu_result",
      method = "One-sample prediction interval for a future observation (z; sigma known; summary stats)",
      parameter = "future_y",
      conf.level = conf.level,
      alpha = alpha,
      side = side,
      estimate = xbar,
      pi = pi,
      assumptions = c(
        "Observations are independent",
        "Future observation comes from the same population/process",
        "Population sigma is known",
        "Population is normal or the sample size is large"
      ),
      inputs = list(xbar = xbar, n = n, s = NULL, sigma = sigma),
      procedure = "one_z_known_sigma",
      se_pred = se_pred,
      df = NA_real_,
      crit = crit,
      crit_label = "z*",
      margin = margin
    )

    if (!quiet) {
      cat("\n", out$method, "\n", sep = "")
      cat("conf.level = ", .ci_format_num(out$conf.level, digits), "\n", sep = "")
      cat("xbar = ", .ci_format_num(out$estimate, digits),
          "  sigma = ", .ci_format_num(sigma, digits),
          "  n = ", n, "\n", sep = "")
      cat("SE_pred = ", .ci_format_num(out$se_pred, digits),
          "  ", out$crit_label, " = ", .ci_format_num(out$crit, digits),
          "  E = ", .ci_format_num(out$margin, digits), "\n", sep = "")
      cat("PI: ", .ci_format_ci(out$pi, digits), "\n", sep = "")
    }

    return(invisible(out))
  }

  if (!is.numeric(s) || length(s) != 1L || !is.finite(s) || s < 0) {
    stop(fun, ": s must be one finite numeric value >= 0.")
  }
  if (n < 2L) {
    stop(fun, ": t prediction intervals require n >= 2 when sigma is unknown.")
  }

  df <- n - 1L
  se_pred <- s * sqrt(1 + 1 / n)
  crit <- stats::qt(1 - alpha_tail, df = df)
  margin <- crit * se_pred
  pi <- mk_pi(xbar, margin)

  out <- .new_prediction_result(
    class_name = "pi_mu_result",
    method = "One-sample prediction interval for a future observation (t; sigma unknown; summary stats)",
    parameter = "future_y",
    conf.level = conf.level,
    alpha = alpha,
    side = side,
    estimate = xbar,
    pi = pi,
    assumptions = c(
      "Observations are independent",
      "Future observation comes from the same population/process",
      "Population is approximately normal or the sample size is large",
      "Population sigma is unknown"
    ),
    inputs = list(xbar = xbar, n = n, s = s, sigma = NULL),
    procedure = "one_t_unknown_sigma",
    se_pred = se_pred,
    df = df,
    crit = crit,
    crit_label = "t*",
    margin = margin
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    cat("conf.level = ", .ci_format_num(out$conf.level, digits),
        "   df = ", .ci_format_num(out$df, digits), "\n", sep = "")
    cat("xbar = ", .ci_format_num(out$estimate, digits),
        "  s = ", .ci_format_num(s, digits),
        "  n = ", n, "\n", sep = "")
    cat("SE_pred = ", .ci_format_num(out$se_pred, digits),
        "  ", out$crit_label, " = ", .ci_format_num(out$crit, digits),
        "  E = ", .ci_format_num(out$margin, digits), "\n", sep = "")
    cat("PI: ", .ci_format_ci(out$pi, digits), "\n", sep = "")
  }

  invisible(out)
}
