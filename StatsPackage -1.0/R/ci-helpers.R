# Internal helpers ---------------------------------------------------------

.ci_is_whole_number <- function(x, tol = 1e-8) {
  is.finite(x) & (abs(x - round(x)) <= tol)
}

.ci_validate_conf_level <- function(conf.level, fun) {
  if (!is.numeric(conf.level) ||
      length(conf.level) != 1L ||
      !is.finite(conf.level) ||
      conf.level <= 0 ||
      conf.level >= 1) {
    stop(fun, ": conf.level must be a single number between 0 and 1.")
  }
}

.ci_validate_digits <- function(digits, fun) {
  if (!is.numeric(digits) ||
      length(digits) != 1L ||
      !is.finite(digits) ||
      digits < 0 ||
      abs(digits - round(digits)) > 1e-8) {
    stop(fun, ": digits must be a single whole number >= 0.")
  }
  as.integer(round(digits))
}

.ci_validate_flag <- function(x, arg, fun) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(fun, ": ", arg, " must be TRUE or FALSE.")
  }
  x
}

.ci_format_num <- function(x, digits = 4L) {
  vapply(
    x,
    FUN.VALUE = character(1),
    FUN = function(z) {
      if (is.na(z)) return("NA")
      if (is.nan(z)) return("NaN")
      if (is.infinite(z)) return(if (z > 0) "Inf" else "-Inf")
      formatC(z, format = "f", digits = digits)
    }
  )
}

.ci_format_ci <- function(ci, digits = 4L) {
  paste0("[", .ci_format_num(ci[1], digits), ", ", .ci_format_num(ci[2], digits), "]")
}

.new_ci_result <- function(class_name,
                           method,
                           parameter,
                           conf.level,
                           alpha,
                           side = "two.sided",
                           estimate,
                           ci,
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
      ci = ci,
      assumptions = assumptions,
      inputs = inputs
    ),
    list(...)
  )
  class(out) <- c(class_name, "ci_result", "list")
  out
}

.ci_warn_prop_wald <- function(successes, totals, fun) {
  if (any(successes < 5L | (totals - successes) < 5L)) {
    warning(
      fun,
      ": Wald normal approximation may be unreliable because some groups have fewer than 5 successes or 5 failures.",
      call. = FALSE
    )
  }
}

.ci_warn_z_unknown_sigma <- function(n, fun) {
  if (any(n < 30L)) {
    warning(
      fun,
      ': method = "z" with unknown sigma is a large-sample approximation; some sample sizes are < 30.',
      call. = FALSE
    )
  }
}
