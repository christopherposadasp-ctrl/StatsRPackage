############################
# Shared helpers for hypothesis tests -------------------------------------
################################
.ht_build_region_nonsymmetric <- function(stat_name,
                                          alternative,
                                          crit,
                                          formatter = .ht_internal_format_num) {
  f <- formatter

  if (alternative == "two.sided") {
    if (!is.numeric(crit) || length(crit) != 2L || any(!is.finite(crit))) {
      stop(
        ".ht_build_region_nonsymmetric(): for alternative = 'two.sided', ",
        "crit must be a finite numeric vector of length 2."
      )
    }

    return(
      paste0(
        stat_name, " < ", f(crit[1L]),
        " or ", stat_name, " > ", f(crit[2L])
      )
    )
  }

  crit_val <- if (length(crit) == 1L) {
    crit
  } else if (alternative == "less") {
    crit[1L]
  } else {
    crit[length(crit)]
  }

  direction <- if (alternative == "less") "< " else "> "
  paste0(stat_name, " ", direction, f(crit_val))
}
.ht_validate_alpha <- function(alpha, fun) {
  if (!is.numeric(alpha) ||
      length(alpha) != 1L ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1) {
    stop(fun, ": alpha must be a single number between 0 and 1.")
  }
  alpha
}

.ht_validate_finite_scalar <- function(x, arg, fun) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x)) {
    stop(fun, ": ", arg, " must be a single finite number.")
  }
  x
}

.ht_validate_positive_scalar <- function(x, arg, fun) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x) ||
      x <= 0) {
    stop(fun, ": ", arg, " must be a single positive finite number.")
  }
  x
}

.ht_validate_probability_closed <- function(x, arg, fun) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x) ||
      x < 0 ||
      x > 1) {
    stop(fun, ": ", arg, " must be a single number between 0 and 1.")
  }
  x
}

.ht_validate_difference_null <- function(x, arg, fun) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x) ||
      x < -1 ||
      x > 1) {
    stop(fun, ": ", arg, " must be a single number between -1 and 1.")
  }
  x
}

.ht_validate_integer_vector <- function(x, arg, fun, min_value = 0L) {
  if (!is.numeric(x) ||
      length(x) == 0L ||
      any(!is.finite(x)) ||
      any(!.ci_is_whole_number(x)) ||
      any(x < min_value)) {
    stop(fun, ": ", arg, " must contain integer values >= ", min_value, ".")
  }
  as.integer(round(x))
}

.ht_validate_optional_flag <- function(x, arg, fun) {
  if (is.null(x)) {
    return(NULL)
  }
  .ci_validate_flag(x, arg, fun)
}

.ht_internal_format_num <- function(x) {
  vapply(
    x,
    FUN.VALUE = character(1),
    FUN = function(z) {
      if (is.na(z)) return("NA")
      if (is.nan(z)) return("NaN")
      if (is.infinite(z)) return(if (z > 0) "Inf" else "-Inf")
      format(z, digits = 15, scientific = FALSE, trim = TRUE)
    }
  )
}

.ht_alt_symbol <- function(alternative) {
  switch(alternative,
         less = "<",
         greater = ">",
         two.sided = "!=")
}

.ht_warn_ignored_arg <- function(fun, arg, reason = "ignored in this branch.") {
  warning(fun, ": argument '", arg, "' is ", reason, call. = FALSE)
}

.ht_build_region <- function(stat_name,
                             alternative,
                             crit,
                             estimate_crit = NULL,
                             estimate_label = NULL,
                             corrected = FALSE,
                             exact = FALSE,
                             formatter = .ht_internal_format_num) {
  if (exact) {
    return("Discrete exact test; decision is based on the p-value.")
  }

  stat_display <- if (corrected && identical(toupper(stat_name), "Z")) "Z_cc" else stat_name
  f <- formatter

  if (alternative == "two.sided" &&
      stat_name %in% c("Chi^2", "chi^2", "F")) {
    stop(
      ".ht_build_region(): two-sided ", stat_name,
      " procedures must use .ht_build_region_nonsymmetric()."
    )
  }

  if (alternative == "two.sided") {
    crit_val <- if (length(crit) == 2L) max(abs(crit)) else abs(crit)

    if (!is.null(estimate_crit) &&
        length(estimate_crit) == 2L &&
        all(is.finite(estimate_crit)) &&
        !is.null(estimate_label)) {
      return(
        paste0(
          "|", stat_display, "| > ", f(crit_val),
          "  (equiv: ", estimate_label, " < ", f(estimate_crit[1L]),
          " or ", estimate_label, " > ", f(estimate_crit[2L]), ")"
        )
      )
    }

    return(paste0("|", stat_display, "| > ", f(crit_val)))
  }

  direction <- if (alternative == "less") "< " else "> "
  crit_val <- if (length(crit) == 1L) crit else if (alternative == "less") crit[1L] else crit[length(crit)]

  if (!is.null(estimate_crit) &&
      length(estimate_crit) == 1L &&
      is.finite(estimate_crit) &&
      !is.null(estimate_label)) {
    return(
      paste0(
        stat_display, " ", direction, f(crit_val),
        "  (equiv: ", estimate_label, " ", direction, f(estimate_crit), ")"
      )
    )
  }

  paste0(stat_display, " ", direction, f(crit_val))
}

.new_htest_result <- function(class_name,
                              method,
                              parameter,
                              alternative,
                              alpha,
                              null,
                              estimate,
                              statistic,
                              stat_name,
                              p_value,
                              assumptions = character(),
                              inputs = list(),
                              ...) {
  reject <- p_value < alpha

  out <- c(
    list(
      method = method,
      parameter = parameter,
      alternative = alternative,
      alpha = alpha,
      null = null,
      estimate = estimate,
      statistic = statistic,
      stat_name = stat_name,
      p_value = p_value,
      reject = reject,
      decision = if (reject) "REJECT H0" else "FAIL TO REJECT H0",
      assumptions = assumptions,
      inputs = inputs
    ),
    list(...)
  )

  class(out) <- c(class_name, "htest_result", "list")
  out
}

.ht_prop_adequacy <- function(pooled, x1, n1, x2, n2, p_pool) {
  if (pooled) {
    values <- c(
      n1p = n1 * p_pool,
      n1q = n1 * (1 - p_pool),
      n2p = n2 * p_pool,
      n2q = n2 * (1 - p_pool)
    )
  } else {
    values <- c(
      x1 = x1,
      n1_minus_x1 = n1 - x1,
      x2 = x2,
      n2_minus_x2 = n2 - x2
    )
  }

  list(
    ok = all(values >= 5),
    values = values
  )
}

.ht_warn_prop_adequacy <- function(ok, pooled, fun) {
  if (!ok) {
    if (pooled) {
      warning(
        fun,
        ": normal approximation may be unreliable because some pooled expected counts are < 5.",
        call. = FALSE
      )
    } else {
      warning(
        fun,
        ": normal approximation may be unreliable because some observed counts are < 5.",
        call. = FALSE
      )
    }
  }
}
.ht_resolve_two_prop_options <- function(delta0, pooled, continuity, fun) {
  pooled <- .ht_validate_optional_flag(pooled, "pooled", fun)
  continuity <- .ci_validate_flag(continuity, "continuity", fun)

  if (is.null(pooled)) {
    pooled <- isTRUE(all.equal(delta0, 0))
  }

  if (!isTRUE(all.equal(delta0, 0)) && isTRUE(pooled)) {
    stop(fun, ": pooled = TRUE is only supported when p0 = 0 in the two-sample case.")
  }

  list(pooled = pooled, continuity = continuity)
}

.ht_two_prop_method_suffix <- function(delta0, pooled, continuity) {
  suffix <- if (isTRUE(pooled)) {
    "standard pooled test of equality"
  } else if (isTRUE(all.equal(delta0, 0))) {
    "unpooled large-sample approximation"
  } else {
    "large-sample approximation for nonzero null difference"
  }

  if (isTRUE(continuity)) {
    paste0(suffix, "; continuity corrected")
  } else {
    suffix
  }
}