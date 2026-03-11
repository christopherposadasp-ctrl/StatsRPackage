# Internal helpers for width planning -------------------------------------

.new_width_result <- function(class_name,
                              method,
                              conf.level,
                              alpha,
                              n,
                              target_full_width,
                              target_half_width,
                              achieved_full_width,
                              achieved_half_width,
                              assumptions = character(),
                              inputs = list(),
                              ...) {
  out <- c(
    list(
      method = method,
      conf.level = conf.level,
      alpha = alpha,
      n = n,
      width_symbol = "w",
      width_definition = "total CI width",
      half_width_symbol = "E",
      half_width_definition = "CI half-width",
      target_full_width = target_full_width,
      target_half_width = target_half_width,
      achieved_full_width = achieved_full_width,
      achieved_half_width = achieved_half_width,
      assumptions = assumptions,
      inputs = inputs
    ),
    list(...)
  )
  class(out) <- c(class_name, "width_plan_result", "list")
  out
}

.width_validate_positive_scalar <- function(x, arg, fun) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x) ||
      x <= 0) {
    stop(fun, ": ", arg, " must be a single positive finite number.")
  }
  x
}

.width_validate_probability_scalar <- function(x, arg, fun) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x) ||
      x <= 0 ||
      x >= 1) {
    stop(fun, ": ", arg, " must be a single number strictly between 0 and 1.")
  }
  x
}

.width_validate_integer_scalar <- function(x, arg, fun, min_value = 1L) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x) ||
      !.ci_is_whole_number(x) ||
      x < min_value) {
    stop(fun, ": ", arg, " must be a single integer >= ", min_value, ".")
  }
  as.integer(round(x))
}

.width_warn_p_ignored <- function(fun) {
  warning(fun, ": p is ignored when worst_case = TRUE; using p = 0.5.", call. = FALSE)
}

.width_warn_wald_adequacy <- function(n, p_used, fun) {
  if (n * p_used < 5 || n * (1 - p_used) < 5) {
    warning(
      fun,
      ": Wald planning result may be unreliable because n * p_used or n * (1 - p_used) is < 5.",
      call. = FALSE
    )
  }
}
