#################power-----
# Helpers for power functions ---------------------------------------------

.new_power_result <- function(class_name,
                              method,
                              parameter,
                              alternative,
                              alpha,
                              null,
                              true_value,
                              power,
                              beta,
                              assumptions = character(),
                              inputs = list(),
                              ...) {
  out <- c(
    list(
      method = method,
      parameter = parameter,
      alternative = alternative,
      alpha = alpha,
      null = null,
      true_value = true_value,
      power = power,
      beta = beta,
      assumptions = assumptions,
      inputs = inputs
    ),
    list(...)
  )
  class(out) <- c(class_name, "power_result", "list")
  out
}

.power_clip_prob <- function(p) {
  pmin(1, pmax(0, p))
}

.power_validate_probability_vector <- function(x, arg, fun) {
  if (!is.numeric(x) ||
      length(x) == 0L ||
      any(!is.finite(x)) ||
      any(x < 0) ||
      any(x > 1)) {
    stop(fun, ": ", arg, " must be numeric values in [0, 1].")
  }
  x
}

.power_validate_positive_numeric_vector <- function(x, arg, fun) {
  if (!is.numeric(x) ||
      length(x) == 0L ||
      any(!is.finite(x)) ||
      any(x <= 0)) {
    stop(fun, ": ", arg, " must be positive finite numeric values.")
  }
  x
}

.power_prob_less_normal <- function(cutoff, mean, sd) {
  if (!is.numeric(sd) || length(sd) != 1L || !is.finite(sd) || sd < 0) {
    stop(".power_prob_less_normal(): sd must be a single finite number >= 0.")
  }
  if (sd == 0) {
    return(as.numeric(mean < cutoff))
  }
  stats::pnorm(cutoff, mean = mean, sd = sd)
}

.power_prob_greater_normal <- function(cutoff, mean, sd) {
  if (!is.numeric(sd) || length(sd) != 1L || !is.finite(sd) || sd < 0) {
    stop(".power_prob_greater_normal(): sd must be a single finite number >= 0.")
  }
  if (sd == 0) {
    return(as.numeric(mean > cutoff))
  }
  1 - stats::pnorm(cutoff, mean = mean, sd = sd)
}

.power_prob_two_sided_normal <- function(lower, upper, mean, sd) {
  if (!is.numeric(sd) || length(sd) != 1L || !is.finite(sd) || sd < 0) {
    stop(".power_prob_two_sided_normal(): sd must be a single finite number >= 0.")
  }
  if (sd == 0) {
    return(as.numeric(mean < lower || mean > upper))
  }
  stats::pnorm(lower, mean = mean, sd = sd) +
    (1 - stats::pnorm(upper, mean = mean, sd = sd))
}

.power_warn_prop_approx <- function(fun, scenario = c("one_sample", "two_sample")) {
  scenario <- match.arg(scenario)
  msg <- if (scenario == "one_sample") {
    "normal approximation may be unreliable because some expected counts are < 5 under the null or alternative."
  } else {
    "normal approximation may be unreliable because some expected counts are < 5."
  }
  warning(fun, ": ", msg, call. = FALSE)
}
