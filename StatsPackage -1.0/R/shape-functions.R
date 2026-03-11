#' Descriptive shape functions
#'
#' Computes skewness and kurtosis measures with optional small-sample
#' adjustments and plain-language interpretations.
#'
#' These functions return classed result objects with unrounded stored values
#' and optional printed summaries controlled by `digits` and `quiet`.
#'
#' @param x Numeric sample vector.
#' @param method Estimator choice for the shape measure.
#' @param na_rm Logical; if `TRUE`, remove missing values before computing the measure.
#' @param digits Integer number of decimal places used only for printed output.
#' @param quiet Logical; if `TRUE`, suppress printed output.
#' @param excess Logical; for `kurt()`, whether to report excess kurtosis instead of Pearson kurtosis.
#' @name shape_functions
NULL

# Descriptive shape helpers -------------------------------------------------

.new_shape_result <- function(class_name,
                              method,
                              parameter,
                              estimate,
                              benchmark,
                              interpretation,
                              assumptions = character(),
                              inputs = list(),
                              ...) {
  out <- c(
    list(
      method = method,
      parameter = parameter,
      estimate = estimate,
      benchmark = benchmark,
      interpretation = interpretation,
      assumptions = assumptions,
      inputs = inputs
    ),
    list(...)
  )

  class(out) <- c(class_name, "shape_result", "list")
  out
}

.shape_validate_numeric_sample <- function(x,
                                           arg,
                                           fun,
                                           na_rm = FALSE,
                                           min_n = 2L) {
  if (!is.numeric(x) || length(x) == 0L) {
    stop(fun, ": ", arg, " must be a non-empty numeric vector.")
  }

  if (anyNA(x)) {
    if (!na_rm) {
      stop(fun, ": ", arg, " contains missing values; set na_rm = TRUE to remove them.")
    }
    x <- x[!is.na(x)]
  }

  if (length(x) < min_n) {
    stop(fun, ": ", arg, " must contain at least ", min_n, " non-missing observations.")
  }

  if (any(!is.finite(x))) {
    stop(fun, ": ", arg, " must contain only finite values.")
  }

  x
}

.shape_sample_moments <- function(x, fun) {
  n <- length(x)
  xbar <- mean(x)
  d <- x - xbar
  m2 <- mean(d^2)

  if (!is.finite(m2) || m2 <= 0) {
    stop(fun, ": skewness and kurtosis are undefined when all observations are identical.")
  }

  list(
    n = n,
    mean = xbar,
    m2 = m2,
    m3 = mean(d^3),
    m4 = mean(d^4)
  )
}

.shape_interpret_skew <- function(estimate) {
  tol <- sqrt(.Machine$double.eps)

  if (estimate > tol) {
    return(list(
      direction = "right-skewed",
      interpretation = paste(
        "Positive skewness indicates a longer or heavier right tail.",
        "Values farther from 0 indicate greater asymmetry."
      )
    ))
  }

  if (estimate < -tol) {
    return(list(
      direction = "left-skewed",
      interpretation = paste(
        "Negative skewness indicates a longer or heavier left tail.",
        "Values farther from 0 indicate greater asymmetry."
      )
    ))
  }

  list(
    direction = "approximately symmetric",
    interpretation = paste(
      "Skewness near 0 indicates an approximately symmetric distribution.",
      "When skewness is not near 0, the sign indicates the direction of the longer tail."
    )
  )
}

.shape_interpret_kurt <- function(estimate, benchmark, excess = FALSE) {
  tol <- sqrt(.Machine$double.eps)

  if (estimate > benchmark + tol) {
    if (excess) {
      return(list(
        shape = "leptokurtic",
        interpretation = paste(
          "Positive excess kurtosis indicates heavier tails and a sharper peak than a normal distribution.",
          "Larger positive values indicate more tail weight relative to normal."
        )
      ))
    }

    return(list(
      shape = "leptokurtic",
      interpretation = paste(
        "Kurtosis above 3 indicates heavier tails and a sharper peak than a normal distribution.",
        "Larger values indicate more tail weight relative to normal."
      )
    ))
  }

  if (estimate < benchmark - tol) {
    if (excess) {
      return(list(
        shape = "platykurtic",
        interpretation = paste(
          "Negative excess kurtosis indicates lighter tails and a flatter shape than a normal distribution.",
          "More negative values indicate less tail weight relative to normal."
        )
      ))
    }

    return(list(
      shape = "platykurtic",
      interpretation = paste(
        "Kurtosis below 3 indicates lighter tails and a flatter shape than a normal distribution.",
        "Smaller values indicate less tail weight relative to normal."
      )
    ))
  }

  if (excess) {
    return(list(
      shape = "mesokurtic",
      interpretation = "Excess kurtosis near 0 indicates tail weight broadly similar to a normal distribution."
    ))
  }

  list(
    shape = "mesokurtic",
    interpretation = "Kurtosis near 3 indicates tail weight broadly similar to a normal distribution."
  )
}

# Descriptive shape functions -----------------------------------------------

#' @describeIn shape_functions Sample skewness measure and interpretation.
#' @export
skew <- function(x,
                 method = c("moment", "adjusted"),
                 na_rm = FALSE,
                 digits = 4,
                 quiet = FALSE) {
  fun <- "skew()"

  digits <- .ci_validate_digits(digits, fun)
  na_rm <- .ci_validate_flag(na_rm, "na_rm", fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  method <- match.arg(method)

  min_n <- if (method == "adjusted") 3L else 2L
  x <- .shape_validate_numeric_sample(x, "x", fun, na_rm = na_rm, min_n = min_n)

  moms <- .shape_sample_moments(x, fun)
  g1 <- moms$m3 / (moms$m2^(3 / 2))

  estimate <- if (method == "moment") {
    g1
  } else {
    sqrt(moms$n * (moms$n - 1)) / (moms$n - 2) * g1
  }

  interp <- .shape_interpret_skew(estimate)

  out <- .new_shape_result(
    class_name = "skew_result",
    method = paste0("Sample skewness (", method, ")"),
    parameter = "skewness",
    estimate = estimate,
    benchmark = 0,
    interpretation = interp$interpretation,
    assumptions = c(
      "x is a numeric sample of finite observations",
      "Skewness is a descriptive shape summary, not an inferential procedure",
      "Skewness is undefined when all observations are identical"
    ),
    inputs = list(method = method, na_rm = na_rm),
    n = moms$n,
    mean = moms$mean,
    m2 = moms$m2,
    m3 = moms$m3,
    estimator = if (method == "moment") {
      "g1 = m3 / m2^(3/2), using sample central moments with divisor n"
    } else {
      "G1 = sqrt(n(n - 1)) / (n - 2) * g1"
    },
    direction = interp$direction,
    reference = "0 indicates symmetry; positive values indicate right-skewness and negative values indicate left-skewness."
  )

  if (!quiet) {
    fmt <- function(y) .ci_format_num(y, digits)

    cat("\n", out$method, "\n", sep = "")
    cat("n = ", out$n, "\n", sep = "")
    cat("Estimate (skewness) = ", fmt(out$estimate), "\n", sep = "")
    cat("Reference value = ", fmt(out$benchmark), "\n", sep = "")
    cat("Direction = ", out$direction, "\n", sep = "")
    cat("Meaning: ", out$interpretation, "\n", sep = "")
  }

  invisible(out)
}

#' @describeIn shape_functions Sample kurtosis measure and interpretation.
#' @export
kurt <- function(x,
                 method = c("moment", "adjusted"),
                 excess = FALSE,
                 na_rm = FALSE,
                 digits = 4,
                 quiet = FALSE) {
  fun <- "kurt()"

  digits <- .ci_validate_digits(digits, fun)
  excess <- .ci_validate_flag(excess, "excess", fun)
  na_rm <- .ci_validate_flag(na_rm, "na_rm", fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  method <- match.arg(method)

  min_n <- if (method == "adjusted") 4L else 2L
  x <- .shape_validate_numeric_sample(x, "x", fun, na_rm = na_rm, min_n = min_n)

  moms <- .shape_sample_moments(x, fun)
  g2_pearson <- moms$m4 / (moms$m2^2)
  g2_excess <- g2_pearson - 3

  if (method == "moment") {
    estimate <- if (excess) g2_excess else g2_pearson
  } else {
    adjusted_excess <- ((moms$n - 1) / ((moms$n - 2) * (moms$n - 3))) *
      ((moms$n + 1) * g2_excess + 6)
    estimate <- if (excess) adjusted_excess else adjusted_excess + 3
  }

  benchmark <- if (excess) 0 else 3
  interp <- .shape_interpret_kurt(estimate, benchmark = benchmark, excess = excess)

  out <- .new_shape_result(
    class_name = "kurt_result",
    method = paste0(
      "Sample ",
      if (excess) "excess kurtosis" else "kurtosis",
      " (", method, ")"
    ),
    parameter = if (excess) "excess kurtosis" else "kurtosis",
    estimate = estimate,
    benchmark = benchmark,
    interpretation = interp$interpretation,
    assumptions = c(
      "x is a numeric sample of finite observations",
      "Kurtosis is a descriptive shape summary, not an inferential procedure",
      "Kurtosis is undefined when all observations are identical"
    ),
    inputs = list(method = method, excess = excess, na_rm = na_rm),
    n = moms$n,
    mean = moms$mean,
    m2 = moms$m2,
    m4 = moms$m4,
    estimator = if (method == "moment") {
      if (excess) {
        "g2 = m4 / m2^2 - 3, using sample central moments with divisor n"
      } else {
        "b2 = m4 / m2^2, using sample central moments with divisor n"
      }
    } else {
      if (excess) {
        "Adjusted excess kurtosis = ((n - 1) / ((n - 2)(n - 3))) * ((n + 1) * g2 + 6)"
      } else {
        "Adjusted Pearson kurtosis = adjusted excess kurtosis + 3"
      }
    },
    shape = interp$shape,
    reference = if (excess) {
      "0 indicates tail weight similar to normal; positive values indicate heavier tails and negative values indicate lighter tails."
    } else {
      "3 indicates tail weight similar to normal; values above 3 indicate heavier tails and values below 3 indicate lighter tails."
    },
    pearson_kurtosis = g2_pearson,
    excess_kurtosis = g2_excess
  )

  if (!quiet) {
    fmt <- function(y) .ci_format_num(y, digits)

    cat("\n", out$method, "\n", sep = "")
    cat("n = ", out$n, "\n", sep = "")
    cat("Estimate (", out$parameter, ") = ", fmt(out$estimate), "\n", sep = "")
    cat("Normal-reference value = ", fmt(out$benchmark), "\n", sep = "")
    cat("Shape label = ", out$shape, "\n", sep = "")
    cat("Meaning: ", out$interpretation, "\n", sep = "")
  }

  invisible(out)
}
