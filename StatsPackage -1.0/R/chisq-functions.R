#' Chapter 14 chi-square and contingency-table functions
#'
#' `chisq_gof_probs()` performs a one-way chi-square goodness-of-fit test to
#' specified category probabilities.
#'
#' `chisq_gof_dist()` performs a chi-square goodness-of-fit test to a
#' distribution, supporting at least Poisson, normal, and exponential models.
#'
#' `chisq_table()` performs a chi-square test for independence or homogeneity
#' in an r x c contingency table.
#'
#' `table_props()` computes descriptive row, column, or overall proportions
#' for a contingency table.
#'
#' @param observed Observed counts. For `chisq_gof_probs()` and grouped
#'   `chisq_gof_dist()` inputs, use a nonnegative integer vector. For
#'   `chisq_table()` and `table_props()`, use a nonnegative two-way table or
#'   matrix.
#' @param p Probabilities under the null for `chisq_gof_probs()`. If `NULL`,
#'   equal probabilities are assumed.
#' @param labels Optional category labels for one-way grouped inputs.
#' @param min_expected Minimum expected count threshold used for warnings and,
#'   where implemented, class combining.
#' @param alpha Significance level in (0, 1).
#' @param digits Integer number of decimal places used only for printed output.
#' @param quiet Logical; if `TRUE`, suppress printed output.
#' @param x Raw sample data for `chisq_gof_dist()`.
#' @param dist Distribution for `chisq_gof_dist()`: `"pois"`, `"norm"`, or
#'   `"exp"`.
#' @param k Number of classes used for raw-data GOF tests when `breaks` is not
#'   supplied.
#' @param breaks Class boundaries for grouped `chisq_gof_dist()` input.
#' @param params Named list of distribution parameters when parameters are not
#'   estimated from raw data.
#' @param estimate Logical; if `TRUE`, estimate parameters from raw data where
#'   applicable.
#' @param params_estimated Logical; for grouped GOF inputs, whether supplied
#'   parameters should count against the degrees of freedom.
#' @param type For `chisq_table()`, whether the interpretation is
#'   `"independence"` or `"homogeneity"`.
#' @param correct Logical; if `TRUE`, apply Yates correction in the 2 x 2
#'   case for `chisq_table()`.
#' @param margin For `table_props()`, one of `"row"`, `"col"`, or `"overall"`.
#'
#' @return
#' For the chi-square test functions, a classed `htest_result` object with
#' statistic, df, p-value, critical value, rejection region, decision, and
#' diagnostic outputs such as observed counts, expected counts, contributions,
#' and residuals. For `chisq_table()`, standardized residuals are also returned.
#'
#' For `table_props()`, a classed `table_props_result` object is returned with
#' the original table and the requested proportions.
#'
#' @name chisq_functions
NULL

#' @describeIn chisq_functions One-way chi-square GOF test to specified probabilities.
#' @export
chisq_gof_probs <- function(observed,
                            p = NULL,
                            labels = NULL,
                            min_expected = 5,
                            alpha = 0.05,
                            digits = 4,
                            quiet = FALSE) {
  fun <- "chisq_gof_probs()"
  p_missing <- is.null(p)

  alpha <- .ht_validate_alpha(alpha, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)

  if (!is.numeric(min_expected) || length(min_expected) != 1L ||
      !is.finite(min_expected) || min_expected <= 0) {
    stop(fun, ": min_expected must be a single positive number.")
  }

  observed <- .ht_validate_integer_vector(observed, "observed", fun, min_value = 0L)
  if (sum(observed) <= 0) {
    stop(fun, ": the total observed count must be > 0.")
  }

  labels <- .validate_labels_1way(labels, observed, fun)
  p <- .validate_prob_vector(p, length(observed), fun)

  expected <- .chisq_expected_1way(observed, p)
  .chisq_warn_small_expected(expected, min_expected, fun)

  contrib <- .chisq_contrib(observed, expected)
  residuals <- .chisq_residuals(observed, expected)
  phat <- observed / sum(observed)

  chi_stat <- sum(contrib)
  df <- length(observed) - 1L
  p_val <- stats::pchisq(chi_stat, df = df, lower.tail = FALSE)

  region_obj <- .format_chisq_region(alpha, df)
  chi_crit <- region_obj$crit
  reject_region <- region_obj$region

  out <- .new_htest_result(
    class_name = "chisq_gof_probs_result",
    method = if (p_missing) {
      "Chi-square GOF test for equal category probabilities"
    } else {
      "Chi-square GOF test for specified category probabilities"
    },
    parameter = "category probabilities",
    alternative = "greater",
    alpha = alpha,
    null = stats::setNames(p, labels),
    estimate = stats::setNames(phat, labels),
    statistic = chi_stat,
    stat_name = "chi^2",
    p_value = p_val,
    assumptions = c(
      "Observations are independent",
      "Expected counts should generally be at least 5"
    ),
    inputs = list(
      observed = observed,
      p = p,
      labels = labels,
      min_expected = min_expected,
      alpha = alpha
    ),
    observed = stats::setNames(observed, labels),
    expected = stats::setNames(expected, labels),
    contrib = stats::setNames(contrib, labels),
    residuals = stats::setNames(residuals, labels),
    std_residuals = NULL,
    labels = labels,
    df = df,
    chi_stat = chi_stat,
    chi_crit = chi_crit,
    crit = chi_crit,
    crit_label = "chi-square critical value",
    reject_region = reject_region,
    region = reject_region
  )

  if (!quiet) {
    fmt <- function(y) .ci_format_num(y, digits)
    reject_region_print <- .format_chisq_region(alpha, df, formatter = fmt)$region

    cat("\n", out$method, "\n", sep = "")
    cat("H0: category probabilities equal ",
        if (p_missing) "equal probabilities" else "the specified probabilities",
        "\n", sep = "")
    cat("alpha = ", fmt(alpha), "   df = ", df, "\n", sep = "")
    cat("Test statistic: chi^2 = ", fmt(chi_stat), "\n", sep = "")
    cat("p-value = ", sprintf("%.6f", p_val), "\n", sep = "")
    cat("Rejection region: ", reject_region_print, "\n", sep = "")
    cat("Decision: ", out$decision, "\n", sep = "")

    tab <- data.frame(
      Category = labels,
      Observed = as.numeric(observed),
      Expected = as.numeric(round(expected, digits)),
      Contribution = as.numeric(round(contrib, digits)),
      Residual = as.numeric(round(residuals, digits))
    )
    cat("\nObserved vs expected:\n")
    print(tab, row.names = FALSE)
  }

  invisible(out)
}

#' @describeIn chisq_functions Chi-square GOF test to a distribution.
#' @export
chisq_gof_dist <- function(x = NULL,
                           observed = NULL,
                           dist = c("exp", "norm", "pois"),
                           k = NULL,
                           breaks = NULL,
                           params = NULL,
                           estimate = FALSE,
                           params_estimated = FALSE,
                           min_expected = 5,
                           alpha = 0.05,
                           digits = 4,
                           quiet = FALSE) {
  fun <- "chisq_gof_dist()"

  alpha <- .ht_validate_alpha(alpha, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  dist <- match.arg(dist)

  if (!is.numeric(min_expected) || length(min_expected) != 1L ||
      !is.finite(min_expected) || min_expected <= 0) {
    stop(fun, ": min_expected must be a single positive number.")
  }

  if (is.null(x) && is.null(observed)) {
    stop(fun, ": provide either x (raw data) or observed (grouped counts).")
  }
  if (!is.null(x) && !is.null(observed)) {
    stop(fun, ": provide only one of x or observed, not both.")
  }

  raw <- !is.null(x)
  note <- character()
  used_params <- list()
  breaks_used <- NULL
  initial_observed <- NULL
  initial_expected <- NULL
  initial_labels <- NULL
  group_map <- NULL
  combined <- FALSE

  # ---- continuous distributions: exp / norm
  if (dist %in% c("exp", "norm")) {
    if (raw) {
      x <- as.numeric(x)
      x <- x[is.finite(x)]
      n <- length(x)
      if (n < 1L) stop(fun, ": x must contain at least one finite value.")

      if (is.null(k)) k <- 5L
      k <- as.integer(k)
      if (!is.finite(k) || k < 4L) stop(fun, ": k must be an integer >= 4.")
    } else {
      observed <- .ht_validate_integer_vector(observed, "observed", fun, min_value = 0L)
      if (sum(observed) <= 0) stop(fun, ": the total observed count must be > 0.")
      if (is.null(breaks)) stop(fun, ": grouped continuous inputs require breaks.")
      breaks <- as.numeric(breaks)
      if (length(breaks) != length(observed) + 1L) {
        stop(fun, ": length(breaks) must equal length(observed) + 1.")
      }
      n <- sum(observed)
    }

    if (dist == "exp") {
      if (raw && estimate) {
        rate <- 1 / mean(x)
      } else {
        if (is.null(params)) stop(fun, ": for dist = 'exp', provide params = list(rate = ...) when estimate = FALSE.")
        rate <- params$rate
        if (is.null(rate)) rate <- params$lambda
        if (is.null(rate)) stop(fun, ": for dist = 'exp', params must include rate (or lambda).")
      }
      if (!is.finite(rate) || rate <= 0) stop(fun, ": exponential rate must be > 0.")
      used_params <- list(rate = rate)
      m_est <- if (estimate || params_estimated) 1L else 0L

      if (raw) {
        if (any(x < 0)) {
          note <- c(note, "Exp model assumes x >= 0; negative values were retained in the raw data.")
        }

        if (is.null(breaks)) {
          probs <- (1:(k - 1L)) / k
          cuts <- stats::qexp(probs, rate = rate)
          breaks <- c(0, cuts, Inf)
        } else {
          breaks <- as.numeric(breaks)
        }

        f <- cut(x, breaks = breaks, right = FALSE, include.lowest = TRUE)
        obs0 <- as.numeric(table(f))
        labels0 <- levels(f)
        exp0 <- n * diff(stats::pexp(breaks, rate = rate))
      } else {
        obs0 <- observed
        labels0 <- .chisq_make_interval_labels(breaks)
        exp0 <- n * diff(stats::pexp(breaks, rate = rate))
      }

      breaks_used <- breaks
      method <- if (raw) {
        "Chi-square GOF test: Exponential distribution (raw data)"
      } else {
        "Chi-square GOF test: Exponential distribution (grouped counts)"
      }
    } else {
      if (raw && estimate) {
        mu <- mean(x)
        sigma <- stats::sd(x)
      } else {
        if (is.null(params)) stop(fun, ": for dist = 'norm', provide params = list(mean = ..., sd = ...) when estimate = FALSE.")
        mu <- params$mean
        if (is.null(mu)) mu <- params$mu
        sigma <- params$sd
        if (is.null(sigma)) sigma <- params$sigma
        if (is.null(mu) || is.null(sigma)) {
          stop(fun, ": for dist = 'norm', params must include mean/mu and sd/sigma.")
        }
      }
      if (!is.finite(mu)) stop(fun, ": normal mean must be finite.")
      if (!is.finite(sigma) || sigma <= 0) stop(fun, ": normal sd must be > 0.")

      used_params <- list(mean = mu, sd = sigma)
      m_est <- if (estimate || params_estimated) 2L else 0L

      if (raw) {
        if (is.null(breaks)) {
          probs <- (1:(k - 1L)) / k
          cuts <- stats::qnorm(probs, mean = mu, sd = sigma)
          breaks <- c(-Inf, cuts, Inf)
        } else {
          breaks <- as.numeric(breaks)
        }

        f <- cut(x, breaks = breaks, right = FALSE, include.lowest = TRUE)
        obs0 <- as.numeric(table(f))
        labels0 <- levels(f)
        exp0 <- n * diff(stats::pnorm(breaks, mean = mu, sd = sigma))
      } else {
        obs0 <- observed
        labels0 <- .chisq_make_interval_labels(breaks)
        exp0 <- n * diff(stats::pnorm(breaks, mean = mu, sd = sigma))
      }

      breaks_used <- breaks
      method <- if (raw) {
        "Chi-square GOF test: Normal distribution (raw data)"
      } else {
        "Chi-square GOF test: Normal distribution (grouped counts)"
      }
    }

    initial_observed <- obs0
    initial_expected <- exp0
    initial_labels <- labels0

    if (any(exp0 < min_expected)) {
      cmb <- .combine_sparse_adjacent(obs0, exp0, labels0, min_expected = min_expected)
      obs <- cmb$observed
      exp <- cmb$expected
      labels <- cmb$labels
      group_map <- cmb$group_map
      combined <- cmb$combined

      if (combined) {
        note <- c(note, "Adjacent classes were combined to improve expected counts.")
      }
      if (!cmb$all_expected_ok) {
        .chisq_warn_small_expected(exp, min_expected, fun)
      }
    } else {
      obs <- obs0
      exp <- exp0
      labels <- labels0
      group_map <- as.list(labels0)
    }
  }

  # ---- Poisson
  if (dist == "pois") {
    if (raw) {
      x <- as.numeric(x)
      x <- x[is.finite(x)]
      if (length(x) < 1L) stop(fun, ": x must contain at least one finite value.")
      if (any(x < 0) || any(abs(x - round(x)) > 1e-8)) {
        stop(fun, ": for Poisson GOF, x must be nonnegative integers.")
      }
      x <- round(x)
      n <- length(x)
      max_val <- max(x)
      obs_vals <- tabulate(x + 1L, nbins = max_val + 1L)
    } else {
      obs_vals <- .ht_validate_integer_vector(observed, "observed", fun, min_value = 0L)
      if (sum(obs_vals) <= 0) stop(fun, ": the total observed count must be > 0.")
      n <- sum(obs_vals)
      max_val <- length(obs_vals) - 1L
    }

    values <- 0:max_val

    if (estimate) {
      lambda <- sum(values * obs_vals) / n
    } else {
      if (is.null(params)) stop(fun, ": for dist = 'pois', provide params = list(lambda = ...) when estimate = FALSE.")
      lambda <- params$lambda
      if (is.null(lambda)) lambda <- params$mean
      if (is.null(lambda)) stop(fun, ": for dist = 'pois', params must include lambda (or mean).")
    }
    if (!is.finite(lambda) || lambda <= 0) stop(fun, ": Poisson lambda must be > 0.")

    used_params <- list(lambda = lambda)
    m_est <- if (estimate || params_estimated) 1L else 0L
    method <- "Chi-square GOF test: Poisson distribution"

    cmb <- .combine_pois_right_tail(obs_vals, lambda = lambda, n = n, min_expected = min_expected)
    obs <- cmb$observed
    exp <- cmb$expected
    labels <- cmb$labels
    group_map <- cmb$group_map
    combined <- cmb$combined

    initial_observed <- obs_vals
    initial_expected <- c(n * stats::dpois(0:max_val, lambda = lambda))
    initial_labels <- as.character(0:max_val)

    if (combined) {
      note <- c(note, paste0("Right-tail classes were combined into the category ", utils::tail(labels, 1L), "."))
    }
    if (!cmb$all_expected_ok) {
      .chisq_warn_small_expected(exp, min_expected, fun)
    }
  }

  if (any(exp <= 0)) {
    stop(fun, ": nonpositive expected counts encountered.")
  }

  contrib <- .chisq_contrib(obs, exp)
  residuals <- .chisq_residuals(obs, exp)
  chi_stat <- sum(contrib)
  df <- length(obs) - 1L - m_est

  if (df <= 0) {
    stop(fun, ": df <= 0 after adjustment; check the number of classes and estimated parameters.")
  }

  p_val <- stats::pchisq(chi_stat, df = df, lower.tail = FALSE)
  region_obj <- .format_chisq_region(alpha, df)
  chi_crit <- region_obj$crit
  reject_region <- region_obj$region

  out <- .new_htest_result(
    class_name = "chisq_gof_dist_result",
    method = method,
    parameter = "distribution",
    alternative = "greater",
    alpha = alpha,
    null = list(dist = dist, params = used_params),
    estimate = used_params,
    statistic = chi_stat,
    stat_name = "chi^2",
    p_value = p_val,
    assumptions = c(
      "Observations are independent",
      "Expected counts should generally be at least 5",
      "Degrees of freedom are adjusted for estimated parameters"
    ),
    inputs = list(
      x = if (raw) x else NULL,
      observed = if (!raw) observed else NULL,
      dist = dist,
      k = k,
      breaks = breaks,
      params = params,
      estimate = estimate,
      params_estimated = params_estimated,
      min_expected = min_expected,
      alpha = alpha
    ),
    dist = dist,
    n = n,
    df = df,
    chi_stat = chi_stat,
    chi_crit = chi_crit,
    crit = chi_crit,
    crit_label = "chi-square critical value",
    reject_region = reject_region,
    region = reject_region,
    observed = stats::setNames(obs, labels),
    expected = stats::setNames(exp, labels),
    contrib = stats::setNames(contrib, labels),
    residuals = stats::setNames(residuals, labels),
    std_residuals = NULL,
    labels = labels,
    breaks = breaks_used,
    params = used_params,
    params_estimated_n = m_est,
    initial_observed = initial_observed,
    initial_expected = initial_expected,
    initial_labels = initial_labels,
    group_map = group_map,
    combined = combined,
    note = if (length(note)) paste(note, collapse = " ") else NULL
  )

  if (!quiet) {
    fmt <- function(y) .ci_format_num(y, digits)
    reject_region_print <- .format_chisq_region(alpha, df, formatter = fmt)$region

    cat("\n", out$method, "\n", sep = "")
    cat("Model: ", dist, "\n", sep = "")
    if (length(used_params)) {
      cat("Parameters: ",
          paste(
            paste0(names(used_params), " = ", vapply(used_params, fmt, character(1L))),
            collapse = ", "
          ),
          "\n", sep = "")
    }
    if (!is.null(out$note)) {
      cat("Note: ", out$note, "\n", sep = "")
    }
    cat("alpha = ", fmt(alpha), "   df = ", df, "\n", sep = "")
    cat("Test statistic: chi^2 = ", fmt(chi_stat), "\n", sep = "")
    cat("p-value = ", sprintf("%.6f", p_val), "\n", sep = "")
    cat("Rejection region: ", reject_region_print, "\n", sep = "")
    cat("Decision: ", out$decision, "\n", sep = "")

    tab <- data.frame(
      Category = labels,
      Observed = as.numeric(obs),
      Expected = as.numeric(round(exp, digits)),
      Contribution = as.numeric(round(contrib, digits)),
      Residual = as.numeric(round(residuals, digits))
    )
    cat("\nObserved vs expected:\n")
    print(tab, row.names = FALSE)
  }

  invisible(out)
}

#' @describeIn chisq_functions Chi-square test for a contingency table.
#' @export
chisq_table <- function(observed,
                        type = c("independence", "homogeneity"),
                        correct = FALSE,
                        min_expected = 5,
                        alpha = 0.05,
                        digits = 4,
                        quiet = FALSE) {
  fun <- "chisq_table()"

  alpha <- .ht_validate_alpha(alpha, fun)
  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  type <- match.arg(type)
  correct <- .ci_validate_flag(correct, "correct", fun)

  if (!is.numeric(min_expected) || length(min_expected) != 1L ||
      !is.finite(min_expected) || min_expected <= 0) {
    stop(fun, ": min_expected must be a single positive number.")
  }

  observed <- .validate_table_input(observed, fun)

  if (correct && !(nrow(observed) == 2L && ncol(observed) == 2L)) {
    warning(fun, ": argument 'correct' is ignored unless the table is 2 x 2.", call. = FALSE)
    correct <- FALSE
  }

  expected <- .chisq_expected_table(observed)
  .chisq_warn_small_expected(expected, min_expected, fun)

  contrib <- .chisq_contrib(observed, expected)
  residuals <- .chisq_residuals(observed, expected)
  std_residuals <- .chisq_std_residuals(observed, expected)

  if (correct) {
    chi_stat <- sum((pmax(abs(observed - expected) - 0.5, 0)^2) / expected)
  } else {
    chi_stat <- sum(contrib)
  }

  df <- (nrow(observed) - 1L) * (ncol(observed) - 1L)
  p_val <- stats::pchisq(chi_stat, df = df, lower.tail = FALSE)

  region_obj <- .format_chisq_region(alpha, df)
  chi_crit <- region_obj$crit
  reject_region <- region_obj$region

  null_text <- if (type == "independence") {
    "the row and column classifications are independent"
  } else {
    "the population distributions are homogeneous"
  }

  method <- if (correct) {
    paste0("Chi-square test for ", type, " (Yates-corrected 2 x 2 table)")
  } else {
    paste0("Chi-square test for ", type)
  }

  out <- .new_htest_result(
    class_name = "chisq_table_result",
    method = method,
    parameter = paste0(type, " in contingency table"),
    alternative = "greater",
    alpha = alpha,
    null = null_text,
    estimate = NULL,
    statistic = chi_stat,
    stat_name = "chi^2",
    p_value = p_val,
    assumptions = c(
      "Observations are independent",
      "Expected cell counts should generally be at least 5"
    ),
    inputs = list(
      observed = observed,
      type = type,
      correct = correct,
      min_expected = min_expected,
      alpha = alpha
    ),
    observed = observed,
    expected = expected,
    contrib = contrib,
    residuals = residuals,
    std_residuals = std_residuals,
    row_totals = rowSums(observed),
    col_totals = colSums(observed),
    grand_total = sum(observed),
    df = df,
    chi_stat = chi_stat,
    chi_crit = chi_crit,
    crit = chi_crit,
    crit_label = "chi-square critical value",
    correct = correct,
    reject_region = reject_region,
    region = reject_region
  )

  if (!quiet) {
    fmt <- function(y) .ci_format_num(y, digits)
    reject_region_print <- .format_chisq_region(alpha, df, formatter = fmt)$region

    cat("\n", out$method, "\n", sep = "")
    cat("H0: ", null_text, "\n", sep = "")
    cat("alpha = ", fmt(alpha), "   df = ", df, "\n", sep = "")
    cat("Test statistic: chi^2 = ", fmt(chi_stat), "\n", sep = "")
    cat("p-value = ", sprintf("%.6f", p_val), "\n", sep = "")
    cat("Rejection region: ", reject_region_print, "\n", sep = "")
    cat("Decision: ", out$decision, "\n", sep = "")

    cat("\nObserved counts:\n")
    print(observed)

    cat("\nExpected counts:\n")
    print(round(expected, digits))
  }

  invisible(out)
}

#' @describeIn chisq_functions Descriptive proportions for a contingency table.
#' @export
table_props <- function(observed,
                        margin = c("row", "col", "overall"),
                        digits = 4,
                        quiet = FALSE) {
  fun <- "table_props()"

  digits <- .ci_validate_digits(digits, fun)
  quiet <- .ci_validate_flag(quiet, "quiet", fun)
  margin <- match.arg(margin)

  observed <- .validate_table_input(observed, fun)

  proportions <- switch(
    margin,
    row = prop.table(observed, margin = 1L),
    col = prop.table(observed, margin = 2L),
    overall = prop.table(observed)
  )

  method <- switch(
    margin,
    row = "Row proportions for contingency table",
    col = "Column proportions for contingency table",
    overall = "Overall proportions for contingency table"
  )

  out <- .new_table_props_result(
    class_name = "table_props_result",
    method = method,
    margin = margin,
    observed = observed,
    proportions = proportions,
    inputs = list(
      observed = observed,
      margin = margin
    ),
    row_totals = rowSums(observed),
    col_totals = colSums(observed),
    grand_total = sum(observed)
  )

  if (!quiet) {
    cat("\n", out$method, "\n", sep = "")
    cat("Displayed with ", digits, " decimal places.\n", sep = "")
    print(round(proportions, digits))
  }

  invisible(out)
}