# Internal helpers for Chapter 14 chi-square functions ---------------------

.validate_prob_vector <- function(p, k, fun) {
  if (is.null(p)) {
    return(rep(1 / k, k))
  }

  if (!is.numeric(p) || length(p) != k || any(!is.finite(p)) || any(p < 0)) {
    stop(fun, ": p must be a numeric vector of length ", k, " with nonnegative finite values.")
  }

  s <- sum(p)
  if (!is.finite(s) || s <= 0) {
    stop(fun, ": p must sum to a positive value.")
  }
  if (abs(s - 1) > 1e-8) {
    stop(fun, ": p must sum to 1.")
  }

  p / s
}

.validate_labels_1way <- function(labels, observed, fun) {
  k <- length(observed)

  if (is.null(labels)) {
    nm <- names(observed)
    if (!is.null(nm) && length(nm) == k && all(nzchar(nm))) {
      return(as.character(nm))
    }
    return(as.character(seq_len(k)))
  }

  if (!is.character(labels) || length(labels) != k) {
    stop(fun, ": labels must be a character vector of length ", k, ".")
  }

  labels
}

.validate_table_input <- function(observed, fun) {
  obs <- as.matrix(observed)

  if (length(dim(obs)) != 2L) {
    stop(fun, ": observed must be a two-way table or matrix.")
  }
  if (nrow(obs) < 2L || ncol(obs) < 2L) {
    stop(fun, ": observed must have at least 2 rows and 2 columns.")
  }
  if (!is.numeric(obs) || any(!is.finite(obs)) || any(obs < 0)) {
    stop(fun, ": observed must contain nonnegative finite counts.")
  }
  if (any(abs(obs - round(obs)) > 1e-8)) {
    stop(fun, ": observed must contain whole-number counts.")
  }

  obs <- round(obs)

  if (sum(obs) <= 0) {
    stop(fun, ": the total count must be > 0.")
  }
  if (any(rowSums(obs) == 0)) {
    stop(fun, ": each row total must be > 0.")
  }
  if (any(colSums(obs) == 0)) {
    stop(fun, ": each column total must be > 0.")
  }

  if (is.null(rownames(obs))) {
    rownames(obs) <- paste0("Row", seq_len(nrow(obs)))
  }
  if (is.null(colnames(obs))) {
    colnames(obs) <- paste0("Col", seq_len(ncol(obs)))
  }

  obs
}

.chisq_expected_1way <- function(observed, p) {
  sum(observed) * p
}

.chisq_expected_table <- function(observed) {
  n <- sum(observed)
  outer(rowSums(observed), colSums(observed)) / n
}

.chisq_contrib <- function(observed, expected) {
  if (any(expected <= 0)) {
    stop(".chisq_contrib(): expected counts must all be positive.")
  }
  (observed - expected)^2 / expected
}

.chisq_residuals <- function(observed, expected) {
  if (any(expected <= 0)) {
    stop(".chisq_residuals(): expected counts must all be positive.")
  }
  (observed - expected) / sqrt(expected)
}

.chisq_std_residuals <- function(observed, expected) {
  if (!is.matrix(observed) || !is.matrix(expected)) {
    stop(".chisq_std_residuals(): observed and expected must both be matrices.")
  }

  n <- sum(observed)
  rprop <- rowSums(observed) / n
  cprop <- colSums(observed) / n

  denom <- sqrt(expected * outer(1 - rprop, 1 - cprop))
  out <- (observed - expected) / denom
  out[!is.finite(out)] <- NA_real_

  dimnames(out) <- dimnames(observed)
  out
}

.format_chisq_region <- function(alpha, df, formatter = .ht_internal_format_num) {
  crit <- stats::qchisq(1 - alpha, df = df)
  region <- .ht_build_region_nonsymmetric(
    stat_name = "Chi^2",
    alternative = "greater",
    crit = crit,
    formatter = formatter
  )
  list(crit = crit, region = region)
}

.chisq_make_interval_labels <- function(breaks) {
  k <- length(breaks) - 1L
  labs <- character(k)

  for (i in seq_len(k)) {
    lo <- breaks[i]
    hi <- breaks[i + 1L]

    lo_s <- if (is.infinite(lo)) {
      if (lo < 0) "-Inf" else "Inf"
    } else {
      format(lo, trim = TRUE, scientific = FALSE)
    }

    hi_s <- if (is.infinite(hi)) {
      if (hi < 0) "-Inf" else "Inf"
    } else {
      format(hi, trim = TRUE, scientific = FALSE)
    }

    if (is.infinite(lo) && lo < 0) {
      labs[i] <- paste0("(-Inf, ", hi_s, ")")
    } else if (is.infinite(hi) && hi > 0) {
      labs[i] <- paste0("[", lo_s, ", Inf)")
    } else {
      labs[i] <- paste0("[", lo_s, ", ", hi_s, ")")
    }
  }

  labs
}

.merge_labels <- function(labels) {
  if (length(labels) == 1L) labels else paste(labels, collapse = " | ")
}

.combine_sparse_adjacent <- function(observed, expected, labels, min_expected = 5) {
  k <- length(observed)

  if (length(expected) != k || length(labels) != k) {
    stop(".combine_sparse_adjacent(): observed, expected, and labels must have the same length.")
  }

  if (k <= 1L) {
    return(list(
      observed = observed,
      expected = expected,
      labels = labels,
      group_map = as.list(labels),
      combined = FALSE,
      all_expected_ok = all(expected >= min_expected)
    ))
  }

  obs_new <- numeric(0)
  exp_new <- numeric(0)
  lab_new <- character(0)
  groups <- list()

  cur_obs <- 0
  cur_exp <- 0
  cur_idx <- integer(0)

  flush_group <- function() {
    obs_new <<- c(obs_new, cur_obs)
    exp_new <<- c(exp_new, cur_exp)
    groups[[length(groups) + 1L]] <<- cur_idx
    lab_new <<- c(lab_new, .merge_labels(labels[cur_idx]))
  }

  for (i in seq_len(k)) {
    cur_obs <- cur_obs + observed[i]
    cur_exp <- cur_exp + expected[i]
    cur_idx <- c(cur_idx, i)

    if (cur_exp >= min_expected) {
      flush_group()
      cur_obs <- 0
      cur_exp <- 0
      cur_idx <- integer(0)
    }
  }

  if (length(cur_idx) > 0L) {
    if (length(groups) == 0L) {
      flush_group()
    } else {
      obs_new[length(obs_new)] <- obs_new[length(obs_new)] + cur_obs
      exp_new[length(exp_new)] <- exp_new[length(exp_new)] + cur_exp
      groups[[length(groups)]] <- c(groups[[length(groups)]], cur_idx)
      lab_new[length(lab_new)] <- .merge_labels(labels[groups[[length(groups)]]])
    }
  }

  list(
    observed = obs_new,
    expected = exp_new,
    labels = lab_new,
    group_map = lapply(groups, function(idx) labels[idx]),
    combined = (length(obs_new) != k || any(lengths(groups) > 1L)),
    all_expected_ok = all(exp_new >= min_expected)
  )
}

.combine_pois_right_tail <- function(obs_vals, lambda, n, min_expected = 5) {
  max_val <- length(obs_vals) - 1L
  tail_start <- max_val + 1L

  repeat {
    if (tail_start > 0L) {
      ind_vals <- 0:(tail_start - 1L)
      exp_ind <- n * stats::dpois(ind_vals, lambda = lambda)
    } else {
      exp_ind <- numeric(0)
    }

    exp_tail <- n * (1 - stats::ppois(tail_start - 1L, lambda = lambda))

    if (exp_tail >= min_expected && all(exp_ind >= min_expected)) break

    tail_start <- tail_start - 1L
    if (tail_start <= 0L) break
  }

  if (tail_start <= 0L) {
    tail_start <- 1L
    ind_vals <- 0:(tail_start - 1L)
    exp_ind <- n * stats::dpois(ind_vals, lambda = lambda)
    exp_tail <- n * (1 - stats::ppois(tail_start - 1L, lambda = lambda))
  }

  obs_ind <- if (tail_start > 0L) obs_vals[seq_len(tail_start)] else numeric(0)

  obs_tail <- if (tail_start <= max_val) {
    sum(obs_vals[(tail_start + 1L):(max_val + 1L)])
  } else {
    0
  }

  labels <- c(as.character(0:(tail_start - 1L)), paste0(tail_start, "+"))
  group_map <- c(
    as.list(as.character(0:(tail_start - 1L))),
    list(paste0(tail_start, "+"))
  )

  list(
    observed = c(obs_ind, obs_tail),
    expected = c(exp_ind, exp_tail),
    labels = labels,
    group_map = group_map,
    tail_start = tail_start,
    combined = (tail_start <= max_val),
    all_expected_ok = all(c(exp_ind, exp_tail) >= min_expected)
  )
}

.chisq_warn_small_expected <- function(expected, min_expected, fun, context = "") {
  if (any(expected < min_expected)) {
    warning(
      fun, ": some expected counts are < ", min_expected,
      if (nzchar(context)) paste0(" ", context) else "",
      call. = FALSE
    )
  }
}

.new_table_props_result <- function(class_name,
                                    method,
                                    margin,
                                    observed,
                                    proportions,
                                    inputs = list(),
                                    ...) {
  out <- c(
    list(
      method = method,
      margin = margin,
      observed = observed,
      proportions = proportions,
      inputs = inputs
    ),
    list(...)
  )

  class(out) <- c(class_name, "table_props_result", "list")
  out
}