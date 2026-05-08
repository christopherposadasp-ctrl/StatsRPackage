test_that("chisq_gof_probs matches the hand calculation for specified probabilities", {
  observed <- c(57, 11, 330, 6)
  p0 <- c(0.177, 0.032, 0.734, 0.057)

  out <- chisq_gof_probs(observed = observed, p = p0, quiet = TRUE)

  expected <- sum(observed) * p0
  contrib <- (observed - expected)^2 / expected
  residuals <- (observed - expected) / sqrt(expected)
  chi_stat <- sum(contrib)
  p_val <- stats::pchisq(chi_stat, df = 3, lower.tail = FALSE)

  expect_s3_class(out, "htest_result")
  expect_equal(as.numeric(out$expected), expected, tolerance = 1e-12)
  expect_equal(as.numeric(out$contrib), contrib, tolerance = 1e-12)
  expect_equal(as.numeric(out$residuals), residuals, tolerance = 1e-12)
  expect_equal(out$chi_stat, chi_stat, tolerance = 1e-12)
  expect_equal(out$df, 3)
  expect_equal(out$p_value, p_val, tolerance = 1e-12)
})

test_that("chisq_gof_probs stores explicit labels, estimates, and critical values", {
  observed <- c(57, 11, 330, 6)
  p0 <- c(0.177, 0.032, 0.734, 0.057)
  labels <- c("A", "B", "C", "D")

  out <- chisq_gof_probs(
    observed = observed,
    p = p0,
    labels = labels,
    alpha = 0.01,
    quiet = TRUE
  )

  expect_equal(names(out$observed), labels)
  expect_equal(names(out$expected), labels)
  expect_equal(as.numeric(out$estimate), observed / sum(observed), tolerance = 1e-12)
  expect_equal(out$chi_crit, stats::qchisq(0.99, df = 3), tolerance = 1e-12)
  expect_match(out$reject_region, "^Chi\\^2 >")
})

test_that("chisq_gof_probs defaults to equal probabilities", {
  observed <- c(A = 10, B = 20, C = 30)

  out <- chisq_gof_probs(observed = observed, quiet = TRUE)

  expected <- rep(20, 3)

  expect_equal(as.numeric(out$expected), expected, tolerance = 1e-12)
  expect_equal(out$df, 2)
})

test_that("chisq_gof_probs generates default labels when labels are omitted", {
  out <- chisq_gof_probs(observed = c(10, 20, 30), quiet = TRUE)

  expect_equal(out$labels, c("1", "2", "3"))
  expect_equal(names(out$expected), c("1", "2", "3"))
  expect_equal(as.numeric(out$expected), rep(20, 3), tolerance = 1e-12)
})

test_that("chisq_gof_probs warns when expected counts are small", {
  observed <- c(20, 1, 1)
  p0 <- c(0.9, 0.05, 0.05)

  expect_warning(
    chisq_gof_probs(observed = observed, p = p0, quiet = TRUE),
    "expected counts"
  )
})

test_that("chisq_gof_dist adjusts df correctly when parameters are estimated for normal raw data", {
  x <- seq(-2.45, 2.45, length.out = 50)

  out <- chisq_gof_dist(
    x = x,
    dist = "norm",
    k = 5,
    estimate = TRUE,
    quiet = TRUE
  )

  expect_s3_class(out, "htest_result")
  expect_equal(as.numeric(out$expected), rep(10, 5), tolerance = 1e-10)
  expect_equal(out$df, 2)
  expect_equal(out$chi_stat, sum(out$contrib), tolerance = 1e-12)
})

test_that("chisq_gof_dist allows k = 3 for exponential raw data when df stays positive", {
  x <- seq(0.1, 3, length.out = 30)

  out <- chisq_gof_dist(
    x = x,
    dist = "exp",
    k = 3,
    estimate = TRUE,
    quiet = TRUE
  )

  expect_s3_class(out, "htest_result")
  expect_equal(as.numeric(out$expected), rep(10, 3), tolerance = 1e-10)
  expect_equal(out$df, 1)
  expect_equal(out$chi_stat, sum(out$contrib), tolerance = 1e-12)
})

test_that("chisq_gof_dist raw exponential fixed-parameter bins match hand calculation", {
  x <- seq(0.05, 4.95, length.out = 50)
  rate <- 0.8

  out <- chisq_gof_dist(
    x = x,
    dist = "exp",
    k = 5,
    params = list(rate = rate),
    estimate = FALSE,
    quiet = TRUE
  )

  breaks <- c(0, stats::qexp((1:4) / 5, rate = rate), Inf)
  observed <- as.numeric(table(cut(x, breaks = breaks, right = FALSE, include.lowest = TRUE)))
  expected <- length(x) * diff(stats::pexp(breaks, rate = rate))
  contrib <- (observed - expected)^2 / expected

  expect_equal(out$breaks, breaks, tolerance = 1e-12)
  expect_equal(as.numeric(out$observed), observed, tolerance = 1e-12)
  expect_equal(as.numeric(out$expected), expected, tolerance = 1e-12)
  expect_equal(out$chi_stat, sum(contrib), tolerance = 1e-12)
  expect_equal(out$df, length(observed) - 1L)
})

test_that("chisq_gof_dist grouped normal honors params_estimated df adjustment", {
  observed <- c(8, 17, 20, 16, 9)
  breaks <- c(-Inf, -1, 0, 1, 2, Inf)

  out <- chisq_gof_dist(
    observed = observed,
    breaks = breaks,
    dist = "norm",
    params = list(mean = 0.5, sd = 1.25),
    params_estimated = TRUE,
    min_expected = 1,
    alpha = 0.10,
    quiet = TRUE
  )

  expected <- sum(observed) * diff(stats::pnorm(breaks, mean = 0.5, sd = 1.25))
  contrib <- (observed - expected)^2 / expected

  expect_equal(as.numeric(out$expected), expected, tolerance = 1e-12)
  expect_equal(out$params_estimated_n, 2L)
  expect_equal(out$df, length(observed) - 1L - 2L)
  expect_equal(out$chi_stat, sum(contrib), tolerance = 1e-12)
  expect_equal(out$chi_crit, stats::qchisq(0.90, df = out$df), tolerance = 1e-12)
})

test_that("chisq_gof_dist supports raw uniform GOF with estimated bounds", {
  x <- seq(0, 1, length.out = 30)

  out <- chisq_gof_dist(
    x = x,
    dist = "unif",
    k = 5,
    estimate = TRUE,
    quiet = TRUE
  )

  expect_s3_class(out, "htest_result")
  expect_equal(out$params$min, 0, tolerance = 1e-12)
  expect_equal(out$params$max, 1, tolerance = 1e-12)
  expect_equal(as.numeric(out$expected), rep(6, 5), tolerance = 1e-10)
  expect_equal(out$df, 2)
  expect_equal(out$chi_stat, sum(out$contrib), tolerance = 1e-12)
})

test_that("chisq_gof_dist combines sparse adjacent continuous classes", {
  out <- suppressWarnings(
    chisq_gof_dist(
      x = c(0.01, 0.02, 0.03, 0.10, 0.20, 2.5, 3.0, 3.5, 4.0, 4.5),
      dist = "exp",
      params = list(rate = 1),
      breaks = c(0, 0.05, 0.1, 0.2, 1, Inf),
      min_expected = 2,
      quiet = TRUE
    )
  )

  expect_true(out$combined)
  expect_equal(sum(out$observed), out$n, tolerance = 1e-12)
  expect_equal(sum(out$expected), out$n, tolerance = 1e-12)
  expect_true(all(as.numeric(out$expected) > 0))
  expect_equal(out$chi_stat, sum(out$contrib), tolerance = 1e-12)
})

test_that("chisq_gof_dist supports grouped uniform GOF with fixed bounds", {
  out <- chisq_gof_dist(
    observed = c(10, 10, 10, 10),
    breaks = seq(0, 1, by = 0.25),
    dist = "unif",
    params = list(lower = 0, upper = 1),
    quiet = TRUE
  )

  expect_s3_class(out, "htest_result")
  expect_equal(as.numeric(out$expected), rep(10, 4), tolerance = 1e-12)
  expect_equal(out$df, 3)
  expect_equal(out$chi_stat, sum(out$contrib), tolerance = 1e-12)
})

test_that("chisq_gof_dist rejects grouped uniform GOF when estimate = TRUE", {
  expect_error(
    chisq_gof_dist(
      observed = c(10, 10, 10, 10),
      breaks = seq(0, 1, by = 0.25),
      dist = "unif",
      estimate = TRUE,
      quiet = TRUE
    ),
    "supported only for raw x input"
  )
})

test_that("chisq_gof_dist rejects k = 3 for normal raw data with estimated parameters", {
  x <- seq(-2.45, 2.45, length.out = 50)

  expect_error(
    chisq_gof_dist(
      x = x,
      dist = "norm",
      k = 3,
      estimate = TRUE,
      quiet = TRUE
    ),
    "too small for dist = 'norm'"
  )
})

test_that("chisq_gof_dist combines sparse right-tail classes for Poisson", {
  observed <- c(40, 30, 15, 8, 4, 2, 1)

  out <- chisq_gof_dist(
    observed = observed,
    dist = "pois",
    params = list(lambda = 1.2),
    estimate = FALSE,
    min_expected = 5,
    quiet = TRUE
  )

  expect_true(any(grepl("\\+$", out$labels)))
  expect_true(all(as.numeric(out$expected) >= 5) || length(out$expected) == 1L)
  expect_equal(out$chi_stat, sum(out$contrib), tolerance = 1e-12)
})

test_that("chisq_gof_dist raw Poisson estimated lambda adjusts df", {
  x <- c(rep(0, 36), rep(1, 31), rep(2, 18), rep(3, 9), rep(4, 4), rep(5, 2))

  out <- chisq_gof_dist(
    x = x,
    dist = "pois",
    estimate = TRUE,
    min_expected = 5,
    quiet = TRUE
  )

  expect_equal(out$params$lambda, mean(x), tolerance = 1e-12)
  expect_equal(out$params_estimated_n, 1L)
  expect_equal(out$df, length(out$observed) - 1L - 1L)
  expect_true(any(grepl("\\+$", out$labels)))
  expect_true(all(as.numeric(out$expected) > 0))
})

test_that("chisq_table expected counts, statistic, and residuals match chisq.test", {
  observed <- matrix(
    c(25, 10,
      24, 32,
      28, 17,
      19, 34),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(
      c("<16", "16-17", "18-20", ">20"),
      c("Male", "Female")
    )
  )

  out <- chisq_table(observed, type = "independence", correct = FALSE, quiet = TRUE)
  ref <- suppressWarnings(stats::chisq.test(observed, correct = FALSE))

  expect_s3_class(out, "htest_result")
  expect_equal(out$expected, ref$expected, tolerance = 1e-12)
  expect_equal(out$chi_stat, unname(ref$statistic), tolerance = 1e-12)
  expect_equal(out$df, unname(ref$parameter))
  expect_equal(out$p_value, ref$p.value, tolerance = 1e-12)
  expect_equal(out$residuals, ref$residuals, tolerance = 1e-12)
  expect_equal(out$std_residuals, ref$stdres, tolerance = 1e-12)
})

test_that("chisq_table supports Yates correction for 2 x 2 tables", {
  observed <- matrix(c(12, 5, 7, 10), nrow = 2, byrow = TRUE)

  out <- chisq_table(observed, correct = TRUE, quiet = TRUE)
  ref <- suppressWarnings(stats::chisq.test(observed, correct = TRUE))

  expect_equal(out$chi_stat, unname(ref$statistic), tolerance = 1e-12)
  expect_equal(out$p_value, ref$p.value, tolerance = 1e-12)
})

test_that("chisq_table ignores Yates correction outside 2 x 2 tables", {
  observed <- matrix(
    c(25, 10,
      24, 32,
      28, 17),
    nrow = 3,
    byrow = TRUE
  )

  out_uncorrected <- chisq_table(observed, correct = FALSE, quiet = TRUE)

  expect_warning(
    out_warned <- chisq_table(observed, correct = TRUE, quiet = TRUE),
    "ignored"
  )

  expect_false(out_warned$correct)
  expect_equal(out_warned$chi_stat, out_uncorrected$chi_stat, tolerance = 1e-12)
  expect_equal(out_warned$p_value, out_uncorrected$p_value, tolerance = 1e-12)
})

test_that("chisq_table independence and homogeneity differ only by interpretation", {
  observed <- matrix(
    c(25, 10,
      24, 32,
      28, 17),
    nrow = 3,
    byrow = TRUE
  )

  out_ind <- chisq_table(observed, type = "independence", quiet = TRUE)
  out_hom <- chisq_table(observed, type = "homogeneity", quiet = TRUE)

  expect_equal(out_hom$expected, out_ind$expected, tolerance = 1e-12)
  expect_equal(out_hom$contrib, out_ind$contrib, tolerance = 1e-12)
  expect_equal(out_hom$chi_stat, out_ind$chi_stat, tolerance = 1e-12)
  expect_equal(out_hom$p_value, out_ind$p_value, tolerance = 1e-12)
  expect_false(identical(out_hom$null, out_ind$null))
})

test_that("chisq_table warns when expected counts are small", {
  observed <- matrix(c(1, 9, 8, 2), nrow = 2, byrow = TRUE)

  expect_warning(
    chisq_table(observed, quiet = TRUE),
    "expected counts"
  )
})

test_that("table_props returns row proportions correctly", {
  observed <- matrix(
    c(25, 10,
      24, 32),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("A", "B"), c("Male", "Female"))
  )

  out <- table_props(observed, margin = "row", quiet = TRUE)

  expect_s3_class(out, "table_props_result")
  expect_equal(out$proportions, prop.table(observed, 1), tolerance = 1e-12)
  expect_equal(rownames(out$proportions), rownames(observed))
  expect_equal(colnames(out$proportions), colnames(observed))
  expect_equal(out$row_totals, rowSums(observed), tolerance = 1e-12)
  expect_equal(out$col_totals, colSums(observed), tolerance = 1e-12)
  expect_equal(out$grand_total, sum(observed), tolerance = 1e-12)
})

test_that("table_props returns column and overall proportions correctly", {
  observed <- matrix(
    c(25, 10,
      24, 32),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("A", "B"), c("Male", "Female"))
  )

  out_col <- table_props(observed, margin = "col", quiet = TRUE)
  out_all <- table_props(observed, margin = "overall", quiet = TRUE)

  expect_equal(out_col$proportions, prop.table(observed, 2), tolerance = 1e-12)
  expect_equal(out_all$proportions, prop.table(observed), tolerance = 1e-12)
})

test_that("table_props digits affect display only", {
  observed <- matrix(
    c(25, 10,
      24, 32),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("A", "B"), c("Male", "Female"))
  )

  out2 <- table_props(observed, margin = "row", digits = 2, quiet = TRUE)
  out6 <- table_props(observed, margin = "row", digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    table_props(observed, margin = "row", digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    table_props(observed, margin = "row", digits = 6, quiet = FALSE)
  )

  expect_equal(out2$proportions, out6$proportions, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})
