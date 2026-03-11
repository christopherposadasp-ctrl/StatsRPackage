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

test_that("chisq_gof_probs defaults to equal probabilities", {
  observed <- c(A = 10, B = 20, C = 30)

  out <- chisq_gof_probs(observed = observed, quiet = TRUE)

  expected <- rep(20, 3)

  expect_equal(as.numeric(out$expected), expected, tolerance = 1e-12)
  expect_equal(out$df, 2)
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