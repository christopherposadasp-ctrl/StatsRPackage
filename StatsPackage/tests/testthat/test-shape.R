test_that("skew moment estimator matches the hand formula and interpretation", {
  x <- c(1, 1, 2, 2, 3, 9)
  d <- x - mean(x)
  target <- mean(d^3) / (mean(d^2)^(3 / 2))

  out <- skew(x, quiet = TRUE)

  expect_s3_class(out, "skew_result")
  expect_s3_class(out, "shape_result")
  expect_equal(out$estimate, target, tolerance = 1e-12)
  expect_equal(out$benchmark, 0)
  expect_equal(out$direction, "right-skewed")
  expect_match(out$interpretation, "right tail")
})

test_that("skew adjusted estimator matches the Fisher-Pearson adjustment", {
  x <- c(1, 1, 2, 2, 3, 9)
  n <- length(x)
  d <- x - mean(x)
  g1 <- mean(d^3) / (mean(d^2)^(3 / 2))
  target <- sqrt(n * (n - 1)) / (n - 2) * g1

  out <- skew(x, method = "adjusted", quiet = TRUE)

  expect_equal(out$estimate, target, tolerance = 1e-12)
  expect_match(out$estimator, "sqrt")
})

test_that("skew interpretation labels cover left-skewed and symmetric samples", {
  out_left <- skew(c(-9, -3, -2, -2, -1, -1), quiet = TRUE)
  out_sym <- skew(c(-2, -1, 0, 1, 2), quiet = TRUE)

  expect_equal(out_left$direction, "left-skewed")
  expect_match(out_left$interpretation, "left tail")
  expect_equal(out_sym$estimate, 0, tolerance = 1e-12)
  expect_equal(out_sym$direction, "approximately symmetric")
})

test_that("skew returns invisibly with the expected class chain", {
  out <- invisible(skew(c(1, 1, 2, 2, 3, 9), quiet = TRUE))

  expect_s3_class(out, "skew_result")
  expect_s3_class(out, "shape_result")
  expect_s3_class(out, "list")
})

test_that("skew validates missing values, minimum n, zero variance, and digits", {
  expect_error(
    skew(c(1, NA, 2), quiet = TRUE),
    "missing values"
  )

  out_rm <- skew(c(1, 2, NA, 4), na_rm = TRUE, quiet = TRUE)
  expect_equal(out_rm$n, 3L)

  expect_error(
    skew(c(1, 2), method = "adjusted", quiet = TRUE),
    "at least 3"
  )

  expect_error(
    skew(c(5, 5, 5), quiet = TRUE),
    "identical"
  )

  expect_error(
    skew(c(1, 2, Inf), quiet = TRUE),
    "finite values"
  )

  out2 <- skew(c(1, 1, 2, 2, 3, 9), digits = 2, quiet = TRUE)
  out6 <- skew(c(1, 1, 2, 2, 3, 9), digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(skew(c(1, 1, 2, 2, 3, 9), digits = 2, quiet = FALSE))
  txt6 <- utils::capture.output(skew(c(1, 1, 2, 2, 3, 9), digits = 6, quiet = FALSE))

  expect_equal(out2$estimate, out6$estimate, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("kurt moment estimator matches the Pearson formula and interpretation", {
  x <- c(-10, 0, 0, 0, 0, 0, 10)
  d <- x - mean(x)
  target <- mean(d^4) / (mean(d^2)^2)

  out <- kurt(x, quiet = TRUE)

  expect_s3_class(out, "kurt_result")
  expect_s3_class(out, "shape_result")
  expect_equal(out$estimate, target, tolerance = 1e-12)
  expect_equal(out$benchmark, 3)
  expect_equal(out$shape, "leptokurtic")
  expect_match(out$interpretation, "heavier tails")
})

test_that("kurt excess and adjusted options match hand formulas", {
  x <- c(1, 1, 2, 2, 3, 9)
  n <- length(x)
  d <- x - mean(x)
  pearson <- mean(d^4) / (mean(d^2)^2)
  excess <- pearson - 3
  adjusted_excess <- ((n - 1) / ((n - 2) * (n - 3))) * ((n + 1) * excess + 6)

  out_excess <- kurt(x, excess = TRUE, quiet = TRUE)
  out_adjusted <- kurt(x, method = "adjusted", excess = TRUE, quiet = TRUE)
  out_adjusted_pearson <- kurt(x, method = "adjusted", excess = FALSE, quiet = TRUE)

  expect_equal(out_excess$estimate, excess, tolerance = 1e-12)
  expect_equal(out_excess$benchmark, 0)
  expect_equal(out_adjusted$estimate, adjusted_excess, tolerance = 1e-12)
  expect_equal(out_adjusted_pearson$estimate, adjusted_excess + 3, tolerance = 1e-12)
  expect_equal(out_adjusted_pearson$benchmark, 3)
})

test_that("kurt interpretation labels cover platykurtic and mesokurtic samples", {
  meso <- c(-1, 0, 0, 0, 0, 1)
  out_platy <- kurt(c(-2, -1, 0, 1, 2), quiet = TRUE)
  out_meso <- kurt(meso, quiet = TRUE)
  out_excess_meso <- kurt(meso, excess = TRUE, quiet = TRUE)

  expect_equal(out_platy$shape, "platykurtic")
  expect_match(out_platy$interpretation, "lighter tails")
  expect_equal(out_meso$estimate, 3, tolerance = 1e-12)
  expect_equal(out_meso$shape, "mesokurtic")
  expect_equal(out_excess_meso$estimate, 0, tolerance = 1e-12)
  expect_equal(out_excess_meso$benchmark, 0)
  expect_equal(out_excess_meso$shape, "mesokurtic")
})

test_that("kurt returns invisibly with the expected class chain", {
  out <- invisible(kurt(c(-10, 0, 0, 0, 0, 0, 10), quiet = TRUE))

  expect_s3_class(out, "kurt_result")
  expect_s3_class(out, "shape_result")
  expect_s3_class(out, "list")
})

test_that("kurt validates missing values, minimum n, zero variance, and digits", {
  expect_error(
    kurt(c(1, NA, 2), quiet = TRUE),
    "missing values"
  )

  out_rm <- kurt(c(1, 2, 3, NA, 4), na_rm = TRUE, quiet = TRUE)
  expect_equal(out_rm$n, 4L)

  expect_error(
    kurt(c(1, 2, 3), method = "adjusted", quiet = TRUE),
    "at least 4"
  )

  expect_error(
    kurt(c(5, 5, 5), quiet = TRUE),
    "identical"
  )

  expect_error(
    kurt(c(1, 2, Inf), quiet = TRUE),
    "finite values"
  )

  out2 <- kurt(c(-10, 0, 0, 0, 0, 0, 10), digits = 2, quiet = TRUE)
  out6 <- kurt(c(-10, 0, 0, 0, 0, 0, 10), digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(kurt(c(-10, 0, 0, 0, 0, 0, 10), digits = 2, quiet = FALSE))
  txt6 <- utils::capture.output(kurt(c(-10, 0, 0, 0, 0, 0, 10), digits = 6, quiet = FALSE))

  expect_equal(out2$estimate, out6$estimate, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})
