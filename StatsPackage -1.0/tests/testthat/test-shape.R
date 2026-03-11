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

  expect_equal(out_excess$estimate, excess, tolerance = 1e-12)
  expect_equal(out_excess$benchmark, 0)
  expect_equal(out_adjusted$estimate, adjusted_excess, tolerance = 1e-12)
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

  out2 <- kurt(c(-10, 0, 0, 0, 0, 0, 10), digits = 2, quiet = TRUE)
  out6 <- kurt(c(-10, 0, 0, 0, 0, 0, 10), digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(kurt(c(-10, 0, 0, 0, 0, 0, 10), digits = 2, quiet = FALSE))
  txt6 <- utils::capture.output(kurt(c(-10, 0, 0, 0, 0, 0, 10), digits = 6, quiet = FALSE))

  expect_equal(out2$estimate, out6$estimate, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})
