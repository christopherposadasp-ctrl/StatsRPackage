#############################
# Prediction Interval Tests----
#############################

test_that("pi_mu t prediction interval matches the hand formula", {
  out <- pi_mu(xbar = 12.4, n = 15, s = 3.2, quiet = TRUE)

  alpha <- 0.05
  se_pred <- 3.2 * sqrt(1 + 1 / 15)
  crit <- stats::qt(1 - alpha / 2, df = 14)
  target <- c(lower = 12.4 - crit * se_pred, upper = 12.4 + crit * se_pred)

  expect_s3_class(out, "prediction_result")
  expect_s3_class(out, "pi_mu_result")
  expect_equal(out$se_pred, se_pred, tolerance = 1e-12)
  expect_equal(out$pi, target, tolerance = 1e-12)
  expect_equal(out$df, 14)
  expect_equal(out$parameter, "future_y")
})

test_that("pi_mu z prediction interval matches the hand formula", {
  out <- pi_mu(xbar = 2.25, n = 36, sigma = 1.5, quiet = TRUE)

  alpha <- 0.05
  se_pred <- 1.5 * sqrt(1 + 1 / 36)
  crit <- stats::qnorm(1 - alpha / 2)
  target <- c(lower = 2.25 - crit * se_pred, upper = 2.25 + crit * se_pred)

  expect_equal(out$se_pred, se_pred, tolerance = 1e-12)
  expect_equal(out$pi, target, tolerance = 1e-12)
  expect_true(is.na(out$df))
  expect_equal(out$procedure, "one_z_known_sigma")
})

test_that("pi_mu lower and upper one-sided intervals use infinite bounds", {
  out_lower <- pi_mu(xbar = 10, n = 25, s = 4, side = "lower", quiet = TRUE)
  out_upper <- pi_mu(xbar = 10, n = 25, s = 4, side = "upper", quiet = TRUE)

  expect_true(is.infinite(out_lower$pi["upper"]))
  expect_true(is.infinite(out_upper$pi["lower"]))
  expect_true(out_lower$pi["upper"] > 0)
  expect_true(out_upper$pi["lower"] < 0)
})

test_that("pi_mu digits affect display only", {
  out2 <- pi_mu(xbar = 2.23456, n = 11, sigma = 1.23456, digits = 2, quiet = TRUE)
  out6 <- pi_mu(xbar = 2.23456, n = 11, sigma = 1.23456, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    pi_mu(xbar = 2.23456, n = 11, sigma = 1.23456, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    pi_mu(xbar = 2.23456, n = 11, sigma = 1.23456, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$pi, out6$pi, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("pi_mu printed output labels the interval as PI", {
  txt <- utils::capture.output(
    pi_mu(xbar = 12.4, n = 15, s = 3.2, quiet = FALSE)
  )

  expect_true(any(grepl("^PI:", txt)))
  expect_false(any(grepl("^CI:", txt)))
})

test_that("pi_mu validates inputs", {
  expect_error(pi_mu(xbar = 1, n = 5, s = 2, sigma = 2, quiet = TRUE), "only one")
  expect_error(pi_mu(xbar = 1, n = 5, quiet = TRUE), "either sigma")
  expect_error(pi_mu(xbar = 1, n = 5.5, s = 2, quiet = TRUE), "whole-number")
  expect_error(pi_mu(xbar = 1, n = 1, s = 2, quiet = TRUE), "n >= 2")
  expect_error(pi_mu(xbar = c(1, 2), n = 5, s = 2, quiet = TRUE), "one finite")
  expect_error(pi_mu(xbar = 1, n = 5, s = -2, quiet = TRUE), ">= 0")
  expect_error(pi_mu(xbar = 1, n = 5, sigma = -2, quiet = TRUE), ">= 0")
})
