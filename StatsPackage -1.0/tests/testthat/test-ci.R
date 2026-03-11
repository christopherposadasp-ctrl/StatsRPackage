##################
# CI Tests----
################
test_that("ci_mu one-sample z interval matches the hand formula", {
  out <- ci_mu(xbar = 2.25, n = 36, sigma = 1.5, quiet = TRUE)

  alpha <- 0.05
  se <- 1.5 / sqrt(36)
  crit <- stats::qnorm(1 - alpha / 2)
  target <- c(lower = 2.25 - crit * se, upper = 2.25 + crit * se)

  expect_s3_class(out, "ci_result")
  expect_equal(out$se, se, tolerance = 1e-12)
  expect_equal(out$ci, target, tolerance = 1e-12)
})

test_that("ci_mu allows n = 1 when sigma is known", {
  expect_no_error(ci_mu(xbar = 5, n = 1, sigma = 2, quiet = TRUE))
})

test_that("ci_mu rejects non-integer sample sizes", {
  expect_error(
    ci_mu(xbar = 2.25, n = 36.5, sigma = 1.5, quiet = TRUE),
    "integer sample sizes"
  )
})

test_that("ci_mu lower and upper one-sided intervals use infinite bounds correctly", {
  out_lower <- ci_mu(xbar = 10, n = 25, sigma = 4, side = "lower", quiet = TRUE)
  out_upper <- ci_mu(xbar = 10, n = 25, sigma = 4, side = "upper", quiet = TRUE)

  expect_true(is.infinite(out_lower$ci["upper"]))
  expect_true(is.infinite(out_upper$ci["lower"]))
  expect_true(out_lower$ci["upper"] > 0)
  expect_true(out_upper$ci["lower"] < 0)
})

test_that("ci_mu warns when z is used with unknown sigma and small n", {
  expect_warning(
    ci_mu(
      xbar = c(5, 4),
      n = c(12, 14),
      s = c(2.1, 1.8),
      method = "z",
      quiet = TRUE
    ),
    "large-sample approximation"
  )
})

test_that("ci_mu digits affect display only", {
  out2 <- ci_mu(xbar = 2.23456, n = 11, sigma = 1.23456, digits = 2, quiet = TRUE)
  out6 <- ci_mu(xbar = 2.23456, n = 11, sigma = 1.23456, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    ci_mu(xbar = 2.23456, n = 11, sigma = 1.23456, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    ci_mu(xbar = 2.23456, n = 11, sigma = 1.23456, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$ci, out6$ci, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("ci_p exact one-sample interval matches binom.test", {
  out <- ci_p(x = 52, n = 100, conf.level = 0.95, quiet = TRUE)
  bt <- stats::binom.test(52, 100, conf.level = 0.95)

  expect_equal(out$estimate, 0.52, tolerance = 1e-12)
  expect_equal(out$ci, setNames(unname(bt$conf.int), c("lower", "upper")), tolerance = 1e-12)
})

test_that("ci_p rejects non-integer counts", {
  expect_error(
    ci_p(x = 10.5, n = 20, quiet = TRUE),
    "integer counts"
  )
})

test_that("ci_p warns when the Wald approximation is weak", {
  expect_warning(
    ci_p(x = 1, n = 10, exact_1s = FALSE, quiet = TRUE),
    "Wald normal approximation"
  )

  expect_warning(
    ci_p(x = c(1, 9), n = c(10, 10), quiet = TRUE),
    "Wald normal approximation"
  )
})

test_that("ci_p digits affect display only", {
  out2 <- ci_p(x = 52, n = 101, exact_1s = FALSE, digits = 2, quiet = TRUE)
  out6 <- ci_p(x = 52, n = 101, exact_1s = FALSE, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    ci_p(x = 52, n = 101, exact_1s = FALSE, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    ci_p(x = 52, n = 101, exact_1s = FALSE, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$ci, out6$ci, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("ci_var one-sample interval matches the chi-square formula", {
  out <- ci_var(s = 4.2, n = 12, quiet = TRUE)

  df <- 11
  s2 <- 4.2^2
  q <- stats::qchisq(c(1 - 0.05 / 2, 0.05 / 2), df = df)
  target <- setNames(df * s2 / q, c("lower", "upper"))

  expect_equal(out$estimate, s2, tolerance = 1e-12)
  expect_equal(out$ci, target, tolerance = 1e-12)
  expect_equal(out$ci_sd, sqrt(target), tolerance = 1e-12)
})

test_that("ci_var allows zero sample SD in the one-sample case", {
  out <- ci_var(s = 0, n = 12, quiet = TRUE)

  expect_equal(out$estimate, 0)
  expect_equal(out$ci, c(lower = 0, upper = 0))
  expect_equal(out$ci_sd, c(lower = 0, upper = 0))
})

test_that("ci_var errors when the denominator SD is zero in the ratio CI", {
  expect_error(
    ci_var(s = c(4.2, 0), n = c(12, 10), quiet = TRUE),
    "undefined/infinite"
  )
})

test_that("ci_var digits affect display only", {
  out2 <- ci_var(s = 4.23456, n = 12, digits = 2, quiet = TRUE)
  out6 <- ci_var(s = 4.23456, n = 12, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(ci_var(s = 4.23456, n = 12, digits = 2, quiet = FALSE))
  txt6 <- utils::capture.output(ci_var(s = 4.23456, n = 12, digits = 6, quiet = FALSE))

  expect_equal(out2$ci, out6$ci, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("ci_lambda_exp matches the chi-square inversion formula", {
  out <- ci_lambda_exp(Sum = 3.54, n = 5, conf.level = 0.90, quiet = TRUE)

  alpha <- 0.10
  df <- 10
  q <- stats::qchisq(c(alpha / 2, 1 - alpha / 2), df = df)
  target <- setNames(q / (2 * 3.54), c("lower", "upper"))

  expect_equal(out$estimate, 5 / 3.54, tolerance = 1e-12)
  expect_equal(out$ci, target, tolerance = 1e-12)
})

test_that("ci_lambda_exp rejects invalid inputs", {
  expect_error(ci_lambda_exp(Sum = 0, n = 5, quiet = TRUE), "positive")
  expect_error(ci_lambda_exp(Sum = 3.54, n = 5.5, quiet = TRUE), "integer")
})

test_that("ci_lambda_exp digits affect display only", {
  out2 <- ci_lambda_exp(Sum = 3.54321, n = 5, digits = 2, quiet = TRUE)
  out6 <- ci_lambda_exp(Sum = 3.54321, n = 5, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    ci_lambda_exp(Sum = 3.54321, n = 5, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    ci_lambda_exp(Sum = 3.54321, n = 5, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$ci, out6$ci, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("ci_mu does not warn for two-sample known-sigma method = z", {
  expect_no_warning(
    ci_mu(
      xbar = c(105, 100),
      n = c(64, 49),
      sigma = c(12, 15),
      method = "z",
      quiet = TRUE
    )
  )
})

test_that("ci_mu warns for two-sample known-sigma method not equal to z", {
  expect_warning(
    ci_mu(
      xbar = c(105, 100),
      n = c(64, 49),
      sigma = c(12, 15),
      method = "welch",
      quiet = TRUE
    ),
    "z-based"
  )
})

test_that("ci_mu warns when method is supplied for one-sample or paired intervals", {
  expect_warning(
    ci_mu(xbar = 2.25, n = 36, sigma = 1.5, method = "z", quiet = TRUE),
    "ignored for one-sample intervals"
  )

  expect_warning(
    ci_mu(xbar = -1.4, n = 18, s = 2.2, paired = TRUE, method = "welch", quiet = TRUE),
    "ignored for paired intervals"
  )
})