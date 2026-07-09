#Tests----------
test_that("n_width_mu_z matches the hand formula and returns a minimal n", {
  out <- n_width_mu_z(w = 1, sigma = 3, conf.level = 0.95, quiet = TRUE)

  alpha <- 0.05
  z <- stats::qnorm(1 - alpha / 2)
  n_star <- (2 * z * 3 / 1)^2

  expect_s3_class(out, "width_plan_result")
  expect_equal(out$n_star, n_star, tolerance = 1e-12)
  expect_equal(out$n, ceiling(n_star), tolerance = 1e-12)
  expect_lte(out$achieved_full_width, out$target_full_width + 1e-12)
  expect_true(out$n_is_minimal)

  if (out$n > 1) {
    expect_gt(out$width_at_n_minus_1, out$target_full_width)
  }
})

test_that("n_width_mu_z can return n = 1 for a very wide target", {
  out <- n_width_mu_z(w = 20, sigma = 1, conf.level = 0.95, quiet = TRUE)

  expect_equal(out$n, 1)
  expect_true(out$meets_target)
  expect_true(out$n_is_minimal)
  expect_true(is.na(out$width_at_n_minus_1))
})

test_that("n_width_mu_z rejects invalid inputs", {
  expect_error(
    n_width_mu_z(w = 0, sigma = 3, quiet = TRUE),
    "w must be a single positive finite number"
  )
  expect_error(
    n_width_mu_z(w = 1, sigma = c(3, 4), quiet = TRUE),
    "sigma must be a single positive finite number"
  )
  expect_error(
    n_width_mu_z(w = 1, sigma = 3, conf.level = 1, quiet = TRUE),
    "conf.level"
  )
})

test_that("n_width_mu_z digits affect display only", {
  out2 <- n_width_mu_z(w = 1, sigma = 3.12345, digits = 2, quiet = TRUE)
  out6 <- n_width_mu_z(w = 1, sigma = 3.12345, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    n_width_mu_z(w = 1, sigma = 3.12345, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    n_width_mu_z(w = 1, sigma = 3.12345, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$achieved_full_width, out6$achieved_full_width, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("n_width_p_wald matches the hand formula with a fixed planning p", {
  out <- n_width_p_wald(w = 0.2, conf.level = 0.95, p = 0.3, worst_case = FALSE, quiet = TRUE)

  alpha <- 0.05
  z <- stats::qnorm(1 - alpha / 2)
  n_star <- ((2 * z)^2 * 0.3 * 0.7) / 0.2^2

  expect_s3_class(out, "width_plan_result")
  expect_equal(out$p_used, 0.3, tolerance = 1e-12)
  expect_equal(out$n_star, n_star, tolerance = 1e-12)
  expect_equal(out$n, ceiling(n_star), tolerance = 1e-12)
  expect_lte(out$achieved_full_width, out$target_full_width + 1e-12)
  expect_true(out$n_is_minimal)

  if (out$n > 1) {
    expect_gt(out$width_at_n_minus_1, out$target_full_width)
  }
})

test_that("n_width_p_wald uses p = 0.5 in worst-case mode and warns if p is supplied", {
  expect_warning(
    out <- n_width_p_wald(w = 0.2, conf.level = 0.95, p = 0.3, worst_case = TRUE, quiet = TRUE),
    "p is ignored"
  )

  expect_equal(out$p_used, 0.5, tolerance = 1e-12)
  expect_true(out$worst_case)
})

test_that("n_width_p_wald warns when the resulting Wald conditions are weak", {
  expect_warning(
    n_width_p_wald(w = 1.5, conf.level = 0.95, worst_case = TRUE, quiet = TRUE),
    "may be unreliable"
  )
})

test_that("n_width_p_wald rejects invalid p when worst_case is FALSE", {
  expect_error(
    n_width_p_wald(w = 0.2, conf.level = 0.95, p = 1, worst_case = FALSE, quiet = TRUE),
    "strictly between 0 and 1"
  )
  expect_error(
    n_width_p_wald(w = 0.2, conf.level = 0.95, p = c(0.3, 0.4), worst_case = FALSE, quiet = TRUE),
    "strictly between 0 and 1"
  )
})

test_that("n_width_p_wald digits affect display only", {
  out2 <- n_width_p_wald(w = 0.2, conf.level = 0.95, p = 0.3, worst_case = FALSE, digits = 2, quiet = TRUE)
  out6 <- n_width_p_wald(w = 0.2, conf.level = 0.95, p = 0.3, worst_case = FALSE, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    n_width_p_wald(w = 0.2, conf.level = 0.95, p = 0.3, worst_case = FALSE, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    n_width_p_wald(w = 0.2, conf.level = 0.95, p = 0.3, worst_case = FALSE, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$achieved_full_width, out6$achieved_full_width, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("n_width_mu_t returns an n that meets the target and is minimal", {
  out <- n_width_mu_t(w = 1, s = 3, conf.level = 0.95, quiet = TRUE)

  z <- stats::qnorm(1 - 0.05 / 2)
  expect_s3_class(out, "width_plan_result")
  expect_equal(out$n_star_initial, (2 * z * 3 / 1)^2, tolerance = 1e-12)
  expect_true(out$converged)
  expect_gte(out$n, 2)
  expect_lte(out$achieved_full_width, out$target_full_width + 1e-12)
  expect_true(out$n_is_minimal)

  if (out$n > 2) {
    expect_gt(out$width_at_n_minus_1, out$target_full_width)
  }
})

test_that("n_width_mu_t never falls below n = 2", {
  out <- n_width_mu_t(w = 100, s = 3, conf.level = 0.95, quiet = TRUE)

  expect_equal(out$n, 2)
  expect_true(out$meets_target)
  expect_true(out$n_is_minimal)
})

test_that("n_width_mu_t rejects invalid n_start and max_iter", {
  expect_error(
    n_width_mu_t(w = 1, s = 3, n_start = 1, quiet = TRUE),
    "n_start must be a single integer >= 2"
  )
  expect_error(
    n_width_mu_t(w = 1, s = 3, max_iter = 0, quiet = TRUE),
    "max_iter must be a single integer >= 1"
  )
  expect_error(
    n_width_mu_t(w = 1, s = 3, n_start = 10.5, quiet = TRUE),
    "n_start must be a single integer >= 2"
  )
})

test_that("n_width_mu_t warns on nonconvergence and still returns a valid n", {
  expect_warning(
    out <- n_width_mu_t(w = 1, s = 3, conf.level = 0.95, max_iter = 1, quiet = TRUE),
    "did not converge"
  )

  expect_false(out$converged)
  expect_gte(out$n, 2)
  expect_lte(out$achieved_full_width, out$target_full_width + 1e-12)
  expect_true(out$n_is_minimal)
})

test_that("n_width_mu_t digits affect display only", {
  out2 <- n_width_mu_t(w = 1, s = 3.12345, digits = 2, quiet = TRUE)
  out6 <- n_width_mu_t(w = 1, s = 3.12345, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    n_width_mu_t(w = 1, s = 3.12345, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    n_width_mu_t(w = 1, s = 3.12345, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$achieved_full_width, out6$achieved_full_width, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})
