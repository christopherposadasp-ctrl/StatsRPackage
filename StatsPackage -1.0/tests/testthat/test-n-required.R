test_that("default alternative is two-sided across the required-n family", {
  toy_power <- function(n) 1 - exp(-n / 5)

  out_generic <- n_required_from_power(toy_power, target_power = 0.80, quiet = TRUE)
  out_z <- n_required_z_mu(mu_a = 1, mu0 = 0, sigma = 1, beta_target = 0.20, quiet = TRUE)
  out_t <- n_required_t_mu(mu_a = 1, mu0 = 0, sigma_true = 1, beta_target = 0.20, quiet = TRUE)
  out_p <- suppressWarnings(
    n_required_p_z(p_a = 0.90, p0 = 0.10, beta_target = 0.50, quiet = TRUE)
  )
  out_v <- n_required_var_chisq(sigma_a = 2, sigma0 = 1, beta_target = 0.20, quiet = TRUE)
  out_f <- n_required_var_ratio_F(sigma_a = c(2, 1), ratio0 = 1, beta_target = 0.20, quiet = TRUE)

  expect_s3_class(out_generic, "sample_size_result")
  expect_equal(out_z$alternative, "two.sided")
  expect_equal(out_t$alternative, "two.sided")
  expect_equal(out_p$alternative, "two.sided")
  expect_equal(out_v$alternative, "two.sided")
  expect_equal(out_f$alternative, "two.sided")
})

test_that("n_required_from_power finds the minimal integer n for a monotone power curve", {
  toy_power <- function(n) 1 - exp(-n / 50)

  out <- n_required_from_power(
    power_at_n = toy_power,
    target_power = 0.80,
    n_min = 1,
    n_max = 10000,
    quiet = TRUE
  )

  expect_s3_class(out, "sample_size_result")
  expect_equal(out$n, 81L)
  expect_true(out$achieved_power >= 0.80)
  expect_true(toy_power(out$n - 1L) < 0.80)
})

test_that("n_required_from_power validates its interface and digits affect display only", {
  expect_error(
    n_required_from_power(power_at_n = 3, target_power = 0.80, quiet = TRUE),
    "must be a function"
  )

  expect_error(
    n_required_from_power(power_at_n = function(n) 0.5, target_power = 0.80, n_min = 1.5, quiet = TRUE),
    "single integer"
  )

  expect_error(
    n_required_from_power(power_at_n = function(n) 0.5, target_power = 0.80, quiet = 1),
    "TRUE or FALSE"
  )

  toy_power <- function(n) 1 - exp(-n / 50)

  out2 <- n_required_from_power(toy_power, target_power = 0.80, digits = 2, quiet = TRUE)
  out6 <- n_required_from_power(toy_power, target_power = 0.80, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    n_required_from_power(toy_power, target_power = 0.80, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    n_required_from_power(toy_power, target_power = 0.80, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$achieved_power, out6$achieved_power, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("n_required_z_mu one-sample one-sided design meets target and is minimal", {
  out <- n_required_z_mu(
    mu_a = 30500,
    mu0 = 30000,
    sigma = 1500,
    alpha = 0.01,
    beta_target = 0.05,
    alternative = "greater",
    quiet = TRUE
  )

  chk <- power_z_mu(
    mu_a = 30500,
    mu0 = 30000,
    sigma = 1500,
    n = out$n,
    alpha = 0.01,
    alternative = "greater",
    quiet = TRUE
  )

  prev <- power_z_mu(
    mu_a = 30500,
    mu0 = 30000,
    sigma = 1500,
    n = out$n - 1L,
    alpha = 0.01,
    alternative = "greater",
    quiet = TRUE
  )

  expect_s3_class(out, "sample_size_result")
  expect_true(out$achieved_power >= 0.95)
  expect_equal(out$achieved_power, chk$power, tolerance = 1e-12)
  expect_true(prev$power < 0.95)
})

test_that("n_required_z_mu two-sided design can return n = 1 and two-sample rounding is respected", {
  out_one <- n_required_z_mu(
    mu_a = 10,
    mu0 = 0,
    sigma = 1,
    alpha = 0.05,
    beta_target = 0.20,
    alternative = "two.sided",
    quiet = TRUE
  )

  expect_equal(out_one$n, 1L)

  out_two <- n_required_z_mu(
    mu_a = c(65, 63),
    mu0 = 0,
    sigma = 3,
    alpha = 0.05,
    beta_target = 0.10,
    alternative = "two.sided",
    n_ratio = 1.5,
    quiet = TRUE
  )

  prev <- power_z_mu(
    mu_a = c(65, 63),
    mu0 = 0,
    sigma = c(3, 3),
    n = c(out_two$n1 - 1L, max(1L, ceiling(1.5 * (out_two$n1 - 1L)))),
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  expect_equal(out_two$sigma, c(3, 3), tolerance = 1e-12)
  expect_equal(out_two$n2, as.integer(ceiling(1.5 * out_two$n1)))
  expect_true(out_two$achieved_power >= 0.90)
  expect_true(prev$power < 0.90)
})

test_that("n_required_z_mu warns when n_ratio is ignored and digits affect display only", {
  expect_warning(
    n_required_z_mu(mu_a = 1, mu0 = 0, sigma = 1, n_ratio = 2, quiet = TRUE),
    "ignored"
  )

  out2 <- n_required_z_mu(mu_a = 1, mu0 = 0, sigma = 1, beta_target = 0.20, digits = 2, quiet = TRUE)
  out6 <- n_required_z_mu(mu_a = 1, mu0 = 0, sigma = 1, beta_target = 0.20, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    n_required_z_mu(mu_a = 1, mu0 = 0, sigma = 1, beta_target = 0.20, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    n_required_z_mu(mu_a = 1, mu0 = 0, sigma = 1, beta_target = 0.20, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$achieved_power, out6$achieved_power, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("n_required_t_mu one-sample design meets target and paired branch warns once for ignored args", {
  out <- n_required_t_mu(
    mu_a = 2.0,
    mu0 = 3.0,
    sigma_true = 1.5,
    alpha = 0.05,
    beta_target = 0.10,
    alternative = "less",
    quiet = TRUE
  )

  prev <- power_t_mu(
    mu_a = 2.0,
    mu0 = 3.0,
    sigma_true = 1.5,
    n = out$n - 1L,
    alpha = 0.05,
    alternative = "less",
    quiet = TRUE
  )

  expect_s3_class(out, "sample_size_result")
  expect_true(out$achieved_power >= 0.90)
  expect_true(prev$power < 0.90)

  res <- collect_warnings(
    n_required_t_mu(
      mu_a = 1.5,
      mu0 = 0,
      sigma_true = 2,
      alpha = 0.05,
      beta_target = 0.10,
      alternative = "greater",
      paired = TRUE,
      method = "pooled",
      n_ratio = 2,
      quiet = TRUE
    )
  )

  expect_equal(sum(grepl("argument 'method'", res$warnings, fixed = TRUE)), 1L)
  expect_equal(sum(grepl("argument 'n_ratio'", res$warnings, fixed = TRUE)), 1L)
})

test_that("n_required_t_mu rejects paired = TRUE with two-sample input", {
  expect_error(
    n_required_t_mu(
      mu_a = c(10, 8),
      mu0 = 0,
      sigma_true = c(3, 2.5),
      paired = TRUE,
      quiet = TRUE
    ),
    "paired = TRUE"
  )
})

test_that("n_required_p_z warns once for ignored one-sample arguments and once for final approximation issues", {
  res_ignored <- collect_warnings(
    n_required_p_z(
      p_a = 0.15,
      p0 = 0.10,
      alpha = 0.05,
      beta_target = 0.10,
      alternative = "greater",
      pooled = TRUE,
      continuity = FALSE,
      n_ratio = 2,
      quiet = TRUE
    )
  )

  expect_equal(sum(grepl("argument 'pooled'", res_ignored$warnings, fixed = TRUE)), 1L)
  expect_equal(sum(grepl("argument 'continuity'", res_ignored$warnings, fixed = TRUE)), 1L)
  expect_equal(sum(grepl("argument 'n_ratio'", res_ignored$warnings, fixed = TRUE)), 1L)
  expect_true(res_ignored$value$achieved_power >= 0.90)

  res_approx <- collect_warnings(
    n_required_p_z(
      p_a = c(0.95, 0.05),
      p0 = 0,
      alpha = 0.05,
      beta_target = 0.50,
      alternative = "two.sided",
      pooled = TRUE,
      continuity = TRUE,
      n_min = 1,
      n_max = 50,
      quiet = TRUE
    )
  )

  expect_equal(sum(grepl("normal approximation may be unreliable", res_approx$warnings, fixed = TRUE)), 1L)
  expect_false(res_approx$value$approximation_ok)
})

test_that("n_required_p_z rejects pooled = TRUE unless p0 = 0", {
  expect_error(
    n_required_p_z(
      p_a = c(0.30, 0.20),
      p0 = 0.10,
      pooled = TRUE,
      quiet = TRUE
    ),
    "pooled = TRUE"
  )
})

test_that("n_required_var_chisq allows n = 2 when sufficient", {
  out <- n_required_var_chisq(
    sigma_a = 100,
    sigma0 = 1,
    alpha = 0.05,
    beta_target = 0.20,
    alternative = "greater",
    quiet = TRUE
  )

  expect_s3_class(out, "sample_size_result")
  expect_equal(out$n, 2L)
  expect_true(out$achieved_power >= 0.80)
})

test_that("n_required_var_ratio_F checks one-sided direction early", {
  expect_error(
    n_required_var_ratio_F(
      sigma_a = c(2, 2),
      ratio0 = 1,
      alternative = "greater",
      quiet = TRUE
    ),
    "must be > ratio0"
  )
})
test_that("n_required_var_chisq stores two-sided chi-square region with both tails", {
  out <- n_required_var_chisq(
    sigma_a = 70,
    sigma0 = 60,
    alpha = 0.05,
    beta_target = 0.10,
    alternative = "two.sided",
    quiet = TRUE
  )

  chk <- power_var_chisq(
    sigma_a = 70,
    sigma0 = 60,
    n = out$n,
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  expect_identical(out$reject_region, chk$reject_region)
  expect_match(out$reject_region, "^Chi\\^2 <")
  expect_match(out$reject_region, "or Chi\\^2 >")
})

test_that("n_required_var_ratio_F stores two-sided F region with both tails", {
  out <- n_required_var_ratio_F(
    sigma_a = c(23, 50),
    ratio0 = 1,
    alpha = 0.05,
    beta_target = 0.10,
    alternative = "two.sided",
    quiet = TRUE
  )

  chk <- power_var_ratio_F(
    sigma_a = c(23, 50),
    ratio0 = 1,
    n = c(out$n1, out$n2),
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  expect_identical(out$reject_region, chk$reject_region)
  expect_match(out$reject_region, "^F <")
  expect_match(out$reject_region, "or F >")
})
