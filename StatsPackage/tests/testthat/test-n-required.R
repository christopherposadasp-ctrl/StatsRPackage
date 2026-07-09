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

  out_min <- n_required_from_power(
    power_at_n = function(n) 0.99,
    target_power = 0.80,
    n_min = 7,
    n_max = 20,
    quiet = TRUE
  )

  expect_equal(out_min$n, 7L)
  expect_equal(out_min$bracket_upper, 7L)
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

  expect_error(
    n_required_from_power(function(n) 0.5, target_power = 0.80, n_min = 1, n_max = 8, quiet = TRUE),
    "target power was not reached"
  )

  expect_error(
    n_required_from_power(function(n) c(0.5, 0.6), target_power = 0.80, quiet = TRUE),
    "single finite value"
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

test_that("n_required_z_mu two-sample one-sided shortcut respects allocation and minimality", {
  out <- n_required_z_mu(
    mu_a = c(10, 8),
    mu0 = 0,
    sigma = c(3, 4),
    alpha = 0.05,
    beta_target = 0.10,
    alternative = "greater",
    n_ratio = 1.7,
    quiet = TRUE
  )

  chk <- power_z_mu(
    mu_a = c(10, 8),
    mu0 = 0,
    sigma = c(3, 4),
    n = c(out$n1, out$n2),
    alpha = 0.05,
    alternative = "greater",
    quiet = TRUE
  )

  prev <- power_z_mu(
    mu_a = c(10, 8),
    mu0 = 0,
    sigma = c(3, 4),
    n = c(out$n1 - 1L, max(1L, ceiling(1.7 * (out$n1 - 1L)))),
    alpha = 0.05,
    alternative = "greater",
    quiet = TRUE
  )

  expect_equal(out$n2, as.integer(ceiling(1.7 * out$n1)))
  expect_equal(out$achieved_power, chk$power, tolerance = 1e-12)
  expect_true(prev$power < out$target_power)
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

test_that("n_required_t_mu two-sample pooled and Welch designs meet target and are minimal", {
  out_pooled <- n_required_t_mu(
    mu_a = c(9, 7),
    mu0 = 0,
    sigma_true = 3,
    alpha = 0.05,
    beta_target = 0.20,
    alternative = "greater",
    method = "pooled",
    n_ratio = 1.4,
    quiet = TRUE
  )

  chk_pooled <- power_t_mu(
    mu_a = c(9, 7),
    mu0 = 0,
    sigma_true = 3,
    n = c(out_pooled$n1, out_pooled$n2),
    alpha = 0.05,
    alternative = "greater",
    method = "pooled",
    quiet = TRUE
  )

  prev_pooled <- power_t_mu(
    mu_a = c(9, 7),
    mu0 = 0,
    sigma_true = 3,
    n = c(out_pooled$n1 - 1L, max(2L, ceiling(1.4 * (out_pooled$n1 - 1L)))),
    alpha = 0.05,
    alternative = "greater",
    method = "pooled",
    quiet = TRUE
  )

  out_welch <- n_required_t_mu(
    mu_a = c(7, 10),
    mu0 = 0,
    sigma_true = c(3, 5),
    alpha = 0.04,
    beta_target = 0.20,
    alternative = "less",
    method = "welch",
    n_ratio = 1.25,
    quiet = TRUE
  )

  chk_welch <- power_t_mu(
    mu_a = c(7, 10),
    mu0 = 0,
    sigma_true = c(3, 5),
    n = c(out_welch$n1, out_welch$n2),
    alpha = 0.04,
    alternative = "less",
    method = "welch",
    quiet = TRUE
  )

  prev_welch <- power_t_mu(
    mu_a = c(7, 10),
    mu0 = 0,
    sigma_true = c(3, 5),
    n = c(out_welch$n1 - 1L, max(2L, ceiling(1.25 * (out_welch$n1 - 1L)))),
    alpha = 0.04,
    alternative = "less",
    method = "welch",
    quiet = TRUE
  )

  expect_equal(out_pooled$n2, as.integer(ceiling(1.4 * out_pooled$n1)))
  expect_equal(out_pooled$achieved_power, chk_pooled$power, tolerance = 1e-12)
  expect_true(prev_pooled$power < out_pooled$target_power)
  expect_equal(out_welch$n2, as.integer(ceiling(1.25 * out_welch$n1)))
  expect_equal(out_welch$achieved_power, chk_welch$power, tolerance = 1e-12)
  expect_true(prev_welch$power < out_welch$target_power)
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

test_that("n_required_p_z two-sample defaults and continuity match power_p_z", {
  out_pooled <- suppressWarnings(
    n_required_p_z(
      p_a = c(0.30, 0.20),
      p0 = 0,
      alpha = 0.05,
      beta_target = 0.20,
      alternative = "greater",
      n_ratio = 1.3,
      quiet = TRUE
    )
  )

  chk_pooled <- suppressWarnings(
    power_p_z(
      p_a = c(0.30, 0.20),
      p0 = 0,
      n = c(out_pooled$n1, out_pooled$n2),
      alpha = 0.05,
      alternative = "greater",
      pooled = TRUE,
      quiet = TRUE
    )
  )

  prev_pooled <- suppressWarnings(
    power_p_z(
      p_a = c(0.30, 0.20),
      p0 = 0,
      n = c(out_pooled$n1 - 1L, max(1L, ceiling(1.3 * (out_pooled$n1 - 1L)))),
      alpha = 0.05,
      alternative = "greater",
      pooled = TRUE,
      quiet = TRUE
    )
  )

  out_unpooled <- suppressWarnings(
    n_required_p_z(
      p_a = c(0.55, 0.42),
      p0 = 0.08,
      alpha = 0.05,
      beta_target = 0.20,
      alternative = "greater",
      n_ratio = 0.75,
      quiet = TRUE
    )
  )

  chk_unpooled <- suppressWarnings(
    power_p_z(
      p_a = c(0.55, 0.42),
      p0 = 0.08,
      n = c(out_unpooled$n1, out_unpooled$n2),
      alpha = 0.05,
      alternative = "greater",
      pooled = FALSE,
      quiet = TRUE
    )
  )

  out_cc <- suppressWarnings(
    n_required_p_z(
      p_a = c(0.40, 0.32),
      p0 = 0,
      alpha = 0.05,
      beta_target = 0.20,
      alternative = "two.sided",
      continuity = TRUE,
      quiet = TRUE
    )
  )

  chk_cc <- suppressWarnings(
    power_p_z(
      p_a = c(0.40, 0.32),
      p0 = 0,
      n = c(out_cc$n1, out_cc$n2),
      alpha = 0.05,
      alternative = "two.sided",
      pooled = TRUE,
      continuity = TRUE,
      quiet = TRUE
    )
  )

  expect_true(out_pooled$pooled)
  expect_equal(out_pooled$n2, as.integer(ceiling(1.3 * out_pooled$n1)))
  expect_equal(out_pooled$achieved_power, chk_pooled$power, tolerance = 1e-12)
  expect_true(prev_pooled$power < out_pooled$target_power)
  expect_false(out_unpooled$pooled)
  expect_equal(out_unpooled$n2, as.integer(ceiling(0.75 * out_unpooled$n1)))
  expect_equal(out_unpooled$achieved_power, chk_unpooled$power, tolerance = 1e-12)
  expect_true(out_cc$continuity)
  expect_equal(out_cc$achieved_power, chk_cc$power, tolerance = 1e-12)
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

test_that("n_required_var_chisq less-tail design meets target and is minimal", {
  out <- n_required_var_chisq(
    sigma_a = 45,
    sigma0 = 60,
    alpha = 0.025,
    beta_target = 0.20,
    alternative = "less",
    quiet = TRUE
  )

  chk <- power_var_chisq(
    sigma_a = 45,
    sigma0 = 60,
    n = out$n,
    alpha = 0.025,
    alternative = "less",
    quiet = TRUE
  )

  prev <- power_var_chisq(
    sigma_a = 45,
    sigma0 = 60,
    n = out$n - 1L,
    alpha = 0.025,
    alternative = "less",
    quiet = TRUE
  )

  expect_equal(out$achieved_power, chk$power, tolerance = 1e-12)
  expect_true(prev$power < out$target_power)
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

test_that("n_required_var_ratio_F honors non-1 ratio0 allocation and minimality", {
  out <- n_required_var_ratio_F(
    sigma_a = c(7, 4),
    ratio0 = 2,
    alpha = 0.04,
    beta_target = 0.25,
    alternative = "greater",
    n_ratio = 1.6,
    quiet = TRUE
  )

  chk <- power_var_ratio_F(
    sigma_a = c(7, 4),
    ratio0 = 2,
    n = c(out$n1, out$n2),
    alpha = 0.04,
    alternative = "greater",
    quiet = TRUE
  )

  prev <- power_var_ratio_F(
    sigma_a = c(7, 4),
    ratio0 = 2,
    n = c(out$n1 - 1L, max(2L, ceiling(1.6 * (out$n1 - 1L)))),
    alpha = 0.04,
    alternative = "greater",
    quiet = TRUE
  )

  expect_equal(out$n2, as.integer(ceiling(1.6 * out$n1)))
  expect_equal(out$achieved_power, chk$power, tolerance = 1e-12)
  expect_true(prev$power < out$target_power)
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
