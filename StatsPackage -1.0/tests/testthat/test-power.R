#Power tests-------
test_that("default alternative is two-sided across the power family", {
  out_z <- power_z_mu(mu_a = 0, mu0 = 0, sigma = 1, n = 1, quiet = TRUE)
  out_t <- power_t_mu(mu_a = 0, mu0 = 0, sigma_true = 1, n = 10, quiet = TRUE)
  out_p <- power_p_z(p_a = 0.30, p0 = 0.30, n = 100, quiet = TRUE)
  out_v <- power_var_chisq(sigma_a = 60, sigma0 = 60, n = 25, quiet = TRUE)
  out_f <- power_var_ratio_F(sigma_a = c(2, 2), ratio0 = 1, n = c(20, 20), quiet = TRUE)

  expect_equal(out_z$alternative, "two.sided")
  expect_equal(out_t$alternative, "two.sided")
  expect_equal(out_p$alternative, "two.sided")
  expect_equal(out_v$alternative, "two.sided")
  expect_equal(out_f$alternative, "two.sided")
})

test_that("power_z_mu one-sample power matches the hand formula", {
  out <- power_z_mu(
    mu_a = 30500,
    mu0 = 30000,
    sigma = 1500,
    n = 16,
    alpha = 0.01,
    alternative = "greater",
    quiet = TRUE
  )

  se <- 1500 / sqrt(16)
  ncp <- (30500 - 30000) / se
  zcrit <- stats::qnorm(1 - 0.01)
  target <- 1 - stats::pnorm(zcrit, mean = ncp, sd = 1)

  expect_s3_class(out, "power_result")
  expect_equal(out$se, se, tolerance = 1e-12)
  expect_equal(out$ncp, ncp, tolerance = 1e-12)
  expect_equal(out$power, target, tolerance = 1e-12)
})

test_that("power_z_mu two-sample power matches the hand formula and recycles scalar n and sigma", {
  out <- power_z_mu(
    mu_a = c(65, 63),
    mu0 = 0,
    sigma = 3,
    n = 40,
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  se <- sqrt(3^2 / 40 + 3^2 / 40)
  ncp <- ((65 - 63) - 0) / se
  zcrit <- stats::qnorm(1 - 0.05 / 2)
  target <- stats::pnorm(-zcrit, mean = ncp, sd = 1) +
    (1 - stats::pnorm(zcrit, mean = ncp, sd = 1))

  expect_equal(out$n, c(40L, 40L))
  expect_equal(out$sigma, c(3, 3), tolerance = 1e-12)
  expect_equal(out$se, se, tolerance = 1e-12)
  expect_equal(out$power, target, tolerance = 1e-12)
})

test_that("power_z_mu under the null equals alpha and n = 1 is allowed", {
  out <- power_z_mu(
    mu_a = 0,
    mu0 = 0,
    sigma = 1,
    n = 1,
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  expect_equal(out$power, 0.05, tolerance = 1e-12)
})

test_that("power_z_mu rejects non-integer n and digits affect display only", {
  expect_error(
    power_z_mu(mu_a = 0, mu0 = 0, sigma = 1, n = 1.5, quiet = TRUE),
    "integer values"
  )

  out2 <- power_z_mu(mu_a = 30500, mu0 = 30000, sigma = 1500, n = 16, digits = 2, quiet = TRUE)
  out6 <- power_z_mu(mu_a = 30500, mu0 = 30000, sigma = 1500, n = 16, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    power_z_mu(mu_a = 30500, mu0 = 30000, sigma = 1500, n = 16, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    power_z_mu(mu_a = 30500, mu0 = 30000, sigma = 1500, n = 16, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$power, out6$power, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("power_t_mu one-sample power matches the hand formula", {
  out <- power_t_mu(
    mu_a = 2.0,
    mu0 = 3.0,
    sigma_true = 1.5,
    n = 16,
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  df <- 15
  se <- 1.5 / sqrt(16)
  ncp <- (2 - 3) / se
  tcrit <- stats::qt(1 - 0.05 / 2, df = df)
  target <- stats::pt(-tcrit, df = df, ncp = ncp) +
    (1 - stats::pt(tcrit, df = df, ncp = ncp))

  expect_s3_class(out, "power_result")
  expect_equal(out$df, df, tolerance = 1e-12)
  expect_equal(out$se, se, tolerance = 1e-12)
  expect_equal(out$ncp, ncp, tolerance = 1e-12)
  expect_equal(out$power, target, tolerance = 1e-12)
})

test_that("power_t_mu paired power matches the hand formula and warns when method is ignored", {
  expect_warning(
    out <- power_t_mu(
      mu_a = 1.5,
      mu0 = 0,
      sigma_true = 2,
      n = 10,
      alpha = 0.05,
      alternative = "greater",
      paired = TRUE,
      method = "pooled",
      quiet = TRUE
    ),
    "ignored"
  )

  df <- 9
  se <- 2 / sqrt(10)
  ncp <- 1.5 / se
  tcrit <- stats::qt(1 - 0.05, df = df)
  target <- 1 - stats::pt(tcrit, df = df, ncp = ncp)

  expect_equal(out$se, se, tolerance = 1e-12)
  expect_equal(out$ncp, ncp, tolerance = 1e-12)
  expect_equal(out$power, target, tolerance = 1e-12)
})

test_that("power_t_mu pooled two-sample power matches the hand formula", {
  out <- power_t_mu(
    mu_a = c(10, 8),
    mu0 = 0,
    sigma_true = 3,
    n = c(40, 35),
    alpha = 0.05,
    alternative = "two.sided",
    method = "pooled",
    quiet = TRUE
  )

  df <- 40 + 35 - 2
  se <- 3 * sqrt(1 / 40 + 1 / 35)
  ncp <- (10 - 8) / se
  tcrit <- stats::qt(1 - 0.05 / 2, df = df)
  target <- stats::pt(-tcrit, df = df, ncp = ncp) +
    (1 - stats::pt(tcrit, df = df, ncp = ncp))

  expect_equal(out$df, df, tolerance = 1e-12)
  expect_equal(out$se, se, tolerance = 1e-12)
  expect_equal(out$ncp, ncp, tolerance = 1e-12)
  expect_equal(out$power, target, tolerance = 1e-12)
})

test_that("power_t_mu Welch two-sample power matches the intended approximation", {
  out <- power_t_mu(
    mu_a = c(10, 8),
    mu0 = 0,
    sigma_true = c(3, 2.5),
    n = c(40, 35),
    alpha = 0.05,
    alternative = "greater",
    method = "welch",
    quiet = TRUE
  )

  se <- sqrt(3^2 / 40 + 2.5^2 / 35)
  df <- (3^2 / 40 + 2.5^2 / 35)^2 /
    ((3^2 / 40)^2 / 39 + (2.5^2 / 35)^2 / 34)
  ncp <- (10 - 8) / se
  tcrit <- stats::qt(1 - 0.05, df = df)
  target <- 1 - stats::pt(tcrit, df = df, ncp = ncp)

  expect_equal(out$df, df, tolerance = 1e-12)
  expect_equal(out$se, se, tolerance = 1e-12)
  expect_equal(out$ncp, ncp, tolerance = 1e-12)
  expect_equal(out$power, target, tolerance = 1e-12)
})

test_that("power_t_mu under the null equals alpha and digits affect display only", {
  out_null <- power_t_mu(
    mu_a = 0,
    mu0 = 0,
    sigma_true = 1.5,
    n = 16,
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  expect_equal(out_null$power, 0.05, tolerance = 1e-12)

  out2 <- power_t_mu(mu_a = 2, mu0 = 3, sigma_true = 1.5, n = 16, digits = 2, quiet = TRUE)
  out6 <- power_t_mu(mu_a = 2, mu0 = 3, sigma_true = 1.5, n = 16, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    power_t_mu(mu_a = 2, mu0 = 3, sigma_true = 1.5, n = 16, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    power_t_mu(mu_a = 2, mu0 = 3, sigma_true = 1.5, n = 16, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$power, out6$power, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("power_t_mu rejects non-integer n", {
  expect_error(
    power_t_mu(mu_a = 2, mu0 = 3, sigma_true = 1.5, n = 16.5, quiet = TRUE),
    "integer values"
  )
})

test_that("power_p_z one-sample normal-approximation power matches the hand formula", {
  out <- power_p_z(
    p_a = 0.25,
    p0 = 0.20,
    n = 541,
    alpha = 0.05,
    alternative = "greater",
    quiet = TRUE
  )

  se0 <- sqrt(0.20 * 0.80 / 541)
  sd_alt <- sqrt(0.25 * 0.75 / 541)
  zcrit <- stats::qnorm(1 - 0.05)
  phat_crit <- 0.20 + zcrit * se0
  target <- 1 - stats::pnorm(phat_crit, mean = 0.25, sd = sd_alt)

  expect_s3_class(out, "power_result")
  expect_equal(out$se0, se0, tolerance = 1e-12)
  expect_equal(out$sd_alt, sd_alt, tolerance = 1e-12)
  expect_equal(out$power, target, tolerance = 1e-12)
})

test_that("power_p_z one-sample under the null equals alpha when the approximation is regular", {
  out <- power_p_z(
    p_a = 0.30,
    p0 = 0.30,
    n = 100,
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  expect_equal(out$power, 0.05, tolerance = 1e-12)
})

test_that("power_p_z two-sample pooled power matches the hand formula", {
  out <- power_p_z(
    p_a = c(0.30, 0.20),
    p0 = 0,
    n = c(100, 120),
    alpha = 0.05,
    alternative = "greater",
    quiet = TRUE
  )

  delta_a <- 0.30 - 0.20
  p_pool <- (100 * 0.30 + 120 * 0.20) / (100 + 120)
  se0 <- sqrt(p_pool * (1 - p_pool) * (1 / 100 + 1 / 120))
  sd_alt <- sqrt(0.30 * 0.70 / 100 + 0.20 * 0.80 / 120)
  zcrit <- stats::qnorm(1 - 0.05)
  dcrit <- 0 + zcrit * se0
  target <- 1 - stats::pnorm(dcrit, mean = delta_a, sd = sd_alt)

  expect_true(out$pooled)
  expect_equal(out$p_pool, p_pool, tolerance = 1e-12)
  expect_equal(out$se0, se0, tolerance = 1e-12)
  expect_equal(out$sd_alt, sd_alt, tolerance = 1e-12)
  expect_equal(out$power, target, tolerance = 1e-12)
})

test_that("power_p_z two-sample unpooled two-sided branch uses the continuity-corrected region", {
  out <- power_p_z(
    p_a = c(0.30, 0.20),
    p0 = 0.05,
    n = c(100, 120),
    alpha = 0.05,
    alternative = "two.sided",
    pooled = FALSE,
    continuity = TRUE,
    quiet = TRUE
  )

  delta_a <- 0.30 - 0.20
  se0 <- sqrt(0.30 * 0.70 / 100 + 0.20 * 0.80 / 120)
  sd_alt <- se0
  zcrit <- stats::qnorm(1 - 0.05 / 2)
  cc <- 0.5 * (1 / 100 + 1 / 120)
  band <- zcrit * se0 + cc
  d_lo <- 0.05 - band
  d_hi <- 0.05 + band
  target <- stats::pnorm(d_lo, mean = delta_a, sd = sd_alt) +
    (1 - stats::pnorm(d_hi, mean = delta_a, sd = sd_alt))

  expect_false(out$pooled)
  expect_true(out$continuity_correction)
  expect_equal(out$cc, cc, tolerance = 1e-12)
  expect_equal(out$se0, se0, tolerance = 1e-12)
  expect_equal(out$sd_alt, sd_alt, tolerance = 1e-12)
  expect_equal(out$power, target, tolerance = 1e-12)
})

test_that("power_p_z enforces pooled policy, warns on ignored arguments and weak approximation", {
  expect_error(
    power_p_z(p_a = c(0.30, 0.20), p0 = 0.05, n = c(100, 120), pooled = TRUE, quiet = TRUE),
    "only valid when p0 = 0"
  )

  expect_warning(
    power_p_z(p_a = 0.25, p0 = 0.20, n = 100, pooled = TRUE, quiet = TRUE),
    "ignored"
  )

  expect_warning(
    power_p_z(p_a = c(0.30, 0.20), p0 = 0, n = c(100, 120),
              alternative = "greater", continuity = TRUE, quiet = TRUE),
    "ignored"
  )

  expect_warning(
    power_p_z(p_a = 0.02, p0 = 0.02, n = 20, quiet = TRUE),
    "normal approximation may be unreliable"
  )
})

test_that("power_p_z has an explicit boundary policy for degenerate alternative variance", {
  out <- suppressWarnings(
    power_p_z(
      p_a = 1,
      p0 = 0.20,
      n = 10,
      alpha = 0.05,
      alternative = "greater",
      quiet = TRUE
    )
  )

  expect_true(out$boundary_degenerate)
  expect_equal(out$sd_alt, 0, tolerance = 1e-12)
  expect_equal(out$power, 1, tolerance = 1e-12)
})

test_that("power_p_z digits affect display only", {
  out2 <- power_p_z(p_a = 0.25, p0 = 0.20, n = 541, digits = 2, quiet = TRUE)
  out6 <- power_p_z(p_a = 0.25, p0 = 0.20, n = 541, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    power_p_z(p_a = 0.25, p0 = 0.20, n = 541, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    power_p_z(p_a = 0.25, p0 = 0.20, n = 541, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$power, out6$power, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("power_var_chisq hand formula matches the implementation", {
  out <- power_var_chisq(
    sigma_a = 70,
    sigma0 = 60,
    n = 25,
    alpha = 0.05,
    alternative = "greater",
    quiet = TRUE
  )

  df <- 24
  k <- (70^2) / (60^2)
  chi_crit <- stats::qchisq(1 - 0.05, df = df)
  target <- 1 - stats::pchisq(chi_crit / k, df = df)

  expect_s3_class(out, "power_result")
  expect_equal(out$df, df, tolerance = 1e-12)
  expect_equal(out$k, k, tolerance = 1e-12)
  expect_equal(out$power, target, tolerance = 1e-12)
})

test_that("power_var_chisq under the null equals alpha and digits affect display only", {
  out_null <- power_var_chisq(
    sigma_a = 60,
    sigma0 = 60,
    n = 25,
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  expect_equal(out_null$power, 0.05, tolerance = 1e-12)

  out2 <- power_var_chisq(sigma_a = 70, sigma0 = 60, n = 25, digits = 2, quiet = TRUE)
  out6 <- power_var_chisq(sigma_a = 70, sigma0 = 60, n = 25, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    power_var_chisq(sigma_a = 70, sigma0 = 60, n = 25, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    power_var_chisq(sigma_a = 70, sigma0 = 60, n = 25, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$power, out6$power, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("power_var_chisq rejects non-integer n", {
  expect_error(
    power_var_chisq(sigma_a = 70, sigma0 = 60, n = 25.5, quiet = TRUE),
    "integer values"
  )
})

test_that("power_var_ratio_F hand formula matches the implementation", {
  out <- power_var_ratio_F(
    sigma_a = c(23, 50),
    ratio0 = 1,
    n = c(20, 20),
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  df1 <- 19
  df2 <- 19
  ratio_a <- (23^2) / (50^2)
  k <- ratio_a / 1
  flo <- stats::qf(0.05 / 2, df1 = df1, df2 = df2)
  fhi <- stats::qf(1 - 0.05 / 2, df1 = df1, df2 = df2)
  target <- stats::pf(flo / k, df1 = df1, df2 = df2) +
    (1 - stats::pf(fhi / k, df1 = df1, df2 = df2))

  expect_s3_class(out, "power_result")
  expect_equal(out$ratio_a, ratio_a, tolerance = 1e-12)
  expect_equal(out$k, k, tolerance = 1e-12)
  expect_equal(out$power, target, tolerance = 1e-12)
})

test_that("power_var_ratio_F under the null equals alpha and digits affect display only", {
  out_null <- power_var_ratio_F(
    sigma_a = c(2, 2),
    ratio0 = 1,
    n = c(20, 20),
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  expect_equal(out_null$power, 0.05, tolerance = 1e-12)

  out2 <- power_var_ratio_F(sigma_a = c(23, 50), ratio0 = 1, n = c(20, 20), digits = 2, quiet = TRUE)
  out6 <- power_var_ratio_F(sigma_a = c(23, 50), ratio0 = 1, n = c(20, 20), digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    power_var_ratio_F(sigma_a = c(23, 50), ratio0 = 1, n = c(20, 20), digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    power_var_ratio_F(sigma_a = c(23, 50), ratio0 = 1, n = c(20, 20), digits = 6, quiet = FALSE)
  )

  expect_equal(out2$power, out6$power, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("power_var_ratio_F rejects non-integer n", {
  expect_error(
    power_var_ratio_F(sigma_a = c(23, 50), ratio0 = 1, n = c(20, 20.5), quiet = TRUE),
    "integer values"
  )
})
test_that("power_var_chisq stores two-sided chi-square region with both tails", {
  out <- power_var_chisq(
    sigma_a = 70,
    sigma0 = 60,
    n = 25,
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  expected <- c(
    lower = stats::qchisq(0.025, df = 24),
    upper = stats::qchisq(0.975, df = 24)
  )

  expect_equal(out$chi_crit, expected, tolerance = 1e-12)
  expect_match(out$reject_region, "^Chi\\^2 <")
  expect_match(out$reject_region, "or Chi\\^2 >")
})

test_that("power_var_ratio_F stores two-sided F region with both tails", {
  out <- power_var_ratio_F(
    sigma_a = c(23, 50),
    ratio0 = 1,
    n = c(20, 20),
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  expected <- c(
    lower = stats::qf(0.025, df1 = 19, df2 = 19),
    upper = stats::qf(0.975, df1 = 19, df2 = 19)
  )

  expect_equal(out$f_crit, expected, tolerance = 1e-12)
  expect_match(out$reject_region, "^F <")
  expect_match(out$reject_region, "or F >")
})