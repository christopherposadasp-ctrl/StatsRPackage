################################
# TESts-----
test_that("default alternative is two-sided across the hypothesis-test family", {
  out_z <- z_test_mu(xbar = 2.25, mu0 = 3, sigma = 1.5, n = 36, quiet = TRUE)
  out_t <- t_test_mu(xbar = 90, mu0 = 0, s = 100, n = 16, quiet = TRUE)
  out_p <- p_test(x = 30, n = 100, p0 = 0.40, quiet = TRUE)
  out_v <- var_test_chisq(s = 42.2, n = 10, sigma0 = 60, quiet = TRUE)

  expect_equal(out_z$alternative, "two.sided")
  expect_equal(out_t$alternative, "two.sided")
  expect_equal(out_p$alternative, "two.sided")
  expect_equal(out_v$alternative, "two.sided")
})

test_that("z_test_mu one-sample statistics match the hand formula", {
  out <- z_test_mu(
    xbar = 2.25,
    mu0 = 3,
    sigma = 1.5,
    n = 36,
    alpha = 0.05,
    alternative = "less",
    quiet = TRUE
  )

  se <- 1.5 / sqrt(36)
  z_stat <- (2.25 - 3) / se
  p_val <- stats::pnorm(z_stat)

  expect_s3_class(out, "htest_result")
  expect_equal(out$se, se, tolerance = 1e-12)
  expect_equal(out$z_stat, z_stat, tolerance = 1e-12)
  expect_equal(out$p_value, p_val, tolerance = 1e-12)
  expect_true(out$reject)
})

test_that("z_test_mu two-sample statistics match the hand formula", {
  out <- z_test_mu(
    xbar = c(10, 8),
    mu0 = 0,
    sigma = c(2, 3),
    n = c(25, 36),
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  se <- sqrt(2^2 / 25 + 3^2 / 36)
  z_stat <- (10 - 8 - 0) / se
  p_val <- 2 * stats::pnorm(-abs(z_stat))

  expect_equal(out$se, se, tolerance = 1e-12)
  expect_equal(out$z_stat, z_stat, tolerance = 1e-12)
  expect_equal(out$p_value, p_val, tolerance = 1e-12)
})

test_that("z_test_mu stores critical and estimate-critical boundaries", {
  out_less <- z_test_mu(
    xbar = 2.25,
    mu0 = 3,
    sigma = 1.5,
    n = 36,
    alpha = 0.05,
    alternative = "less",
    quiet = TRUE
  )

  se_less <- 1.5 / sqrt(36)
  crit_less <- stats::qnorm(0.05)

  expect_equal(out_less$crit, crit_less, tolerance = 1e-12)
  expect_equal(out_less$estimate_crit, 3 + crit_less * se_less, tolerance = 1e-12)
  expect_match(out_less$reject_region, "Z <")
  expect_match(out_less$reject_region, "xbar <")

  out_two <- z_test_mu(
    xbar = c(10, 8),
    mu0 = 0,
    sigma = c(2, 3),
    n = c(25, 36),
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  se_two <- sqrt(2^2 / 25 + 3^2 / 36)
  crit_two <- stats::qnorm(0.975)
  target_est_crit <- c(lower = -crit_two * se_two, upper = crit_two * se_two)

  expect_equal(out_two$crit, c(lower = -crit_two, upper = crit_two), tolerance = 1e-12)
  expect_equal(out_two$estimate_crit, target_est_crit, tolerance = 1e-12)
  expect_match(out_two$reject_region, "\\|Z\\| >")
  expect_match(out_two$reject_region, "xbar1 - xbar2 <")
})

test_that("z_test_mu keeps extreme greater-tail p-values nonzero", {
  out <- z_test_mu(
    xbar = 9,
    mu0 = 0,
    sigma = 1,
    n = 1,
    alpha = 0.05,
    alternative = "greater",
    quiet = TRUE
  )

  target <- stats::pnorm(out$z_stat, lower.tail = FALSE)

  expect_equal(out$p_value, target, tolerance = 0)
  expect_gt(out$p_value, 0)
})

test_that("z_test_mu rejects invalid sigma alias usage and non-integer n", {
  expect_error(
    z_test_mu(xbar = 2.25, mu0 = 3, sigma = 1.5, s = 1.5, n = 36, quiet = TRUE),
    "only one of sigma or s"
  )

  expect_error(
    z_test_mu(xbar = 2.25, mu0 = 3, sigma = 1.5, n = 36.5, quiet = TRUE),
    "integer values"
  )
})

test_that("z_test_mu digits affect display only", {
  out2 <- z_test_mu(xbar = 2.23456, mu0 = 3, sigma = 1.23456, n = 11, digits = 2, quiet = TRUE)
  out6 <- z_test_mu(xbar = 2.23456, mu0 = 3, sigma = 1.23456, n = 11, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    z_test_mu(xbar = 2.23456, mu0 = 3, sigma = 1.23456, n = 11, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    z_test_mu(xbar = 2.23456, mu0 = 3, sigma = 1.23456, n = 11, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$z_stat, out6$z_stat, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("t_test_mu one-sample statistics match the hand formula", {
  out <- t_test_mu(
    xbar = 90,
    mu0 = 0,
    s = 100,
    n = 16,
    alpha = 0.02,
    alternative = "two.sided",
    quiet = TRUE
  )

  se <- 100 / sqrt(16)
  t_stat <- (90 - 0) / se
  p_val <- 2 * stats::pt(-abs(t_stat), df = 15)

  expect_s3_class(out, "htest_result")
  expect_equal(out$se, se, tolerance = 1e-12)
  expect_equal(out$t_stat, t_stat, tolerance = 1e-12)
  expect_equal(out$df, 15, tolerance = 1e-12)
  expect_equal(out$p_value, p_val, tolerance = 1e-12)
})

test_that("t_test_mu Welch two-sample statistics match the hand formula", {
  out <- t_test_mu(
    xbar = c(115.7, 129.3),
    mu0 = 0,
    s = c(5.03, 5.38),
    n = c(6, 6),
    alpha = 0.05,
    alternative = "two.sided",
    var.equal = FALSE,
    quiet = TRUE
  )

  se <- sqrt(5.03^2 / 6 + 5.38^2 / 6)
  t_stat <- (115.7 - 129.3 - 0) / se
  df <- (5.03^2 / 6 + 5.38^2 / 6)^2 /
    ((5.03^2 / 6)^2 / 5 + (5.38^2 / 6)^2 / 5)
  p_val <- 2 * stats::pt(-abs(t_stat), df = df)

  expect_equal(out$se, se, tolerance = 1e-12)
  expect_equal(out$t_stat, t_stat, tolerance = 1e-12)
  expect_equal(out$df, df, tolerance = 1e-12)
  expect_equal(out$p_value, p_val, tolerance = 1e-12)
})

test_that("t_test_mu stores critical and estimate-critical boundaries", {
  out_one <- t_test_mu(
    xbar = 90,
    mu0 = 0,
    s = 100,
    n = 16,
    alpha = 0.02,
    alternative = "two.sided",
    quiet = TRUE
  )

  se_one <- 100 / sqrt(16)
  crit_one <- stats::qt(0.99, df = 15)
  target_est_crit <- c(lower = -crit_one * se_one, upper = crit_one * se_one)

  expect_equal(out_one$crit, c(lower = -crit_one, upper = crit_one), tolerance = 1e-12)
  expect_equal(out_one$estimate_crit, target_est_crit, tolerance = 1e-12)
  expect_match(out_one$reject_region, "\\|T\\| >")

  out_paired <- suppressWarnings(
    t_test_mu(
      xbar = 1.5,
      mu0 = 0,
      s = 2,
      n = 10,
      alpha = 0.05,
      alternative = "greater",
      paired = TRUE,
      var.equal = TRUE,
      quiet = TRUE
    )
  )

  se_paired <- 2 / sqrt(10)
  crit_paired <- stats::qt(0.95, df = 9)

  expect_equal(out_paired$crit, crit_paired, tolerance = 1e-12)
  expect_equal(out_paired$estimate_crit, crit_paired * se_paired, tolerance = 1e-12)
  expect_match(out_paired$reject_region, "T >")
  expect_match(out_paired$reject_region, "dbar >")
})

test_that("t_test_mu pooled two-sample statistics match the hand formula", {
  out <- t_test_mu(
    xbar = c(10, 8),
    mu0 = 0,
    s = c(2, 3),
    n = c(25, 36),
    alpha = 0.05,
    alternative = "greater",
    var.equal = TRUE,
    quiet = TRUE
  )

  df <- 25 + 36 - 2
  sp2 <- ((25 - 1) * 2^2 + (36 - 1) * 3^2) / df
  se <- sqrt(sp2 * (1 / 25 + 1 / 36))
  t_stat <- (10 - 8 - 0) / se
  p_val <- stats::pt(t_stat, df = df, lower.tail = FALSE)

  expect_equal(out$sp2, sp2, tolerance = 1e-12)
  expect_equal(out$se, se, tolerance = 1e-12)
  expect_equal(out$t_stat, t_stat, tolerance = 1e-12)
  expect_equal(out$p_value, p_val, tolerance = 1e-12)
})

test_that("t_test_mu paired branch matches the hand formula and warns when var.equal is ignored", {
  expect_warning(
    out <- t_test_mu(
      xbar = 1.5,
      mu0 = 0,
      s = 2,
      n = 10,
      alpha = 0.05,
      alternative = "greater",
      paired = TRUE,
      var.equal = TRUE,
      quiet = TRUE
    ),
    "ignored"
  )

  se <- 2 / sqrt(10)
  t_stat <- 1.5 / se
  p_val <- stats::pt(t_stat, df = 9, lower.tail = FALSE)

  expect_equal(out$se, se, tolerance = 1e-12)
  expect_equal(out$t_stat, t_stat, tolerance = 1e-12)
  expect_equal(out$p_value, p_val, tolerance = 1e-12)
})

test_that("t_test_mu rejects invalid n and digits affect display only", {
  expect_error(
    t_test_mu(xbar = 90, mu0 = 0, s = 100, n = 16.5, quiet = TRUE),
    "integer values"
  )

  out2 <- t_test_mu(xbar = 90, mu0 = 0, s = 100, n = 16, digits = 2, quiet = TRUE)
  out6 <- t_test_mu(xbar = 90, mu0 = 0, s = 100, n = 16, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    t_test_mu(xbar = 90, mu0 = 0, s = 100, n = 16, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    t_test_mu(xbar = 90, mu0 = 0, s = 100, n = 16, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$t_stat, out6$t_stat, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("p_test one-sample exact p-value matches binom.test", {
  out <- p_test(
    x = 30,
    n = 100,
    p0 = 0.40,
    alpha = 0.05,
    alternative = "less",
    quiet = TRUE
  )

  bt <- stats::binom.test(30, 100, p = 0.40, alternative = "less", conf.level = 0.95)

  expect_s3_class(out, "htest_result")
  expect_equal(out$phat, 0.30, tolerance = 1e-12)
  expect_equal(out$p_value, bt$p.value, tolerance = 1e-12)
  expect_equal(out$conf_int, setNames(unname(bt$conf.int), c("lower", "upper")), tolerance = 1e-12)
})

test_that("p_test one-sample exact p-value matches binom.test for all alternatives", {
  for (alt in c("two.sided", "less", "greater")) {
    out <- p_test(
      x = 30,
      n = 100,
      p0 = 0.40,
      alpha = 0.05,
      alternative = alt,
      quiet = TRUE
    )

    bt <- stats::binom.test(30, 100, p = 0.40, alternative = alt, conf.level = 0.95)

    expect_equal(out$p_value, bt$p.value, tolerance = 1e-12)
    expect_equal(out$conf_int, setNames(unname(bt$conf.int), c("lower", "upper")), tolerance = 1e-12)
    expect_equal(out$statistic, 30)
  }
})

test_that("p_test two-sample pooled z test with p0 = 0 matches the hand formula", {
  out <- p_test(
    x = c(24, 8),
    n = c(55, 52),
    p0 = 0,
    alpha = 0.05,
    alternative = "greater",
    quiet = TRUE
  )

  phat1 <- 24 / 55
  phat2 <- 8 / 52
  p_pool <- (24 + 8) / (55 + 52)
  se0 <- sqrt(p_pool * (1 - p_pool) * (1 / 55 + 1 / 52))
  z_stat <- (phat1 - phat2 - 0) / se0
  p_val <- stats::pnorm(z_stat, lower.tail = FALSE)

  expect_true(out$pooled)
  expect_equal(out$p_pool, p_pool, tolerance = 1e-12)
  expect_equal(out$se0, se0, tolerance = 1e-12)
  expect_equal(out$z_stat, z_stat, tolerance = 1e-12)
  expect_equal(out$p_value, p_val, tolerance = 1e-12)
})

test_that("p_test two-sample nonzero-null default is unpooled and uncorrected", {
  out <- p_test(
    x = c(24, 8),
    n = c(55, 52),
    p0 = 0.1,
    alpha = 0.05,
    alternative = "two.sided",
    pooled = FALSE,
    quiet = TRUE
  )

  phat1 <- 24 / 55
  phat2 <- 8 / 52
  se0 <- sqrt(phat1 * (1 - phat1) / 55 + phat2 * (1 - phat2) / 52)
  z_stat <- ((phat1 - phat2) - 0.1) / se0
  p_val <- 2 * stats::pnorm(-abs(z_stat))

  expect_false(out$pooled)
  expect_false(out$continuity)
  expect_false(out$continuity_correction)
  expect_equal(out$se0, se0, tolerance = 1e-12)
  expect_equal(out$z_stat, z_stat, tolerance = 1e-12)
  expect_equal(out$p_value, p_val, tolerance = 1e-12)
})

test_that("p_test applies continuity correction when explicitly requested", {
  out <- p_test(
    x = c(24, 8),
    n = c(55, 52),
    p0 = 0.1,
    alpha = 0.05,
    alternative = "two.sided",
    pooled = FALSE,
    continuity = TRUE,
    quiet = TRUE
  )

  phat1 <- 24 / 55
  phat2 <- 8 / 52
  diff_hat <- phat1 - phat2
  se0 <- sqrt(phat1 * (1 - phat1) / 55 + phat2 * (1 - phat2) / 52)
  cc <- 0.5 * (1 / 55 + 1 / 52)
  num_cc <- max(abs(diff_hat - 0.1) - cc, 0)
  z_stat <- sign(diff_hat - 0.1) * (num_cc / se0)
  p_val <- 2 * stats::pnorm(-abs(z_stat))

  expect_false(out$pooled)
  expect_true(out$continuity)
  expect_true(out$continuity_correction)
  expect_equal(out$se0, se0, tolerance = 1e-12)
  expect_equal(out$z_stat, z_stat, tolerance = 1e-12)
  expect_equal(out$p_value, p_val, tolerance = 1e-12)
})

test_that("p_test continuity correction handles less and greater alternatives", {
  phat1 <- 24 / 55
  phat2 <- 8 / 52
  diff_hat <- phat1 - phat2
  delta0 <- 0.1
  se0 <- sqrt(phat1 * (1 - phat1) / 55 + phat2 * (1 - phat2) / 52)
  cc <- 0.5 * (1 / 55 + 1 / 52)

  out_less <- p_test(
    x = c(24, 8),
    n = c(55, 52),
    p0 = delta0,
    alpha = 0.05,
    alternative = "less",
    pooled = FALSE,
    continuity = TRUE,
    quiet = TRUE
  )

  crit_less <- stats::qnorm(0.05)
  z_less <- (diff_hat - delta0 + cc) / se0

  expect_equal(out_less$cc, cc, tolerance = 1e-12)
  expect_equal(out_less$z_stat, z_less, tolerance = 1e-12)
  expect_equal(out_less$p_value, stats::pnorm(z_less), tolerance = 1e-12)
  expect_equal(out_less$crit, crit_less, tolerance = 1e-12)
  expect_equal(out_less$estimate_crit, delta0 + crit_less * se0 - cc, tolerance = 1e-12)

  out_greater <- p_test(
    x = c(24, 8),
    n = c(55, 52),
    p0 = delta0,
    alpha = 0.05,
    alternative = "greater",
    pooled = FALSE,
    continuity = TRUE,
    quiet = TRUE
  )

  crit_greater <- stats::qnorm(0.95)
  z_greater <- (diff_hat - delta0 - cc) / se0

  expect_equal(out_greater$cc, cc, tolerance = 1e-12)
  expect_equal(out_greater$z_stat, z_greater, tolerance = 1e-12)
  expect_equal(out_greater$p_value, stats::pnorm(z_greater, lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(out_greater$crit, crit_greater, tolerance = 1e-12)
  expect_equal(out_greater$estimate_crit, delta0 + crit_greater * se0 + cc, tolerance = 1e-12)
})

test_that("p_test enforces integer counts, forbids pooled nonzero-null tests, and warns on weak approximation", {
  expect_error(
    p_test(x = 10.5, n = 20, p0 = 0.4, quiet = TRUE),
    "integer values"
  )

  expect_error(
  p_test(x = c(24, 8), n = c(55, 52), p0 = 0.1, pooled = TRUE, quiet = TRUE),
  "p0 = 0"
)

  expect_warning(
    p_test(x = c(1, 1), n = c(10, 10), p0 = 0, alternative = "greater", quiet = TRUE),
    "normal approximation may be unreliable"
  )
})

test_that("p_test digits affect display only", {
  out2 <- p_test(x = c(24, 8), n = c(55, 52), p0 = 0, alternative = "greater", digits = 2, quiet = TRUE)
  out6 <- p_test(x = c(24, 8), n = c(55, 52), p0 = 0, alternative = "greater", digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    p_test(x = c(24, 8), n = c(55, 52), p0 = 0, alternative = "greater", digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    p_test(x = c(24, 8), n = c(55, 52), p0 = 0, alternative = "greater", digits = 6, quiet = FALSE)
  )

  expect_equal(out2$z_stat, out6$z_stat, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("var_test_chisq one-sample chi-square statistics match the hand formula", {
  out <- var_test_chisq(
    s = 42.2,
    n = 10,
    sigma0 = 60,
    alpha = 0.05,
    alternative = "less",
    quiet = TRUE
  )

  df <- 9
  stat <- df * 42.2^2 / 60^2
  p_val <- stats::pchisq(stat, df = df)

  expect_s3_class(out, "htest_result")
  expect_equal(out$df, df, tolerance = 1e-12)
  expect_equal(out$chi_stat, stat, tolerance = 1e-12)
  expect_equal(out$p_value, p_val, tolerance = 1e-12)
})

test_that("var_test_chisq two-sample F statistics match the hand formula", {
  out <- var_test_chisq(
    s = c(42.2, 35.0),
    n = c(10, 12),
    ratio0 = 1,
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  ratio_hat <- 42.2^2 / 35^2
  stat <- ratio_hat / 1
  p_left <- stats::pf(stat, df1 = 9, df2 = 11)
  p_val <- min(1, 2 * min(p_left, 1 - p_left))

  expect_equal(out$ratio_hat, ratio_hat, tolerance = 1e-12)
  expect_equal(out$f_stat, stat, tolerance = 1e-12)
  expect_equal(out$p_value, p_val, tolerance = 1e-12)
})

test_that("var_test_chisq stores tail-specific critical values", {
  out_greater <- var_test_chisq(
    s = 42.2,
    n = 10,
    sigma0 = 60,
    alpha = 0.05,
    alternative = "greater",
    quiet = TRUE
  )

  df <- 9
  stat <- df * 42.2^2 / 60^2

  expect_equal(out_greater$chi_stat, stat, tolerance = 1e-12)
  expect_equal(out_greater$p_value, stats::pchisq(stat, df = df, lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(out_greater$crit, stats::qchisq(0.95, df = df), tolerance = 1e-12)
  expect_match(out_greater$reject_region, "Chi\\^2 >")

  out_two <- var_test_chisq(
    s = 42.2,
    n = 10,
    sigma0 = 60,
    alpha = 0.05,
    alternative = "two.sided",
    quiet = TRUE
  )

  p_left <- stats::pchisq(stat, df = df)
  p_right <- stats::pchisq(stat, df = df, lower.tail = FALSE)
  target_crit <- c(
    lower = stats::qchisq(0.025, df = df),
    upper = stats::qchisq(0.975, df = df)
  )

  expect_equal(out_two$p_value, min(1, 2 * min(p_left, p_right)), tolerance = 1e-12)
  expect_equal(out_two$crit, target_crit, tolerance = 1e-12)
  expect_match(out_two$reject_region, "^Chi\\^2 <")
  expect_match(out_two$reject_region, "or Chi\\^2 >")
})

test_that("var_test_chisq two-sample F test supports non-1 ratio0", {
  out <- var_test_chisq(
    s = c(42.2, 35.0),
    n = c(10, 12),
    ratio0 = 1.5,
    alpha = 0.05,
    alternative = "greater",
    quiet = TRUE
  )

  ratio_hat <- 42.2^2 / 35^2
  stat <- ratio_hat / 1.5
  crit <- stats::qf(0.95, df1 = 9, df2 = 11)

  expect_equal(out$ratio0, 1.5, tolerance = 1e-12)
  expect_equal(out$ratio_hat, ratio_hat, tolerance = 1e-12)
  expect_equal(out$f_stat, stat, tolerance = 1e-12)
  expect_equal(out$p_value, stats::pf(stat, df1 = 9, df2 = 11, lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(out$crit, crit, tolerance = 1e-12)
  expect_match(out$reject_region, "F >")
})

test_that("var_test_chisq allows one-sample s = 0, warns on ignored sigma0 in the two-sample branch, and errors on zero denominator SD", {
  out_zero <- var_test_chisq(
    s = 0,
    n = 10,
    sigma0 = 1,
    alpha = 0.05,
    alternative = "less",
    quiet = TRUE
  )

  expect_equal(out_zero$chi_stat, 0, tolerance = 1e-12)
  expect_equal(out_zero$p_value, 0, tolerance = 1e-12)

  expect_warning(
    var_test_chisq(s = c(4.2, 3.1), n = c(12, 10), sigma0 = 60, ratio0 = 1, quiet = TRUE),
    "ignored"
  )

  expect_error(
    var_test_chisq(s = c(4.2, 0), n = c(12, 10), ratio0 = 1, quiet = TRUE),
    "undefined when s\\[2\\] = 0"
  )
})

test_that("var_test_chisq rejects non-integer n and digits affect display only", {
  expect_error(
    var_test_chisq(s = 4.2, n = 12.5, sigma0 = 1, quiet = TRUE),
    "integer values"
  )



  out2 <- var_test_chisq(s = 42.2, n = 10, sigma0 = 60, digits = 2, quiet = TRUE)
  out6 <- var_test_chisq(s = 42.2, n = 10, sigma0 = 60, digits = 6, quiet = TRUE)

  txt2 <- utils::capture.output(
    var_test_chisq(s = 42.2, n = 10, sigma0 = 60, digits = 2, quiet = FALSE)
  )
  txt6 <- utils::capture.output(
    var_test_chisq(s = 42.2, n = 10, sigma0 = 60, digits = 6, quiet = FALSE)
  )

  expect_equal(out2$chi_stat, out6$chi_stat, tolerance = 1e-12)
  expect_false(identical(txt2, txt6))
})

test_that("var_test_chisq prints two-sided chi-square region with both tails", {
  out <- var_test_chisq(s = 4.2, n = 12, sigma0 = 5, quiet = TRUE)
  expect_match(out$reject_region, "^Chi\\^2 <")
  expect_match(out$reject_region, "or Chi\\^2 >")
})

test_that("var_test_chisq prints two-sided F region with both tails", {
  out <- var_test_chisq(s = c(4.2, 5.0), n = c(12, 10), ratio0 = 1, quiet = TRUE)
  expect_match(out$reject_region, "^F <")
  expect_match(out$reject_region, "or F >")
})
