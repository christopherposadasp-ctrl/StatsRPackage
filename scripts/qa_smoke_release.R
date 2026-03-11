root <- normalizePath(".", winslash = "/", mustWork = TRUE)
pkg_dir <- file.path(root, "StatsPackage -1.0")
qa_dir <- file.path(root, "qa")
log_path <- file.path(qa_dir, "smoke_test_output.txt")
results_path <- file.path(qa_dir, "smoke_test_results.csv")

dir.create(qa_dir, showWarnings = FALSE, recursive = TRUE)

sink(log_path, split = FALSE)
on.exit({
  sink(NULL)
}, add = TRUE)

cat("StatsPackage Deployment Smoke Test\n")
cat("Root:", root, "\n")
cat("Package directory:", pkg_dir, "\n")
cat("Log path:", log_path, "\n")
cat("Results path:", results_path, "\n\n")

if (!requireNamespace("StatsPackage", quietly = TRUE)) {
  if (!file.exists(pkg_dir)) {
    stop("StatsPackage is not installed and local package directory was not found: ", pkg_dir)
  }
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("StatsPackage is not installed. Install it first or install devtools for local fallback install.")
  }
  cat("StatsPackage not found in library. Installing from local package directory...\n")
  devtools::install(pkg_dir, upgrade = "never", quiet = TRUE, dependencies = FALSE)
}

suppressPackageStartupMessages(library(StatsPackage))

results <- data.frame(
  check = character(),
  status = character(),
  message = character(),
  stringsAsFactors = FALSE
)

record_result <- function(check_name, status, message = "") {
  results <<- rbind(
    results,
    data.frame(
      check = check_name,
      status = status,
      message = message,
      stringsAsFactors = FALSE
    )
  )
}

assert_true <- function(ok, message) {
  if (!isTRUE(ok)) {
    stop(message, call. = FALSE)
  }
}

assert_has_fields <- function(x, required_fields, object_name) {
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0L) {
    stop(
      object_name,
      " is missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }
}

assert_probability <- function(x, label) {
  assert_true(
    is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0 && x <= 1,
    paste0(label, " must be a single finite value in [0, 1].")
  )
}

run_check <- function(name, expr) {
  cat("##", name, "\n")
  tryCatch(
    {
      force(expr)
      record_result(name, "PASS", "")
      cat("PASS\n\n")
    },
    error = function(e) {
      msg <- conditionMessage(e)
      record_result(name, "FAIL", msg)
      cat("FAIL:", msg, "\n\n")
    }
  )
}

run_check("ci_mu_structure", {
  out <- StatsPackage::ci_mu(xbar = 12.4, n = 15, s = 3.2, quiet = TRUE)
  assert_true(inherits(out, "ci_result"), "ci_mu() should return an object inheriting from ci_result.")
  assert_has_fields(out, c("estimate", "ci", "se", "conf.level"), "ci_mu() result")
  assert_true(all(c("lower", "upper") %in% names(out$ci)), "ci_mu() result$ci must include lower and upper.")
  assert_true(is.finite(out$estimate), "ci_mu() estimate must be finite.")
  assert_true(is.finite(out$se) && out$se >= 0, "ci_mu() se must be finite and nonnegative.")
  assert_true(is.numeric(out$ci[["lower"]]) && is.numeric(out$ci[["upper"]]), "ci_mu() bounds must be numeric.")
  assert_true(out$ci[["lower"]] <= out$ci[["upper"]], "ci_mu() lower bound must be <= upper bound.")
})

run_check("p_test_nonzero_null_defaults", {
  out <- StatsPackage::p_test(x = c(35, 24), n = c(100, 100), p0 = 0.02, quiet = TRUE)
  assert_true(inherits(out, "htest_result"), "p_test() should return an object inheriting from htest_result.")
  assert_has_fields(
    out,
    c("z_stat", "p_value", "pooled", "continuity_correction"),
    "p_test() result"
  )
  assert_true(isFALSE(out$pooled), "p_test() with nonzero p0 should default to unpooled.")
  assert_true(isFALSE(out$continuity_correction), "p_test() should default continuity correction to FALSE.")
  assert_true(is.finite(out$z_stat), "p_test() z_stat must be finite.")
  assert_probability(out$p_value, "p_test() p_value")
})

run_check("var_test_chisq_two_sided_region", {
  out <- StatsPackage::var_test_chisq(
    s = 4.2,
    n = 12,
    sigma0 = 5,
    alternative = "two.sided",
    quiet = TRUE
  )
  assert_true(
    inherits(out, "htest_result"),
    "var_test_chisq() should return an object inheriting from htest_result."
  )
  assert_has_fields(out, c("chi_stat", "p_value", "reject_region"), "var_test_chisq() result")
  assert_true(grepl("^Chi\\^2 <", out$reject_region), "Reject region should start with lower-tail chi-square form.")
  assert_true(grepl("or Chi\\^2 >", out$reject_region), "Reject region should include the upper-tail chi-square form.")
  assert_true(is.finite(out$chi_stat), "var_test_chisq() chi_stat must be finite.")
  assert_probability(out$p_value, "var_test_chisq() p_value")
})

run_check("chisq_table_structure", {
  observed <- matrix(c(22, 18, 14, 26), nrow = 2, byrow = TRUE)
  out <- StatsPackage::chisq_table(observed, type = "independence", quiet = TRUE)
  assert_true(inherits(out, "htest_result"), "chisq_table() should return an object inheriting from htest_result.")
  assert_has_fields(out, c("expected", "chi_stat", "p_value"), "chisq_table() result")
  assert_true(is.matrix(out$expected), "chisq_table() expected must be a matrix.")
  assert_true(all(dim(out$expected) == dim(observed)), "chisq_table() expected dimensions must match observed table.")
  assert_true(is.finite(out$chi_stat), "chisq_table() chi_stat must be finite.")
  assert_probability(out$p_value, "chisq_table() p_value")
})

run_check("power_p_z_nonzero_null_defaults", {
  out <- StatsPackage::power_p_z(p_a = c(0.40, 0.25), p0 = 0.05, n = c(100, 100), quiet = TRUE)
  assert_true(inherits(out, "power_result"), "power_p_z() should return an object inheriting from power_result.")
  assert_has_fields(
    out,
    c("power", "pooled", "continuity_correction"),
    "power_p_z() result"
  )
  assert_true(isFALSE(out$pooled), "power_p_z() with nonzero p0 should default to unpooled.")
  assert_true(isFALSE(out$continuity_correction), "power_p_z() should default continuity correction to FALSE.")
  assert_probability(out$power, "power_p_z() power")
})

run_check("n_required_p_z_structure", {
  out <- StatsPackage::n_required_p_z(p_a = c(0.40, 0.25), p0 = 0.05, beta_target = 0.20, quiet = TRUE)
  assert_true(
    inherits(out, "sample_size_result"),
    "n_required_p_z() should return an object inheriting from sample_size_result."
  )
  assert_has_fields(
    out,
    c("n1", "n2", "achieved_power", "pooled", "continuity_correction"),
    "n_required_p_z() result"
  )
  assert_true(is.numeric(out$n1) && length(out$n1) == 1L && out$n1 >= 1, "n_required_p_z() n1 must be >= 1.")
  assert_true(is.numeric(out$n2) && length(out$n2) == 1L && out$n2 >= 1, "n_required_p_z() n2 must be >= 1.")
  assert_true(isFALSE(out$pooled), "n_required_p_z() with nonzero p0 should default to unpooled.")
  assert_true(isFALSE(out$continuity_correction), "n_required_p_z() should default continuity correction to FALSE.")
  assert_probability(out$achieved_power, "n_required_p_z() achieved_power")
})

write.csv(results, results_path, row.names = FALSE)

n_fail <- sum(results$status == "FAIL")
n_pass <- sum(results$status == "PASS")

cat("Smoke test complete.\n")
cat("Passed:", n_pass, "\n")
cat("Failed:", n_fail, "\n")
cat("Results CSV:", results_path, "\n")
cat("Execution log:", log_path, "\n")

if (n_fail > 0L) {
  stop("Smoke test failed: ", n_fail, " check(s) failed. See results/log under qa/.")
}

