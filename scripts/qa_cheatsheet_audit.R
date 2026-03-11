root <- normalizePath(".", winslash = "/", mustWork = TRUE)
pkg_dir <- file.path(root, "StatsPackage -1.0")
cheat_path <- file.path(root, "2.CheatsheetV4.r")
qa_dir <- file.path(root, "qa")
log_path <- file.path(qa_dir, "cheatsheet_audit_output.txt")
issues_path <- file.path(qa_dir, "cheatsheet_audit_issues.csv")

dir.create(qa_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(pkg_dir)) {
  stop("Package directory not found: ", pkg_dir)
}

if (!file.exists(cheat_path)) {
  stop("Cheat sheet file not found: ", cheat_path)
}

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("devtools is required for the cheat sheet audit.")
}

devtools::install(pkg_dir, upgrade = "never", quiet = TRUE, dependencies = FALSE)
suppressPackageStartupMessages(library(StatsPackage))

lines <- readLines(cheat_path, warn = FALSE, encoding = "UTF-8")
start_line <- grep("^defs <- list\\(", lines)[1]
if (is.na(start_line)) {
  start_line <- 1
}

exprs <- parse(text = lines[start_line:length(lines)], keep.source = TRUE)

issues <- data.frame(
  line = integer(),
  type = character(),
  message = character(),
  expression = character(),
  stringsAsFactors = FALSE
)

sink(log_path, split = FALSE)
on.exit({
  sink(NULL)
}, add = TRUE)

cat("Cheat Sheet QA Audit\n")
cat("Root:", root, "\n")
cat("Package:", pkg_dir, "\n")
cat("Cheat sheet:", cheat_path, "\n\n")

for (i in seq_along(exprs)) {
  expr <- exprs[[i]]
  sr <- attr(expr, "srcref")
  rel_line <- if (!is.null(sr)) sr[[1]] else NA_integer_
  abs_line <- if (is.na(rel_line)) NA_integer_ else (start_line - 1L + rel_line)
  expr_text <- paste(deparse(expr, width.cutoff = 500L), collapse = " ")

  cat("## Expression", i, "(line", abs_line, ")\n")
  cat(expr_text, "\n")

  tryCatch(
    withCallingHandlers(
      eval(expr, envir = .GlobalEnv),
      warning = function(w) {
        issues <<- rbind(
          issues,
          data.frame(
            line = abs_line,
            type = "warning",
            message = conditionMessage(w),
            expression = expr_text,
            stringsAsFactors = FALSE
          )
        )
        cat("WARNING:", conditionMessage(w), "\n")
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      issues <<- rbind(
        issues,
        data.frame(
          line = abs_line,
          type = "error",
          message = conditionMessage(e),
          expression = expr_text,
          stringsAsFactors = FALSE
        )
      )
      cat("ERROR:", conditionMessage(e), "\n")
    }
  )

  cat("\n")
}

write.csv(issues, issues_path, row.names = FALSE)

cat("Audit complete.\n")
cat("Issues found:", nrow(issues), "\n")
cat("Issue report:", issues_path, "\n")
cat("Execution log:", log_path, "\n")
