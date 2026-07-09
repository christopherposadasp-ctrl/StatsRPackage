root <- normalizePath(".", winslash = "/", mustWork = TRUE)
pkg_dir <- file.path(root, "StatsPackage")
qa_dir <- file.path(root, "qa")
log_path <- file.path(qa_dir, "cheatsheet_audit_output.txt")
issues_path <- file.path(qa_dir, "cheatsheet_audit_issues.csv")
cheat_specs <- list(
  list(
    name = "2.CheatsheetV8.r",
    path = file.path(root, "2.CheatsheetV8.r"),
    start_pattern = "^defs <- list\\("
  ),
  list(
    name = "2.CheatsheetV9_Narrow.r",
    path = file.path(root, "2.CheatsheetV9_Narrow.r"),
    start_pattern = "^library\\(StatsPackage\\)"
  )
)

dir.create(qa_dir, showWarnings = FALSE, recursive = TRUE)
unlink(c(log_path, issues_path), force = TRUE)

if (!dir.exists(pkg_dir)) {
  stop("Package directory not found: ", pkg_dir)
}

missing_sheets <- vapply(cheat_specs, function(x) !file.exists(x$path), logical(1))
if (any(missing_sheets)) {
  stop(
    "Cheat sheet file(s) not found: ",
    paste(vapply(cheat_specs[missing_sheets], `[[`, character(1), "path"), collapse = ", ")
  )
}

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("devtools is required for the cheat sheet audit.")
}

audit_lib <- tempfile("stats-package-cheatsheet-lib-")
dir.create(audit_lib, recursive = TRUE)
old_lib_paths <- .libPaths()
.libPaths(c(audit_lib, old_lib_paths))
on.exit({
  .libPaths(old_lib_paths)
  unlink(audit_lib, recursive = TRUE, force = TRUE)
}, add = TRUE)

if ("package:StatsPackage" %in% search()) {
  detach("package:StatsPackage", unload = TRUE, character.only = TRUE)
} else if ("StatsPackage" %in% loadedNamespaces()) {
  unloadNamespace("StatsPackage")
}

devtools::install(
  pkg_dir,
  upgrade = "never",
  quiet = TRUE,
  dependencies = FALSE
)
suppressPackageStartupMessages(
  library("StatsPackage", lib.loc = audit_lib, character.only = TRUE)
)

local_version <- numeric_version(
  read.dcf(file.path(pkg_dir, "DESCRIPTION"))[1, "Version"]
)
if (utils::packageVersion("StatsPackage") != local_version) {
  stop("The cheat-sheet audit did not load the exact local package version.")
}

issues <- data.frame(
  sheet = character(),
  line = integer(),
  type = character(),
  message = character(),
  expression = character(),
  stringsAsFactors = FALSE
)

sink_open <- FALSE
sink(log_path, split = TRUE)
sink_open <- TRUE
on.exit({
  if (sink_open) {
    sink(NULL)
  }
}, add = TRUE)

cat("Cheat Sheet QA Audit\n")
cat("Root:", root, "\n")
cat("Package:", pkg_dir, "\n")
cat("Installed version:", as.character(local_version), "\n\n")

for (spec in cheat_specs) {
  lines <- readLines(spec$path, warn = FALSE, encoding = "UTF-8")
  start_line <- grep(spec$start_pattern, lines)[1]
  if (is.na(start_line)) {
    stop("Audit start marker not found in ", spec$name, ".")
  }

  exprs <- parse(text = lines[start_line:length(lines)], keep.source = TRUE)
  source_refs <- attr(exprs, "srcref")
  audit_env <- new.env(parent = .GlobalEnv)

  cat("#", spec$name, "\n")
  cat("Path:", spec$path, "\n")
  cat("Starting at line:", start_line, "\n\n")

  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    sr <- if (length(source_refs) >= i) source_refs[[i]] else NULL
    rel_line <- if (is.null(sr)) NA_integer_ else as.integer(sr[[1]])
    abs_line <- if (is.na(rel_line)) NA_integer_ else start_line - 1L + rel_line
    expr_text <- paste(deparse(expr, width.cutoff = 500L), collapse = " ")

    cat("## Expression", i, "(line", abs_line, ")\n")
    cat(expr_text, "\n")

    tryCatch(
      withCallingHandlers(
        eval(expr, envir = audit_env),
        warning = function(w) {
          issues <<- rbind(
            issues,
            data.frame(
              sheet = spec$name,
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
            sheet = spec$name,
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
}

write.csv(issues, issues_path, row.names = FALSE)

cat("Audit complete.\n")
cat("Cheat sheets checked:", length(cheat_specs), "\n")
cat("Issues found:", nrow(issues), "\n")
cat("Issue report:", issues_path, "\n")
cat("Execution log:", log_path, "\n")

sink(NULL)
sink_open <- FALSE

if (nrow(issues) > 0L) {
  stop("Cheat-sheet audit failed: ", nrow(issues), " issue(s) found.", call. = FALSE)
}
