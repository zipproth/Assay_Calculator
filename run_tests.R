#!/usr/bin/env Rscript
#
# Test runner for the Assay Calculator.
#
#   Rscript run_tests.R                 # run the whole suite
#   Rscript run_tests.R calculate-data  # only test files matching a pattern
#
# Most of the suite consists of characterization tests: they do not describe
# what the app *should* do, they record what it *does* do, so that refactorings
# and new features can be shown not to change any of the existing results.

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required to run the test suite.", call. = FALSE)
}

suppressPackageStartupMessages(library(testthat))

# Directory of this script, so the runner also works from other directories.
script_dir <- local({
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 1) dirname(sub("^--file=", "", file_arg)) else getwd()
})

args <- commandArgs(trailingOnly = TRUE)
filter <- if (length(args) > 0 && nzchar(args[1])) args[1] else NULL

test_dir(
  file.path(script_dir, "tests", "testthat"),
  filter = filter,
  reporter = "summary",
  stop_on_failure = TRUE,
  load_package = "none"
)
