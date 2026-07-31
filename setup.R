#!/usr/bin/env Rscript
#
# Install everything the Assay Calculator needs.
#
#   Rscript setup.R          # runtime packages only
#   Rscript setup.R --tests  # runtime packages and the test suite
#
# The app deliberately has no Java dependency: Excel files are read with readxl
# and written with openxlsx.

options(repos = c(CRAN = "https://cloud.r-project.org"))

runtime <- c(
  "shiny",          # >= 1.5.0
  "shinydashboard",
  "ggplot2",        # >= 3.4.0, linewidth= in element_line()
  "plyr",
  "reshape2",
  "gridExtra",
  "gtools",
  "ggsci",
  "png",
  "readxl",
  "openxlsx",
  "viridisLite"
)

testing <- c("testthat")

wanted <- runtime
if ("--tests" %in% commandArgs(trailingOnly = TRUE)) {
  wanted <- c(runtime, testing)
}

missing <- wanted[!vapply(wanted, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) == 0) {
  cat("All required packages are already installed.\n")
} else {
  cat("Installing:", paste(missing, collapse = ", "), "\n")
  install.packages(missing)
}

cat("\nR", as.character(getRversion()), "\n")
for (p in wanted) {
  cat(sprintf("%-16s %s\n", p,
      if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p))
      else "MISSING"))
}
