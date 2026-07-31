# Shared setup for the Assay Calculator test suite.
#
# The calculations, the plot builders and the download writers live in R/ as
# plain functions.  Sourcing those modules into a private environment gives the
# tests direct access to the real production code without starting a Shiny
# session; the reactive layer in server.R is exercised with shiny::testServer().

APP_DIR <- normalizePath(file.path("..", ".."), mustWork = TRUE)

app <- new.env(parent = globalenv())
suppressWarnings(suppressMessages({
  library(shiny)
  library(ggplot2)
  library(plyr)
  library(reshape2)
  library(shinydashboard)
  library(gridExtra)
  library(gtools)
  library(ggsci)
  library(readxl)
  library(openxlsx)

  for (module in list.files(file.path(APP_DIR, "R"), pattern = "[.][Rr]$",
                            full.names = TRUE)) {
    sys.source(module, envir = app)
  }
}))

# Build the data frame that Shiny's fileInput hands to the server, for one of
# the example files shipped with the app.
example_upload <- function(filename) {
  path <- normalizePath(file.path(APP_DIR, filename), mustWork = TRUE)
  data.frame(
    name = filename,
    size = file.size(path),
    type = "",
    datapath = path,
    stringsAsFactors = FALSE
  )
}

# Default inputs for shiny::testServer() based tests.  Everything the server
# reads has to be set explicitly, because the inputs that the app creates via
# renderUI (dc, xlim, ylim, mean_overlay) do not exist in a headless session.
app_inputs <- function(...) {
  modifyList(
    list(
      assay_type = "1",
      data_type = "2",
      dc = 15,
      exclude4max = 4,
      file_name = TRUE,
      mean_overlay = FALSE,
      mean_overlay_plate = FALSE,
      mean_overlay_WT = FALSE,
      bar_rotation = "1",
      bar_columns = "1",
      graph_sorting = "1",
      settings_mean = character(0)
    ),
    list(...)
  )
}

# A fresh directory below the session temp directory, removed when R exits.
new_temp_dir <- function() {
  path <- tempfile("assaycalc-test-")
  dir.create(path)
  path
}

# Write a synthetic Luminoscan .txt export: tab separated with a comma as
# decimal separator, a leading label column, and the wells spread over several
# blocks that each start with a header row of well names.
write_luminoscan <- function(path, well_blocks, n_values, offset = 0) {
  lines <- character(0)
  for (wells in well_blocks) {
    lines <- c(lines, paste(c("Time", wells), collapse = "\t"))
    for (i in seq_len(n_values)) {
      values <- offset + i + seq_along(wells) / 10
      lines <- c(lines, paste(c(i, sub(".", ",", sprintf("%.1f", values),
                                       fixed = TRUE)),
                              collapse = "\t"))
    }
  }
  writeLines(lines, path)
  path
}

# A matching pair of measurement and discharge exports for one quadrant.
quadrant_files <- function(dir, wells_1, wells_2) {
  list(
    measurement = write_luminoscan(file.path(dir, "QE.txt"),
                                   list(wells_1, wells_2), 72),
    discharge = write_luminoscan(file.path(dir, "QD.txt"),
                                 list(wells_1, wells_2), 6, offset = 1000)
  )
}

# A compact fingerprint of a numeric object.  Pinning these instead of whole
# matrices keeps the expected values readable while still catching any change
# in the numbers.
fingerprint <- function(x) {
  v <- as.numeric(as.matrix(x))
  finite <- v[is.finite(v)]
  list(
    n = length(v),
    n_na = sum(is.na(v)),
    min = min(finite),
    max = max(finite),
    mean = mean(finite),
    sum = sum(finite)
  )
}
