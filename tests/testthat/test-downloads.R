# The Excel and PDF downloads.  These are end to end tests: they run the real
# download handlers and read the produced files back.

library(shiny)

read_download <- function(session, name, extension) {
  path <- session$getOutput(name)
  expect_true(file.exists(path))
  expect_gt(file.size(path), 0)
  expect_match(path, paste0("\\.", extension, "$"), ignore.case = TRUE)
  path
}

test_that("the calcium Excel download carries all data sheets", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      xlim = c(0, 1910), ylim = 1.80872539596421,
      auc_rotation = "1", auc_columns = "1", auc_method = "trapezoid",
      auc_range = c(0, 1910),
      reference_source = "none",
      settings_data_download1 = character(0)
    ))

    path <- read_download(session, "downloadData", "xlsx")
    wb <- XLConnect::loadWorkbook(path)

    expect_equal(XLConnect::getSheets(wb),
                 c("raw data", "normalized data", "data with names",
                   "mean data", "maxima", "mean data with sd",
                   "maxima with sd", "area under curve"))

    auc <- XLConnect::readWorksheet(wb, sheet = "area under curve")
    expect_equal(nrow(auc), 28L)
    expect_equal(names(auc),
                 c("elicitor", "genotype", "values", "sd", "n",
                   "from", "to", "method", "unit"))
    expect_equal(unique(auc$method), "trapezoid")
    expect_equal(unique(auc$unit), "L/Lmax x s")
    expect_equal(auc$values, auc_calculate()$auc$values, tolerance = 1e-8)

    maxima <- XLConnect::readWorksheet(wb, sheet = "maxima")
    expect_equal(ncol(maxima), 28L)
  })
})

test_that("the ROS Excel download carries the SDOM sheets", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      assay_type = "2",
      data_file = example_upload("ROSExample.xlsx"),
      layout_file = example_upload("ROSExample_layout.xlsx"),
      xlim = c(0, 690), ylim = 30262,
      auc_rotation = "1", auc_columns = "1", auc_method = "trapezoid",
      auc_range = c(-10, 59),
      reference_source = "none",
      settings_data_download1 = character(0)
    ))

    path <- suppressMessages(read_download(session, "downloadData", "xlsx"))
    wb <- XLConnect::loadWorkbook(path)

    expect_equal(XLConnect::getSheets(wb),
                 c("raw data", "data with names", "mean data", "maxima",
                   "mean data with sdom", "maxima with sdom",
                   "area under curve"))

    auc <- XLConnect::readWorksheet(wb, sheet = "area under curve")
    expect_equal(nrow(auc), 10L)
    expect_equal(unique(auc$unit),
                 "Relative Luminescence x measurement points")
  })
})

test_that("the Excel download can embed the plots", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      xlim = c(0, 1910), ylim = 1.80872539596421,
      auc_rotation = "1", auc_columns = "1", auc_method = "trapezoid",
      auc_range = c(0, 1910),
      reference_source = "none",
      settings_data_download1 = "1"
    ))

    path <- read_download(session, "downloadData", "xlsx")
    wb <- XLConnect::loadWorkbook(path)

    expect_true(all(c("wellplots", "platelayout", "maximabarplots",
                      "meangraphs", "aucbarplots") %in%
                      XLConnect::getSheets(wb)))
  })
})

test_that("the download leaves no files behind in the app directory", {
  before <- list.files(APP_DIR)

  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      xlim = c(0, 1910), ylim = 1.80872539596421,
      auc_rotation = "1", auc_columns = "1", auc_method = "trapezoid",
      auc_range = c(0, 1910),
      reference_source = "none",
      settings_data_download1 = "1"
    ))
    read_download(session, "downloadData", "xlsx")
  })

  expect_equal(list.files(APP_DIR), before)
})

test_that("the PDF download contains all plots of the analysis", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      xlim = c(0, 1910), ylim = 1.80872539596421,
      auc_rotation = "1", auc_columns = "1", auc_method = "trapezoid",
      auc_range = c(0, 1910),
      reference_source = "none"
    ))

    path <- read_download(session, "downloadPlot", "pdf")

    # well curves, plate layout, maxima, area under curve, mean kinetics
    pages <- length(grep("/Type\\s*/Page[^s]",
                         readLines(path, warn = FALSE), value = TRUE))
    expect_gte(pages, 5L)
  })
})

test_that("the combined quadrant download writes a csv", {
  dir <- new_temp_dir()
  files <- quadrant_files(dir, c("M:A1", "M:A2"), c("M:B1", "M:B2"))

  testServer(APP_DIR, {
    session$setInputs(
      Q1E_file = data.frame(name = "QE.txt", size = 1, type = "",
                            datapath = files$measurement,
                            stringsAsFactors = FALSE),
      Q1D_file = data.frame(name = "QD.txt", size = 1, type = "",
                            datapath = files$discharge,
                            stringsAsFactors = FALSE)
    )

    path <- session$getOutput("downloadCombined")
    combined <- read.csv(path)
    expect_equal(dim(combined), c(78L, 4L))
    expect_equal(names(combined), c("A1", "A2", "B1", "B2"))
  })
})
