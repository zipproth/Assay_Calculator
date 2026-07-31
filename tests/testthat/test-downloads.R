# The Excel, csv and PDF downloads.  These are end to end tests: they run the
# real download handlers and read the produced files back.

library(shiny)

read_download <- function(session, name, extension) {
  path <- session$getOutput(name)
  expect_true(file.exists(path))
  expect_gt(file.size(path), 0)
  expect_match(path, paste0("\\.", extension, "$"), ignore.case = TRUE)
  path
}

calcium_download_inputs <- function(...) {
  modifyList(
    app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      xlim = c(0, 1910), ylim = 1.80872539596421,
      auc_rotation = "1", auc_columns = "1", auc_method = "trapezoid",
      auc_range = c(0, 1910), auc_reference = "",
      reference_source = "none",
      settings_data_download1 = character(0)
    ),
    list(...)
  )
}

test_that("the calcium Excel download carries all data sheets", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_download_inputs())

    path <- read_download(session, "downloadData", "xlsx")

    expect_equal(openxlsx::getSheetNames(path),
                 c("raw data", "normalized data", "settings",
                   "data with names", "mean data", "maxima",
                   "mean data with sd", "maxima with sd",
                   "area under curve"))

    auc <- openxlsx::read.xlsx(path, sheet = "area under curve")
    expect_equal(nrow(auc), 28L)
    expect_equal(names(auc), c("elicitor", "genotype", "values", "sd", "n"))
    expect_equal(auc$values, auc_calculate()$auc$values, tolerance = 1e-8)

    maxima <- openxlsx::read.xlsx(path, sheet = "maxima")
    expect_equal(ncol(maxima), 28L)
    expect_equal(nrow(maxima), 1L)
  })
})

test_that("the settings sheet documents how the numbers were produced", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_download_inputs())

    path <- read_download(session, "downloadData", "xlsx")
    settings <- openxlsx::read.xlsx(path, sheet = "settings")

    expect_equal(names(settings), c("setting", "value"))
    values <- setNames(settings$value, settings$setting)

    expect_equal(values[["assay type"]], "Calcium")
    expect_equal(values[["data file"]], "CaExample.xlsx")
    expect_equal(values[["measurement interval [s]"]], "10")
    expect_equal(values[["discharge measurement points"]], "15")
    expect_equal(values[["excluded wells"]], "none")
    expect_equal(values[["area under curve window"]], "0 to 1910")
    expect_equal(values[["area under curve method"]], "trapezoid")
    expect_equal(values[["area under curve unit"]], "L/Lmax x s")
  })
})

test_that("the ROS settings sheet documents the background window", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      assay_type = "2",
      data_file = example_upload("ROSExample.xlsx"),
      layout_file = example_upload("ROSExample_layout.xlsx"),
      xlim = c(0, 4140), ylim = 30262,
      auc_rotation = "1", auc_columns = "1", auc_method = "trapezoid",
      auc_range = c(-10, 59), auc_reference = "",
      reference_source = "none",
      settings_data_download1 = character(0)
    ))

    path <- read_download(session, "downloadData", "xlsx")

    expect_equal(openxlsx::getSheetNames(path),
                 c("raw data", "settings", "mean data", "maxima",
                   "mean data with sdom", "maxima with sdom",
                   "area under curve"))

    settings <- openxlsx::read.xlsx(path, sheet = "settings")
    values <- setNames(settings$value, settings$setting)

    expect_equal(values[["assay type"]], "ROS")
    expect_equal(values[["measurement points before elicitation"]], "10")
    expect_equal(values[["background points used"]], "5")
    expect_equal(values[["background window (measurement points)"]], "5 to 9")
    expect_equal(values[["area under curve unit"]],
                 "Relative Luminescence x measurement points")

    # a changed background has to show up in the sheet
    session$setInputs(ros_bg_points = 12, ros_bg_used = 7)
    path <- read_download(session, "downloadData", "xlsx")
    values <- setNames(openxlsx::read.xlsx(path, sheet = "settings")$value,
                       openxlsx::read.xlsx(path, sheet = "settings")$setting)
    expect_equal(values[["measurement points before elicitation"]], "12")
    expect_equal(values[["background points used"]], "7")
    expect_equal(values[["background window (measurement points)"]], "5 to 11")
  })
})

test_that("the Excel download can embed the plots", {
  testServer(APP_DIR, {
    do.call(session$setInputs,
            calcium_download_inputs(settings_data_download1 = "1"))

    path <- read_download(session, "downloadData", "xlsx")

    expect_true(all(c("wellplots", "platelayout", "maximabarplots",
                      "aucbarplots", "meangraphs") %in%
                      openxlsx::getSheetNames(path)))
  })
})

test_that("the csv download contains one file per data set", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_download_inputs())

    path <- read_download(session, "downloadCsv", "zip")
    entries <- basename(utils::unzip(path, list = TRUE)$Name)

    expect_true(all(c("raw_data.csv", "normalized_data.csv", "settings.csv",
                      "mean_data.csv", "maxima.csv",
                      "area_under_curve.csv") %in% entries))

    dir <- new_temp_dir()
    utils::unzip(path, exdir = dir)
    auc <- read.csv(file.path(dir, "area_under_curve.csv"))
    expect_equal(nrow(auc), 28L)
  })
})

test_that("the download leaves no files behind in the app directory", {
  before <- list.files(APP_DIR)

  testServer(APP_DIR, {
    do.call(session$setInputs,
            calcium_download_inputs(settings_data_download1 = "1"))
    read_download(session, "downloadData", "xlsx")
    read_download(session, "downloadCsv", "zip")
  })

  expect_equal(list.files(APP_DIR), before)
})

test_that("the PDF download contains all plots of the analysis", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_download_inputs())

    path <- read_download(session, "downloadPlot", "pdf")

    # well curves, plate layout, maxima, area under curve, mean kinetics.
    # The PDF is binary, so it is scanned byte wise for the page markers.
    bytes <- readBin(path, "raw", file.size(path))
    marker <- charToRaw("/Type /Page ")
    positions <- which(bytes == marker[1])
    pages <- sum(vapply(positions, function(i){
      identical(bytes[i:min(i + length(marker) - 1, length(bytes))], marker)
    }, logical(1)))
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
