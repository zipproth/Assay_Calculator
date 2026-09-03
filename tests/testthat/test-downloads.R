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

# Count the pages of a PDF.  The file is binary, so it is scanned byte wise for
# the page markers.
pdf_page_count <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  marker <- charToRaw("/Type /Page ")
  positions <- which(bytes == marker[1])
  sum(vapply(positions, function(i){
    identical(bytes[i:min(i + length(marker) - 1, length(bytes))], marker)
  }, logical(1)))
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

# Inputs for the ROS example, the counterpart of calcium_download_inputs().
ros_download_inputs <- function(...) {
  modifyList(
    app_inputs(
      assay_type = "2",
      data_file = example_upload("ROSExample.xlsx"),
      layout_file = example_upload("ROSExample_layout.xlsx"),
      xlim = c(0, 4140), ylim = 30262,
      auc_rotation = "1", auc_columns = "1", auc_method = "trapezoid",
      auc_range = c(-10, 59), auc_reference = "",
      reference_source = "none",
      settings_data_download1 = character(0)
    ),
    list(...)
  )
}

# Every well of the ROS example that carries an elicitor.  Excluding all of them
# leaves a plate of nothing but controls, and the controls are the blank: they
# have no peak of their own, so ros_normalize() reports an empty table of maxima
# and the bar plot is built over zero rows.  ggplot notices that only when the
# plot is drawn, and drawing is the one thing the download handlers do while a
# graphics device of their own is open.
ros_elicited_wells <- function() {
  layout <- app$read_plate_layout(example_upload("ROSExample_layout.xlsx"))
  wells <- app$annotated_wells(layout$plate_layout)
  return(wells$well[wells$elicitor != "control"])
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

    # well curves, plate layout, maxima, area under curve, mean kinetics
    expect_gte(pdf_page_count(path), 5L)
  })
})

test_that("a plot that fails to draw closes the pdf device again", {
  # The handler opened pdf(), printed the five plots and closed the device on
  # the last line.  A print() that throws never reached that dev.off(), and the
  # pdf device stayed the current one: every later renderPlot() of the same R
  # process drew into the orphaned file instead of the browser, so the session
  # looked frozen with empty plots.  On shiny-server one process serves a whole
  # session, so a single failed download spoiled everything that came after it.
  devices <- dev.list()

  testServer(APP_DIR, {
    do.call(session$setInputs,
            ros_download_inputs(excluded_wells = ros_elicited_wells()))

    expect_equal(nrow(bar_plots_max()$data), 0L)
    expect_error(session$getOutput("downloadPlot"),
                 "Faceting variables must have at least one value")
  })

  expect_equal(dev.list(), devices)
})

test_that("a plot that fails to draw closes the png device again", {
  # add_plot_sheet() in R/export.R has the same shape, png() instead of pdf(),
  # and it leaked one device per attempt.  The Excel download renders the plots
  # only when they were asked for, so the leak needs the plot setting.
  devices <- dev.list()

  testServer(APP_DIR, {
    do.call(session$setInputs,
            ros_download_inputs(excluded_wells = ros_elicited_wells(),
                                settings_data_download1 = "1"))

    expect_error(session$getOutput("downloadData"),
                 "Faceting variables must have at least one value")
  })

  expect_equal(dev.list(), devices)
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

test_that("the download carries the area without ever opening the AUC tab", {
  # The integration window slider is rendered into the "Area under Curve" tab,
  # and Shiny suspends an output while its element is hidden, so input$auc_range
  # does not exist until that tab has been opened once.  A download taken
  # straight after the upload therefore used to come out without the area sheet,
  # without the area rows in the settings and without the area plot, and nothing
  # told the user that anything was missing.  Leaving auc_range out of the inputs
  # is exactly that state: every other input of the tab is a plain radioButton
  # that the browser registers whether the tab is shown or not.
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_download_inputs(auc_range = NULL))

    path <- read_download(session, "downloadData", "xlsx")

    expect_true("area under curve" %in% openxlsx::getSheetNames(path))
    auc <- openxlsx::read.xlsx(path, sheet = "area under curve")
    expect_equal(nrow(auc), 28L)

    # The window defaults to the whole measurement, and the settings sheet says
    # so: a defaulted window that is written down stays reproducible, an absent
    # one does not.
    settings <- openxlsx::read.xlsx(path, sheet = "settings")
    values <- setNames(settings$value, settings$setting)
    expect_equal(values[["area under curve window"]], "0 to 1910")
    expect_equal(values[["area under curve method"]], "trapezoid")

    # And it is the same area the user gets after opening the tab and leaving
    # the slider alone, so visiting the tab never changes a number.
    opened <- auc$values
    session$setInputs(auc_range = c(0, 1910))
    expect_equal(auc_calculate()$auc$values, opened, tolerance = 1e-8)
  })
})

test_that("the csv and PDF downloads carry the area without the AUC tab", {
  # The same omission reached the other two formats: build_csv_archive() skips a
  # NULL table, so the archive lost area_under_curve.csv, and the PDF lost its
  # area page and came out with four pages instead of five.
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_download_inputs(auc_range = NULL))

    path <- read_download(session, "downloadCsv", "zip")
    expect_true("area_under_curve.csv" %in%
                  basename(utils::unzip(path, list = TRUE)$Name))

    expect_gte(pdf_page_count(read_download(session, "downloadPlot", "pdf")), 5L)
    expect_equal(bar_plots_auc()$labels$title,
                 "Area under Curve (0 to 1910 s) with SD")
  })
})
