# ROS_calculate() averages the wells per genotype x elicitor, subtracts the
# "control" elicitor of the same genotype, subtracts the background of the
# first measurement points and picks the peak of every mean curve.

library(shiny)

ros_maxima_names <- c(
  "genotype1 Compound 1 [1µM]", "genotype2 Compound 1 [1µM]",
  "genotype1 Compound 2 [1µM]", "genotype2 Compound 2 [1µM]",
  "genotype1 Compound 3 [1µM]", "genotype2 Compound 3 [1µM]",
  "genotype1 Compound 4 [1µM]", "genotype2 Compound 4 [1µM]",
  "genotype1 MeOH", "genotype2 MeOH"
)

ros_maxima <- c(2464.575, 1785.475, 14804.275, 1456.575, 10064.45,
                1595.125, -253.325, 1754.625, 1871.875, 2135.625)

ros_maxima_sd <- c(1105.79061487, 138.401847634, 1147.79205745, 268.845531194,
                   2644.2326725, 223.722384569, 1427.14971935, 205.264963784,
                   177.755845544, 260.90443225)

ros_inputs <- function(...) {
  modifyList(
    app_inputs(
      assay_type = "2",
      data_file = example_upload("ROSExample.xlsx"),
      layout_file = example_upload("ROSExample_layout.xlsx"),
      xlim = c(0, 4140),
      ylim = 30262
    ),
    list(...)
  )
}

test_that("the background window keeps its off-by-one and stays in the file", {
  # With 10 points recorded before the elicitation and 5 of them used, the
  # window covers the points 5 to 9, not 6 to 10.  The asymmetry is what the
  # original implementation did and the README names it: correcting it would
  # move every ROS number the app has ever produced.
  expect_equal(app$background_window(10, 5), 5:9)
  expect_equal(app$background_window(10, 5, 70), 5:9)

  # The window ends one point before the elicitation, so the last row it reads
  # is bg_values - 1 and a measurement of 70 points carries at most 71 points
  # before it.  Beyond that the window used to run past the end of the data:
  # ros_normalize() indexes a matrix and aborted with "subscript out of
  # bounds", while ros_well_values() indexes a data frame and silently
  # returned NA for every well of the plate.
  expect_equal(app$background_window(71, 5, 70), 66:70)
  expect_equal(app$background_window(72, 5, 70), 66:70)
  expect_equal(app$background_window(1000, 5, 70), 66:70)
  expect_true(all(app$background_window(1000, 1000, 70) %in% 1:70))

  # The number of points used can never push the window out on its own; it is
  # clamped against the points recorded before the elicitation.
  expect_equal(app$background_window(10, 1000), 1:9)
  expect_equal(app$background_window(10, 0), 9L)

  # Fewer than two points before the elicitation leave no window at all.  The
  # sidebar cannot reach this, but the arithmetic used to produce the row index
  # 0 for 1, and negative indices below it -- and a negative index does not
  # select a row, it drops one, so the "background" became the mean of nearly
  # the whole measurement.
  expect_equal(app$background_window(1, 5, 70), 1L)
  expect_equal(app$background_window(0, 5, 70), 1L)
})

test_that("ROS_calculate is only run for the ROS assay", {
  testServer(APP_DIR, {
    do.call(session$setInputs, ros_inputs(assay_type = "1"))
    expect_null(ROS_calculate())
  })
})

test_that("ROS_calculate needs a plate layout", {
  testServer(APP_DIR, {
    do.call(session$setInputs, ros_inputs(layout_file = NULL))
    expect_null(ROS_calculate())
  })
})

test_that("the control wells are subtracted and dropped from the maxima", {
  testServer(APP_DIR, {
    do.call(session$setInputs, ros_inputs())
    ros <- suppressMessages(ROS_calculate())

    expect_named(ros, c("normdata_melted", "normdata", "normdata_sd",
                        "normdata_w_sd", "maxima_melted", "maxima",
                        "maxima_sd", "maxima_w_sd"))

    # 6 elicitors x 2 genotypes, plus the time column
    expect_equal(dim(ros$normdata), c(70L, 13L))
    expect_equal(colnames(ros$normdata)[1:3],
                 c("time", "genotype1 Compound 1 [1µM]",
                   "genotype2 Compound 1 [1µM]"))

    # the two control groups are removed again from the maxima
    expect_equal(names(ros$maxima), ros_maxima_names)
    expect_equal(unname(ros$maxima), ros_maxima, tolerance = 1e-10)
    expect_equal(unname(ros$maxima_sd), ros_maxima_sd, tolerance = 1e-9)
  })
})

test_that("the ROS time axis starts before the elicitation", {
  testServer(APP_DIR, {
    do.call(session$setInputs, ros_inputs())
    ros <- suppressMessages(ROS_calculate())

    # 10 background measurement points are numbered -10 .. -1
    expect_equal(unname(ros$normdata[, "time"]), seq(-10, 59))
    expect_equal(rownames(ros$normdata)[1:3], c("-10", "-9", "-8"))
  })
})

test_that("a background longer than the measurement is refused, not calculated", {
  # ROSExample.xlsx has 70 measurement points and "Measurement points before
  # elicitation" has no upper bound, so 72 is simply typed into the sidebar.
  # The background window then ended on row 71 and ros_normalize() aborted with
  # "subscript out of bounds", which turned every ROS output red.
  testServer(APP_DIR, {
    do.call(session$setInputs, ros_inputs(ros_bg_points = 71))

    # 71 is the longest background a 70 point measurement can carry: the window
    # ends on the very last row of the file.
    expect_true(ros_background_fits())
    expect_false(is.null(suppressMessages(ROS_calculate())))

    session$setInputs(ros_bg_points = 72)
    expect_false(ros_background_fits())
    expect_null(suppressMessages(ROS_calculate()))
    expect_null(suppressMessages(analysis_well_values()))
    expect_null(suppressMessages(bar_plots_max()))
    expect_null(suppressMessages(mean_graphs()))
  })
})

test_that("the hint names how many points the measurement can carry", {
  # The plots fall back to summary_hint() when their data is NULL.  Without a
  # sentence of its own the hint blamed the well names of the layout for a
  # background that was only too long.
  testServer(APP_DIR, {
    do.call(session$setInputs, ros_inputs(ros_bg_points = 200))

    expect_match(summary_hint(), "70 measurement points", fixed = TRUE)
    expect_match(summary_hint(), "at most 71", fixed = TRUE)
  })
})

test_that("the normalized ROS values are unchanged", {
  testServer(APP_DIR, {
    do.call(session$setInputs, ros_inputs())
    ros <- suppressMessages(ROS_calculate())

    expect_equal(unname(ros$normdata[1:3, 2]),
                 c(-389.675, -186.8, -178.675), tolerance = 1e-10)
    expect_equal(unname(colSums(ros$normdata)[2:4]),
                 c(85489, 77840.125, 358839.125), tolerance = 1e-10)
    expect_equal(unname(ros$normdata_sd[1:3, 1]),
                 c(1108.97689235, 955.959615106, 976.951197787),
                 tolerance = 1e-9)

    expect_equal(dim(ros$normdata_sd), c(70L, 12L))
    expect_equal(dim(ros$normdata_melted), c(840L, 6L))
    expect_equal(names(ros$normdata_melted),
                 c("time", "name", "values", "genotype", "elicitor", "sd"))
  })
})

test_that("the exported ROS sheets interleave the values with their SDOM", {
  testServer(APP_DIR, {
    do.call(session$setInputs, ros_inputs())
    ros <- suppressMessages(ROS_calculate())

    expect_equal(dim(ros$normdata_w_sd), c(70L, 25L))
    expect_equal(colnames(ros$normdata_w_sd)[1:3],
                 c("time", "genotype1 Compound 1 [1µM]", "SDOM"))
    expect_equal(dim(ros$maxima_w_sd), c(10L, 2L))
  })
})

test_that("the ROS plots are built from the normalized data", {
  testServer(APP_DIR, {
    do.call(session$setInputs, ros_inputs())

    bp <- suppressMessages(bar_plots_max())
    expect_s3_class(bp, "ggplot")
    expect_equal(bp$labels$title, "Maxima with SDOM")
    expect_equal(bp$labels$y, "Relative Luminescence")
    expect_equal(nrow(bp$data), 10L)
    expect_equal(bp$data$values, ros_maxima, tolerance = 1e-10)

    mg <- suppressMessages(mean_graphs())
    expect_s3_class(mg, "ggplot")
    expect_equal(mg$labels$y, "Relative Luminescence")
    expect_equal(nrow(mg$data), 840L)
  })
})

test_that("the ROS well curves are drawn from raw data", {
  testServer(APP_DIR, {
    do.call(session$setInputs, ros_inputs())
    wp <- well_plots()
    expect_s3_class(wp, "ggplot")
    expect_equal(nrow(wp$data), 6720L)
  })
})
