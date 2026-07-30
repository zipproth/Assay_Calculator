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
      xlim = c(0, 690),
      ylim = 30262
    ),
    list(...)
  )
}

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
