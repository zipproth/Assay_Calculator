# calculate_data() reads a measurement workbook, normalizes it and prepares
# every data set the plots and the Excel export are built from.  The expected
# numbers below were recorded from the example files shipped with the app.

library(shiny)

ca <- app$calculate_data(example_upload("CaExample.xlsx"), 15, "2", "1")
ros <- app$calculate_data(example_upload("ROSExample.xlsx"), 0, "2", "2")

test_that("the calcium example is reshaped into 192 time points and 96 wells", {
  expect_equal(dim(ca$ms_rawdata), c(192L, 97L))
  expect_equal(dim(ca$ms_normdata), c(192L, 97L))

  # the "M:" prefix of the measurement export is stripped and the wells are
  # sorted naturally (A1, A2, ... A10, not A1, A10, A2)
  expect_equal(head(colnames(ca$ms_rawdata), 5), c("A1", "A2", "A3", "A4", "A5"))
  expect_equal(tail(colnames(ca$ms_rawdata), 3), c("H11", "H12", "time"))

  expect_equal(ca$well_plate, 96)
  expect_equal(ca$ms_rawdata$time, seq(0, 1910, by = 10))
  expect_equal(ca$ms_normdata$time, seq(0, 1910, by = 10))
})

test_that("the discharge measurement points are cut off", {
  # 207 rows in the file, 15 of them discharge
  full <- app$calculate_data(example_upload("CaExample.xlsx"), 0, "2", "1")
  expect_equal(nrow(full$ms_rawdata), 207L)
  expect_equal(nrow(ca$ms_rawdata), 192L)
})

test_that("the empty data template is refused with a message about the template", {
  # Datatemplate.xlsx is offered for download on the Instructions tab and
  # carries the 96 well names without a single measurement point.  Uploading it
  # unchanged aborted in [.data.frame with "only 0's may be mixed with negative
  # subscripts", because 1:(0 - 15) counts backwards.
  template <- example_upload("Datatemplate.xlsx")
  expect_equal(dim(app$read_measurement_file(template)), c(0L, 96L))

  expect_error(app$calculate_data(template, 15, "2", "1"),
               "contains no measurement points")

  # The ROS assay reads with dc = 0, where 1:(0 - 0) is c(1, 0).  That did not
  # abort at all: it produced one row of NAs and carried that fabricated
  # measurement point into the plots and the export.
  expect_error(app$calculate_data(template, 0, "2", "2"),
               "contains no measurement points")
})

test_that("a measurement shorter than the discharge setting names the slider", {
  # The discharge slider goes up to 20 points, so a short pilot measurement
  # reaches the same backwards slice with a file that is not empty at all.
  dir <- new_temp_dir()
  path <- file.path(dir, "short.csv")
  write.csv(app$read_measurement_file(example_upload("CaExample.xlsx"))[1:15, ],
            path, row.names = FALSE)
  short <- data.frame(name = "short.csv", size = file.size(path), type = "",
                      datapath = path, stringsAsFactors = FALSE)

  # As many discharge points as measurement points did not abort either:
  # 1:(15 - 15) is c(1, 0), so the app cut the whole measurement away and then
  # plotted its very first point as the entire result.
  expect_error(app$calculate_data(short, 15, "2", "1"),
               "15 measurement points, and the last 15")
  # One point further the index runs backwards and [.data.frame aborts.
  expect_error(app$calculate_data(short, 16, "2", "1"),
               "15 measurement points, and the last 16")

  # One point more than the discharge setting is the shortest measurement that
  # still yields a result, and its single time stamp is 0.
  shortest <- app$calculate_data(short, 14, "2", "1")
  expect_equal(nrow(shortest$ms_rawdata), 1L)
  expect_equal(shortest$ms_rawdata$time, 0)
})

test_that("uploading the data template does not break the app", {
  # The same path through the reactive layer: the user downloads the template
  # on the Instructions tab and uploads it again without filling it in, with
  # the discharge slider left at its default of 15.
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("Datatemplate.xlsx")
    ))
    expect_error(calculate(), "contains no measurement points")
  })
})

test_that("raw and normalized values of the calcium example are unchanged", {
  wells <- setdiff(names(ca$ms_rawdata), "time")

  expect_equal(
    fingerprint(ca$ms_rawdata[wells]),
    list(n = 18432L, n_na = 0L, min = 0.009, max = 16.54,
         mean = 0.581874782986111, sum = 10725.116),
    tolerance = 1e-10
  )
  expect_equal(
    fingerprint(ca$ms_normdata[wells]),
    list(n = 18432L, n_na = 0L, min = 0.00471061143736457,
         max = 1.80872539596421, mean = 0.103372154459803,
         sum = 1905.35555100309),
    tolerance = 1e-10
  )
})

test_that("the axis ranges follow the chosen data set", {
  expect_equal(ca$yrange, c(0.00471061143736457, 1.80872539596421),
               tolerance = 1e-10)
  expect_equal(ca$xrange, c(0, 1910))

  # for raw data the range is the range of the raw values
  ca_raw <- app$calculate_data(example_upload("CaExample.xlsx"), 15, "1", "1")
  expect_equal(ca_raw$yrange, c(0.009, 16.54), tolerance = 1e-10)
})

test_that("the plotted data set depends on data_type and assay_type", {
  ca_raw <- app$calculate_data(example_upload("CaExample.xlsx"), 15, "1", "1")
  expect_equal(ca_raw$chosen_set, ca_raw$ms_rd_melted)

  # normalized data for the calcium assay
  expect_equal(ca$chosen_set, ca$ms_nd_melted)

  # the ROS assay always plots raw data, even when "normalized" is selected
  expect_equal(ros$chosen_set, ros$ms_rd_melted)
})

test_that("the melted data sets carry time, well and value", {
  expect_equal(names(ca$ms_rd_melted), c("time", "well", "value"))
  expect_equal(dim(ca$ms_rd_melted), c(18432L, 3L))
})

test_that("plate_mean is the mean over all wells of the plate", {
  # Up to this fork plate_mean returned the values of well A1: the column
  # subset used a logical vector over the *rows* (!time), which selects the
  # first column instead of all wells.  The green "plot plate mean" overlay
  # showed a single well.
  expect_equal(dim(ca$plate_mean), c(192L, 2L))
  expect_equal(names(ca$plate_mean), c("time", "values"))
  expect_equal(ca$plate_mean$time, seq(0, 1910, by = 10))

  wells <- setdiff(names(ca$ms_normdata), "time")
  expect_equal(ca$plate_mean$values, unname(rowMeans(ca$ms_normdata[wells])),
               tolerance = 1e-10)
  expect_equal(head(ca$plate_mean$values, 4),
               c(0.0362501274546103, 0.0289486197222434,
                 0.0259973935078381, 0.0252451116019937),
               tolerance = 1e-10)

  # and it is no longer the first well
  expect_false(isTRUE(all.equal(head(ca$plate_mean$values, 4),
                                head(ca$ms_normdata$A1, 4))))
})

test_that("wells that were never filled do not count into the plate mean", {
  data <- ca
  data$ms_normdata$A1 <- 0

  expect_equal(app$well_rowmeans(data$ms_normdata),
               rowMeans(data$ms_normdata[setdiff(names(data$ms_normdata),
                                                 c("time", "A1"))]),
               tolerance = 1e-10)
})

test_that("the ROS example is read without normalization", {
  expect_equal(dim(ros$ms_rawdata), c(70L, 97L))
  expect_equal(ros$xrange, c(0, 690))
  expect_equal(ros$yrange, c(156, 30262))
  expect_equal(
    fingerprint(ros$ms_rawdata[setdiff(names(ros$ms_rawdata), "time")]),
    list(n = 6720L, n_na = 0L, min = 156, max = 30262,
         mean = 3378.54330357143, sum = 22703811),
    tolerance = 1e-10
  )
})
