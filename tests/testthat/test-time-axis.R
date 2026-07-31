# Seconds / minutes on the x axis of the kinetics plots.

library(shiny)

test_that("calcium time is already in seconds", {
  axis <- app$scale_time_axis(c(0, 60, 120), assay_type = 1, unit = "s")
  expect_equal(axis$time, c(0, 60, 120))
  expect_equal(axis$label, "Time [s]")

  axis <- app$scale_time_axis(c(0, 60, 120), assay_type = 1, unit = "min")
  expect_equal(axis$time, c(0, 1, 2))
  expect_equal(axis$label, "Time [min]")
})

test_that("ROS measurement points are left untouched by default", {
  axis <- app$scale_time_axis(c(-10, 0, 59), assay_type = 2, unit = "points")
  expect_equal(axis$time, c(-10, 0, 59))
  expect_equal(axis$label, "Measurement points")
})

test_that("ROS measurement points become time once the interval is known", {
  axis <- app$scale_time_axis(c(-1, 0, 2), assay_type = 2, unit = "s",
                              interval = 30)
  expect_equal(axis$time, c(-30, 0, 60))
  expect_equal(axis$label, "Time [s]")

  axis <- app$scale_time_axis(c(-1, 0, 2), assay_type = 2, unit = "min",
                              interval = 30)
  expect_equal(axis$time, c(-0.5, 0, 1))
  expect_equal(axis$label, "Time [min]")
})

test_that("the calcium kinetics can be switched to minutes", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx")
    ))

    # seconds is the default of the calcium assay
    seconds <- mean_graphs()
    expect_equal(seconds$labels$x, "Time [s]")
    expect_equal(range(seconds$data$time), c(0, 1910))

    session$setInputs(time_unit = "min")
    minutes <- mean_graphs()
    expect_equal(minutes$labels$x, "Time [min]")
    expect_equal(range(minutes$data$time), c(0, 1910 / 60))
    expect_equal(minutes$data$values, seconds$data$values)
  })
})

test_that("the ROS kinetics keep measurement points until a unit is chosen", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      assay_type = "2",
      data_file = example_upload("ROSExample.xlsx"),
      layout_file = example_upload("ROSExample_layout.xlsx")
    ))

    points <- mean_graphs()
    expect_equal(points$labels$x, "Measurement points")
    expect_equal(range(points$data$time), c(-10, 59))

    session$setInputs(time_unit = "min", interval_value = 30, interval_unit = "s")
    minutes <- mean_graphs()
    expect_equal(minutes$labels$x, "Time [min]")
    expect_equal(range(minutes$data$time), c(-5, 29.5))
  })
})

test_that("the measurement interval defaults per assay and accepts minutes", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      assay_type = "2",
      data_file = example_upload("ROSExample.xlsx"),
      layout_file = example_upload("ROSExample_layout.xlsx"), time_unit = "s"
    ))
    # the ROS default is one minute
    expect_equal(measurement_interval(), 60)

    # 10 minutes is a common, high value and has to be possible
    session$setInputs(interval_value = 10, interval_unit = "min")
    expect_equal(measurement_interval(), 600)

    session$setInputs(interval_value = 90, interval_unit = "s")
    expect_equal(measurement_interval(), 90)

    # unusable entries fall back to the default of the assay
    session$setInputs(interval_value = 0)
    expect_equal(measurement_interval(), 60)

    session$setInputs(interval_value = NA)
    expect_equal(measurement_interval(), 60)

    session$setInputs(assay_type = "1", interval_value = NULL)
    expect_equal(measurement_interval(), 10)
  })
})

test_that("the calcium interval scales the normalization and the time axis", {
  fast <- app$calculate_data(example_upload("CaExample.xlsx"), 15, "2", "1",
                             interval = 10)
  slow <- app$calculate_data(example_upload("CaExample.xlsx"), 15, "2", "1",
                             interval = 20)

  expect_equal(fast$ms_normdata$time, seq(0, 1910, by = 10))
  expect_equal(slow$ms_normdata$time, seq(0, 3820, by = 20))

  # L/Lmax is a rate per interval, so doubling the interval halves it
  expect_equal(slow$ms_normdata$A1, fast$ms_normdata$A1 / 2, tolerance = 1e-12)
  # the raw values never change
  expect_equal(slow$ms_rawdata$A1, fast$ms_rawdata$A1)
})
