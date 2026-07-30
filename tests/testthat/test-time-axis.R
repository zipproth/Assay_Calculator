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
      layout_file = example_upload("CaExample_layout.xlsx"),
      xlim = c(0, 1910), ylim = 1.80872539596421
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

test_that("the ROS kinetics keep measurement points until an interval is given", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      assay_type = "2",
      data_file = example_upload("ROSExample.xlsx"),
      layout_file = example_upload("ROSExample_layout.xlsx"),
      xlim = c(0, 690), ylim = 30262
    ))

    points <- suppressMessages(mean_graphs())
    expect_equal(points$labels$x, "Measurement points")
    expect_equal(range(points$data$time), c(-10, 59))

    session$setInputs(time_unit = "min", ros_interval = 30)
    minutes <- suppressMessages(mean_graphs())
    expect_equal(minutes$labels$x, "Time [min]")
    expect_equal(range(minutes$data$time), c(-5, 29.5))
  })
})

test_that("an unusable interval falls back to one minute", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      assay_type = "2",
      data_file = example_upload("ROSExample.xlsx"),
      layout_file = example_upload("ROSExample_layout.xlsx"),
      xlim = c(0, 690), ylim = 30262, time_unit = "s"
    ))
    expect_equal(ros_interval(), 60)

    session$setInputs(ros_interval = 0)
    expect_equal(ros_interval(), 60)

    session$setInputs(ros_interval = NA)
    expect_equal(ros_interval(), 60)

    session$setInputs(ros_interval = 90)
    expect_equal(ros_interval(), 90)
  })
})
