# Area under the curve with a selectable integration window.

library(shiny)

test_that("area_under_curve integrates by the trapezoidal rule", {
  # a rectangle of height 2 over 3 seconds
  expect_equal(app$area_under_curve(c(0, 10, 20, 30), c(2, 2, 2, 2)), 60)
  # a triangle from 0 to 10 over 10 seconds
  expect_equal(app$area_under_curve(c(0, 10), c(0, 10)), 50)
  # unevenly spaced points are weighted by their distance
  expect_equal(app$area_under_curve(c(0, 1, 10), c(0, 2, 2)), 1 + 18)
})

test_that("area_under_curve can also just sum the values", {
  expect_equal(app$area_under_curve(c(0, 10, 20), c(1, 2, 3), method = "sum"), 6)
  expect_equal(app$area_under_curve(c(0, 10, 20), c(1, 2, 3),
                                    from = 10, method = "sum"), 5)
})

test_that("the integration window is applied inclusively", {
  x <- seq(0, 100, by = 10)
  y <- rep(1, length(x))

  expect_equal(app$area_under_curve(x, y), 100)
  expect_equal(app$area_under_curve(x, y, from = 20, to = 50), 30)
  # a window that contains a single point has no area
  expect_equal(app$area_under_curve(x, y, from = 20, to = 20), 0)
  # a window outside the data has no value at all
  expect_true(is.na(app$area_under_curve(x, y, from = 200, to = 300)))
})

test_that("area_under_curve ignores missing values", {
  expect_equal(app$area_under_curve(c(0, 10, 20), c(1, NA, 1)), 20)
  expect_true(is.na(app$area_under_curve(c(0, 10), c(NA, NA))))
})

test_that("group_auc averages the areas of the replicate wells", {
  layout <- data.frame(
    well = c("A1", "A2", "B1", "B2"),
    genotype = c("wt", "wt", "mut", "mut"),
    elicitor = rep("flg22", 4),
    stringsAsFactors = FALSE
  )
  time <- c(0, 10, 20)
  wells <- data.frame(A1 = c(1, 1, 1), A2 = c(3, 3, 3),
                      B1 = c(0, 0, 0), B2 = c(2, 2, 2))

  res <- app$group_auc(wells, time, layout, from = 0, to = 20)

  expect_equal(res$elicitor, rep("flg22", 2))
  expect_equal(as.character(res$genotype), c("wt", "mut"))
  expect_equal(res$values, c(40, 20))          # mean of 20/60 and 0/40
  expect_equal(res$sd, c(sd(c(20, 60)), sd(c(0, 40))))
  expect_equal(res$n, c(2L, 2L))
})

test_that("group_auc can report the standard error of the mean", {
  layout <- data.frame(
    well = c("A1", "A2", "A3", "A4"),
    genotype = rep("wt", 4),
    elicitor = rep("flg22", 4),
    stringsAsFactors = FALSE
  )
  wells <- data.frame(A1 = c(1, 1), A2 = c(2, 2), A3 = c(3, 3), A4 = c(4, 4))

  sd_res <- app$group_auc(wells, c(0, 1), layout, 0, 1, spread = "sd")
  sdom_res <- app$group_auc(wells, c(0, 1), layout, 0, 1, spread = "sdom")

  expect_equal(sdom_res$sd, sd_res$sd / 2)
})

test_that("group_auc skips groups without wells on the plate", {
  layout <- data.frame(
    well = c("A1", "A2"),
    genotype = c("wt", "mut"),
    elicitor = c("flg22", "water"),
    stringsAsFactors = FALSE
  )
  wells <- data.frame(A1 = c(1, 1), A2 = c(2, 2))

  res <- app$group_auc(wells, c(0, 1), layout, 0, 1)

  # 2 elicitors x 2 genotypes, but only 2 combinations exist
  expect_equal(nrow(res), 2L)
  expect_equal(paste(res$genotype, res$elicitor), c("wt flg22", "mut water"))
})

test_that("the axis label follows assay and integration method", {
  expect_equal(app$auc_label(1, "trapezoid"), "L/Lmax x s")
  expect_equal(app$auc_label(1, "sum"), "Sum of L/Lmax")
  expect_equal(app$auc_label(2, "trapezoid"),
               "Relative Luminescence x measurement points")
  expect_equal(app$auc_label(2, "sum"), "Sum of Relative Luminescence")
})

test_that("the per well ROS values average to the published mean curves", {
  # ros_well_values() blanks and background corrects every well individually;
  # averaging the wells of a group has to reproduce ROS_calculate() exactly,
  # otherwise the areas would not belong to the plotted curves.
  layout <- app$read_plate_layout(example_upload("ROSExample_layout.xlsx"))
  raw_plate_layout <- layout$plate_layout[, 1:4]
  raw_plate_layout <- raw_plate_layout[complete.cases(raw_plate_layout), ]

  data <- app$calculate_data(example_upload("ROSExample.xlsx"), 0, "2", "2")
  wells <- app$ros_well_values(data$ms_rawdata, raw_plate_layout)

  expect_equal(dim(wells), c(70L, 96L))

  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      assay_type = "2",
      data_file = example_upload("ROSExample.xlsx"),
      layout_file = example_upload("ROSExample_layout.xlsx")
    ))
    published <- suppressMessages(ROS_calculate())$normdata

    for (group in colnames(published)[-1]) {
      genotype <- sub(" .*", "", group)
      elicitor <- sub("^[^ ]+ ", "", group)
      group_wells <- raw_plate_layout$well[
        raw_plate_layout$genotype == genotype &
          raw_plate_layout$elicitor == elicitor]
      expect_equal(unname(rowMeans(wells[group_wells])),
                   unname(published[, group]),
                   tolerance = 1e-9,
                   info = group)
    }
  })
})

test_that("the per well ROS values follow an excluded control well", {
  # A control well is the blank of its genotype, and ros_normalize() drops an
  # excluded one from that blank.  ros_well_values() kept blanking against every
  # control well of the plate, so excluding a control well moved the maxima
  # while the areas stayed on the old blank: the mean curves and the areas of
  # the same download no longer belonged to each other.  The averaged wells have
  # to reproduce the published curves with an exclusion as well.
  layout <- app$read_plate_layout(example_upload("ROSExample_layout.xlsx"))
  raw_plate_layout <- layout$plate_layout[, 1:4]
  raw_plate_layout <- raw_plate_layout[complete.cases(raw_plate_layout), ]

  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      assay_type = "2",
      data_file = example_upload("ROSExample.xlsx"),
      layout_file = example_upload("ROSExample_layout.xlsx"),
      excluded_wells = "A11"           # a control well of genotype1
    ))
    published <- suppressMessages(ROS_calculate())$normdata
    wells <- analysis_well_values()$values

    for (group in colnames(published)[-1]) {
      genotype <- sub(" .*", "", group)
      elicitor <- sub("^[^ ]+ ", "", group)
      group_wells <- raw_plate_layout$well[
        raw_plate_layout$genotype == genotype &
          raw_plate_layout$elicitor == elicitor]
      group_wells <- setdiff(group_wells, "A11")
      expect_equal(unname(rowMeans(wells[group_wells])),
                   unname(published[, group]),
                   tolerance = 1e-9,
                   info = group)
    }
  })
})

test_that("the calcium areas are calculated over the selected window", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      auc_rotation = "1", auc_columns = "1", auc_method = "trapezoid",
      auc_range = c(0, 1910)
    ))

    expect_equal(auc_time_range(), c(0, 1910, 10))

    res <- auc_calculate()
    expect_equal(res$ylabel, "L/Lmax x s")
    expect_equal(nrow(res$auc), 28L)
    expect_equal(names(res$auc),
                 c("elicitor", "genotype", "values", "sd", "n"))
    expect_equal(levels(res$auc$genotype), c("genotype 1", "genotype 2"))
    expect_true(all(res$auc$n > 0))

    # the whole plate was measured over 1910 s, so every area is positive
    expect_true(all(res$auc$values > 0))

    # a narrower window can never yield more area for a positive curve
    session$setInputs(auc_range = c(500, 1000))
    narrow <- auc_calculate()
    expect_true(all(narrow$auc$values < res$auc$values))
    expect_equal(narrow$from, 500)
    expect_equal(narrow$to, 1000)
  })
})

test_that("summing gives the trapezoidal area divided by the interval", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      auc_rotation = "1", auc_columns = "1", auc_method = "sum",
      auc_range = c(0, 1910)
    ))

    summed <- auc_calculate()
    expect_equal(summed$ylabel, "Sum of L/Lmax")

    session$setInputs(auc_method = "trapezoid")
    integrated <- auc_calculate()

    # the trapezoidal rule weights the two end points with one half, so the
    # two results only differ by those halves
    expect_true(all(integrated$auc$values / 10 < summed$auc$values))
    expect_equal(integrated$auc$values / 10, summed$auc$values,
                 tolerance = 0.02)
  })
})

test_that("the ROS areas skip the control wells", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      assay_type = "2",
      data_file = example_upload("ROSExample.xlsx"),
      layout_file = example_upload("ROSExample_layout.xlsx"),
      auc_rotation = "1", auc_columns = "1", auc_method = "trapezoid",
      auc_range = c(-10, 59)
    ))

    expect_equal(auc_time_range(), c(-10, 59, 1))

    res <- auc_calculate()
    expect_equal(res$ylabel, "Relative Luminescence x measurement points")
    # 5 elicitors (control excluded) x 2 genotypes
    expect_equal(nrow(res$auc), 10L)
    expect_false("control" %in% res$auc$elicitor)
    expect_equal(res$auc$n, rep(8L, 10))
  })
})

test_that("the ROS well values survive a background longer than the file", {
  # ros_well_values() takes the other path into background_window(): it indexes
  # a data frame, and reading past its last row does not raise an error, it
  # yields NA rows.  The background was then NA for every well and so was every
  # area; group_auc() dropped them all and the bar plot vanished behind the
  # hint about mismatched well names.
  data <- app$calculate_data(example_upload("ROSExample.xlsx"), 0, "2", "2")
  layout <- app$read_plate_layout(example_upload("ROSExample_layout.xlsx"))
  raw_plate_layout <- app$annotated_wells(layout$plate_layout)

  fits <- app$ros_well_values(data$ms_rawdata, raw_plate_layout, bg_values = 71)
  too_long <- app$ros_well_values(data$ms_rawdata, raw_plate_layout,
                                  bg_values = 72)

  expect_false(any(is.na(as.matrix(fits))))
  expect_false(any(is.na(as.matrix(too_long))))
  # The clamp lands on the longest window that fits; it does not invent one.
  expect_equal(too_long, fits)
})

test_that("no ROS areas are calculated when the background does not fit", {
  # The refusal has to reach this path as well, otherwise the areas would be
  # calculated from a shortened background while the kinetics and the maxima of
  # the same plate report nothing.
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      assay_type = "2",
      data_file = example_upload("ROSExample.xlsx"),
      layout_file = example_upload("ROSExample_layout.xlsx"),
      auc_rotation = "1", auc_columns = "1", auc_method = "trapezoid",
      auc_range = c(-10, 59), ros_bg_points = 72, ros_bg_used = 5
    ))

    expect_null(suppressMessages(analysis_well_values()))
    expect_null(suppressMessages(auc_calculate()))
    expect_null(suppressMessages(bar_plots_auc()))
  })
})

test_that("the area bar plot is built from the calculated areas", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      auc_rotation = "1", auc_columns = "1", auc_method = "trapezoid",
      auc_range = c(0, 1910)
    ))

    bp <- bar_plots_auc()
    expect_s3_class(bp, "ggplot")
    expect_equal(bp$labels$title, "Area under Curve (0 to 1910 s) with SD")
    expect_equal(bp$labels$y, "L/Lmax x s")
    expect_equal(bp$facet$params$ncol, 1)
    expect_equal(nrow(bp$data), 28L)
    expect_equal(bp$data$values, auc_calculate()$auc$values)

    session$setInputs(auc_columns = "2", auc_rotation = "2")
    flipped <- bar_plots_auc()
    expect_equal(flipped$facet$params$ncol, 2)
    expect_s3_class(flipped$coordinates, "CoordFlip")
  })
})

test_that("no areas are calculated without a layout", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"), auc_range = c(0, 1910)
    ))
    expect_null(auc_calculate())
  })
})

test_that("the integration window defaults to the whole measurement", {
  # The slider that sets the window is rendered into the "Area under Curve"
  # tab, and Shiny suspends an output while its element is hidden, so
  # input$auc_range does not exist before that tab has been opened once.  That
  # used to make auc_calculate() return NULL, and every download then quietly
  # lost its area sheet, its area plot and its window in the settings.  The
  # default is the value the slider itself starts with, so opening the tab
  # changes no number.
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx")
    ))

    defaulted <- auc_calculate()
    expect_equal(c(defaulted$from, defaulted$to), auc_time_range()[1:2])
    expect_equal(defaulted$from, 0)
    expect_equal(defaulted$to, 1910)
    expect_equal(nrow(defaulted$auc), 28L)

    session$setInputs(auc_range = c(0, 1910))
    expect_equal(auc_calculate()$auc$values, defaulted$auc$values)
  })
})
