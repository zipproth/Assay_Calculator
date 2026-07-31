# Structural tests for the plot builders.  These deliberately check the
# structure of the ggplot objects (layers, facets, labels, scales) rather than
# rendered pixels, so they stay stable across ggplot2 versions while still
# catching accidental changes to what is drawn.

ca_data <- app$calculate_data(example_upload("CaExample.xlsx"), 15, "2", "1")
ca_layout <- app$read_plate_layout(example_upload("CaExample_layout.xlsx"))

test_that("draw_well_plate builds a 12 column facetted line plot", {
  g <- app$draw_well_plate(
    data = ca_data,
    empty_wells = ca_layout$empty_wells,
    excluded_wells = NULL,
    mean_melted = NULL,
    plate_mean = NULL,
    reference_plate_mean = NULL,
    file_name = FALSE,
    data_file = "CaExample.xlsx",
    xlim = ca_data$xrange,
    ylim = ca_data$yrange[2]
  )

  expect_s3_class(g, "ggplot")
  expect_equal(as.character(rlang::quo_get_expr(g$mapping$x)), "time")
  expect_equal(as.character(rlang::quo_get_expr(g$mapping$y)), "value")
  expect_equal(g$facet$params$ncol, 12)
  expect_equal(names(g$facet$params$facets), "well")

  # one line layer for the curves and one text layer for the well labels
  geoms <- vapply(g$layers, function(l) class(l$geom)[1], character(1))
  expect_setequal(geoms, c("GeomLine", "GeomText"))

  expect_silent(invisible(ggplot2::ggplot_build(g)))
})

test_that("draw_well_plate switches to 24 columns for 384 well plates", {
  fake <- ca_data
  fake$well_plate <- 384
  g <- app$draw_well_plate(fake, NULL, NULL, NULL, NULL, NULL, FALSE,
                           "CaExample.xlsx", ca_data$xrange, ca_data$yrange[2])
  expect_equal(g$facet$params$ncol, 24)
})

test_that("draw_well_plate adds the file name as a title on request", {
  g <- app$draw_well_plate(ca_data, NULL, NULL, NULL, NULL, NULL, TRUE,
                           "CaExample.xlsx", ca_data$xrange, ca_data$yrange[2])
  expect_equal(g$labels$title, "rawdata file: CaExample.xlsx")
})

test_that("empty wells are blanked out before plotting", {
  data_with_empty <- app$calculate_data(example_upload("CaExample.xlsx"), 15, "2", "1")
  empty <- c("A1", "A2")
  g <- app$draw_well_plate(data_with_empty, empty, NULL, NULL, NULL, NULL, FALSE,
                           "CaExample.xlsx", data_with_empty$xrange,
                           data_with_empty$yrange[2])
  blanked <- g$data[g$data$well %in% empty, ]
  expect_true(all(blanked$value == 0))
})

test_that("draw_layout builds a 96 well plate map", {
  g <- app$draw_layout(ca_layout$plate_layout, 96, "CaExample_layout.xlsx")

  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$title, "Plate Layout: CaExample_layout.xlsx")
  expect_length(g$layers, 3)
  expect_silent(invisible(ggplot2::ggplot_build(g)))
})

test_that("draw_layout builds a 384 well plate map", {
  g <- app$draw_layout(ca_layout$plate_layout, 384, "CaExample_layout.xlsx")
  expect_s3_class(g, "ggplot")
  expect_length(g$layers, 3)
})
