# A reference curve drawn into every facet of the kinetics plots, taken either
# from a group of the current plate or from an uploaded file.

library(shiny)

write_reference <- function(dir, data, name = "reference.csv") {
  path <- file.path(dir, name)
  write.csv(data, path, row.names = FALSE)
  data.frame(name = name, size = file.size(path), type = "",
             datapath = path, stringsAsFactors = FALSE)
}

test_that("a reference file keeps its time column and its curves", {
  dir <- new_temp_dir()
  upload <- write_reference(dir, data.frame(time = c(0, 10, 20),
                                            wt = c(1, 2, 3),
                                            mutant = c(3, 2, 1)))

  reference <- app$read_reference_file(upload)

  expect_equal(colnames(reference), c("time", "wt", "mutant"))
  expect_equal(reference$time, c(0, 10, 20))
  expect_equal(reference$wt, c(1, 2, 3))
})

test_that("non numeric columns of a reference file are dropped", {
  dir <- new_temp_dir()
  upload <- write_reference(dir, data.frame(time = c(0, 10),
                                            label = c("a", "b"),
                                            wt = c(1, 2)))

  reference <- app$read_reference_file(upload)
  expect_equal(colnames(reference), c("time", "wt"))
})

test_that("unusable reference files are rejected instead of breaking the plot", {
  dir <- new_temp_dir()

  expect_null(app$read_reference_file(NULL))
  # only a time column, no curve
  expect_null(app$read_reference_file(
    write_reference(dir, data.frame(time = c(0, 10)), "onecol.csv")))
  # the first column has to be the time axis
  expect_null(app$read_reference_file(
    write_reference(dir, data.frame(label = c("a", "b"), wt = c(1, 2)),
                    "notime.csv")))
  # unsupported file type
  txt <- file.path(dir, "reference.txt")
  writeLines("time,wt", txt)
  expect_null(app$read_reference_file(
    data.frame(name = "reference.txt", size = 1, type = "",
               datapath = txt, stringsAsFactors = FALSE)))
})

test_that("the mean curves of the plate can serve as reference curves", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      xlim = c(0, 1910), ylim = 1.80872539596421,
      reference_source = "plate"
    ))

    curves <- reference_curves()
    expect_equal(dim(curves), c(192L, 29L))
    # the group names survive unmangled, so they can be shown in the picker
    expect_equal(colnames(curves)[1:2],
                 c("time", "genotype 1 Super Elicitor [5µM]"))

    session$setInputs(reference_group = "genotype 1 Super Elicitor [5µM]")
    reference <- reference_curve()
    expect_equal(names(reference), c("time", "values"))
    expect_equal(reference$time, seq(0, 1910, by = 10))
    expect_equal(reference$values,
                 unname(mean_max_calculate()$graphs_shaped[, 2]))
  })
})

test_that("the reference curve is drawn into the kinetics plot", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      xlim = c(0, 1910), ylim = 1.80872539596421,
      reference_source = "none"
    ))

    plain <- mean_graphs()
    expect_length(plain$layers, 1L)

    session$setInputs(reference_source = "plate",
                      reference_group = "genotype 2 Compound 5 [20µM]")
    with_reference <- mean_graphs()

    expect_length(with_reference$layers, 2L)
    reference_layer <- with_reference$layers[[2]]
    expect_s3_class(reference_layer$geom, "GeomLine")
    expect_false(reference_layer$inherit.aes)
    expect_equal(reference_layer$aes_params$linetype, "dashed")
    expect_equal(reference_layer$aes_params$colour, "black")
    # no elicitor and no genotype, so the line appears in every facet
    expect_equal(names(reference_layer$data), c("time", "values"))
    expect_equal(nrow(reference_layer$data), 192L)

    expect_silent(invisible(ggplot2::ggplot_build(with_reference)))
  })
})

test_that("the reference curve follows the selected time unit", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      xlim = c(0, 1910), ylim = 1.80872539596421,
      reference_source = "plate",
      reference_group = "genotype 1 Super Elicitor [5µM]",
      time_unit = "min"
    ))

    plot <- mean_graphs()
    expect_equal(range(plot$layers[[2]]$data$time), c(0, 1910 / 60))
  })
})

test_that("an uploaded reference curve is drawn as well", {
  dir <- new_temp_dir()
  upload <- write_reference(dir, data.frame(time = seq(0, 1910, by = 10),
                                            previous = seq_len(192) / 192))

  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      xlim = c(0, 1910), ylim = 1.80872539596421,
      reference_source = "file",
      reference_file = upload
    ))

    expect_equal(colnames(reference_curves()), c("time", "previous"))

    session$setInputs(reference_group = "previous")
    plot <- mean_graphs()
    expect_length(plot$layers, 2L)
    expect_equal(plot$layers[[2]]$data$values, seq_len(192) / 192)
  })
})

test_that("no reference curve is drawn without a selection", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      xlim = c(0, 1910), ylim = 1.80872539596421,
      reference_source = "plate"
    ))
    # curves are available but nothing is picked yet
    expect_false(is.null(reference_curves()))
    expect_null(reference_curve())

    # a group that is not in the data is ignored
    session$setInputs(reference_group = "does not exist")
    expect_null(reference_curve())
  })
})

test_that("the ROS mean curves can serve as reference curves", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      assay_type = "2",
      data_file = example_upload("ROSExample.xlsx"),
      layout_file = example_upload("ROSExample_layout.xlsx"),
      xlim = c(0, 690), ylim = 30262,
      reference_source = "plate"
    ))

    curves <- suppressMessages(reference_curves())
    expect_equal(dim(curves), c(70L, 13L))
    expect_equal(colnames(curves)[1:2],
                 c("time", "genotype1 Compound 1 [1µM]"))

    session$setInputs(reference_group = "genotype1 MeOH")
    reference <- suppressMessages(reference_curve())
    expect_equal(reference$time, seq(-10, 59))
  })
})
