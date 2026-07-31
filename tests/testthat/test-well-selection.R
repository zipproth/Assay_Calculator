# Excluding wells from the analysis, expressing the areas relative to a
# reference group, and picking single wells as a reference curve.

library(shiny)

calcium_inputs2 <- function(...) {
  modifyList(
    app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      xlim = c(0, 1910), ylim = 1.80872539596421,
      auc_rotation = "1", auc_columns = "1", auc_method = "trapezoid",
      auc_range = c(0, 1910), auc_reference = "",
      reference_source = "none"
    ),
    list(...)
  )
}

test_that("well_groups only reports combinations that have wells", {
  layout <- data.frame(
    well = c("A1", "A2", "B1"),
    genotype = c("wt", "wt", "mut"),
    elicitor = c("flg22", "water", "flg22"),
    stringsAsFactors = FALSE
  )

  # 2 elicitors x 2 genotypes, but only 3 combinations exist on the plate
  groups <- app$well_groups(layout)
  expect_length(groups, 3L)
  expect_equal(vapply(groups, function(g) g$label, character(1)),
               c("wt flg22", "mut flg22", "wt water"))

  # and a well that is not in the measurement drops out
  groups <- app$well_groups(layout, available_wells = c("A1", "A2"))
  expect_length(groups, 2L)
  expect_equal(groups[[1]]$wells, "A1")
})

test_that("excluded wells are left out of the means and maxima", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_inputs2())
    full <- mean_max_calculate()

    session$setInputs(excluded_wells = c("A1", "A2"))
    reduced <- mean_max_calculate()

    # the first group loses two of its wells, so its mean changes
    expect_false(isTRUE(all.equal(as.numeric(full$all_means[1, 3]),
                                  as.numeric(reduced$all_means[1, 3]))))
    # groups that keep all their wells are untouched
    expect_equal(as.numeric(full$all_means[5, 3]),
                 as.numeric(reduced$all_means[5, 3]))
  })
})

test_that("excluded wells are left out of the areas", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_inputs2())
    full <- auc_calculate()$auc

    session$setInputs(excluded_wells = c("A1", "A2"))
    reduced <- auc_calculate()$auc

    expect_equal(full$n[1], 9L)
    expect_equal(reduced$n[1], 7L)
    expect_equal(full$n[3], reduced$n[3])
  })
})

test_that("a group loses its bar once all of its wells are excluded", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_inputs2())
    layout <- app$read_plate_layout(example_upload("CaExample_layout.xlsx"))
    first_group <- layout$plate_layout$well[
      layout$plate_layout$genotype == "genotype 1" &
        layout$plate_layout$elicitor == "Super Elicitor [5µM]"]

    session$setInputs(excluded_wells = first_group)
    reduced <- auc_calculate()$auc

    expect_equal(nrow(reduced), 27L)
    expect_false("genotype 1 Super Elicitor [5µM]" %in%
                   paste(reduced$genotype, reduced$elicitor))
  })
})

test_that("excluded wells are marked in the well curves", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_inputs2(excluded_wells = c("A1")))
    g <- well_plots()

    # the curve of an excluded well is flattened, like an empty well
    expect_true(all(g$data$value[g$data$well == "A1"] == 0))
    labels <- g$layers[[2]]$data
    expect_true(labels$excluded[labels$well == "A1"])
    expect_false(labels$excluded[labels$well == "A2"])
  })
})

test_that("relative_auc turns a reference group into 100 percent", {
  auc <- data.frame(
    elicitor = c("flg22", "water"),
    genotype = factor(c("wt", "wt")),
    values = c(50, 200),
    sd = c(5, 20),
    n = c(3L, 3L),
    stringsAsFactors = FALSE
  )

  relative <- app$relative_auc(auc, "wt water")
  expect_equal(relative$values, c(25, 100))
  expect_equal(relative$sd, c(2.5, 10))

  # an unknown or empty reference leaves the values alone
  expect_equal(app$relative_auc(auc, "does not exist")$values, c(50, 200))
  expect_equal(app$relative_auc(auc, "")$values, c(50, 200))
  expect_equal(app$relative_auc(auc, NULL)$values, c(50, 200))
})

test_that("a reference of zero is ignored instead of producing infinities", {
  auc <- data.frame(elicitor = "flg22", genotype = factor("wt"),
                    values = 0, sd = 0, n = 3L, stringsAsFactors = FALSE)
  expect_equal(app$relative_auc(auc, "wt flg22")$values, 0)
})

test_that("the area bars can be shown as percent of a chosen group", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_inputs2())
    absolute <- auc_calculate()

    session$setInputs(auc_reference = "genotype 1 Super Elicitor [5µM]")
    relative <- auc_calculate()

    expect_equal(relative$ylabel, "% of genotype 1 Super Elicitor [5µM]")
    expect_equal(relative$auc$values[1], 100)
    expect_equal(relative$auc$values,
                 absolute$auc$values / absolute$auc$values[1] * 100,
                 tolerance = 1e-10)

    expect_equal(bar_plots_auc()$labels$y,
                 "% of genotype 1 Super Elicitor [5µM]")
  })
})

test_that("a single well can be used as the reference curve", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_inputs2(reference_source = "wells",
                                               reference_wells = "A1"))
    reference <- reference_curve()

    expect_equal(names(reference), c("time", "values"))
    expect_equal(reference$time, seq(0, 1910, by = 10))
    expect_equal(reference$values, calculate()$ms_normdata$A1, tolerance = 1e-12)

    expect_length(mean_graphs()$layers, 2L)
  })
})

test_that("the mean over a selection of wells can be used as the reference", {
  testServer(APP_DIR, {
    do.call(session$setInputs,
            calcium_inputs2(reference_source = "wells",
                            reference_wells = c("A1", "A2", "A3")))
    reference <- reference_curve()

    normdata <- calculate()$ms_normdata
    expect_equal(reference$values,
                 unname(rowMeans(normdata[c("A1", "A2", "A3")])),
                 tolerance = 1e-12)
  })
})

test_that("a ROS reference well carries the blanked values", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      assay_type = "2",
      data_file = example_upload("ROSExample.xlsx"),
      layout_file = example_upload("ROSExample_layout.xlsx"),
      xlim = c(0, 4140), ylim = 30262,
      reference_source = "wells", reference_wells = "A1"
    ))

    reference <- reference_curve()
    expect_equal(reference$time, seq(-10, 59))

    wells <- analysis_well_values()
    expect_equal(reference$values, wells$values$A1, tolerance = 1e-12)
  })
})

test_that("no reference curve is drawn without a well selection", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_inputs2(reference_source = "wells"))
    expect_null(reference_curve())

    session$setInputs(reference_wells = "does not exist")
    expect_null(reference_curve())
  })
})
