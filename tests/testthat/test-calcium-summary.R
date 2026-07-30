# mean_max_calculate() groups the wells by genotype x elicitor, averages them
# and picks the peak height of every mean curve.  These are the numbers the
# "Mean Maxima" bar plot, the "Mean Kinetics" plot and the Excel export are
# built from, so they are pinned in full.

library(shiny)

ca_maxima <- c(
  1.02204822894147, 0.05329608136327, 0.086304966041751, 0.081527989976265,
  0.168989113841767, 0.186975531946513, 0.498964984431053, 0.421646485131634,
  0.182695915796342, 0.187232117645021, 0.0409700458578299, 0.0376182567253271,
  0.0407325247782745, 0.0356533625799842, 0.577054643550094, 0.0553988932198309,
  0.65582291970998, 0.0907857175075003, 0.680309125672588, 0.0497179900303394,
  1.00026707040379, 0.0386242819946262, 0.999694999071315, 0.0361940467445138,
  0.46992993639859, 0.0465481732516809, 0.595782301011231, 0.0275588215760308
)

ca_maxima_sd <- c(
  0.50706245904731, 0.0470519949060551, 0.032643827403413, 0.0766247576827632,
  0.187236641325716, 0.119216897405968, 0.279641657224877, 0.0780431220087124,
  0.0523147563281533, 0.0261510306008074, 0.0200214824735766, 0.0102217522561646,
  0.0133738880202339, 0.0272787449004889, 0.232010108538231, 0.0215653745319111,
  0.118278677706643, 0.0201049265315746, 0.0766057645541009, 0.0303724183328325,
  0.0340436077984785, 0.00708111884045393, 0.157611454500061, 0.0107965035092826,
  0.0510319762709533, 0.0170589453911087, 0.0381642107612334, 0.00516457886549791
)

calcium_inputs <- function(...) {
  modifyList(
    app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = example_upload("CaExample_layout.xlsx"),
      xlim = c(0, 1910),
      ylim = 1.80872539596421
    ),
    list(...)
  )
}

test_that("mean_max_calculate returns one mean and one maximum per group", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_inputs())
    mm <- mean_max_calculate()

    expect_named(mm, c("all_means", "all_means_shaped", "all_means_shaped_w_sd",
                       "all_means_graph", "graphs_shaped", "graphs_shaped_w_sd",
                       "mean_melted", "ms_normdata_names"))

    # 14 elicitors x 2 genotypes
    expect_equal(dim(mm$all_means), c(28L, 4L))
    expect_equal(mm$all_means[1, 1:2], c("Super Elicitor [5µM]", "genotype 1"))
    expect_equal(mm$all_means[28, 1:2], c("Compound 13 [25 µg/ml]", "genotype 2"))

    expect_equal(as.numeric(mm$all_means[, 3]), ca_maxima, tolerance = 1e-10)
    expect_equal(as.numeric(mm$all_means[, 4]), ca_maxima_sd, tolerance = 1e-10)
  })
})

test_that("the mean kinetics per group are unchanged", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_inputs())
    mm <- mean_max_calculate()

    # time column plus one mean curve per group
    expect_equal(dim(mm$graphs_shaped), c(192L, 29L))
    expect_equal(colnames(mm$graphs_shaped)[1:2],
                 c("time", "genotype 1 Super Elicitor [5µM]"))
    expect_equal(mm$graphs_shaped[, 1], seq(0, 1910, by = 10))

    expect_equal(mm$graphs_shaped[c(1, 50, 100, 192), 2],
                 c(0.035773598848, 0.786651971067,
                   0.217665428332, 0.0256613984817),
                 tolerance = 1e-9)
    expect_equal(unname(colSums(mm$graphs_shaped)[2:4]),
                 c(54.9427151158, 5.43859963573, 7.28701914225),
                 tolerance = 1e-9)

    expect_equal(dim(mm$all_means_graph), c(5376L, 5L))
    expect_equal(dim(mm$mean_melted), c(18432L, 3L))
    expect_equal(names(mm$mean_melted), c("time", "well", "value"))
  })
})

test_that("the exported sheets interleave the values with their SD", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_inputs())
    mm <- mean_max_calculate()

    expect_equal(dim(mm$graphs_shaped_w_sd), c(192L, 57L))
    expect_equal(dim(mm$all_means_shaped_w_sd), c(2L, 56L))
    expect_equal(colnames(mm$all_means_shaped_w_sd)[1:4],
                 c("genotype 1 Super Elicitor [5µM]", "SD",
                   "genotype 2 Super Elicitor [5µM]", "SD"))

    # documented current behaviour: the maxima matrix is allocated with two
    # rows but only the first one is ever filled, so the exported sheet has a
    # trailing empty row
    expect_true(all(is.na(mm$all_means_shaped[2, ])))
  })
})

test_that("the well data sheet is labelled with genotype and elicitor", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_inputs())
    mm <- suppressWarnings(mean_max_calculate())
    nm <- colnames(mm$ms_normdata_names)

    expect_length(nm, 97L)
    expect_equal(nm[1], "genotype 1 Super Elicitor [5µM]")
    # documented current behaviour: the labels are derived by comparing two
    # vectors of different length, so the time column ends up without a name
    expect_true(is.na(nm[97]))
  })
})

test_that("the exclude slider moves the start of the maximum search", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_inputs(exclude4max = 0))
    expect_equal(as.numeric(mean_max_calculate()$all_means[1:4, 3]),
                 c(1.02204822894, 0.149468246997,
                   0.0863049660418, 0.0815279899763),
                 tolerance = 1e-9)

    session$setInputs(exclude4max = 20)
    expect_equal(as.numeric(mean_max_calculate()$all_means[1:4, 3]),
                 c(1.02204822894, 0.0424600300276,
                   0.0863049660418, 0.0815279899763),
                 tolerance = 1e-9)
  })
})

test_that("mean_max_calculate needs both a layout and a data file", {
  testServer(APP_DIR, {
    do.call(session$setInputs, app_inputs(
      data_file = example_upload("CaExample.xlsx"),
      layout_file = NULL, xlim = c(0, 1910), ylim = 1.8
    ))
    expect_null(mean_max_calculate())
  })
})

test_that("the calcium bar plot and kinetics plot are built from the means", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_inputs())

    bp <- bar_plots_max()
    expect_s3_class(bp, "ggplot")
    expect_equal(bp$labels$title, "Maxima with SD")
    expect_equal(bp$labels$y, "L/Lmax")
    expect_equal(nrow(bp$data), 28L)
    expect_equal(bp$data$values, ca_maxima, tolerance = 1e-10)
    expect_equal(bp$facet$params$ncol, 1)

    mg <- mean_graphs()
    expect_s3_class(mg, "ggplot")
    expect_equal(mg$labels$y, "L/Lmax")
    expect_equal(mg$labels$title, "Mean")
    expect_equal(names(mg$facet$params$facets), "elicitor")
    expect_equal(nrow(mg$data), 5376L)
  })
})

test_that("the plot settings change the layout of the summary plots", {
  testServer(APP_DIR, {
    do.call(session$setInputs, calcium_inputs(bar_columns = "2",
                                              bar_rotation = "2"))
    bp <- bar_plots_max()
    expect_equal(bp$facet$params$ncol, 2)
    expect_s3_class(bp$coordinates, "CoordFlip")

    session$setInputs(graph_sorting = "2")
    expect_equal(names(mean_graphs()$facet$params$facets), "genotype")

    session$setInputs(settings_mean = "1")
    mg <- mean_graphs()
    expect_equal(mg$labels$title, "Mean with SD")
    # mean line, +/- SD lines and error bars
    expect_length(mg$layers, 4L)
  })
})
