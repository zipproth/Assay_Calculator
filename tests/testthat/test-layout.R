# layout() reads a plate layout workbook and derives the plate coordinates
# (Row/Column) from the well names.

test_that("layout returns NULL without a file", {
  expect_null(app$read_plate_layout(NULL))
})

test_that("layout of the calcium example is read completely", {
  lay <- app$read_plate_layout(example_upload("CaExample_layout.xlsx"))

  expect_named(lay, c("empty_wells", "plate_layout"))
  expect_equal(names(lay$plate_layout),
               c("well", "genotype", "elicitor", "Row", "Column"))
  expect_equal(nrow(lay$plate_layout), 96)

  # A1..A12 is row 1, column 1..12; B1 starts row 2
  expect_equal(head(lay$plate_layout$Row, 14), c(rep(1, 12), 2, 2))
  expect_equal(head(lay$plate_layout$Column, 14), c(1:12, 1, 2))

  expect_equal(unique(lay$plate_layout$genotype), c("genotype 1", "genotype 2"))
  expect_equal(unique(lay$plate_layout$elicitor),
               c("Super Elicitor [5µM]", "Compound 1 [20 µM]",
                 "Compound 2 [20 µM]", "Compound 3 [20µM]",
                 "Compound 4 [20 µM]", "Compound 5 [20µM]",
                 "Compound 6 [20µM]", "Compound 7 [20 µM]",
                 "Compund 8 [5µM]", "Compund 9 [5µM]",
                 "Compund 10 [10µM]", "Compound 11 [10µM]",
                 "Compound 12 [25 µg/ml]", "Compound 13 [25 µg/ml]"))

  # every well of the example plate is annotated
  expect_equal(lay$empty_wells, character(0))
})

test_that("layout of the ROS example contains the control elicitor", {
  lay <- app$read_plate_layout(example_upload("ROSExample_layout.xlsx"))

  expect_equal(nrow(lay$plate_layout), 96)
  expect_equal(unique(lay$plate_layout$genotype), c("genotype1", "genotype2"))
  # ROS normalization requires wells marked "control"
  expect_true("control" %in% lay$plate_layout$elicitor)
  expect_equal(unique(lay$plate_layout$elicitor),
               c("Compound 1 [1µM]", "Compound 2 [1µM]",
                 "Compound 3 [1µM]", "Compound 4 [1µM]",
                 "MeOH", "control"))
  expect_equal(lay$empty_wells, character(0))
})

test_that("wells without genotype and elicitor are reported as empty", {
  lay <- app$read_plate_layout(example_upload("Layouttemplate.xlsx"))

  expect_equal(nrow(lay$plate_layout), 96)
  expect_length(lay$empty_wells, 96)
})
