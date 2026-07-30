# curateQ() reshapes the raw Luminoscan .txt export of one plate quadrant into
# a well-per-column data frame ("Combine Files" tool).  The repository does not
# ship an example .txt, so the tests build a synthetic file that follows the
# layout the function expects:
#
#   * tab separated, comma as decimal separator
#   * a leading label column that is discarded
#   * the wells of a quadrant are spread over several blocks stacked on top of
#     each other; every block starts with a header row of well names
#   * measurement export: 1 header + 72 value rows per block
#   * discharge export:   1 header + 6 value rows per block
#
# write_luminoscan() and quadrant_files() build such a file, see helper-app.R.

test_that("curateQ returns 0 when a file is missing", {
  expect_equal(app$curateQ(NULL, NULL), 0)
  expect_equal(app$curateQ("something.txt", NULL), 0)
  expect_equal(app$curateQ(NULL, "something.txt"), 0)
})

test_that("curateQ stacks measurement and discharge values per well", {
  dir <- new_temp_dir()
  files <- quadrant_files(dir, c("M:A1", "M:A2", "M:A3"), c("M:B1", "M:B2", "M:B3"))

  q <- app$curateQ(files$measurement, files$discharge)

  expect_s3_class(q, "data.frame")
  # 72 measurement + 6 discharge rows, 6 wells
  expect_equal(dim(q), c(78L, 6L))
  # the "M:" prefix of the Luminoscan export is stripped
  expect_equal(colnames(q), c("A1", "A2", "A3", "B1", "B2", "B3"))

  # first block, first well: values 1.1 .. 72.1 then discharge 1001.1 .. 1006.1
  expect_equal(q$A1[1:3], c(1.1, 2.1, 3.1))
  expect_equal(q$A1[72], 72.1)
  expect_equal(q$A1[73:78], c(1001.1, 1002.1, 1003.1, 1004.1, 1005.1, 1006.1))
  # second block starts over at the same values
  expect_equal(q$B1[1:3], c(1.1, 2.1, 3.1))
  expect_equal(q$A3[1], 1.3)
  expect_true(all(vapply(q, is.numeric, logical(1))))
})

test_that("curateQ replaces missing values with zero", {
  dir <- new_temp_dir()
  files <- quadrant_files(dir, c("M:A1", "M:A2"), c("M:B1", "M:B2"))

  # blank out one measurement cell
  lines <- readLines(files$measurement)
  lines[3] <- "2\t\t2,2"
  writeLines(lines, files$measurement)

  q <- app$curateQ(files$measurement, files$discharge)
  expect_equal(q$A1[2], 0)
})
