# norm.well() is the core normalization of the calcium assay: every measured
# value is expressed relative to the luminescence still remaining in the well
# (L/Lmax), assuming a fixed measurement interval of 10 seconds.

test_that("norm.well normalizes against the reverse cumulative sum", {
  # cs = c(10, 9, 7, 4); x / (10 * cs) * 1000
  expect_equal(
    app$norm.well(c(1, 2, 3, 4)),
    c(10, 1000 * 2 / 90, 1000 * 3 / 70, 100),
    tolerance = 1e-12
  )
})

test_that("norm.well leaves empty wells untouched", {
  expect_equal(app$norm.well(c(0, 0, 0)), c(0, 0, 0))
  expect_equal(app$norm.well(rep(0, 10)), rep(0, 10))
})

test_that("the last measurement point of a non-empty well is always 100", {
  set.seed(1)
  x <- c(runif(20, 1, 100), 5)
  expect_equal(app$norm.well(x)[21], 100, tolerance = 1e-12)
})

test_that("norm.well is scale invariant", {
  x <- c(3, 17, 42, 8, 1)
  expect_equal(app$norm.well(x), app$norm.well(x * 7.5), tolerance = 1e-12)
})

test_that("trailing zeros produce NaN (documented current behaviour)", {
  # 0 / (10 * 0) * 1000 -> NaN.  calculate_data() relies on this and replaces
  # the resulting NaN columns with 0 further downstream.
  expect_true(is.nan(app$norm.well(c(1, 2, 0))[3]))
})
