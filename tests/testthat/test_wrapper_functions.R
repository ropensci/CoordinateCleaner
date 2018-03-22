library(CoordinateCleaner)
context("Coordinate cleaning")

set.seed(1)
exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimallongitude = runif(250, min = 42, max = 51),
                    decimallatitude = runif(250, min = -26, max = -11))

test_that("cc_cap arguments work", {
  expect_equal(ncol(CleanCoordinates(x = exmpl)), 12)
  expect_equal(nrow(CleanCoordinates(x = exmpl)), 250)
})