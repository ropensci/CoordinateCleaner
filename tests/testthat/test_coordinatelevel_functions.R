library(CoordinateCleaner)
context("Coordinate cleaning")

set.seed(1)
exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimallongitude = runif(250, min = 42, max = 51),
                    decimallatitude = runif(250, min = -26, max = -11),
                    countrycode = "RUS")

test <- cc_cap(x = exmpl, value = "flags")

summary(test)
sum(cc_cen(exmpl, value = "flags"))

test_that("cc_cap works", {
  expect_equal(sum(cc_cap(x = exmpl, value = "flags")), 249)
  expect_equal(sum(cc_cap(x = exmpl, buffer = 250, value = "flags")), 0)
  
  expect_error(cc_cap(x = exmpl, lon = "longitude", value = "flags"), "undefined columns selected")
})

test_that("cc_cen works", {
  expect_equal(sum(cc_cen(x = exmpl, value = "flags")), 249)
  expect_equal(sum(cc_cen(x = exmpl, buffer = 250, value = "flags")), 0)
  
  expect_error(cc_cen(x = exmpl, lon = "longitude", value = "flags"), "undefined columns selected")
})

test_that("cc_coun works", {
  expect_equal(sum(cc_coun(x = exmpl, value = "flags")), 165)
  
  expect_error(cc_coun(x = exmpl, lon = "longitude", value = "flags"), "undefined columns selected")
})