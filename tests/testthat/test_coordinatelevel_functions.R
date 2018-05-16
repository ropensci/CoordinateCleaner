library(CoordinateCleaner)
context("Coordinate cleaning")

set.seed(1)
lon <- runif(250, min = 42, max = 51)
set.seed(1)
lat <- runif(250, min = -26, max = -11)
exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimallongitude = lon,
                    decimallatitude = lat,
                    countrycode = "RUS")

test_that("cc_cap works", {
  expect_equal(sum(cc_cap(x = exmpl, value = "flags")), 250)
  expect_equal(sum(cc_cap(x = exmpl, buffer = 250, value = "flags")), 0)
  
  expect_error(cc_cap(x = exmpl, lon = "longitude", value = "flags"), 
               "undefined columns selected")
})

test_that("cc_cen works", {
  expect_equal(sum(cc_cen(x = exmpl, value = "flags")), 243)
  expect_equal(sum(cc_cen(x = exmpl, buffer = 250, value = "flags")), 0)
  
  expect_error(cc_cen(x = exmpl, lon = "longitude", value = "flags"), 
               "undefined columns selected")
})

test_that("cc_coun works", {
  #skip_on_cran()
  expect_equal(sum(cc_coun(x = exmpl, value = "flags")), 65)
  
  expect_error(cc_coun(x = exmpl, lon = "longitude", value = "flags"), 
               "undefined columns selected")
})


test_that("cc_outl works", {
  expect_equal(sum(cc_outl(x = exmpl, value = "flags")), 245)
  expect_equal(sum(cc_outl(x = exmpl, value = "flags", mltpl = 0.1)), 181)
  expect_equal(sum(cc_outl(x = exmpl, value = "flags", mltpl = 1000)), 250)
  
  expect_equal(sum(cc_outl(x = exmpl, 
                           value = "flags", 
                           method = "distance", 
                           tdi = .001)), 31)
  expect_equal(sum(cc_outl(x = exmpl, 
                           value = "flags", 
                           method = "distance", tdi = 10000)), 250)

  expect_error(cc_dupl(x = exmpl, 
                       lon = "longitude", 
                       value = "flags"), 
               "undefined columns selected")
})

#extend input data
exmpl <- rbind(exmpl, data.frame(species = rep("a", 50), 
                                 decimallongitude = rep( 50.25, 50), 
                                 decimallatitude = rep(55.77), 
                                 countrycode = "RUS"))

exmpl <- data.frame(exmpl, 
                    collector = "Bonpl",
                    collector.number = c(1001, 354),
                    collection = rep(c("K", "WAG","FR", "P", "S"), 20))


test_that("cc_dupl works", {
  expect_equal(sum(cc_dupl(x = exmpl, value = "flags")), 251)
  expect_equal(sum(cc_dupl(x = exmpl, 
                           additions = c("collector", 
                                         "collector.number", 
                                         "collection"), 
                           value = "flags")), 260)
  
  expect_error(cc_dupl(x = exmpl, 
                       lon = "longitude", 
                       value = "flags"), 
               "undefined columns selected")
})
