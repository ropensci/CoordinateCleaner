context("Coordinate cleaning")

set.seed(1)
#tc_range
minages <- runif(n = 11, min = 0.1, max = 25)
test <- data.frame(species = c(letters[1:10], "z"),
                lng = c(runif(n = 9, min = 4, max = 16), 75, 7),
                lat = c(runif(n = 11, min = -5, max = 5)),
                min_ma = minages, 
                max_ma = minages + c(runif(n = 10, min = 0, max = 5), 25))




test_that("tc_range identifies existing bias", {
  #return value
  expect_is(tc_range(test, value = "flagged", taxon = ""), "logical")
  expect_is(tc_range(test, value = "clean", taxon = ""), "data.frame")
  
  #outlier method
  expect_equal(sum(tc_range(test, value = "flagged", method = "quantile", taxon = "")), 10)
  expect_equal(sum(tc_range(test, value = "flagged", method = "mad", taxon = "")), 10)
  expect_equal(sum(tc_range(test, value = "flagged", method = "time", taxon = "", max.range = 20, uniq.loc = F)), 10)
  
})


#tc_outl
set.seed(1)
minages <- c(runif(n = 11, min = 10, max = 25), 62.5)
test <- data.frame(species = c(letters[1:10], rep("z", 2)),
                lng = c(runif(n = 10, min = 4, max = 16), 75, 7),
                lat = c(runif(n = 12, min = -5, max = 5)),
                min_ma = minages, 
                max_ma = c(minages[1:11] + runif(n = 11, min = 0, max = 5), 65))


test_that("tc_outl identifies existing bias", {
  #return value
  expect_is(tc_outl(test, value = "flagged", taxon = ""), "logical")
  expect_is(tc_outl(test, value = "clean", taxon = ""), "data.frame")
  
  #outlier method
  expect_equal(sum(tc_outl(test, value = "flagged", method = "quantile", taxon = "")), 10)
  expect_equal(sum(tc_outl(test, value = "flagged", method = "quantile", taxon = "", uniq.loc = F)), 10)
  expect_equal(sum(tc_outl(test, value = "flagged", method = "mad", taxon = "")), 10)
  expect_equal(sum(tc_outl(test, value = "flagged", method = "mad", taxon = "", uniq.loc = F)), 10)
  
})