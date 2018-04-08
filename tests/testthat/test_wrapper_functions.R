library(CoordinateCleaner)
context("Coordinate cleaning")

set.seed(1)

# Coordinate level cleaning
exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimallongitude = runif(250, min = 42, max = 51),
                    decimallatitude = runif(250, min = -26, max = -11))

test_that("cc_cap arguments work", {
  expect_equal(ncol(CleanCoordinates(x = exmpl)), 12)
  expect_equal(nrow(CleanCoordinates(x = exmpl)), 250)
})


#Dataset level cleaning
#Create test dataset
clean <- data.frame(dataset = rep("clean", 1000),
                    decimallongitude = runif(min = -42, max = -40, n = 1000),
                    decimallatitude = runif(min = -12, max = -10, n = 1000))

bias.long <- c(round(runif(min = -42, max = -40, n = 500), 1),
               round(runif(min = -42, max = -40, n = 300), 0),
               runif(min = -42, max = -40, n = 200))
bias.lat <- c(round(runif(min = -12, max = -10, n = 500), 1),
              round(runif(min = -12, max = -10, n = 300), 0),
              runif(min = -12, max = -10, n = 200))
bias <- data.frame(dataset = rep("biased", 1000),
                   decimallongitude = bias.long,
                   decimallatitude = bias.lat)
test <- rbind(clean, bias)


test_that("dataset level cleaning works", {
  skip_on_cran()
  expect_is(CleanCoordinatesDS(test), "data.frame")
  expect_equal(sum(CleanCoordinatesDS(test)$summary), 1)
})


#Fossil wrapper function
minages <- runif(250, 0, 65)
exmpl <- data.frame(accepted_name = sample(letters, size = 250, replace = TRUE),
                    lng = runif(250, min = 42, max = 51),
                    lat = runif(250, min = -26, max = -11),
                    min_ma = minages,
                    max_ma = minages + runif(250, 0.1, 65))


test_that("fossil wrapper cleaning works", {
  skip_on_cran()
  expect_is(CleanCoordinatesFOS(exmpl), "spatialvalid")
  expect_equal(sum(CleanCoordinatesFOS(exmpl)$summary), 1)
})

