library(CoordinateCleaner)
context("Coordinate cleaning")

# Coordinate level cleaning
set.seed(1)
sp <- sample(letters, size = 250, replace = TRUE)
set.seed(1)
lon <- runif(250, min = 42, max = 51)
set.seed(1)
lat <- runif(250, min = -26, max = -11)

exmpl <- data.frame(species = sp,
                    decimallongitude = lon,
                    decimallatitude = lat,
                    ISO3 = "RUS")
t1 <- CleanCoordinates(x = exmpl)

test_that("CleanCoordinates produces correct output", {

  expect_equal(ncol(t1), 12)
  expect_equal(nrow(t1), 250)
  expect_equal(sum(t1$summary), 187)

})

test_that("CleanCoordinates countries argument produces correct output", {
  skip_on_cran()
  expect_equal(sum(CleanCoordinates(x = exmpl, countries = "ISO3", countrycheck = T)$summary), 2)
})

test_that("CleanCoordinates S3 methods work", {
  expect_is(plot(t1), "gg")
  expect_is(plot(t1, clean = FALSE), "gg")
  expect_is(plot(t1, details = FALSE), "gg")
  expect_is(plot(t1, details = FALSE, clean = FALSE), "gg")
  
  expect_is(summary(t1), "integer")
  
  expect_equal(is(t1), "spatialvalid")
})



#Dataset level cleaning
#Create test dataset
clean <- data.frame(dataset = rep("clean", 1000),
                    decimallongitude = runif(min = -43, max = -40, n = 1000),
                    decimallatitude = runif(min = -13, max = -10, n = 1000))

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
  #test activated
  expect_is(CleanCoordinatesDS(test), "data.frame")
  expect_is(CleanCoordinatesDS(test, periodicity = F), "data.frame")
  expect_is(CleanCoordinatesDS(test, ddmm = F), "data.frame")
  
  #Output value
  expect_is(CleanCoordinatesDS(test, value = "clean"), "data.frame")
  expect_is(CleanCoordinatesDS(test, value = "flags"), "data.frame")
  
  expect_equal(sum(CleanCoordinatesDS(test)$summary), 1)
  

})


#Fossil wrapper function
set.seed(1)
minages <- runif(250, 0, 65)
set.seed(1)
lat <- runif(250, min = -26, max = -11)
set.seed(1)
lng <- runif(250, min = 42, max = 51)
set.seed(1)
age <- runif(250, 0.1, 65)

exmpl <- data.frame(accepted_name = sample(letters, size = 250, replace = TRUE),
                    lng = lng,
                    lat = lat,
                    min_ma = minages,
                    max_ma = minages + age)


test_that("fossil wrapper cleaning works", {
  expect_is(CleanCoordinatesFOS(exmpl), "spatialvalid")
  expect_equal(sum(CleanCoordinatesFOS(exmpl)$summary), 248)
})



#Write Pyrate output

test.str1 <- "test.pdf"

test_that("WritePyRate interal functions work", {
  expect_is(CoordinateCleaner:::.NoExtension(test.str1), "character")
  expect_equal(CoordinateCleaner:::.NoExtension(test.str1), "test")
})
