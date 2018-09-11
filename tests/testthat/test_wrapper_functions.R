context("Wrapper functions")

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


test_that("clean_coordinates produces correct output", {
  t1 <- clean_coordinates(x = exmpl)
  expect_equal(ncol(t1), 13)
  expect_equal(nrow(t1), 250)
  expect_equal(sum(t1$summary), 184)
  
  expect_is(plot(t1), "gg")
  expect_is(plot(t1, clean = FALSE), "gg")
  expect_is(plot(t1, details = FALSE), "gg")
  expect_is(plot(t1, details = FALSE, clean = FALSE), "gg")
  
  expect_is(summary(t1), "integer")
  
  expect_equal(is(t1), "spatialvalid")

})

test_that("clean_coordinates countries argument produces correct output", {
  #skip_on_cran()
  expect_equal(sum(
    clean_coordinates(x = exmpl, countries = "ISO3", 
                      tests = c("countries", "seas"))$summary), 0)
})


test_that("CleanCoordinates work", {
  expect_equal(sum(CleanCoordinates(exmpl)$summary), 184)
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
  expect_is(clean_dataset(test), "data.frame")
  expect_is(clean_dataset(test, tests = c("ddmm")), "data.frame")
  expect_is(clean_dataset(test, tests = c("periodicity")), "data.frame")
  
  #Output value
  expect_is(clean_dataset(test, value = "clean"), "data.frame")
  expect_is(clean_dataset(test, value = "flagged"), "data.frame")
  
  expect_equal(sum(clean_dataset(test)$summary), 1)
})

# test_that("CleanCoordinatesDS work", {
#   expect_equal(CleanCoordinatesDS(test), 250)
# })



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
  expect_is(clean_fossils(exmpl), "spatialvalid")
  expect_equal(sum(clean_fossils(exmpl)$summary), 249)
})

test_that("CleanCoordinatesFOS work", {
  expect_equal(sum(CleanCoordinatesFOS(exmpl)$summary), 249)
})

#Write Pyrate output

test.str1 <- "test.pdf"

test_that("WritePyRate interal functions work", {
  expect_is(CoordinateCleaner:::.NoExtension(test.str1), "character")
  expect_equal(CoordinateCleaner:::.NoExtension(test.str1), "test")
})
