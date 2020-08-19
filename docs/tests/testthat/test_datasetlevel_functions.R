context("Dataset level functions ds_*")

#Create test dataset
set.seed(1)
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

#cd_round
test_that("cd_round identifies existing bias", {
  skip_on_cran()
  #test target
  ## multiple datasets
  expect_equal(mean(cd_round(test, value = "dataset", 
                             graphs = F, test = "both")$summary), 0.5)
  expect_equal(mean(cd_round(test, value = "dataset", 
                             graphs = F, test = "lat")$summary), 0.5)
  expect_equal(mean(cd_round(test, value = "dataset", 
                             graphs = F, test = "lon")$summary), 0.5)
  
  # single dataset
  
  expect_equal(mean(cd_round(bias, value = "dataset", 
                             graphs = F, test = "both")$summary), 0)
  expect_equal(mean(cd_round(bias, value = "dataset", 
                             graphs = F, test = "lat")$summary), 0)
  expect_equal(mean(cd_round(bias, value = "dataset", 
                             graphs = F, test = "lon")$summary), 0)
  
  #dataset output
  t1 <- cd_round(test, value = "dataset", graphs = F)
  expect_is(t1, "data.frame")
  expect_equal(sum(t1$summary), 1)
  
  #flags output
  t2 <- cd_round(test, value = "flagged", graphs = F)
  expect_is(t2, "logical")
  expect_equal(mean(t2), 0.5)
  
  #graphs
  expect_equal(mean(cd_round(test, value = "flagged", graphs = T)), 0.5)
  
  # test targets
  
  
  #column specification
  expect_error(cd_round(x = test, lat = "latitude"))
  expect_error(cd_round(x = test, lon = "longitude"))
  expect_error(cd_round(x = test, ds = "source"))
})


# cd_ddmm
set.seed(1)
clean <- data.frame(species = letters[1:10], 
                    decimallongitude = runif(100, -180, 180), 
                    decimallatitude = runif(100, -90,90),
                    dataset = "clean")
#problematic dataset
lon <- sample(0:180, size = 100, replace = TRUE) + runif(100, 0,0.59)
lat <- sample(0:90, size = 100, replace = TRUE) + runif(100, 0,0.59)

prob <-  data.frame(species = letters[1:10], 
                    decimallongitude = lon, 
                    decimallatitude = lat,
                    dataset = "prob")

test <- rbind(prob,clean)

test_that("cd_ddmm identifies existing bias", {
  skip_on_cran()
  t1 <- cd_ddmm(test, value = "dataset")
  expect_is(t1, "data.frame")
  expect_equal(sum(t1$pass), 1)
  
  t2 <- cd_ddmm(test, value = "flagged")
  expect_is(t2, "logical")
  expect_equal(mean(t2), 0.5)
  
  expect_equal(mean(cd_ddmm(test, value = "flagged")), 0.5)
  
  expect_error(cd_ddmm(x = test, lat = "latitude"))
  expect_error(cd_ddmm(x = test, lon = "longitude"))
  expect_error(cd_ddmm(x = test, ds = "source"))
})
