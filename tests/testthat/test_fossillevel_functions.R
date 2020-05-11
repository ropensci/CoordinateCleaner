context("Fossil cleaning tc_*")

set.seed(1)
#cf_range
minages <- runif(n = 100, min = 0.1, max = 25)
set.seed(1)
maxages <- minages + c(runif(n = 99, min = 0, max = 5), 25)

test <- data.frame(species = c(letters[1:9], "z"),
                lng = c(runif(n = 98, min = 4, max = 16), 75, 7),
                lat = c(runif(n = 100, min = -5, max = 5)),
                min_ma = minages, 
                max_ma = maxages)



# cf_range
test_that("cf_range identifies existing bias", {
#  skip_on_cran()
  #return value
  expect_is(cf_range(test, value = "flagged", taxon = ""), "logical")
  expect_is(cf_range(test, value = "clean", taxon = ""), "data.frame")
  
  #outlier method
  expect_equal(sum(cf_range(test, value = "flagged", 
                            method = "quantile", taxon = "")), 99)
  expect_equal(sum(cf_range(test, value = "flagged", 
                            method = "mad", taxon = "")), 99)
  expect_equal(sum(cf_range(test, value = "flagged", 
                            method = "time", taxon = "", 
                            max_range = 20)), 99)
  
  expect_equal(nrow(cf_range(test, value = "clean", 
                            method = "quantile", taxon = "",
                            uniq_loc = TRUE)), 99)
  expect_equal(nrow(cf_range(test, value = "clean", 
                             method = "mad", taxon = "",
                             uniq_loc = TRUE)), 99)
  expect_equal(nrow(cf_range(test, value = "clean", 
                             method = "time", taxon = "",
                             uniq_loc = TRUE)), 100)
  
  expect_equal(nrow(cf_range(test, value = "clean", 
                             method = "quantile", taxon = "species",
                             uniq_loc = TRUE)), 99)
  
  expect_equal(nrow(cf_range(test, value = "clean", 
                             method = "quantile", taxon = "species",
                             uniq_loc = TRUE)), 99)
})

#cf_age
test_that("cf_age runs", {
  skip_on_cran()
  #return value
  expect_is(cf_age(test, value = "flagged", taxon = ""), "logical")
  expect_is(cf_age(test, value = "clean", taxon = ""), "data.frame")
  
  #outlier method
  expect_equal(sum(cf_age(test, value = "flagged", 
                          method = "quantile",
                          taxon = "", replicates = 10)), 100)
  expect_equal(sum(cf_age(test, value = "flagged", 
                          method = "quantile", taxon = "", 
                          uniq_loc = F, replicates = 10)), 100)
  expect_equal(sum(cf_age(test, value = "flagged", 
                          method = "quantile", taxon = "species", 
                          uniq_loc = F, replicates = 10)), 100)
  expect_equal(sum(cf_age(test, value = "flagged", 
                          method = "mad", taxon = "",
                          replicates = 10, flag_thresh = 0.1, 
                          mltpl = 10)), 100)
  expect_equal(sum(cf_age(test, value = "flagged", 
                          method = "mad", taxon = "species",
                          replicates = 10, flag_thresh = 0.1, 
                          mltpl = 10)), 100)
  expect_equal(sum(cf_age(test, value = "flagged", 
                          method = "mad", taxon = "", 
                          uniq_loc = F)), 100)
})


#cf_outl
set.seed(1)
minages <- c(runif(n = 11, min = 10, max = 25), 62.5)
test <- data.frame(species = c(letters[1:10], rep("z", 2)),
                lng = c(runif(n = 10, min = 4, max = 16), 75, 7),
                lat = c(runif(n = 12, min = -5, max = 5)),
                min_ma = minages, 
                max_ma = c(minages[1:11] + 
                             runif(n = 11, min = 0, max = 5), 65))

test_that("cf_outl identifies existing bias", {
  skip_on_cran()
  #return value
  expect_is(cf_outl(test, value = "flagged", taxon = ""), "logical")
  expect_is(cf_outl(test, value = "clean", taxon = ""), "data.frame")
  
  #outlier method
  expect_equal(sum(cf_outl(test, value = "flagged", 
                           method = "quantile", taxon = "")), 10)
  expect_equal(sum(cf_outl(test, 
                           taxon = "", 
                           value = "flagged", 
                           method = "quantile")), 10)
  expect_equal(sum(cf_outl(test, value = "flagged", 
                           method = "mad", 
                           taxon = "")), 10)
  expect_equal(sum(cf_outl(test, value = "flagged", 
                           method = "mad", 
                           taxon = "")), 10)

})






