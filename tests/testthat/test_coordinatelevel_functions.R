context("Individual coordinate functions cc_*")

require(rnaturalearth)
require(dplyr)

# setup test data
set.seed(1)
lon <- runif(250, min = 42, max = 51)
set.seed(2)
lat <- runif(250, min = -26, max = -11)
exmpl <- data.frame(species = sample(letters[-1], size = 250, replace = TRUE),
                    decimallongitude = lon,
                    decimallatitude = lat,
                    countrycode = "RUS")

range_species_A <- cbind(cbind(c(-180,-180, 180, 180,-180),
                               c(-90,90,90,-90,-90)))
range <- terra::vect(range_species_A, "polygons")
range$binomial <- "a"
terra::crs(range) <- '+proj=longlat +datum=WGS84 +no_defs'

# run tests
## cc_cap
test_that("cc_cap works", {
  expect_equal(sum(cc_cap(x = exmpl, value = "flagged")), 250)
  expect_equal(sum(cc_cap(x = exmpl, value = "flagged"), verbose = FALSE), 250)
  expect_equal(sum(cc_cap(x = exmpl, buffer = 1000000, value = "flagged")), 0)
  expect_equal(sum(cc_cap(x = exmpl, buffer = 1000000, 
                          value = "flagged", verify = TRUE)), 0)
  
  expect_error(cc_cap(x = exmpl, lon = "longitude", value = "flagged"), 
               "undefined columns selected")
})

## cc_cen
test_that("cc_cen works", {
  expect_equal(sum(cc_cen(x = exmpl, value = "flagged")), 250)
  expect_equal(sum(cc_cen(x = exmpl, value = "flagged"), verbose = FALSE), 250)
  expect_equal(sum(cc_cen(x = exmpl, buffer = 900000, value = "flagged")), 0)
  
  expect_error(cc_cen(x = exmpl, lon = "longitude", value = "flagged"), 
               "undefined columns selected")
})

## cc_coun
test_that("cc_coun works", {
skip_on_cran()
  exmpl2 <-  data.frame(decimallatitude = c(51.5, -10), 
                        decimallongitude = c(8, 40),
                        countrycode = c("DEU", "DEU"))
  
  cust_ref1 <- terra::vect(rnaturalearth::ne_countries(scale = "small"))
  cust_ref1 <- terra::project(cust_ref1, '+proj=longlat +datum=WGS84 +no_defs')
  cust_ref2 <- cust_ref1

  expect_is(cc_coun(exmpl, value = "flagged"), "logical")
  expect_is(cc_coun(exmpl, value = "clean"), "data.frame")
  
  #customized references
  expect_equal(nrow(cc_coun(x = exmpl2)), 1)
  expect_error(cc_coun(x = exmpl2, ref = cust_ref1, ref_col = "test"))
  expect_equal(nrow(cc_coun(
    x = exmpl2, ref = cust_ref2, ref_col = "iso_a3_eh"
  )), 1)
  
  
  expect_equal(sum(
    cc_coun(x = exmpl, value = "flagged", ref = cust_ref1)
  ), 0)
  expect_equal(sum(cc_coun(x = exmpl, value = "flagged")), 0)
  
  expect_error(cc_coun(x = exmpl, lon = "longitude", value = "flagged"), 
               "undefined columns selected")

})

## cc_gbif
test_that("cc_gbif works", {
  
  t.gbif <- rbind(exmpl, data.frame(species = c("a", "a", "bia"), 
                                    decimallongitude = c(12.58, 12.585, 12.58),
                                    decimallatitude = c(55.67, 55.676, 55.67),
                                    countrycode = "RUS"))
  
  expect_is(cc_gbif(exmpl, value = "flagged"), "logical")
  expect_is(cc_gbif(exmpl, value = "clean"), "data.frame")
  
  expect_equal(sum(cc_gbif(x = t.gbif, value = "flagged")), 250)
  expect_equal(sum(cc_gbif(x = t.gbif, value = "flagged", geod = FALSE)), 250)
  
  expect_equal(sum(cc_gbif(x = t.gbif, value = "flagged", verify = T)), 252)
  expect_equal(sum(cc_gbif(x = t.gbif, value = "flagged",
                           verify = T, buffer = 100)), 251)
  
  expect_error(cc_gbif(x = exmpl, lon = "longitude", value = "flagged"), 
               "undefined columns selected")
})

# cc_inst
test_that("cc_inst works", {
skip_on_cran()
  t.inst <- rbind(exmpl, data.frame(species = c("a", "a", "bia"), 
                                    decimallongitude = c(12.58, 12.585, 12.58),
                                    decimallatitude = c(55.67, 55.676, 55.67),
                                    countrycode = "RUS"))
  resA <- cc_inst(t.inst, value = "flagged")
  expect_is(resA, "logical")
  expect_is(cc_inst(t.inst, value = "clean"), "data.frame")
  
  expect_equal(sum(resA), 253)
  expect_equal(sum(cc_inst(x = t.inst, value = "flagged", geod = FALSE)), 253)
  
  expect_equal(sum(cc_inst(x = t.inst, value = "flagged", 
                           geod = FALSE, buffer = 0.01)), 253)
  
  expect_equal(sum(cc_inst(x = t.inst, 
                           value = "flagged",
                           verify = T)), 
               253)
  expect_equal(sum(cc_inst(x = t.inst, 
                           value = "flagged", 
                           verify = T, 
                           geod = FALSE, 
                           buffer = 100)), 
               253)
  
  expect_error(cc_inst(x = exmpl, lon = "longitude", value = "flagged"), 
               "undefined columns selected")
})


# cc_iucn
test_that("cc_iucn works", {
skip_on_cran()
  res_iucn <- expect_warning(cc_iucn(x = exmpl, range = range,
                      value = "flagged",
                      verbose = FALSE))
  expect_equal(sum(res_iucn),
               nrow(exmpl))
  res_iucn2 <-
    expect_warning(cc_iucn(x = exmpl, range = range, value = "flagged"))
  expect_true(sum(res_iucn2) > 0)
})

# cc_outl (ADD SUPRESS WARNINGS)
test_that("cc_outl works", {
skip_on_cran() 
  
  expect_equal(sum(cc_outl(
    x = exmpl, value = "flagged", min_occs = 5
  )), 249)
  expect_equal(sum(
    cc_outl(x = exmpl, value = "flagged", verbose = FALSE, min_occs = 5)
  ), 249)
  expect_equal(sum(
    cc_outl(x = exmpl, value = "flagged", mltpl = 0.1, min_occs = 5)
  ), 197)
  expect_equal(sum(
    cc_outl(x = exmpl, value = "flagged", mltpl = 1000, min_occs = 5)
  ), 250)
  
  
  if (class(try(rgbif::occ_count(country = "DEU"), silent = TRUE)) == "try-error") {
    expect_equal(sum(cc_outl(x = exmpl, value = "flagged", 
                             sampling_thresh = 0.2, mltpl = 0.1, 
                             min_occs = 5)), 197) 
  } else {
    expect_equal(sum(cc_outl(x = exmpl, value = "flagged", 
                             sampling_thresh = 0.2, mltpl = 0.1,
                             min_occs = 5)), 197) 
  }
  expect_equal(sum(cc_outl(x = exmpl, 
                           value = "flagged", 
                           method = "distance", 
                           tdi = .001, min_occs = 5)), 0)
  expect_equal(sum(cc_outl(x = exmpl, 
                           value = "flagged", 
                           method = "distance", tdi = 10000, 
                           min_occs = 5)), 250)

  expect_error(cc_dupl(x = exmpl, 
                       lon = "longitude", 
                       value = "flagged"), 
               "undefined columns selected")
})

# cc_sea
test_that("cc_sea works", {
  skip_on_cran()
  skip_if_offline()
  
  cust_ref1 <- cust_ref2 <- terra::vect(rnaturalearth::ne_download(scale = 
                                                         "medium", 
                                                       type = 'land', 
                                                       category = "physical"))
  # terra::project(cust_ref2, "")
  # proj4string(cust_ref2) <- ""

  expect_is(cc_sea(exmpl, value = "flagged", ref = cust_ref1), "logical")
  expect_is(cc_sea(exmpl, value = "clean", ref = cust_ref1), "data.frame")
  
  # custom reference
  expect_equal(sum(cc_sea(x = exmpl, value = "flagged")), 98)
  expect_equal(sum(cc_sea(x = exmpl, value = "flagged", ref = cust_ref1)), 98)
  expect_equal(sum(cc_sea(x = exmpl, value = "flagged", ref = cust_ref2)), 98)

  # speedup
  expect_equal(sum(cc_sea(x = exmpl, value = "flagged", speedup = FALSE)), 98)
  
  expect_error(cc_sea(x = exmpl, lon = "longitude", value = "flagged"), 
               "undefined columns selected")
})

# cc_urb

test_that("cc_urb works", {
  skip_on_cran()
  skip_if_offline()
  
  cust_ref <-
    terra::vect(rnaturalearth::ne_download(scale = "medium", type = 'urban_areas'))
  xy <- terra::crds(cust_ref)[sample(1:terra::ncell(cust_ref), 200), ]
  city_exmpl <- data.frame(species = letters[1:10], 
                           xy )
  names(city_exmpl) <- c("species", "decimallongitude", "decimallatitude")
  city_exmpl <- dplyr::bind_rows(exmpl, city_exmpl)
  
  expect_is(cc_urb(city_exmpl, value = "flagged"), "logical")
  expect_is(cc_urb(city_exmpl, value = "clean"), "data.frame")

  expect_equal(sum(cc_urb(x = city_exmpl, value = "flagged")), 250)
  expect_equal(sum(cc_urb(x = city_exmpl, value = "flagged",
                          ref = cust_ref)), 250)
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
skip_on_cran()
  expect_equal(sum(cc_dupl(x = exmpl, value = "flagged")), 251)
  expect_equal(sum(cc_dupl(x = exmpl, 
                           additions = c("collector", 
                                         "collector.number", 
                                         "collection"), 
                           value = "flagged")), 260)
  
  expect_error(cc_dupl(x = exmpl, 
                       lon = "longitude", 
                       value = "flagged"), 
               "undefined columns selected")
})
