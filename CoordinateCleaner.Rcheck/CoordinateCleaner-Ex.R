pkgname <- "CoordinateCleaner"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "CoordinateCleaner-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('CoordinateCleaner')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("CleanCoordinates")
### * CleanCoordinates

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CleanCoordinates
### Title: Geographic Cleaning of Coordinates from Biologic Collections
### Aliases: CleanCoordinates summary.spatialvalid is.spatialvalid
### Keywords: Coordinate cleaning wrapper

### ** Examples


exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimallongitude = runif(250, min = 42, max = 51),
                    decimallatitude = runif(250, min = -26, max = -11))

test <- CleanCoordinates(x = exmpl)

summary(test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CleanCoordinates", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CleanCoordinatesDS")
### * CleanCoordinatesDS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CleanCoordinatesDS
### Title: Geographic Coordinate Cleaning based on Dataset Properties
### Aliases: CleanCoordinatesDS
### Keywords: Coordinate cleaning wrapper

### ** Examples

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

## Not run: 
##D                   
##D #run CleanCoordinatesDS
##D flags <- CleanCoordinatesDS(test)
##D 
##D #check problems
##D #clean
##D hist(test[test$dataset == rownames(flags[flags$summary,]), "decimallongitude"])
##D #biased
##D hist(test[test$dataset == rownames(flags[!flags$summary,]), "decimallongitude"])
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CleanCoordinatesDS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CleanCoordinatesFOS")
### * CleanCoordinatesFOS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CleanCoordinatesFOS
### Title: Geographic and Temporal Cleaning of Records from Fossil
###   Collections
### Aliases: CleanCoordinatesFOS
### Keywords: Fossil Coordinate cleaning Temporal cleaning

### ** Examples

minages <- runif(250, 0, 65)
exmpl <- data.frame(accepted_name = sample(letters, size = 250, replace = TRUE),
                    lng = runif(250, min = 42, max = 51),
                    lat = runif(250, min = -26, max = -11),
                    min_ma = minages,
                    max_ma = minages + runif(250, 0.1, 65))

test <- CleanCoordinatesFOS(x = exmpl)

summary(test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CleanCoordinatesFOS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("WritePyRate")
### * WritePyRate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: WritePyRate
### Title: Create Input Files for PyRate
### Aliases: WritePyRate
### Keywords: Fossil

### ** Examples

minages <- runif(250, 0, 65)
exmpl <- data.frame(identified_name = sample(letters, size = 250, replace = TRUE),
                    lng = runif(250, min = 42, max = 51),
                    lat = runif(250, min = -26, max = -11),
                    min_ma = minages,
                    max_ma = minages + runif(250, 0.1, 65))

#a vector with the status for each record, 
#make sure species are only classified as either extinct or extant, 
#otherwise the function will drop an error

status <- sample(c("extinct", "extant"), size = nrow(exmpl), replace = TRUE)

#or from a list of species
status <- sample(c("extinct", "extant"), size = length(letters), replace = TRUE)
names(status) <- letters
status <- status[exmpl$identified_name]

## Not run: 
##D WritePyRate(x = exmpl,fname = "test", status = status)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("WritePyRate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("capitals")
### * capitals

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: capitals
### Title: Global Capital Locations
### Aliases: capitals
### Keywords: gazetteers

### ** Examples

data(capitals)
str(capitals)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("capitals", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cc_cap")
### * cc_cap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cc_cap
### Title: Flag Coordinates in Vicinity of Country Capitals.
### Aliases: cc_cap
### Keywords: Coordinate cleaning

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90))

cc_cap(x)
cc_cap(x, value = "flags")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cc_cap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cc_cen")
### * cc_cen

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cc_cen
### Title: Flag Coordinates in Vicinity of Country and Province Centroids
### Aliases: cc_cen
### Keywords: Coordinate cleaning

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90))
                
cc_cen(x)
cc_cen(x, value = "flags")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cc_cen", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cc_coun")
### * cc_coun

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cc_coun
### Title: Flag Coordinates Outside their Reported Country
### Aliases: cc_coun
### Keywords: Coordinate cleaning

### ** Examples

## Not run: 
##D x <- data.frame(species = letters[1:10], 
##D                 decimallongitude = runif(100, -20, 30), 
##D                 decimallatitude = runif(100, 35,60),
##D                 countrycode = "RUS")
##D 
##D cc_coun(x, value = "flags")#non-terrestrial records are not flagged! Use cc_sea for these
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cc_coun", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cc_dupl")
### * cc_dupl

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cc_dupl
### Title: Flag Duplicated Records
### Aliases: cc_dupl
### Keywords: Coordinate cleaning

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = sample(x = 0:10, size = 100, replace = TRUE), 
                decimallatitude = sample(x = 0:10, size = 100, replace = TRUE),
                collector = "Bonpl",
                collector.number = c(1001, 354),
                collection = rep(c("K", "WAG","FR", "P", "S"), 20))

cc_dupl(x, value = "flags")
cc_dupl(x, additions = c("collector", "collector.number"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cc_dupl", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cc_equ")
### * cc_equ

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cc_equ
### Title: Flag Records with Identical lat/lon
### Aliases: cc_equ
### Keywords: Coordinate cleaning

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90))

cc_equ(x)
cc_equ(x, value = "flags")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cc_equ", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cc_gbif")
### * cc_gbif

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cc_gbif
### Title: Flag Records Assigned to GBIF Headquarters
### Aliases: cc_gbif
### Keywords: Coordinate cleaning

### ** Examples

x <- data.frame(species = "A", 
                decimallongitude = c(12.58, 12.58), 
                decimallatitude = c(55.67, 30.00))
                
cc_gbif(x)
cc_gbif(x, value = "flags")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cc_gbif", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cc_inst")
### * cc_inst

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cc_inst
### Title: Flag Records in the Vicinity of Biodiversity Institutions
### Aliases: cc_inst
### Keywords: Coordinate cleaning

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90))
                
cc_inst(x, buffer = 5)#large buffer for demonstration
cc_inst(x, value = "flags", buffer = 5)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cc_inst", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cc_outl")
### * cc_outl

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cc_outl
### Title: Flag Geographic Outliers in Species Distributions
### Aliases: cc_outl
### Keywords: Coordinate cleaning

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90))
                
cc_outl(x)
cc_outl(x, method = "quantile", value = "flags")
cc_outl(x, method = "distance", value = "flags", tdi = 10000)
cc_outl(x, method = "distance", value = "flags", tdi = 1000)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cc_outl", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cc_sea")
### * cc_sea

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cc_sea
### Title: Flag Non-terrestrial Coordinates
### Aliases: cc_sea
### Keywords: Coordinate cleaning

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(10, -30, 30), 
                decimallatitude = runif(10, -30, 30))
                
cc_sea(x, value = "flags")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cc_sea", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cc_urb")
### * cc_urb

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cc_urb
### Title: Flag Records Inside Urban Areas
### Aliases: cc_urb
### Keywords: Coordinate cleaning

### ** Examples

## Not run: 
##D # load reference 
##D #See details section on where to download the reference data
##D load("extra_gazetteers/urbanareas.rda")
##D 
##D x <- data.frame(species = letters[1:10], 
##D                 decimallongitude = runif(100, -180, 180), 
##D                 decimallatitude = runif(100, -90,90))
##D                 
##D cc_urb(x, ref = urbanareas)
##D cc_urb(x, value = "flags", ref = urbanareas)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cc_urb", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cc_val")
### * cc_val

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cc_val
### Title: Check Coordinate Validity in lat/lon
### Aliases: cc_val
### Keywords: Coordinate cleaning

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = c(runif(106, -180, 180), NA, "13W33'", "67,09", 305), 
                decimallatitude = runif(110, -90,90))
                
cc_val(x)
cc_val(x, value = "flags")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cc_val", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cc_zero")
### * cc_zero

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cc_zero
### Title: Flag Zero Coordinates
### Aliases: cc_zero
### Keywords: Coordinate cleaning

### ** Examples

x <- data.frame(species = "A", 
                decimallongitude = c(0,34.84, 0, 33.98), 
                decimallatitude = c(23.08, 0, 0, 15.98))
                
cc_zero(x)
cc_zero(x, value = "flags")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cc_zero", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("centroids")
### * centroids

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: centroids
### Title: Global Country and Province Centroids
### Aliases: centroids
### Keywords: gazetteers

### ** Examples

data(centroids)
str(centroids)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("centroids", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("countryref")
### * countryref

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: countryref
### Title: Country Centroids and Country Capitals
### Aliases: countryref
### Keywords: gazetteers

### ** Examples

data(countryref)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("countryref", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dc_ddmm")
### * dc_ddmm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dc_ddmm
### Title: Flag Datasets with a Degree Conversion Error
### Aliases: dc_ddmm
### Keywords: "Coordinate cleaning" "Dataset level cleaning"

### ** Examples

clean <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90),
                dataset = "FR")
                
dc_ddmm(x = clean, value = "flags")

#problematic dataset
lon <- sample(-180:180, size = 100, replace = TRUE) + runif(100, 0,0.59)
lat <- sample(-90:90, size = 100, replace = TRUE) + runif(100, 0,0.59)

prob <-  data.frame(species = letters[1:10], 
                decimallongitude = lon, 
                decimallatitude = lat,
                dataset = "FR")
                
dc_ddmm(x = prob, value = "flags")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dc_ddmm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dc_round")
### * dc_round

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dc_round
### Title: Flags Datasets with Rasterized Coordinates
### Aliases: dc_round
### Keywords: "Coordinate cleaning" "Dataset level cleaning"

### ** Examples

#simulate bias grid, one degree resolution, 10% error on a 1000 records dataset
  ##simulate biased fraction of the data, grid resolution = 1 degree
  
  
  
  #simulate non-biased fraction of the data
  bi <- sample(3 + 0:5, size = 100, replace = TRUE)
  mu <- runif(3, 0, 15)
  sig <- runif(3, 0.1, 5)
  cl <- rnorm(n = 900, mean = mu, sd = sig)
  lon <- c(cl, bi)
  
  bi <- sample(9:13, size = 100, replace = TRUE)
  mu <- runif(3, 0, 15)
  sig <- runif(3, 0.1, 5)
  cl <- rnorm(n = 900, mean = mu, sd = sig)
  lat <- c(cl, bi)
  
  #add biased data
  
  inp <- data.frame(decimallongitude = lon,
                    decimallatitude = lat,
                    dataset = "test")
            
  #plot overview
  suma <- inp
  suma[,1:2] <- round(suma[,1:2], 0)
  suma <- aggregate(dataset ~ decimallongitude + decimallatitude, FUN = "length", data = suma)
  colo <- rev(heat.colors(max(suma$dataset)))
  plot(suma$decimallatitude ~ suma$decimallongitude, col = colo[suma$dataset])
          
  #run test
  
  dc_round(inp, value = "dataset")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dc_round", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("institutions")
### * institutions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: institutions
### Title: Global Locations of Biodiversity Institutions
### Aliases: institutions
### Keywords: gazetteers

### ** Examples

data(institutions)
str(institutions)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("institutions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("landmass")
### * landmass

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: landmass
### Title: Global Coastlines
### Aliases: landmass
### Keywords: gazetteers

### ** Examples

data("landmass")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("landmass", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.spatialvalid")
### * plot.spatialvalid

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.spatialvalid
### Title: Plot Method for Class Spatialvalid
### Aliases: plot.spatialvalid
### Keywords: Visualisation

### ** Examples


exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimallongitude = runif(250, min = 42, max = 51),
                    decimallatitude = runif(250, min = -26, max = -11))

test <- CleanCoordinates(exmpl, species = "species", verbose = FALSE)

summary(test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.spatialvalid", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tc_equal")
### * tc_equal

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tc_equal
### Title: Flag Fossils with equal min and max age
### Aliases: tc_equal
### Keywords: Temporal cleaning Fossils

### ** Examples

minages <- runif(n = 10, min = 0.1, max = 25)
x <- data.frame(species = letters[1:10], 
                min_ma = minages, 
                max_ma = minages + runif(n = 10, min = 0, max = 10))
x <- rbind(x, data.frame(species = "z", 
                min_ma = 5, 
                max_ma = 5))
                
tc_equal(x, value = "flags")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tc_equal", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tc_outl")
### * tc_outl

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tc_outl
### Title: Flag Fossil Outlier Records in Space and Time
### Aliases: tc_outl
### Keywords: Fossil Coordinate cleaning Temporal cleaning

### ** Examples

minages <- c(runif(n = 11, min = 10, max = 25), 62.5)
x <- data.frame(species = c(letters[1:10], rep("z", 2)),
                lng = c(runif(n = 10, min = 4, max = 16), 75, 7),
                lat = c(runif(n = 12, min = -5, max = 5)),
                min_ma = minages, 
                max_ma = c(minages[1:11] + runif(n = 11, min = 0, max = 5), 65))

tc_outl(x, value = "flags", taxon = "")





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tc_outl", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tc_range")
### * tc_range

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tc_range
### Title: Flag Fossils with Extreme Age Ranges
### Aliases: tc_range
### Keywords: Fossil Temporal cleaning

### ** Examples

minages <- runif(n = 11, min = 0.1, max = 25)
x <- data.frame(species = c(letters[1:10], "z"),
                lng = c(runif(n = 9, min = 4, max = 16), 75, 7),
                lat = c(runif(n = 11, min = -5, max = 5)),
                min_ma = minages, 
                max_ma = minages + c(runif(n = 10, min = 0, max = 5), 25))

tc_range(x, value = "flags", taxon = "")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tc_range", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
