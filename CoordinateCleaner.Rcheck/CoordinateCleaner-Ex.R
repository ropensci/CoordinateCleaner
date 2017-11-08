pkgname <- "CoordinateCleaner"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('CoordinateCleaner')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("CleanCoordinates")
### * CleanCoordinates

flush(stderr()); flush(stdout())

### Name: CleanCoordinates
### Title: Geographic Cleaning of Coordinates from Biologic Collections
### Aliases: CleanCoordinates summary.spatialvalid is.spatialvalid
### Keywords: Coordinate cleaning

### ** Examples


exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimallongitude = runif(250, min = 42, max = 51),
                    decimallatitude = runif(250, min = -26, max = -11))

test <- CleanCoordinates(x = exmpl)

summary(test)
#plot(test)




cleanEx()
nameEx("CleanCoordinatesDS")
### * CleanCoordinatesDS

flush(stderr()); flush(stdout())

### Name: CleanCoordinatesDS
### Title: Geographic Coordinate Cleaning based on Dataset Properties
### Aliases: CleanCoordinatesDS
### Keywords: Coordinate cleaning

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



cleanEx()
nameEx("capitals")
### * capitals

flush(stderr()); flush(stdout())

### Name: capitals
### Title: Global Capital Locations
### Aliases: capitals
### Keywords: gazetteers

### ** Examples

data(capitals)
str(capitals)



cleanEx()
nameEx("cc_cap")
### * cc_cap

flush(stderr()); flush(stdout())

### Name: cc_cap
### Title: Flag coordinates in cicinity of country Capitals.
### Aliases: cc_cap
### Keywords: "Coordinate cleaning"

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90))

cc_cap(x)
cc_cap(x, value = "flags")



cleanEx()
nameEx("cc_cen")
### * cc_cen

flush(stderr()); flush(stdout())

### Name: cc_cen
### Title: Flag Coordinates in Vicinity of Centroids of Countries and
###   Provinces.
### Aliases: cc_cen
### Keywords: "Coordinate cleaning"

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90))
                
cc_cen(x)
cc_cen(x, value = "flags")



cleanEx()
nameEx("cc_coun")
### * cc_coun

flush(stderr()); flush(stdout())

### Name: cc_coun
### Title: Flag Coordinates Outside their Reported Country
### Aliases: cc_coun
### Keywords: "Coordinate cleaning"

### ** Examples

## Not run: 
##D x <- data.frame(species = letters[1:10], 
##D                 decimallongitude = runif(100, -20, 30), 
##D                 decimallatitude = runif(100, 35,60),
##D                 countrycode = "RUS")
##D 
##D cc_coun(x, value = "flags")#non-terrestrial records are not flagged! Use cc_sea for these
## End(Not run)




cleanEx()
nameEx("cc_dupl")
### * cc_dupl

flush(stderr()); flush(stdout())

### Name: cc_dupl
### Title: Flag Duplicated Records
### Aliases: cc_dupl
### Keywords: "Coordinate cleaning"

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = sample(x = 0:10, size = 100, replace = TRUE), 
                decimallatitude = sample(x = 0:10, size = 100, replace = TRUE),
                collector = "Bonpl",
                collector.number = c(1001, 354),
                collection = rep(c("K", "WAG","FR", "P", "S"), 20))

cc_dupl(x, value = "flags")
cc_dupl(x, additions = c("collector", "collector.number"))



cleanEx()
nameEx("cc_equ")
### * cc_equ

flush(stderr()); flush(stdout())

### Name: cc_equ
### Title: Flags Records with Identical lat/lon
### Aliases: cc_equ
### Keywords: "Coordinate cleaning"

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90))

cc_equ(x)
cc_equ(x, value = "flags")



cleanEx()
nameEx("cc_gbif")
### * cc_gbif

flush(stderr()); flush(stdout())

### Name: cc_gbif
### Title: Flag Records Assigned to GBIF Headquarters
### Aliases: cc_gbif
### Keywords: "Coordinate cleaning"

### ** Examples

x <- data.frame(species = "A", 
                decimallongitude = c(12.58, 12.58), 
                decimallatitude = c(55.67, 30.00))
                
cc_gbif(x)
cc_gbif(x, value = "flags")



cleanEx()
nameEx("cc_inst")
### * cc_inst

flush(stderr()); flush(stdout())

### Name: cc_inst
### Title: Flag Records in the Vicinity of Biodiversity Institutions
### Aliases: cc_inst
### Keywords: "Coordinate cleaning"

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90))
                
cc_inst(x, buffer = 5)#large buffer for demonstration
cc_inst(x, value = "flags", buffer = 5)



cleanEx()
nameEx("cc_outl")
### * cc_outl

flush(stderr()); flush(stdout())

### Name: cc_outl
### Title: Flag Geographic Outliers in Species Distributions
### Aliases: cc_outl
### Keywords: "Coordinate cleaning"

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90))
                
cc_outl(x)
cc_outl(x, method = "quantile", value = "flags")
cc_outl(x, method = "distance", value = "flags", tdi = 10000)
cc_outl(x, method = "distance", value = "flags", tdi = 1000)



cleanEx()
nameEx("cc_sea")
### * cc_sea

flush(stderr()); flush(stdout())

### Name: cc_sea
### Title: Flag Non-terrestrial Coordinates
### Aliases: cc_sea
### Keywords: "Coordinate cleaning"

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90))
                
cc_sea(x, value = "flags")




cleanEx()
nameEx("cc_urb")
### * cc_urb

flush(stderr()); flush(stdout())

### Name: cc_urb
### Title: Flag Records Inside Urban Areas
### Aliases: cc_urb
### Keywords: "Coordinate cleaning"

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



cleanEx()
nameEx("cc_val")
### * cc_val

flush(stderr()); flush(stdout())

### Name: cc_val
### Title: Check Coordinate Validity in lat/lon
### Aliases: cc_val
### Keywords: "Coordinate cleaning"

### ** Examples

x <- data.frame(species = letters[1:10], 
                decimallongitude = c(runif(106, -180, 180), NA, "13W33'", "67,09", 305), 
                decimallatitude = runif(110, -90,90))
                
cc_val(x)
cc_val(x, value = "flags")



cleanEx()
nameEx("cc_zero")
### * cc_zero

flush(stderr()); flush(stdout())

### Name: cc_zero
### Title: Flag Zero Coordinates
### Aliases: cc_zero
### Keywords: "Coordinate cleaning"

### ** Examples

x <- data.frame(species = "A", 
                decimallongitude = c(0,34.84, 0, 33.98), 
                decimallatitude = c(23.08, 0, 0, 15.98))
                
cc_zero(x)
cc_zero(x, value = "flags")



cleanEx()
nameEx("centroids")
### * centroids

flush(stderr()); flush(stdout())

### Name: centroids
### Title: Global Country and Province Centroids
### Aliases: centroids
### Keywords: gazetteers

### ** Examples

data(centroids)
str(centroids)



cleanEx()
nameEx("countryref")
### * countryref

flush(stderr()); flush(stdout())

### Name: countryref
### Title: Country Centroids and Country Capitals
### Aliases: countryref
### Keywords: gazetteers

### ** Examples

data(countryref)



cleanEx()
nameEx("dc_ddmm")
### * dc_ddmm

flush(stderr()); flush(stdout())

### Name: dc_ddmm
### Title: Flag Datasets with a Degree Conversion Error.
### Aliases: dc_ddmm
### Keywords: "Coordinate cleaning"

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



cleanEx()
nameEx("dc_round")
### * dc_round

flush(stderr()); flush(stdout())

### Name: dc_round
### Title: Flag Datasets with a Significant Fraction of Rounded Coordinates
### Aliases: dc_round
### Keywords: "Coordinate cleaning"

### ** Examples

clean <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90),
                dataset = "clean")
#biased dataset        
bias.long <- c(round(runif(min = -42, max = -40, n = 500), 1),
               round(runif(min = -42, max = -40, n = 300), 0),
               runif(min = -42, max = -40, n = 200))
bias.lat <- c(round(runif(min = -12, max = -10, n = 500), 1),
              round(runif(min = -12, max = -10, n = 300), 0),
              runif(min = -12, max = -10, n = 200))
bias <- data.frame(species = letters[1:10],
                   decimallongitude = bias.long,
                   decimallatitude = bias.lat,
                   dataset = "rounded")
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
## End(Not run)



cleanEx()
nameEx("institutions")
### * institutions

flush(stderr()); flush(stdout())

### Name: institutions
### Title: Global Locations of Biodiversity Institutions.
### Aliases: institutions
### Keywords: gazetteers

### ** Examples

data(institutions)
str(institutions)



cleanEx()
nameEx("landmass")
### * landmass

flush(stderr()); flush(stdout())

### Name: landmass
### Title: Global Coastlines
### Aliases: landmass
### Keywords: gazetteers

### ** Examples

data("landmass")



cleanEx()
nameEx("plot.spatialvalid")
### * plot.spatialvalid

flush(stderr()); flush(stdout())

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
#plot(test)



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
