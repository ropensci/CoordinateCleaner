library(tidyverse)
tt <- read.csv("C:/Users/alexander.zizka/Dropbox (Antonelli Lab)/Arbeit/Gothenburg/projects/29_CoordinateCleaner/analyses/input/pbdb_data_no_header.csv")

tt <- tt[1:100000,]
#more than 7 records
coun <- table(tt$identified_name)
coun <- coun[coun > 7]

tt.red <- tt[tt$identified_name %in% names(coun),]
tt.red$identified_name <- as.character(tt.red$identified_name)
rownames(tt.red) <- NULL

test <- tc_ages(x = tt, value = "flags")
test <- tc_equal(x = tt, value = "flags")

test <- tc_outl(x = tt.red, value = "flags")


tt.red <- data.frame(tt.red, flag = test)
check <- tt.red[!tt.red$flag, ]
check <- tt.red[tt.red$identified_name %in% check$identified_name,]

#plot records flagged as outleirs and check

# test CleanCOordinatesFOS
#countrycode
library(countrycode)
library(CoordinateCleaner)
tt$cc <- countrycode(tt$cc, origin = "iso2c", destination = "iso3c")

test.big <- CleanCoordinatesFOS(tt, outliers.threshold = 3)




