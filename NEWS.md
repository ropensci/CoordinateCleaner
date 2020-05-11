CoordinateCleaner 2.0-15 (2020-05-04)
=========================

### MINOR IMPROVEMENTS
  * adapted the url format in the description files


CoordinateCleaner 2.0-14 (2020-05-04)
=========================

### MINOR IMPROVEMENTS
  * fixed a bug in cc_cen when setting an alternative reference
  * added the ref_col argument to cc_coun to customize the column with ISO codes in the reference data
  * adapted code to changes in sp and rgdal
  * defunct CleanCoordinates, CleanCoordinatesDS, and CleanCoordinatesFOS
  * fixed issue with input data.frame with unordered rownames in cc_outl
  * fixed the 'ras not found' bug in cc_outl
  
  

CoordinateCleaner 2.0-13 (2019-06-18)
=========================

### MINOR IMPROVEMENTS
  * addressed the "ras not found" bug in cc_outl
  
CoordinateCleaner 2.0-12 (2019-05-2)
=========================

### MINOR IMPROVEMENTS
  * improved documentation of cc_outl
  * improved handling of rownames in cc_outl
  
  
CoordinateCleaner 2.0-11 (2019-04-24)
=========================

### MINOR IMPROVEMENTS
  * changes to the description file

CoordinateCleaner 2.0-10 (2019-04-23)
=========================

### MINOR IMPROVEMENTS
  * improved error handling by cc_sea and cc_urb, in case the default reference cannot be obtained from the web
  * added a reference for the methodology to the description file


CoordinateCleaner 2.0-9 (2019-04-02)
=========================

### MINOR IMPROVEMENTS
  * recoded cc_outl, and added a thinning argument to account for sampling bias
  * fixed a bug with the cc_outl test, that produced erroneous flags under some settings of mltpl
  * extended the example dataset for the coordinate level-test suite to be more realistic


CoordinateCleaner 2.0-8 (2019-03-21)
=========================

### MINOR IMPROVEMENTS
  * moved vignettes to online documentation
  * added an area column to the countryref dataset
  * fixed some minor spelling issues in the documentation


CoordinateCleaner 2.0-7 (2019-01-22)
=========================

### MINOR IMPROVEMENTS
  * added citation
  * reduced testing time on CRAN
  * improved documentation of the cc_outl function

CoordinateCleaner 2.0-6 (2019-01-16)
=========================

### MINOR IMPROVEMENTS
  * further url fixes

CoordinateCleaner 2.0-5 (2019-01-15)
=========================

### MINOR IMPROVEMENTS
  * fixed broken url to the CIA factbook
  

CoordinateCleaner 2.0-4 (2019-01-14)
=========================

### MINOR IMPROVEMENTS
  * minor bugfix with cc_cap
  * corrected duplicated vignette index entries 
  * updated maintainer email
  
  
CoordinateCleaner 2.0-3 (2018-10-22)
=========================

### MINOR IMPROVEMENTS
  * removed convenience functionality to only download data from rnaturalearth at first use, to comply with CRAN guidelines


CoordinateCleaner 2.0-2 (2018-10-12)
=========================

### MAJOR IMPROVEMENTS

  * tutorial on outlier detection on the bookdown documentation
  * tutorial on using custom gazetteers
  * rasterisation heuristic in cc_outl
  * added sampling correction to cc_outl  
  * added verify option to cc_inst
  * transfer to rOpenSci
  
  
### MINOR IMPROVEMENTS

  * reduced packages size, by switching to data download from rnaturalearth for urbanareas and landmass
  * fixed issue with names of plot.spatialvalid
  * grouped functions on documentation webpage
  * fixed broken links in the help pages
  ' improved documentation structure
  

CoordinateCleaner 2.0-1 (2018-06-08)
=========================

### MAJOR IMPROVEMENTS

  * changed and more consistent naming scheme for the functions
  
### MINOR IMPROVEMENTS
  * fixed typos in Readme
  * set a download from naturalearth as default for cc_urb
  * reduced vignette memory use and size
  * enables sf format for custom references
  * added speedup option for cc_sea
  * added webpage (https://azizka.github.io/CoordinateCleaner/)


CoordinateCleaner 1.2-1 (2018-06-08)
=========================

### MAJOR IMPROVEMENTS

  * Adapted function and argument names consistently to underscore_case
  * Simplified internal code structure of wrapper functions
  
### MINOR IMPROVEMENTS
  * adapted package to rOpenSci reviews
  
### DEPRECATED AND DEFUNCT
  * CleanCoordinates deprecated, replaced by clean_coordinates
  * CleanCoordinatesDS deprecated, replaced by clean_dataset
  * CleanCoordinatesFOS deprecated, replaced by clean_fossils
  * WritePyrate deprecated, replaced by write_pyrate

CoordinateCleaner 1.1-1 (2018-05-15)
=========================

### MINOR IMPROVEMENTS
  * Switched documentation and NAMESPACE generation to roxygen2
  * Switched from sapply to vapply
  * Improved code readability


CoordinateCleaner 1.1-0 (2018-04-08)
=========================

### NEW FEATURES

### MINOR IMPROVEMENTS

  * Adaption of code to rOpenSci guidelines

### BUG FIXES

### DEPRECATED AND DEFUNCT
