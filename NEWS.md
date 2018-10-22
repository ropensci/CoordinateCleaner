CoordinateCleaner 2.0-3 (2018-10-22)
=========================

### MINOR IMPROVEMENTS
  * Removed convenience functionality to only download data from rnaturalearth at first use, to comply with CRAN guidelines


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

  * reduced packages size, by switching to data download from rnauralearth for urbanareas and landmass
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
  * enables sf format for sustom references
  * added speedup option for cc_sea
  * added webpage (https://azizka.github.io/CoordinateCleaner/)


CoordinateCleaner 1.2-1 (2018-06-08)
=========================

### MAJOR IMPROVEMENTS

  * Adapted function and argument names consistently to underscore_case
  * Simplified internal code structure of wrapper functions
  
### MINOR IMPROVEMENTS
  * adapted package to Ropensci reviews
  
### DEPRECATED AND DEFUNCT
  * CleanCoordinates deprecated, replaced by clean_coordinates
  * CleanCoordinatesDS deprecated, replaced by clean_dataset
  * CleanCoordinatesFOS deprecated, replaced by clean_fossils
  * WritePyrate deprecated, replaced by writ_pyrate

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

  * Adaption of code to ROpensci guidelines

### BUG FIXES

### DEPRECATED AND DEFUNCT
