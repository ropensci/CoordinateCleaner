
CoordinateCleaner 2.0-2 (2018-08-20)
=========================

### MAJOR IMPROVEMENTS

  * sampling correction into cc_outl
  * tutorial on outlier detection on the bookdown documentation
  * subsampling heuristic in cc_outl
  
### MINOR IMPROVEMENTS

  * reduced packages size, by switching to data download from rnauralearth for urbanareas and landmass
  * fixed issue with names of plot.spatialvalid
  

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
