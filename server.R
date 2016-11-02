library(shiny)
library(sp)
library(raster)
library(geosphere)
library(rgeos)
library(viridis)

source("helpers.R")

#hardcode options
buffer <- 1
capitals.rad <- 0.1
centroids.rad <- 0.1
centroid.details <- "both"
outliers.mtp <- NULL
zeros.rad <- 0.5

#add plotting

shinyServer(function(input, output) {
  
  #set maximum file size
  options(shiny.maxRequestSize=35*1024^2) # change maximum upload size
  
  #load the input data
  dataInput <- reactive({
    inFile <- input$file1
    
    read.csv(inFile$datapath, sep = "\t")
  })
  
  #Define input values
  #coordinates
  x <- reactive({data.frame(dataInput()[,c("decimallongitude", "decimallatitude")])})
  
  #species identity
  species <- reactive({
    if("species" %in% names(dataInput())){
    dataInput()[,"species"]
    }else{
      NULL
    }
    })
  
  #country information
  countries <- reactive({
    if("countrycode" %in% names(dataInput())){
      dataInput()[,"countrycode"]
  }else{
        NULL
      }})
  
  #capital coordinates
 cap <- reactive({if(input$cap){
    load("capitals.rda")
    .CapitalCoordinates(x(), testdist = capitals.rad, buffer = buffer, 
                               referencedat = capitals)
  } else {
    rep(NA, nrow(x()))
  }})
 
  #Country check
 con <- reactive({if(input$con){
   load("countryborders.rda")
   if(is.null(countries())){
     rep(NA, nrow(x()))
   }else{
     .CountryCheck(x(), countries = countries(), poly = countryborders)
   }
   }else{rep(NA, nrow(x()))}
})
 
 #Country centroids
 cen <- reactive({if(input$cen){
   load("centroids.rda")
   .CentroidCoordinates(x(), testdist = centroids.rad, buffer = buffer, 
                               testtype = centroids.detail, referencedat = centroids)
 } else {
   rep(NA, nrow(x()))
 }})
 
 #Duplicates
 dpl <- reactive({if(input$dpl){
   if (is.null(species())) {
     dpl.test <- x()

   } else {
     dpl.test <- data.frame(x(), species())
   }
   !duplicated(dpl.test)
   
 } else {
   rep(NA, nrow(x()))
 }})
 
 #GBIF
 gbf <- reactive({if(input$gbf){
   .GBIF(x())
 } else {
    rep(NA, nrow(x()))
 }})
 
 #Outliers
 otl <- reactive({if(input$otl){
   if(is.null(species())){
     rep(NA, nrow(x()))
   }else{
     .OutlierCoordinates(x(), species = species(), mltpl = NULL, tdi = input$outl.dist)
   }
 } else {
   rep(NA, nrow(x()))
 }})
 
 #Seas
 sea <- reactive({if(input$sea){
   load("landmass.rda")
   .WaterCoordinates(x(), poly = landmass)
 } else {
   rep(NA, nrow(x()))
 }})
 
 #Urban
 urb <- reactive({if(input$urb){
   load("urbanareas.rda")
   .UrbanCoordinates(x(), poly = urbanareas)
 } else {
   rep(NA, nrow(x()))
 }})
 
 #Validity
 val <- reactive( 
   if(input$val){
     .ValidCoordinates(x())
   } else {
     rep(NA, nrow(x()))
   }
 )
 
 #Zeros
 zer <- reactive({if(input$zer){
   .ZeroCoordinates(x(), pointlim = zeros.rad)
 } else {
   rep(NA, nrow(x()))
 }})
 
 out <- reactive({
   out <- list(val(), zer(), cap(), cen(), sea(), urb(), con(), otl(), gbf(), dpl())
   out <- Filter(function(k) !all(is.na(k)), out)
   out <- Reduce("&", out)
   
   out <- data.frame(x(), capitals = cap(), countrycheck = con(), centroids = cen(),
                     duplicates = dpl(),  
                     gbif = gbf(), outliers = otl(),  sea = sea(), urban = urb(), 
                     validity = val(), zeros = zer(), summary = out)
   Filter(function(k) !all(is.na(k)), out)

 })
 
 ras <- reactive({
   if(sum(!out()$summary) > 0){
     pts <- SpatialPoints(out()[!out()$summary,1:2])
     pts.2 <- SpatialPoints(out()[,1:2])
     r <- raster(extent(pts.2))
     res(r) <- 0.1
     ras <- rasterize(pts, r, fun = "count")
     data.frame(coordinates(ras), as.data.frame(ras))
   }else{
     NULL
   }

 })

  output$distPlot <- renderPlot({
    if(is.null(input$file1))
      return()
    load("landmass.rda")
    plotter(out(), clean = T, detail = T)
  })
  
  output$log <- renderTable({
    if(is.null(input$file1))
      return()
    data.frame(Test = as.character(names(out()[-c(1:2)])),
               Flags = colSums(!out()[-c(1:2)]))
  }, digits = 0)
  
  output$raster <- renderPlot({
    if(is.null(input$file1))
      return()
    if(is.null(ras()))
      return()
      rasPlotter(x = ras(), y = out())  
    })
  
  #fix this
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("flagged_coordinates", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(out(), file, row.names = FALSE)
    }
  )

  

})
