library(shiny)
library(sp)
library(raster)
library(geosphere)
library(rgeos)

source("helpers.R")

#hardcode options
buffer <- 1
capitals.rad <- 0.1
centroids.rad <- 0.1
centroid.details <- "both"
outliers.mtp <- 25
outliers.td <- NULL
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
  
  x <- reactive({data.frame(dataInput()[,c("decimallongitude", "decimallatitude")])})
  species <- reactive({dataInput()[,"species"]})
  countries <- reactive({dataInput()[,"countrycode"]})
  
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
   .CountryCheck(x(), countries = countries(), poly = countryborders)
 } else {
   rep(NA, nrow(x()))
 }})
 
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
   .OutlierCoordinates(x(), species = species(), mltpl = outliers.mtp, tdi = outliers.td)
 } else {
   rep(NA, nrow(x()))
 }})
 
 #Seas
 sea <- reactive({if(input$sea){
   load("landmass.rda")
   .WaterCoordinates(x())
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
  #switch to data tble or show summary instead
  output$table <- renderTable({
    if(is.null(input$file1))
      return()
    head(data.frame(out()))
  })
  
  output$distPlot <- renderPlot({
    if(is.null(input$file1))
      return()
    load("landmass.rda")
    plotter(out(), clean = T, detail = T)
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
