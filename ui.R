library(ggplot2)
library(DT)
library(shiny)
library(shinyBS)

shinyUI(fluidPage(

  # Application title
  titlePanel("CoordinateCleaner"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", label = h4("Input Data")),
      h4("Tests"),
      checkboxInput("val", label = "Validity", value = T),
      checkboxInput("cap", label = "Capitals"),
      checkboxInput("cen", label = "Centroids"),
      checkboxInput("con", label = "Country check"),
      checkboxInput("dpl", label = "Duplicates"),
      checkboxInput("gbf", label = "GBIF"),
      checkboxInput("otl", label = "Outliers"),
      checkboxInput("urb", label = "Urban"),
      checkboxInput("sea", label = "Seas"),
      checkboxInput("zer", label = "Zeros"),
      
      numericInput("outl.dist", label = h4("Outlier threshold distance"), value = 1000),
      
      h4("Data Download"),
    downloadButton('downloadData', label = 'Download Results'),
    
    bsPopover(id = "downloadData", title = "Download flags for each record", 
              placement = "right", trigger = "hover"),
    bsPopover(id = "file1", title = "A tab delimited file containing columns named: decimallongitude, decimallatitude, and optionally species, and countrycode", 
              placement = "right", trigger = "click"),
    bsPopover(id = "outl.dist", title = "The maximum distance to the next point of the same species.", 
              placement = "right", trigger = "click"),
    bsPopover(id = "val", title = "Coordinates validity under lat/lon", 
              placement = "right", trigger = "hover"),
    bsPopover(id = "cap", title = "0.1° radius around capitals", 
              placement = "right", trigger = "hover"),
    bsPopover(id = "cen", title = "0.1° radius around country and province centroids", 
              placement = "right", trigger = "hover"),
    bsPopover(id = "con", title = "Within the country specified by countrycode, needs a countrycode column in the input", 
              placement = "right", trigger = "hover"),
    bsPopover(id = "dpl", title = "Identical species + identical coordinates", 
              placement = "right", trigger = "hover"),
    bsPopover(id = "gbf", title = "0.5° radius around GBIF headquaters in Copenhagen", 
              placement = "right", trigger = "hover"),
    bsPopover(id = "otl", title = "More than outlier threshold distance in km to the next record of the same species, needs a species column in the input", 
              placement = "right", trigger = "hover"),
    bsPopover(id = "urb", title = "Within urbanized area", 
              placement = "right", trigger = "hover"),
    bsPopover(id = "sea", title = "In the ocean", 
              placement = "right", trigger = "hover"),
    bsPopover(id = "zer", title = "Plain zero coordinates or 0.5° radius around 0/0, or lat = long", 
              placement = "right", trigger = "hover")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(column(12,
                      plotOutput("distPlot")
               )),
      
      fluidRow(column(8, plotOutput("raster")),
               column(4, tableOutput("log")))
    )
  )
))
