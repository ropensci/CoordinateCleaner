library(ggplot2)
library(DT)
library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("CoordinateCleaner"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", label = h3("Input Data")),
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
      
    downloadButton('downloadData', label = 'Download Results')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
       tableOutput("table")
      ),
      fluidRow(column(9,
        plotOutput("distPlot")
      ),
      column(3,
             tableOutput("summary")))
    )
  )
))
