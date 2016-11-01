
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(ggplot2)
library(DT)
library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", label = h3("Occurence Data")),
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
      fluidRow(
        plotOutput("distPlot")
      )
    )
  )
))
