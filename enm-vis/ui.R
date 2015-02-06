library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("ENM Interactive Evaluation with GBIF"),
  helpText("Calibrate and evaluate ENMs with GBIF data."),
  
#   fluidRow(
#     column(3, 
#     ),
    
    
    
    
  
  sidebarLayout(
    sidebarPanel(
      textInput("gbif.name", label = "Enter scientific name of species", value = ''),
      actionButton("goName", "Submit name"),
      br(),
      numericInput("gbif.lim", label = "Enter limit for number of records", value = 500),
      actionButton('goLim', 'Submit limit')
    ),
    mainPanel(
      textOutput('gbif.txt1'),
      br(),
      textOutput('gbif.txt2'),
      plotOutput('gbif.map')
    )
  )
))