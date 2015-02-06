library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("ENM Interactive Evaluation with GBIF"),
  helpText("Calibrate and evaluate ENMs with GBIF data."),
  
  fluidRow(
    column(3, wellPanel(
      textInput("gbifName", label = "Enter scientific name of species", value = ''),
      actionButton("goName", "Submit name")
    )),
    
    column(6,
      conditionalPanel("input.gbifName",
                       htmlOutput('gbif.txt'),
                       br(),
                       plotOutput('gbif.map'))
    ))  

))