library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("ENM Interactive Evaluation with GBIF"),
  helpText("Calibrate and evaluate ENMs with GBIF data."),
  
  fluidRow(
    column(3, wellPanel(
      textInput("gbifName", label = "Enter scientific name of species", value = ''),
      actionButton("goName", "Submit name"),
      br(),
      br(),
      conditionalPanel("input.goName",
                       actionButton("goMap", "Map points")),
      br(),
      conditionalPanel("input.goName",
                         actionButton("goThin", "Run spThin")),
      br(),
      conditionalPanel("input.goName",
                         actionButton("goEval", "Run ENMeval"))
      )),
    
    column(8,
      conditionalPanel("input.goName",
                       textOutput('GBIFtxt')),
      conditionalPanel("input.goMap",
                       textOutput('mapText')),
      conditionalPanel("input.goThin",
                       textOutput('thinText')),
      br(),
      conditionalPanel("input.goMap",
                       plotOutput('GBIFmap'))
    )
  )
))
