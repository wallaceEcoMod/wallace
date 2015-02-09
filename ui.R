library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("ENM Interactive Evaluation with GBIF"),
  helpText("Calibrate and evaluate ENMs with GBIF data."),
  
  fluidRow(
    column(3, wellPanel(
      textOutput('writeCurMap'),
      textInput("gbifName", label = "Enter scientific name of species", value = ''),
      actionButton("goName", "Submit name"),
      br(),
      actionButton("goMap", "Map points")
    ),

      conditionalPanel("input.goName",
                       actionButton("goThin", "Thin occurrences"))
    ),
    
    column(8,
      conditionalPanel("input.goName",
                       htmlOutput('GBIFtxt')),
      br(),
      conditionalPanel("input.goMap",
                       plotOutput('GBIFmap'),
                       textOutput('mapText'))
    ),
    
    column(4, conditionalPanel("input.goThin",
                               verbatimTextOutput("thinConsole")))
  )

))