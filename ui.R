library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("ENM Interactive Evaluation with GBIF"),
  helpText("Calibrate and evaluate ENMs with GBIF data."),
  
  fluidRow(
    column(3, wellPanel(
      textInput("gbifName", label = "Enter scientific name of species", value = ''),
      actionButton("goName", "Submit name")
    ),

      conditionalPanel("input.goName",
                       actionButton("goThin", "Thin occurrences"))
    ),
    
    column(5,
      conditionalPanel("input.goName",
                       htmlOutput('GBIFtxt'),
                       br(),
                       plotOutput('GBIFmap')
                       )
    ),
    
    column(4, conditionalPanel("input.goThin",
                               verbatimTextOutput("thinConsole")))
  )

))