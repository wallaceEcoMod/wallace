library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("ENM Interactive Evaluation with GBIF"),
  helpText("Calibrate and evaluate ENMs with GBIF data."),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel("output.fileUploaded",
                       textInput("gbifName", label = "Enter scientific name of species", value = ''),
                       actionButton("goName", "Submit name")),
      br(),
      conditionalPanel("output.gbifSearched",
                       fileInput(inputId = "csvInput", label = "Upload Occurrence CSV")),
      br(),
      br(),
      conditionalPanel("input.goName",
                       actionButton("goMap", "Map points")),
      br(),
      conditionalPanel("input.goName",
                       numericInput("thinDist", label = "Thinning distance (km)", value = 25),
                       actionButton("goThin", "Run spThin")),
      br(),
      conditionalPanel("input.goName",
                       checkboxGroupInput("fcs", label = "Select feature classes", 
                                          choices = list("L" = "L", "LQ" = "LQ", "H" = "H",
                                                         "LQH" = "LQH", "LQHP" = "LQHP", "LQHPT" = "LQHPT"),
                                          selected = c("L", "LQ")),
                       sliderInput("rms", label = "Select regularization multipliers",
                                   min = 0, max = 10, value = c(1, 5)),
                       numericInput("rmsBy", label = "RM step value", value = 1),
                       selectInput("method", label = "Evaluation method",
                                   choices = list("jackknife" = "jackknife", "block" = "block",
                                                  "checkerboard1" = "checkerboard1", "checkerboard2" = "checkerboard2",
                                                  "randomkfold" = "randomkfold", "user" = "user"), selected = "block"),
                       actionButton("goEval", "Run ENMeval"))
      ),
  
    mainPanel(
      tabsetPanel(
        tabPanel("Main", 
                 conditionalPanel("input.goName", textOutput('GBIFtxt')),
                 conditionalPanel("output.fileUploaded", textOutput('a')),
                 conditionalPanel("input.goMap", textOutput('mapText')),
                 conditionalPanel("input.goThin", textOutput('thinText')),
                 conditionalPanel("input.goEval", textOutput('evalTxt')),
                 br(),
                 plotOutput('GBIFmap')),
        tabPanel("Occurrence Table", tableOutput('occTbl')),
        tabPanel("ENMeval Table", tableOutput('evalTbl'))
        #tabPanel("Plots", plotOutput())
      )
    )
  )
))
