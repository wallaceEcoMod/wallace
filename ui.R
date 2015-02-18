library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("ENM Interactive Evaluation with GBIF"),
  helpText("Calibrate and evaluate ENMs with GBIF data."),
  
  sidebarPanel(
    conditionalPanel("input.conditionedPanels == 1",
                     textInput("gbifName", label = "Enter scientific name of species", value = ''),
                     actionButton("goName", "Submit name"),
                     br(),
                     br(),
                     actionButton("goMap", "Map points")
    ),
    conditionalPanel("input.conditionedPanels == 2",
                     numericInput("thinDist", label = "Thinning distance (km)", value = 25),
                     actionButton("goThin", "Run spThin")
    ),
    conditionalPanel("input.conditionedPanels == 3",
                     selectInput("pred", label = "Predictor raster resolution",
                                 choices = list("Choose resolution" = "", 
                                                "30 arcsec" = "wc30arcsec", 
                                                "2.5 arcmin" = "wc2.5arcmin", 
                                                "5 arcmin" = "wc5arcmin", 
                                                "10 arcmin" = "wc10arcmin")),
                     selectInput("backg", label = "Background selection",
                                 choices = list("Choose option" = "", "Bounding box" = 'bb', 
                                                "Minimum convex polygon" = 'mcp')),
                     numericInput("backgBuf", label = "Background selection buffer distance", value = 0)
    ),
    conditionalPanel("input.conditionedPanels == 4",
                     checkboxGroupInput("fcs", label = "Select feature classes", 
                                        choices = list("L" = "L", "LQ" = "LQ", "H" = "H",
                                                       "LQH" = "LQH", "LQHP" = "LQHP", "LQHPT" = "LQHPT"),
                                        selected = c("L", "LQ")),
                     sliderInput("rms", label = "Select regularization multipliers",
                                 min = 0, max = 10, value = c(1, 5)),
                     numericInput("rmsBy", label = "RM step value", value = 1),
                     selectInput("method", label = "Evaluation method",
                                 choices = list("jackknife" = "jackknife", 
                                                "block" = "block",
                                                "checkerboard1" = "checkerboard1", 
                                                "checkerboard2" = "checkerboard2",
                                                "randomkfold" = "randomkfold"), selected = "block"),
                     actionButton("goEval", "Run ENMeval")
    )
  ),
  
  mainPanel(
    tabsetPanel(id = "conditionedPanels",
      tabPanel("Download Occurrence Points / Plot", value = 1,
               conditionalPanel("input.goName", textOutput('GBIFtxt')),
               conditionalPanel("input.goMap", textOutput('mapText')),
               conditionalPanel("input.goEval", textOutput('evalTxt')),
               br(),
               plotOutput('GBIFmap')
               # tableOutput('occTbl'), 
               ),
      tabPanel("Thin occurrences", value = 2,
               conditionalPanel("input.goThin", textOutput('thinText')),
               tableOutput('occTbl')),
      tabPanel("Prediction Rasters", value = 3,
               conditionalPanel("input.pred != ''", textOutput('predExtTxt'))
               ),
      tabPanel("Evaluate ENMs", value = 4,
               tableOutput('evalTbl'))
    )
  )
))
