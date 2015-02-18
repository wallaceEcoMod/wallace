library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("ENM Interactive Evaluation with GBIF"),
  helpText("Calibrate and evaluate ENMs with GBIF data."),
  
  sidebarPanel(
    conditionalPanel("input.conditionedPanels == 1",
                     "The first step is to download occurrence data from GBIF. After we
                     acquire these points, it helps to examine them on a map plot. Future
                     versions will allow the user to upload their own occurrence points as
                     an alternate option.",
                     hr(),
                     textInput("gbifName", label = "Enter scientific name of species", value = ''),
                     actionButton("goName", "Submit name"),
                     br(),
                     br(),
                    sliderInput("occurrences", "Maximun number of occurrences:", min = 1, max = 500, value = 50)
                     #actionButton("goMap", "Map points")
    ),
    conditionalPanel("input.conditionedPanels == 2",
                     span(strong("spThin"), style = "color:purple; font-size:18pt"), br(),
                     span(em("Spatial Thinning of Species Occurrence Records"), style = "font-size:10pt"), br(),
                     a("CRAN homepage", href = "http://cran.r-project.org/web/packages/spThin/index.html"),
                     hr(),
                     "The spatial thinning step aims to reduce spatial autocorrelation for the
                     environmental predictors, and is optional.",
                     hr(),
                     numericInput("thinDist", label = "Thinning distance (km)", value = 0),
                     actionButton("goThin", "Run spThin")
    ),
    conditionalPanel("input.conditionedPanels == 3",
                     "The user then chooses which environmental variables to use as
                     predictors. This data is in raster form and can be any resolution.
                     For this demonstration, only the Worldclim bioclimatic rasters are 
                     available at 4 resolutions, but future versions will allow users 
                     to upload their own rasters.", br(),
                     a("Worldclim homepage", href = "http://worldclim.org"),
                     hr(),
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
                     span(strong("ENMeval"), style = "color:purple; font-size:18pt"), br(),
                     span(em("Automated Runs and Evaluations of Ecological Niche Models"), style = "font-size:10pt"), br(),
                     a("CRAN homepage", href = "http://cran.r-project.org/web/packages/ENMeval/index.html"),
                     hr(),
                     "The evaluation step runs models with different limitations on complexity
                     and outputs how well each performed on test data. This step aims to help the
                     user decide on an optimal combination of complexity parameters.",
                     hr(),
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
      tabPanel("1) Download GBIF Occurrence Points / Plot", value = 1,
               br(),
               conditionalPanel("input.goName", textOutput('GBIFtxt')),
               br(),
               plotOutput('GBIFmap1'),
               tableOutput('gbifOccTbl')
               ),
      tabPanel("2) Thin occurrences", value = 2,
               br(),
               conditionalPanel("input.goThin", textOutput('thinText')),
               plotOutput('GBIFmap2'),
               tableOutput('thinOccTbl')
               ),
      tabPanel("3) Choose Prediction Rasters", value = 3,
               conditionalPanel("input.pred != ''", textOutput('predExtTxt')),
               plotOutput('GBIFmap3')
               ),
      tabPanel("4) Evaluate ENMs", value = 4,
               conditionalPanel("input.goEval", textOutput('evalTxt')),
               tableOutput('evalTbl'))
    )
  )
))
