if (!require("shiny"))
  install.packages("shiny")
if (!require("devtools"))
  install.packages("devtools")
if (!require('shinyapps')) devtools::install_github("rstudio/shinyapps")
if (!require('leaflet')) devtools::install_github("rstudio/leaflet")
if (!require("DT")) devtools::install_github("rstudio/DT")

library(shiny)
library(shinyapps)
library(leaflet)
library(DT)

# function to make two small input boxes next to each other
# textInputRow<-function (inputId, label, value = "") 
# {
#   div(style="display:inline-block",
#       tags$label(label, `for` = inputId), 
#       tags$input(id = inputId, type = "text", value = value,class="input-small"))
# }

title <- HTML('<span style="font-size:25pt">WALLACE beta v0.1:</span>
<span style="font-size:15pt">Harnessing Digital Biodiversity Data for Predictive Modeling, fueled by R</span><br>
<span style="font-size:10pt">Developers: Jamie M. Kass, Matthew Aiello-Lammens, Bruno Vilela, Robert Muscarella, Robert P. Anderson</span><br><br>')

# Define UI for application
shinyUI(pageWithSidebar(title,
                  sidebarPanel(includeCSS("styles.css"),
                               includeScript("scroll.js"),
                    conditionalPanel("input.tabs == 1",
                                     includeMarkdown("www/tab1content.Rmd"),
                                     radioButtons("unusedDBselect", "Select Occurrence Data Source", 
                                                  choices = list("GBIF via rgbif", "eBird via rebird (not functional)", 
                                                                 "user input (not functional)"),
                                                  selected = "GBIF via rgbif"),
                                     br(), 
                                     textInput("gbifName", label = "Enter scientific name of species (format: genus species)", value = 'tremarctos ornatus'),
                                     actionButton("goName", "Search GBIF"),
                                     br(), br(),
                                     sliderInput("occurrences", "Maximum number of occurrences:", min = 1, max = 500, value = 20),
                                     br(),
                                     fileInput("userCSV", label = "Upload Occurrence CSV"),
                                     downloadButton('downloadGBIFcsv', "Download Occurrence CSV")
                    ),
                    conditionalPanel("input.tabs == 2",
                                     includeMarkdown("www/tab2content.Rmd"),
                                     numericInput("thinDist", label = "Thinning distance (km)", value = 0),
                                     actionButton("goThin", "Run spThin"),
                                     br(), br(),
                                     downloadButton('downloadThincsv', "Download Thinned Occurrence CSV")
                    ),
                    conditionalPanel("input.tabs == 3",
                                     includeMarkdown("www/tab3Acontent.Rmd"),
                                     radioButtons("unusedRasterselect", "Select Environmental Data Source", 
                                                  choices = list("WorldClim", "Climond (not functional)", "PRISM (not functional)", 
                                                                 "user-specified (not functional)"),
                                                  selected = "WorldClim"),
                                     br(),
                                     selectInput("pred", label = "Choose environmental data resolution",
                                                 choices = list("Choose resolution" = "",  
                                                                "2.5 arcmin WorldClim bio1-19" = 2.5, 
                                                                "5 arcmin WorldClim bio1-19" = 5, 
                                                                "10 arcmin WorldClim bio1-19" = 10,
                                                                "Upload from Dropbox" = 'db')),
                                     conditionalPanel("input.pred == 'db'",
                                                      textInput("dbAscFname", "File name"),
                                                      textInput("dbAscKey", "Dropbox Key"),
                                                      textInput("dbAscCRS", "Coordinate System",value='+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'),
                                                      textInput("dbAscDims", "Dimensions")),
                                     selectInput("backg", label = "Study region selection",
                                                 choices = list("Choose option" = '', "Bounding box" = 'bb', 
                                                                "Minimum convex polygon" = 'mcp',
                                                                "User-specified shapefile" = 'user')),
                                     conditionalPanel("input.backg == 'user'",
                                                      #  shinyFilesButton('userBackg', label='Upload Shapefile', title='Please select a file', multiple=TRUE)),
                                                      fileInput("userBackg", label = "Upload Shapefile",
                                                                accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj", ".csv"), multiple=TRUE)),
                                     numericInput("backgBuf", label = "Study region buffer distance (degree)", value = 0),
                                     includeMarkdown("www/tab3Bcontent.Rmd")
                    ),
                    conditionalPanel("input.tabs == 4",
                                     includeMarkdown("www/tab4content.Rmd"),
                                     radioButtons("unusedAlgselect", "Select Algorithm", 
                                                  choices = list("Maxent", "Boosted Regression Trees (not functional)", 
                                                                 "Random Forest (not functional)", "GAM (not functional)"),
                                                  selected = "Maxent"),
                                     br(),
                                     checkboxGroupInput("fcs", label = "Select feature classes (flexibility of modeled response)", 
                                                        choices = list("L" = "L", "LQ" = "LQ", "H" = "H",
                                                                       "LQH" = "LQH", "LQHP" = "LQHP", "LQHPT" = "LQHPT")
                                     ),
                                     sliderInput("rms", label = "Select regularization multipliers (penalty against complexity)",
                                                 min = 0, max = 10, value = c(1, 5)),
                                     numericInput("rmsBy", label = "RM step value", value = 1),
                                     selectInput("method", label = "Occurrence record partitioning method",
                                                 choices = list("Choose method" = "",
                                                                "jackknife" = "jack", 
                                                                "block" = "block",
                                                                "checkerboard1 (not functional)" = "cb1", 
                                                                "checkerboard2 (not functional)" = "cb2",
                                                                "randomkfold (not functional)" = "random",
                                                                "user-specified" = "user")),
                                     conditionalPanel("input.method == 'user'", 
                                                      uiOutput('occgrpSel'),
                                                      uiOutput('bggrpSel')),
                                     actionButton("goEval", "Run ENMeval"),
                                     br(),
                                     br(),
                                     downloadButton('downloadEvalcsv', "Download ENMeval Results CSV")
                    ),
                    conditionalPanel("input.tabs == 5",
                                     includeMarkdown("www/tab5content.Rmd"),
                                     uiOutput("predSel"),
                                     selectInput('predThresh', label = "Set threshold",
                                                 choices = list("No threshold" = 'no', 
                                                                "MTP" = 'mtp', "10%" = 'p10')),
                                     br(),
                                     actionButton("plotPred", "Plot Prediction"),
                                     br(), br(),
                                     downloadButton('downloadPred', "Download Current Prediction")
                    )
                  ),
                  mainPanel(
                    tabsetPanel(id = "tabs",
                                tabPanel("1) Get Data", value=1),
                                tabPanel("2) Process", value=2),
                                tabPanel("3) Variables", value=3),
                                tabPanel("4) Model", value=4),
                                tabPanel("5) Predict", value=5),
                                tabPanel("6) ABOUT", value=6)
                    ),
                    "LOG",
                    div(id = "wallaceLog", class = "scrollbox", htmlOutput("log")),
                    br(),
                    conditionalPanel("input.tabs == 1", actionButton("erasePoly", "Erase Polygon"), actionButton("selectPoly", "Select Pts With Poly")),
                    br(),
                    conditionalPanel("input.tabs != 4 && input.tabs != 6", leafletOutput("map", height=600)),
                    br(),
                    conditionalPanel("input.tabs != 4 && input.tabs != 6", DT::dataTableOutput('occTbl')),
                    conditionalPanel("input.tabs == 4", uiOutput('evalTabs')),
                    conditionalPanel("input.tabs == 6", 
                                     fluidPage(titlePanel(h4("Wallace was created by an international team of ecologists:")),
                                               fluidRow(
                                                 column(4, includeMarkdown("www/tab6Acontent.Rmd")),
                                                 column(4, includeMarkdown("www/tab6Bcontent.Rmd"))
                                               )
                                     )
                    )
                  )
))
