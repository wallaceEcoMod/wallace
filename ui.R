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

source("ui_content.R")

# function to make two small input boxes next to each other
textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

#"Harnessing Digital Biodiversity Data via a GUI interface fueled by R"
title <- HTML(paste0(span("WALLACE beta v0.1: ", style = "font-size:16pt"), 
                     span("Harnessing Digital Biodiversity Data for Predictive Modeling, fueled by R", 
                          style = "font-size:10pt"), "  |  ", 
                     span("Developers: Jamie M. Kass, Matthew Aiello-Lammens, Bruno Vilela, 
              Robert Muscarella, Robert P. Anderson", style = "font-size:7pt")))
# Define UI for application that draws a histogram
shinyUI(fluidPage(title,
                  tags$head(
                    tags$style(HTML(
                      "#text { overflow: auto; }
                       #header {
                        position: relative;
                        min-height: 75px;
#                         background: #FFFFFF;
                        z-index:10;
                        background: inherit;
                       }
                      #header-content {
                        position: absolute;
                        z-index: 5;
                        bottom: 0;
                        left: 0;
                      }
                      #textContainer {
                        height:100px; 
                        overflow:auto; 
                        border:1px solid green; 
                        padding:5px
                      }
                       }"
                    ))),
                  fluidRow(
                    column(4,
                           tabsetPanel(id='tabs',
                             tabPanel("1) Get Data",
                                      tab1content,
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
                             tabPanel("2) Process",
                                      tab2content,
                                      numericInput("thinDist", label = "Thinning distance (km)", value = 0),
                                      actionButton("goThin", "Run spThin"),
                                      br(), br(),
                                      downloadButton('downloadThincsv', "Download Thinned Occurrence CSV")
                             ),
                             tabPanel("3) Variables",
                                      tab3contentA,
                                      radioButtons("unusedRasterselect", "Select Environmental Data Source", 
                                                   choices = list("WorldClim", "Climond (not functional)", "PRISM (not functional)", 
                                                                  "user-specified (not functional)"),
                                                   selected = "WorldClim"),
                                      br(),
                                      selectInput("pred", label = "Choose environmental data resolution",
                                                  choices = list("Choose resolution" = "",  
                                                                 "2.5 arcmin WorldClim bio1-19" = 2.5, 
                                                                 "5 arcmin WorldClim bio1-19" = 5, 
                                                                 "10 arcmin WorldClim bio1-19" = 10)),
                                      conditionalPanel("input.pred == 'user'",
                                                       textInput("userPred", "Enter path to directory")),
                                      selectInput("backg", label = "Study region selection",
                                                  choices = list("Choose option" = '', "Bounding box" = 'bb', 
                                                                 "Minimum convex polygon" = 'mcp',
                                                                 "User-specified shapefile" = 'user')),
                                      conditionalPanel("input.backg == 'user'",
#                                                        shinyFilesButton('userBackg', label='Upload Shapefile', title='Please select a file', multiple=TRUE)),
                                                       fileInput("userBackg", label = "Upload Shapefile",
                                                                 accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj", ".csv"), multiple=TRUE)),
                                      numericInput("backgBuf", label = "Study region buffer distance (degree)", value = 0),
                                      tab3contentB
                             ),
                             tabPanel("4) Model", value=4,
                                        tab4content,
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
                                                                   "checkerboard1" = "cb1", 
                                                                   "checkerboard2" = "cb2",
                                                                   "randomkfold (not functional)" = "random",
                                                                   "user-specified (not functional)" = "user")),
                                        conditionalPanel("input.method == 'user'", 
                                                         uiOutput('occgrpSel'),
                                                         uiOutput('bggrpSel')),
                                        actionButton("goEval", "Run ENMeval"),
                                        br(),
                                        br(),
                                        downloadButton('downloadEvalcsv', "Download ENMeval Results CSV")
                             ),
                             tabPanel("5) Predict",
                                      tab5content,
                                      uiOutput("predSel"),
                                      actionButton("plotRas", "Plot Raster"),
                                      checkboxInput("plotpoints", label = "Plot occurrence points", value = FALSE),
                                      br(),
                                      downloadButton('downloadPred', "Download Current Prediction Raster")
                             ),
                             tabPanel("ABOUT",
                                      fluidPage(titlePanel(h4("Wallace was created by an international team of ecologists:")),
                                                fluidRow(
                                                  column(4, tab6contentA),
                                                  column(4, tab6contentB)
                                                )
                                      )
                             )
                           )
                    ),
                    column(8,
                           div(id = "textContainer", htmlOutput("log")),
                           br(),
                           actionButton("erasePoly", "Erase Polygon"),
                           actionButton("selectPoly", "Select Pts With Poly"),
                           br(), br(),
                           conditionalPanel("input.tabs != 4", leafletOutput("map", height=600)),
                           br(),
                           conditionalPanel("input.tabs != 4", DT::dataTableOutput('occTbl')),
                           conditionalPanel("input.tabs != 4", DT::dataTableOutput('evalTbl')),
                           br(),
                           plotOutput('evalPlot', width = 600)
                    )
                  )        
)
)
