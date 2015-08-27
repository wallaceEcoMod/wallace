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
                                     conditionalPanel("input.tabs == 0",
                                                      includeMarkdown("www/tab0content.Rmd")
                                     ),
                                     conditionalPanel("input.tabs == 1",
                                                      h4("Obtain Occurrence Data"),
                                                      radioButtons("dbSelect", "Modules Available:", 
                                                                   choices = list("GBIF", "eBird (not functional)", "user input" = 'user'),
                                                                   selected = ''),
                                                      HTML('<hr>'),
                                                      conditionalPanel("input.dbSelect == 'GBIF'",
                                                                       div('Module: GBIF', id="mod"),
                                                                       span('via', id="pkgDes"),
                                                                       span('rgbif', id="rpkg"),
                                                                       span('package: Interface to the Global Biodiversity Information Facility API', id="pkgDes"),
                                                                       br(), br(),
                                                                       textInput("gbifName", label = "Enter scientific name of species (format: genus species)", value = 'tremarctos ornatus'),
                                                                       actionButton("goName", "Search GBIF"),
                                                                       br(), br(),
                                                                       sliderInput("occurrences", "Maximum number of occurrences:", min = 1, max = 500, value = 20),
                                                                       br(),
                                                                       downloadButton('downloadGBIFcsv', "Download Occurrence CSV")
                                                      ),
                                                      conditionalPanel("input.dbSelect == 'user'",
                                                                       fileInput("userCSV", label = "Upload Occurrence CSV")),
                                                      includeMarkdown("www/tab1content.Rmd"),
                                                      conditionalPanel("input.dbSelect == 'GBIF'",
                                                                       span("rgbif", id = "rpkg"), "references", br(),
                                                                       div('Developers: Scott Chamberlain, Karthik Ram, Vijay Barve, Dan Mcglinn', id="pkgDes"),
                                                                       a("CRAN", href = "http://cran.r-project.org/web/packages/rgbif/index.html", target = "_blank"),
                                                                       " | ",
                                                                       a("documentation", href = "http://www.gbif.org/resource/81747", target = "_blank")
                                                      )
                                     ),
                                     conditionalPanel("input.tabs == 2",
                                                      h4("Process Occurrence Data"),
                                                      radioButtons("procOccSelect", "Modules Available:", 
                                                                   choices = list("spatial thin", "environmental thin (not functional)"),
                                                                   selected = ''),
                                                      HTML('<hr>'),
                                                      conditionalPanel("input.procOccSelect == 'spatial thin'",
                                                                       div('Module: Spatial Thin', id="mod"),
                                                                       span('via', id="pkgDes"),
                                                                       span('spThin', id="rpkg"),
                                                                       span('package: Spatial Thinning of Species Occurrence Records', id="pkgDes"),
                                                                       br(), br(),
                                                                       numericInput("thinDist", label = "Thinning distance (km)", value = 0),
                                                                       actionButton("goThin", "Run spThin"),
                                                                       br(), br(),
                                                                       downloadButton('downloadThincsv', "Download Occurrence CSV")
                                                      ),
                                                      includeMarkdown("www/tab2content.Rmd"),
                                                      conditionalPanel("input.procOccSelect == 'spatial thin'",
                                                                       span("spThin", id = "rpkg"), "references", br(),
                                                                       div('Developers:  Matthew E. Aiello-Lammens, Rob A. Boria, Alex Radosavljevic, Bruno Vilela, 
                                                                           Robert P. Anderson', id="pkgDes"),
                                                                       a("CRAN", href = "http://cran.r-project.org/web/packages/spThin/index.html", target = "_blank"),
                                                                       " | ",
                                                                       a("software note", href="http://onlinelibrary.wiley.com/doi/10.1111/ecog.01132/abstract", target = "_blank")
                                                      )
                                     ),
                                     conditionalPanel("input.tabs == 3",
                                                      h4("Obtain Environmental Data"),
                                                      radioButtons("envSelect", "Modules Available:", 
                                                                   choices = list("WorldClim", "Climond (not functional)", "PRISM (not functional)", 
                                                                                  "user-specified (not functional)"),
                                                                   selected = ''),
                                                      HTML('<hr>'),
                                                      conditionalPanel("input.envSelect == 'WorldClim'",
                                                                       div('Module: Worldclim', id="mod"),
                                                                       span('via', id="pkgDes"),
                                                                       span('dismo', id="rpkg"),
                                                                       span('package: Species Distribution Modeling', id="pkgDes"),
                                                                       br(), br(),
                                                                       selectInput("pred", label = "Choose environmental data resolution",
                                                                                   choices = list("Choose resolution" = "",  
                                                                                                  "2.5 arcmin WorldClim bio1-19" = 2.5, 
                                                                                                  "5 arcmin WorldClim bio1-19" = 5, 
                                                                                                  "10 arcmin WorldClim bio1-19" = 10,
                                                                                                  "User input (not functional)" = 'user'))
                                                                                                  #"Upload from Dropbox" = 'db'))
                                                      ),
                                                      includeMarkdown("www/tab3content.Rmd"),
                                                      conditionalPanel("input.envSelect == 'WorldClim'",
                                                                       span("dismo", id = "rpkg"), "references", br(),
                                                                       div('Developers:  Robert J. Hijmans, Steven Phillips, John Leathwick, Jane Elith', id="pkgDes"),
                                                                       a("CRAN", href = "http://cran.r-project.org/web/packages/dismo/index.html", target = "_blank"),
                                                                       " | ",
                                                                       a("documentation", href="https://cran.r-project.org/web/packages/dismo/dismo.pdf", target = "_blank"),
                                                                       " | ", 
                                                                       a("Worldclim", href="http://worldclim.org", target="_blank")
                                                      )
#                                                       conditionalPanel("input.pred == 'db'",
#                                                                        textInput("dbAscFname", "File name"),
#                                                                        textInput("dbAscKey", "Dropbox Key"),
#                                                                        textInput("dbAscCRS", "Coordinate System",value='+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'),
#                                                                        textInput("dbAscDims", "Dimensions"))
                                     ),
                                     conditionalPanel("input.tabs == 4",
                                                      h4("Process Environmental Data"),
                                                      radioButtons("backgSelect", "Modules Available:", 
                                                                   choices = list("Bounding box" = 'bb', "Minimum convex polygon" = 'mcp',
                                                                                  "User-specified shapefile" = 'user'), 
                                                                   selected = ''),
                                                      HTML('<hr>'),
                                                      conditionalPanel("input.backgSelect == 'bb'",
                                                                       div('Module: Bounding Box', id="mod")),
                                                      conditionalPanel("input.backgSelect == 'mcp'",
                                                                       div('Module: Minimum Convex Polygon', id="mod")),
                                                      conditionalPanel("input.backgSelect == 'bb' || input.backgSelect == 'mcp'",
                                                                       span('via', id="pkgDes"),
                                                                       span('sp', id="rpkg"),
                                                                       span('and', id="pkgDes"),
                                                                       span('rgeos', id="rpkg"),
                                                                       span('packages: Title Classes and Methods for Spatial Data | 
                                                                            Interface to Geometry Engine - Open Source (GEOS)', id="pkgDes"),
                                                                       br(), br()
                                                      ),
                                                      conditionalPanel("input.backgSelect == 'user'",
                                                                       #  shinyFilesButton('userBackg', label='Upload Shapefile', title='Please select a file', multiple=TRUE)),
                                                                       fileInput("userBackg", label = "Upload Shapefile",
                                                                                 accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj", ".csv"), multiple=TRUE)),
                                                      conditionalPanel("input.backgSelect != null",
                                                        numericInput("backgBuf", label = "Study region buffer distance (degree)", value = 0)
                                                      ),
                                                      includeMarkdown("www/tab4content.Rmd"),
                                                      conditionalPanel("input.backgSelect == 'bb' || input.backgSelect == 'mcp'",
                                                                       span("sp", id = "rpkg"), "references", br(),
                                                                       div('Developers:  Edzer Pebesma, Roger Bivand, Barry Rowlingson, Virgilio Gomez-Rubio, 
                                                                           Robert Hijmans, Michael Sumner, Don MacQueen, Jim Lemon, Josh O\'Brien', id="pkgDes"), br(),
                                                                       a("CRAN", href = "http://cran.r-project.org/web/packages/sp/index.html", target = "_blank"),
                                                                       " | ",
                                                                       a("documentation", href="https://cran.r-project.org/web/packages/sp/sp.pdf", target = "_blank"),
                                                                       span("rgeos", id = "rpkg"), "references", br(),
                                                                       div('rgeos Developers:  Roger Bivand, Colin Rundel, Edzer Pebesma, Karl Ove Hufthammer', id="pkgDes"),
                                                                       a("CRAN", href = "http://cran.r-project.org/web/packages/rgeos/index.html", target = "_blank"),
                                                                       " | ",
                                                                       a("documentation", href="https://cran.r-project.org/web/packages/rgeos/rgeos.pdf", target = "_blank")
                                                      )

                                     ),
                                     conditionalPanel("input.tabs == 5",
                                                      h4("Partition Occurrence Data"),
                                                      radioButtons("partSelect", "Modules Available:", 
                                                                   choices = list("jackknife" = "jack", 
                                                                                  "block" = "block",
                                                                                  "checkerboard1 (not functional)" = "cb1", 
                                                                                  "checkerboard2 (not functional)" = "cb2",
                                                                                  "randomkfold (not functional)" = "random",
                                                                                  "user-specified" = "user"),
                                                                   selected = ''),
                                                      HTML('<hr>'),
                                                      conditionalPanel("input.partSelect == 'jack'",
                                                                       div('Module: Jackknife Partition', id="mod")),
                                                      conditionalPanel("input.partSelect == 'block'",
                                                                       div('Module: Block Partition', id="mod")),
                                                      conditionalPanel("input.partSelect == 'user'", 
                                                                       div('Module: User Partition', id="mod")),
                                                      conditionalPanel("input.partSelect != null",
                                                                       span('via', id="pkgDes"),
                                                                       span('ENMeval', id="rpkg"),
                                                                       span('package: Automated Runs and Evaluations of Ecological Niche Models', id="pkgDes")),
                                                      conditionalPanel("input.partSelect == 'cb1' | input.partSelect == 'cb2'",
                                                                       numericInput("aggFact", label = "Aggregation Factor", value = 1)),
                                                      conditionalPanel("input.partSelect == 'random'",
                                                                       numericInput("kfolds", label = "Number of Folds", value = 2)),
                                                      conditionalPanel("input.partSelect == 'user'",
                                                                       br(), br(),
                                                                       uiOutput('occgrpSel'),
                                                                       uiOutput('bggrpSel')),
                                                      conditionalPanel("input.partSelect != null",
                                                                       #downloadButton('downloadThincsv', "Download Occurrence CSV")),
                                                                       actionButton("goPart", "Partition")),
                                                      includeMarkdown("www/tab5content.Rmd"),
                                                      conditionalPanel("input.partSelect != null",
                                                                       span("ENMeval", id = "rpkg"), "references", br(),
                                                                       div('Developers:  Robert Muscarella, Peter J. Galante, Mariano Soley-Guardia, Robert A. Boria, 
                                                                           Jamie M. Kass, Maria Uriarte, Robert P. Anderson', id="pkgDes"),
                                                                       a("CRAN", href = "http://cran.r-project.org/web/packages/ENMeval/index.html", target = "_blank"),
                                                                       " | ",
                                                                       a("software note", href="http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12261/abstract", target = "_blank")
                                                      )
                                     ),
                                     conditionalPanel("input.tabs == 6",
                                                      h4("Build Niche Model"),
                                                      radioButtons("modSelect", "Modules Available:", 
                                                                   choices = list("Maxent", "Bioclim", "Boosted Regression Trees (not functional)", 
                                                                                  "Random Forest (not functional)", "GAM (not functional)"),
                                                                   selected = ''),
                                                      HTML('<hr>'),
                                                      conditionalPanel("input.modSelect == 'Maxent'",
                                                                       div('Module: Maxent', id="mod")),
                                                      conditionalPanel("input.modSelect == 'Bioclim'",
                                                                       div('Module: Bioclim', id="mod")),
                                                      conditionalPanel("input.modSelect != null",
                                                                       span('via', id="pkgDes"),
                                                                       span('ENMeval', id="rpkg"),
                                                                       span('package: Automated Runs and Evaluations of Ecological Niche Models', id="pkgDes")),
                                                      conditionalPanel("input.modSelect == 'Maxent'",
                                                                       checkboxGroupInput("fcs", label = "Select feature classes (flexibility of modeled response)", 
                                                                                          choices = list("L" = "L", "LQ" = "LQ", "H" = "H",
                                                                                                         "LQH" = "LQH", "LQHP" = "LQHP", "LQHPT" = "LQHPT")),
                                                                       sliderInput("rms", label = "Select regularization multipliers (penalty against complexity)",
                                                                                   min = 0, max = 10, value = c(1, 5)),
                                                                       numericInput("rmsBy", label = "RM step value", value = 1)            
                                                      ),
                                                      conditionalPanel("input.modSelect == 'Bioclim'",
                                                                       "BIOCLIM STUFF"),
                                                      conditionalPanel("input.modSelect != null",
                                                                       actionButton("goEval", "Build & Evaluate Models"), br(), br(),
                                                                       downloadButton('downloadEvalcsv', "Download Results CSV"), br(), br()),
                                                      includeMarkdown("www/tab6content.Rmd"),
                                                      conditionalPanel("input.modSelect != null",
                                                                       span("ENMeval", id = "rpkg"), "references", br(),
                                                                       div('Developers:  Robert Muscarella, Peter J. Galante, Mariano Soley-Guardia, Robert A. Boria, 
                                                                           Jamie M. Kass, Maria Uriarte, Robert P. Anderson', id="pkgDes"),
                                                                       a("CRAN", href = "http://cran.r-project.org/web/packages/ENMeval/index.html", target = "_blank"),
                                                                       " | ",
                                                                       a("software note", href="http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12261/abstract", target = "_blank")
                                                      )
                                     ),
                                     conditionalPanel("input.tabs == 7",
                                                      h4("Visualize Model Prediction"),
                                                      radioButtons("visSelect", "Modules Available:", 
                                                                   choices = list("Map Prediction" = 'map', "Response Curves (not functional)"),
                                                                   selected = ''),
                                                      HTML('<hr>'),
                                                      conditionalPanel("input.visSelect == 'map'",
                                                                       div('Module: Map Prediction', id="mod"),
                                                                       span('via', id="pkgDes"),
                                                                       span('raster', id="rpkg"),
                                                                       span('package: Geographic Data Analysis and Modeling', id="pkgDes"),
                                                                       br(), br(),
                                                                       uiOutput("predSel"),
                                                                       selectInput('predThresh', label = "Set threshold",
                                                                                   choices = list("No threshold" = 'raw', 
                                                                                                  "MTP" = 'mtp', "10%" = 'p10')),
                                                                       br(),
                                                                       actionButton("plotPred", "Plot Prediction"),
                                                                       br(), br(),
                                                                       selectInput('predFileType', label = "Select File Type",
                                                                                   choices = list("GRD" = 'raster', "ASCII" = 'ascii', "GeoTIFF" = 'GTiff')),
                                                                       downloadButton('downloadPred', "Download Current Prediction")
                                                      ),
                                                      includeMarkdown("www/tab7content.Rmd"),
                                                      conditionalPanel("input.visSelect == 'map'",
                                                                       span("raster", id = "rpkg"), "references", br(),
                                                                       div('Developers:  Robert J. Hijmans, Jacob van Etten, Joe Cheng, Matteo Mattiuzzi, 
                                                                           Michael Sumner, Jonathan A. Greenberg, Oscar Perpinan Lamigueiro, Andrew Bevan, 
                                                                           Etienne B. Racine, Ashton Shortridge', id="pkgDes"),
                                                                       a("CRAN", href = "http://cran.r-project.org/web/packages/raster/index.html", target = "_blank"),
                                                                       " | ",
                                                                       a("documentation", href="https://cran.r-project.org/web/packages/raster/raster.pdf", target = "_blank")
                                                      )
                                                      
                                     ),
                                     conditionalPanel("input.tabs == 8",
                                                      h4("About")
                                     )
                        ),
                        mainPanel(
                          tabsetPanel(id = "tabs",
                                      tabPanel("Introduction", value=0),
                                      tabPanel("1) Obtain Occ Data", value=1),
                                      tabPanel("2) Process Occ Data", value=2),
                                      tabPanel("3) Obtain Env Data", value=3),
                                      tabPanel("4) Process Env Data", value=4),
                                      tabPanel("5) Partition Occ Data", value=5),
                                      tabPanel("6) Build Niche Model", value=6),
                                      tabPanel("7) Visualize Prediction", value=7),
                                      tabPanel("About", value=8)
                          ),
                          fluidRow(
                            column(9,
                                   conditionalPanel("input.tabs != 0 && input.tabs != 8",
                                                    "LOG",
                                                    div(id = "wallaceLog", class = "scrollbox", htmlOutput("log")))
                            ),
                            column(3,
                                   conditionalPanel("input.tabs == 1", actionButton("erasePoly", "Erase Polygon"), br(), br(),
                                                    actionButton("selectPoly", "Select Pts With Poly"))                        
                            )
                          ),
                          br(),
                          conditionalPanel("input.tabs != 6 && input.tabs != 8", leafletOutput("map", height=600)),
                          br(),
                          conditionalPanel("input.tabs != 0 && input.tabs != 6 && input.tabs != 8", DT::dataTableOutput('occTbl')),
                          conditionalPanel("input.tabs == 6", uiOutput('evalTabs')),
                          conditionalPanel("input.tabs == 8", 
                                           fluidPage(titlePanel(h4("Wallace was created by an international team of ecologists:")),
                                                     fluidRow(
                                                       column(4, includeMarkdown("www/tab8Acontent.Rmd")),
                                                       column(4, includeMarkdown("www/tab8Bcontent.Rmd"))
                                                     )
                                           )
                          )
                        )
))
