if (!require("shiny")) install.packages("shiny")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("devtools")) install.packages("devtools")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require('shinyapps')) devtools::install_github("rstudio/shinyapps")
if (!require('leaflet')) devtools::install_github("rstudio/leaflet")
if (!require("DT")) devtools::install_github("rstudio/DT")
if (!require("shinyBS")) install.packages("shinyBS")

library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyapps)
library(shinyjs)
library(leaflet)
library(DT)
source("functions.R")

useShinyjs()

# for colored action buttons
# actionButton <- function(inputId, label, btn.style = "" , css.class = "") {
#   if ( btn.style %in% c("primary","info","success","warning","danger","inverse","link")) {
#     btn.css.class <- paste("btn",btn.style,sep="-")
#   } else btn.css.class = ""
#   
#   tags$button(id=inputId, type="button", class=paste("btn action-button",btn.css.class,css.class,collapse=" "), label)
# }

# function to make two small input boxes next to each other
# textInputRow<-function (inputId, label, value = "")
# {
#   div(style="display:inline-block",
#       tags$label(label, `for` = inputId),
#       tags$input(id = inputId, type = "text", value = value,class="input-small"))
# }

# title <- HTML('&nbsp;&nbsp;<span style="font-size:25pt">WALLACE beta v0.2:</span>
#               &nbsp;<span style="font-size:15pt">An R-based Modular Web App to Harness Biodiversity Data for Spatial Modeling</span><br>
#               &nbsp;&nbsp;<span style="font-size:10pt">Developers: Jamie M. Kass, Matthew Aiello-Lammens, Bruno Vilela, Robert Muscarella, Robert P. Anderson</span><br><br>'
#               )



# Define UI for application
shinyUI(navbarPage(theme=shinytheme('united'), id='tabs', collapsible=TRUE,
                   title='Wallace',
                   tabPanel("Intro", value=0),
                   tabPanel("1 Get Occs", value=1),
                   tabPanel("2 Clean Occs", value=2),
                   tabPanel("3 Get Env", value=3),
                   tabPanel("4 Study Extent", value=4),
                   tabPanel("5 Partition Occs", value=5),
                   tabPanel("6 Model", value=6),
                   tabPanel("7 Visualize", value=7),
                   tabPanel("8 Project", value=8),
                   tabPanel("History", value='rmd'),
                   tabPanel("About", value='about'),
                   
                   fluidRow(column(4,
                                   wellPanel(
                                     includeCSS("styles.css"),
                                     includeScript("scroll.js"),
                                     conditionalPanel("input.tabs == 0", h4("Introduction"),
                                                      includeMarkdown("www/intro_tab.Rmd")
                                     ),
                                     conditionalPanel("input.tabs == 1",
                                                      h4("Obtain Occurrence Data"),
                                                      radioButtons("dbSelect", "Modules Available:",
                                                                   choices = list("GBIF", "User-specified" = 'user'),
                                                                   selected = 'GBIF'),
                                                      HTML('<hr>'),
                                                      conditionalPanel("input.dbSelect == 'GBIF'",
                                                                       div('Module: GBIF', id="mod"),
                                                                       span('via', id="pkgDes"),
                                                                       span('rgbif', id="rpkg"),
                                                                       span('package: Interface to the Global Biodiversity Information Facility API', id="pkgDes"),
                                                                       br(),
#                                                                        conditionalPanel("input.togMD1",
#                                                                                         includeMarkdown("www/tab1_gbif.Rmd")
                                                      HTML('<hr>'),
                                                      textInput("gbifName", label = "Enter scientific name of species (format: genus species)", value = 'heteromys anomalus'),
                                                      actionButton("goName", "Search GBIF"),
                                                      br(), br(),
                                                      sliderInput("gbifNum", "Maximum number of occurrences:", min = 1, max = 500, value = 12),
                                                      #br(),
                                                      #uiOutput('gbifDnld'),
                                                      downloadButton('downloadOrigOccs', "Download Occurrence CSV"),
                                                      HTML('<hr>')
                                     ),
                                     conditionalPanel("input.dbSelect == 'user'",
                                                      includeMarkdown("www/tab1_user.Rmd"),
                                                      HTML('<hr>'),
                                                      fileInput("userCSV", label = "Upload Occurrence CSV"),
                                                      HTML('<hr>')),
                                     
                                     "The output of this step is a CSV file with rows of localities, and columns containing species,
                                                             longitude, and latitude (as well as any other fields provided by GBIF or the user.",
                                     conditionalPanel("input.dbSelect == 'GBIF'",
                                                      HTML('<hr>'),
                                                      span("rgbif", id = "rpkg"), "references", br(),
                                                      div('Developers: Scott Chamberlain, Karthik Ram, Vijay Barve, Dan Mcglinn', id="pkgDes"),
                                                      a("CRAN", href = "http://cran.r-project.org/web/packages/rgbif/index.html", target = "_blank"),
                                                      " | ",
                                                      a("documentation", href = "https://cran.r-project.org/web/packages/rgbif/rgbif.pdf", target = "_blank")
                                     )
                                   ),
                                   conditionalPanel("input.tabs == 2",
                                                    h4("Process Occurrence Data"),
                                                    radioButtons("procOccSel", "Modules Available:",
                                                                 choices = list("Select Localities" = 'selpts',
                                                                                "Spatial Thin" = 'spthin')),
                                                    HTML('<hr>'),
                                                    conditionalPanel("input.procOccSel == 'selpts'",
                                                                     div('Module: Select By Polygon', id="mod"),
                                                                     checkboxInput('togMD2A', "Hide / Display Guidance Text", value = FALSE),
                                                                     conditionalPanel("input.togMD2A",
                                                                                      includeMarkdown("www/tab2_selpts.Rmd")),
                                                                     HTML('<hr>'),
                                                                     numericInput("remLoc", label="Enter the record ID to be removed", value = 0),
                                                                     actionButton("remove", "Remove Locality"),
                                                                     br(), br(),
                                                                     actionButton("selectPoly", "Select With Polygon"),
                                                                     br(), br(),
                                                                     actionButton("erasePolySelLocs", "Reset Localities"),
                                                                     br(),
                                                                     HTML('<hr>')),
                                                    conditionalPanel("input.procOccSel == 'spthin'",
                                                                     div('Module: Spatial Thin', id="mod"),
                                                                     span('via', id="pkgDes"),
                                                                     span('spThin', id="rpkg"),
                                                                     span('package: Spatial Thinning of Species Occurrence Records', id="pkgDes"), br(),
                                                                     checkboxInput('togMD2B', "Hide / Display Guidance Text", value = FALSE),
                                                                     conditionalPanel("input.togMD2B", includeMarkdown("www/tab2_spthin.Rmd")),
                                                                     HTML('<hr>'),
                                                                     numericInput("thinDist", label = "Thinning distance (km)", value = 0),
                                                                     actionButton("goThin", "Thin Localities"),
                                                                     br(), br(),
                                                                     downloadButton('downloadThincsv', "Download Thinned Occurrence CSV"),
                                                                     #br(), 
                                                                     #uiOutput('thinDnld'),
                                                                     HTML('<hr>')
                                                    ),
                                                    includeMarkdown("www/tab2_input.Rmd"),
                                                    conditionalPanel("input.procOccSel == 'spthin'",
                                                                     HTML('<hr>'),
                                                                     span("spThin", id = "rpkg"), "references", br(),
                                                                     div('Developers:  Matthew E. Aiello-Lammens, Rob A. Boria, Alex Radosavljevic, Bruno Vilela,
                                                                                  Robert P. Anderson', id="pkgDes"),
                                                                     a("CRAN", href = "http://cran.r-project.org/web/packages/spThin/index.html", target = "_blank"),
                                                                     " | ",
                                                                     a("documentation", href="https://cran.r-project.org/web/packages/spThin/spThin.pdf", target = "_blank"),
                                                                     " | ",
                                                                     a("software note", href="http://onlinelibrary.wiley.com/doi/10.1111/ecog.01132/abstract", target = "_blank"),
                                                                     HTML('<hr>'),
                                                                     includeMarkdown("www/tab2_spthin_refs.Rmd")
                                                    )
                                   ),
                                   conditionalPanel("input.tabs == 3",
                                                    h4("Obtain Environmental Data"),
                                                    radioButtons("envSelect", "Modules Available:",
                                                                 choices = list("WorldClim")),
                                                    HTML('<hr>'),
                                                    conditionalPanel("input.envSelect == 'WorldClim'",
                                                                     div('Module: WorldClim', id="mod"),
                                                                     span('via', id="pkgDes"),
                                                                     span('dismo', id="rpkg"),
                                                                     span('package: Species Distribution Modeling', id="pkgDes"),
                                                                     br(),
                                                                     checkboxInput('togMD3', "Hide / Display Guidance Text", value = FALSE),
                                                                     conditionalPanel("input.togMD3",
                                                                                      includeMarkdown("www/tab3_wc.Rmd")),
                                                                     HTML('<hr>'),
                                                                     selectInput("bcRes", label = "Choose environmental data resolution",
                                                                                 choices = list("Choose resolution" = "",
                                                                                                "2.5 arcmin WorldClim bio1-19" = 2.5,
                                                                                                "5 arcmin WorldClim bio1-19" = 5,
                                                                                                "10 arcmin WorldClim bio1-19" = 10)),
                                                                     actionButton("predDnld", "Download Env Data"),
                                                                     HTML('<hr>')
                                                    ),
                                                    conditionalPanel("input.envSelect == 'Climond'",
                                                                     a("Climond website", href = "https://www.climond.org/", target = "_blank"), HTML('<hr>')),
                                                    conditionalPanel("input.envSelect == 'MARSPEC'",
                                                                     a("MARSPEC website", href = "http://www.marspec.org/Home.html", target = "_blank"), HTML('<hr>')),
                                                    conditionalPanel("input.envSelect == 'PRISM'",
                                                                     a("PRISM website", href = "http://www.prism.oregonstate.edu/", target = "_blank"), HTML('<hr>')),
                                                    conditionalPanel("input.envSelect == 'user'",
                                                                     includeMarkdown("www/tab3_user.Rmd"),
                                                                     HTML('<hr>')),
                                                    conditionalPanel("input.envSelect == 'WorldClim'",
                                                                     span("dismo", id = "rpkg"), "references", br(),
                                                                     div('Developers:  Robert J. Hijmans, Steven Phillips, John Leathwick, Jane Elith', id="pkgDes"),
                                                                     a("CRAN", href = "http://cran.r-project.org/web/packages/dismo/index.html", target = "_blank"),
                                                                     " | ",
                                                                     a("documentation", href="https://cran.r-project.org/web/packages/dismo/dismo.pdf", target = "_blank"),
                                                                     " | ",
                                                                     a("WorldClim", href="http://worldclim.org", target="_blank"),
                                                                     HTML('<hr>'),
                                                                     includeMarkdown("www/tab3_wc_refs.Rmd")
                                                    )
                                   ),
                                   conditionalPanel("input.tabs == 4",
                                                    h4("Process Environmental Data"),
                                                    radioButtons("envProcSelect", "Modules Available:",
                                                                 choices = list("Select Study Region" = "backg")),
                                                    
                                                    HTML('<hr>'),
                                                    conditionalPanel("input.envProcSelect == 'backg'",
                                                                     div('Module: Select Study Region', id="mod"),
                                                                     span('via', id="pkgDes"),
                                                                     span('sp', id="rpkg"),
                                                                     span('and', id="pkgDes"),
                                                                     span('rgeos', id="rpkg"),
                                                                     span('packages: Title Classes and Methods for Spatial Data |
                                                                                   Interface to Geometry Engine - Open Source (GEOS)', id="pkgDes"),
                                                                     br(),
                                                                     checkboxInput('togMD4', "Hide / Display Guidance Text", value = FALSE),
                                                                     conditionalPanel("input.togMD4",
                                                                                      includeMarkdown("www/tab4_backg.Rmd")
                                                                     ),
                                                                     HTML('<hr>'),
                                                                     radioButtons("backgSelect", "Background Extents:",
                                                                                  choices = list("Bounding box" = 'bb', "Minimum convex polygon" = 'mcp',
                                                                                                 "User-specified polygon" = 'user'))
                                                    ),
                                                    conditionalPanel("input.backgSelect == 'user'",
                                                                     #  shinyFilesButton('userBackg', label='Upload Shapefile', title='Please select a file', multiple=TRUE)),
                                                                     fileInput("userBackg", label = "Upload Polygon (.csv)",
                                                                               accept=c(".csv"), multiple=TRUE)),
                                                    conditionalPanel("input.envProcSelect == 'backg' && (input.backgSelect == 'bb' || input.backgSelect == 'mcp')",
                                                                     numericInput("backgBuf", label = "Study region buffer distance (degree)", value = 0, min = 0, step = 0.5)),
                                                    conditionalPanel("input.envProcSelect == 'backg'",
                                                                     actionButton("goBackgMask", "Clip Env Data by Polygon"), br(), br(),
                                                                     selectInput('mskPredsFileType', label = "Select File Type",
                                                                                 choices = list("GRD" = 'raster', "ASCII" = 'ascii', "GeoTIFF" = 'GTiff')),
                                                                     downloadButton('downloadMskPreds', "Download Clipped Env Data"),
                                                                     HTML('<hr>')),
                                                    "Input flows into this step from Step 3. Step 4 provides clipped grids that can be saved in the
                                                             same formats: GRD, GEOTIFF, or ASCII files. Additionally (for clipping the grids in this step),
                                                             the user may provide a file delimiting the study region via a CSV that contains the vertices of a polygon.",
                                                    conditionalPanel("input.envProcSelect == 'backg'",
                                                                     HTML('<hr>'),
                                                                     span("sp", id = "rpkg"), "references", br(),
                                                                     div('Developers:  Edzer Pebesma, Roger Bivand, Barry Rowlingson, Virgilio Gomez-Rubio,
                                                                                  Robert Hijmans, Michael Sumner, Don MacQueen, Jim Lemon, Josh O\'Brien', id="pkgDes"),
                                                                     a("CRAN", href = "http://cran.r-project.org/web/packages/sp/index.html", target = "_blank"),
                                                                     " | ",
                                                                     a("documentation", href="https://cran.r-project.org/web/packages/sp/sp.pdf", target = "_blank"),
                                                                     p(), span("rgeos", id = "rpkg"), "references", br(),
                                                                     div('rgeos Developers:  Roger Bivand, Colin Rundel, Edzer Pebesma, Karl Ove Hufthammer', id="pkgDes"),
                                                                     a("CRAN", href = "http://cran.r-project.org/web/packages/rgeos/index.html", target = "_blank"),
                                                                     " | ",
                                                                     a("documentation", href="https://cran.r-project.org/web/packages/rgeos/rgeos.pdf", target = "_blank"),
                                                                     HTML('<hr>'),
                                                                     includeMarkdown("www/tab4_backg_refs.Rmd")
                                                    )
                                                    
                                   ),
                                   conditionalPanel("input.tabs == 5",
                                                    h4("Partition Occurrence Data"),
                                                    radioButtons("partSelect", "Options Available:",
                                                                 choices = list("Non-spatial Partition" = 'nsp',
                                                                                "Spatial Partition" = 'sp')),
                                                    HTML('<hr>'),
                                                    conditionalPanel("input.partSelect == 'sp'",
                                                                     div('Module: Spatial Partition', id="mod")),
                                                    conditionalPanel("input.partSelect == 'nsp'",
                                                                     div('Module: Non-spatial Partition', id="mod")),
                                                    conditionalPanel("input.partSelect == 'sp' || input.partSelect == 'nsp'",
                                                                     span('via', id="pkgDes"),
                                                                     span('ENMeval', id="rpkg"),
                                                                     span('package: Automated Runs and Evaluations of Ecological Niche Models', id="pkgDes"),
                                                                     conditionalPanel("input.partSelect == 'sp'",
                                                                                      checkboxInput('togMD5A', "Hide / Display Guidance Text", value = FALSE),
                                                                                      conditionalPanel("input.togMD5A",
                                                                                                       includeMarkdown("www/tab5_sp.Rmd")),
                                                                                      HTML('<hr>')),
                                                                     conditionalPanel("input.partSelect == 'nsp'",
                                                                                      checkboxInput('togMD5B', "Hide / Display Guidance Text", value = FALSE),
                                                                                      conditionalPanel("input.togMD5B",
                                                                                                       includeMarkdown("www/tab5_nsp.Rmd")),
                                                                                      HTML('<hr>')),
                                                                     selectInput("partSelect2", "Modules Available:",
                                                                                 choices = list("None selected" = ''))
                                                    ),
                                                    conditionalPanel("input.partSelect == 'sp' & (input.partSelect2 == 'cb1' | input.partSelect2 == 'cb2')",
                                                                     numericInput("aggFact", label = "Aggregation Factor", value = 2, min = 2)),
                                                    conditionalPanel("input.partSelect2 == 'random'",
                                                                     numericInput("kfolds", label = "Number of Folds", value = 2, min = 2)),
                                                    # conditionalPanel("input.partSelect == 'user'",
                                                    #   br(), br(),
                                                    #   uiOutput('occgrpSel'),
                                                    #   uiOutput('bggrpSel')),
                                                    conditionalPanel("input.partSelect == 'sp' || input.partSelect == 'nsp'",
                                                                     actionButton("goPart", "Partition"), br(), br(),
                                                                     downloadButton('downloadPart', "Download Partitioned Data CSV"),
                                                                     HTML('<hr>')),
                                                    includeMarkdown("www/tab5content.Rmd"),
                                                    HTML('<hr>'),
                                                    conditionalPanel("input.partSelect == 'sp' || input.partSelect == 'nsp'",
                                                                     span("ENMeval", id = "rpkg"), "references", br(),
                                                                     div('Developers:  Robert Muscarella, Peter J. Galante, Mariano Soley-Guardia, Robert A. Boria,
                                                                                  Jamie M. Kass, Maria Uriarte, Robert P. Anderson', id="pkgDes"),
                                                                     a("CRAN", href = "http://cran.r-project.org/web/packages/ENMeval/index.html", target = "_blank"),
                                                                     " | ",
                                                                     a("documentation", href="https://cran.r-project.org/web/packages/ENMeval/ENMeval.pdf", target = "_blank"),
                                                                     " | ",
                                                                     a("software note", href="http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12261/abstract", target = "_blank"),
                                                                     HTML('<hr>'),
                                                                     includeMarkdown("www/tab5_both_refs.Rmd"))
                                   ),
                                   conditionalPanel("input.tabs == 6",
                                                    h4("Build and Evaluate Niche Model"),
                                                    radioButtons("modSelect", "Modules Available:",
                                                                 choices = list("BIOCLIM", "Maxent")),
                                                    HTML('<hr>'),
                                                    conditionalPanel("input.modSelect == 'Maxent'",
                                                                     div('Module: Maxent', id="mod")),
                                                    conditionalPanel("input.modSelect == 'BIOCLIM'",
                                                                     div('Module: BIOCLIM', id="mod")),
                                                    conditionalPanel("input.modSelect == 'BIOCLIM' || input.modSelect == 'Maxent'",
                                                                     span('via', id="pkgDes"),
                                                                     span('ENMeval', id="rpkg"),
                                                                     span('and', id="pkgDes"),
                                                                     span('dismo', id="rpkg"),
                                                                     span('packages: Automated Runs and Evaluations of Ecological Niche Models |
                                                                                   Species Distribution Modeling', id="pkgDes")),
                                                    conditionalPanel("input.modSelect == 'BIOCLIM'",
                                                                     checkboxInput('togMD6A', "Hide / Display Guidance Text", value = FALSE),
                                                                     conditionalPanel("input.togMD6A",
                                                                                      includeMarkdown("www/tab6_bc.Rmd")
                                                                     ),
                                                                     HTML('<hr>')
                                                    ),
                                                    conditionalPanel("input.modSelect == 'Maxent'",
                                                                     checkboxInput('togMD6B', "Hide / Display Guidance Text", value = FALSE),
                                                                     conditionalPanel("input.togMD6B",
                                                                                      includeMarkdown("www/tab6_maxent.Rmd")
                                                                     ),
                                                                     HTML('<hr>'),
                                                                     checkboxGroupInput("fcs", label = "Select feature classes (flexibility of modeled response)",
                                                                                        choices = list("L" = "L", "LQ" = "LQ", "H" = "H",
                                                                                                       "LQH" = "LQH", "LQHP" = "LQHP", "LQHPT" = "LQHPT")),
                                                                     sliderInput("rms", label = "Select regularization multipliers (penalty against complexity)",
                                                                                 min = 0, max = 10, value = c(1, 2)),
                                                                     numericInput("rmsBy", label = "RM step value", value = 1)
                                                    ),
                                                    conditionalPanel("input.modSelect == 'BIOCLIM' || input.modSelect == 'Maxent'",
                                                                     actionButton("goEval", "Build & Evaluate Models"), br(), br(),
                                                                     downloadButton('downloadEvalcsv', "Download Results CSV")
                                                    ),
                                                    conditionalPanel("input.modSelect == 'BIOCLIM' || input.modSelect == 'Maxent'",
                                                                     HTML('<hr>'),
                                                                     span("ENMeval", id = "rpkg"), "references", br(),
                                                                     div('Developers:  Robert Muscarella, Peter J. Galante, Mariano Soley-Guardia, Robert A. Boria,
                                                                                  Jamie M. Kass, Maria Uriarte, Robert P. Anderson', id="pkgDes"),
                                                                     a("CRAN", href = "http://cran.r-project.org/web/packages/ENMeval/index.html", target = "_blank"),
                                                                     " | ",
                                                                     a("documentation", href="https://cran.r-project.org/web/packages/ENMeval/ENMeval.pdf", target = "_blank"),
                                                                     " | ",
                                                                     a("software note", href="http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12261/abstract", target = "_blank"), br(),
                                                                     p(), span("dismo", id = "rpkg"), "references", br(),
                                                                     div('Developers:  Robert J. Hijmans, Steven Phillips, John Leathwick, Jane Elith', id="pkgDes"),
                                                                     a("CRAN", href = "http://cran.r-project.org/web/packages/dismo/index.html", target = "_blank"),
                                                                     " | ",
                                                                     a("documentation", href="https://cran.r-project.org/web/packages/dismo/dismo.pdf", target = "_blank")
                                                    ),
                                                    conditionalPanel("input.modSelect == 'BIOCLIM'",
                                                                     HTML('<hr>'),
                                                                     includeMarkdown("www/tab6_bc_refs.Rmd")
                                                    ),
                                                    conditionalPanel("input.modSelect == 'Maxent'",
                                                                     HTML('<hr>'),
                                                                     includeMarkdown("www/tab6_maxent_refs.Rmd")
                                                    )
                                   ),
                                   conditionalPanel("input.tabs == 7",
                                                    h4("Visualize Model Results"),
                                                    radioButtons("visSelect", "Modules Available:",
                                                                 choices = list("Map Prediction" = 'map', "Plot Response Curves" = 'response',
                                                                                "BIOCLIM Envelope Plots" = 'bcEnvel', "Maxent Evaluation Plots" = 'mxEval')),
                                                    HTML('<hr>'),
                                                    conditionalPanel("input.visSelect == 'map'",
                                                                     div('Module: Map Prediction', id="mod")),
                                                    conditionalPanel("input.visSelect == 'response'",
                                                                     div('Module: Response Curves', id="mod")),
                                                    conditionalPanel("input.visSelect == 'bcEnvel'",
                                                                     div('Module: BIOCLIM Envelope Plots', id="mod")),
                                                    conditionalPanel("input.visSelect == 'mxEval'",
                                                                     div('Module: Maxent Evaluation Plots', id="mod")),
                                                    span('via', id="pkgDes"),
                                                    span('dismo', id="rpkg"),
                                                    span('package: Automated Runs and Evaluations of Ecological Niche Models', id="pkgDes"),
                                                    conditionalPanel("input.visSelect == 'map'",
                                                                     checkboxInput('togMD7A', "Hide / Display Guidance Text", value = FALSE),
                                                                     conditionalPanel("input.togMD7A",
                                                                                      includeMarkdown("www/tab7_map.Rmd")),
                                                                     HTML('<hr>'),
                                                                     uiOutput("modelSel1"),
                                                                     conditionalPanel("input.modSelect == 'Maxent'",
                                                                                      selectInput('predForm', label = "Prediction output",
                                                                                                  choices = list("raw" = 'raw', "logistic" = 'log'),
                                                                                                  selected = 'log')
                                                                     ),
                                                                     selectInput('predThresh', label = "Set threshold",
                                                                                 choices = list("No threshold" = 'noThresh',
                                                                                                "ORmin" = 'mtp', "OR10" = 'p10'),
                                                                                 selected = ''),
                                                                     actionButton("plotPred", "Plot Prediction"),
                                                                     br(), br(),
                                                                     selectInput('predFileType', label = "Select File Type",
                                                                                 choices = list("GRD" = 'raster', "ASCII" = 'ascii', "GeoTIFF" = 'GTiff',
                                                                                                "PNG" = "png")),
                                                                     downloadButton('downloadPred', "Download Displayed Prediction"),
                                                                     HTML('<hr>')
                                                    ),
                                                    conditionalPanel("input.visSelect == 'response'",
                                                                     checkboxInput('togMD7B', "Hide / Display Guidance Text", value = FALSE),
                                                                     conditionalPanel("input.togMD7B",
                                                                                      includeMarkdown("www/tab7_respCurves.Rmd")
                                                                     ),
                                                                     HTML('<hr>'),
                                                                     uiOutput("modelSel2"),
                                                                     uiOutput("predVarSel")
                                                    ),
                                                    conditionalPanel("input.visSelect == 'bcEnvel'",
                                                                     checkboxInput('togMD7C', "Hide / Display Guidance Text", value = FALSE),
                                                                     conditionalPanel("input.togMD7C",
                                                                                      includeMarkdown("www/tab7_bcPlots.Rmd")
                                                                     ),
                                                                     HTML('<hr>'),
                                                                     "Pick a bioclimatic variable number for each axis",
                                                                     numericInput("bc1", "Axis 1", value = 1, min = 1, max = 19),
                                                                     numericInput("bc2", "Axis 2", value = 2, min = 1, max = 19),
                                                                     numericInput("bcProb", "Set threshold", value = 0.9, min = 0.75, max = 1, step = 0.05)
                                                    ),
                                                    conditionalPanel("input.visSelect == 'mxEval'",
                                                                     checkboxInput('togMD7D', "Hide / Display Guidance Text", value = FALSE),
                                                                     conditionalPanel("input.togMD7D",
                                                                                      includeMarkdown("www/tab7_mxEvalPlots.Rmd")
                                                                     ),
                                                                     HTML('<hr>'),
                                                                     selectInput('mxEvalSel', label = "Select Evaluation Plot",
                                                                                 choices = list("mean AUC" = 'Mean.AUC', "mean AUC DIFF" = 'Mean.AUC.DIFF', "mean OR MIN" = 'Mean.ORmin', 
                                                                                                "mean OR 10%" = 'Mean.OR10', "delta AICc" = 'delta.AICc'),
                                                                                 selected = 'Mean.AUC'),
                                                                     downloadButton('downloadEvalPlots', "Download All Evaluation Plots")
                                                    ),
                                                    'For input, this step pulls from the output of Step 6. For output, it provides a GRD, GEOTIFF,
                                                             or ASCII grid file (or PNG image) of the suitability prediction across the study region.',
                                                    conditionalPanel("input.visSelect == 'map'",
                                                                     HTML('<hr>'),
                                                                     span("dismo", id = "rpkg"), "references", br(),
                                                                     div('Developers:  Robert J. Hijmans, Steven Phillips, John Leathwick, Jane Elith', id="pkgDes"),
                                                                     a("CRAN", href = "http://cran.r-project.org/web/packages/dismo/index.html", target = "_blank"),
                                                                     " | ",
                                                                     a("documentation", href="https://cran.r-project.org/web/packages/dismo/dismo.pdf", target = "_blank"),
                                                                     HTML('<hr>'),
                                                                     includeMarkdown("www/tab7_pred_refs.Rmd")
                                                    )
                                   ),
                                   conditionalPanel("input.tabs == 8",
                                                    h4("Project Niche Model"),
                                                    radioButtons("projSelect", "Modules Available:",
                                                                 choices = list("Project to Current Extent" = 'pjCur', "View MESS" = 'mess'),
                                                                 selected = 'pjCur'),
                                                    HTML('<hr>'),
                                                    conditionalPanel("input.projSelect == 'pjCur'",
                                                                     div('Module: Project to Extent', id="mod")),
                                                    conditionalPanel("input.projSelect == 'mess'",
                                                                     div('Module: Multivariate Environmental Surface Similarity', id="mod")),
                                                    span('via', id="pkgDes"),
                                                    span('dismo', id="rpkg"),
                                                    span('package: Automated Runs and Evaluations of Ecological Niche Models', id="pkgDes"),
                                                    conditionalPanel("input.projSelect == 'pjCur'",
                                                                     checkboxInput('togMD8A', "Hide / Display Guidance Text", value = FALSE),
                                                                     conditionalPanel("input.togMD8A",
                                                                                      includeMarkdown("www/tab8_pjcur.Rmd")
                                                                     )
                                                    ),
                                                    conditionalPanel("input.projSelect == 'mess'",
                                                                     checkboxInput('togMD8B', "Hide / Display Guidance Text", value = FALSE),
                                                                     conditionalPanel("input.togMD8B",
                                                                                      includeMarkdown("www/tab8_mess.Rmd")
                                                                     )
                                                    ),
                                                    HTML('<hr>'),
                                                    uiOutput("modelSel3"),
                                                    actionButton("poly2Sel", "Select Projection Extent"),
                                                    br(), br(),
                                                    actionButton("erasePolyProjExt", "Reset Projection Extent"),
                                                    br(),
                                                    HTML('<hr>'),
                                                    conditionalPanel("input.projSelect == 'pjCur'",
                                                                     actionButton('goPjCur', "Project to Current Extent")
                                                    ),
                                                    conditionalPanel("input.projSelect == 'mess'",
                                                                     actionButton('goMESS', "View MESS for Current Extent")
                                                    ),
                                                    br(), br(),
                                                    #                                                              radioButtons('pjExtType', 'Select Current Display', 
                                                    #                                                                           choices = list("Project to Area" = 'pjArea', "View MESS" = 'mess')),
                                                    selectInput('pjFileType', label = "Select File Type",
                                                                choices = list("GRD" = 'raster', "ASCII" = 'ascii', "GeoTIFF" = 'GTiff',
                                                                               "PNG" = "png")),
                                                    downloadButton('downloadPj', "Download Displayed Grid")
                                   )
                   )
                   ),
                   column(8,
                          bsCollapse(id = 'guidanceText', bsCollapsePanel('Guidance Text', uiOutput('gtextOut'))),
                          conditionalPanel("input.tabs != 0 && input.tabs != 'rmd' && input.tabs != 'about'",
                                           div(id = "wallaceLog", class = "scrollbox", htmlOutput("log"))),
                          br(),
                          conditionalPanel("input.tabs == 1 || input.tabs == 2 || input.tabs == 3 || input.tabs == 4 || input.tabs == 5
                                                    || (input.tabs == 7 && input.visSelect == 'map') || input.tabs == 8", 
                                           leafletOutput("map", height=500)),
                          br(),
                          conditionalPanel("input.tabs != 0 && input.tabs != 6 && input.tabs != 7 && input.tabs != 8 &&
                                                    input.tabs != 'rmd' && input.tabs != 'about'", DT::dataTableOutput('occTbl')),
                          conditionalPanel("input.tabs == 6", dataTableOutput('evalTbl')),
                          conditionalPanel("input.tabs == 7 && input.visSelect == 'response'", imageOutput('respCurv')),
                          conditionalPanel("input.tabs == 7 && input.visSelect == 'bcEnvel'", imageOutput('bcEnvelPlot')),
                          conditionalPanel("input.tabs == 7 && input.visSelect == 'mxEval'", imageOutput('mxEvalPlot')),
                          conditionalPanel("input.tabs == 'rmd'",
                                           column(8,
                                                  selectInput('mdType', label = "R Markdown Download Type",
                                                              choices = list("Rmd", "PDF", "HTML", "Word")),
                                                  downloadButton('downloadMD', 'Download History in R Markdown'), br(), br(),
                                                  includeMarkdown("www/tab9_mdtext.Rmd")
                                           )
                          ),
                          conditionalPanel("input.tabs == 0",
                                           column(11,
                                                  includeMarkdown("www/intro.Rmd")
                                           )
                          ),
                          conditionalPanel("input.tabs == 'about'",
                                           fluidPage(titlePanel(h4("Wallace was created by an international team of ecologists:")),
                                                     fluidRow(
                                                       column(2, includeMarkdown("www/tab10Acontent.Rmd")),
                                                       column(3, includeMarkdown("www/tab10Ccontent.Rmd")),
                                                       column(5, includeMarkdown("www/tab10Bcontent.Rmd"))
                                                     )
                                           )
                          )
                   )
                   )
))