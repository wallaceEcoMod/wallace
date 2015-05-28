library(shiny)
library(leaflet)

#"Harnessing Digital Biodiversity Data via a GUI interface fueled by R"
title <- HTML(paste0("<strong>", span("WALLACE: ", style = "font-size:16pt"), "</strong>", 
              span("Harnessing Digital Biodiversity Data for Predictive Modeling, fueled by R", 
                   style = "font-size:10pt"), "  :::  ", 
              span("Developers: Jamie M. Kass, Matthew Aiello-Lammens, Bruno Vilela, 
              Robert Muscarella, Robert P. Anderson", style = "font-size:8pt")))
# Define UI for application that draws a histogram
shinyUI(navbarPage(title, id = "conditionedPanels",
                   tabPanel("1) Download / Plot / Clean Occurrence Data",
                            sidebarLayout(
                              sidebarPanel(
                                span(strong("rgbif"), style = "color:purple; font-size:18pt"), br(),
                                span(em("Interface to the Global Biodiversity Information Facility API"), style = "font-size:10pt"), br(),
                                span(em("Developers of rgbif:"), style = "font-size:10pt"), br(),
                                span(em("Other packages used: ggplot2"), style = "color:gray; font-size:8pt"), br(),
                                a("CRAN homepage", href = "http://cran.r-project.org/web/packages/rgbif/index.html"),
                                br(),
                                a("rgbif documentation @ GBIF", href = "http://www.gbif.org/resource/81747"),
                                hr(),
                                "The first step is to download occurrence data (e.g. from GBIF; duplicate 
                                records are removed). After acquiring these points, it is useful to examine 
                                them on a map.",
                                br(),
                                br(),
                                "Such datasets can contain errors; as a preliminary method of data-cleaning, 
                                here the user can specify records to be removed. Additionally, the user can 
                                download the records as a CSV file.",
                                br(),
                                br(),
                                "Future versions will allow the user to download occurrence records from other 
                                databases, as well as upload their own occurrence records as an alternate option.",
                                hr(),
                                #span(em("<Not currently functional>"), style = "color:gray; font-size:10pt"),
                                radioButtons("unusedDBselect", "Select Occurrence Data Source", 
                                             choices = list("GBIF via rgbif", "eBird via rebird (not functional)", 
                                                            "user input (not functional)"),
                                             selected = "GBIF via rgbif"),
                                hr(),
                                textInput("gbifName", label = "Enter scientific name of species (format: genus species)", value = 'tremarctos ornatus'),
                                actionButton("goName", "Submit name"),
                                br(),
                                br(),
                                sliderInput("occurrences", "Maximum number of occurrences:", min = 1, max = 500, value = 5),
                                hr(),
                                numericInput("num",  label="Enter the record ID to be removed", value = 0),
                                actionButton("remove", "Remove"),
                                hr(),
                                downloadButton('downloadGBIFcsv', "Download Occurrence CSV")
                              ),
                              mainPanel(
                                br(),
                                conditionalPanel("input.goName", textOutput('GBIFtxt')),
                                br(),
                              leafletOutput("map1"),
                              tableOutput('gbifOccTbl')
                              )
                            )
                   ),
                   tabPanel("2) Process Occurrence Data",
                            sidebarLayout(
                              sidebarPanel(
                                span(strong("spThin"), style = "color:purple; font-size:18pt"), br(),
                                span(em("Spatial Thinning of Species Occurrence Records"), style = "font-size:10pt"), br(),
                                span(em("Other packages used: ggplot2"), style = "color:gray; font-size:8pt"), br(),
                                span(em("Developers of spThin:"), style = "font-size:10pt"), br(),
                                a("CRAN homepage", href = "http://cran.r-project.org/web/packages/spThin/index.html"),
                                hr(),
                                "Datasets of occurrence records typically suffer from the effects of biased sampling 
                                across geography. spThin implements one way to reduce the effects of such biases, by 
                                spatial thinning that removes occurrence records less than a user-specified distance 
                                from other records. The user can download the thinned records as a CSV file. This step 
                                is optional.",
                                hr(),
                                numericInput("thinDist", label = "Thinning distance (km)", value = 0),
                                actionButton("goThin", "Run spThin"),
                                hr(),
                                downloadButton('downloadThincsv', "Download Thinned Occurrence CSV")
                              ),
                              mainPanel(
                                br(),
                                conditionalPanel("input.goThin", textOutput('thinText')),
                                textOutput('test'),
                             leafletOutput("map2"),
                             tableOutput('thinOccTbl')
                              )
                            )

                   ),
                   tabPanel("3) Choose Environmental Variables",
                            sidebarLayout(
                              sidebarPanel(
                                span(em("Packages used: ggplot2, sp, rgeos"), style = "color:gray; font-size:8pt"), br(),
                                "The user then chooses which environmental variables to use as
                                predictors. These data are in raster form.
                                For this demonstration, WorldClim bioclimatic variables are made available at 4 
                                resolutions. For Maxent and many other niche/distribution modeling approaches, 
                                selection of a study region is critical because it defines the pixels whose 
                                environmental values are compared with those of the pixels holding occurrence 
                                records of the species (citation/s). As one way to do so, the user can choose a 
                                bounding box or minimum convex polygon around the occurrence records, as well as 
                                buffer distance for either.", 
                                br(),
                                br(),
                                "Future versions will include other sets of environmental variables, as well as 
                                allow users to upload their own sets of environmental variables and designate a 
                                shapefile indicating a custom study region.", br(),
                                a("Worldclim homepage", href = "http://worldclim.org"),
                                hr(),
                                #span(em("<Not currently functional>"), style = "color:gray; font-size:10pt"),
                                radioButtons("unusedRasterselect", "Select Environmental Data Source", 
                                             choices = list("WorldClim", "Climond (not functional)", "PRISM (not functional)", 
                                                            "user-specified (not functional)"),
                                             selected = "WorldClim"),
                                hr(),
                                selectInput("pred", label = "Choose environmental data resolution",
                                            choices = list("Choose resolution" = "",  
                                                           "2.5 arcmin WorldClim bio1-19" = 2.5, 
                                                           "5 arcmin WorldClim bio1-19" = 5, 
                                                           "10 arcmin WorldClim bio1-19" = 10)),
                                conditionalPanel("input.pred == 'user'",
                                                 textInput("userPred", "Enter path to directory")),
                                selectInput("backg", label = "Study region selection",
                                            choices = list("Choose option" = "", "Bounding box" = 'bb', 
                                                           "Minimum convex polygon" = 'mcp',
                                                           "User-specified shapefile (not functional)")),
                                numericInput("backgBuf", label = "Study region buffer distance (degree)", value = 0)
                              ),
                              mainPanel(
                                conditionalPanel("input.pred != ''", uiOutput('predTxt1')),
                                conditionalPanel("input.pred == 'user'", uiOutput('predTxt2')),
                                leafletMap(
                                  "map3", "100%", 600,
                                  initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                                  initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
                                  options=list(
                                    center = c(0, 0),
                                    zoom = 2,
                                    minZoom  = 1,
                                    maxZoom = 12
                                  )
                                )                             
                              )
                            )
                   ),
                   tabPanel("4) Build & Evaluate Niche / Distribution Models",
                            sidebarLayout(
                              sidebarPanel(
                                span(strong("ENMeval"), style = "color:purple; font-size:18pt"), br(),
                                span(em("Automated Runs and Evaluations of Ecological Niche Models"), style = "font-size:10pt"), br(),
                                span(em("Other packages used: ggplot2, raster, dismo"), style = "color:gray; font-size:8pt"), br(),
                                span(em("Developers of ENMeval:"), style = "font-size:10pt"), br(),
                                a("CRAN homepage", href = "http://cran.r-project.org/web/packages/ENMeval/index.html"),
                                hr(),
                                "The output of niche/distribution models varies greatly depending on model settings, 
                                in particular those affecting the level of model complexity. This step automates the 
                                tedious building of a suite of candidate models with differing limitations on complexity. 
                                Furthermore, it quantifies their performance on test records. These metrics can aid the
                                user in selecting optimal settings.",
                                br(),
                                br(),
                                "Future versions will include other options for partitioning occurrence data. 
                                Furthermore, future development of ENMeval or similar packages could provide similar 
                                implementations with other algorithms.",
                                hr(),
                                #span(em("<Not currently functional>"), style = "color:gray; font-size:10pt"),
                                radioButtons("unusedAlgselect", "Select Algorithm", 
                                             choices = list("Maxent", "Boosted Regression Trees (not functional)", 
                                                            "Random Forest (not functional)", "GAM (not functional)"),
                                             selected = "Maxent"),
                                hr(),
                                checkboxGroupInput("fcs", label = "Select feature classes (flexibility of modeled response)", 
                                                   choices = list("L" = "L", "LQ" = "LQ", "H" = "H",
                                                                  "LQH" = "LQH", "LQHP" = "LQHP", "LQHPT" = "LQHPT")
                                ),
                                sliderInput("rms", label = "Select regularization multipliers (penalty against complexity)",
                                            min = 0, max = 10, value = c(1, 5)),
                                numericInput("rmsBy", label = "RM step value", value = 1),
                                selectInput("method", label = "Occurrence record partitioning method",
                                            choices = list("jackknife" = "jackknife", 
                                                           "block" = "block",
                                                           "checkerboard1" = "checkerboard1", 
                                                           "checkerboard2" = "checkerboard2",
                                                           "randomkfold (not functional)" = "randomkfold",
                                                           "user-specified (not functional)" = "user"), selected = "block"),
                                actionButton("goEval", "Run ENMeval"),
                                br(),
                                br(),
                                downloadButton('downloadEvalcsv', "Download ENMeval Results CSV")
                              ),
                              mainPanel(
                                conditionalPanel("input.goEval", textOutput('evalTxt')),
                                tableOutput('evalTbl'),
                                conditionalPanel("input.goEval", plotOutput('evalPlot', width = 600))
                              )
                            )
                   ),
                   tabPanel("5) View Predictions",
                            sidebarLayout(
                              sidebarPanel(
                                span(em("Packages used: raster"), style = "color:gray; font-size:8pt"), br(),
                                hr(),
                                "View the prediction rasters.",
                                hr(),
                                uiOutput("predSel"),
                                br(),
                                br(),
                                downloadButton('downloadPred', "Download Current Prediction Raster")
                              ),
                              mainPanel(
                                textOutput('test1'),
                                plotOutput('plotPred', width = 600)
                              )
                            )
                   )
))
