source("funcs/functions.R", local = TRUE)
# load modules
for (f in list.files('./modules')) {
  source(file.path('modules', f), local=TRUE)
}

# Define UI for application
shinyUI(tagList(
  shinyjs::useShinyjs(),
  navbarPage(theme=shinythemes::shinytheme('united'), id='tabs', collapsible=TRUE,
             title='Wallace',
             tabPanel("Intro", value=0),
             tabPanel("1 Occ Data", value=1),
             tabPanel("2 Process Occs", value=2),
             tabPanel("3 Env Data", value=3),
             tabPanel("4 Process Envs", value=4),
             tabPanel("5 Partition Occs", value=5),
             tabPanel("6 Model", value=6),
             tabPanel("7 Visualize", value=7),
             tabPanel("8 Project", value=8),
             tabPanel("Session Code", value='rmd'),
             
             fluidRow(column(4,
                             wellPanel(
                               includeCSS(system.file("css", "styles.css", package = "wallace")),
                               includeScript(system.file("js", "scroll.js", package = "wallace")),
                               conditionalPanel("input.tabs == 0",
                                                includeMarkdown(system.file("Rmd", "text_intro_tab.Rmd", package = "wallace"))
                               ),
                               # tab 1 ####
                               conditionalPanel("input.tabs == 1",
                                                h4("Obtain Occurrence Data"),
                                                radioButtons("occSel", "Modules Available:",
                                                             choices = list("Query Database" = 'db', "User-specified Occurrences" = 'user'),
                                                             selected = 'db'),
                                                HTML('<hr>'),
                                                conditionalPanel("input.occSel == 'db'",
                                                                 uiTop('Query Database', 'spocc', 'Interface to Species Occurrence Data Sources'),
                                                                 queryDB_UI('c1_queryDB'),
                                                                 actionButton("goDbOccs", "Query Database"),
                                                                 strong("Download database occurrence localities (.csv)"), br(), br(),
                                                                 downloadButton('dlDbOccs', "Download DB Occurrences"),
                                                                 uiBottom('spocc', "Scott Chamberlain, Karthik Ram, Ted Hart")
                                                ),
                                                conditionalPanel("input.occSel == 'user'",
                                                                 div('Module: User-specified Occurrences', id="mod"),
                                                                 HTML('<hr>'),
                                                                 userOccs_UI('c1_userOccs'),
                                                                 actionButton("goUserOccs", "Load Occurrences")
                                                )
                               ),
                               # tab 2 ####
                               conditionalPanel("input.tabs == 2",
                                                h4("Process Occurrence Data"),
                                                radioButtons("procOccSel", "Modules Available:",
                                                             choices = list("Select Localities" = 'selpts',
                                                                            "Spatial Thin" = 'spthin')),
                                                HTML('<hr>'),
                                                # placeholder for select on map
                                                conditionalPanel("input.procOccSel == 'spthin'",
                                                                 uiTop('Spatial Thin', 'spThin', 'Spatial Thinning of Species Occurrence Records'),
                                                                 thinOccs_UI('c2_thinOccs'),
                                                                 actionButton("goThinOccs", "Thin Occurrences"), br(), br(),
                                                                 uiBottom('spThin', "Matthew E. Aiello-Lammens, Rob A. Boria, 
                                                                                          Alex Radosavljevic, Bruno Vilela, Robert P. Anderson"),
                                                                 " | ", a("software note", href="http://onlinelibrary.wiley.com/doi/10.1111/ecog.01132/abstract", target = "_blank")
                                                ),
                                                actionButton("erasePolySelLocs", "Reset"),
                                                HTML('<hr>'),
                                                strong("Download processed occurrence localities (.csv)"), br(), br(),
                                                downloadButton('dlProcOccCsv', "Download")
                               ),
                               # tab 3 ####
                               conditionalPanel("input.tabs == 3",
                                                h4("Obtain Environmental Data"),
                                                radioButtons("envSel", "Modules Available:",
                                                             choices = list("WorldClim Bioclims" = 'wcbc',
                                                                            "User" = 'user')),
                                                HTML('<hr>'),
                                                conditionalPanel("input.envSel == 'wcbc'",
                                                                 uiTop('WorldClim', 'raster', 'Geographic Data Analysis and Modeling'),
                                                                 wcBioclims_UI("c3_wcBioclims"),
                                                                 strong("Using map center coordinates as reference for tile download."),
                                                                 textOutput('ctrLatLon'), br(),
                                                                 actionButton("goEnvData", "Download Env Data"),
                                                                 uiBottom('raster', "Robert J. Hijmans, Jacob van Etten, Joe Cheng, Matteo Mattiuzzu, 
                                                                                 Michael Sumner, Jonathan A. Greenberg, Oscar Perpinan Lamigueriro, Andrew Bevan, 
                                                                                 Etienne B. Racine, Ashton Shortridge"),
                                                                 " | ", a("WorldClim", href="http://worldclim.org", target="_blank")
                                                ),
                                                conditionalPanel("input.envSel == 'user'",
                                                                 div('Module: User-specified Environmental Predictors', id="mod"),
                                                                 HTML('<hr>'),
                                                                 userEnvs_UI('c3_userEnvs'),
                                                                 actionButton('goUserEnvs', 'Load Rasters')
                                                )
                               ),
                               # tab 4 ####
                               conditionalPanel("input.tabs == 4",
                                                h4("Process Environmental Data"),
                                                radioButtons("envProcSel", "Modules Available:",
                                                             choices = list("Select Study Region" = "bgSel",
                                                                            "User-specified Study Region" = "bgUser")),
                                                
                                                HTML('<hr>'),
                                                conditionalPanel("input.envProcSel == 'bgSel'",
                                                                 uiTop('Select Study Region', 'sp | rgeos', 'Title Classes and Methods for Spatial Data |
                                                                            Interface to Geometry Engine - Open Source (GEOS)'),
                                                                 bgSelect_UI('c4_bgSelect'),
                                                                 actionButton("goBgSel", "Choose"),
                                                                 strong('Mask environmental predictor rasters by polygon'), br(), br(),
                                                                 actionButton("goBgMask", "Mask"), br(), br(),
                                                                 selectInput('mskPredsFileType', label = "Select File Type",
                                                                             choices = list("GRD" = 'raster', "ASCII" = 'ascii', "GeoTIFF" = 'GTiff')),
                                                                 strong("Download masked environmental predictors"), br(), br(),
                                                                 downloadButton('downloadMskPreds', "Download"),
                                                                 uiBottom('spThin', "Matthew E. Aiello-Lammens, Rob A. Boria, 
                                                                                Alex Radosavljevic, Bruno Vilela, Robert P. Anderson")
                                                ),
                                                conditionalPanel("input.envProcSel == 'backg'",
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
                                                                 a("documentation", href="https://cran.r-project.org/web/packages/rgeos/rgeos.pdf", target = "_blank")
                                                )
                                                
                               )
                             )
             ),
             column(8,
                    conditionalPanel("input.tabs != 0 && input.tabs != 'rmd'",
                                     div(id = "wallaceLog", class = "scrollbox", htmlOutput("log")),
                                     absolutePanel(top = -10, right = 20, width = 150, draggable = TRUE,
                                                   selectInput("bmap", "Change Base Map", choices = c('ESRI Topo'="Esri.WorldTopoMap",
                                                                                                      'Stamen Terrain'="Stamen.Terrain",
                                                                                                      'Open Topo'="OpenTopoMap",
                                                                                                      'ESRI Imagery'="Esri.WorldImagery",
                                                                                                      'ESRI Nat Geo'='Esri.NatGeoWorldMap'),
                                                               selected = "Esri.WorldTopoMap"))
                    ),
                    br(),
                    conditionalPanel("input.tabs != 'rmd' & input.tabs != 0",
                                     tabsetPanel(id = 'main',
                                                 tabPanel('Map', leaflet::leafletOutput("map", height=600)),
                                                 tabPanel('Occs Tbl', DT::dataTableOutput('occTbl')),
                                                 tabPanel('Results', 
                                                          conditionalPanel("input.tabs == 3", verbatimTextOutput('envsPrint')),
                                                          conditionalPanel("input.tabs == 6", DT::dataTableOutput('evalTbl')),
                                                          conditionalPanel("input.tabs == 7 && input.visSel == 'response'",
                                                                           imageOutput('respCurv')),
                                                          conditionalPanel("input.tabs == 7 && input.visSel == 'bcEnvel' && input.enmSel == 'BIOCLIM'",
                                                                           imageOutput('bcEnvelPlot')),
                                                          conditionalPanel("input.tabs == 7 && input.visSel == 'mxEval'  && input.enmSel == 'Maxent'",
                                                                           imageOutput('mxEvalPlot'))),
                                                 tabPanel('Component Guidance', uiOutput('gtext_comp')),
                                                 tabPanel('Module Guidance', uiOutput('gtext_mod'))
                                     )
                    ),
                    conditionalPanel("input.tabs == 'rmd'",
                                     column(8,
                                            includeMarkdown(system.file("Rmd", "text_sessionCode.Rmd", package = "wallace"))
                                     )
                    ),
                    conditionalPanel("input.tabs == 0",
                                     tabsetPanel(id = 'introTabs',
                                                 tabPanel('Intro', includeMarkdown(system.file("Rmd", "text_intro.Rmd", package = "wallace"))),
                                                 tabPanel('About',
                                                          fluidRow(
                                                            column(8,
                                                                   includeMarkdown(system.file("Rmd", "text_about.Rmd", package = "wallace"))
                                                            )
                                                            
                                                          )
                                                 )
                                     )
                    )
             )
             )
  )))
