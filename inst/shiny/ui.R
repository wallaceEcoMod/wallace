resourcePath <- system.file("shiny", "www", package = "wallace")

tagList(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(
    script = file.path(resourcePath, "js", "shinyjs-funcs.js"),
    functions = c("scrollLogger", "removeModule")
  ),
  shinyalert::useShinyalert(),
  navbarPage(
    theme = shinythemes::shinytheme('united'),
    id = 'tabs',
    collapsible = TRUE,
    header = tagList(
      tags$head(tags$link(href = "css/styles.css", rel = "stylesheet"))
    ),
    title = glue::glue('#WallaceEcoMod'),
    tabPanel("Intro", value = 'intro'),
    tabPanel("Occ Data", value = 'occs'),
    tabPanel("Env Data", value = 'envs'),
    tabPanel("Process Occs", value = 'poccs'),
    tabPanel("Process Envs", value = 'penvs'),
    # tabPanel("Sampling", value='samp'),
    tabPanel("Env Space", value = 'espace'),
    tabPanel("Partition Occs", value = 'part'),
    tabPanel("Model", value = 'model'),
    tabPanel("Visualize", value = 'vis'),
    tabPanel("Project", value = 'proj'),
    tabPanel("Session Code", value = 'rmd')
  ),
  tags$div(
    class = "container-fluid",
    fluidRow(
      column(
        4,
        wellPanel(
          conditionalPanel(
            "input.tabs == 'intro'",
            includeMarkdown("Rmd/text_intro_tab.Rmd")
          ),
          # OBTAIN OCCS ####
          conditionalPanel(
            "input.tabs == 'occs'",
            h4("Obtain Occurrence Data"),
            radioButtons(
              "occsSel", "Modules Available:",
              choices = insert_modules_options("occs")
            ),
            tags$hr(),
            insert_modules_ui("occs")
          ),
          # OBTAIN ENVS ####
          conditionalPanel(
            "input.tabs == 'envs'",
            h4("Obtain Environmental Data"),
            radioButtons(
              "envsSel", "Modules Available:",
              choices = insert_modules_options("envs")
            ),
            tags$hr(),
            insert_modules_ui("envs")
          ),
          # PROCESS OCCS ####
          conditionalPanel(
            "input.tabs == 'poccs'",
            h4("Process Occurrence Data"),
            radioButtons(
              "poccsSel", "Modules Available:",
              choices = insert_modules_options("poccs")
              #"Profile Occurrences" = "profOccs"), # CM
            ),
            tags$hr(),
            insert_modules_ui("poccs"),
            # CM: start comment
            # conditionalPanel("input.poccsSel == 'profOccs'",
            #                  uiTop(profileOccs_INFO),
            #                  actionButton("goProfileOccs", "Profile Occurrences"), br(), br(),
            #                  profileOccs_UI('poccs_profileOccs_uiID'),
            #                  actionButton("goProfileOccsClean", "Clean Occurrences"),
            #                  uiBottom(profileOccs_INFO)
            # ),
            # CM: End comment
            tags$hr(),
            tags$strong("Reset to original occurrences"),
            tags$br(),
            actionButton("goResetOccs", "Reset", class = 'butResOccs'),
            tags$head(tags$style(".butResOccs {background-color: #C51E10;
                               color: white;
                               padding: 5px 5px;
                               border: none;}
                               .butResOccs:hover {background-color: #830D03;
                               color: white;}"))
          ),
          conditionalPanel(
            "input.tabs == 'penvs'",
            h4("Process Environmental Data"),
            radioButtons(
              "penvsSel", "Modules Available:",
              choices = insert_modules_options("penvs")
            ),
            tags$hr(),
            insert_modules_ui("penvs"),
            tags$hr(),
            tags$strong("Reset background (**)"),
            tags$br(),
            actionButton("goReset_penvs", "Reset", class = 'butResPenvs'),
            tags$head(tags$style(".butResPenvs {background-color: #C51E10;
                               color: white;
                               padding: 5px 5px;
                               border: none;}
                               .butResPenvs:hover {background-color: #830D03;
                               color: white;}"))
          ),
          # SAMPLING BIAS ####
          # conditionalPanel(
          #   "input.tabs == 'samp'",
          #   h4("Accounting for Sampling Bias"),
          #   radioButtons(
          #     "samplingBias", "Modules Available:",
          #     choices = list("User-specified" = "biasBgUser",
          #                    "Make Target Group" = "biasBgMake",
          #                    #"Sampling Covariates" = "sampCov",
          #                    "Bias Surface" = "biasFile")
          #   ),
          #   tags$hr(),
          #   conditionalPanel(
          #     "input.samplingBias == 'bgUserTarget'",
          #     #uiTop(bgExtent_INFO),
          #     span("Upload custom background", class="stepText"), br(), br(),
          #     userBiasBg_UI('samp_biasBG_uiID'),
          #     actionButton("goUserBiasBgUpload", "Select"), br(), br()#,
          #   ),
          #   conditionalPanel(
          #     "input.samplingBias == 'biasBgMake'",
          #     #uiTop(bgExtent_INFO),
          #     span("Step 1:", class="step"),
          #     span("Specify Target Group Species", class="stepText"), br(), br(),
          #     #queryDb_UI('samp_queryDb_uiID'),
          #     actionButton("goTargetDbOccs", "Query Database"), br(), br()#,
          #   ),
          #   # Placeholder not implemented yet
          #   # conditionalPanel("input.samplingBias == 'sampCov'",
          #   #                  #uiTop(userBgExtent_INFO),
          #   #                  span("Step 1:", class="step"),
          #   #                  span("Choose Background Extent", class="stepText"), br(), br(),
          #   #                  userBgExtent_UI('c4_userBgExtent'),
          #   #                  actionButton("goUserBg", "Load")),
          #   conditionalPanel(
          #     "input.samplingBias == 'biasFile'",
          #     #uiTop(drawBgExtent_INFO),
          #     span("Upload Bias File", class="stepText"), br(), br(),
          #     userBiasFile_UI('samp_biasFileUpload'),
          #     actionButton("goBiasFileUpload", "Upload")
          #   ),
          #   tags$hr()
          #   #conditionalPanel("input.penvsSel == 'bgTarget'", uiBottom(bgExtent_INFO)),
          #   #conditionalPanel("input.penvsSel == 'sampCov'", uiBottom(userBgExtent_INFO)),
          #   #conditionalPanel("input.penvsSel == 'biasSurf'", uiBottom(drawBgExtent_INFO))
          # ),
          # ESPACE ####
          conditionalPanel(
            "input.tabs == 'espace'",
            h4("Environmental Space"),
            radioButtons(
              "espaceSel", "Modules Available:",
              choices = insert_modules_options("espace")
            ),
            tags$hr(),
            insert_modules_ui("espace")
          ),
          # PARTITION ####
          conditionalPanel(
            "input.tabs == 'part'",
            h4("Partition Occurrence Data"),
            radioButtons(
              "partSel", "Modules Available:",
              choices = insert_modules_options("part")
            ),
            tags$hr(),
            insert_modules_ui("part")
          ),
          # MODEL ####
          conditionalPanel(
            "input.tabs == 'model'",
            h4("Build and Evaluate Niche Model"),
            radioButtons("modelSel", "Modules Available:",
              choices = insert_modules_options("model")),
            tags$hr(),
            insert_modules_ui("model")
          ),
          # VISUALIZE ####
          conditionalPanel(
            "input.tabs == 'vis'",
            h4("Visualize Model Results"),
            radioButtons("visSel", "Modules Available:",
              choices = insert_modules_options("vis")),
            tags$hr(),
            insert_modules_ui("vis")
          ),
          # PROJECT ####
          conditionalPanel(
            "input.tabs == 'proj'",
            h4("Project Model"),
            radioButtons(
              "projSel", "Modules Available:",
              choices = insert_modules_options("proj")),
            tags$hr(),
            insert_modules_ui("proj")
            # strong("Reset projection extent"), br(),
            # actionButton("goResetProj", "Reset", class = 'butResPj'),
            # tags$head(tags$style(".butResPj {background-color: #C51E10;
            #                      color: white;
            #                      padding: 1px 1px;
            #                      border: none;}
            #                      .butResPj:hover {background-color: #830D03;
            #                      color: white;}"))
          ),
          # SESSION CODE ####
          conditionalPanel(
            "input.tabs == 'rmd'",
            h4("Download Session Code and Metadata"),
            uiTop(rmd_INFO),
            strong("Select download file type"),
            selectInput('rmdFileType', label = "",
                        choices = list("Rmd", "PDF", "HTML", "Word")),
            downloadButton('dlRMD', 'Download Session Code'),
            tags$hr(),
            strong("Download metadata CSV files (**)"), br(), br(),
            downloadButton('dlRMM', 'Download Metadata'),
            tags$hr(),
            uiBottom(rmd_INFO)
          )
        )
      ),
      # --- RESULTS WINDOW ---
      column(
        8,
        conditionalPanel(
          "input.tabs != 'intro' & input.tabs != 'rmd'",
          fixedRow(
            column(
              4,
              absolutePanel(
                uiOutput("curSpUI"),
                uiOutput("curModelUI")
              )
            ),
            column(
              2,
              offset = 1,
              align = "left",
              div(
                id = "wallaceLog",
                div(id = "logHeader", div(id = "logContent"))
              )
            )
          )
        ),
        br(),
        conditionalPanel(
          "input.tabs != 'rmd' & input.tabs != 'intro'",
          tabsetPanel(
            id = 'main',
            tabPanel(
              'Map',
              leaflet::leafletOutput("map", height = 700),
              absolutePanel(
                top = 160, right = 20, width = 150, draggable = TRUE,
                selectInput("bmap", "",
                            choices = c('ESRI Topo' = "Esri.WorldTopoMap",
                                        'Stamen Terrain' = "Stamen.Terrain",
                                        'Open Topo' = "OpenTopoMap",
                                        'ESRI Imagery' = "Esri.WorldImagery",
                                        'ESRI Nat Geo' = 'Esri.NatGeoWorldMap'),
                            selected = "Esri.WorldTopoMap"
                )
              )
            ),
            tabPanel(
              'Table', br(),
              DT::dataTableOutput('occTbl')
            ),
            tabPanel(
              'Results',
              lapply(COMPONENTS, function(component) {
                conditionalPanel(
                  glue::glue("input.tabs == '{component}'"),
                  insert_modules_results(component)
                )
              })
            ),
            tabPanel(
              'Component Guidance',
              uiOutput('gtext_component')
            ),
            tabPanel(
              'Module Guidance',
              uiOutput('gtext_module')
            ),
            tabPanel(
              'Save session',
              h4("Save session"),
              p(paste0("By saving your session into a file, you can resume ",
                       "working on it at a later time or you can share the file",
                       " with a collaborator.")),
              shinyjs::hidden(p(
                id = "save_warning",
                icon("warning"),
                paste0("The current session data is large, which means the ",
                       "downloaded file may be large and the download might",
                       " take a long time.")
                )),
              downloadButton("save_session", "Save Session")
            ),
            tabPanel(
              'Download',
              conditionalPanel(
                "input.tabs == 'occs'",
                br(),
                fluidRow(
                  column(3, h5("Download original occurrence data")),
                  column(2, shinyjs::disabled(downloadButton('dlDbOccs', "CVS file")))
                ),
                br(),
                fluidRow(
                  column(3, h5("Download current table")),
                  column(2, shinyjs::disabled(downloadButton('dlOccs', "CVS file")))
                ),
                br(),
                fluidRow(
                  column(3, h5("Download all data")),
                  column(2, shinyjs::disabled(downloadButton('dlAllOccs', "CVS file")))
                )
              ),
              conditionalPanel(
                "input.tabs == 'poccs'",
                br(),
                fluidRow(
                  column(3, h5("Download processed occurence table")),
                  column(2, shinyjs::disabled(downloadButton('dlProcOccs', "CVS file")))
                )
              ),
              conditionalPanel(
                "input.tabs == 'envs'",
                br(),
                fluidRow(
                  column(3, h5("Download environmental variables (Select download file type)")),
                  column(2, selectInput('globalEnvsFileType',
                                        label = NULL,
                                        choices = list("GeoTIFF" = 'GTiff',
                                                       "GRD" = 'raster',
                                                       "ASCII" = 'ascii'))),
                  column(2, shinyjs::disabled(downloadButton('dlGlobalEnvs', "ZIP file")))
                )
              ),
              conditionalPanel(
                "input.tabs == 'penvs'",
                br(),
                fluidRow(
                  column(3, h5("Download shapefile of background extent")),
                  column(2, shinyjs::disabled(downloadButton('dlBgShp', "ZIP file")))
                ),
                br(),
                fluidRow(
                  column(3, h5("Download predictor rasters masked to background extent (Select download file type)")),
                  column(2, selectInput('bgMskFileType',
                                        label = NULL,
                                        choices = list("GeoTIFF" = 'GTiff',
                                                       "GRD" = 'raster',
                                                       "ASCII" = 'ascii'))),
                  column(2, shinyjs::disabled(downloadButton('dlMskEnvs', "ZIP file")))
                ),
                br(),
                fluidRow(
                  column(3, h5("Download sample background points")),
                  column(2, shinyjs::disabled(downloadButton('dlBgPts', "CVS file")))
                )
              ),
              conditionalPanel(
                "input.tabs == 'espace'",
                br(),
                fluidRow(
                  column(3, h5("Download PCA results")),
                  column(2, shinyjs::disabled(downloadButton('dlPcaResults', "ZIP file")))
                ),
                br(),
                fluidRow(
                  column(3, h5("Download Occurence density grid")),
                  column(2, shinyjs::disabled(downloadButton('dlOccDens', "PNG file")))
                ),
                br(),
                fluidRow(
                  column(3, h5("Download Niche Overlap plot")),
                  column(2, shinyjs::disabled(downloadButton('dlNicheOvPlot', "PNG file")))
                )
              ),
              conditionalPanel(
                "input.tabs == 'part'",
                br(),
                fluidRow(
                  column(3, h5("Download occurrence and background localities with partition values")),
                  column(2, shinyjs::disabled(downloadButton('dlPart', "CVS file")))
                )
              ),
              conditionalPanel(
                "input.tabs == 'model'",
                br(),
                fluidRow(
                  column(3, h5("Download evaluation table")),
                  column(2, shinyjs::disabled(downloadButton('dlEvalTbl', "CSV file")))
                ), br(),
                fluidRow(
                  column(3, h5("Download evaluation groups table")),
                  column(2, shinyjs::disabled(downloadButton('dlEvalTblBins', "CSV file")))
                )
              ),
              conditionalPanel(
                "input.tabs == 'vis'",
                br(),
                conditionalPanel(
                  "input.modelSel == 'BIOCLIM'",
                  fluidRow(
                    column(3, h5("Download Bioclim plot (**)")),
                    column(2, shinyjs::disabled(downloadButton('dlVisBioclim', "PNG file")))
                  )
                ),
                conditionalPanel(
                  "input.modelSel == 'Maxent'",
                  fluidRow(
                    column(3, h5("Download Maxent plots (**)")),
                    column(2, shinyjs::disabled(downloadButton('dlMaxentPlots', "ZIP file")))
                  ),
                  br(),
                  fluidRow(
                    column(3, h5("Download Response plots (**)")),
                    column(2, shinyjs::disabled(downloadButton('dlRespCurves', "ZIP file")))
                  )
                ),
                br(),
                fluidRow(
                  column(3, h5("Download current prediction (Select download file type**)")),
                  column(2, selectInput('predFileType',
                                        label = NULL,
                                        choices = list("GeoTIFF" = 'GTiff',
                                                       "GRD" = 'raster',
                                                       "ASCII" = 'ascii',
                                                       "PNG" = 'png'))),
                  column(2, shinyjs::disabled(downloadButton('dlPred', "Predition file(**)")))
                )
              ),
              conditionalPanel(
                "input.tabs == 'proj'",
                fluidRow(
                  column(3, h5("Download shapefile of projection extent")),
                  column(2, shinyjs::disabled(downloadButton('dlPjShp', "ZIP file")))
                ),
                br(),
                fluidRow(
                  column(3, h5("Download projection (Select download file type**)")),
                  column(2, selectInput('projFileType',
                                        label = NULL,
                                        choices = list("GeoTIFF" = 'GTiff',
                                                       "GRD" = 'raster',
                                                       "ASCII" = 'ascii',
                                                       "PNG" = 'png'))),
                  column(2, shinyjs::disabled(downloadButton('dlProj', "Projection file(**)")))
                ),
                fluidRow(
                  column(3, h5("Download MESS (Select download file type**)")),
                  column(2, selectInput('messFileType',
                                        label = NULL,
                                        choices = list("GeoTIFF" = 'GTiff',
                                                       "GRD" = 'raster',
                                                       "ASCII" = 'ascii',
                                                       "PNG" = 'png'))),
                  column(2, shinyjs::disabled(downloadButton('dlMess', "MESS file(**)")))
                )
              )
            )

          )
        ),
        conditionalPanel(
          "input.tabs == 'rmd'",
          column(8,
                 includeMarkdown("Rmd/text_sessionCode.Rmd")
          )
        ),
        conditionalPanel(
          "input.tabs == 'intro'",
          tabsetPanel(
            id = 'introTabs',
            tabPanel(
              'Intro',
              includeMarkdown("Rmd/text_intro.Rmd")
            ),
            tabPanel(
              'About',
              fluidRow(
                column(8, includeMarkdown("Rmd/text_about.Rmd")
                )
              )
            ),
            tabPanel(
              'Load',
              h4("Load session"),
              fileInput("load_session", "", accept = ".rds"),
              actionButton('goLoad_session', 'Load RDS')
            )
          )
        )
      )
    )
  )
)
