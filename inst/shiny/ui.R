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
    title = glue::glue('Wallace v{packageVersion("wallace")}'),
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
            insert_modules_ui("poccs")  #,
            # CM: start comment
            # conditionalPanel("input.poccsSel == 'profOccs'",
            #                  uiTop(profileOccs_INFO),
            #                  actionButton("goProfileOccs", "Profile Occurrences"), br(), br(),
            #                  profileOccs_UI('poccs_profileOccs_uiID'),
            #                  actionButton("goProfileOccsClean", "Clean Occurrences"),
            #                  uiBottom(profileOccs_INFO)
            # ),
            # CM: End comment

            # GEPB: Reset button
            # tags$br(),
            # tags$strong("Reset to original occurrences"),
            # tags$br(),
            # actionButton("goResetOccs", "Reset", class = 'butResOccs'),
            # tags$head(tags$style(".butResOccs {background-color: #C51E10;
            #                    color: white;
            #                    padding: 5px 5px;
            #                    border: none;}
            #                    .butResOccs:hover {background-color: #830D03;
            #                    color: white;}"))
            # GEPB: End Comment
          ),
          conditionalPanel(
            "input.tabs == 'penvs'",
            h4("Process Environmental Data"),
            radioButtons(
              "penvsSel", "Modules Available:",
              choices = c(
                insert_modules_options("penvs"),
                "Select Study Region" = "bgSel",
                "User-specified" = "bgUser")
            ),
            tags$hr(),
            insert_modules_ui("penvs"),
            conditionalPanel(
              "input.penvsSel == 'bgSel'",
              uiTop(bgExtent_INFO),
              span("Step 1:", class = "step"),
              span("Choose Background Extent", class = "stepText"), br(), br(),
              bgExtent_UI('c4_bgExtent_uiID'),
              actionButton("goBgExt", "Select"), br(), br()
            ),
            conditionalPanel(
              "input.penvsSel == 'bgUser'",
              uiTop(userBgExtent_INFO),
              span("Step 1:", class = "step"),
              span("Choose Background Extent", class = "stepText"), br(), br(),
              userBgExtent_UI('c4_userBgExtent'),
              actionButton("goUserBg", "Load")),
            conditionalPanel(
              "input.penvsSel == 'bgSel' | input.penvsSel == 'bgUser'",
              tags$hr(),
              span("Step 2:", class = "step"),
              span("Sample Background Points", class = "stepText"), br(), br(),
              strong('Mask predictor rasters by background extent and sample background points'), br(), br(),
              bgMskAndSamplePts_UI('c4_bgMskAndSamplePts'),
              actionButton("goBgMask", "Sample")
            ),
            tags$hr(),
            conditionalPanel(
              "input.penvsSel == 'bgSel'",
              uiBottom(bgExtent_INFO)
            ),
            conditionalPanel(
              "input.penvsSel == 'bgUser'",
              uiBottom(userBgExtent_INFO)
            )
          ),
          # SAMPLING BIAS ####
          conditionalPanel(
            "input.tabs == 'samp'",
            h4("Accounting for Sampling Bias"),
            radioButtons(
              "samplingBias", "Modules Available:",
              choices = list("User-specified" = "biasBgUser",
                             "Make Target Group" = "biasBgMake",
                             #"Sampling Covariates" = "sampCov",
                             "Bias Surface" = "biasFile")
            ),
            tags$hr(),
            conditionalPanel(
              "input.samplingBias == 'bgUserTarget'",
              #uiTop(bgExtent_INFO),
              span("Upload custom background", class="stepText"), br(), br(),
              userBiasBg_UI('samp_biasBG_uiID'),
              actionButton("goUserBiasBgUpload", "Select"), br(), br()#,
            ),
            conditionalPanel(
              "input.samplingBias == 'biasBgMake'",
              #uiTop(bgExtent_INFO),
              span("Step 1:", class="step"),
              span("Specify Target Group Species", class="stepText"), br(), br(),
              #queryDb_UI('samp_queryDb_uiID'),
              actionButton("goTargetDbOccs", "Query Database"), br(), br()#,
            ),
            # Placeholder not implemented yet
            # conditionalPanel("input.samplingBias == 'sampCov'",
            #                  #uiTop(userBgExtent_INFO),
            #                  span("Step 1:", class="step"),
            #                  span("Choose Background Extent", class="stepText"), br(), br(),
            #                  userBgExtent_UI('c4_userBgExtent'),
            #                  actionButton("goUserBg", "Load")),
            conditionalPanel(
              "input.samplingBias == 'biasFile'",
              #uiTop(drawBgExtent_INFO),
              span("Upload Bias File", class="stepText"), br(), br(),
              userBiasFile_UI('samp_biasFileUpload'),
              actionButton("goBiasFileUpload", "Upload")
            ),
            tags$hr()
            #conditionalPanel("input.penvsSel == 'bgTarget'", uiBottom(bgExtent_INFO)),
            #conditionalPanel("input.penvsSel == 'sampCov'", uiBottom(userBgExtent_INFO)),
            #conditionalPanel("input.penvsSel == 'biasSurf'", uiBottom(drawBgExtent_INFO))
          ),
          # ESPACE ####
          conditionalPanel(
            "input.tabs == 'espace'",
            h4("Environmental Space"),
            radioButtons(
              "espaceSel", "Modules Available:",
              choices = c(insert_modules_options("espace"),
                "Principal Components Analysis" = "pca",
                "Occurrence Density Grid" = "occDens",
                "Niche Overlap" = "nicheOv")
            ),
            tags$hr(),
            insert_modules_ui("espace"),
            conditionalPanel(
              "input.espaceSel == 'pca'",
              uiTop(espace_pca_INFO),
              pca_controlsUI('cEspace_PCA_uiID'),
              actionButton("goPCA", "Run")
            ),
            conditionalPanel(
              "input.espaceSel == 'occDens'",
              uiTop(espace_occDens_INFO),
              occDens_controlsUI('cEspace_occDens_uiID'),
              actionButton("goOccDens", "Run")
            ),
            conditionalPanel(
              "input.espaceSel == 'nicheOv'",
              uiTop(espace_nicheOv_INFO),
              nicheOv_controlsUI('cEspace_nicheOv_uiID'),
              actionButton("goNicheOv", "Run")
            ),
            tags$hr(),
            conditionalPanel(
              "input.espaceSel == 'pca'",
              uiBottom(espace_pca_INFO)
            ),
            conditionalPanel(
              "input.espaceSel == 'occDens'",
              uiBottom(espace_occDens_INFO)
            ),
            conditionalPanel(
              "input.espaceSel == 'nicheOv'",
              uiBottom(espace_nicheOv_INFO)
            )
          ),
          # PARTITION ####
          conditionalPanel(
            "input.tabs == 'part'",
            h4("Partition Occurrence Data"),
            radioButtons(
              "partSel", "Modules Available:",
              choices = c(
                insert_modules_options("part"),
                "Non-spatial Partition" = 'nsp',
                "Spatial Partition" = 'sp'),
              selected = 'sp' # Check default (no selected)
            ),
            tags$hr(),
            insert_modules_ui("part"),
            conditionalPanel(
              "input.partSel == 'sp'",
              uiTop(partitionSpat_INFO),
              partitionSpat_UI('cParts_partitionSpat_uiID'),
              actionButton("goPartitionSpat", "Partition")),
            conditionalPanel(
              "input.partSel == 'nsp'",
              uiTop(partitionNonSpat_INFO),
              partitionNonSpat_UI('cParts_partitionNonSpat_uiID'),
              actionButton("goPartitionNonSpat", "Partition")),
            tags$hr(),
            conditionalPanel(
              "input.partSel == 'sp'",
              uiBottom(partitionSpat_INFO)
            ),
            conditionalPanel(
              "input.partSel == 'nsp'",
              uiBottom(partitionNonSpat_INFO)
            )
          ),
          # MODEL ####
          conditionalPanel(
            "input.tabs == 'model'",
            h4("Build and Evaluate Niche Model"),
            radioButtons(
              "modelSel", "Modules Available:",
              choices = c(
                "BIOCLIM", "Maxent",
                insert_modules_options("model")
              ),
              selected = "Maxent" # Check default (no selected)
            ),
            tags$hr(),
            conditionalPanel(
              "input.modelSel == 'Maxent'",
              uiTop(runMaxent_INFO),
              htmlOutput('maxentJar'), br(),
              "(", HTML("<font color='blue'><b>NOTE</b></font>"),
              ": see module guidance for troubleshooting tips if you are experiencing problems.)",
              tags$hr(),
              runMaxent_UI('runMaxent_uiID'),
              actionButton('goMaxent', 'Run')
            ),
            conditionalPanel(
              "input.modelSel == 'BIOCLIM'",
              uiTop(runBIOCLIM_INFO),
              runBIOCLIM_UI('runBIOCLIM_uiID'),
              actionButton('goBIOCLIM', 'Run')
            ),
            insert_modules_ui("model"),
            tags$hr(),
            conditionalPanel(
              "input.modelSel == 'Maxent'",
              uiBottom(runMaxent_INFO)
            ),
            conditionalPanel(
              "input.modelSel == 'BIOCLIM'",
              uiBottom(runBIOCLIM_INFO)
            )
          ),
          # VISUALIZE ####
          conditionalPanel(
            "input.tabs == 'vis'",
            h4("Visualize Model Results"),
            radioButtons(
              "visSel", "Modules Available:",
              choices = c(
                insert_modules_options("vis"),
                "BIOCLIM Envelope Plots" = 'bioclimPlot',
                "Maxent Evaluation Plots" = 'maxentEval',
                "Plot Response Curves" = 'response',
                "Map Prediction" = 'mapPreds'),
              selected = 'mapPreds' # Check default (no selected param)
            ),
            tags$hr(),
            insert_modules_ui("vis"),
            conditionalPanel(
              "input.visSel == 'bioclimPlot'",
              uiTop(bioclimPlot_INFO),
              bioclimPlot_UI('c7_bioclimPlot')
            ),
            conditionalPanel(
              "input.visSel == 'maxentEval'",
              uiTop(maxentEvalPlot_INFO),
              maxentEvalPlot_UI('c7_maxentEvalPlot')
            ),
            conditionalPanel(
              "input.visSel == 'response'",
              uiTop(responsePlot_INFO),
              responsePlot_UI('c7_responsePlot')
            ),
            conditionalPanel(
              "input.visSel == 'mapPreds'",
              uiTop(mapPreds_INFO),
              mapPreds_UI('c7_mapPreds'),
              actionButton("goMapPreds", "Plot"), br()
            ),
            tags$hr(),
            conditionalPanel(
              "input.visSel == 'bioclimPlot'",
              uiBottom(bioclimPlot_INFO)
            ),
            conditionalPanel(
              "input.visSel == 'maxentEval'",
              uiBottom(maxentEvalPlot_INFO)
            ),
            conditionalPanel(
              "input.visSel == 'response'",
              uiBottom(responsePlot_INFO)
            ),
            conditionalPanel(
              "input.visSel == 'mapPreds'",
              uiBottom(mapPreds_INFO)
            )
          ),
          # PROJECT ####
          conditionalPanel(
            "input.tabs == 'proj'",
            h4("Project Model"),
            radioButtons(
              "projSel", "Modules Available:",
              choices = c(
                insert_modules_options("proj"),
                "Project to New Extent" = 'projArea',
                "Project to New Time" = 'projTime',
                "Project to User-files (**)" = 'projUser',
                "Calculate Environmental Similarity" = 'mess'),
              selected = 'projArea'
            ),
            tags$hr(),
            insert_modules_ui("proj"),
            conditionalPanel(
              "input.projSel == 'projArea'",
              uiTop(projectArea_INFO),
              span("Step 1:", class = "step"),
              span("Choose Study Region (**)", class = "stepText"), br(), br(),
              projExtent_UI('c8_projectExtentArea_uiID'),
              actionButton("goProjExtArea", "Create(**)"), br(), br(),
              tags$hr(),
              span("Step 2:", class = "step"),
              span("Project (**)", class = "stepText"), br(), br(),
              p("Project model to project extent (red) (**)"), br(),
              projectArea_UI('c8_projectArea'),
              actionButton('goProjectArea', "Project")
            ),
            conditionalPanel(
              "input.projSel == 'projTime'",
              uiTop(projectTime_INFO),
              span("Step 1:", class = "step"),
              span("Choose Study Region (**)", class = "stepText"), br(), br(),
              projExtent_UI('c8_projectExtentTime_uiID'),
              actionButton("goProjExtTime", "Create(**)"), br(), br(),
              tags$hr(),
              span("Step 2:", class = "step"),
              span("Project (**)", class = "stepText"), br(), br(),
              p("Project model to project extent (red) (**)"), br(),
              projectTime_UI('c8_projectTime'),
              actionButton('goProjectTime', "Project")
            ),
            conditionalPanel(
              "input.projSel == 'projUser'",
              uiTop(projectUser_INFO),
              span("Step 1:", class = "step"),
              span("Choose Study Region (**)", class = "stepText"), br(), br(),
              projExtent_UI('c8_projectExtentUser_uiID'),
              actionButton("goProjExtUser", "Create(**)"), br(), br(),
              tags$hr(),
              span("Step 2:", class = "step"),
              span("Project (**)", class = "stepText"), br(), br(),
              p("Project model to project extent (red) (**)"),
              projectUser_UI('c8_projectUser_uiID'),
              actionButton('goProjectUser', "Project")
            ),
            conditionalPanel(
              "input.projSel == 'mess'",
              uiTop(envSimilarity_INFO),
              envSimilarity_UI('c8_envSimilarity'),
              strong("Calculate MESS for current extent"), br(), br(),
              actionButton('goEnvSimilarity', "Calculate MESS")
            ),
            strong("Reset projection extent"), br(),
            actionButton("goResetProj", "Reset", class = 'butResPj'),
            tags$head(tags$style(".butResPj {background-color: #C51E10;
                                 color: white;
                                 padding: 1px 1px;
                                 border: none;}
                                 .butResPj:hover {background-color: #830D03;
                                 color: white;}")),
            tags$hr(),
            conditionalPanel(
              "input.projSel == 'projArea'",
              uiBottom(projectArea_INFO)
            ),
            conditionalPanel(
              "input.projSel == 'projTime'",
              uiBottom(projectTime_INFO)
            ),
            conditionalPanel(
              "input.projSel == 'projUser'",
              uiBottom(projectUser_INFO)
            ),
            conditionalPanel(
              "input.projSel == 'mess'",
              uiBottom(envSimilarity_INFO)
            )
          ),
          # SESSION CODE ####
          conditionalPanel(
            "input.tabs == 'rmd'",
            h4("Save session"),
            p("By saving your session into a file, you can resume working on it at a later time or you can share the file with a collaborator."),
            shinyjs::hidden(p(
              id = "save_warning",
              icon("warning"),
              "The current session data is large, which means the downloaded file may be large and the download might take a long time."
            )),
            checkboxInput("save_portable", "Ensure the session data is portable and can be loaded on other computers", FALSE),
            downloadButton("save_session", "Save Session"), br(), br(),
            fileInput("load_session", "Load Session", accept = ".rds"),
            h4("Download Session Code"),
            uiTop(rmd_INFO),
            selectInput('rmdFileType', label = "Select download file type",
                        choices = list("Rmd", "PDF", "HTML", "Word")),
            downloadButton('dlRMD', 'Download Session Code'),
            tags$hr(),
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
            #uiOutput("curEnvUI"))),
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
              }),
              # conditionalPanel("input.tabs == 'poccs'",
              #                  profileOccs_resultsUI("poccs_profileOccs_uiID")),
              conditionalPanel(
                "input.tabs == 'model'",
                conditionalPanel("input.modelSel == 'BIOCLIM' || input.modelSel == 'Maxent'",
                                 uiOutput('evalTbls'))
              ),
              conditionalPanel(
                "input.tabs == 'vis' && input.visSel == 'response'",
                imageOutput('responsePlot')
              ),
              conditionalPanel(
                "input.tabs == 'vis' && input.visSel == 'bioclimPlot' && input.modelSel == 'BIOCLIM'",
                imageOutput('bioclimPlot')
              ),
              conditionalPanel(
                "input.tabs == 'vis' && input.visSel == 'maxentEval' && input.modelSel == 'Maxent'",
                imageOutput('maxentEvalPlot')
              ),
              conditionalPanel(
                "input.tabs == 'espace' && input.espaceSel == 'pca'",
                pca_resultsUI("cEspace_PCA_uiID")
              ),
              conditionalPanel(
                "input.tabs == 'espace' && input.espaceSel == 'occDens'",
                occDens_resultsUI("cEspace_occDens_uiID")
              ),
              conditionalPanel(
                "input.tabs == 'espace' && input.espaceSel == 'nicheOv'",
                nicheOv_resultsUI("cEspace_nicheOv_uiID")
              )
            ),
            tabPanel(
              'Lambdas',
              verbatimTextOutput('lambdas')
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
                  column(3, h5("No download data available in this component"))
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
            )
          )
        )
      )
    )
  )
)
