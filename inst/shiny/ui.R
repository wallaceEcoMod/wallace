resourcePath <- system.file("shiny", "www", package = "wallace")
shiny::addResourcePath("wallaceres", resourcePath)

tagList(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(
    script = file.path("wallaceres", "js", "shinyjs-funcs.js"),
    functions = c("scrollLogger", "disableModule", "enableModule")
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
    tabPanel("Transfer", value = 'proj'),
    tabPanel("Reproduce", value = 'rep'),
    navbarMenu("Support", icon = icon("life-ring"),
               HTML('<a href="https://wallaceecomod.github.io/" target="_blank">Wallace Homepage</a>'),
               HTML('<a href="https://groups.google.com/g/wallaceEcoMod" target="_blank">Google Group</a>'),
               HTML('<a href="https://github.com/wallaceEcoMod/wallace/issues" target="_blank">GitHub Issues</a>'),
               HTML('<a href="mailto: WallaceEcoMod@gmail.com" target="_blank">Send Email</a>')),
    tabPanel(NULL, icon = icon("power-off"), value = "_stopapp")
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
            div("Component: Obtain Occurrence Data", class = "componentName"),
            help_comp_ui("occsHelp"),
            radioButtons(
              "occsSel", "Modules Available:",
              choices = insert_modules_options("occs"),
              selected = character(0)
            ),
            tags$hr(),
            insert_modules_ui("occs")
          ),
          # OBTAIN ENVS ####
          conditionalPanel(
            "input.tabs == 'envs'",
            div("Component: Obtain Environmental Data", class = "componentName"),
            help_comp_ui("envsHelp"),
            radioButtons(
              "envsSel", "Modules Available:",
              choices = insert_modules_options("envs"),
              selected = character(0)
            ),
            tags$hr(),
            insert_modules_ui("envs")
          ),
          # PROCESS OCCS ####
          conditionalPanel(
            "input.tabs == 'poccs'",
            div("Component: Process Occurrence Data", class = "componentName"),
            help_comp_ui("poccsHelp"),
            radioButtons(
              "poccsSel", "Modules Available:",
              choices = insert_modules_options("poccs"),
              selected = character(0)
              #"Profile Occurrences" = "profOccs"), # CM
            ),
            tags$hr(),
            insert_modules_ui("poccs")
            # CM: start comment
            # conditionalPanel("input.poccsSel == 'profOccs'",
            #                  uiTop(profileOccs_INFO),
            #                  actionButton("goProfileOccs", "Profile Occurrences"), br(), br(),
            #                  profileOccs_UI('poccs_profileOccs_uiID'),
            #                  actionButton("goProfileOccsClean", "Clean Occurrences"),
            #                  uiBottom(profileOccs_INFO)
            # ),
            # CM: End comment
          ),
          conditionalPanel(
            "input.tabs == 'penvs'",
            div("Component: Process Environmental Data", class = "componentName"),
            help_comp_ui("penvsHelp"),
            radioButtons(
              "penvsSel", "Modules Available:",
              choices = insert_modules_options("penvs"),
              selected = character(0)
            ),
            tags$hr(),
            insert_modules_ui("penvs")
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
            div("Component: Characterize Environmental Space", class = "componentName"),
            help_comp_ui("espaceHelp"),
            radioButtons(
              "espaceSel", "Modules Available:",
              choices = insert_modules_options("espace"),
              selected = character(0)
            ),
            tags$hr(),
            insert_modules_ui("espace")
          ),
          # PARTITION ####
          conditionalPanel(
            "input.tabs == 'part'",
            div("Component: Partition Occurrence Data", class = "componentName"),
            help_comp_ui("partHelp"),
            radioButtons(
              "partSel", "Modules Available:",
              choices = insert_modules_options("part"),
              selected = character(0)
            ),
            tags$hr(),
            insert_modules_ui("part")
          ),
          # MODEL ####
          conditionalPanel(
            "input.tabs == 'model'",
            div("Component: Build and Evaluate Niche Model", class = "componentName"),
            help_comp_ui("modelHelp"),
            radioButtons("modelSel", "Modules Available:",
                         choices = insert_modules_options("model"),
                         selected = character(0)),
            tags$hr(),
            insert_modules_ui("model")
          ),
          # VISUALIZE ####
          conditionalPanel(
            "input.tabs == 'vis'",
            div("Component: Visualize Model Results", class = "componentName"),
            help_comp_ui("visHelp"),
            radioButtons("visSel", "Modules Available:",
                         choices = insert_modules_options("vis"),
                         selected = character(0)),
            tags$hr(),
            insert_modules_ui("vis")
          ),
          # PROJECT ####
          conditionalPanel(
            "input.tabs == 'proj'",
            div("Component: Model Transfer", class = "componentName"),
            help_comp_ui("projHelp"),
            radioButtons(
              "projSel", "Modules Available:",
              choices = insert_modules_options("proj"),
              selected = character(0)),
            tags$hr(),
            insert_modules_ui("proj")
          ),
          # REPRODUCIBILITY
          conditionalPanel(
            "input.tabs == 'rep'",
            div("Component: Reproduce", class = "componentName"),
            radioButtons(
              "repSel", "Modules Available:",
              choices = insert_modules_options("rep"),
              selected = character(0)
            ),
            tags$hr(),
            insert_modules_ui("rep")
          )
        )
      ),
      # --- RESULTS WINDOW ---
      column(
        8,
        conditionalPanel(
          "input.tabs != 'intro' & input.tabs != 'rep'",
          fixedRow(
            column(
              4,
              absolutePanel(
                div(style = "margin-top: -10px"),
                uiOutput("curSpUI"),
                div(style = "margin-top: -12px"),
                uiOutput("curModelUI")
              )
            ),
            column(
              2,
              offset = 1,
              align = "left",
              div(style = "margin-top: -10px"),
              strong("Window log"),
              div(style = "margin-top: 5px"),
              div(
                id = "wallaceLog",
                div(id = "logHeader", div(id = "logContent"))
              )
            )
          )
        ),
        br(),
        conditionalPanel(
          "input.tabs != 'intro' & input.tabs != 'rep'",
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
              'Occurrences', br(),
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
              'Component Guidance', icon = icon("info-circle"),
              uiOutput('gtext_component')
            ),
            tabPanel(
              'Module Guidance', icon = icon("info-circle", class = "mod_icon"),
              uiOutput('gtext_module')
            ),
            tabPanel(
              'Save', icon = icon("save", class = "save_icon"),
              br(),
              h5(em("Note: To save your session code or metadata, use the Reproduce component")),
              wellPanel(
                h4(strong("Save Session")),
                p(paste0("By saving your session into an RDS file, you can resume ",
                       "working on it at a later time or you can share the file",
                       " with a collaborator.")),
                shinyjs::hidden(p(
                  id = "save_warning",
                  icon("exclamation-triangle"),
                  paste0("The current session data is large, which means the ",
                         "downloaded file may be large and the download might",
                         " take a long time.")
                  )),
                downloadButton("save_session", "Save Session"),
                br()
              ),
              wellPanel(
                h4(strong("Download Data")),
                p(paste0("Download data/results from analyses from currently selected module")),
                ## save module data BEGIN ##
                # save occs #
                conditionalPanel(
                  "input.tabs == 'occs'",
                  br(),
                  fluidRow(
                    column(3, h5("Download original occurrence data")),
                    column(2, shinyjs::disabled(downloadButton('dlDbOccs', "CSV file")))
                  ),
                  br(),
                  fluidRow(
                    column(3, h5("Download current table")),
                    column(2, shinyjs::disabled(downloadButton('dlOccs', "CSV file")))
                  ),
                  br(),
                  fluidRow(
                    column(3, h5("Download all data")),
                    column(2, shinyjs::disabled(downloadButton('dlAllOccs', "CSV file")))
                  )
                ),
                # save envs #
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
                # save poccs #
                conditionalPanel(
                  "input.tabs == 'poccs'",
                  br(),
                  fluidRow(
                    column(3, h5("Download processed occurence table")),
                    column(2, shinyjs::disabled(downloadButton('dlProcOccs', "CSV file")))
                  )
                ),
                # save penvs #
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
                    column(2, shinyjs::disabled(downloadButton('dlBgPts', "CSV file")))
                  )
                ),
                # save espace #
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
                # save part #
                conditionalPanel(
                  "input.tabs == 'part'",
                  br(),
                  fluidRow(
                    column(3, h5("Download occurrence and background localities with partition values")),
                    column(2, shinyjs::disabled(downloadButton('dlPart', "CSV file")))
                  )
                ),
                # save model #
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
                # save vis #
                conditionalPanel(
                  "input.tabs == 'vis'",
                  br(),
                  fluidRow(
                    column(3, h5("Download Bioclim plot")),
                    column(2, shinyjs::disabled(downloadButton('dlVisBioclim', "PNG file")))
                  ),
                  br(),
                  fluidRow(
                    column(3, h5("Download Maxent plots")),
                    column(2, shinyjs::disabled(downloadButton('dlMaxentPlots', "ZIP file")))
                  ),
                  br(),
                  fluidRow(
                    column(3, h5("Download Response plots")),
                    column(2, shinyjs::disabled(downloadButton('dlRespCurves', "ZIP file")))
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
                    column(2, shinyjs::disabled(downloadButton('dlPred', "Prediction file")))
                  )
                ),
                # save proj #
                conditionalPanel(
                  "input.tabs == 'proj'",
                  br(),
                  fluidRow(
                    column(3, h5("Download shapefile of projection extent")),
                    column(2, shinyjs::disabled(downloadButton('dlPjShp', "ZIP file")))
                  ),
                  br(),
                  fluidRow(
                    column(3, h5("Download projected environmental variables (Select download file type)")),
                    column(2, selectInput('projEnvsFileType',
                                          label = NULL,
                                          choices = list("GeoTIFF" = 'GTiff',
                                                         "GRD" = 'raster',
                                                         "ASCII" = 'ascii'))),
                    column(2, shinyjs::disabled(downloadButton('dlProjEnvs', "ZIP file")))
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
                    column(2, shinyjs::disabled(downloadButton('dlProj', "Projection file")))
                  ),
                  br(),
                  fluidRow(
                    column(3, h5("Download MESS (Select download file type**)")),
                    column(2, selectInput('messFileType',
                                          label = NULL,
                                          choices = list("GeoTIFF" = 'GTiff',
                                                         "GRD" = 'raster',
                                                         "ASCII" = 'ascii',
                                                         "PNG" = 'png'))),
                    column(2, shinyjs::disabled(downloadButton('dlMess', "MESS file")))
                  )
                )
              )
            )
          )
        ),
        ## save module data END ##
        conditionalPanel(
          "input.tabs == 'rep' & input.repSel == 'rep_markdown'",
          column(8,
                 includeMarkdown("modules/rep_markdown.Rmd")
          )
        ),
        conditionalPanel(
          "input.tabs == 'rep' & input.repSel == 'rep_rmms'",
          column(8,
                 includeMarkdown("modules/rep_rmms.Rmd")
          )
        ),
        conditionalPanel(
          "input.tabs == 'rep' & input.repSel == 'rep_refPackages'",
          column(8,
                 includeMarkdown("modules/rep_refPackages.Rmd")
          )
        ),
        conditionalPanel(
          "input.tabs == 'intro'",
          tabsetPanel(
            id = 'introTabs',
            tabPanel(
              'About', #note this used to be named 'intro' so the files will be named 'intro'
              includeMarkdown("Rmd/text_intro.Rmd")
            ),
            tabPanel(
              'Team', #note this used to be named 'about' so the files will be named 'about'
              fluidRow(
                column(8, includeMarkdown("Rmd/text_about.Rmd")
                )
              )
            ),
            tabPanel(
              'How To Use',
              includeMarkdown("Rmd/text_how_to_use.Rmd")
            ),
            tabPanel(
              'Load Prior Session',
              h4("Load session"),
              h5("If you saved a prior session of Wallace (RDS file), you can load it here."),
              fileInput("load_session", "", accept = ".rds"),
              actionButton('goLoad_session', 'Load RDS')
            )
          )
        )
      )
    )
  )
)
