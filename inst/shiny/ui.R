resourcePath <- system.file("shiny", "www", package = "wallace")
shiny::addResourcePath("wallaceres", resourcePath)

tagList(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(
    script = file.path("wallaceres", "js", "shinyjs-funcs.js"),
    functions = c("scrollLogger", "disableModule", "enableModule")
  ),
  navbarPage(
    theme = bslib::bs_theme(version = 3,
                            bootswatch = "united"),
    id = 'tabs',
    collapsible = TRUE,
    header = tagList(
      tags$head(tags$link(href = "css/styles.css", rel = "stylesheet"))
    ),
    title = img(src = "logo.png", height = '50', width = '50',
                style = "margin-top: -15px"),
    windowTitle = "#WallaceEcoMod",
    tabPanel("Intro", value = 'intro'),
    tabPanel("Occ Data", value = 'occs'),
    tabPanel("Env Data", value = 'envs'),
    tabPanel("Process Occs", value = 'poccs'),
    tabPanel("Process Envs", value = 'penvs'),
    tabPanel("Env Space", value = 'espace'),
    tabPanel("Partition Occs", value = 'part'),
    tabPanel("Model", value = 'model'),
    tabPanel("Visualize", value = 'vis'),
    tabPanel("Transfer", value = 'xfer'),
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
            ),
            tags$hr(),
            insert_modules_ui("poccs")
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
          # TRANSFER ####
          conditionalPanel(
            "input.tabs == 'xfer'",
            div("Component: Model Transfer", class = "componentName"),
            help_comp_ui("xferHelp"),
            radioButtons(
              "xferSel", "Modules Available:",
              choices = insert_modules_options("xfer"),
              selected = character(0)),
            tags$hr(),
            insert_modules_ui("xfer")
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
              strong("Log window"),
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
              'Component Guidance', icon = icon("circle-info"),
              uiOutput('gtext_component')
            ),
            tabPanel(
              'Module Guidance', icon = icon("circle-info", class = "mod_icon"),
              uiOutput('gtext_module')
            ),
            tabPanel(
              'Save', icon = icon("floppy-disk", class = "save_icon"),
              br(),
              h5(em("Note: To save your session code or metadata, use the Reproduce component")),
              wellPanel(
                h4(strong("Save Session")),
                p(paste0("By saving your session into an RDS file, you can resume ",
                       "working on it at a later time or you can share the file",
                       " with a collaborator.")),
                shinyjs::hidden(p(
                  id = "save_warning",
                  icon("triangle-exclamation"),
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
                # save xfer #
                conditionalPanel(
                  "input.tabs == 'xfer'",
                  br(),
                  fluidRow(
                    column(3, h5("Download shapefile of extent of transfer")),
                    column(2, shinyjs::disabled(downloadButton('dlXfShp', "ZIP file")))
                  ),
                  br(),
                  fluidRow(
                    column(3, h5("Download environmental variables of transfer (Select download file type)")),
                    column(2, selectInput('xferEnvsFileType',
                                          label = NULL,
                                          choices = list("GeoTIFF" = 'GTiff',
                                                         "GRD" = 'raster',
                                                         "ASCII" = 'ascii'))),
                    column(2, shinyjs::disabled(downloadButton('dlXferEnvs', "ZIP file")))
                  ),
                  br(),
                  fluidRow(
                    column(3, h5("Download transfer (Select download file type**)")),
                    column(2, selectInput('xferFileType',
                                          label = NULL,
                                          choices = list("GeoTIFF" = 'GTiff',
                                                         "GRD" = 'raster',
                                                         "ASCII" = 'ascii',
                                                         "PNG" = 'png'))),
                    column(2, shinyjs::disabled(downloadButton('dlXfer', "Transfer file")))
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
          "input.tabs == 'rep' & input.repSel == null",
          column(8,
                 includeMarkdown("Rmd/gtext_rep.Rmd")
          )
        ),
        conditionalPanel(
          "input.tabs == 'rep' & input.repSel == 'rep_markdown'",
          column(8,
                 includeMarkdown("modules/rep_markdown.md")
          )
        ),
        conditionalPanel(
          "input.tabs == 'rep' & input.repSel == 'rep_rmms'",
          column(8,
                 includeMarkdown("modules/rep_rmms.md")
          )
        ),
        conditionalPanel(
          "input.tabs == 'rep' & input.repSel == 'rep_refPackages'",
          column(8,
                 includeMarkdown("modules/rep_refPackages.md")
          )
        ),
        conditionalPanel(
          "input.tabs == 'intro'",
          tabsetPanel(
            id = 'introTabs',
            tabPanel(
              'About',
              includeMarkdown("Rmd/text_about.Rmd")
            ),
            tabPanel(
              'Team',
              fluidRow(
                column(8, includeMarkdown("Rmd/text_team.Rmd")
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
              includeMarkdown("Rmd/text_loadsesh.Rmd"),
              fileInput("load_session", "", accept = ".rds"),
              actionButton('goLoad_session', 'Load RDS')
            )
          )
        )
      )
    )
  )
)
