model_maxent_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    htmlOutput('maxentJar'), br(), "(",
    HTML("<font color='blue'><b>NOTE</b></font>"),
    ": see module guidance for troubleshooting tips if you are experiencing problems.)",
    tags$hr(),
    strong("Select algorithm"), br(),
    tags$div(title = 'text',
             radioButtons(ns("algMaxent"), label = '',
                          choices = list("maxnet", "maxent.jar"), inline = TRUE)),
    strong("Select feature classes "),
    strong(em("(flexibility of modeled response)")), br(),
    "key: ", strong("L"), "inear, ", strong("Q"), "uadratic, ",
    strong("H"), "inge, ", strong("P"), "roduct, ", strong("T"), "hreshold",
    tags$div(title = paste0('Feature combinations to be explored. Features are ',
                            'constructed using different relationships within and ',
                            'among the environmental predictors, and are used to ',
                            'constrain the computed probability distribution. ',
                            'In short, more features = more potential model ',
                            'complexity.'),
             checkboxGroupInput(ns("fcs"), label = '',
                                choices = list("L", "LQ", "H", "LQH", "LQHP",
                                               "LQHPT"),
                                inline = TRUE,
                                selected = c("L", "LQ"))), # Check default (no selected param)
    strong("Select regularization multipliers "),
    strong(em("(penalty against complexity)")),
    tags$div(title = paste0('Range of regularization multipliers to explore. ',
                            'Greater values of the regularization multiplier lead ',
                            'to increased penalty against overly complex and/or ',
                            'overfit models. A value of 0 results in no ',
                            'regularization.'),
             sliderInput(ns("rms"), label = "",
                         min = 0.5, max = 10, step = 0.5, value = c(1, 2))),
    tags$div(title = paste0('Value used to step through regularization multiplier ',
                            'range (e.g. range of 1-3 with step 0.5 results in ',
                            '[1, 1.5, 2, 2.5, 3]).'),
             numericInput(ns("rmsStep"), label = "Multiplier step value",
                          value = 1)),
    strong("Are you using a categorical variable? (**)"),
    tags$div(title = '',
             selectInput(ns("categSel"), label = '',
                         choices = list("NO", "YES")),
             conditionalPanel(sprintf("input['%s'] == 'YES'", ns("categSel")),
                              uiOutput('catEnvs'))),
    strong("Clamping?"),
    tags$div(title = 'Clamp model predictions?',
             selectInput(ns("clamp"), label = '',
                         choices = list("", "TRUE", "FALSE"))),
    checkboxInput(ns("batch"), label = strong("Batch"), value = T), # Check default (value = FALSE)
    actionButton(ns("goMaxent"), "Run")
  )
}

model_maxent_module_server <- function(input, output, session, common) {

  allSp <- common$allSp
  curSp <- common$curSp
  spp <- common$spp
  logger <- common$logger

  observe({
    if(input$algMaxent == "maxnet") {
      updateSelectInput(session, "clamp", selected = "TRUE") # Check default (selected = "")
      shinyjs::enable("clamp")
    } else {
      updateSelectInput(session, "clamp", selected = "TRUE")
      shinyjs::disable("clamp")
    }
  })

  observeEvent(input$goMaxent, {
    if(is.null(input$fcs)) {
      logger %>% writeLog(type = 'error', "No feature classes selected.")
      return()
    }
    if(input$clamp == "") {
      logger %>% writeLog(type = 'error', "Please specify clamping setting.")
      return()
    }

    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()

    # PROCESSING ####
    for(sp in spLoop) {
      # ERRORS ####
      if (is.null(spp[[sp]]$occs$partition)) {
        logger %>% writeLog(type = 'error', "Before building a model, please
                                partition occurrences for cross-validation for ",
                            spName(spp[[sp]]), ".")
        return()
      }

      # Define vector of categorical variables if they exits
      if (input$categSel == 'NO') {
        catEnvs <- NULL
      } else if (input$categSel == 'YES') {
        catEnvs <- selCatEnvs()
      }
      # FUNCTION CALL ####
      res.maxent <- model_maxent(spp[[sp]]$occs,
                                 spp[[sp]]$bg,
                                 spp[[sp]]$occs$partition,
                                 spp[[sp]]$bg$partition,
                                 spp[[sp]]$procEnvs$bgMask,
                                 input$rms,
                                 input$rmsStep,
                                 input$fcs,
                                 input$clamp,
                                 input$algMaxent,
                                 catEnvs,
                                 logger)
      req(res.maxent)

      # LOAD INTO SPP ####
      spp[[sp]]$evalOut <- res.maxent

      # METADATA ####
      spp[[sp]]$rmm$model$algorithm <- input$algMaxent
      spp[[sp]]$rmm$model$maxent$featureSet <- input$fcs
      spp[[sp]]$rmm$model$maxent$regularizationMultiplierSet <- input$rms
      spp[[sp]]$rmm$model$maxent$regularizationRule <- paste("increment by",
                                                             input$rmsStep)
      spp[[sp]]$rmm$model$maxent$clamping <- input$clamp
      if(input$algMaxent == "maxent.jar") {
        ver <- paste("Maxent", maxentJARversion(), "via dismo",
                     packageVersion('dismo'))
      }
      if(input$algMaxent == "maxnet") {
        ver <- paste("maxnet", packageVersion('maxnet'))
      }
      spp[[sp]]$rmm$model$maxent$algorithmNotes <- ver
    }
    common$update_component(tab = "Results")
    common$remove_module(component = "vis", module = "vis_bioclimPlot")
})

  output$maxentPrint <- renderPrint({
    req(spp[[curSp()]]$evalOut)
    spp[[curSp()]]$evalOut
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))

}

model_maxent_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  verbatimTextOutput(ns("maxentPrint"))
}

model_maxent_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    module_knit = species$rmm$code$wallaceSettings$someFlag,
    var1 = species$rmm$code$wallaceSettings$someSetting1,
    var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}

