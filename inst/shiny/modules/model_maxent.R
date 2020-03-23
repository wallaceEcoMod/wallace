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
                                choices = list("L", "LQ", "H", "LQH", "LQHP"),
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
  curModel <- common$curModel

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
                            em(spName(sp)), ".")
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
                                 logger,
                                 spN = sp)
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
})

  output$evalTbls <- renderUI({
    req(spp[[curSp()]]$rmm$model$algorithm)
    if (spp[[curSp()]]$rmm$model$algorithm == "maxnet" |
        spp[[curSp()]]$rmm$model$algorithm == "maxent.jar") {
      req(spp[[curSp()]]$evalOut)
      res <- spp[[curSp()]]$evalOut@results
      res.grp <- spp[[curSp()]]$evalOut@results.grp
      tuned.n <- ncol(spp[[curSp()]]$evalOut@tune.settings)
      if(tuned.n > 0) {
        res.round <- cbind(res[,seq(1, tuned.n)],
                           round(res[,seq(tuned.n+1, ncol(res))], digits = 3))
        res.grp.round <- cbind(res.grp[,seq(1, tuned.n+1)],
                               round(res.grp[,seq(tuned.n+2, ncol(res.grp))],
                                     digits = 3))
      } else {
        res.round <- cbind(round(res[, 1:13], digits = 3))
        res.grp.round <- cbind(fold = res.grp[, 1],
                               round(res.grp[, 2:5], digits = 3))
      }
      # define contents for both evaluation tables
      options <- list(scrollX = TRUE, sDom  = '<"top">rtp<"bottom">')
      output$evalTbl <- DT::renderDataTable(res.round, options = options)
      output$evalTblBins <- DT::renderDataTable(res.grp.round, options = options)
      output$lambdas <- renderPrint({
        req(spp[[curSp()]]$evalOut)
        if(spp[[curSp()]]$rmm$model$algorithm == "maxnet") {
          spp[[curSp()]]$evalOut@models[[curModel()]]$betas
        } else if(spp[[curSp()]]$rmm$model$algorithm == "maxent.jar") {
          spp[[curSp()]]$evalOut@models[[curModel()]]@lambdas
        }
      })

      tabsetPanel(
        tabPanel("Evaluation",
                 tagList(br(),
                         span("Evaluation statistics: full model and partition averages",
                              class = "stepText"), br(), br(),
                         DT::dataTableOutput(session$ns('evalTbl')), br(),
                         span("Evaluation statistics: individual partitions",
                              class = "stepText"), br(), br(),
                         DT::dataTableOutput(session$ns('evalTblBins')))
        ),
        tabPanel("Lambdas",
                 br(),
                 span("Maxent Lambdas File", class = "stepText"), br(), br(),
                 verbatimTextOutput(session$ns("lambdas"))
        )
      )
    }
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
  uiOutput(ns('evalTbls'))
}

model_maxent_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    module_knit = species$rmm$code$wallaceSettings$someFlag,
    var1 = species$rmm$code$wallaceSettings$someSetting1,
    var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}
