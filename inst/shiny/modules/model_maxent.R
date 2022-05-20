model_maxent_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    htmlOutput('maxentJar'), "(",
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
    strong("H"), "inge, ", strong("P"), "roduct",
    tags$div(title = paste0('Feature combinations to be explored. Features are ',
                            'constructed using different relationships within and ',
                            'among the environmental predictors, and are used to ',
                            'constrain the computed probability distribution. ',
                            'In short, more features = more potential model ',
                            'complexity.'),
             checkboxGroupInput(ns("fcs"), label = '',
                                choices = list("L", "LQ", "H", "LQH", "LQHP"),
                                inline = TRUE)), # Check default (no selected param)
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
    strong("Are you using a categorical variable?"),
    tags$div(title = '',
             selectInput(ns("categSel"), label = '',
                         choices = list("NO", "YES")),
             conditionalPanel(sprintf("input['%s'] == 'YES'", ns("categSel")),
                              uiOutput('catEnvs'))),
    strong("Clamping?"),
    tags$div(title = 'Clamp model predictions?',
             selectInput(ns("clamp"), label = '',
                         choices = list("None selected" = '',
                                        "TRUE" = "TRUE",
                                        "FALSE" = "FALSE"))),
    strong("Parallel?"),
    tags$div(
      title = 'Use parallel option for quicker analysis?',
      selectInput(ns("parallel"), label = '',
                  choices = list("None selected" = '',
                                 "TRUE" = "TRUE",
                                 "FALSE" = "FALSE")),
      conditionalPanel(
        sprintf("input['%s'] == 'TRUE'", ns("parallel")),
        numericInput(
          ns("numCores"),
          label = paste0("Specify the number of cores (max. ", parallel::detectCores(), ")"),
          value = parallel::detectCores() - 1, min = 1,
          max = parallel::detectCores(), step = 1
        ))),
    tags$div(
      title = "Apply selection to ALL species loaded",
      checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE) # Check default (value = FALSE)
    ),
    actionButton(ns("goMaxent"), "Run")
  )
}

model_maxent_module_server <- function(input, output, session, common) {

  allSp <- common$allSp
  curSp <- common$curSp
  spp <- common$spp
  logger <- common$logger
  curModel <- common$curModel
  selCatEnvs <- common$selCatEnvs

  updateSelectInput(session, "clamp", selected = "") # Check default (selected = "")

  observeEvent(input$goMaxent, {
    if(is.null(input$fcs)) {
      logger %>% writeLog(type = 'error', "No feature classes selected.")
      return()
    }
    if(input$clamp == "") {
      logger %>% writeLog(type = 'error', "Please specify clamping setting.")
      return()
    }
    if(input$parallel == "") {
      logger %>% writeLog(type = 'error', "Please specify parallel setting.")
      return()
    }
    if(input$rmsStep <= 0) {
      logger %>% writeLog(type = 'error', "Please specify a positive multiplier step value that is greater than 0.")
      return()
    }

    # loop over all species if batch is on
    if (input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()

    # PROCESSING ####
    for(sp in spLoop) {
      # ERRORS ####
      if (is.null(spp[[sp]]$occs$partition)) {
        logger %>% writeLog(type = 'error', hlSpp(sp),
                            "Before building a model, please partition ",
                            "occurrences for cross-validation.")
        return()
      }

      # Define vector of categorical variables if they exits
      if (input$categSel == 'NO') {
        catEnvs <- NULL
      } else if (input$categSel == 'YES') {
        catEnvs <- selCatEnvs()
      }
      user_grp <- list(occs.grp = spp[[sp]]$occs$partition,
                       bg.grp = spp[[sp]]$bg$partition)
      # FUNCTION CALL ####
      res.maxent <- model_maxent(spp[[sp]]$occs,
                                 spp[[sp]]$bg,
                                 user_grp,
                                 spp[[sp]]$procEnvs$bgMask,
                                 input$rms,
                                 input$rmsStep,
                                 input$fcs,
                                 as.logical(input$clamp),
                                 input$algMaxent,
                                 catEnvs,
                                 input$parallel,
                                 input$numCores,
                                 logger,
                                 spN = sp)
      req(res.maxent)

      # LOAD INTO SPP ####
      spp[[sp]]$evalOut <- res.maxent

      # METADATA ####
      # Metadata obtained from ENMeval RMM object
      spp[[sp]]$rmm$model$algorithm <- res.maxent@rmm$model$algorithm
      spp[[sp]]$rmm$model$tuneSettings <- res.maxent@rmm$model$tuneSettings
      spp[[sp]]$rmm$assessment <- res.maxent@rmm$assessment
      # Overwrite metadata
      spp[[sp]]$rmm$model$algorithms <- input$algMaxent
      spp[[sp]]$rmm$model$algorithm$maxent$clamping <- as.logical(input$clamp)
      spp[[sp]]$rmm$model$algorithm$maxent$regularizationMultiplierSet <- input$rms
      spp[[sp]]$rmm$model$algorithm$maxent$featureSet <- input$fcs
      spp[[sp]]$rmm$model$algorithm$maxent$regularizationRule <- paste("increment by",
                                                             input$rmsStep)
      spp[[sp]]$rmm$model$algorithm$maxent$categorical <- catEnvs
      spp[[sp]]$rmm$model$algorithm$maxent$parallel <- input$parallel
      spp[[sp]]$rmm$model$algorithm$maxent$nCores <- input$numCores

    }
    # REFERENCES
    if (input$algMaxent == "maxent.jar") knitcitations::citep(citation("dismo"))
    if (input$algMaxent == "maxnet") knitcitations::citep(citation("maxnet"))
    knitcitations::citep(citation("ENMeval", auto = TRUE))

    common$update_component(tab = "Results")
})

  output$evalTbls <- renderUI({
    req(spp[[curSp()]]$rmm$model$algorithms)
    if (spp[[curSp()]]$rmm$model$algorithms == "maxnet" |
        spp[[curSp()]]$rmm$model$algorithms == "maxent.jar") {
      req(spp[[curSp()]]$evalOut)
      res <- spp[[curSp()]]$evalOut@results
      res.grp <- spp[[curSp()]]$evalOut@results.partitions
      tuned.n <- ncol(spp[[curSp()]]$evalOut@tune.settings)
      if(tuned.n > 0) {
        res.round <- cbind(res[,seq(1, tuned.n)],
                           round(res[,seq(tuned.n+1, ncol(res))], digits = 3))
        res.grp.round <- cbind(res.grp[, 1:2],
                               round(res.grp[, 3:6], digits = 3))
      } else {
        res.round <- cbind(round(res[, 1:13], digits = 3))
        res.grp.round <- cbind(fold = res.grp[, 1],
                               round(res.grp[, 2:6], digits = 3))
      }
      # define contents for both evaluation tables
      options <- list(scrollX = TRUE, sDom  = '<"top">rtp<"bottom">')
      output$evalTbl <- DT::renderDataTable(res.round, options = options)
      output$evalTblBins <- DT::renderDataTable(res.grp.round, options = options)
      output$lambdas <- renderPrint({
        req(spp[[curSp()]]$evalOut)
        if(spp[[curSp()]]$rmm$model$algorithms == "maxnet") {
          spp[[curSp()]]$evalOut@models[[curModel()]]$betas
        } else if(spp[[curSp()]]$rmm$model$algorithms == "maxent.jar") {
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
      list(
        algMaxent = input$algMaxent,
        fcs = input$fcs,
        rms = input$rms,
        rmsStep = input$rmsStep,
        categSel = input$categSel,
        clamp = input$clamp,
        parallel = input$parallel,
        numCores = input$numCores
      )
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      updateRadioButtons(session, "algMaxent", selected = state$algMaxent)
      updateCheckboxGroupInput(session, "fcs", selected = state$fcs)
      updateSliderInput(session, "rms", value = state$rms)
      updateNumericInput(session, "rmsStep", value = state$rmsStep)
      updateSelectInput(session, "categSel", selected = state$categSel)
      updateSelectInput(session, "clamp", selected = state$clamp)
      updateSelectInput(session, "parallel", selected = state$parallel)
      updateNumericInput(session, "numCores", value = state$numCores)
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
    model_maxent_knit =
      if (!is.null(species$rmm$model$algorithms)) {
      species$rmm$model$algorithms != "BIOCLIM"
      } else {FALSE},
    rms_rmd =  printVecAsis(species$rmm$model$algorithm$maxent$regularizationMultiplierSet),
    rmsStep_rmd =  gsub("increment by", "", species$rmm$model$algorithm$maxent$regularizationRule),
    fcs_rmd = printVecAsis(species$rmm$model$algorithm$maxent$featureSet),
    clampSel_rmd = species$rmm$model$algorithm$maxent$clamping,
    algMaxent_rmd = species$rmm$model$algorithms,
    parallel_rmd = species$rmm$model$algorithm$maxent$parallel,
    numCores_rmd = print(species$rmm$model$algorithm$maxent$nCores),
    cat_envs_knit = !is.null(species$rmm$model$algorithm$maxent$categorical),
    catEnvs_rmd =  if(!is.null(species$rmm$model$algorithm$maxent$categorical)){species$rmm$model$algorithm$maxent$categorical} else {NULL},
    catEnvsNum_rmd = if(!is.null(species$rmm$model$algorithm$maxent$categorical)){
      length(species$rmm$model$algorithm$maxent$categorical)} else {0}
    )
}

