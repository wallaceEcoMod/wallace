
runMaxent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    strong("Select algorithm"), br(),
    tags$div(title = 'text',
             radioButtons(ns("algMaxent"), label='',
                          choices = list("maxnet", "maxent.jar"), inline = TRUE)),
    strong("Select feature classes "), strong(em("(flexibility of modeled response)")), br(),
    "key: ", strong("L"), "inear, ", strong("Q"), "uadratic, ", strong("H"), "inge, ", strong("P"), "roduct, ", strong("T"), "hreshold",
    tags$div(title='Feature combinations to be explored. Features are constructed using different relationships within and among the environmental predictors, and are used to constrain the computed probability distribution. In short, more features = more potential model complexity.',
             checkboxGroupInput(ns("fcs"), label='',
                                choices = list("L", "LQ", "H", "LQH", "LQHP", "LQHPT"), inline = TRUE)),
    strong("Select regularization multipliers "), strong(em("(penalty against complexity)")),
    tags$div(title='Range of regularization multipliers to explore. Greater values of the regularization multiplier lead to increased penalty against overly complex and/or overfit models. A value of 0 results in no regularization.',
             sliderInput(ns("rms"), label = "",
                         min = 0.5, max = 10, step=0.5, value = c(1, 2))),
    tags$div(title='Value used to step through regularization multiplier range (e.g. range of 1-3 with step 0.5 results in [1, 1.5, 2, 2.5, 3]).',
             numericInput(ns("rmsStep"), label = "Multiplier step value", value = 1)),
    strong("Clamping?"), tags$div(title = 'Clamp model predictions?',
                                  selectInput(ns("clamp"), label='', choices = list("", "TRUE", "FALSE"), selected = "")),
    checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE)
  )
}

runMaxent_MOD <- function(input, output, session) {
  observe({
    if(input$algMaxent == "maxnet") {
      updateSelectInput(session, "clamp", selected = "")
      shinyjs::enable("clamp")
    } else {
      updateSelectInput(session, "clamp", selected = "TRUE")  
      shinyjs::disable("clamp")
    }
  })
  
  reactive({
    
    if(is.null(input$fcs)) {
      shinyLogs %>% writeLog(type = 'error', "No feature classes selected.")
      return()
    }
    if(input$clamp == "") {
      shinyLogs %>% writeLog(type = 'error', "Please specify clamping setting.")
      return()
    }
    
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()
    
    # PROCESSING ####
    for(sp in spLoop) {
      # ERRORS ####
      if (is.null(spp[[sp]]$occs$partition)) {
        shinyLogs %>% writeLog(type = 'error', "Before building a model, please
                                partition occurrences for cross-validation for ",
                                spName(spp[[sp]]), ".")
        return()
      }
      # FUNCTION CALL ####
      res.maxent <- runMaxent(spp[[sp]]$occs, 
                              spp[[sp]]$bg, 
                              spp[[sp]]$occs$partition,
                              spp[[sp]]$bg$partition,
                              spp[[sp]]$procEnvs$bgMask, 
                              input$rms, 
                              input$rmsStep, 
                              input$fcs, 
                              input$clamp,
                              input$algMaxent,
                              shinyLogs)
      req(res.maxent)
      
      # LOAD INTO SPP ####
      spp[[sp]]$results <- res.maxent
      
      # METADATA ####
      spp[[sp]]$rmm$model$algorithm <- input$algMaxent
      spp[[sp]]$rmm$model$maxent$featureSet <- input$fcs
      spp[[sp]]$rmm$model$maxent$regularizationMultiplierSet <- input$rms
      spp[[sp]]$rmm$model$maxent$regularizationRule <- paste("increment by", input$rmsStep)
      spp[[sp]]$rmm$model$maxent$clamping <- input$clamp
      if(input$algMaxent == "maxent.jar") {
        ver <- paste("Maxent", maxentJARversion(), "via dismo", packageVersion('dismo'))
      }
      if(input$algMaxent == "maxnet") {
        ver <- paste("maxnet", packageVersion('maxnet'))
      }
      spp[[sp]]$rmm$model$maxent$algorithmNotes <- ver
      
    }
  })
}

runMaxent_INFO <- infoGenerator(modName = "Maxent",
                             modAuts = "Jamie M. Kass, Robert Muscarella, Bruno
                             Vilela, Gonzalo E. Pinilla-Buitrago, Robert P. Anderson",
                             pkgName = c("ENMeval", "dismo", "maxnet"))

runMaxent_TBL <- function(input, output, session) {
  req(results())
  output$evalTbls <- renderUI({
    tabsetPanel(
      tabPanel("Evaluation",
               tagList(
                 br(),
                 div("Evaluation statistics: full model and partition averages", id="stepText"), br(), br(),
                 DT::dataTableOutput('evalTbl'), br(),
                 div("Evaluation statistics: individual partitions", id="stepText"), br(), br(),
                 DT::dataTableOutput('evalTblBins')  
               )),
      tabPanel("Lambdas",
               br(),
               div("Maxent lambdas file", id = "stepText"), br(), br(),
               verbatimTextOutput("lambdas")
               )
    )
  })
  options <- list(scrollX = TRUE, sDom  = '<"top">rtp<"bottom">')
  evalTbl <- results()$evalTbl
  evalTblBins <- results()$evalTblBins
  evalTblRound <- cbind(evalTbl[,1:3], round(evalTbl[,4:16], digits=3))
  evalTblBinsRound <- cbind(settings=evalTbl[,1], round(evalTblBins, digits=3))
  # define contents for both evaluation tables
  output$evalTbl <- DT::renderDataTable(evalTblRound, options = options)
  output$evalTblBins <- DT::renderDataTable(evalTblBinsRound, options = options)
  # define contents for lambdas table
  output$lambdas <- renderPrint({
    if(spp[[curSp()]]$rmm$model$algorithm == "maxnet") {
      results()$models[[curModel()]]$betas
    } else if(spp[[curSp()]]$rmm$model$algorithm == "maxent.jar") {
      results()$models[[curModel()]]@lambdas
    }
  })
  
}
