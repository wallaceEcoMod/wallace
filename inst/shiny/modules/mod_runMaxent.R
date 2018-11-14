
runMaxent_UI <- function(id) {
  ns <- NS(id)
  tagList(
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
    checkboxInput(ns("batch"), label = strong("Batch"), value = TRUE)
  )
}

runMaxent_MOD <- function(input, output, session) {
  reactive({
    
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()
    
    # PROCESSING ####
    for(sp in spLoop) {
      # ERRORS ####
      if (is.null(spp[[sp]]$occs$partition)) {
        shinyLogs %>% writeLog(type = 'error', "Before building a model, please partition 
                          occurrences for cross-validation for", spName(spp[[sp]]), ".")
        return()
      }
      # FUNCTION CALL ####
      m.maxent <- runMaxent(spp[[sp]]$occs, 
                            spp[[sp]]$bg, 
                            spp[[sp]]$occs$partition,
                            spp[[sp]]$bg$partition,
                            spp[[sp]]$procEnvs$bgMask, 
                            input$rms, 
                            input$rmsStep, 
                            input$fcs, 
                            shinyLogs)
      req(m.maxent)
      
      # LOAD INTO SPP ####
      spp[[sp]]$results <- m.maxent
      
      # METADATA ####
      spp[[sp]]$rmm$model$algorithm <- "Maxent"
      spp[[sp]]$rmm$model$maxent$featureSet <- input$fcs
      spp[[sp]]$rmm$model$maxent$regularizationMultiplierSet <- input$rms
      spp[[sp]]$rmm$model$maxent$regularizationRule <- paste("increment by", input$rmsStep)
      spp[[sp]]$rmm$model$maxent$notes <- "dismo package implementation"
    }
  })
}

runMaxent_INFO <- infoGenerator(modName = "Maxent",
                             modAuts = "Jamie M. Kass, Robert Muscarella, Bruno
                             Vilela, Gonzalo E. Pinilla-Buitrago, Robert P. 
                             Anderson",
                             pkgName = c("ENMeval", "dismo"))

runMaxent_TBL <- function(input, output, session) {
  output$evalTbls <- renderUI({
    options <- list(scrollX = TRUE, sDom  = '<"top">rtp<"bottom">')
    evalTbl <- results()$evalTbl
    evalTblBins <- results()$evalTblBins
    evalTblRound <- cbind(evalTbl[,1:3], round(evalTbl[,4:16], digits=3))
    evalTblBinsRound <- cbind(settings=evalTbl[,1], round(evalTblBins, digits=3))
    output$evalTbl <- DT::renderDataTable(evalTblRound, options = options)
    output$evalTblBins <- DT::renderDataTable(evalTblBinsRound, options = options)
    tagList(
      br(),
      div("Evaluation statistics: full model and partition averages", id="stepText"), br(), br(),
      DT::dataTableOutput('evalTbl'), br(),
      div("Individual partition bin evaluation statistics", id="stepText"), br(), br(),
      DT::dataTableOutput('evalTblBins')  
    )
  })
}
