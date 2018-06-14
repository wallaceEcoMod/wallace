runBIOCLIM_UI <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("batch"), label = strong("Batch"), value = TRUE)
  )
}

runBIOCLIM_MOD <- function(input, output, session) {
  reactive({
    
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()
    
    # PROCESSING ####
    for(sp in spLoop) {
      # ERRORS ####
      if(is.null(spp[[sp]]$occs$partition)) {
        shinyLogs %>% writeLog(type = 'error', "Before building a model, please partition 
                          occurrences for cross-validation for", spName(spp[[sp]]), ".")
        return()
      }
      
      # FUNCTION CALL ####
      m.bioclim <- runBIOCLIM(spp[[sp]]$occs, 
                              spp[[sp]]$bg, 
                              spp[[sp]]$procEnvs$bgMask, 
                              shinyLogs)
      
      req(m.bioclim)
      
      # LOAD INTO SPP ####
      spp[[sp]]$results <- m.bioclim
      
      # METADATA ####
      spp[[sp]]$rmm$model$algorithm <- "BIOCLIM"
      spp[[sp]]$rmm$model$bioclim$notes <- "dismo package implementation"
    }
  })
}

runBIOCLIM_INFO <- infoGenerator(modName = "BIOCLIM",
                              modAuts = "Jamie M. Kass, Robert Muscarella, Bruno Vilela, Robert P. Anderson",
                              pkgName = c("ENMeval", "dismo"))

runBIOCLIM_TBL <- function(input, output, session) {
  output$evalTbls <- renderUI({
    options <- list(scrollX = TRUE, sDom  = '<"top">rtp<"bottom">')
    evalTbl <- results()$evalTbl
    evalTblBins <- results()$evalTblBins
    evalTblRound <- round(evalTbl, digits=3)
    evalTblBinsRound <- results()$evalTblBins
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
# occVals <- raster::extract(e$predictions, values$modParams$occ.pts)
# 
# values$mtps <- min(occVals)  # apply minimum training presence threshold
# 
# # Define 10% training presence threshold
# if (length(occVals) < 10) {  # if less than 10 occ values, find 90% of total and round down
#   n90 <- floor(length(occVals) * 0.9)
# } else {  # if greater than or equal to 10 occ values, round up
#   n90 <- ceiling(length(occVals) * 0.9)
# }
# 
# values$p10s <- rev(sort(occVals))[n90]  # apply 10% training presence threshold