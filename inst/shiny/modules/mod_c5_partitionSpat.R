
partSp_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("partSpSel"), "Options Available:",
                choices = list("None selected" = '',
                               "Block (k = 4)" = "block",
                               "Checkerboard 1 (k = 2)" = "cb1",
                               "Checkerboard 2 (k = 4)" = "cb2")),
    numericInput(ns("aggFact"), label = "Aggregation Factor", value = 2, min = 2)
  )
}

partSp_MOD <- function(input, output, session) {
  reactive({
    if (is.null(spp[[curSp()]]$bgMask)) {
      logs %>% writeLog(type = 'error', "Before partitioning occurrences, 
                       mask your environmental variables by your background extent.")
      return()
    }
    
    # FUNCTION CALL ####
    group.data <- c5_partitionOccs(spp[[curSp()]]$occs, spp[[curSp()]]$bgPts, input$partSpSel, 
                                   bgMsk = spp[[curSp()]]$bgMsk, aggFact = input$aggFact, logs, shiny=TRUE)
    
    if (is.null(group.data)) return()
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$parts$occ.grp <- group.data$occ.grp
    spp[[curSp()]]$parts$bg.grp <- group.data$bg.grp
    
    # RMD VALUES ####
    # spp[[curSp()]]$rmd$c5 <- list(partNspSel = input$partNspSel, kfolds = input$kfolds)
    
  })
}
