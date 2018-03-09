
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
    if (is.null(spp[[curSp()]]$procEnvs$bgMask)) {
      logs %>% writeLog(type = 'error', "Before partitioning occurrences, 
                       mask your environmental variables by your background extent.")
      return()
    }
    
    # FUNCTION CALL ####
    group.data <- c5_partitionOccs(spp[[curSp()]]$occs, 
                                   spp[[curSp()]]$bg, 
                                   input$partSpSel, 
                                   bgMsk = spp[[curSp()]]$procEnvs$bgMask, 
                                   aggFact = input$aggFact, 
                                   logs, shiny=TRUE)
    req(group.data)
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$occs$grp <- group.data$occ.grp
    spp[[curSp()]]$bg$grp <- group.data$bg.grp
    
    # METADATA ####
    if(input$partSpSel == 'block') {
      rmm$model$partition$numberFolds <- 4
      rmm$model$partition$partitionRule <- 'spatial block'
    }
    if(input$partSpSel == 'cb1') {
      rmm$model$partition$numberFolds <- 2
      rmm$model$partition$partitionRule <- 'checkerboard'
    }
    if(input$partSpSel == 'cb2') {
      rmm$model$partition$numberFolds <- 4
      rmm$model$partition$partitionRule <- 'hierarchical checkerboard'
      rmm$model$partition$notes <- paste('aggregation factor =', input$aggFact)
    }
    
  })
}
