
partNsp_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("partNspSel"), "Options Available:",
                choices = list("None selected" = '', "Jackknife (k = n)" = "jack",
                               "Random k-fold" = "rand")),
    numericInput(ns("kfolds"), label = "Number of Folds", value = 2, min = 2)
  )
}

partNsp_MOD <- function(input, output, session) {
  reactive({
    if (is.null(spp[[curSp()]]$procEnvs$bgMask)) {
      logs %>% writeLog(type = 'error', "Before partitioning occurrences, 
                        mask your environmental variables by your background extent.")
      return()
    }
    
    #### FUNCTION CALL
    group.data <- c5_partitionOccs(spp[[curSp()]]$occs, 
                                   spp[[curSp()]]$bg, 
                                   input$partNspSel, 
                                   kfolds = input$kfolds, 
                                   logs, shiny=TRUE)
    req(group.data)
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$occs$grp <- group.data$occ.grp
    spp[[curSp()]]$bg$grp <- group.data$bg.grp
    
    # METADATA ####
    if(input$partNspSel == 'jack') {
      rmm$model$partition$numberFolds <- nrow(spp[[curSp()]]$occs)
      rmm$model$partition$partitionRule <- 'jackknife'
    }
    if(input$partNspSel == 'rand') {
      rmm$model$partition$numberFolds <- input$kfolds
      rmm$model$partition$partitionRule <- 'random k-fold'
    }
  })
}
