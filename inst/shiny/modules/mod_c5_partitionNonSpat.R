
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
    
    for(sp in spIn()) {
      if (is.null(spp[[curSp()]]$procEnvs$bgMask)) {
        logs %>% writeLog(type = 'error', "Before partitioning occurrences for ", spName(spp[[sp]]), 
                        ", mask your environmental variables by your background extent.")
        return()
      }
      
      #### FUNCTION CALL
      group.data <- c5_partitionOccs(spp[[sp]]$occs, 
                                     spp[[sp]]$bg, 
                                     input$partNspSel, 
                                     kfolds = input$kfolds, 
                                     logs=logs, shiny=TRUE)
      req(group.data)
      
      # LOAD INTO SPP ####
      spp[[sp]]$occs$partition <- group.data$occ.grp
      spp[[sp]]$bg$partition <- group.data$bg.grp
      
      # METADATA ####
      if(input$partNspSel == 'jack') {
        spp[[sp]]$rmm$model$partition$numberFolds <- nrow(spp[[sp]]$occs)
        spp[[sp]]$rmm$model$partition$partitionRule <- 'jackknife'
      }
      if(input$partNspSel == 'rand') {
        spp[[sp]]$rmm$model$partition$numberFolds <- input$kfolds
        spp[[sp]]$rmm$model$partition$partitionRule <- 'random k-fold'
      }
    }
  })
}
