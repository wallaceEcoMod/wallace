
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
    for(sp in spIn()) {
      if (is.null(bgMask())) {
        shinyLogs %>% writeLog(type = 'error', "Before partitioning occurrences for ", sp,
                               ", mask your environmental variables by your background extent.")
        return()
      }
      
      # FUNCTION CALL ####
      group.data <- c5_partitionOccs(spp[[sp]]$occs, 
                                     spp[[sp]]$bg, 
                                     input$partSpSel, 
                                     bgMsk = spp[[sp]]$procEnvs$bgMask, 
                                     aggFact = input$aggFact, 
                                     shinyLogs)
      req(group.data)
      
      # LOAD INTO SPP ####
      spp[[sp]]$occs$partition <- group.data$occ.grp
      spp[[sp]]$bg$partition <- group.data$bg.grp
      
      # METADATA ####
      if(input$partSpSel == 'block') {
        spp[[sp]]$rmm$model$partition$numberFolds <- 4
        spp[[sp]]$rmm$model$partition$partitionRule <- 'spatial block'
      }
      if(input$partSpSel == 'cb1') {
        spp[[sp]]$rmm$model$partition$numberFolds <- 2
        spp[[sp]]$rmm$model$partition$partitionRule <- 'checkerboard'
      }
      if(input$partSpSel == 'cb2') {
        spp[[sp]]$rmm$model$partition$numberFolds <- 4
        spp[[sp]]$rmm$model$partition$partitionRule <- 'hierarchical checkerboard'
        spp[[sp]]$rmm$model$partition$notes <- paste('aggregation factor =', input$aggFact)
      }
    }
    
  })
}

partitionSpat_INFO <- infoGenerator(modName = "Spatial Partition",
                                    modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson",
                                    pkgName = "ENMeval")