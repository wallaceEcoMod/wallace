
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
    if (is.null(spp[[curSp()]]$bgMask)) {
      logs %>% writeLog(type = 'error', "Before partitioning occurrences, 
                        mask your environmental variables by your background extent.")
      return()
    }
    
    #### FUNCTION CALL
    group.data <- c5_partitionOccs(spp[[curSp()]]$occs, spp[[curSp()]]$bgPts, input$partSpSel, 
                                   kfolds = input$kfolds, logs, shiny=TRUE)
    
    if (is.null(group.data)) return()
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$parts$occ.grp <- group.data$occ.grp
    spp[[curSp()]]$parts$bg.grp <- group.data$bg.grp
    
    # RMD VALUES ####
    # spp[[curSp()]]$rmd$c5 <- list(partNspSel = input$partNspSel, kfolds = input$kfolds)
    
  })
}
