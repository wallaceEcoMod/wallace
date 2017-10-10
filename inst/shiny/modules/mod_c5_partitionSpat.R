
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

partSp_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$bgMsk)) {
      rvs %>% writeLog(type = 'error', "Before partitioning occurrences, 
                       mask your environmental variables by your background extent.")
      return()
    }
    if (input$partSpSel == '') {
      rvs %>% writeLog(type = 'error', "Please select a partitioning option.")
      return()
    }
    if (input$partSpSel == 'cb1' | input$partSpSel == 'cb2') {
      if (is.na(input$aggFact) | input$aggFact <= 1) {
        rvs %>% writeLog(type = 'error', "Please specify a positive aggregation 
                         factor greater than 1.")
        return()
      }
    }
    # record for RMD
    rvs$comp5 <- input$partSpSel
    rvs$aggFact <- input$aggFact
    
    occs.xy <- rvs$occs %>% dplyr::select(longitude, latitude)

    if (input$partSpSel == 'block') {
      group.data <- ENMeval::get.block(occs.xy, rvs$bgPts)
      rvs %>% writeLog("Occurrences partitioned by block method.")
    } else if (input$partSpSel == 'cb1') {
      withProgress(message = "Aggregating rasters...", {
        group.data <- ENMeval::get.checkerboard1(occs.xy, rvs$bgMsk, rvs$bgPts, input$aggFact)
        rvs %>% writeLog(paste0("Occurrences partitioned by checkerboard 1 method with 
                         aggregation factor ", input$aggFact, "."))
      })
    } else if (input$partSpSel == 'cb2') {
      withProgress(message = "Aggregating rasters...", {
        group.data <- ENMeval::get.checkerboard2(occs.xy, rvs$bgMsk, rvs$bgPts, input$aggFact)
        rvs %>% writeLog(paste0("Occurrences partitioned by checkerboard 2 method with 
                         aggregation factor ", input$aggFact, "."))
      })
    }
    
    return(group.data)
  })
}
