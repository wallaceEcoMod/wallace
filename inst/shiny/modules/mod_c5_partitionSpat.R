
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
    req(rvs$occs, rvs$bgPts, rvs$bgMsk)
    
    # record for RMD
    rvs$partSel <- input$partSpSel
    rvs$aggFact <- input$aggFact
    
    occs.xy <- rvs$occs %>% dplyr::select(longitude, latitude)

    if (input$partSpSel == 'block') {
      group.data <- ENMeval::get.block(occs.xy, rvs$bgPts)
      rvs %>% writeLog("Occurrences partitioned by block method.")
    } else if (input$partSpSel == 'cb1') {
      withProgress(message = "Aggregating rasters...", {
        group.data <- ENMeval::get.checkerboard1(occs.xy, rvs$bgMsk, rvs$bgPts, input$aggFact)
        rvs %>% writeLog("Occurrences partitioned by checkerboard 1 method.")
      })
    } else if (input$partSpSel == 'cb2') {
      withProgress(message = "Aggregating rasters...", {
        group.data <- ENMeval::get.checkerboard2(occs.xy, rvs$bgMsk, rvs$bgPts, input$aggFact)
        rvs %>% writeLog("Occurrences partitioned by checkerboard 2 method.")
      })
    }
    
    return(group.data)
  })
}
