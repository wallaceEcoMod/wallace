
partNsp_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("partNspSel"), "Options Available:",
                choices = list("None selected" = '', "Jackknife (k = n)" = "jack",
                               "Random k-fold" = "rand")),
    numericInput(ns("kfolds"), label = "Number of Folds", value = 2, min = 2)
  )
}

partNsp_MOD <- function(input, output, session, rvs, occs, bgPts) {
  reactive({
    if (is.null(rvs$bgMsk)) {
      rvs %>% writeLog(type = 'error', "Before partitioning occurrences, 
                       mask your environmental variables by your background extent.")
      return()
    }
    if (input$partNspSel == '') {
      rvs %>% writeLog(type = 'error', "Please select a partitioning option.")
      return()
    }
    
    # record for RMD
    rvs$comp5 <- input$partNspSel
    rvs$kfolds <- input$kfolds

    occs.xy <- rvs$occs %>% dplyr::select(longitude, latitude)

    if (input$partNspSel == 'jack') {
      group.data <- ENMeval::get.jackknife(occs.xy, rvs$bgPts)
      rvs %>% writeLog("Occurrences partitioned by jackknife method.")
    } else if (input$partNspSel == 'rand') {
      group.data <- ENMeval::get.randomkfold(occs.xy, rvs$bgPts, input$kfolds)
      rvs %>% writeLog("Occurrences partitioned by random k-fold (k = ", input$kfolds, ").")
    }
      
    return(group.data)
  })
}
