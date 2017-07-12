
partNsp_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("partNspSel"), "Options Available:",
                choices = list("None selected" = '', "Jackknife (k = n)" = "jack",
                               "Random k-fold" = "rand")),
    numericInput(ns("kfolds"), label = "Number of Folds", value = 2, min = 2)
  )
}

partNsp_MOD <- function(input, output, session, logs, occs, bgPts) {
  reactive({
    req(occs())
    req(bgPts())

    occs.xy <- occs() %>% dplyr::select(longitude, latitude)

    if (input$partNspSel == 'jack') {
      group.data <- ENMeval::get.jackknife(occs.xy, bgPts())
      logs %>% writeLog("Occurrences partitioned by jackknife method.")
    } else if (input$partNspSel == 'rand') {
      group.data <- ENMeval::get.randomkfold(occs.xy, bgPts(), input$kfolds)
      logs %>% writeLog("Occurrences partitioned by random k-fold (k = ", input$kfolds, ").")
    }
      
    return(group.data)
  })
}
