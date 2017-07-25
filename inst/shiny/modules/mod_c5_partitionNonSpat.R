
partNsp_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("partNspSel"), "Options Available:",
                choices = list("None selected" = '', "Jackknife (k = n)" = "jack",
                               "Random k-fold" = "rand"),
                selected='rand'),
    numericInput(ns("kfolds"), label = "Number of Folds", value = 2, min = 2)
  )
}

partNsp_MOD <- function(input, output, session, rvs, occs, bgPts) {
  reactive({
    req(rvs$occs, rvs$bgPts)
    
    # record for RMD
    rvs$partSel <- input$partNspSel
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
