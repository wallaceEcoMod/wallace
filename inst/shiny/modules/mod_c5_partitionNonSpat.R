
partNonSpat_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("partNonSpat"), "Options Available:",
                choices = list("None selected" = '', "Jackknife (k = n)" = "jack",
                               "Random k-fold" = "rand")),
    numericInput(ns("kfolds"), label = "Number of Folds", value = 2, min = 2)
  )
}

partNonSpat_MOD <- function(input, output, session, logs, occs, bgPts) {
  reactive({
    req(occs())
    req(bgPts())

    occs.xy <- occs() %>% dplyr::select(longitude, latitude)
    print(occs.xy)
    if (input$partNonSpat == 'jack') {
      print('JACK')
      group.data <- ENMeval::get.jackknife(occs.xy, bgPts())
      logs %>% writeLog("Occurrences partitioned by jackknife method.")
    } else if (input$partNonSpat == 'rand') {
      print('RAND')
      group.data <- ENMeval::get.randomkfold(occs.xy, bgPts(), input$kfolds)
      logs %>% writeLog("Occurrences partitioned by random k-fold (k = ", input$kfolds, ").")
    }
      
    print(group.data)
    return(group.data)
  })
}
