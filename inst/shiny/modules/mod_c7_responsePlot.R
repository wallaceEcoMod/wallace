
responsePlot_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

responsePlot_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(spp[[curSp()]]$modelList)) {
      shinyLogs %>% writeLog(type = 'error', "Models must first be run in component Model.")
      return()
    }
    dismo::response(results()$models[[curModel()]], var = curEnv())
  })
}

responsePlot_INFO <- infoGenerator(modName = "Response Curves", 
                                   modAuts = "Jamie M. Kass, Robert Muscarella, Bruno Vilela, Robert P. Anderson", 
                                   pkgName = "dismo")
