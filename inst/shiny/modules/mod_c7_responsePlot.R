
responsePlot_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

responsePlot_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(spp[[curSp()]]$modelList)) {
      logs %>% writeLog(type = 'error', "Models must first be run in component Model.")
      return()
    }
    print(curModel())
    print(curEnv())
    print(spp[[curSp()]]$modelList$models[[curModel()]])
    dismo::response(spp[[curSp()]]$modelList$models[[curModel()]], var = curEnv())
  })
}

responsePlot_INFO <- infoGenerator(modName = "Response Curves", 
                                   modAuts = "Jamie M. Kass, Robert Muscarella, Bruno Vilela, Robert P. Anderson", 
                                   pkgName = "dismo")
