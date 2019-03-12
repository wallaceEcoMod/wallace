
responsePlot_UI <- function(id) {
  ns <- NS(id)
  tagList(
    #'Select an individual environmental variable, or "ALL" for all of them, to see the response curves.'
    uiOutput("curEnvUI"),
    h6("Reponse curves are displayed automatically in 'Results' tab(**)")
  )
  
}

responsePlot_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    req(curSp())
    if(is.null(evalOut())) {
      shinyLogs %>% writeLog(type = 'error', "Models must first be run in component Model.")
      return()
    }
    # plot in wallace
    if (spp[[curSp()]]$rmm$model$algorithm == "maxnet") {
      maxnet::response.plot(evalOut()@models[[curModel()]], v = curEnv(), type = "cloglog")
    } else if (spp[[curSp()]]$rmm$model$algorithm == "maxent.jar") {
      dismo::response(evalOut()@models[[curModel()]], var = curEnv())
    }
    
  })
}

responsePlot_INFO <- infoGenerator(modName = "Response Curves", 
                                   modAuts = "Jamie M. Kass, Robert Muscarella, 
                                   Bruno Vilela, Gonzalo E. Pinilla-Buitrago, 
                                   Robert P. Anderson", 
                                   pkgName = c("dismo", "maxnet"))
