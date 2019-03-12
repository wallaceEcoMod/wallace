userSDM_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(occs())) {
      shinyLogs %>% writeLog(type = 'error', "Before uploading raster prediction, obtain occurrence data in component 1.(**)")
      return()
    }
    if (is.null(input$userEnvs)) {
      shinyLogs %>% writeLog(type = 'error', "Raster file not uploaded.")
      return()
    }
    
    userSDM <- postUserSDM(rasPath = input$use$datapath)
    
  })
}

userSDM_INFO <- infoGenerator(modName = "User SDM (**)",
                              modAuts = "Peter Galante, Gonzalo E. Pinilla-Buitrago",
                              pkgName = "raster")