
respPlots_UI <- function(id) {
  ns <- NS(id)
  tagList(
    strong("Download response plot (.png)"), br(),
    checkboxInput(ns('dlRespPlotAll'), "All response plots?"),
    downloadButton(ns('dlRespPlot'), "Download")
  )
}

respPlots_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$mods)) {
      logs %>% writeLog(type = 'error', "Models must first be run in component 6.")
      return()
    }

    # record for RMD
    rvs$comp7 <- isolate(c(rvs$comp7, 'resp'))
    
    modCur <- rvs$mods[[rvs$curMod]]
    
    # handle downloads for Response Plots png
    output$dlRespPlot <- downloadHandler(
      filename = function() {paste0(spName(), "_", rvs$curEnv, "_response.png")},
      content = function(file) {
        png(file)
        if (input$dlRespPlotAll == TRUE) {
          dismo::response(modCur)
        } else {
          dismo::response(modCur, var = rvs$curEnv)  
        }
        dev.off()
      }
    )
    
    dismo::response(modCur, var = rvs$curEnv)
  })
}

respPlots_INFO <- infoGenerator(modName = "Response Curves", 
                                modAuts = "Jamie M. Kass, Robert Muscarella, Bruno Vilela, Robert P. Anderson", 
                                pkgName = "dismo")
