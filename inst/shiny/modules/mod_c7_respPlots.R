
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
      rvs %>% writeLog(type = 'error', "Models must first be run in component 6.")
      return()
    }

    # record for RMD
    rvs$comp7 <- isolate(c(rvs$comp7, 'resp'))
    
    modCur <- rvs$mods[[rvs$modSel]]
    
    # handle downloads for Response Plots png for Maxnet
    if (rvs$algMaxent == "maxnet") {
      output$dlRespPlot <- downloadHandler(
        filename = function() {paste0(spName(), "_", rvs$envSel, "_response.png")},
        content = function(file) {
          png(file)
          if (input$dlRespPlotAll == TRUE) {
            plot(modCur, type = "cloglog")
          } else {
            maxnet::response.plot(modCur, v = rvs$envSel, type = "cloglog")  
          }
          dev.off()
        }
      )
      maxnet::response.plot(modCur, v = rvs$envSel, type = "cloglog")
    } else if (rvs$algMaxent == "maxent.jar") {
      # handle downloads for Response Plots png for maxent.jar
      output$dlRespPlot <- downloadHandler(
        filename = function() {paste0(spName(), "_", rvs$envSel, "_response.png")},
        content = function(file) {
          png(file)
          if (input$dlRespPlotAll == TRUE) {
            dismo::response(modCur)
          } else {
            dismo::response(modCur, var = rvs$envSel)  
          }
          dev.off()
        }
      )
      dismo::response(modCur, var = rvs$envSel)
    }
  })
}
