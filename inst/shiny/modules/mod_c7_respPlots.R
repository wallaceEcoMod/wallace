
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
    
    # handle downloads for Response Plots png
    if (input$dlRespPlotAll == FALSE) {
      output$dlRespPlot <- downloadHandler(
        filename = function() {paste0(spName(), "_", rvs$modSel, "_", rvs$envSel, "_response.png")},
        content = function(file) {
          png(file)
          if (rvs$algMaxent == "maxnet") {
            maxnet::response.plot(modCur, v = rvs$envSel, type = "cloglog")
          } else if (rvs$algMaxent == "maxent.jar") {
            dismo::response(modCur, var = rvs$envSel)
          }
          dev.off()
        })
      } else if (input$dlRespPlotAll == TRUE) {
        output$dlRespPlot <- downloadHandler(
          filename = function() {paste0(spName(), "_", rvs$modSel, "_all_response.png")},
          content = function(file) {
            png(file)
            if (rvs$algMaxent == "maxnet") {
              plot(modCur, type = "cloglog", vars = rvs$mxNonZeroCoefs)
            } else if (rvs$algMaxent == "maxent.jar") {
              dismo::response(modCur, var = rvs$mxNonZeroCoefs)
            }
            dev.off()
          })
      }
    # plot in wallace
    if (rvs$algMaxent == "maxnet") {
      maxnet::response.plot(modCur, v = rvs$envSel, type = "cloglog")
    } else if (rvs$algMaxent == "maxent.jar") {
      dismo::response(modCur, var = rvs$envSel)
    }
  })
}
    

