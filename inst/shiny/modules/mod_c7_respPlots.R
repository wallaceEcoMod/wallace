
respPlots_UI <- function(id) {
  ns <- NS(id)
  tagList(
    strong("Download response plot (.png)"), br(), br(),
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
    
    # handle downloads for Maxent Evaluation Plots png
    output$dlRespPlot <- downloadHandler(
      filename = function() {paste0(spName(), "_", rvs$envSel, "_response.png")},
      content = function(file) {
        png(file)
        dismo::response(modCur, var = rvs$envSel)
        dev.off()
      }
    )
    
    dismo::response(modCur, var = rvs$envSel)
  })
}
