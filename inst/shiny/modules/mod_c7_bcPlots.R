
bcPlots_UI <- function(id) {
  ns <- NS(id)
  tagList(
    "Pick a bioclimatic variable number for each axis",
    numericInput(ns("bc1"), "Axis 1", value = 1, min = 1, max = 19),
    numericInput(ns("bc2"), "Axis 2", value = 2, min = 1, max = 19),
    numericInput(ns("bcProb"), "Set threshold", value = 0.9, min = 0.75, max = 1, step = 0.05),
    strong("Download envelope plot (.png)"), br(), br(),
    downloadButton(ns('dlBcPlot'), "Download")
  )
}

bcPlots_MOD <- function(input, output, session, rvs) {
  
  reactive({
    req(rvs$mods)
    
    # handle downloads for BIOCLIM Plots png
    output$dlBcPlot <- downloadHandler(
      filename = function() {paste0(spName(), "_bc_plot.png")},
      content = function(file) {
        png(file)
        bc.plot(rvs$mods[[1]], a = input$bc1, b = input$bc2, p = input$bcProb)
        dev.off()
      }
    )
    
    bc.plot(rvs$mods[[1]], a = input$bc1, b = input$bc2, p = input$bcProb)
  })
}
