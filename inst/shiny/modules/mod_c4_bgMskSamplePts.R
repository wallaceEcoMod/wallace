
bgMskAndSamplePts_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("bgPtsNum"), label = "No. of background points", value = 10000, min = 1, step = 1)
  )
}

bgMskAndSamplePts_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$bgShp)) {
      rvs %>% writeLog(type = 'error', "Before sampling background points, 
                       define the background extent.")
      return()
    }
    
    if (input$bgPtsNum < 1) {
      rvs %>% writeLog(type = 'warning',
                       "Enter a non-zero number of background points.")
      return()
    }
    
    # record for RMD
    rvs$bgPtsNum <- input$bgPtsNum
    
    # mask envs by background extent
    withProgress(message = "Processing environmental data...", {
      bgCrop <- raster::crop(rvs$envs, rvs$bgShp)
      bgMask <- raster::mask(bgCrop, rvs$bgShp)
    })
    rvs %>% writeLog('Environmental data masked.')
    # sample random background points
    withProgress(message = "Generating background points...", {
      bgXY <- dismo::randomPoints(bgMask, input$bgPtsNum)
    })
    bg.prop <- round(nrow(bgXY)/input$bgPtsNum, digits = 2)
    if(bg.prop == 1) {
      rvs %>% writeLog("Random background points sampled (n =", input$bgPtsNum, ").")
    } else {
      rvs %>% writeLog("Random background points requested (n =", input$bgPtsNum, 
                       "), but only ", 100*bg.prop, "% of points (n = ", nrow(bgXY), ") were able to be sampled.")
    }
    shinyjs::enable("downloadMskPreds")
    return(list(msk = bgMask, pts = bgXY))
  })
}
