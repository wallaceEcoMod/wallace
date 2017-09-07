
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
      rvals <- raster::getValues(bgMask)
      num.vals <- sum(!is.na(rvals))
      pct <- round((input$bgPtsNum / num.vals) * 100, digits = 2)
      bgXY <- dismo::randomPoints(bgMask, input$bgPtsNum)
    })
    rvs %>% writeLog('Random background points sampled (n =', input$bgPtsNum, 
                      ':', pct, '% of cells with values).')
    shinyjs::enable("downloadMskPreds")
    return(list(msk = bgMask, pts = bgXY))
  })
}
