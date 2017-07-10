
bgMskAndSamplePts_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("bgPtsNum"), label = "No. of background points to sample", value = 10000, min = 0, step = 1)
  )
}

bgMskAndSamplePts_MOD <- function(input, output, session, logs, envs, bgShp) {
  req(bgShp)
  reactive({
    if (is.null(envs())) {
      writeLog(type = 'error', 'Obtain environmental data first...')
      return()
    }
    
    # mask envs by background extent
    withProgress(message = "Processing environmental data...", {
      bgCrop <- raster::crop(envs(), bgShp())
      bgMask <- raster::mask(bgCrop, bgShp())
    })
    logs %>% writeLog('Environmental data masked.')
    # sample random background points
    withProgress(message = "Generating background points...", {
      bgXY <- dismo::randomPoints(bgMask, input$bgPtsNum)
    })
    logs %>% writeLog('Random background points sampled (n = 10,000).')
    shinyjs::enable("downloadMskPreds")
    return(list(msk = bgMask, pts = bgXY))
  })
}
