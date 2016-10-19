comp7_mapPred <- function(modelSelPlotStudyExt, predForm, predThresh, proxy) {
  if (is.null(values$evalPreds)) {
    writeLog("! Please run a model before plotting predictions.")
    return()
  }
    
  proxy %>% removeImage('r1')  # remove current raster
  selRasRaw <- values$evalPreds[[as.numeric(modelSelPlotStudyExt)]]
  selRasLog <- values$evalPredsLog[[as.numeric(modelSelPlotStudyExt)]]
  if (predForm == 'raw' | is.null(selRasLog)) {
    selRas <- selRasRaw
    rasVals <- selRas@data@values
  } else if (predForm == 'log' & !is.null(selRasLog)) {
    selRas <- selRasLog
    rasVals <- c(selRas@data@values, 0, 1)  # set to 0-1 scale
  }
  rasVals <- rasVals[!is.na(rasVals)]
  
  if (predThresh == 'mtp') {
    mtp <- values$mtps[as.numeric(modelSelPlotStudyExt)]
    values$predCur <- selRasRaw > mtp
  } else if (predThresh == 'p10') {
    p10 <- values$p10s[as.numeric(modelSelPlotStudyExt)]
    values$predCur <- selRasRaw > p10
  } else {
    values$predCur <- selRas
  }
  values$rasName <- names(selRas)
  shinyjs::enable("downloadPred")
  
  if (!is.null(values$predCur)) {
    if (predThresh == 'mtp' | predThresh == 'p10') {
      pal <- c('gray', 'blue')
      proxy %>% addLegend("topright", colors = pal,
                          title = "Thresholded Suitability", labels = c(0, 1),
                          opacity = 1, layerId = 'threshLegend')
    } else {
      pal <- colorNumeric(c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"), rasVals, na.color='transparent')
      values$leg1 <- list(rasVals=rasVals, pal=pal)
      proxy %>% addLegend("topright", pal = pal, title = "Predicted Suitability",
                          values = rasVals, layerId = 'r1Legend')
    }
    proxy %>% addRasterImage(values$predCur, colors = pal, opacity = 0.7, group = 'r1', layerId = 'r1')
  }
}