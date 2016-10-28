comp7_mapPred <- function(modelSelPlotStudyExt, predForm, predThresh, proxy) {
  if (is.null(values$evalPreds)) {
    writeLog("! Please run a model before plotting predictions.")
    return()
  }
    
  values$goMapPred <- TRUE
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
      legPal <- colorNumeric(rev(rasCols), rasVals, na.color='transparent')
      rasPal <- colorNumeric(rasCols, rasVals, na.color='transparent')
      # values$leg1 <- list(rasVals=rasVals, pal=pal)
      proxy %>% addLegend("topright", pal = legPal, title = "Predicted SEuitability",
                          values = rasVals, layerId = 'r1Legend', labFormat = reverseLabels(2, reverse_order=TRUE))
    }
    proxy %>% addRasterImage(values$predCur, colors = rasPal, opacity = 0.7, group = 'r1', layerId = 'r1')
  }
}