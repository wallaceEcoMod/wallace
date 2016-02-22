comp7_mapPred <- function(modelSel1, predForm, predThresh, proxy) {
  if (is.null(values$evalPreds)) {
    writeLog("! Please run a model before plotting predictions.")
    return()
  }
    
  proxy %>% removeImage('r1')  # remove current raster
  selRasRaw <- values$evalPreds[[as.numeric(modelSel1)]]
  selRasLog <- values$evalPredsLog[[as.numeric(modelSel1)]]
  if (predForm == 'raw' | is.null(selRasLog)) {
    selRas <- selRasRaw
    rasVals <- getValues(selRas)
  } else if (predForm == 'log' & !is.null(selRasLog)) {
    selRas <- selRasLog
    rasVals <- c(getValues(selRas), 0, 1)  # set to 0-1 scale
  }
  rasVals <- rasVals[!is.na(rasVals)]
  
  if (predThresh == 'mtp') {
    mtp <- values$mtps[as.numeric(modelSel1)]
    values$predCur <- selRasRaw > mtp
  } else if (predThresh == 'p10') {
    p10 <- values$p10s[as.numeric(modelSel1)]
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
                          opacity = 1, layerId = 1)
    } else {
      pal <- colorNumeric(c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"), rasVals, na.color='transparent')
      proxy %>% addLegend("topright", pal = pal, title = "Predicted Suitability",
                          values = rasVals, layerId = 1)
    }
    
    proxy %>% addRasterImage(values$predCur, colors = pal, opacity = 0.7, layerId = 'r1')
  }
}