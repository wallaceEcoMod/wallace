comp7_mapPred <- function(predictionSel1, predForm, predThresh, proxy) {
  proxy %>% removeImage('r1')  # remove current raster
  selRasRaw <- values$evalPreds[[as.numeric(predictionSel1)]]
  selRasLog <- values$evalPredsLog[[as.numeric(predictionSel1)]]
  if (predForm == 'raw' | is.null(selRasLog)) {
    selRas <- selRasRaw
    rasVals <- getValues(selRas)
  } else if (predForm == 'log' & !is.null(selRasLog)) {
    selRas <- selRasLog
    rasVals <- c(getValues(selRas), 0, 1)  # set to 0-1 scale
  }
  rasVals <- rasVals[!is.na(rasVals)]
  
  if (predThresh == 'mtp') {
    mtp <- values$mtps[as.numeric(predictionSel1)]
    values$predCur <- selRasRaw > mtp
  } else if (predThresh == 'p10') {
    p10 <- values$p10s[as.numeric(predictionSel1)]
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
      pal <- colorNumeric(c("#fff5f0", "#fb6a4a", "#67000d"), rasVals, na.color='transparent')
      proxy %>% addLegend("topright", pal = pal, title = "Predicted Suitability",
                          values = rasVals, layerId = 1)
    }
    
    proxy %>% addRasterImage(values$predCur, colors = pal, opacity = 0.7, layerId = 'r1')
  }
}