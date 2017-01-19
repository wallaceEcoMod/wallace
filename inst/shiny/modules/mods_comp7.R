comp7_mapPred <- function(modelSelPlotStudyExt, predForm, predThresh, proxy) {
  if (is.null(values$evalPreds)) {
    writeLog('<font color="red"><b>! ERROR</b></font> : Run a model before plotting predictions.')
    return()
  }

  values$predCurThresh <- predThresh
  values$goMapPred <- TRUE
  proxy %>% removeImage('r1ID')  # remove current raster
  selRasRaw <- values$evalPreds[[as.numeric(modelSelPlotStudyExt)]]
  selRasLog <- values$evalPredsLog[[as.numeric(modelSelPlotStudyExt)]]
  if (predForm == 'raw' | is.null(selRasLog)) {
    selRas <- selRasRaw
    rasVals <- selRas@data@values
  } else if (predForm == 'log' & !is.null(selRasLog)) {
    selRas <- selRasLog
    rasVals <- c(selRas@data@values, 0, 1)  # set to 0-1 scale
  }
  values$rasValsArea <- rasVals[!is.na(rasVals)]

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
      values$rasPalArea <- c('gray', 'blue')
      proxy %>%
        removeControl('r1LegCon') %>%
        addLegend("topright", colors = values$rasPalArea,
                  title = "Thresholded Suitability", labels = c(0, 1),
                  opacity = 1, layerId = 'r1LegThr')
    } else {
      values$legPalArea <- colorNumeric(rev(rasCols), values$rasValsArea, na.color='transparent')
      values$rasPalArea <- colorNumeric(rasCols, values$rasValsArea, na.color='transparent')
      proxy %>%
        removeControl('r1LegThr') %>%
        addLegend("topright", pal = values$legPalArea, title = "Predicted Suitability",
                  values = values$rasValsArea, layerId = 'r1LegCon',
                  labFormat = reverseLabels(2, reverse_order=TRUE))
    }
    proxy %>% addRasterImage(values$predCur, colors = values$rasPalArea, opacity = 0.7, group = 'r1', layerId = 'r1ID')
  }
}
