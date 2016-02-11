comp8_selProjExt <- function(modelSel, preds) {
  if (is.null(values$df)) return()
  if (is.null(values$polyPts2)) return()

  values$polyPts2 <- unique(values$polyPts2)  # remove phantom first row after reset
  if (nrow(values$polyPts2) < 3) {
    writeLog("! Please define a shape with at least 3 sides.")
    return()
  }
  values$polyErase <- TRUE  # turn on to signal to prevent the use of an existing map click
  values$polyID <- values$polyID + 1
  
  values$poly2 <- SpatialPolygons(list(Polygons(list(Polygon(values$polyPts2)), ID=values$polyID)))  # create new polygon from coords
  proxy %>% addPolygons(values$polyPts2[,1], values$polyPts2[,2], weight=3, fill=FALSE, color='red', layerId='poly2Sel')
  
  withProgress(message = "Clipping environmental data to current extent...", {
    msk <- crop(preds, values$poly2)
    msk <- mask(msk, values$poly2)
    curMod <- values$evalMods[[as.numeric(modelSel)]]
    values$pjArea <- predict(curMod, msk)
    occVals <- extract(msk, cbind(values$df$longitude, values$df$latitude))
    values$mess <- mess(msk, occVals)
    values$mess[is.infinite(values$mess)] <- 9999
    values$pjMskPreds <- msk
  })
  
  x <- round(values$polyPts2, digits = 2)  # round all coords to 2 decimal digits
  coordsChar <- paste(apply(x, 1, function(b) paste0('(',paste(b, collapse=', '),')')), collapse=', ')  # concatanate coords to a single character
  values$polyPts2 <- NULL
  isolate(writeLog(paste0('* Defined projection extent to: ', coordsChar)))
}

comp8_pjCurExt <- function() {
  writeLog('* PROJECTING to new area.')
  rasVals <- getValues(values$pjArea)
  # pal <- colorNumeric(c("#fff5f0", "#fb6a4a", "#67000d"), rasVals, na.color='transparent')
  # proxy %>% addLegend("topright", pal = pal, title = "Predicted Suitability",
                      # values = rasVals, layerId = 2)
  # proxy %>% removeShape('poly2Sel')
  # proxy %>% clearImages()
  proxy %>% addRasterImage(values$pjArea, opacity = 0.7, layerId = 'r2')
}

comp8_mess <- function() {
  writeLog('* Generating MESS map.')

  # proxy %>% clearShapes()
  # proxy %>% clearImages()
  rasVals <- getValues(values$mess)
  # pal <- colorNumeric(c("#fff5f0", "#fb6a4a", "#67000d"), rasVals, na.color='transparent')
  # proxy %>% addLegend("topright", pal = pal, title = "MESS Values",
                      # values = rasVals, layerId = 2)
  proxy %>% addRasterImage(values$mess, opacity = 0.7, layerId = 'ms')
}