comp8_selProjExt <- function() {
  if (is.null(values$df)) return()
  if (is.null(values$drawPolyCoordsProjExt)) return()
  values$drawPolyCoordsProjExt <- unique(values$drawPolyCoordsProjExt)  # remove phantom first row after reset
  if (nrow(values$drawPolyCoordsProjExt) < 3) {
    writeLog("! Please define a shape with at least 3 sides.")
    return()
  }
  values$polyErase <- TRUE  # turn on to signal to prevent the use of an existing map click
  values$polyID <- values$polyID + 1
  
  values$projExtPoly <- SpatialPolygons(list(Polygons(list(Polygon(values$drawPolyCoordsProjExt)), ID=values$polyID)))  # create new polygon from coords
  
  x <- round(values$drawPolyCoordsProjExt, digits = 2)  # round all coords to 2 decimal digits
  coordsChar <- paste(apply(x, 1, function(b) paste0('(',paste(b, collapse=', '),')')), collapse=', ')  # concatanate coords to a single character
  values$drawPolyCoordsProjExt <- NULL
  isolate(writeLog(paste0('* Defined projection extent to: ', coordsChar)))
}

comp8_pjModel <- function(modelSel, preds) {
  if (is.null(values$projExtPoly)) return()
  curMod <- values$evalMods[[as.numeric(modelSel)]]
  newMskPreds <- crop(preds, values$projExtPoly)
  newMskPreds <- mask(newMskPreds, values$projExtPoly)
  values$pj <- predict(curMod, newMskPreds)
  rasVals <- getValues(values$pj)
  pal <- colorNumeric(c("#fff5f0", "#fb6a4a", "#67000d"), rasVals, na.color='transparent')
  proxy %>% addLegend("topright", pal = pal, title = "Predicted Suitability",
                      values = rasVals, layerId = 2)
  proxy %>% addRasterImage(values$pj, colors = pal, opacity = 0.7, layerId = 'r2')
}