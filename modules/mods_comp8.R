comp8_selProjExt <- function() {
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
    msk <- crop(values$preds, values$poly2)
    values$projMsk <- mask(msk, values$poly2)
  })
  
  x <- round(values$polyPts2, digits = 2)  # round all coords to 2 decimal digits
  coordsChar <- paste(apply(x, 1, function(b) paste0('(',paste(b, collapse=', '),')')), collapse=', ')  # concatanate coords to a single character
  isolate(writeLog(paste0('* Defined projection extent to: ', coordsChar)))
}

comp8_pjArea <- function(modelSel, predForm, enmSel) {
  if (is.null(values$projMsk)) {
    writeLog('* SELECT projection extent first.')
    return()
  }
  writeLog('* PROJECTING to new area.')
  curMod <- values$evalMods[[as.numeric(modelSel)]]
  values$pjArea <- predict(curMod, values$projMsk)
  rasVals <- values$pjArea@data@values
  
  if (predForm == 'log' & enmSel == "Maxent") {
    rasVals <- c(values$pjArea@data@values, 0, 1)  # set to 0-1 scale
  }
  rasVals <- rasVals[!is.na(rasVals)]
  
  proxy %>% removeShape('poly2Sel')
  # proxy %>% clearImages()
  rasVals <- na.omit(rasVals)
  pal <- colorNumeric(c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"), rasVals, na.color='transparent')
  values$leg2 <- list(rasVals=rasVals, pal=pal)
  proxy %>% addLegend("topright", pal = pal, title = "Predicted Suitability",
                      values = rasVals, layerId = 'r2Legend')
  proxy %>% addRasterImage(values$pjArea, colors = pal, group = 'r2', layerId = 'r2')
}

# comp8_pjTime <- function(modelSel, predForm, enmSel, bcRes, bcRCP, bcMod, bcYr) {
#   if (is.null(values$projMsk)) {
#     writeLog('* SELECT projection extent first.')
#     return()
#   }
#   withProgress(message = "Retrieving WorldClim data...", {
#     values$projBC <- getData(name = "worldclim", var = "bio", res = bcRes, 
#                             rcp = bcRCP, model = bcMod, year = bcYr)
#   })
#   writeLog('* PROJECTING to new time.')
#   curMod <- values$evalMods[[as.numeric(modelSel)]]
#   values$pjArea <- predict(curMod, values$projMsk)
#   rasVals <- values$pjArea@data@values
#   
#   if (predForm == 'log' & enmSel == "Maxent") {
#     rasVals <- c(values$pjArea@data@values, 0, 1)  # set to 0-1 scale
#   }
#   rasVals <- rasVals[!is.na(rasVals)]
#   
#   proxy %>% removeShape('poly2Sel')
#   # proxy %>% clearImages()
#   rasVals <- na.omit(rasVals)
#   pal <- colorNumeric(c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"), rasVals, na.color='transparent')
#   values$leg2 <- list(rasVals=rasVals, pal=pal)
#   proxy %>% addLegend("topright", pal = pal, title = "Predicted Suitability",
#                       values = rasVals, layerId = 'r2Legend')
#   proxy %>% addRasterImage(values$pjArea, colors = pal, group = 'r2', layerId = 'r2')
# }

comp8_mess <- function() {
  if (is.null(values$projMsk)) {
    writeLog('! SELECT projection extent first.')
    return()
  }
  writeLog('* Generating MESS map.')
  occVals <- extract(values$preds, cbind(values$df$longitude, values$df$latitude))
  values$mess <- mess(values$projMsk, occVals)
  # proxy %>% clearShapes()
  # proxy %>% clearImages()
  rasVals <- values$mess@data@values
  rasVals <- na.omit(rasVals)
  rasVals[is.infinite(rasVals)] <-

  # pal <- colorNumeric(c("#fff5f0", "#fb6a4a", "#67000d"), rasVals, na.color='transparent')
  # proxy %>% addLegend("topright", pal = pal, title = "MESS Values",
                      # values = rasVals, layerId = values$polyID + 1)
  proxy %>% addRasterImage(values$mess, layerId = 'ms')
}