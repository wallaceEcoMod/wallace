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
  
  x <- round(values$polyPts2, digits = 2)  # round all coords to 2 decimal digits
  coordsChar <- paste(apply(x, 1, function(b) paste0('(',paste(b, collapse=', '),')')), collapse=', ')  # concatanate coords to a single character
  isolate(writeLog(paste0('* Defined projection extent to: ', coordsChar)))
}

comp8_pjArea <- function(modelSel, predForm, enmSel) {
  if (is.null(values$poly2)) {
    writeLog('* SELECT projection extent first.')
    return()
  }
  
  if (is.null(values$projMsk)) {
    withProgress(message = "Clipping environmental data to current extent...", {
      msk <- crop(values$preds, values$poly2)
      values$projMsk <- mask(msk, values$poly2)
    })    
  }
  
  writeLog('* PROJECTING to new area.')
  curMod <- values$evalMods[[as.numeric(modelSel)]]
  values$pjArea <- predict(curMod, values$projMsk)
  rasVals <- values(values$pjArea)
  
  if (predForm == 'log' & enmSel == "Maxent") {
    rasVals <- c(values$pjArea@data@values, 0, 1)  # set to 0-1 scale
  }
  rasVals <- rasVals[!is.na(rasVals)]
  
  proxy %>% removeShape('poly2Sel')
  # proxy %>% clearImages()
  rasVals <- na.omit(rasVals)
  legPal <- colorNumeric(rev(rasCols), rasVals, na.color='transparent')
  rasPal <- colorNumeric(rasCols, rasVals, na.color='transparent')
  # values$leg2 <- list(rasVals=rasVals, pal=pal)
  
  proxy %>% addLegend("topright", pal = legPal, title = "Predicted SEuitability",
                      values = rasVals, layerId = 'r2Legend', labFormat = reverseLabels(reverse_order=TRUE))
  proxy %>% addRasterImage(values$pjArea, colors = rasPal, group = 'r2', layerId = 'r2')
}

comp8_pjTime <- function(modelSel, predForm, enmSel, bcRes, selRCP, selGCM, selTime) {
  if (is.null(values$poly2)) {
    writeLog('* SELECT projection extent first.')
    return()
  }
  
  if (bcRes == 0.5) {
    writeLog('* Project to New Time only available with resolutions >30 arc seconds.')
    return()
  }
  
  withProgress(message = paste("Retrieving WorldClim data for", selTime, selRCP, "..."), {
    values$projTimeVars <- getData('CMIP5', var = "bio", res = bcRes,
                            rcp = selRCP, model = selGCM, year = selTime)
  })
  
  withProgress(message = "Clipping environmental data to current extent...", {
    msk <- crop(values$projTimeVars, values$poly2)
    values$projTimeMsk <- mask(msk, values$poly2)
    names(values$projTimeMsk) <- names(values$preds)  # make names same as original predictors
  })    
  
  writeLog('* PROJECTING to new time.')
  curMod <- values$evalMods[[as.numeric(modelSel)]]
  values$pjTime <- predict(curMod, values$projTimeMsk)
  rasVals <- values(values$pjTime)

  if (predForm == 'log' & enmSel == "Maxent") {
    rasVals <- c(values$pjTime@data@values, 0, 1)  # set to 0-1 scale
  }
  rasVals <- rasVals[!is.na(rasVals)]
  rasMax <- max(rasVals)
  rasMin <- min(rasVals)
  rng <- rasMin:rasMax
  rng.rev <- rev(rng)

  proxy %>% removeShape('poly2Sel')
  # proxy %>% clearImages()
  legPal <- colorNumeric(rev(rasCols), rng, na.color='transparent')
  rasPal <- colorNumeric(rasCols, rng, na.color='transparent')
  # values$leg2 <- list(rasVals=rasVals, pal=pal)
  proxy %>% addLegend("topright", pal = legPal, title = "Predicted Suitability",
                      values = rasVals, layerId = 'r2Legend', labFormat = reverseLabels())
  proxy %>% addRasterImage(values$pjTime, colors = rasPal, group = 'r2', layerId = 'r2')
}

comp8_mess <- function() {
  if (is.null(values$projMsk)) {
    writeLog('! SELECT projection extent first.')
    return()
  }
  writeLog('* Generating MESS map.')
  occVals <- extract(values$preds, cbind(values$df$longitude, values$df$latitude))
  values$mess <- suppressWarnings(mess(values$projMsk, occVals))
  # proxy %>% clearShapes()
  # proxy %>% clearImages()
  rasVals <- values$mess@data@values
  rasVals <- na.omit(rasVals)
  if (sum(is.infinite(rasVals)) > 0) {
    # find max after removing infinite values
    x <- rasVals
    x[is.infinite(x)] <- 0
    rasValsMax <- max(x)
  }
  # set infinite values to max
  rasVals[is.infinite(rasVals)] <- rasValsMax
  values$mess[is.infinite(values$mess)] <- rasValsMax

  pal <- colorNumeric(brewer.pal(n=11, name='Spectral'), rasVals, na.color='transparent')
  
  proxy %>% addLegend("topright", pal=pal, title = "MESS Values",
                      values = rasVals, layerId = 'mess')
  proxy %>% addRasterImage(values$mess, layerId = 'mess')
}