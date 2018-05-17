c8_projectArea <- function(modelo, ambiente, tipoFormato, polyPjXY, polyPjID = 1, shinyLogs = NULL) {
  if (is.null(spp[[curSp()]]$visualization$mapPred)) {
    shinyLogs %>% writeLog(type = 'error', 'Calculate a model prediction in component 7 
                           before projecting.')
    return()
  }
  if (is.null(polyPjXY)) {
    shinyLogs %>% writeLog(type = 'error', "The polygon has not been drawn and finished. 
                           Please use the draw toolbar on the left-hand of the map to complete
                           the polygon.")
    return()
  }
  # create new spatial polygon from coordinates
  newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polyPjXY)), ID = polyPjID)))  
  
  # concatanate coords to a single character
  xy.round <- round(polyPjXY, digits = 2)
  xy.round <- xy.round[-nrow(xy.round),]  # remove last point that completes polygon
  coordsChar <- paste(apply(xy.round, 1, function(b) paste0('(',paste(b, collapse=', '),')')), collapse=', ')  
  shinyLogs %>% writeLog('New area projection for model', curModel(), 'with extent coordinates:', coordsChar)
  
  smartProgress(shinyLogs, message = "Masking environmental grids to projection extent...", {
    projMsk <- raster::crop(ambi, newPoly)
    projMsk <- raster::mask(projMsk, newPoly)
  })
  
  smartProgress(shinyLogs, message = 'Projecting model to new area...', {
    pargs <- paste0("outputformat=", tipoFormato)
    modProjArea <- dismo::predict(modelo, projMsk, args = pargs)
    # generate binary prediction based on selected thresholding rule 
    # (same for all Maxent prediction types because they scale the same)
    modProjArea.thr.call <- callModule(threshPred_MOD, "threshPred", modProjArea)
    modProjArea.thr <- modProjArea.thr.call()
    pjPred <- modProjArea.thr$pred
    rvs$comp8.thr <- modProjArea.thr$thresh
  })
}