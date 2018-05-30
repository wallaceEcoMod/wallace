c8_projectArea <- function(results, curModel, envs, outputType, polyPjXY, polyPjID, shinyLogs = NULL) {
  # create new spatial polygon from coordinates
  newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polyPjXY)), ID = polyPjID)))  
  
  # concatanate coords to a single character
  xy.round <- round(polyPjXY, digits = 2)
  xy.round <- xy.round[-nrow(xy.round),]  # remove last point that completes polygon
  coordsChar <- paste(apply(xy.round, 1, function(b) paste0('(',paste(b, collapse=', '),')')), collapse=', ')  
  shinyLogs %>% writeLog('New area projection for model', curModel, 'with extent coordinates:', coordsChar)
  
  smartProgress(shinyLogs, message = "Masking environmental grids to projection extent...", {
    projMsk <- raster::crop(envs, newPoly)
    projMsk <- raster::mask(projMsk, newPoly)
  })
  
  smartProgress(shinyLogs, message = 'Projecting model to new area...', {
    pargs <- paste0("outputformat=", outputType)
    modProjArea <- dismo::predict(results$models[[curModel]], projMsk, args = pargs)
  })
  return(list(projExt=projMsk, projArea=modProjArea))
}