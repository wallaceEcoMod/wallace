projectArea_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

projectArea_MOD <- function(input, output, session, rvs) {
  reactive({
    req(rvs$envs, rvs$mods, rvs$predCur, rvs$polyXY)
    
    # create new spatial polygon from coordinates
    newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(rvs$polyXY)), ID=rvs$polyID)))  
    
    # concatanate coords to a single character
    xy.round <- round(rvs$polyXY, digits = 2)
    coordsChar <- paste(apply(xy.round, 1, function(b) paste0('(',paste(b, collapse=', '),')')), collapse=', ')  
    rvs %>% writeLog('New area projection for model', rvs$modSel, 'with extent coordinates:', coordsChar)
    
    withProgress(message = "Masking environmental grids to projection extent...", {
      projMsk <- raster::crop(rvs$envs, newPoly)
      projMsk <- raster::mask(projMsk, newPoly)
    })
    
    modCur <- rvs$mods[[rvs$modSel]]
    
    withProgress(message = 'Projecting model to new area...', {
      # values$rasName <- names(values$evalPreds[[as.numeric(modelSel)]])
      modProjArea <- dismo::predict(modCur, projMsk)
    })
    
    return(modProjArea)
  })
}
