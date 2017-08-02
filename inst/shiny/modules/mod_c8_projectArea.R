projectArea_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

projectArea_MOD <- function(input, output, session, rvs) {
  
  reactive({
    if (is.null(rvs$predCur)) {
      rvs %>% writeLog(type = 'error', 'Calculate a model prediction in component 7 
                       before projecting.')
      return()
    }
    if (is.null(rvs$polyPjXY)) {
      rvs %>% writeLog(type = 'error', "The polygon has not been drawn and finished. 
                       Please use the draw toolbar on the left-hand of the map to complete
                       the polygon.")
      return()
    }
    
    # create new spatial polygon from coordinates
    newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(rvs$polyPjXY)), ID=rvs$polyPjID)))  
    
    # concatanate coords to a single character
    xy.round <- round(rvs$polyPjXY, digits = 2)
    xy.round <- xy.round[-nrow(xy.round),]  # remove last point that completes polygon
    coordsChar <- paste(apply(xy.round, 1, function(b) paste0('(',paste(b, collapse=', '),')')), collapse=', ')  
    rvs %>% writeLog('New area projection for model', rvs$modSel, 'with extent coordinates:', coordsChar)
    
    withProgress(message = "Masking environmental grids to projection extent...", {
      projMsk <- raster::crop(rvs$envs, newPoly)
      projMsk <- raster::mask(projMsk, newPoly)
    })
    
    modCur <- rvs$mods[[rvs$modSel]]
    
    withProgress(message = 'Projecting model to new area...', {
      pargs <- paste0("outputformat=", rvs$comp7.type)
      modProjArea <- dismo::predict(modCur, projMsk, args = pargs)
    })
    
    return(list(pjMsk=projMsk, pjPred=modProjArea))
  })
}
