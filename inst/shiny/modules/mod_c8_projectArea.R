projArea_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

projArea_MOD <- function(input, output, session, rvs) {
  reactive({
    req(rvs$envs, rvs$mods, rvs$predCur)
    
    # create new spatial polygon from coordinates
    newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(rvs$polyXY)), ID=rvs$polyID)))  
    
    # concatanate coords to a single character
    coordsChar <- paste(apply(rvs$polyXY, 1, function(b) paste0('(',paste(b, collapse=', '),')')), collapse=', ')  
    rvs %>% writeLog('Defined projection extent to:', coordsChar)
    
    withProgress(message = "Masking environmental grids to projection extent...", {
      projMsk <- raster::crop(rvs$envs, newPoly)
      projMsk <- raster::mask(projMsk, newPoly)
      print(projMsk)
    })
    
    withProgress('Projecting model to extent...')
    print(rvs$mods)
    modCur <- rvs$mods[[rvs$modSel]]
    
    # values$rasName <- names(values$evalPreds[[as.numeric(modelSel)]])
    modProjArea <- dismo::predict(modCur, projMsk)
    modProjArea.vals <- raster::values(modProjArea)
    
    if (rvs$predType == 'log') {
      modProjArea.vals <- c(modProjArea.vals, 0, 1)  # set to 0-1 scale
    }
    # remove NAs
    rasVals <- modProjArea.vals[!is.na(modProjArea.vals)]
    
    return(list(modProjArea, rasVals))
  })
}
