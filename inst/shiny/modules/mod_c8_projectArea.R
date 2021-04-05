projectArea_UI <- function(id) {
  ns <- NS(id)
  tagList(
    threshPred_UI(ns('threshPred'))
  )
}

projectArea_MOD <- function(input, output, session, rvs) {
  
  reactive({
    if (is.null(rvs$predCur)) {
      rvs %>% writeLog(type = 'error', 'Calculate a model prediction in component 7 
                       before projecting.')
      return()
    }
    if (is.null(rvs$polyPjXY) | identical(rvs$polySelXY, rvs$polyPjXY)) {
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
    coordsChar <- paste(apply(xy.round, 1, function(b) paste0('(',paste(b, collapse=', '),')')), 
                        collapse=', ')  
    if (rvs$comp6 == 'bioclim') {
      rvs %>% writeLog('New area projection for BIOCLIM model with extent coordinates:', coordsChar)
    } else if (rvs$comp6 == 'maxent') {
      if (rvs$clamp == T | rvs$algMaxent == "maxent.jar") {
        rvs %>% writeLog('New area projection for clamped model', rvs$modSel, 'with extent coordinates:', 
                         coordsChar)
      } else if (rvs$clamp == F) {
        rvs %>% writeLog('New area projection for unclamped', rvs$modSel, 'with extent coordinates:', 
                         coordsChar)
      }
    }
    
    withProgress(message = "Masking environmental grids to projection extent...", {
      projMsk <- raster::crop(rvs$envs, newPoly)
      projMsk <- raster::mask(projMsk, newPoly)
    })
    
    modCur <- rvs$mods[[rvs$modSel]]
    
    withProgress(message = 'Projecting model to new area...', {
      if (rvs$comp6 == 'bioclim') {
        modProjArea <- dismo::predict(modCur, projMsk)
      } else if (rvs$comp6 == 'maxent') {
        if (rvs$algMaxent == "maxnet") {
          if (rvs$comp7.type == "raw") {pargs <- "exponential"} else {pargs <- rvs$comp7.type}
          modProjArea <- predictMaxnet(modCur, projMsk, type = pargs, clamp = rvs$clamp)
        } else if (rvs$algMaxent == "maxent.jar") {
          pargs <- paste0("outputformat=", rvs$comp7.type)
          modProjArea <- dismo::predict(modCur, projMsk, args = pargs)
        }
      }
      
      raster::crs(modProjArea) <- raster::crs(projMsk)
      # generate binary prediction based on selected thresholding rule 
      # (same for all Maxent prediction types because they scale the same)
      modProjArea.thr.call <- callModule(threshPred_MOD, "threshPred", modProjArea)
      modProjArea.thr <- modProjArea.thr.call()
      pjPred <- modProjArea.thr$pred
      rvs$comp8.thr <- modProjArea.thr$thresh
    })
    
    return(list(pjMsk=projMsk, pjPred=pjPred))
  })
}
