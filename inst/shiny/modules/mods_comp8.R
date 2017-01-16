comp8_selProjExt <- function() {
  if (is.null(values$df)) return()
  if (is.null(values$polyPts2)) return()

  values$polyPts2 <- unique(values$polyPts2)  # remove phantom first row after reset
  if (nrow(values$polyPts2) < 3) {
    writeLog('<font color="red"><b>! ERROR</b></font> : Please define a shape with at least 3 sides.')
    return()
  }
  values$polyErase <- TRUE  # turn on to signal to prevent the use of an existing map click
  values$polyID <- values$polyID + 1

  values$poly2 <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(values$polyPts2)), ID=values$polyID)))  # create new polygon from coords
  proxy %>% addPolygons(values$polyPts2[,1], values$polyPts2[,2], weight=3, fill=FALSE, color='red', layerId='poly2Sel')

  x <- round(values$polyPts2, digits = 2)  # round all coords to 2 decimal digits
  coordsChar <- paste(apply(x, 1, function(b) paste0('(',paste(b, collapse=', '),')')), collapse=', ')  # concatanate coords to a single character
  isolate(writeLog(paste0('> Defined projection extent to: ', coordsChar)))
}

comp8_pjArea <- function(modelSel, predForm, enmSel) {
  if (is.null(values$poly2)) {
    writeLog('<font color="red"><b>! ERROR</b></font> : Select projection extent first.')
    return()
  }

  # reset pjTime
  values$pjTime <- NULL

  if (is.null(values$projMsk)) {
    withProgress(message = "Clipping environmental data to current extent...", {
      msk <- raster::crop(values$preds, values$poly2)
      values$projMsk <- raster::mask(msk, values$poly2)
    })
  }

  writeLog('> PROJECTING to new area.')
  curMod <- values$evalMods[[as.numeric(modelSel)]]
  values$pjArea <- dismo::predict(curMod, values$projMsk)
  rasVals <- raster::values(values$pjArea)

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

  proxy %>% addLegend("topright", pal = legPal, title = "Predicted Suitability",
                      values = rasVals, layerId = 'r2Legend', labFormat = reverseLabels(reverse_order=TRUE))
  proxy %>% addRasterImage(values$pjArea, colors = rasPal, group = 'r2', layerId = 'r2')
}

comp8_pjTime <- function(modelSel, predForm, enmSel, bcRes, selRCP, selGCM, selTime) {
  if (is.null(values$poly2)) {
    writeLog('<font color="red"><b>! ERROR</b></font> : Select projection extent first.')
    return()
  }

  if (bcRes == 0.5) {
    writeLog('<font color="red"><b>! ERROR</b></font> : Project to New Time currently only available with resolutions >30 arc seconds.')
    return()
  }

  # reset pjArea
  values$pjArea <- NULL

  # code taken from dismo getData() function to catch if user is trying to download a missing combo of gcm / rcp
  gcms <- c('AC', 'BC', 'CC', 'CE', 'CN', 'GF', 'GD', 'GS', 'HD', 'HG', 'HE', 'IN', 'IP', 'MI', 'MR', 'MC', 'MP', 'MG', 'NO')
  rcps <- c(26, 45, 60, 85)
  m <- matrix(c(0,1,1,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,0,1,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1), ncol=4)
  i <- m[which(selGCM == gcms), which(selRCP == rcps)]
  if (!i) {
    writeLog('<font color="red"><b>! ERROR</b></font> : This combination of model and rcp is not available. Please make a different selection.')
    return()
  }

  withProgress(message = paste("Retrieving WorldClim data for", selTime, selRCP, "..."), {
    values$projTimeVars <- raster::getData('CMIP5', var = "bio", res = bcRes,
                                           rcp = selRCP, model = selGCM, year = selTime)
  })

  withProgress(message = "Clipping environmental data to current extent...", {
    msk <- raster::crop(values$projTimeVars, values$poly2)
    values$projMsk <- raster::mask(msk, values$poly2)
    names(values$projMsk) <- names(values$preds)  # make names same as original predictors
  })

  withProgress(message = paste("Projecting to", selTime, "for GCM", selGCM, "under RCP", as.character(selRCP/10.0), "..."), {
    curMod <- values$evalMods[[as.numeric(modelSel)]]
    values$pjTime <- dismo::predict(curMod, values$projMsk)
    rasVals <- raster::values(values$pjTime)
  })

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
  if (is.null(values$poly2)) {
    writeLog('<font color="red"><b>! ERROR</b></font> : Select projection extent first.')
    return()
  }

  if (is.null(values$projMsk)) {
    withProgress(message = "Clipping environmental data to current extent...", {
      msk <- raster::crop(values$preds, values$poly2)
      values$projMsk <- raster::mask(msk, values$poly2)
    })
  }

  if (is.null(values$pjTime)) {
    period <- 'present'
  } else if (is.null(values$pjArea)) {
    period <- paste(selTime, "for GCM", selGCM, "under RCP", as.character(selRCP/10.0))
  }

  withProgress(message = paste("Generating MESS map for", period, "..."), {
    occVals <- raster::extract(values$preds, cbind(values$df$longitude, values$df$latitude))
    values$mess <- suppressWarnings(dismo::mess(values$projMsk, occVals))
  })

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

  pal <- colorNumeric(RColorBrewer::brewer.pal(n=11, name='Spectral'), rasVals, na.color='transparent')

  proxy %>% addLegend("topright", pal=pal, title = "MESS Values",
                      values = rasVals, layerId = 'mess')
  proxy %>% addRasterImage(values$mess, layerId = 'mess')
}
