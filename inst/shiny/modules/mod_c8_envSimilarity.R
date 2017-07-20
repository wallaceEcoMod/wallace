envSimilarity_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

envSimilarity_MOD <- function(input, output, session, rvs) {
  req(rvs$envs, rvs$mods, rvs$predCur)
  
  if (is.null(rvs$polyXY)) {
    rvs %>% writeLog(type = 'error', 'Select projection extent first.')
    return()
  }
  
  if (is.null(rvs$projCur)) {
    rvs %>% writeLog(type = 'error', 'Project to new area or time first.')
    return()
  }
  
  occs.xy <- rvs$occs %>% dplyr::select(longitude, latitude)
  
  withProgress(message = "Generating MESS map...", {
    occVals <- raster::extract(rvs$envs, occs.xy)
    pjMESS <- suppressWarnings(dismo::mess(rvs$projCur, occVals))
    if (rvs$projType == 'area') {
      rvs %>% writeLog("Generated MESS map for present.")
    } else if (rvs$projType == 'time') {
      rvs %>% writeLog("Generated MESS map for", paste0('20', rvs$pjTimePar$time), 
                     "for GCM", GCMlookup[rvs$pjTimePar$GCM], 
                     "under RCP", as.numeric(rvs$pjTimePar$rcp)/10.0, ".")
    }
  })
  
  pjMESS.vals <- rasVals <- values$mess@data@values
  values$rasValsMESS <- na.omit(rasVals)
  
  # if (sum(is.infinite(rasVals)) > 0) {
  #   # find max after removing infinite values
  #   x <- rasVals
  #   x[is.infinite(x)] <- 0
  #   rasValsMax <- max(x)
  # }
  # set infinite values to max
  
  values$legPalMESS <- colorNumeric(rev(RColorBrewer::brewer.pal(n=11, name='Spectral')), values$rasValsMESS, na.color='transparent')
  
}
