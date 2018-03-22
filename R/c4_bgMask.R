c4_bgMask <- function(occs, envs, bgExt, logs=NULL, shiny=FALSE) {
  if (is.null(bgExt)) {
    logs %>% writeLog(type = 'error', "Before sampling background points, 
                      define the background extent.")
    return()
  }
  n <- occs$taxon_name[1]
  # mask envs by background extent
  withProgress(message = paste0("Masking rasters for ", n, "..."), {
    bgCrop <- raster::crop(envs, bgExt)
    bgMask <- raster::mask(bgCrop, bgExt)
  })
  logs %>% writeLog(n, ': Environmental data masked.')
  
  return(bgMask)
}