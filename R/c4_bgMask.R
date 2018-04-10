c4_bgMask <- function(occs, envs, bgExt, logs=NULL, shiny=FALSE) {
  if (is.null(bgExt)) {
    logs %>% writeLog(type = 'error', "Before sampling background points, define the background extent.")
    return()
  }
  # mask envs by background extent
  withProgress(message = paste0("Masking rasters for ", spName(occs), "..."), {
    bgCrop <- raster::crop(envs, bgExt)
    bgMask <- raster::mask(bgCrop, bgExt)
  })
  logs %>% writeLog(em(spName(occs)), ': Environmental data masked.')
  
  return(bgMask)
}