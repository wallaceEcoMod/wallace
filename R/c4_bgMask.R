c4_bgMask <- function(occs, envs, bgExt, shinyLogs=NULL) {
  if (is.null(bgExt)) {
    shinyLogs %>% writeLog(type = 'error', "Before sampling background points, define the background extent.")
    return()
  }
  # mask envs by background extent
  smartProgress(shinyLogs, message = paste0("Masking rasters for ", spName(occs), "..."), {
    bgCrop <- raster::crop(envs, bgExt)
    bgMask <- raster::mask(bgCrop, bgExt)
  })
  
  shinyLogs %>% writeLog(em(spName(occs)), ': Environmental data masked.')
  
  return(bgMask)
}