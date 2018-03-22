c4_bgSample <- function(occs, bgMask, bgPtsNum, logs=NULL, shiny=FALSE) {
  # sample random background points
  withProgress(message = "Generating background points...", {
    # rvals <- raster::getValues(bgMask)
    # num.vals <- sum(!is.na(rvals))
    # pct <- round((bgPtsNum / num.vals) * 100, digits = 2)
    bgXY <- dismo::randomPoints(bgMask, bgPtsNum)
    bgXY <- bgXY %>% as.data.frame() %>% dplyr::select(longitude = x, latitude = y)
    bgXY.num <- nrow(bgXY)
  })
  logs %>% writeLog(occs$taxon_name[1], ': Random background points sampled (n =', bgPtsNum, 
                    ',', bgXY.num, 'points generated.')
  
  return(bgXY)
}