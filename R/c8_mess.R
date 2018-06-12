#' @export

c8_mess <- function(occs, bg, bgMsk, projExtRas, time, shinyLogs = NULL) {
  
  occsVals <- occs[,names(bgMsk)]
  bgVals <- bg[,names(bgMsk)]
  allVals <- rbind(occsVals, bgVals)
  
  # rename rasters to match originals
  projExtRas2 <- projExtRas
  names(projExtRas2) <- names(bgMsk)
  
  smartProgress(shinyLogs, message = "Generating MESS map...", {
    mss <- suppressWarnings(dismo::mess(projExtRas2, allVals))
    # for mapping purposes, set all infinite values to NA
    mss[is.infinite(mss)] <- NA
    shinyLogs %>% writeLog("Generated MESS map for", time, ".")
  })
  
  return(mss)
}