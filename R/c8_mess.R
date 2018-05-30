c8_mess <- function(occs, bg, bgMsk, mapProj, time, shinyLogs = NULL) {
  
  occsVals <- occs[[names(bgMsk)]]
  bgVals <- bg[[names(bgMsk)]]
  allVals <- rbind(occsVals, bgVals)
  
  smartProgress(shinyLogs, message = "Generating MESS map...", {
    mss <- suppressWarnings(dismo::mess(mapProj, allVals))
    # for mapping purposes, set all infinite values to NA
    mss[is.infinite(mss)] <- NA
    shinyLogs %>% writeLog("Generated MESS map for", time, ".")
  })
  
  return(mss)
}