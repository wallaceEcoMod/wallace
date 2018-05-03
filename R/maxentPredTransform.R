maxentPredTransform <- function(results, bgMask, predType, shinyLogs = NULL) {
  pargs <- paste0("outputformat=", predType) 
  smartProgress(shinyLogs, message = paste0("Generating ", predType, " predictions..."), {
    transPredsList <- sapply(results$models, function(x) dismo::predict(x, bgMask, args=pargs))
    transPreds <- raster::stack(transPredsList)
    names(transPreds) <- names(results$predictions)
  })  
}