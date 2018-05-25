#' @export

maxentPredTransform <- function(mod, bgMask, predType, shinyLogs = NULL) {
  pargs <- paste0("outputformat=", predType) 
  smartProgress(shinyLogs, message = paste0("Generating ", predType, " prediction..."), {
    transPred <- dismo::predict(mod, bgMask, args=pargs)
  })  
  return(transPred)
}