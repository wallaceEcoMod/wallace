#' @export

maxentPredTransform <- function(results, curModel, bgMask, predType, shinyLogs = NULL) {
  pargs <- paste0("outputformat=", predType) 
  smartProgress(shinyLogs, message = paste0("Generating ", predType, " prediction for model", curModel, "..."), {
    transPred <- dismo::predict(results$models[[curModel]], bgMask, args=pargs)
  })  
  return(transPred)
}