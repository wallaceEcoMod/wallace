c6_bioclim  <- function(occs, bgPts, occsGrp, bgGrp, bgMsk, logs = NULL, shiny = FALSE) {
  if (is.null(occsGrp)) {
    logs %>% writeLog(type = 'error', "Before building a model, partition 
                      occurrences in component 5.")
    return()
  }
  
  occs.xy <- occs %>% dplyr::select(longitude, latitude)
  
  e <- BioClim_eval(occs.xy, bgPts, occsGrp, bgGrp, bgMsk)
  
  logs %>% writeLog("BIOCLIM ran successfully and output evaluation results.")
  
  return(e)
  
}