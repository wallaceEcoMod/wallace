c6_bioclim  <- function(occs, bg, occsGrp, bgGrp, bgMsk, logs = NULL, shiny = FALSE) {
  
  occs.xy <- occs %>% dplyr::select(longitude, latitude)
  
  bg.xy <- bg %>% dplyr::select(longitude, latitude)
  
  e <- BioClim_eval(occs.xy, bg.xy, occsGrp, bgGrp, bgMsk)
  
  n <- formatSpName(occs$taxon_name[1])
  logs %>% writeLog("BIOCLIM ran successfully for", n, "and output evaluation results.")
  
  return(e)
}