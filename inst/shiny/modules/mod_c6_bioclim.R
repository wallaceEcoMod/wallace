
bioclim_UI <- function(id) {
  ns <- NS(id)
  tagList(
  )
}

bioclim_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$occsGrp)) {
      rvs %>% writeLog(type = 'error', "Before building a model, partition 
                       occurrences in component 5.")
      return()
    }

    occs.xy <- rvs$occs %>% dplyr::select(longitude, latitude)
    
    e <- BioClim_eval(occs.xy, rvs$bgPts, rvs$occsGrp, rvs$bgGrp, rvs$bgMsk)

    # occVals <- raster::extract(e$predictions, values$modParams$occ.pts)
    # 
    # values$mtps <- min(occVals)  # apply minimum training presence threshold
    # 
    # # Define 10% training presence threshold
    # if (length(occVals) < 10) {  # if less than 10 occ values, find 90% of total and round down
    #   n90 <- floor(length(occVals) * 0.9)
    # } else {  # if greater than or equal to 10 occ values, round up
    #   n90 <- ceiling(length(occVals) * 0.9)
    # }
    # 
    # values$p10s <- rev(sort(occVals))[n90]  # apply 10% training presence threshold
    
    rvs %>% writeLog("BIOCLIM ran successfully and output evaluation results.")
      
    return(e)
  })
}
