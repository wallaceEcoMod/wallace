
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
    colnames(rvs$bgPts) <- names(occs.xy)
    
    e <- ENMeval::ENMevaluate(occs = occs.xy, envs = rvs$bgMsk, bg = rvs$bgPts,
                              algorithm = "bioclim", partitions = "user",
                              occ.grp = rvs$occsGrp, bg.grp = rvs$bgGrp)

    rvs %>% writeLog("BIOCLIM ran successfully and output evaluation results.")
      
    return(e)
  })
}
