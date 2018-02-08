c5_partitionNonSpat <- function(occs, bgPts, partNspSel, kfolds, logs = NULL, shiny = FALSE) {
  # if (is.null(rvs$bgMsk)) {
  #   logs %>% writeLog(type = 'error', "Before partitioning occurrences,   
  #                      mask your environmental variables by your background extent.")
  #   return()
  # }
  if (partNspSel == '') {
    logs %>% writeLog(type = 'error', "Please select a partitioning option.")
    return()
  }
  
  
  occs.xy <- occs %>% dplyr::select(longitude, latitude)
  
  if (partNspSel == 'jack') {
    group.data <- ENMeval::get.jackknife(occs.xy, bgPts)
    logs %>% writeLog("Occurrences partitioned by jackknife method.")
  } else if (partNspSel == 'rand') {
    group.data <- ENMeval::get.randomkfold(occs.xy, bgPts, kfolds)
    logs %>% writeLog("Occurrences partitioned by random k-fold (k = ", kfolds, ").")
  }
  
  return(group.data)
}