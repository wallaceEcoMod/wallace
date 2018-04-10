c5_partitionOccs <- function(occs, bg, method, kfolds=NULL, bgMsk=NULL, aggFact=NULL, logs = NULL, shiny = FALSE) {

  if (method == '') {
    logs %>% writeLog(type = 'error', "Please select a partitioning option.")
    return()
  }
  
  occs.xy <- occs %>% dplyr::select(longitude, latitude)
  bg.xy <- bg %>% dplyr::select(longitude, latitude)
  
  if (method == 'jack') {
    group.data <- ENMeval::get.jackknife(occs.xy, bg.xy)
    logs %>% writeLog("Occurrences partitioned by jackknife method for ", em(spName(occs)), ".")
  }
  
  if (method == 'rand') {
    if(is.null(kfolds)) {
      logs %>% writeLog(type = 'error', "Please specify a kfold value to use
                        the random partition function for ", em(spName(occs)), ".")
      return()
    }
    if (kfolds < 2) {
      logs %>% writeLog(type = 'error', "Please specify a kfold value greater than 1 for ", em(spName(occs)), ".")  
      return()
    }
    
    group.data <- ENMeval::get.randomkfold(occs.xy, bg.xy, kfolds)
    logs %>% writeLog("Occurrences partitioned by random k-fold (k = ", kfolds, ") for ", em(spName(occs)), ".")
  }
  
  if (method == 'block') {
    group.data <- ENMeval::get.block(occs.xy, bg.xy)
    logs %>% writeLog("Occurrences partitioned by block method for ", em(spName(occs)), ".")
  }
  
  if (method == 'cb1' | method == 'cb2') {
    if(is.na(aggFact) | aggFact <= 1) {
      logs %>% writeLog(type = 'error', "Please specify a positive aggregation 
                        factor greater than 1 for ", em(spName(occs)), ".")
      return()
    }
    if(is.null(bgMsk)) {
      logs %>% writeLog(type = 'error', "Please specify a background mask to use
                        checkerboard partition functions for ", em(spName(occs)), ".")
      return()
    }
    if(is.null(aggFact)) {
      logs %>% writeLog(type = 'error', "Please specify an aggregation factor to use
                        checkerboard partition functions for ", em(spName(occs)), ".")
      return()
    }
  }
  
  if(method == 'cb1') {
    if(shiny == TRUE) {
      withProgress(message = "Aggregating rasters...", {
        group.data <- ENMeval::get.checkerboard1(occs.xy, bgMsk, bg.xy, aggFact)
      })
    } else {
      group.data <- ENMeval::get.checkerboard1(occs.xy, bgMsk, bg.xy, aggFact)
    }
    logs %>% writeLog("Occurrences partitioned by checkerboard 1 method with 
                                 aggregation factor ", aggFact, " for ", em(spName(occs)), ".")
  }
  
  if(method == 'cb2') {
    if(shiny == TRUE) {
      withProgress(message = "Aggregating rasters...", {
        group.data <- ENMeval::get.checkerboard2(occs.xy, bgMsk, bg.xy, aggFact)
      })
    } else {
      group.data <- ENMeval::get.checkerboard2(occs.xy, bgMsk, bg.xy, aggFact)
    }
    logs %>% writeLog("Occurrences partitioned by checkerboard 2 method with 
                             aggregation factor ", aggFact, " for ", em(spName(occs)), ".")
  }
  
  return(group.data)
}
