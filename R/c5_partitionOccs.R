
#' @title c5_partitionOccs
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param occs
#' @param bg
#' @param method
#' @param kfolds
#' @param bgMask
#' @param aggFact
#' @param shinyLogs
# @keywords
#'
# @examples
#'
#'
# @return 
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

c5_partitionOccs <- function(occs, bg, method, kfolds=NULL, bgMask=NULL, aggFact=NULL, shinyLogs = NULL) {

  if (method == '') {
    shinyLogs %>% writeLog(type = 'error', "Please select a partitioning option.")
    return()
  }
  
  occs.xy <- occs %>% dplyr::select(longitude, latitude)
  bg.xy <- bg %>% dplyr::select(longitude, latitude)
  
  if (method == 'jack') {
    group.data <- ENMeval::get.jackknife(occs.xy, bg.xy)
    shinyLogs %>% writeLog("Occurrences partitioned by jackknife method for ", em(spName(occs)), ".")
  }
  
  if (method == 'rand') {
    if(is.null(kfolds)) {
      shinyLogs %>% writeLog(type = 'error', "Please specify a kfold value to use
                        the random partition function for ", em(spName(occs)), ".")
      return()
    }
    if (kfolds < 2) {
      shinyLogs %>% writeLog(type = 'error', "Please specify a kfold value greater than 1 for ", em(spName(occs)), ".")  
      return()
    }
    
    group.data <- ENMeval::get.randomkfold(occs.xy, bg.xy, kfolds)
    shinyLogs %>% writeLog("Occurrences partitioned by random k-fold (k = ", kfolds, ") for ", em(spName(occs)), ".")
  }
  
  if (method == 'block') {
    group.data <- ENMeval::get.block(occs.xy, bg.xy)
    shinyLogs %>% writeLog("Occurrences partitioned by block method for ", em(spName(occs)), ".")
  }
  
  if (method == 'cb1' | method == 'cb2') {
    if(is.na(aggFact) | aggFact <= 1) {
      shinyLogs %>% writeLog(type = 'error', "Please specify a positive aggregation 
                        factor greater than 1 for ", em(spName(occs)), ".")
      return()
    }
    if(is.null(bgMask)) {
      shinyLogs %>% writeLog(type = 'error', "Please specify a background mask to use
                        checkerboard partition functions for ", em(spName(occs)), ".")
      return()
    }
    if(is.null(aggFact)) {
      shinyLogs %>% writeLog(type = 'error', "Please specify an aggregation factor to use
                        checkerboard partition functions for ", em(spName(occs)), ".")
      return()
    }
  }
  
  if(method == 'cb1') {
    smartProgress(shinyLogs, message = "Aggregating rasters...", {
      group.data <- ENMeval::get.checkerboard1(occs.xy, bgMask, bg.xy, aggFact)
    })
    
    shinyLogs %>% writeLog("Occurrences partitioned by checkerboard 1 method with 
                                 aggregation factor ", aggFact, " for ", em(spName(occs)), ".")
  }
  
  if(method == 'cb2') {
    smartProgress(shinyLogs, message = "Aggregating rasters...", {
      group.data <- ENMeval::get.checkerboard2(occs.xy, bgMask, bg.xy, aggFact)
    })
    
    shinyLogs %>% writeLog("Occurrences partitioned by checkerboard 2 method with 
                             aggregation factor ", aggFact, " for ", em(spName(occs)), ".")
  }
  
  return(group.data)
}
