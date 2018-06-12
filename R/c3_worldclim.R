#' @title c3_worldclim
#' @description download worldclim variables. see www.worldclim.com
#'
#' @details
#' See Examples.
#'
#' @param bcRes numeric resolution of the climatic layers
#' @param bcSel list of boolean data. selected variables
#' 
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

c3_worldclim<- function(bcRes, bcSel, shinyLogs=NULL){
  
  if(bcRes == '') {
    shinyLogs %>% writeLog(type = 'error', 'Select a raster resolution.')
    return()
  }
  
  smartProgress(shinyLogs, message = "Retrieving WorldClim data...", {
    if(bcRes == 0.5) {
      wcbc <- raster::getData(name = "worldclim", var = "bio", res = bcRes, 
                              lon = mapCntr()[1], lat = mapCntr()[2])
    }else{
      wcbc <- raster::getData(name = "worldclim", var = "bio", res = bcRes)
      wcbc <- wcbc[[bcSel]]
    }
  }) 
  
  if (raster::nlayers(wcbc) == 19) {
    bcSel <- 'bio1-19'
  } else {
    bcSel <- paste(names(wcbc), collapse = ", ")
  }
  shinyLogs %>% writeLog("WorldClim bioclimatic variables ", bcSel, " at ", 
                    bcRes, " arcmin resolution.")
  
  # change names if bio01 is bio1, and so forth
  i <- grep('bio[0-9]$', names(wcbc))
  editNames <- paste('bio', sapply(strsplit(names(wcbc)[i], 'bio'), function(x) x[2]), sep='0')
  names(wcbc)[i] <- editNames
  
  return(wcbc)
}
