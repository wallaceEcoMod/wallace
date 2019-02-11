
#' @title samp_userBiasFile
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param csvPath x
#' @param csvName x
#' @param shinyLogs x
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
# @family - a family name. All functions that have the same family tag will be linked
#  in the documentation.

#' @export

# consolidate this with getuserENvs

samp_userBiasFileCheck <- function(biasLayer,envs, shinyLogs=NULL){
  
  if(!raster::crs(biasLayer)==raster::crs(envs)) {
    shinyLogs %>% writeLog(type = "warning",'Bias file and env layers have different projection. Attempting to reproject.')
    biasLayer=try(raster::projectRaster(biasLayer,envs))
    if(class(biasLayer)=='try-error'){
      shinyLogs %>% writeLog(type = "error",paste0('Could not reproject bias layer to the projection of environmental layers. You must upload a bias file that has the same projection as the environmental layers, which is: ',raster::crs(envs)))
      return() # what do we want to happen if this breaks?
    }
  }
  if(!sum(raster::values(biasLayer)==1)) {
    shinyLogs %>% writeLog(type = "warning",
                           "The bias file isn't normalized (sums to 1) so we're doing that for you")
    values(biasLayer)=values(biasLayer)/sum(values(biasLayer),na.rm=T)
  }
  return(biasLayer)
}

