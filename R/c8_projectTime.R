
#' @title c8_projectTim
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param results
#' @param curModel
#' @param envs
#' @param outputType
#' @param polyPjXY
#' @param polyPjID
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

c8_projectTime <- function(results, curModel, envs, outputType, polyPjXY, polyPjID, shinyLogs = NULL) {
  # create new spatial polygon from coordinates
  newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(polyPjXY)), ID = polyPjID)))  
  
  # concatanate coords to a single character
  xy.round <- round(polyPjXY, digits = 2)
  xy.round <- xy.round[-nrow(xy.round),]  # remove last point that completes polygon
  coordsChar <- paste(apply(xy.round, 1, function(b) paste0('(',paste(b, collapse=', '),')')), collapse=', ')  
  shinyLogs %>% writeLog('New time projection for model', curModel, 'with extent coordinates:', coordsChar)
  
  smartProgress(shinyLogs, message = "Clipping environmental data to current extent...", {
    pjtMsk <- raster::crop(envs, newPoly)
    pjtMsk <- raster::mask(pjtMsk, newPoly)
  })
  
  smartProgress(shinyLogs, message = ("Projecting to new time..."), {
    pargs <- paste0("outputformat=", outputType)
    modProjTime <- dismo::predict(results$models[[curModel]], pjtMsk, args = pargs)
    
    return(list(projExt=pjtMsk, projTime=modProjTime))
  })
  
}