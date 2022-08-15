#'
#' @title  indic_raster
#' @description Upload user-specified SDM prediction
#'
#' @details
#' See Examples.
#'
#' @param rasPath character of path to rasters, must be the full path including file name and extension.
#' Filename need to be name with genus_species format.
#' @param rasName character vector of raster names to be assigned to loaded rasters
#' @param logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
# @note

# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export
#'
#'

indic_raster <- function(rasPath, rasName, logger = NULL) {
  rasterName <- fileNameNoExt(rasName)
  smartProgress(logger, message = "Uploading user-specified SDM (**)...", {
    r <- raster::raster(rasPath)
   # r <- raster::trim(r) ##this creates a problem when comparing resolutions for overlapping rasters
    names(r) <- fileNameNoExt(rasName)
    extPoly <- raster::extent(r)
    if (extPoly@xmin < -180 | extPoly@xmax > 180 |
        extPoly@ymin < -90 | extPoly@ymax > 90) {
      logger %>%
        writeLog(
          type = "error", "Wrong extent projection. '", rasName,"' cannot be uploaded. (**)")
      return()
    }
    extPoly <- methods::as(extPoly, 'SpatialPolygons')
  })

  return(list(sdm = r, extSdm = extPoly))
}
