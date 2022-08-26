#'
#' @title  mask_userSDM
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
#' @param spN Species name
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

mask_userSDM <- function(rasPath, rasName, logger = NULL, spN = NULL)  {
  smartProgress(logger, message = "Uploading user-specified SDM (**)...", {
    r <- raster::raster(rasPath)
    r <- raster::trim(r)
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

  if (is.na(raster::crs(r))) {
    logger %>%
      writeLog(
        type = "warning", hlSpp(spN),
        'Input rasters have undefined coordinate reference system (CRS). ',
        'Mapping functionality in components Visualize and ',
        'Transfer will not work. If you wish to map rasters in these ',
        'components, please define their projections and upload again. ',
        'See guidance text in this module for more details.')
    return()
  }
  return(list(sdm = r, extSdm = extPoly))
}
