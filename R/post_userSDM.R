#'
#' @title  post_userSDM
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

post_userSDM <- function(rasPath, rasName, logger = NULL) {
  spName <- fileNameNoExt(rasName)
  spName <- paste0(toupper(substring(spName, 1, 1)),
                   substring(spName, 2, nchar(spName)))
  # Check genus and species name on file
  if (!(length(strsplit(spName, " ")[[1]]) == 2 | length(strsplit(spName, "_")[[1]]) == 2)) {
    logger %>%
      writeLog(type = 'warning', "'",
               rasName, "' without genus and species names (e.g. Canis_lupus.tif).")
    return()
  }
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
    extPoly <- as(extPoly, 'SpatialPolygons')
  })

  if (is.na(raster::crs(r))) {
    logger %>%
      writeLog(
        type = "warning", hlSpp(spName),
        'Input rasters have undefined coordinate reference system (CRS). ',
        'Mapping functionality in components Visualize Model Results and ',
        'Project Model will not work. If you wish to map rasters in these ',
        'components, please define their projections and upload again. ',
        'See guidance text in this module for more details.')
    return()
  }
  return(list(sdm = r, extSdm = extPoly))
}
