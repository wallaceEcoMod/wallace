#' @title  indic_raster
#' @description Upload user-specified SDM prediction
#' @param rasPath character of path to rasters, must be the full path including
#'  file name and extension.
#' @param rasName character vector of raster names to be assigned to loaded
#'  rasters.
#' @param overlapArea x. An sf object.
#' @param logger stores all notification messages to be displayed in the
#'  Log Window of Wallace GUI. insert the logger reactive list here for
#'  running in shiny, otherwise leave the default NULL.
#' @param spN Species name
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @export

indic_raster <- function(rasPath, rasName, overlapArea,
                         logger = NULL, spN = NULL) {
  rasterName <- fileNameNoExt(rasName)
  smartProgress(logger, message = "Uploading user-specified raster (**)...", {
    r <- raster::raster(rasPath)
    r <- raster::trim(r)
    names(r) <- "overlapRaster"
    extPoly <- raster::extent(r)
    if (extPoly@xmin < -180 | extPoly@xmax > 180 |
        extPoly@ymin < -90 | extPoly@ymax > 90) {
      logger %>%
        writeLog(
          type = "error", hlSpp(spN),
          "Wrong extent projection. '", rasterName,
          "' cannot be uploaded. (**)")
      return()
    }
    if (sum(lengths(sf::st_intersects(sf::st_as_sfc(sf::st_bbox(r)),
                                      overlapArea))) == 0) {
      logger %>% writeLog(
        type = 'error', hlSpp(spN),
        "Overlap raster does not match with range map extent. ",
        "Please specify a raster. (**)"
      )
      return()
    }
  })
  logger %>% writeLog(hlSpp(spN), "User raster file loaded.")
  return(r)
}
