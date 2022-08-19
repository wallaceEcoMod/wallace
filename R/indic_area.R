#' @title  indic_area
#' @description Calculate Range size in square kilometers.
#'
#' @param r raster. Thresholded prediction raster. It could be NULL if occs provided.
#' @param wkt character. Well-known text representation of coordinate reference systems
#'  to calculate area.
#' @param logger stores all notification messages to be displayed in the Log
#'  Window of Wallace GUI. insert the logger reactive list here for running in
#'  shiny, otherwise leave the default NULL
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @export
#'

indic_area <- function(r, wkt, logger = NULL) {
  r <- terra::rast(r)
  ## Unsuitable for NAs
  r[r == 0] <- NA
  ## Raster to polygon
  p <- terra::as.polygons(r)
  ## Project to SR-ORG 8287
  p <- terra::project(p, y = wkt)
  areaRange <- terra::expanse(p, unit = "km")
  return(areaRange)
}
