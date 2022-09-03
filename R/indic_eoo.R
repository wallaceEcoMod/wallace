#' @title  indic_eoo
#' @description Calculate EOO.
#'
#' @param r raster. Thresholded prediction raster. It could be NULL if occs provided.
#' @param occs data.frame. Table with occurrences. It could be NULL if raster
#' provided. If occs provided raster is ignored.
#' @param lon character. Column name of longitude.
#' @param lat character. Column name of latitude.
#' @param wkt character. Well-known text representation of coordinate reference systems
#'  to calculate area.
#' @param logger stores all notification messages to be displayed in the Log
#'  Window of Wallace GUI. Insert the logger reactive list here for running in
#'  shiny, otherwise leave the default NULL
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @export
#'

indic_eoo <- function(r = NULL, occs = NULL, lon = NULL, lat = NULL,
                      wkt, logger) {
  if (!is.null(occs)) {
    if (is.null(lon) | is.null(lat)) {
      logger %>% writeLog("No longitude and/or latitude name provided (**).")
      return()
    } else {
      p.pts <- occs
      if (sum(c(lon, lat) %in% names(p.pts)) != 2) {
        logger %>% writeLog("Longitude and/or latitude names not found (**).")
        return()
      } else {
        p.pts <- p.pts %>% dplyr::select(c(lon, lat))
      }
    }
  } else if (!is.null(r)) {
    r <- terra::rast(r)
    r[r == 0] <- NA
    p.pts <- terra::as.points(r) %>%
      terra::geom() %>% data.frame() %>%
      dplyr::select(tidyselect::all_of(c("x", "y")))
  } else {
    logger %>% writeLog("Provide occurrences or raster (**).")
    return()
  }
  wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  # Create a minimum convex polygon around the occurrences
  eoo <- changeRangeR::mcp(p.pts, wgs84)
  # Project to WCEA (IUCN recommendation)
  eooPr <- terra::project(terra::vect(eoo), y = wkt)
  areaEOO <- terra::expanse(eooPr, unit = "km")
  return(list(area = areaEOO, eooPoly = eoo))
}
