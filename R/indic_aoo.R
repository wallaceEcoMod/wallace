##' @title  indic_aoo
#' @description Calculate AOO.
#'
#' @param r raster. Thresholded prediction raster. It could be NULL if occs provided.
#' @param occs data.frame. Table with occurrences. It could be NULL if raster
#' provided. If occs provided raster is ignored.
#' @param lon character. Column name of longitude.
#' @param lat character. Column name of latitude.
#' @param wktFrom character. Well-known text representation of coordinate reference systems
#'  of the provided data.
#' @param wktTo character. Well-known text representation of coordinate reference systems
#'  to calculate area.
#' @param logger stores all notification messages to be displayed in the Log
#'  Window of Wallace GUI. Insert the logger reactive list here for running in
#'  shiny, otherwise leave the default NULL
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @export
#'

indic_aoo <- function(r = NULL, occs = NULL, lon = NULL, lat = NULL,
                      wktFrom, wktTo, logger) {
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
        p.pts <- p.pts %>%
          dplyr::select(c(lon, lat)) %>%
          terra::vect(geom = c(lon, lat))
        terra::crs(p.pts) <- wktFrom
        rast_temp <- terra::rast(terra::ext(terra::project(p.pts, wktTo)) + 2000,
                                 resolution = 2000, crs = wktTo)
        rast_temp <- terra::project(rast_temp, wktFrom)
        AOOraster <- terra::rasterize(p.pts, rast_temp, field = 1, update = TRUE) %>%
          terra::trim()
        AOOarea <- terra::freq(AOOraster, value = 1)$count * 4
      }
    }
  } else if (!is.null(r)) {
    r <- terra::rast(r)
    ## Unsuitable for NAs
    r[r == 0] <- NA
    p.poly <- terra::as.polygons(r)
    rast_temp <- terra::rast(terra::ext(terra::project(p.poly, wktTo)) + 2000,
                             resolution = 2000, crs = wktTo,
                             vals = 1)
    rast_temp <- terra::project(rast_temp , wktFrom)
    AOOraster <- terra::mask(rast_temp, p.poly)
    AOOarea <- terra::freq(AOOraster, value = 1)$count * 4
  } else {
    logger %>% writeLog("Provide occurrences or raster (**).")
    return()
  }
  return(list(area = AOOarea, AOOraster = methods::as(AOOraster, "Raster")))
}
