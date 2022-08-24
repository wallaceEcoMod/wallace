#' @title indic_inputPoly
#' @description x
#'
#' @details x
#' @param bgShp_path Path to the user provided shapefile
#' @param bgShp_name Name of the user provided shapefile
#' @param overlapArea x. An sf object.
#' @param logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#' otherwise leave the default NULL
#' @param spN x
# @keywords

# @keywords
#'
#' @return x
#' @author Gonzalo E. Pinilla-Buitrago < gpinillabuitrago@@gradcenter.cuny.edu>
# @note

#' @export

indic_inputPoly <- function(bgShp_path, bgShp_name, overlapArea,
                             logger = NULL, spN = NULL) {
  pathdir <- dirname(bgShp_path)
  pathfile <- basename(bgShp_path)
  # get extensions of all input files
  exts <- sapply(strsplit(bgShp_name, '\\.'), FUN = function(x) x[2])
  if ('shp' %in% exts) {
    if (length(exts) < 3) {
      logger %>%
        writeLog(type = 'error', hlSpp(spN),
                 paste0('If entering a shapefile, please select all the ',
                        'following files: .shp, .shx, .dbf.'))
      return()
    }
    # get index of .shp
    i <- which(exts == 'shp')
    if (!file.exists(file.path(pathdir, bgShp_name)[i])) {
      file.rename(bgShp_path, file.path(pathdir, bgShp_name))
    }
    smartProgress(logger, message = "Uploading shapefile ...", {
      polyData <- sf::read_sf(file.path(pathdir, bgShp_name)[i])
    })
  } else {
    logger %>%
      writeLog(type = 'error', hlSpp(spN),
               'Please enter shapefile (.shp, .shx, .dbf).')
    return()
  }
  if (is.na(sf::st_crs(polyData))) {
    logger %>% writeLog(
      type = 'warning', hlSpp(spN),
      "Projection not found for shapefile. It is assume that shapefile datum ",
      "is WGS84 (**)"
    )
    sf::st_crs(polyData) <- 4326
  }
  if (sf::st_crs(polyData)$input != "EPSG:4326") {
    polyData <- sf::st_transform(polyData, 4326)
    logger %>% writeLog(
      type = 'warning', hlSpp(spN),
      "Original coordinate reference system (CRS) is not WGS84 (EPSG:4326). ",
      "Shapefile was reprojected to this CRS. (**)"
    )
  }
  if (sum(lengths(sf::st_intersects(polyData, overlapArea))) == 0) {
    logger %>% writeLog(
      type = 'error', hlSpp(spN),
      "Shapefile does not intersect the area to overlap. Please specify a new polygon. (**)"
    )
    return()
  }
  smartProgress(logger, message = "Intersecting spatial data ...", {
    nmOverlap <- names(overlapArea)
    nmOverlap <- nmOverlap[!(nmOverlap %in% "geometry")]
    spatialPoly <- sf::st_intersection(polyData, overlapArea) %>%
      dplyr::select(-nmOverlap)
  })
  logger %>% writeLog(hlSpp(spN), "Spatial data uploaded.")
  return(spatialPoly)
}
