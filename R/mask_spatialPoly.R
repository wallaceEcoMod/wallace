#' @title mask_spatialPoly
#' @description x
#'
#' @details
#' x
#'
#' @param bgShp_path Path to the user provided shapefile
#' @param bgShp_name Name of the user porvided shapefile
#' @param sdm x
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

mask_spatialPoly <- function(bgShp_path, bgShp_name, sdm,
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
      polyData <- rgdal::readOGR(file.path(pathdir, bgShp_name)[i])
    })

  } else {
    logger %>%
      writeLog(type = 'error',
               'Please enter shapefile (.shp, .shx, .dbf).')
    return()
  }

  if (is.na(raster::crs(polyData, asText = TRUE))) {
    logger %>% writeLog(
      type = 'warning', hlSpp(spN),
      "Projection not found for shapefile. It is assume that shapefile datum ",
      "is WGS84 (**)"
    )
  }
  if (!rgeos::gIntersects(methods::as(raster::extent(sdm), 'SpatialPolygons'),
                          polyData)) {
    logger %>% writeLog(
      type = 'error', hlSpp(spN),
      "Shapefile does not match with sdm extent. Please specify a new polygon. (**)"
    )
    return()
  }

  smartProgress(logger, message = "Intersecting spatial data ...", {
    sdm <- terra::rast(sdm)
    sdm <- sdm >= 0

    polR <- sf::st_as_sf(terra::as.polygons(sdm, trunc = TRUE, dissolve = TRUE,
                                            values = TRUE),
                         as_points = FALSE, merge = TRUE)
    polyData <- rgeos::gBuffer(polyData, byid = TRUE, width = 0)
    polyData <- sf::st_as_sf(polyData) %>%
      dplyr::mutate(
        dplyr::across(where(~is.character(.)),
                      ~tidyr::replace_na(.x, "NA"))
      )
    # Check for NAs
    v <- terra::extract(sdm, polyData)
    v <- is.na(unlist(v))

    if (sum(v) == length(v)) {
      logger %>% writeLog(
        type = 'error', hlSpp(spN),
        paste("The polygon just included NA values.",
              "Please, select a polygon that intersects model prediction.(**)")
      )
      return()
    }

    if (sum(v) > 0) {
      logger %>% writeLog(
        type = 'warning', hlSpp(spN),
        paste("The polygon selected included some cells with NA values.",
              "You cannot changes the predictions (suitable or unsuitable),",
              "in these cells.(**)")
      )
    }
    sf::st_crs(polyData) <- sf::st_crs(polR)
    spatialPoly <- sf::as_Spatial(sf::st_intersection(sf::st_make_valid(polyData),
                                                      sf::st_make_valid(polR)))
  })
  logger %>% writeLog(hlSpp(spN), "Spatial data uploaded.")
  return(spatialPoly)
}
