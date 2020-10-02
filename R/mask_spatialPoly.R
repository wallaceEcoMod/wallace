#' @title mask_spatialPoly
#' @description x
#'
#' @details
#' x
#'
#' @param bgShp_path Path to the user provided shapefile
#' @param bgShp_name Name of the user porvided shapefile
#' @param bgExt x
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

mask_spatialPoly <- function(bgShp_path, bgShp_name, bgExt,
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
    # read in shapefile and extract coords
    polyData <- rgdal::readOGR(file.path(pathdir, bgShp_name)[i])
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

  if (!rgeos::gIntersects(bgExt, polyData)) {
    logger %>% writeLog(
      type = 'error', hlSpp(spN),
      "Shapefile does not match with background extent. Please specify a new polygon. (**)"
    )
    return()
  }

  smartProgress(logger, message = "Uploading spatial data ...", {
    spatialPoly <- raster::intersect(polyData, bgExt)
  })
  return(spatialPoly)
}
