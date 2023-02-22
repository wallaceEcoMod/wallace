#'
#' @title  envs_userEnvs
#' @description Load user provided rasters
#'
#' @details This function is called by the module envs to load user provided raster
#'   variables for use in further analyses. It returns either a rasterStack or
#'   rasterBrick of loaded variables with appropriate names for further analyses.
#'
#' @param rasPath character. Path to rasters, must be the full path including
#'   file name and extension
#' @param rasName character. Vector of raster names to be assigned to
#'   loaded rasters
#' @param doBrick logical. Converts downloaded rasters to brick for faster
#'   processing
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
# @keywords
#'
#' @examples
#' \dontrun{
#' pathRast <- list.files(system.file("extdata/wc", package = "wallace"),
#'                        pattern = ".tif$", full.names = TRUE)
#' nameRast <- list.files(system.file("extdata/wc", package = "wallace"),
#'                        pattern = ".tif$", full.names = FALSE)
#' userEnvs <- envs_userEnvs(rasPath = pathRast, rasName = nameRast)
#' }
#'
#' @return A rasterStack or a rasterBrick (if doBrick = TRUE) of user
#'   provided rasters
#'
#' @author Jamie Kass <jamie.m.kass@@gmail.com >
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @export
#'
#'

envs_userEnvs <- function(rasPath, rasName, doBrick = FALSE, logger = NULL){

  smartProgress(logger, message = "Reading in rasters...", {
    rasStack <- raster::stack(rasPath)
  })
  # assign names
  names(rasStack) <- tools::file_path_sans_ext(rasName)

  if (is.na(raster::crs(rasStack))) {
    logger %>% writeLog(
      type = "warning",
      paste0('Input rasters have undefined coordinate reference system (CRS). ',
             'Mapping functionality in components Visualize Model Results and ',
             'Transfer Model will not work. If you wish to map rasters in these ',
             'components, please define their projections and upload again. ',
             'See guidance text in this module for more details.'))
  }

  # convert to brick for faster processing
  if (doBrick == TRUE) {
    smartProgress(logger,
                  message = "Converting to RasterBrick for faster processing...", {
      rasStack  <- raster::brick(rasStack)
    })
  }

  return(rasStack)
}
