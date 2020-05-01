#'
#' @title  envs_worldclim
#' @description Download worldclim variables. see www.worldclim.com
#'
#' @details
#' See Examples.
#'
#' @param rasPath character of directory to rasters
#' @param rasName character vector of raster names
#' @param doBrick Brick option
# @keywords
#'
# @examples
#'
#'
# @return
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

envs_userEnvs <- function(rasPath, rasName, doBrick = FALSE, logger = NULL){

  smartProgress(logger, message = "Reading in rasters...", {
    rasStack <- raster::stack(rasPath)
  })
  # assign names
  names(rasStack) <- tools::file_path_sans_ext(rasName)

  if(is.na(raster::crs(rasStack))) {
    logger %>% writeLog(
      type = "warning",
      paste0('Input rasters have undefined coordinate reference system (CRS). ',
             'Mapping functionality in components Visualize Model Results and ',
             'Project Model will not work. If you wish to map rasters in these ',
             'components, please define their projections and upload again. ',
             'See guidance text in this module for more details.'))
  }

  # convert to brick for faster processing
  if(doBrick == TRUE) {
    smartProgress(logger, message = "Converting to RasterBrick for faster processing...", {
      rasStack  <- raster::brick(rasStack)
    })
  }

  return(rasStack)
}
