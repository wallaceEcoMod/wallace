#' @title envs_worldclim
#' @description download worldclim variables. see www.worldclim.com
#'
#' @details
#' See Examples.
#'
#' @param bcRes numeric resolution of the climatic layers
#' @param bcSel list of boolean data. selected variables
#'
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

envs_worldclim <- function(bcRes, bcSel, mapCntr, doBrick, logger = NULL) {
  if(bcRes == '') {
    logger %>% writeLog(type = 'error', 'Select a raster resolution.')
    return()
  }

  smartProgress(logger, message = "Retrieving WorldClim data...", {
    if (bcRes == 0.5) {
      wcbc <- raster::getData(name = "worldclim", var = "bio", res = bcRes,
                              lon = mapCntr[1], lat = mapCntr[2])
    } else {
      wcbc <- raster::getData(name = "worldclim", var = "bio", res = bcRes)
      nam_wcbc <- names(wcbc)
      wcbc <- wcbc[[bcSel]]
      names(wcbc) <- nam_wcbc
    }
  })
  # convert to brick for faster processing
  if(doBrick == TRUE) {
    smartProgress(logger, message = "Converting to RasterBrick for faster processing...", {
      wcbc <- raster::brick(wcbc)
    })
  }

  # change names if bio01 is bio1, and so forth
  if (bcRes == 0.5) {
    names(wcbc) <- gsub("_.*", "", names(wcbc))
  }
  i <- grep('bio[0-9]$', names(wcbc))
  editNames <- paste('bio', sapply(strsplit(names(wcbc)[i], 'bio'), function(x) x[2]), sep = '0')
  names(wcbc)[i] <- editNames
  logger %>% writeLog("WorldClim bioclimatic variables ",
                      paste(names(wcbc), collapse = ", "), " at ",
                      bcRes, " arcmin resolution.")


  return(wcbc)
}
