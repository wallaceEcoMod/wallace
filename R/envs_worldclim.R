#' @title envs_worldclim Obtain worldclim variables
#' @description download worldclim variables. see www.worldclim.com
#'
#' @details
#' This function is called by the module envs to download worldclim variables from www.worldclim.com.
#' The variables to be dowloaded are selected by the user with bcSel and the resolution with bcRes
#' It returns either a rasterStack or rasterBrick of selected variables with appropriate names for further analyses.
#'
#' @param bcRes numeric resolution of the climatic layers. Currently avaialable resolutions are 0.5, 2.5 and 10
#' @param bcSel character vector with bionames to be selected
#' @param mapCntr numeric vector with longitude and latitude for a tile. Required for bcRes 0.5, for other resolutions world data will be downloaded
#' @param doBrick logical converts downloaded rasters to brick for faster processing
#' @param logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#'
# @keywords
#'
#' @examples
#' bcRes <- 10 # (10 arcmin)
#' envar <- c('bio01', 'bio02', 'bio03', 'bio10', 'bio11')
#' arcmin10 <- envs_worldclim(bcRes, bcSel= envar,doBrick=T)
#'
#' @return A rasterStack or a rasterBrick (if doBrick=TRUE) of downloaded worldclim rasters at the requested resolution
#'
#' @author Jamie Kass < jamie.m.kass@@gmail.com >
#' @author Gonzalo E. Pinilla-Buitrago < gpinillabuitrago@@gradcenter.cuny.edu>
# @note
#' @seealso \code{\link[raster]{getData}}
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
      wcbc <- raster::getData(name = "worldclim", var = "bio", res = bcRes,
                              lon = mapCntr[1], lat = mapCntr[2])
      # change names if bio01 is bio1, and so forth
      if (bcRes == 0.5) {
        names(wcbc) <- gsub("_.*", "", names(wcbc))
      }
      i <- grep('bio[0-9]$', names(wcbc))
      editNames <- paste('bio', sapply(strsplit(names(wcbc)[i], 'bio'), function(x) x[2]), sep = '0')
      names(wcbc)[i] <- editNames

      wcbc <- wcbc[[bcSel]]
  })

  # convert to brick for faster processing
  if(doBrick == TRUE) {
    smartProgress(logger, message = "Converting to RasterBrick for faster processing...", {
      wcbc <- raster::brick(wcbc)
    })
  }

  logger %>% writeLog("WorldClim bioclimatic variables ",
                      paste(names(wcbc), collapse = ", "), " at ",
                      bcRes, " arcmin resolution.")
  return(wcbc)
}
