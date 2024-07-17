# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# envs_worldclim.R
# File author: Wallace EcoMod Dev Team. 2023.
# --------------------------------------------------------------------------
# This file is part of the Wallace EcoMod application
# (hereafter “Wallace”).
#
# Wallace is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# Wallace is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Wallace. If not, see <http://www.gnu.org/licenses/>.
# --------------------------------------------------------------------------
#
#' @title envs_worldclim Obtain WorldClim variables
#' @description download WorldClim variables. See www.worldclim.com.
#'
#' @details This function is called by the module envs to download
#'   WorldClim variables from www.worldclim.com. The variables to be downloaded
#'   are selected by the user with bcSel and the resolution with bcRes. It
#'   returns either a rasterStack or rasterBrick of selected variables with
#'   appropriate names for further analyses.
#'
#' @param bcRes numeric. Resolution of the climatic layers. Currently
#'   available resolutions are 0.5, 2.5 and 10.
#' @param bcSel character. Vector with bionames to be selected.
#' @param mapCntr numeric. Vector with longitude and latitude for a tile.
#'   Required for bcRes 0.5, for other resolutions world data will be downloaded.
#' @param doBrick logical. Converts downloaded rasters to brick for faster
#'   processing.
#' @param logger Stores all notification messages to be displayed in the
#'   Log Window of Wallace GUI. Insert the logger reactive list here for
#'   running in shiny, otherwise leave the default NULL.
#'
#' @examples
#' \dontrun{
#' bcRes <- 10 # (10 arcmin)
#' envar <- c('bio05', 'bio06', 'bio13', 'bio14')
#' arcmin10 <- envs_worldclim(bcRes, bcSel = envar)
#' }
#'
#' @return A rasterStack or a rasterBrick (if doBrick=TRUE) of downloaded
#'  worldclim rasters at the requested resolution.
#'
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gepinillab@@gmail.com>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
#'
#' @seealso \code{\link[geodata]{worldclim_global}}, \code{\link[geodata]{worldclim_tile}}
#'
#' @export

envs_worldclim <- function(bcRes, bcSel, mapCntr, doBrick = FALSE,
                           logger = NULL) {
  if(bcRes == '') {
    logger %>% writeLog(type = 'error', 'Select a raster resolution.')
    return()
  }

  smartProgress(logger, message = "Retrieving WorldClim data...", {
    if (bcRes == 0.5) {
      wcbc <- tryCatch(expr = geodata::worldclim_tile(var = "bio",
                                                      lon = mapCntr[1], lat = mapCntr[2],
                                                      path = tempdir(),
                                                      version="2.1"),
                       error = function(e) NULL)
    } else {
      wcbc <- tryCatch(expr = geodata::worldclim_global(var = "bio",
                                                        res = bcRes,
                                                        path = tempdir(),
                                                        version = "2.1"),
                       error= function(e) NULL)
    }
    #trycatch error
    if (is.null(wcbc)) {
      logger %>% writeLog(
        type = "error",
        paste0("Unable to retrieve data from WorldClim.
               Server may be down.
               Please use User-Specified module instead."))
      return()
    } else {
    # change names to bioXX
    names(wcbc) <- gsub(".*_", "bio", names(wcbc))
    # change names if bio01 is bio1, and so forth
      i <- grep('bio[0-9]$', names(wcbc))
      editNames <- paste('bio', sapply(strsplit(names(wcbc)[i], 'bio'),
                                       function(x) x[2]), sep = '0')
      names(wcbc)[i] <- editNames
      wcbc <- wcbc[[bcSel]]
    }
  })

  # convert from spatraster to raster
  wcbc <- raster::stack(wcbc)

  # convert to brick for faster processing
  if(doBrick == TRUE) {
    smartProgress(logger,
                         message = "Converting to RasterBrick for faster processing...", {
      wcbc <- raster::brick(wcbc)
    })
  }

  logger %>% writeLog("WorldClim bioclimatic variables ",
                      paste(names(wcbc), collapse = ", "), " at ",
                      bcRes, " arcmin resolution.")
  return(wcbc)
}
