# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# xfer_area.R
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

#' @title xfer_area Transfer model to a new area
#' @description Function transfers the model generated in previous components to
#'   a new user drawn area.
#'
#' @details
#' This functions transfers the model created in previous
#'   components to a new area. The area of transfer is user provided in the map
#'   of the GUI. The model will be transferred to the new area as long as the
#'   environmental variables are available for the area. This function returns
#'   a list including the cropped environmental variables used for transferring
#'   and the transferred model.
#' @param evalOut ENMevaluate output from previous module and using any of the
#'   available algorithms.
#' @param curModel If algorithm is maxent, model selected by user as best or
#'   optimal, in terms of feature class and regularization multiplier
#'   (e.g 'L_1'). Else must be 1.
#' @param envs environmental layers to be used for transferring the model. They
#'   must match the layers used for generating the model in the model component.
#' @param outputType output type to be used when algorithm is maxnet
#'   or maxent.jar.
#' @param alg character. modeling algorithm used in the model component. Can
#'   be one of : 'BIOCLIM', 'maxent.jar' or 'maxnet'.
#' @param xfExt extent of the area to transfer the model. This is defined by the
#'   user in the map of the GUI and is provided as a SpatialPolygons object.
#' @param clamp logical. Whether transfer will be of clamped or unclamped
#'   model.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running
#'   in shiny, otherwise leave the default NULL.
#' @param spN Character used to obtain species name for logger messages
#' @examples
#' \dontrun{
#' envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
#'                                            package = "wallace"),
#'                       pattern = ".tif$", full.names = TRUE),
#'                       rasName = list.files(system.file("extdata/wc",
#'                                            package = "wallace"),
#'                       pattern = ".tif$", full.names = FALSE))
#' # extent of transfer
#' longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331,
#'                -66.47149, -66.71319, -71.11931)
#' latitude <- c(13.18379, 7.52315, 0.93105,
#'               -1.70167, 0.98391, 6.09208, 12.74980)
#' selCoords <- matrix(c(longitude, latitude), byrow = FALSE, ncol = 2)
#' polyExt <-
#'   sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)),
#'                       ID = 1)))
#' # load model
#' m <- readRDS(system.file("extdata/model.RDS",
#'                          package = "wallace"))
#' modXfer <- xfer_area(evalOut = m, curModel = 1, envs,
#'                      outputType = 'cloglog', alg = 'maxent.jar',
#'                      clamp = TRUE, xfExt = polyExt)
#' }
#'
#' @return A list of two elements: xferExt and xferArea. The first is a
#'   RasterBrick or a RasterStack of the environmental variables cropped to the
#'   area of transfer. The second element is a raster of the transferred model with
#'   the specified output type.
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gepinillab@@gmail.com>
# @note
#' @seealso \code{\link[dismo]{predict}}, \code{\link{xfer_time}}
#' \code{\link{xfer_userEnvs}}
#' @export

xfer_area <- function(evalOut, curModel, envs, xfExt, alg, outputType = NULL,
                      clamp = NULL, logger = NULL, spN = NULL) {
  newPoly <- xfExt

  if (alg == 'BIOCLIM') {
    logger %>% writeLog(hlSpp(spN),
                               'New area of transfer for BIOCLIM model.')
  } else if (alg == 'maxent.jar'| clamp == TRUE) {
    logger %>% writeLog(hlSpp(spN),
                               'New area of transfer for clamped model ',
                               curModel, '.')
  } else if (clamp == FALSE) {
    logger %>% writeLog(hlSpp(spN),
                               'New area of transfer for unclamped model ',
                               curModel, '.')
  }


  smartProgress(
    logger,
    message = "Masking environmental grids to extent of transfer...", {
    xferMsk <- raster::crop(envs, newPoly)
    xferMsk <- raster::mask(xferMsk, newPoly)
  })

  smartProgress(logger, message = 'Transferring model to new area...', {
    if (alg == 'BIOCLIM') {
      modXferArea <- dismo::predict(evalOut@models[[curModel]], xferMsk,
                                    useC = FALSE)
    } else if (alg == 'maxnet') {
      if (outputType == "raw") outputType <- "exponential"
      modXferArea <- predictMaxnet(evalOut@models[[curModel]], xferMsk,
                                          type = outputType, clamp = clamp)
    } else if (alg == 'maxent.jar') {
      modXferArea <- dismo::predict(
        evalOut@models[[curModel]], xferMsk,
        args = c(paste0("outputformat=", outputType),
                 paste0("doclamp=", tolower(as.character(clamp)))),
        na.rm = TRUE)
    }
  })

  return(list(xferExt = xferMsk, xferArea = modXferArea))
}

