# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# xfer_userEnvs.R
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
#' @title xfer_userEnvs Transfer model to user specified area and time
#' @description The function transfers the model generated in previous components
#'   to user uploaded environmental variables.
#'
#' @details
#' This functions allows transferring the model created in previous
#'   components to a new time and area provided by the user. The transferring
#'   time and area is user-provided. The model will be transferred to the new
#'   time and area as long as the environmental variables provided are
#'   available for the area and match the variables used for model building.
#'   This function returns a list including the cropped environmental variables
#'   used for transferring and the transferred model.
#' @param evalOut ENMevaluate output from previous module and using any of the
#'   available algorithms.
#' @param curModel if algorithm is maxent, model selected by user as best or
#'   optimal, in terms of feature class and regularization multiplier (e.g
#'   'L_1'). Otherwise it must be 1.
#' @param envs user provided environmental layers (in raster format) to be
#'   used for transferring.
#' @param outputType output type to be used when algorithm is maxnet or
#'   maxent.jar.
#' @param alg modeling algorithm used in the model component. Can be one of:
#'   'BIOCLIM', 'maxent.jar' or 'maxnet'.
#' @param xfExt extent of the area to transfer the model. This must be provided
#'   by the user as a shapefile or as a SpatialPolygons object.
#' @param clamp logical. Whether transfer will be of clamped or unclamped
#'   model.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL.
#' @param spN character. Used to obtain species name for logger messages.
#' @examples
#' \dontrun{
#' ## extent to transfer
#' # set coordinates
#' longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331, -66.47149, -66.71319,
#'                -71.11931)
#' latitude <- c(13.18379, 7.52315, 0.93105, -1.70167, 0.98391, 6.09208, 12.74980)
#' # generate matrix
#' selCoords <- matrix(c(longitude, latitude), byrow = FALSE, ncol = 2)
#' polyExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)),
#'                                                  ID = 1)))
#' # build model
#' occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",package = "wallace"))
#' bg <- read.csv(system.file("extdata/Bassaricyon_alleni_bgPoints.csv",
#' package = "wallace"))
#' envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
#' package = "wallace"), pattern = ".tif$", full.names = TRUE),
#' rasName = list.files(system.file("extdata/wc",package = "wallace"),
#' pattern = ".tif$", full.names = FALSE))
#' partblock <- part_partitionOccs(occs, bg, method = 'block')
#' m <- model_maxent(occs, bg, user.grp = partblock, bgMsk = envs, rms = c(1:2),
#' rmsStep = 1, fcs = c('L', 'LQ'), clampSel = TRUE, algMaxent = "maxnet", parallel = FALSE)
#' envsFut <- list.files(path = system.file('extdata/wc/future',
#'                                          package = "wallace"),
#'                       full.names = TRUE)
#' envsFut <- raster::stack(envsFut)
#' ### run function
#' modXfer <- xfer_userEnvs(evalOut = m, curModel = 1, envs = envsFut,
#'                          outputType = "cloglog", alg = "maxnet",
#'                          clamp = FALSE, xfExt = polyExt)
#' }
#'
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gepinillab@@gmail.com>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
# @note
#' @seealso \code{\link[dismo]{predict}}, \code{\link{xfer_time}}
#'   \code{\link{xfer_userExtent}}
#' @export

xfer_userEnvs <- function(evalOut, curModel, envs, xfExt, alg, outputType = NULL,
                          clamp = NULL, logger = NULL, spN = NULL) {
  newPoly <- xfExt

  if (alg == 'BIOCLIM') {
    logger %>% writeLog(
      hlSpp(spN),
      'User specified transfer for BIOCLIM model.')
  } else if (alg == 'maxent.jar' | clamp == TRUE) {
    logger %>% writeLog(
      hlSpp(spN),
      'User specified transfer for clamped model ', curModel, '.')
  } else if (clamp == FALSE) {
    logger %>% writeLog(
      hlSpp(spN),
      'User specified transfer for unclamped model', curModel, '.')
  }

  smartProgress(
    logger,
    message = "Masking environmental grids to transfer extent...", {
      xferMsk <- raster::crop(envs, newPoly)
      xferMsk <- raster::mask(xferMsk, newPoly)
  })

  smartProgress(
    logger,
    message = 'Transferring model to user uploaded environmental variables & area', {
    if (alg == 'BIOCLIM') {
      modXferUser <- dismo::predict(evalOut@models[[curModel]], terra::rast(xferMsk))
    } else if (alg == 'maxnet') {
      if (outputType == "raw") outputType <- "exponential"
      modXferUser <- predictMaxnet(evalOut@models[[curModel]], xferMsk,
                                          type = outputType, clamp = clamp)
    } else if (alg == 'maxent.jar') {
      modXferUser <- dismo::predict(
        evalOut@models[[curModel]], terra::rast(xferMsk),
        args = c(paste0("outputformat=", outputType),
                 paste0("doclamp=", tolower(as.character(clamp)))))
    }
  })

  return(list(xferExt = xferMsk, xferUser = modXferUser))
}
