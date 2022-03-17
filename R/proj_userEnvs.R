#' @title proj_userEnvs Project model to user specified area and time
#' @description The function projects the model generated in previous components
#'   to user uploaded environmental variables.
#'
#' @details
#' This functions allows for the projection of the model created in previous
#'   components to a new time and area provided by the user. The projection
#'   time and area is user provided. The model will be projected to the new
#'   time and area as long as the environmental variables provided are
#'   available for the area and match the variables used for model building.
#'   This function returns a list including the cropped environmental variables
#'   used for projecting and the projected model.
#' @param evalOut ENMevaluate output from previous module and using any of the
#'   available algorithms.
#' @param curModel if algorithm is maxent, model selected by user as best or
#'   optimal, in terms of feature class and regularization multiplier (e.g
#'   'L_1'). Otherwise it must be 1.
#' @param envs user provided environmental layers (in raster format) to be
#'   used for projection.
#' @param outputType output type to be used when algorithm is maxnet or
#'   maxent.jar.
#' @param alg modeling algorithm used in the model component. Can be one of:
#'   'BIOCLIM', 'maxent.jar' or 'maxnet'.
#' @param pjExt extent of the area to project the model. This must be provided
#'   by the user as a shapefile or as a SpatialPolygons object.
#' @param clamp logical. Whether projection will be of clamped or unclamped
#'   model.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL.
#' @param spN character. Used to obtain species name for logger messages.
#' @examples
#' ## extent to project
#' # set coordinates
#' longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331, -66.47149, -66.71319,
#'                -71.11931)
#' latitude <- c(13.18379, 7.52315, 0.93105, -1.70167, 0.98391, 6.09208, 12.74980)
#' # generate matrix
#' selCoords <- matrix(c(longitude, latitude), byrow = F, ncol = 2)
#' polyExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)),
#'                                                  ID = 1)))
#' # load model
#' m <- readRDS(system.file("extdata/model.RDS",
#'                          package = "wallace"))
#' envsFut <- list.files(path = system.file('extdata/wc/future',
#'                                          package = "wallace"),
#'                       full.names = TRUE)
#' envsFut <- raster::stack(envsFut)
#' ### run function
#' modProj <- proj_userEnvs(evalOut = m, curModel = 1, envs = envsFut,
#'                          outputType = "cloglog", alg = "maxent.jar",
#'                          clamp = FALSE, pjExt = polyExt)
#'
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
# @note
#' @seealso \code{\link[dismo]{predict}}, \code{\link{proj_time}}
#'   \code{\link{proj_userExtent}}
#' @export

proj_userEnvs <- function(evalOut, curModel, envs, pjExt, alg, outputType = NULL,
                          clamp = NULL, logger = NULL, spN = NULL) {
  newPoly <- pjExt

  if (alg == 'BIOCLIM') {
    logger %>% alfred.writeLog(
      alfred.hlSpp(spN),
      'User specified projection for BIOCLIM model.')
  } else if (alg == 'maxent.jar' | clamp == TRUE) {
    logger %>% alfred.writeLog(
      alfred.hlSpp(spN),
      'User specified projection for clamped model ', curModel, '.')
  } else if (clamp == FALSE) {
    logger %>% alfred.writeLog(
      alfred.hlSpp(spN),
      'User specified projection for unclamped model', curModel, '.')
  }

  alfred.smartProgress(
    logger,
    message = "Masking environmental grids to projection extent...", {
      projMsk <- raster::crop(envs, newPoly)
      projMsk <- raster::mask(projMsk, newPoly)
  })

  alfred.smartProgress(
    logger,
    message = 'Projecting model to user uploaded environmental variables & area', {
    if (alg == 'BIOCLIM') {
      modProjUser <- dismo::predict(evalOut@models[[curModel]], projMsk,
                                    useC = FALSE)
    } else if (alg == 'maxnet') {
      if (outputType == "raw") outputType <- "exponential"
      modProjUser <- alfred.predictMaxnet(evalOut@models[[curModel]], projMsk,
                                          type = outputType, clamp = clamp)
    } else if (alg == 'maxent.jar') {
      modProjUser <- dismo::predict(
        evalOut@models[[curModel]], projMsk,
        args = c(paste0("outputformat=", outputType),
                 paste0("doclamp=", tolower(as.character(clamp)))),
        na.rm = TRUE)
    }
  })

  return(list(projExt = projMsk, projUser = modProjUser))
}
