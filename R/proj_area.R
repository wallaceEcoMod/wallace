
#' @title proj_area Project model to a new area
#' @description Function projects the model generated in previous components to a new user drawn area.
#'
#' @details
#' This functions allows for the projection of the model created in previous components to a new area.
#' The projection area is user provided in the map of the GUI. The model will be projected to the new area as long as the environmental variables are available for the area.
#' This function returns a list including the cropped environmental variables used for projecting and the projected model.
#' @param evalOut ENMevaluate output from previous module and using any of the available algorithms
#' @param curModel If algorithm is maxent, model selected by user as best or optimal, in terms of feature class and regularization multiplier (e.g 'L_1'). Else must be 1
#' @param envs environmental layers to be used for projecting the model. They must match the layers used for generating the model in the model component
#' @param outputType output type to be used when algorithm is maxnet or maxent.jar
#' @param alg character. modeling algorithm used in the model component. Can be one of : 'BIOCLIM', 'maxent.jar' or 'maxnet'
#' @param pjExt extent of the area to project the model. This is defined by the user in the map of the GUI and is provided as a SpatialPolygons object
#' @param clamp logical. Whether projection will be of clamped or unclamped model
#' @param logger Stores all notification messages to be displayed in the Log Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @param spN Character used to obtain species name for logger messages
# @keywords
#'
#' @examples
#' out.gbif <- occs_queryDb(spName = "Panthera onca", occDb = "gbif",
#'                          occNum = 100)
#' occs <- as.data.frame(out.gbif[[1]]$cleaned)
#' envs <- envs_worldclim(bcRes = 10,
#'                        bcSel = c("bio01","bio02","bio13","bio14"),
#'                        doBrick = FALSE)
#' bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)
#' bgMask <- penvs_bgMask(occs, envs, bgExt)
#' bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 10000)
#' partblock <- part_partitionOccs(occs, bg, method = 'block',
#'                                 kfolds = NULL, bgMask = NULL,
#'                                 aggFact = NULL)
#' # extent to project
#' longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331,
#'                -66.47149, -66.71319, -71.11931)
#' latitude <- c(13.18379, 7.52315, 0.93105,
#'               -1.70167, 0.98391, 6.09208, 12.74980)
#' selCoords <- matrix(c(longitude, latitude), byrow = FALSE, ncol = 2)
#' expertAddedPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)), ID=1)))
#' modAlg <- model_bioclim(occs, bg, partblock$occ.grp,
#'                         partblock$bg.grp, bgMask,spN=occs)
#' modProj <- proj_area(evalOut = modAlg, curModel = 1, envs,
#'                      outputType = 'raw', alg = 'BIOCLIM',
#'                      pjExt = expertAddedPoly)
#'
#' @return A list of two elements: projExt and projArea.
#' The first is a RasterBrick or a RasterStack of the environmental variables cropped to the projection area.
#' The second element is a raster of the projected model with the specified output type.
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
# @note
#' @seealso \code{\link[dismo]{predict}}, \code{\link{proj_time}} \code{\link{proj_userEnvs}}
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

proj_area <- function(evalOut, curModel, envs, pjExt, alg, outputType = NULL,
                      clamp = NULL, logger = NULL, spN = NULL) {
  newPoly <- pjExt

  if (alg == 'BIOCLIM') {
    logger %>% writeLog(alfred.hlSpp(spN), 'New area projection for BIOCLIM model.')
  } else if (alg == 'maxent.jar'| clamp == TRUE) {
    logger %>% writeLog(alfred.hlSpp(spN), 'New area projection for clamped model ', curModel, '.')
  } else if (clamp == FALSE) {
    logger %>% writeLog(alfred.hlSpp(spN), 'New area projection for unclamped model ', curModel, '.')
  }


  smartProgress(logger,
                message = "Masking environmental grids to projection extent...", {
    projMsk <- raster::crop(envs, newPoly)
    projMsk <- raster::mask(projMsk, newPoly)
  })

  smartProgress(logger, message = 'Projecting model to new area...', {
    if (alg == 'BIOCLIM') {
      modProjArea <- dismo::predict(evalOut@models[[curModel]], projMsk,
                                    useC = FALSE)
    } else if (alg == 'maxnet') {
      if (outputType == "raw") outputType <- "exponential"
      modProjArea <- predictMaxnet(evalOut@models[[curModel]], projMsk,
                                   type = outputType, clamp = clamp)
    } else if (alg == 'maxent.jar') {
      modProjArea <- dismo::predict(evalOut@models[[curModel]], projMsk,
                                    args = c(paste0("outputformat=", outputType),
                                             paste0("doclamp=", tolower(as.character(clamp)))),
                                    na.rm = TRUE)
    }
  })

  return(list(projExt = projMsk, projArea = modProjArea))
}

