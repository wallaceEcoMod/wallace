#' @title proj_userEnvs Project model to user specified area and time
#' @description The function projects the model generated in previous components to user uploaded environmental variables.
#'
#' @details
#' This functions allows for the projection of the model created in previous components to a new time and area provided by the user.
#' The projection time and area is user provided. The model will be projected to the new time and area as long as the environmental variables provided are available for the area and match the variables used for model building.
#' This function returns a list including the cropped environmental variables used for projecting and the projected model.

#' @param evalOut ENMevaluate output from previous module and using any of the available algorithms
#' @param curModel if algorithm is maxent, model selected by user as best or optimal, in terms of feature class and regularization multiplier (e.g 'L_1'). Otherwise it must be 1
#' @param envs user provided environmental layers (in raster format) to be used for projection
#' @param outputType output type to be used when algorithm is maxnet or maxent.jar
#' @param alg modeling algorithm used in the model component. Can be one of : 'BIOCLIM', 'maxent.jar' or 'maxnet'
#' @param pjExt extent of the area to project the model. This must be provided by the user as a shapefile or as a SpatialPolygons object
#' @param clamp logical. Whether projection will be of clamped or unclamped model
#' @param logger Stores all notification messages to be displayed in the Log Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @param spN character. Used to obtain species name for logger messages

# @keywords
#'
#' @examples
#' spN<-"Panthera onca"
#'out.gbif <- occs_queryDb(spName = spN, occDb = "gbif", occNum = 100)
#"occs <- as.data.frame(out.gbif[[1]]$cleaned)
#'envs <- envs_worldclim(bcRes = 10, bcSel = c('bio01','bio19'), doBrick = FALSE)
#"bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=spN)
#'bgMask <- penvs_bgMask(occs, envs, bgExt,spN=spN)
#'bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 10000,spN=spN)
#'partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,aggFact = NULL,spN=spN)
#'Path <- list.files(path='./tests/testthat/shapefile', pattern = "COL_adm0.", full.names = TRUE)
#'userExt<-rgdal::readOGR(Path[2])
#'modAlg <- model_bioclim(occs, partblock$bg.grp,  partblock$occ.grp, bgGrp, bgMask,spN=spN)
#'envsFut<-list.files(path='./wc10/Future', pattern = ".tif$", full.names = TRUE)
#'envsFut<-raster::stack(envsFut)
#'modProj <- proj_userEnvs(evalOut = modAlg, curModel=1, envs=envsFut,alg='BIOCLIM', pjExt = userExt)
#'
# @return
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
# @note
#' @seealso \code{\link[dismo]{predict}}, \code{\link{proj_time}} \code{\link{proj_userExtent}}
#'
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

proj_userEnvs <- function(evalOut, curModel, envs, pjExt, alg, outputType = NULL,
                          clamp = NULL, logger = NULL, spN = NULL) {
  newPoly <- pjExt

  if (alg == 'BIOCLIM') {
    logger %>% writeLog(hlSpp(spN), 'User specified projection for BIOCLIM model.')
  } else if (alg == 'maxent.jar' | clamp == TRUE) {
    logger %>% writeLog(hlSpp(spN), 'User specified projection for clamped model ', curModel, '.')
  } else if (clamp == FALSE) {
    logger %>% writeLog(hlSpp(spN), 'User specified projection for unclamped model', curModel, '.')
  }

  smartProgress(logger,
                message = "Masking environmental grids to projection extent...", {
    projMsk <- raster::crop(envs, newPoly)
    projMsk <- raster::mask(projMsk, newPoly)
  })

  smartProgress(logger,
                message = 'Projecting model to user uploaded environmental variables & area', {
    if (alg == 'BIOCLIM') {
      modProjUser <- dismo::predict(evalOut@models[[curModel]], projMsk)
    } else if (alg == 'maxnet') {
      if (outputType == "raw") outputType <- "exponential"
      modProjUser <- predictMaxnet(evalOut@models[[curModel]], projMsk,
                                   type = outputType, clamp = clamp)
    } else if (alg == 'maxent.jar') {
      modProjUser <- dismo::predict(evalOut@models[[curModel]], projMsk,
                                    args = c(paste0("outputformat=", outputType),
                                             paste0("doclamp=", tolower(as.character(clamp)))),
                                    na.rm = TRUE)
    }
  })

  return(list(projExt = projMsk, projUser = modProjUser))
}
