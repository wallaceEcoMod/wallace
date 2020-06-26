#' @title proj_user Project model to user specified area
#' @description The function projects the model generated in previous components to a user uploaded area
#'
#' @details
#' This functions allows for the projection of the model created in previous components to a new area provided by the user.
#' The projection area/time is user uploaded. The model will be projected to the new area as long as the environmental variables are available for the area.
#' This function returns a list including the cropped environmental variables used for projecting and the projected model.

#' @param evalOut ENMevaluate output from previous module and using any of the available algorithms
#' @param curModel If algorithm is maxent, model selected by user as best or optimal, in terms of feature class and regularization multiplier (e.g 'L_1'). Else must be 1
#' @param envs Environmental layers used for model generation and to be used for projection.
#' @param outputType Output type to be used when algorithm is maxnet or maxent.jar.
#' @param alg Modeling algorithm used in the model component. Can be one of : 'bioclim', 'maxent.jar' or 'maxnet'
#' @param clamp logical, whether projection will be of clamped or unclamped model.
#' @param pjExt Extent of the area to project the model to. This must be provided by the user as a shapefile
#' @param logger logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL

# @keywords
#'
#' @examples
#'out.gbif <- occs_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
#"occs <- as.data.frame(out.gbif[[1]]$cleaned)
#'envs <- envs_worldclim(bcRes = 10, bcSel = list(TRUE,TRUE,TRUE,TRUE,TRUE), doBrick = FALSE)
#"bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=occs)
#'bgMask <- penvs_bgMask(occs, envs, bgExt,spN=occs)
#'bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 10000,spN=occs)
#'partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,aggFact = NULL,spN=occs)
#'Path <- list.files(path='./tests/testthat/shapefile', pattern = "COL_adm0.", full.names = TRUE)
#'userExt<-rgdal::readOGR(Path[2])
#'modAlg <- model_bioclim(occs, partblock$bg.grp,  partblock$occ.grp, bgGrp, bgMask,spN=occs)
#'modProj <- proj_user(evalOut = modAlg, curModel=1, envs=envs,alg='bioclim',clamp=FALSE, pjExt = userExt )
#'
# @return
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Gonzalo E. Pinilla-Buitrago < gpinillabuitrago@@gradcenter.cuny.edu>
# @note
#' @seealso \code{\link[dismo]{predict}}, \code{\link[ENMeval]{maxnet.predictRaster}}, \code{\link{proj_time}} \code{\link{proj_area}}

# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

proj_user <- function(evalOut, curModel, envs, outputType, alg, clamp, pjExt,
                      logger = NULL) {
  newPoly <- pjExt

  if (alg == 'bioclim') {
    logger %>% writeLog('User specified projection for BIOCLIM model.')
  }
  else if (alg == 'maxent.jar'|clamp==TRUE) {
    logger %>% writeLog('User specified projection for clamped model ', curModel, '.')
    }
  else if (clamp == FALSE) {
      logger %>% writeLog('User specified projection for unclamped model', curModel, '.')
    }

  smartProgress(logger,
                message = "Masking environmental grids to projection extent...", {
    projMsk <- raster::crop(envs, newPoly)
  })

  smartProgress(logger,
                message = 'Projecting model to user uploaded area', {
    if (alg == 'bioclim') {
      modProjUser <- dismo::predict(evalOut@models[[curModel]], projMsk)
    } else if (alg == 'maxnet') {
      if (outputType == "raw") {pargs <- "exponential"} else {pargs <- outputType}
      modProjUser <- ENMeval::enm.maxnet@pred(mod = evalOut@models[[curModel]],
                                                   envs = projMsk, doClamp = clamp,
                                              pred.type = pargs)


    } else if (alg == "maxent.jar") {
      pargs <- paste0("outputformat=", outputType)
      modProjUser <- dismo::predict(evalOut@models[[curModel]], projMsk,
                                    args = pargs)
    }
  })

  return(list(projExt = projMsk, projUser = modProjUser))
}
