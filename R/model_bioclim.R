
#' @title model_bioclim Generate Biolcim model
#' @description This functions generates maxent or maxnet models using ENMeval 2.0
#'
#' @details
#' The function generates model in ENMeval using a user provided partition of occurrences from previous components in the GUI.
#'
#' @param occs data frame of cleaned or processed occurrences obtained from components occs: Obtain occurrence data or, poccs: Process occurrence data.
#' @param bg  Coordinates of background points to be used for modelling.
#' @param user.grp  A list of two vectors containing group assignments for occurrences (occs.grp) and background points (bg.grp).
#' @param bgMsk A RasterStack or a RasterBrick of environmental layers cropped and masked to match the provided background extent.
#' @param logger logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @param spN Species name to be used for all logger messages

# @keywords
#'
#' @examples
#'spN="Panthera onca"
#'out.gbif <- occs_queryDb(spName = spN, occDb = "gbif", occNum = 100)
#'occs <- as.data.frame(out.gbif[[1]]$cleaned)
#'occs <- poccs_thinOccs(occs = occs, thinDist = 10,spN=spN)
#'envs <- envs_worldclim(bcRes = 10, bcSel = list(TRUE,TRUE,TRUE,TRUE,TRUE), doBrick = FALSE)
#'bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=spN)
#'bgMask <- penvs_bgMask(occs, envs, bgExt,spN=spN)
#'bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 10000,spN=spN)
#'partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL, aggFact = NULL,spN=spN)
#'bioclimAlg <- model_bioclim(occs, bg, user.grp=partblock, bgMask,spN=spN)
#'
#' @return Function returns an ENMevaluate object with all the evaluated models and a selection of appropriate fields.

#' @author Jamie M. Kass <jkass@@gradcenter.cuny.edu>
#' @author Gonzalo E. Pinilla-Buitrago < gpinillabuitrago@@gradcenter.cuny.edu>
# @note

#' @seealso \code{\link[ENMeval]{ENMevaluate}}
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.

#' @export

model_bioclim <- function(occs, bg, user.grp, bgMsk, logger = NULL,
                          spN = NULL) {

  # get just coordinates
  occs.xy <- occs %>% dplyr::select(longitude, latitude)
  bg.xy <- bg %>% dplyr::select(longitude, latitude)

  smartProgress(logger,
                message = paste0("Building/Evaluating BIOCLIM model for ",
                                 spName(spN), "..."), {
     e <- ENMeval::ENMevaluate(occs = occs.xy, envs = bgMsk, bg = bg.xy,
                               algorithm = "bioclim", partitions = "user",
                               user.grp= user.grp)
  })

  logger %>% writeLog(hlSpp(spN),
                      "BIOCLIM ran successfully and output evaluation results.")

  return(e)
}
