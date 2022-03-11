
#' @title model_bioclim Generate Biolcim model
#' @description This functions generates maxent or maxnet models using ENMeval 2.0
#'
#' @details
#' The function generates model in ENMeval using a user provided partition of occurrences from previous components in the GUI.
#'
#' @param occs data frame of cleaned or processed occurrences obtained from components occs: Obtain occurrence data or, poccs: Process occurrence data.
#' @param bg  coordinates of background points to be used for modeling
#' @param user.grp  a list of two vectors containing group assignments for occurrences (occs.grp) and background points (bg.grp)
#' @param bgMsk a RasterStack or a RasterBrick of environmental layers cropped and masked to match the provided background extent
#' @param logger Stores all notification messages to be displayed in the Log Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @param spN character. Species name to be used for all logger messages

# @keywords
#'
#' @examples
#' out.gbif <- occs_queryDb(spName = "Panthera onca", occDb = "gbif",
#'                          occNum = 100)
#' occs <- as.data.frame(out.gbif[[1]]$cleaned)
#' occs <- poccs_thinOccs(occs = occs, thinDist = 10)
#' envs <- envs_worldclim(bcRes = 10,
#'                        bcSel = c("bio03", "bio04", "bio13", "bio14"),
#'                        doBrick = FALSE)
#' bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)
#' bgMask <- penvs_bgMask(occs, envs, bgExt)
#' bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 10000)
#' partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL,
#'                                 bgMask = NULL, aggFact = NULL)
#' bioclimAlg <- model_bioclim(occs, bg, user.grp = partblock, bgMask)
#'
#' @return Function returns an ENMevaluate object with all the evaluated models and a selection of appropriate fields.

#' @author Jamie M. Kass <jkass@@gradcenter.cuny.edu>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
# @note

#' @seealso \code{\link[ENMeval]{ENMevaluate}}
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @importFrom rlang .data
#' @export

model_bioclim <- function(occs, bg, user.grp, bgMsk, logger = NULL,
                          spN = NULL) {

  # get just coordinates
  occs.xy <- occs %>% dplyr::select(.data$longitude, .data$latitude)
  bg.xy <- bg %>% dplyr::select(.data$longitude, .data$latitude)

  alfred.smartProgress(logger,
                message = paste0("Building/Evaluating BIOCLIM model for ",
                                 alfred.spName(spN), "..."), {
     e <- ENMeval::ENMevaluate(occs = occs.xy, envs = bgMsk, bg = bg.xy,
                               algorithm = "bioclim", partitions = "user",
                               user.grp= user.grp)
  })

  logger %>% writeLog(alfred.hlSpp(spN),
                      "BIOCLIM ran successfully and output evaluation results.")

  return(e)
}
