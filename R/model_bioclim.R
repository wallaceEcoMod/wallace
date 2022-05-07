
#' @title model_bioclim Generate Bioclim model
#' @description The function generates a BIOCLIM model using
#'   ENMeval 2.0
#'
#' @details
#' The function generates a model in ENMeval using a user provided partition of
#'   occurrences from previous components in the GUI.
#'
#' @param occs data frame of cleaned or processed occurrences obtained from
#'   components occs: Obtain occurrence data or, poccs: Process occurrence data.
#' @param bg  coordinates of background points to be used for modeling.
#' @param user.grp  a list of two vectors containing group assignments for
#'   occurrences (occs.grp) and background points (bg.grp).
#' @param bgMsk a RasterStack or a RasterBrick of environmental layers cropped
#'   and masked to match the provided background extent.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running
#'   in shiny, otherwise leave the default NULL.
#' @param spN character. Species name to be used for all logger messages.
#'
#' @examples
#' \dontrun{
#' envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
#'                                            package = "wallace"),
#'                       pattern = ".tif$", full.names = TRUE),
#'                       rasName = list.files(system.file("extdata/wc",
#'                                            package = "wallace"),
#'                       pattern = ".tif$", full.names = FALSE))
#' occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
#'                  package = "wallace"))
#' bg <- read.csv(system.file("extdata/Bassaricyon_alleni_bgPoints.csv",
#'                package = "wallace"))
#' partblock <- part_partitionOccs(occs, bg, method = 'block')
#' m <- model_bioclim(occs, bg, partblock, envs)
#' }
#'
#' @return Function returns an ENMevaluate object with all the evaluated models
#'   and a selection of appropriate fields.

#' @author Jamie M. Kass <jkass@@gradcenter.cuny.edu>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
# @note

#' @seealso \code{\link[ENMeval]{ENMevaluate}}
#' @importFrom rlang .data
#' @export

model_bioclim <- function(occs, bg, user.grp, bgMsk, logger = NULL,
                          spN = NULL) {

  # get just coordinates
  occs.xy <- occs %>% dplyr::select(.data$longitude, .data$latitude)
  bg.xy <- bg %>% dplyr::select(.data$longitude, .data$latitude)

  smartProgress(logger,
                       message = paste0("Building/Evaluating BIOCLIM model for ",
                                 spName(spN), "..."), {
     e <- ENMeval::ENMevaluate(occs = occs.xy, envs = bgMsk, bg = bg.xy,
                               algorithm = "bioclim", partitions = "user",
                               user.grp = user.grp)
  })

  logger %>% writeLog(hlSpp(spN),
                      "BIOCLIM ran successfully and output evaluation results.")

  return(e)
}
