# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# model_bioclim.R
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

#' @author Jamie M. Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gepinillab@@gmail.com>
#' @author Bethany A. Johnson <bjohnso005@@citymail.cuny.edu>
# @note

#' @seealso \code{\link[ENMeval]{ENMevaluate}}
#' @importFrom rlang .data
#' @export

model_bioclim <- function(occs, bg, user.grp, bgMsk, logger = NULL,
                          spN = NULL) {

  # get just coordinates
  occs.xy <- occs %>% dplyr::select("longitude", "latitude")
  bg.xy <- bg %>% dplyr::select("longitude", "latitude")

  smartProgress(logger,
                       message = paste0("Building/Evaluating BIOCLIM model for ",
                                 spName(spN), "..."), {
     e <- ENMeval::ENMevaluate(occs = occs.xy, envs = terra::rast(bgMsk), bg = bg.xy,
                               algorithm = "bioclim", partitions = "user",
                               user.grp = user.grp)
  })

  logger %>% writeLog(hlSpp(spN),
                      "BIOCLIM ran successfully and output evaluation results.")

  return(e)
}
