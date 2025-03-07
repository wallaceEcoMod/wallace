# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# part_partitionOccs.R
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

#' @title part_partitionOccs Partition occurrence data
#' @description This function partitions occurrence data and background points
#'   according to a user-selected method.
#'
#' @details
#' This function is used in the partition occurrence data component.
#' A user-selected method is used to partition occurrence and background points
#'   into different groups for model testing.
#' A list of group assignments for both occurrences and background points is
#'   returned.
#'
#' @param occs data frame of cleaned or processed occurrences obtained from
#'   components occs: Obtain occurrence data or, poccs: Process occurrence data.
#' @param bg coordinates of background points to be used for modeling.
#' @param method character. Partitioning method to be used, one of 5 options: \cr
#'  (1) 'jack' Non-spatial Partition - jackknife  \cr
#'  (2) 'rand' Non-spatial Partition - random k-fold  \cr
#'  (3) 'block' spatial Partition - block  \cr
#'  (4) 'cb1' spatial Partition - checkerboard (k=2)  \cr
#'  (5) 'cb2' spatial Partition - hierarchical checkerboard (k=4)  \cr
#' @param kfolds numeric. Number of partitions to create if selected method is
#'   random k-fold (must be >=2). If other method then keep default of NULL.
#' @param bgMask a RasterStack or a RasterBrick of environmental layers cropped
#'   and masked.
#' @param aggFact numeric. Aggregation factor to be used when using checkerboard
#'   partition (must be >= 1).
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running
#'   in shiny, otherwise leave the default NULL.
#' @param spN data frame of cleaned occurrences obtained from component
#'   occs: Obtain occurrence data. Used to obtain species name for logger
#'   messages.
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
#' partblock <- part_partitionOccs(occs, bg, method = 'rand', kfold = 4)
#' }
#'
#' @return A list of two vectors containing group assignments for occurrences
#'   (occs.grp) and background points (bg.grp).
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gepinillab@@gmail.com>
#' @author Andrea Paz <paz.andreita@@gmail.com>
#' @author Bethany A Johnson <bjohnso005@@citymail.cuny.edu>
# @note
#' @seealso \code{\link[ENMeval]{partitions}}
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be
# linked in the documentation.
#' @importFrom rlang .data
#' @export

part_partitionOccs <- function(occs, bg, method, kfolds = NULL, bgMask = NULL,
                               aggFact = NULL, logger = NULL, spN = NULL) {

  if (method == '') {
    logger %>% writeLog(
      type = 'error',
      "Please select a partitioning option.")
    return()
  }

  occs.xy <- occs %>% dplyr::select("longitude", "latitude")
  bg.xy <- bg %>% dplyr::select("longitude", "latitude")

  if (method == 'jack') {
    group.data <- ENMeval::get.jackknife(occs.xy, bg.xy)
    logger %>% writeLog(
      hlSpp(spN),
      "Occurrences partitioned by jackknife method.")
  }

  if (method == 'rand') {
    if(is.null(kfolds)) {
      logger %>% writeLog(
        type = 'error',
        hlSpp(spN),
        "Please specify a kfold value to use the random partition function.")
      return()
    }
    if (kfolds < 2) {
      logger %>% writeLog(
        type = 'error',
        hlSpp(spN),
        "Please specify a kfold value greater than 1.")
      return()
    }
    group.data <- ENMeval::get.randomkfold(occs.xy, bg.xy, kfolds)
    logger %>% writeLog(
      hlSpp(spN),
      "Occurrences partitioned by random k-fold (k = ", kfolds, ").")
  }

  if (method == 'block') {
    group.data <- ENMeval::get.block(occs.xy, bg.xy)
    logger %>% writeLog(
      hlSpp(spN),
      "Occurrences partitioned by block method.")
  }

  if (method == 'cb1' | method == 'cb2') {
    if(is.null(aggFact)) {
      logger %>% writeLog(
        type = 'error',
        hlSpp(spN),
        paste0("Please specify an aggregation factor to use checkerboard ",
               "partition functions."))
      return()
    }
    if(is.na(aggFact) | aggFact <= 1) {
      logger %>% writeLog(
        type = 'error',
        hlSpp(spN),
        "Please specify a positive aggregation factor greater than 1.")
      return()
    }
    if(is.null(bgMask)) {
      logger %>% writeLog(
        type = 'error',
        hlSpp(spN),
        paste0("Please specify a background mask to use checkerboard ",
               "partition functions."))
      return()
    }

  }

  if(method == 'cb1') {
    smartProgress(logger, message = "Aggregating rasters...", {
      group.data <- ENMeval::get.checkerboard(occs.xy, terra::rast(bgMask), bg.xy, aggFact)
    })

    logger %>% writeLog(hlSpp(spN),
                        "Occurrences partitioned by checkerboard method with ",
                        "aggregation factor of ", aggFact, ".")
  }

  if(method == 'cb2') {
    smartProgress(logger, message = "Aggregating rasters...", {
      group.data <- ENMeval::get.checkerboard(occs.xy, terra::rast(bgMask),
                                               bg.xy, c(aggFact, aggFact))
    })

    logger %>% writeLog(hlSpp(spN),
                        "Occurrences partitioned by hierarchical checkerboard method with ",
                        "aggregation factor of ", aggFact, ".")
  }
  return(group.data)
}
