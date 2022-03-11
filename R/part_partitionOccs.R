
#' @title part_partitionOccs Partition occurrence data
#' @description This function partitions occurrence data and background points according to a user selected method.
#'
#' @details
#' This function is used in the partition occurrence data component.
#' A user selected method is used to partition occurrence and background points into different groups for model testing.
#' A list of group assignments for both occurrences and background points is returned.
#'
#' @param occs data frame of cleaned or processed occurrences obtained from components occs: Obtain occurrence data or, poccs: Process occurrence data.
#' @param bg coordinates of background points to be used for modeling
#' @param method character. Partitioning method to be used, one of 5 options: \cr
#'  (1) 'jack' Non-spatial Partition - jackknife  \cr
#'  (2) 'rand' Non-spatial Partition - random k-fold  \cr
#'  (3) 'block' spatial Partition - block  \cr
#'  (4) 'cb1' spatial Partition - checkerboard 1 (K=2)  \cr
#'  (5) 'cb2' spatial Partition - checkerboard 2 (K=4)  \cr
#' @param kfolds numeric. Number of partitions to create if selected method is random k-fold (must be >=2). If other method then keep default of NULL
#' @param bgMask a RasterStack or a RasterBrick of environmental layers cropped and masked
#' @param aggFact numeric. Aggregation factor to be used when using checkerboard partition (must be >=1)
#' @param logger Stores all notification messages to be displayed in the Log Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#' otherwise leave the default NULL
#' @param spN data frame of cleaned occurrences obtained from component occs: Obtain occurrence data. Used to obtain species name for logger messages
# @keywords
#'
#' @examples
#' occs <- occs_queryDb(spName = "Panthera onca", occDb = "gbif", occNum = 100)
#' occs <- as.data.frame(occs[[1]]$cleaned)
#' envs <- envs_worldclim(bcRes = 10,
#'                        bcSel = c("bio03", "bio04", "bio13", "bio14"),
#'                        doBrick = FALSE)
#' bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)
#' bgMask <- penvs_bgMask(occs, envs, bgExt)
#' bgSample <- penvs_bgSample(occs, bgMask, bgPtsNum = 1000)
#' folds <- 'rand'
#' partfold <- part_partitionOccs(occs, bgSample, method = folds, kfolds = 4,
#'                                bgMask = NULL, aggFact = NULL)
#'
#' @return A list of two vectors containing group assignments for occurrences (occs.grp) and background points (bg.grp).
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Andrea Paz <paz.andreita@@gmail.com>
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
    logger %>% alfred.writeLog(type = 'error', "Please select a partitioning option.")
    return()
  }

  occs.xy <- occs %>% dplyr::select(.data$longitude, .data$latitude)
  bg.xy <- bg %>% dplyr::select(.data$longitude, .data$latitude)

  if (method == 'jack') {
    group.data <- ENMeval::get.jackknife(occs.xy, bg.xy)
    logger %>% alfred.writeLog(alfred.hlSpp(spN), "Occurrences partitioned by jackknife method.")
  }

  if (method == 'rand') {
    if(is.null(kfolds)) {
      logger %>% alfred.writeLog(type = 'error', alfred.hlSpp(spN),
                          "Please specify a kfold value to use the random partition function.")
      return()
    }
    if (kfolds < 2) {
      logger %>% alfred.writeLog(type = 'error', alfred.hlSpp(spN),
                          "Please specify a kfold value greater than 1.")
      return()
    }
    group.data <- ENMeval::get.randomkfold(occs.xy, bg.xy, kfolds)
    logger %>% alfred.writeLog(alfred.hlSpp(spN),
                        "Occurrences partitioned by random k-fold (k = ", kfolds, ").")
  }

  if (method == 'block') {
    group.data <- ENMeval::get.block(occs.xy, bg.xy)
    logger %>% alfred.writeLog(alfred.hlSpp(spN), "Occurrences partitioned by block method.")
  }

  if (method == 'cb1' | method == 'cb2') {
    if(is.null(aggFact)) {
      logger %>% alfred.writeLog(type = 'error', alfred.hlSpp(spN),
                          "Please specify an aggregation factor to use checkerboard partition functions.")
      return()
    }
    if(is.na(aggFact) | aggFact <= 1) {
      logger %>% alfred.writeLog(type = 'error', alfred.hlSpp(spN),
                          "Please specify a positive aggregation factor greater than 1.")
      return()
    }
    if(is.null(bgMask)) {
      logger %>% alfred.writeLog(type = 'error', alfred.hlSpp(spN),
                          "Please specify a background mask to use checkerboard partition functions.")
      return()
    }

  }

  if(method == 'cb1') {
    alfred.smartProgress(logger, message = "Aggregating rasters...", {
      group.data <- ENMeval::get.checkerboard1(occs.xy, bgMask, bg.xy, aggFact)
    })

    logger %>% alfred.writeLog(alfred.hlSpp(spN),
                        "Occurrences partitioned by checkerboard 1 method with ",
                        "aggregation factor ", aggFact, ".")
  }

  if(method == 'cb2') {
    alfred.smartProgress(logger, message = "Aggregating rasters...", {
      group.data <- ENMeval::get.checkerboard2(occs.xy, bgMask, bg.xy, aggFact)
    })

    logger %>% alfred.writeLog(alfred.hlSpp(spN),
                        "Occurrences partitioned by checkerboard 2 method with ",
                        "aggregation factor ", aggFact, ".")
  }
  return(group.data)
}
