#' @title espace_nicheOv Niche Overlap
#' @description Function evaluates niche overlap between the two species for
#'   which the occurrence density grid was computed
#'
#' @details
#' The niche overlap quantification is based on the occurrence densities and
#'   the densities of environmental conditions available in the background extent
#'   that are estimated in the module Occurrence Density Grid. The function
#'   computes 4 different things; Schoener's D, unfilling, stability,
#'   expansion indices (Guisan et al. 2014 TREE), and tests for niche
#'   equivalency and niche similarity.
#'
#' @param z1 ecospat niche object for species 1 from espace_occDens.
#' @param z2 ecospat niche object for species 2 from espace_occDens.
#' @param iter numeric. Number of iterations.
#' @param equivalency logical. Whether to run equivalency test. Default is FALSE.
#' @param similarity logical. Whether to run similarity test. Default is TRUE.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL.
#' @examples
#' \dontrun{
#' sp.name1 <- "Bassaricyon_alleni"
#' sp.name2 <- "Bassaricyon_neblina"
#' envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
#'                                            package = "wallace"),
#'                       pattern = ".tif$", full.names = TRUE),
#'                       rasName = list.files(system.file("extdata/wc",
#'                                            package = "wallace"),
#'                       pattern = ".tif$", full.names = FALSE))
#'
#' occs.z1 <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
#'                     package = "wallace"))
#' occs.z2 <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
#'                     package = "wallace"))
#'
#' bgPts.z1 <- read.csv(system.file("extdata/Bassaricyon_alleni_bgPoints.csv",
#'                      package = "wallace"))
#' bgPts.z2 <- read.csv(system.file("extdata/Bassaricyon_neblina_bgPoints.csv",
#'                      package = "wallace"))
#'
#' occsExt.z1 <- raster::extract(envs, occs.z1[, c("longitude", "latitude")])
#' occsExt.z2 <- raster::extract(envs, occs.z2[, c("longitude", "latitude")])
#' bgExt.z1 <- raster::extract(envs, bgPts.z1[, c("longitude", "latitude")])
#' bgExt.z2 <- raster::extract(envs, bgPts.z2[, c("longitude", "latitude")])
#' pcaZ <- espace_pca(sp.name1, sp.name2,
#'                    occsExt.z1, occsExt.z2,
#'                    bgExt.z1, bgExt.z2)
#' occDens <- espace_occDens(sp.name1, sp.name2, pcaZ)
#' nicheOv <- espace_nicheOv(z1 = occDens[[sp.name1]],
#'                           z2 = occDens[[sp.name2]],
#'                           iter = 100, equivalency = TRUE,
#'                           similarity = TRUE)
#' }
#'
#' @return A list of 4 elements if all is set to TRUE. Elements are overlap
#'   (Schoener's D), USE (ecopstat.niche.dyn.index), equiv and simil.
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Olivier Broennimann <olivier.broennimann@@unil.ch>
#' @seealso  \code{\link{espace_pca}} \code{\link{espace_occDens}}
#'   \code{\link[ecospat]{ecospat.niche.overlap}}
#'   \code{\link[ecospat]{ecospat.niche.dyn.index}}
#'   \code{\link[ecospat]{ecospat.niche.equivalency.test}}
#'   \code{\link[ecospat]{ecospat.niche.similarity.test}}
#' @export

espace_nicheOv <- function(z1, z2, iter = 100, equivalency = FALSE,
                           similarity = TRUE, logger = NULL) {
  nicheOv <- list()

  # Schoener's D
  nicheOv$overlap <- ecospat::ecospat.niche.overlap(z1, z2, cor = TRUE)


  # Unfilling, stability, expansion indices (Guisan et al. 2014 TREE)
  nicheOv$USE <- ecospat::ecospat.niche.dyn.index(z1, z2,
                                                  intersection = 0)$dynamic.index.w

  # niche tests
  if (equivalency == TRUE) {
    smartProgress(logger, message = "Calculating niche equivalency...", {
      nicheOv$equiv <- ecospat::ecospat.niche.equivalency.test(
        z1, z2, iter, overlap.alternative = "higher"
      )
    })
  }

  if (similarity == TRUE) {
    smartProgress(logger, message = "Calculating niche similarity", {
      nicheOv$simil <- ecospat::ecospat.niche.similarity.test(
        z1, z2, iter, overlap.alternative = "higher", rand.type = 1
      )
    })
  }

  return(nicheOv)
}
