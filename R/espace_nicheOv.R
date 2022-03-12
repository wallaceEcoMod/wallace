#' @title espace_nicheOv Niche Overlap
#' @description Function evaluates niche overlap between the two species for which the occurrence density grid was computed
#'
#' @details
#' The niche overlap quantification is based on the occurrence densities and the densities of environmental conditions available in the background extent that are estimated in the module Occurrence Density Grid.
#' The function computes 4 different things, Schoener's D, unfilling, stability, expansion indices (Guisan et al. 2014 TREE), and tests for niche equivalency and niche similarity.
#'
#' @param z1 ecospat niche object for species 1 from espace_occDens
#' @param z2 ecospat niche object for species 2 from espace_occDens
#' @param iter numeric. Number of iterations
#' @param equivalency logical. Whether to run equivalency test. Default is FALSE
#' @param similarity logical. Whether to run similarity test. Default is TRUE
#' @param logger Stores all notification messages to be displayed in the Log Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
# @keywords
#'
#' @examples
#' envs <- envs_worldclim(bcRes = 10,
#'                        bcSel = c("bio01","bio02","bio13","bio14"),
#'                        doBrick = FALSE)
#' sp.name1 <- "Panthera onca"
#' sp.name2 <- "Procyon lotor"
#' occs.z1 <- occs_queryDb(spName = sp.name1, occDb = "gbif",
#'                         occNum = 100)[[1]]$cleaned
#' occs.z2 <- occs_queryDb(spName = sp.name2, occDb = "gbif",
#'                         occNum = 100)[[1]]$cleaned
#' bgExt.z1 <- penvs_bgExtent(occs.z1, bgSel = 'bounding box', bgBuf = 0.5)
#' bgExt.z2 <- penvs_bgExtent(occs.z2, bgSel = 'bounding box', bgBuf = 0.5)
#' bgMask.z1 <- penvs_bgMask(occs.z1, envs, bgExt.z1)
#' bgMask.z2 <- penvs_bgMask(occs.z2, envs, bgExt.z2)
#' bgPts.z1 <- penvs_bgSample(occs.z1, bgMask.z1, bgPtsNum = 1000)
#' bgPts.z2 <- penvs_bgSample(occs.z2, bgMask.z2, bgPtsNum = 1000)
#' occsExt.z1 <- na.omit(raster::extract(envs,
#'                                       occs.z1[, c("longitude", "latitude")]))
#' occsExt.z2 <- na.omit(raster::extract(envs,
#'                                       occs.z2[, c("longitude", "latitude")]))
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
#'
#' @return A list of 4 elements if all is set to TRUE. Elements are overlap (Schoener's D), USE (ecopstat.niche.dyn.index), equiv and simil
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Olivier Broennimann <olivier.broennimann@@unil.ch>
# @note

#' @seealso  \code{\link{espace_pca}} \code{\link{espace_occDens}} \code{\link[ecospat]{ecospat.niche.overlap}}  \code{\link[ecospat]{ecospat.niche.dyn.index}} \code{\link[ecospat]{ecospat.niche.equivalency.test}} \code{\link[ecospat]{ecospat.niche.similarity.test}}
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be
# linked in the documentation.
#' @export

espace_nicheOv <- function(z1, z2, iter = 100, equivalency = FALSE,
                           similarity = TRUE, logger = NULL) {
  nicheOv <- list()

  # Schoener's D
  nicheOv$overlap <- ecospat::ecospat.niche.overlap(z1, z2, cor = TRUE)


  # Unfilling, stability, expansion indices (Guisan et al. 2014 TREE)
  nicheOv$USE <- ecospat::ecospat.niche.dyn.index(z1, z2, intersection = 0)$dynamic.index.w

  # niche tests
  if (equivalency == TRUE) {
    alfred.smartProgress(logger, message = "Calculating niche equivalency...", {
      nicheOv$equiv <- ecospat::ecospat.niche.equivalency.test(
        z1, z2, iter, overlap.alternative = "higher"
      )
    })
  }

  if (similarity == TRUE) {
    alfred.smartProgress(logger, message = "Calculating niche similarity", {
      nicheOv$simil <- ecospat::ecospat.niche.similarity.test(
        z1, z2, iter, overlap.alternative = "higher", rand.type = 1
      )
    })
  }

  return(nicheOv)
}
