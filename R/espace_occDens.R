# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# espace_occDens.R
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

#' @title Occurrence density grid
#' @description calculates the part of environmental space more densely
#'   populated by species & the availability of environmental conditions in the
#'   background
#'
#' @details
#' This function implements a density estimation for each region in the
#'   environmental space (gridded at 100*100 pixels). Then an occurrence
#'   density is estimated using a kernel density approach. The density of
#'   environmental conditions in the background is calculated in the same way.
#
#' @param sp.name1 character name of species 1 to be analyzed.
#' @param sp.name2 character name of species 2 to be analyzed.
#' @param pca pca output of pca component ( in list format)
#' @param logger stores all notification messages to be displayed in the Log
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
#' }
#'
#' @return Returns a list of 2 lists (one for each species). Each list is an
#'   ecospat niche object that contains 10 species specific slots with
#'   information outputed by ecospat::grid.clim.dyn. z.uncor is the density of
#'   occurrence of the species and z.cor the occupancy of the environment by
#'   the species. It has the input parameters as individual slots.
#' @author Jamie Kass <jamie.m.kass@@gmail.com >
#' @author Olivier Broennimann <olivier.broennimann@@unil.ch>
#' @seealso \code{\link{espace_pca}} \code{\link{espace_nicheOv}}
#'  \code{\link[ecospat]{ecospat.grid.clim.dyn}}
#' @export

espace_occDens <- function(sp.name1, sp.name2, pca, logger = NULL) {
  bg <- pca$scores$bg
  sp <- pca$scores$sp
  scores.bg12 <- pca$scores[bg != 'sp', 1:2]
  scores.bg1 <- pca$scores[bg == sp.name1, 1:2]
  scores.occs1 <- pca$scores[sp == sp.name1, 1:2]
  scores.bg2 <- pca$scores[bg == sp.name2, 1:2]
  scores.occs2 <- pca$scores[sp == sp.name2, 1:2]
  # Checking if occs points are outside of PCA bg extent
  axis.min <- apply(scores.bg12, 2, min, na.rm = TRUE)
  axis.max <- apply(scores.bg12, 2, max, na.rm = TRUE)

  occs.check1 <- data.frame(cbind(
    (scores.occs1[, 1] - axis.min[1]) / abs(axis.max[1] - axis.min[1]),
    (scores.occs1[, 2] - axis.min[2]) / abs(axis.max[2]  - axis.min[2])
  ))

  occs.check2 <- data.frame(cbind(
    (scores.occs2[, 1] - axis.min[1]) / abs(axis.max[1] - axis.min[1]),
    (scores.occs2[, 2] - axis.min[2]) / abs(axis.max[2]  - axis.min[2])
  ))
  out.occ1 <- which(occs.check1 < 0, arr.ind = TRUE)
  out.occ2 <- which(occs.check2 < 0, arr.ind = TRUE)

  if (nrow(out.occ1) > 0) {
    scores.occs1 <- scores.occs1[-(out.occ1[1:(length(out.occ1)/2)]), ]
    logger %>% writeLog(
      type = "warning",
      hlSpp(sp.name1),
      "Occurrences outside of the extent of the background points in the ",
      "PCA space. A number of occurrences (n = ", nrow(out.occ1), ") were ",
      "removed for continuing with the occurrence density grid analysis."
    )
  }

  if (nrow(out.occ2) > 0) {
    scores.occs2 <- scores.occs2[-(out.occ2[1:(length(out.occ2)/2)]), ]
    logger %>% writeLog(
      type = "warning",
      hlSpp(sp.name2),
      "Occurrences outside of the extent of the background points in the ",
      "PCA space. A number of occurrences (n = ", nrow(out.occ2), ") were ",
      "removed for continuing with the occurrence density grid analysis."
    )
  }

  smartProgress(logger, message = "Running occurrence density grids...", {

    occDens1 <- ecospat::ecospat.grid.clim.dyn(scores.bg12, scores.bg1,
                                               scores.occs1, 100,
                                               kernel.method = "ks")
   # incProgress(1/2)
    occDens2 <- ecospat::ecospat.grid.clim.dyn(scores.bg12, scores.bg2,
                                               scores.occs2, 100,
                                               kernel.method = "ks")
  # incProgress(1/2)
  })
  occDens <- list()
  occDens[[sp.name1]] <- occDens1
  occDens[[sp.name2]] <- occDens2

  logger %>% writeLog(hlSpp(paste0(sp.name1, " and ", sp.name2)),
                      "Occurrence density grid.")

  return(occDens)
}
