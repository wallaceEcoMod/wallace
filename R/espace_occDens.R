
#' @title Occurence density grid
#' @description calculates the part of environmental space more densly
#'   populated by species & the availability of environmental conditions in the
#'   background
#'
#' @details
#' This fuctions implements a density estimation for each region in the
#'   environmental space (gridded at 100*100 pixels). Then an occurrence
#'   density is estimated using a kernel density approach. The density of
#'   environmental conditions in the background is calcuated in the same way.
#
#' @param sp.name1 character name of species 1 to be analyzed.
#' @param sp.name2 character name of species 2 to be analyzed.
#' @param pca pca output of pca component ( in list format)
#' @param logger stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL.
#' @examples
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
#'
#' @return Returns a list of 2 lists (one for each species). Each list is an
#'   ecospat noche object that contains 10 species specific slots with
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
  alfred.smartProgress(logger, message = "Running occurrence density grids...", {
    occDens1 <- ecospat::ecospat.grid.clim.dyn(scores.bg12, scores.bg1,
                                               scores.occs1, 100)
   # incProgress(1/2)
    occDens2 <- ecospat::ecospat.grid.clim.dyn(scores.bg12, scores.bg2,
                                               scores.occs2, 100)
  # incProgress(1/2)
  })
  occDens <- list()
  occDens[[sp.name1]] <- occDens1
  occDens[[sp.name2]] <- occDens2

  logger %>% alfred.writeLog(alfred.hlSpp(paste0(sp.name1, " and ", sp.name2)),
                      "Occurrence density grid.")

  return(occDens)
}
