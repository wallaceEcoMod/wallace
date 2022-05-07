#' @title espace_pca Principal component analysis
#' @description Principal component analysis to reduce dimensionality of
#'   environmental space
#' @details
#' This function is called by the component espace to calibrate a PCA for
#'   2 species in environmental space. When using within Wallace,
#'   GUI parameters are obtained from the model object, in particular, table of
#'   occurrences with environmental values and table of background points with
#'   environmental values. User must be careful as these tables must contain only
#'   environmental variables and not the point coordinates as outputted by model
#'   objects. The PCA is calibrated over the whole set of background points.
#'   The provided species name(s) are only used for logger messages and not for
#'   querying or selecting occurrences.

#' @param sp.name1 character. Name of species 1 to be analyzed.
#' @param sp.name2 character. Name of species 2 to be analyzed. Default is NULL.
#' @param occs.z1 table of occurrences with environmental values only for sp1.
#' @param occs.z2 table of occurrences with environmental values only for sp2.
#'   Default is NULL.
#' @param bgPts.z1 table of background points with environmental values only
#'   for sp1.
#' @param bgPts.z2 table of background points with environmental values only
#'   for sp2. Default is NULL.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#'   otherwise leave the default NULL
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
#' }
#' @return A list of 14 elements of classes dudi and pca as in dudi.pca
#' @seealso \code{\link[ade4]{dudi.pca}}
#'
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Olivier Broennimann <olivier.broennimann@@unil.ch>
#' @export

espace_pca <- function(sp.name1, sp.name2 = NULL, occs.z1, occs.z2 = NULL,
                       bgPts.z1, bgPts.z2 = NULL, logger = NULL) {

  if (!is.null(bgPts.z2)) {
    data <- rbind(occs.z1, occs.z2, bgPts.z1, bgPts.z2)
    sp <- c(rep(sp.name1, nrow(occs.z1)), rep(sp.name2, nrow(occs.z2)),
            rep('bg', nrow(bgPts.z1)), rep('bg', nrow(bgPts.z2)))
    bg <- c(rep('sp', nrow(occs.z1)), rep('sp', nrow(occs.z2)),
            rep(sp.name1, nrow(bgPts.z1)), rep(sp.name2, nrow(bgPts.z2)))
  } else {
    data <- rbind(occs.z1, bgPts.z1)
    sp <- c(rep(sp.name1, nrow(occs.z1)), rep('bg', nrow(bgPts.z1)))
    bg <- c(rep('sp', nrow(occs.z1)), rep(sp.name1, nrow(bgPts.z1)))
  }

  # pca calibration and prediction of scores
  pca <- ade4::dudi.pca(data, row.w = bg > 0, center = TRUE, scale = TRUE,
                        scannf = FALSE, nf = ncol(data))

  pca$scores <- cbind(pca$li, sp, bg)

  if (is.null(sp.name2)) {
    spNames <- sp.name1
  } else {
    spNames <- paste0(sp.name1, " and ", sp.name2)
  }
  logger %>% writeLog(hlSpp(spNames),
                             "Principal component analysis.")

  return(pca)
}
