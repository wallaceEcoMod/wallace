#' @title espace_pca Principal component analysis
#' @description Principal component analysis to reduce dimensionalty of environmental space
#' @details
#' This function is called by the component espace to calibrate a pca for one (default) or 2 species in environmental space.
#' When using within Wallace GUI parameters are obtained from the model object, in particular,
#' table of occurences with environmental values and table of background points with environmental values. User must be careful as these tables must contain only
#' environmental variables and not the point coordinates as outputed by model objects. The PCA is calibrated over the whole set of background points
#' The provided species name(s) are only used for logger messages and not for querying or selecting occurrences.

#' @param sp.name1 character. Name of species 1 to be analyzed
#' @param sp.name2 character. Name of species 2 to be analyzed. Default is NULL
#' @param occs.z1 table of occurrences with environmental values only for sp1
#' @param occs.z2 table of occurrences with environmental values only for sp2. Default is NULL
#' @param bgPts.z1 table of background points with environmental values only for sp1
#' @param bgPts.z2 table of background points with environmental values only for sp2. Default is NULL
#' @param logger Stores all notification messages to be displayed in the Log Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
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
  logger %>% alfred.writeLog(alfred.hlSpp(spNames), "Principal component analysis.")

  return(pca)
}
