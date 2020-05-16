#' @title espace_pca
#' @description
#' calibrate a PCA for the background of one or two selected species
#' @details
#' This function is called by the component espace to calibrate a pca for one (default) or 2 species in environmental space.
#' When using within Wallace GUI parameters are obtained from the model object, in particular,
#' table of occurences with environmental values and table of background points with environmental values. User must be careful as these tables must contain only
#' environmental variables and not the point coordinates as outputed by model objects.
#' The provided species name(s) are only used for logger messages and not for querying or selecting occurrences.

#' @param sp.name1 character, name of species 1 to be analyzed
#' @param sp.name2 character, name of species 2 to be analyzed. Default is NULL.
#' @param occs.z1 table of occurrences with environmental values for sp1.
#' @param occs.z2 table of occurrences with environmental values for sp2 Default is NULL.
#' @param bgPts.z1 table of background points with environmental values for sp1.
#' @param bgPts.z2 table of background points with environmental values for sp2. Default is NULL.
#' @param logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @examples
#' occs <-  as.data.frame(wallace::occs_queryDb(spName = sp.name1, occDb = "gbif", occNum = 100)[[1]]$cleaned)
#'envs <- wallace::envs_worldclim(bcRes = 10, bcSel = list(TRUE,TRUE,TRUE,TRUE,TRUE), doBrick = FALSE)
#'bgExt <- wallace::penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=occs)
#'bgMask <- wallace::penvs_bgMask(occs, envs, bgExt,spN=occs)
#'bg <- wallace::penvs_bgSample(occs, bgMask, bgPtsNum = 1000,spN=occs)
#' bioclimAlg <- wallace::model_bioclim(occs, bg, partblock$occ.grp, partblock$bg.grp, bgMask,spN=occs)
#' espace_pca(sp.name1,occs.z1=bioclimAlg@@occs,bgPts.z1=bioclimAlg@@bg)
#'
#' @return A list of 14 elements of classes dudi and pca as in dudi.pca
#' @seealso dudi.pca in the ade4 package
#'
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Olivier Broennimann olivier.broennimann@@unil.ch
#' @export

espace_pca<- function(sp.name1, sp.name2 = NULL, occs.z1, occs.z2 = NULL,
                       bgPts.z1, bgPts.z2 = NULL, logger = NULL) {

  if (!is.null(bgPts.z2)) {
    data <- rbind(occs.z1, occs.z2, bgPts.z1, bgPts.z2)
    sp <- c(rep(sp.name1, nrow(occs.z1)), rep(sp.name2, nrow(occs.z2)),
            rep('bg', nrow(bgPts.z1)), rep('bg', nrow(bgPts.z2)))
    bg <- c(rep('sp', nrow(occs.z1)), rep('sp',nrow(occs.z2)),
            rep(sp.name1, nrow(bgPts.z1)), rep(sp.name2, nrow(bgPts.z2)))
  } else {
    data<-rbind(occs.z1,bgPts.z1)
    sp <- c(rep(sp.name1, nrow(occs.z1)), rep('bg',nrow(bgPts.z1)))
    bg <- c(rep('sp',nrow(occs.z1)), rep(sp.name1, nrow(bgPts.z1)))
  }

  # pca calibration and prediction of scores
  pca <- ade4::dudi.pca(data, row.w = bg > 0, center = TRUE, scale = TRUE,
                        scannf = FALSE, nf = ncol(data))

  pca$scores <- cbind(pca$li, sp, bg)

  if (is.null(sp.name2)) {
    spNames <- sp.name1
  } else {
    spNames <- paste(em(spName(sp.name1)), "and", em(spName(sp.name2)))
  }

  logger %>% writeLog(paste0("Principle components analysis run for ",
                             spNames, "."))

  return(pca)
}
