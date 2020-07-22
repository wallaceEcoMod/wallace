
#' @title Occurence density grid
#' @description calculates the part of environmental space more densly populated by species & the avaialbility of environmental conditions in the background
#'
#' @details
#' This fuctions implements a density estimation for each region in the environmental space (gridded at 100*100 pixels).
#' Then an occurrence density is estimated using a kernel density approach. The density of environmental conditions in the background is calcuated in the same way.
#
#' @param sp.name1 character, name of species 1 to be analyzed
#' @param sp.name2 character, name of species 2 to be analyzed.
#' @param pca pca output of pca component ( in list format)
#' @param logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
# @keywords
#'
#' @examples
#'###SET PARAMETERS (running model)
#'sp.name1<-"Panthera onca"
#'sp.name2<-"Procyon lotor"
#'species<-c(sp.name1,sp.name2)
#' ##make models for both species to get necessary inputs
#'model<-list()
#' for (i in 1:2){
# 'occs <-  occs_queryDb(spName = species[i], occDb = "gbif", occNum = 100)
#' occs <- as.data.frame(occs[[1]]$cleaned)
#' occs <- poccs_thinOccs(occs = occs, thinDist = 10,spN=occs)
#' envs <- envs_worldclim(bcRes = 10,  bcSel = c("bio01","bio02","bio13","bio14"), doBrick = FALSE)
#' bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=occs)
#' bgMask <- penvs_bgMask(occs, envs, bgExt,spN=occs)
#'  bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 1000,spN=occs)
#' partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,aggFact = NULL,spN=occs)
#'bioclimAlg <- model_bioclim(occs, bg, partblock$occ.grp, partblock$bg.grp, bgMask,spN=occs)
#'model[i]<-bioclimAlg }
#' ##Set parameters for running PCA and occDens
#'occs.z1<-model[[1]]@@occs[3:length(model[[1]]@@occs)]
#' occs.z2<-model[[2]]@@occs[3:length(model[[2]]@@occs)]
#' bgPts.z1<-model[[1]]@@bg[3:length(model[[1]]@@bg)]
#' bgPts.z2<-model[[2]]@@bg[3:length(model[[2]]@@bg)]
#' Testpca<-espace_pca(sp.name1,sp.name2,occs.z1,occs.z2,bgPts.z1,bgPts.z2)
#' ###RUN FUNCTION
#' TestOccDens<-espace_occDens(sp.name1, sp.name2,Testpca)
#'
#' @return Returns a list of 2 lists (one for each species). Each list is an ecospat noche object that contains 10 species specific slots with information outputed by ecospat::grid.clim.dyn.
#' z.uncor is the density of occurrence of the species and z.cor the occupancy of the environment by the species. It has the input parameters as individual slots.
#' @author Jamie Kass <jamie.m.kass@@gmail.com >
#' @author Olivier Broennimann <olivier.broennimann@@unil.ch>
# @note
#' @seealso \code{\link{espace_pca}} \code{\link{espace_nicheOv}} \code{\link[ecospat]{grid.clim.dyn}} \code{\link[adeHabitatHR]{kernelUD}}
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

espace_occDens <- function(sp.name1, sp.name2, pca, logger = NULL) {
  bg <- pca$scores$bg
  sp <- pca$scores$sp
  scores.bg12 <- pca$scores[bg != 'sp', 1:2]
  scores.bg1 <- pca$scores[bg == sp.name1, 1:2]
  scores.occs1 <- pca$scores[sp == sp.name1, 1:2]
  scores.bg2 <- pca$scores[bg == sp.name2, 1:2]
  scores.occs2 <- pca$scores[sp == sp.name2, 1:2]
  smartProgress(logger, message = "Running occurrence density grids...", {
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

  logger %>% writeLog(hlSpp(paste0(sp.name1, " and ", sp.name2)),
                      "Occurrence density grid.")

  return(occDens)
}
