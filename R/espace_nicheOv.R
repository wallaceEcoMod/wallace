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
#'sp.name1<-"Panthera onca"
#'sp.name2<-"Procyon lotor"
#'species<-c(sp.name1,sp.name2)
#"model<-list()
#'for (i in 1:2){
#'  occs <-  occs_queryDb(spName = species[i], occDb = "gbif", occNum = 100)
#'  occs <- as.data.frame(occs[[1]]$cleaned)
#'    occs <- poccs_thinOccs(occs = occs, thinDist = 10,spN=species[i])
#' envs <- envs_worldclim(bcRes = 10,  bcSel = c("bio01","bio02","bio13","bio14"), doBrick = FALSE)
#' bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=species[i])
#'bgMask <- penvs_bgMask(occs, envs, bgExt,spN=species[i])
#'bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 1000,spN=species[i])
#' partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,aggFact = NULL,spN=species[i])
#'  bioclimAlg <- model_bioclim(occs, bg, partblock$occ.grp, partblock$bg.grp, bgMask,spN=species[i])
#' model[i]<-bioclimAlg }
##Remove coordinates (lat/long from tables)
#'occs.z1<-model[[1]]@@occs[3:length(model[[1]]@@occs)]
#'occs.z2<-model[[2]]@@occs[3:length(model[[2]]@@occs)]
#'bgPts.z1<-model[[1]]@@bg[3:length(model[[1]]@@bg)]
#'bgPts.z2<-model[[2]]@@bg[3:length(model[[2]]@@bg)]
#'Testpca<-espace_pca(sp.name1,sp.name2,occs.z1,occs.z2,bgPts.z1,bgPts.z2)
#'TestOccDens<-espace_occDens(sp.name1, sp.name2,Testpca)
#'TestNicheOv<-espace_nicheOv(z1=TestOccDens[[sp.name1]], z2=TestOccDens[[sp.name2]], iter=100 , equivalency = TRUE, similarity = TRUE, logger = NULL)
#'
#' @return A list of 4 elements if all is set to TRUE. Elements are overlap (Schoener's D), USE (ecopstat.niche.dyn.index), equiv and simil
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @author Olivier Broennimann <olivier.broennimann@@unil.ch>
# @note

#' @seealso  \code{\link{espace_pca}} \code{\link{espace_occDens}} \code{\link[ecospat]{niche.overlap}}  \code{\link[ecospat]{niche.dyn.index}} \code{\link[ecospat]{niche.equivalency.test}} \code{\link[ecospat]{niche.similarity.test}}
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
