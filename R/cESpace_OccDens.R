#' calibrate a PCA for the background of two selected species
#' 
#' @param pca output of pca component
#' @param spSel species selected (1 or 2)

cESpace_occDens<- function(sp.name1, sp.name2, pca, shinyLogs = NULL) {
  bg <- pca$scores$bg
  sp <- pca$scores$sp
  scores.bg12 <- pca$scores[bg != 'bg', 1:2]
  scores.bg1 <- pca$scores[bg == sp.name1, 1:2]
  scores.occs1 <- pca$scores[sp == sp.name1, 1:2]
  scores.bg2 <- pca$scores[bg == sp.name2, 1:2]
  scores.occs2 <- pca$scores[sp == sp.name2, 1:2]
  occDens1 <- ecospat::ecospat.grid.clim.dyn(scores.bg12, scores.bg1, scores.occs1, 100)
  occDens2 <- ecospat::ecospat.grid.clim.dyn(scores.bg12, scores.bg2, scores.occs2, 100)
  occDens <- list()
  occDens[[sp.name1]] <- occDens1
  occDens[[sp.name2]] <- occDens2
  return(occDens)
}
