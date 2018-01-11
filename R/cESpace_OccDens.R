#' calibrate a PCA for the background of two selected species
#' 
#' @param pca output of pca component
#' @param spSel species selected (1 or 2)

cESpace_OccDens<- function(pca,spSel, logs=NULL, shiny=FALSE) {
  bg<-pca[[1]]$bg
  sp<-pca[[1]]$sp
  scores.bg12<-pca[[1]][bg>0,1:2]
  scores.bg<-pca[[1]][bg==spSel,1:2]
  scores.occs<-pca[[1]][sp==spSel,1:2]
  z<- ecospat::ecospat.grid.clim.dyn(scores.bg12,scores.bg,scores.occs,100)
  
  #plots
  #ecospat::ecospat.plot.niche(z,paste("species: ",spSel))
  return(z)
}
