#' calibrate a PCA for the background of two selected species
#' 
#' @param z1 ecospat niche object for species 1 from cESpace_OccDens
#' @param z2 ecospat niche object for species 2 from cESpace_OccDens

cESpace_OccDens<- function(z1,z2,iter=100,equivalency=F,similarity=T, logs=NULL, shiny=FALSE) {
  
  NicheOverlap<-list()
  
  # Schoener's D
  NicheOverlap$overlap<-ecospat::ecospat.niche.overlap(z1,z2,cor=T)
  
  
  #unfilling, stability, expansion indices (Guisan et al. 2014 TREE)
  NicheOverlap$USE<-ecospat::ecospat.niche.dyn.index(z1,z2,intersection=0)$dynamic.index.w 
  
  #niche tests
  if(equivalency) NicheOverlap$equiv<-ecospat::ecospat.niche.equivalency.test(z1,z2,iter,alternative="greater")
  if(similarity) NicheOverlap$simil<-ecospat::ecospat.niche.similarity.test(z1,z2,iter,alternative="greater",rand.type=1)
  
  #plots
  layout(matrix(c(1,1,1,1,1,1,1,1,2,2,3,3), 4, 3, byrow = F))
  #layout.show(3)
  
  text<-paste("Overlap D = ", round(NicheOverlap$overlap$D,2),"\n",
              "Sp1 only :", round(NicheOverlap$USE[3],2),
              " | Sp2 only :",round(NicheOverlap$USE[1],2),
              " | Both :", round(NicheOverlap$USE[2],2))
  
  ecospat::ecospat.plot.niche.dyn(z1,z2,0.5,text,colz1 = "blue",colz2 = "red",colinter = "purple",colZ1="blue",colZ2="red")
  if(!is.null(NicheOverlap$equiv)) ecospat::ecospat.plot.overlap.test(NicheOverlap$equiv,"D","Equivalency test")
  if(!is.null(NicheOverlap$simil)) ecospat::ecospat.plot.overlap.test(NicheOverlap$simil,"D","Similarity test")
  
  layout(1,1,1)
  
  return(NicheOverlap)
}
