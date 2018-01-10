#' calibrate a PCA for the background of two selected species
#' 
#' @param bgMask1 rasterStack environmental grids for sp1 masked by its background
#' @param bgMask2 rasterStack environmental grids for sp1 masked by its background

bgMask1<-crop(getData('worldclim', var='bio', res=5),extent(-140,-70,30,65))
bgMask2<-crop(getData('worldclim', var='bio', res=5),extent(0,30,40,60))
occs<-occ(query="Centaurea stoebe")$gbif$data[[1]][,2:3]
occs1<-occs[occs$longitude<0,]
occs2<-occs[occs$longitude>0,]

  
cESpace_PCA<- function(bgMask1, bgMask2=NULL, occs1, occs2=NULL, logs=NULL, shiny=FALSE) {
  library(ade4)
  library(raster)
  
  bg1<-bgMask1[[1]]*0+1
  sp1<-rasterFromXYZ(occs1)
  names(bg1)<-"bg"
  bg2<-bgMask2[[1]]*0+2
  names(bg2)<-"bg"
  nl<-nlayers(bgMask1)
  bgMask1[[(nl+1)]]<-bg1
  bgMask2[[(nl+1)]]<-bg2
  
  bgMaskdf<-rbind(getValues(bgMask1),getValues(bgMask2))
  bgMaskdf<-na.exclude(bgMaskdf)
  
  pca <-dudi.pca(bgMaskdf[,-(nl+1)], center = T, scale = T, scannf = F, nf = 2)
  
  s.class(pca$li,as.factor(bgMaskdf[,nl+1]),col=c("blue","red"),cstar = 0, cpoint=0.1)
  s.corcircle(pca$co, lab = names(pca$tab), full = FALSE, box = TRUE)
  
  ecospat.
  
  return(dudi.pca)
}

taxon_name