#' calibrate a PCA for the background of two selected species
#' 
#' @param bgMask1 rasterStack environmental grids for sp1 masked by its background
#' @param bgMask2 rasterStack environmental grids for sp2 masked by its background
#' @param occs1 table of occurrences with environmental values for sp1
#' @param occs2 table of occurrences with environmental values for sp2


#generate fake input to play with
#'
#'occs<-occ(query="Centaurea stoebe")$gbif$data[[1]][,2:3]
#'
#'bgMask1<-crop(getData('worldclim', var='bio', res=5),extent(0,30,40,60))
#'occs1<-occs[occs$longitude>0,]
#'occs1<-na.exclude(cbind(longitude=rep(1,nrow(occs1)),latitude=rep(1,nrow(occs1)),extract(bgMask1,occs1)))
#'
#'bgMask2<-crop(getData('worldclim', var='bio', res=5),extent(-140,-70,30,65))
#'occs2<-occs[occs$longitude<0,]
#'occs2<-na.exclude(cbind(longitude=rep(1,nrow(occs2)),latitude=rep(1,nrow(occs2)),extract(bgMask2,occs2)))
  
cESpace_PCA<- function(bgMask1, bgMask2=NULL, occs1, occs2=NULL, logs=NULL, shiny=FALSE) {
  
  library(ade4)
  library(raster)
  
  df1<-na.exclude(getValues(bgMask1))
  df2<-na.exclude(getValues(bgMask2))
  
  data<-rbind(occs1[,-(1:2)],occs2[,-(1:2)],df1,df2)
  
  sp<-c(rep(1,nrow(occs1)),rep(2,nrow(occs2)),rep(0,nrow(df1)),rep(0,nrow(df2)))
  bg<-c(rep(0,nrow(occs1)),rep(0,nrow(occs2)),rep(1,nrow(df1)),rep(2,nrow(df2)))
  
  #pca calibration and predicition of scores
  
  pca <-dudi.pca(data, row.w=bg>0, center = T, scale = T, scannf = F, nf = 2)
  
  scores<-cbind(pca$li,sp,bg)
  
  s.class(scores[sp==0,1:2],as.factor(scores[sp==0,]$bg),col=c("blue","red"),cstar = 0, cpoint=0.1)
  s.corcircle(pca$co, lab = names(pca$tab), full = FALSE, box = TRUE)
  
  pca<-list(scores,pca$eig,pca$co)
  
  return(pca)
}
