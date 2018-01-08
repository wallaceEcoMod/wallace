
getWCTiles<-function(occs){
  library(raster)
  c<-unique(occs%/%30*30+0.000001)
  x<-list()
  for (i in 1:nrow(c)){
    x[[i]]<-getData('worldclim', var='tmin', res=0.5, lon=c[i,1], lat=c[i,2])
  }
  y <- do.call(merge, x)
  return(y)
}

### test case
#
#library(spocc)
#
#t1<-Sys.time()
#occs<-occ(query="Centaurea stoebe")$gbif$data[[1]][,2:3]
#mosaic<-getWCTiles(occs)
#
#plot(mosaic[[1]])
#points(occs)
#t2<-Sys.time() 
#t2-t1 #13.35181 mins
