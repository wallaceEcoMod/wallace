
getWCTiles <- function(occs) {
  library(raster)
  c <- unique(occs %/% 30)
  coordTiles <-  data.frame(occs[row.names(c),])
  x <- list()
  for (i in 1:nrow(coordTiles)) {
    x[[i]] <- getData('worldclim', var = 'bio', res = 0.5, lon = coordTiles[i, 1], lat = coordTiles[i, 2])
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
#t2-t1 # 5 tiles ->24.75397 mins, memory.size = 2786.77
