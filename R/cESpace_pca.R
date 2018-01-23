#' calibrate a PCA for the background of two selected species
#' 
#' @param bgMask1 rasterStack environmental grids for sp1 masked by its background
#' @param bgMask2 rasterStack environmental grids for sp2 masked by its background
#' @param occs.z1 table of occurrences with environmental values for sp1
#' @param occs.z2 table of occurrences with environmental values for sp2

cESpace_pca<- function(sp.name1, sp.name2 = NULL, occs.z1, occs.z2 = NULL, 
                       bgPts.z1, bgPts.z2 = NULL, logs = NULL, shiny = FALSE) {
  
  if(!is.null(bgPts.z2)) {
    
    data <- rbind(occs.z1, occs.z2, bgPts.z1, bgPts.z2)
    sp <- c(rep(sp.name1, nrow(occs.z1)), rep(sp.name2, nrow(occs.z2)),
            rep('bg', nrow(bgPts.z1)), rep('bg', nrow(bgPts.z2)))
    bg <- c(rep('sp', nrow(occs.z1)), rep('sp',nrow(occs.z2)),
            rep(sp.name1, nrow(bgPts.z1)), rep(sp.name2, nrow(bgPts.z2)))
    
  }else{
    
    data<-rbind(occs.z1,bgPts.z1)
    sp <- c(rep(sp.name1, nrow(occs.z1)), rep('bg',nrow(bgPts.z1)))
    bg <- c(rep('sp',nrow(occs.z1)), rep(sp.name1, nrow(bgPts.z1)))
  }
  
  # pca calibration and prediction of scores
  
  pca <- ade4::dudi.pca(data, row.w = bg > 0, center = TRUE, scale = TRUE, 
                        scannf = FALSE, nf = 2)
  
  scores <- cbind(pca$li, sp, bg)
  
  pca <- list(scores = scores, eig = pca$eig, co = pca$co)
  
  return(pca)
}
