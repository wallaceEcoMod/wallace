#' calibrate a PCA for the background of two selected species
#' 
#' @param z1 ecospat niche object for species 1 from cESpace_OccDens
#' @param z2 ecospat niche object for species 2 from cESpace_OccDens
#' @export

cESpace_nicheOv <- function(z1, z2, iter = 100, equivalency = FALSE, similarity = TRUE, 
                            shinyLogs = NULL) {
  nicheOv <- list()
  
  # Schoener's D
  nicheOv$overlap <- ecospat::ecospat.niche.overlap(z1, z2, cor=TRUE)
  
  
  #unfilling, stability, expansion indices (Guisan et al. 2014 TREE)
  nicheOv$USE <- ecospat::ecospat.niche.dyn.index(z1, z2, intersection = 0)$dynamic.index.w 
  
  #niche tests
  if(equivalency) {
    smartProgress(shinyLogs, message = "Calculating niche equivalency...", {
      nicheOv$equiv <- ecospat::ecospat.niche.equivalency.test(z1, z2, iter, alternative = "greater")
    })
  }
  
  if(similarity) {
    smartProgress(shinyLogs, message = "Calculating niche similarity", {
      nicheOv$simil <- ecospat::ecospat.niche.similarity.test(z1, z2, iter, alternative = "greater", rand.type = 1)
    })
  }
  
  return(nicheOv)
}
