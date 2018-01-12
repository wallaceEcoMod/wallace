#' calibrate a PCA for the background of two selected species
#' 
#' @param z1 ecospat niche object for species 1 from cESpace_OccDens
#' @param z2 ecospat niche object for species 2 from cESpace_OccDens

cESpace_nicheOv <- function(z1, z2, iter = 100, equivalency = FALSE, similarity = TRUE, 
                            logs = NULL, shiny = FALSE) {
  nicheOv <- list()
  
  # Schoener's D
  nicheOv$overlap <- ecospat::ecospat.niche.overlap(z1, z2, cor=TRUE)
  
  
  #unfilling, stability, expansion indices (Guisan et al. 2014 TREE)
  nicheOv$USE <- ecospat::ecospat.niche.dyn.index(z1, z2, intersection = 0)$dynamic.index.w 
  
  #niche tests
  if(equivalency) {
    if(shiny == TRUE) {
      withProgress(message = "Calculating niche equivalency...", {
        nicheOv$equiv <- ecospat::ecospat.niche.equivalency.test(z1, z2, iter, alternative = "greater")
      })
      }else{
        nicheOv$equiv <- ecospat::ecospat.niche.equivalency.test(z1, z2, iter, alternative = "greater")
      }
  }
  
  if(similarity) {
    if(shiny == TRUE) {
      withProgress(message = "Calculating niche similarity", {
        nicheOv$simil <- ecospat::ecospat.niche.similarity.test(z1, z2, iter, alternative = "greater", rand.type = 1)
      })
    }else{
      nicheOv$simil <- ecospat::ecospat.niche.similarity.test(z1, z2, iter, alternative = "greater", rand.type = 1)
    }
  }
  
  return(nicheOv)
}
