<<<<<<< HEAD
#' @title calibrate a PCA for the background of two selected species
#' @description ..
#'
#' @details
#' See Examples.
#' @param sp.name1
#' @param sp.name2
#' @param pca pca output of pca component
#' @param shinyLogs
# @param spSel species selected (1 or 2)
# @keywords
#'
# @examples
#'
#'
# @return 
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
# @note

# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

=======
#' calibrate a PCA for the background of two selected species
#' 
#' @param pca output of pca component
#' @param spSel species selected (1 or 2)
#' @export
>>>>>>> 7de27dd1010e7833d00078c654d301e4126b6136

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
