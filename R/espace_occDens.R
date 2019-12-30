
#' @title calibrate a PCA for the background of two selected species
#' @description ..
#'
#' @details
#' See Examples.
#' @param sp.name1 x
#' @param sp.name2 x
#' @param pca pca output of pca component
#' @param logger x
# @param spSel species selected (1 or 2)
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Olivier Broennimann, Jamie Kass <jkass@@gradcenter.cuny.edu>
# @note

# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

espace_occDens <- function(sp.name1, sp.name2, pca, logger = NULL) {
  bg <- pca$scores$bg
  sp <- pca$scores$sp
  scores.bg12 <- pca$scores[bg != 'bg', 1:2]
  scores.bg1 <- pca$scores[bg == sp.name1, 1:2]
  scores.occs1 <- pca$scores[sp == sp.name1, 1:2]
  scores.bg2 <- pca$scores[bg == sp.name2, 1:2]
  scores.occs2 <- pca$scores[sp == sp.name2, 1:2]
  smartProgress(logger, message = "Running occurrence density grids...", {
    occDens1 <- ecospat::ecospat.grid.clim.dyn(scores.bg12, scores.bg1,
                                               scores.occs1, 100)
    incProgress(1/2)
    occDens2 <- ecospat::ecospat.grid.clim.dyn(scores.bg12, scores.bg2,
                                               scores.occs2, 100)
    incProgress(1/2)
  })
  occDens <- list()
  occDens[[sp.name1]] <- occDens1
  occDens[[sp.name2]] <- occDens2

  logger %>% writeLog(paste0("Occurrence density grid run for ",
                             spName(sp.name1), " and ", spName(sp.name2), "."))

  return(occDens)
}
