
#' @title Occurence density grid
#' @description calculates the part of environmental space more densly populated by species & the avaialbility of environmental conditions in the background
#'
#' @details
#' This fuctions implements a density estimation for each region in the environmental space (gridded at 100*100 pixels).
#' Then an occurrence density is estimated using a kernel density approach. The density of environmental conditions in the background is calcuated in the same way.
#
#' @param sp.name1 character, name of species 1 to be analyzed
#' @param sp.name2 character, name of species 2 to be analyzed.
#' @param pca pca output of pca component ( in list format)
#' @param logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
# @keywords
#'
# @examples
#'
#'
#' @return
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Olivier Broennimann <olivier.broennimann@@unil.ch>
# @note
#' @seealso \code{\link{espace_pca}} \code{\link{espace_nicheOv}} \code{\link[ecospat]{grid.clim.dyn}} \code{\link[adeHabitatHR]{kernelUD}}
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
                             em(spName(sp.name1)), " and ", em(spName(sp.name2)), "."))

  return(occDens)
}
