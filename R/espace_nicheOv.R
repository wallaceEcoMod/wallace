#' @title espace_nicheOv
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param z1 ecospat niche object for species 1 from cESpace_OccDens
#' @param z2 ecospat niche object for species 2 from cESpace_OccDens
#' @param iter number of iterations
#' @param equivalency x
#' @param similarity x
#' @param logger x
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
# @family - a family name. All functions that have the same family tag will be
# linked in the documentation.
#' @export

espace_nicheOv <- function(z1, z2, iter = 100, equivalency = FALSE,
                            similarity = TRUE, logger = NULL) {
  nicheOv <- list()

  # Schoener's D
  nicheOv$overlap <- ecospat::ecospat.niche.overlap(z1, z2, cor = TRUE)


  #unfilling, stability, expansion indices (Guisan et al. 2014 TREE)
  nicheOv$USE <- ecospat::ecospat.niche.dyn.index(
    z1, z2, intersection = 0)$dynamic.index.w

  # niche tests
  if (equivalency == TRUE) {
    smartProgress(logger, message = "Calculating niche equivalency...", {
      nicheOv$equiv <- ecospat::ecospat.niche.equivalency.test(
        z1, z2, iter, alternative = "greater"
      )
    })
  }

  if (similarity == TRUE) {
    smartProgress(logger, message = "Calculating niche similarity", {
      nicheOv$simil <- ecospat::ecospat.niche.similarity.test(
        z1, z2, iter, alternative = "greater", rand.type = 1
      )
    })
  }

  return(nicheOv)
}
