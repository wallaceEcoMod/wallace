
#' @title model_bioclim
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param occs x
#' @param bg x
#' @param bgMask x
#' @param logger x
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Jamie M. Kass <jkass@@gradcenter.cuny.edu>
# @note

# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.

#' @export

model_bioclim <- function(occs, bg, occsGrp, bgGrp, bgMsk, logger = NULL) {

  # get just coordinates
  occs.xy <- occs %>% dplyr::select(longitude, latitude)
  bg.xy <- bg %>% dplyr::select(longitude, latitude)

  e <- ENMeval::ENMevaluate(occs = occs.xy, envs = bgMsk, bg = bg.xy,
                            mod.name = "bioclim", partition = "user",
                            occ.grp = occsGrp, bg.grp = bgGrp)

  logger %>% writeLog("BIOCLIM ran successfully for ", em(spName(occs)),
                      " and output evaluation results.")

  return(e)
}
