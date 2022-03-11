
#' @title vis_bioclimPlot Visualize bivariate plot of bioclim model
#' @description
#' This functions creates a bivariate plot with two of the environmental variables used for modeling as x and y axes and occurrences as observations.
#'
#' @details
#' This is a bivariate plot with x and y axes representing two of the environmental layers used for modeling (user selected although 1 and 2 as default)/
#' Occurrences used for modeling are shown with differential visualization if they are outside of the selected percentile distribution (for any variable).
#' Plot also includes a rectangle representing the bivariate bioclimatic envelope according to a provided percentile.
#'
#' @param x bioclim model including values for each environmental layer at each occurrence point
#' @param a numeri.c Environmental layer to be used as x axis. Default is layer 1.
#' @param b numeric. Environmental layer to be used as x axis. Default is layer 2.
#' @param p numeric. (0-1) percentile distribution to be used for plotting envelope and showing points outside of envelope. Default is 0.9

# @keywords
#'
#' @examples
#' out.gbif <- occs_queryDb(spName = "Panthera onca", occDb = "gbif",
#'                          occNum = 100)
#' occs <- as.data.frame(out.gbif[[1]],
#'                       bcSel = c("bio01", "bio02", "bio13", "bio14"),
#'                       doBrick = FALSE)
#' bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)
#' bgMask <- penvs_bgMask(occs, envs, bgExt)
#' bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 10000)
#' partblock <- part_partitionOccs(occs, bg, method = 'block',
#'                                 kfolds = NULL, bgMask = NULL,
#'                                 aggFact = NULL)
#' occs$partition <- partblock$occ.grp
#' bg$partition <- partblock$bg.grp
#' bioclimAlg <- model_bioclim(occs, bg, partblock$occ.grp,
#'                             partblock$bg.grp, bgMask)
#' bioclimPlot <- vis_bioclimPlot(x = bioclimAlg@@models$bioclim,
#'                                a = 1, b = 2, p = 1)
#'
#' @return A bivariate plot of environmental values for occurrences. Includes a blue rectangle representing the bioclimatic enveloppe given p.
#' Occurrences that are inside the envelope for all layers (included those not plotted) are shown as green circles and those outside of the envelope for one ore more variables are plotted as orange triangles/
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
# @note
#' @seealso
#'\code{\link{model_bioclim}} \code{\link[ENMeval]{ENMevaluate}}
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

vis_bioclimPlot <- function(x, a = 1, b = 2, p = 0.9) {

  d <- x@presence

  myquantile <- function(x, p) {
    p <- min(1, max(0, p))
    x <- sort(as.vector(stats::na.omit(x)))
    if (p == 0) return(x[1])
    if (p == 1) return(x[length(x)])
    i = (length(x)-1) * p + 1
    ti <- trunc(i)
    below = x[ti]
    above = x[ti+1]
    below + (above-below)*(i-ti)
  }

  p <- min(1,  max(0, p))
  if (p > 0.5) p <- 1 - p
  p <- p / 2
  prd <- dismo::predict(x, d, useC = FALSE)
  i <- prd > p & prd < (1-p)
  plot(d[,a], d[,b], xlab=colnames(d)[a], ylab=colnames(d)[b], cex=0)
  type=6
  x1 <- stats::quantile(d[,a], probs=p, type=type)
  x2 <- stats::quantile(d[,a], probs=1-p, type=type)
  y1 <- stats::quantile(d[,b], probs=p, type=type)
  y2 <- stats::quantile(d[,b], probs=1-p, type=type)
  graphics::polygon(rbind(c(x1,y1), c(x1,y2), c(x2,y2), c(x2,y1), c(x1,y1)), border='#0072B2', lwd=2)
  graphics::points(d[i,a], d[i,b], xlab=colnames(x)[a], ylab=colnames(x)[b], col='#009E73' ,pch=16)
  graphics::points(d[!i,a], d[!i,b], col="#D55E00", pch=17)
}
