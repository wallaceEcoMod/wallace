
#' @title makeBioclimPlot
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param x
#' @param a=1
#' @param b=2
#' @param p=0.9
#' @param ...
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

makeBioclimPlot <- function(x, a=1, b=2, p=0.9, ...) {
  
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
  prd <- dismo::predict(x, d)
  i <- prd > p & prd < (1-p)
  plot(d[,a], d[,b], xlab=colnames(d)[a], ylab=colnames(d)[b], cex=0)
  type=6
  x1 <- quantile(d[,a], probs=p, type=type)
  x2 <- quantile(d[,a], probs=1-p, type=type)
  y1 <- quantile(d[,b], probs=p, type=type)
  y2 <- quantile(d[,b], probs=1-p, type=type)
  #		x1 <- myquantile(x[,a], p)
  #		x2 <- myquantile(x[,a], 1-p)
  #		y1 <- myquantile(x[,b], p)
  #		y2 <- myquantile(x[,b], 1-p)
  polygon(rbind(c(x1,y1), c(x1,y2), c(x2,y2), c(x2,y1), c(x1,y1)), border='blue', lwd=2)
  points(d[i,a], d[i,b], xlab=colnames(x)[a], ylab=colnames(x)[b], col='green' )
  points(d[!i,a], d[!i,b], col='red', pch=3)
}