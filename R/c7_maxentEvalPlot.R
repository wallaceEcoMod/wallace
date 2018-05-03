# plot ENMeval stats based on user selection ("value")
makeMaxentEvalPlot <- function(evalTbl, value) {
  fc <- length(unique(evalTbl$features))
  col <- rainbow(fc)
  rm <- length(unique(evalTbl$rm))
  xlab <- "Regularization Multiplier"
  
  if (value != "delta.AICc") {
    variance <- gsub('avg', 'var', value)
  } else {
    variance <- NULL
  }
  
  y <- evalTbl[,value]
  
  if (value != "delta.AICc") {
    v <- evalTbl[,variance]
    # ylim <- c(min(y-v), max(y+v))
    ylim <- c(0, 1)
  } else {
    ylim <- c(min(y, na.rm=TRUE), max(y, na.rm=TRUE))
  }
  
  
  plot(evalTbl$rm, y, col='white', ylim=ylim, ylab=value, xlab=xlab, axes=F, cex.lab=1.5)
  if (value=="delta.AICc") abline(h=2, lty=3)
  axis(1, at= unique(evalTbl$rm))
  axis(2)
  box()
  for (j in 1:length(unique(evalTbl$features))){
    s <- ((fc*rm)-fc+j)
    points(evalTbl$rm[seq(j, s, fc)], y[seq(j, s, fc)], type="l", col=col[j])
    if (!is.null(variance)) {
      arrows(evalTbl$rm[seq(j, s, fc)],
             y[seq(j, s, fc)] + v[seq(j, s, fc)],
             evalTbl$rm[seq(j, s, fc)],
             y[seq(j, s, fc)] - v[seq(j, s, fc)],
             code=3, length=.05, angle=90, col=col[j])
    }
  }
  points(evalTbl$rm, y, bg=col, pch=21)
  legend("topright", legend=unique(evalTbl$features), pt.bg=col, pch=21, bg='white', cex=1, ncol=2)
}