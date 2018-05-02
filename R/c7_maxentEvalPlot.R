# plot ENMeval stats based on user selection ("value")
makeMaxentEvalPlot <- function(results, value) {
  print('func run')
  fc <- length(unique(results$features))
  col <- rainbow(fc)
  rm <- length(unique(results$rm))
  xlab <- "Regularization Multiplier"
  
  if (value != "delta.AICc") {
    variance <- gsub('avg', 'var', value)
  } else {
    variance <- NULL
  }
  
  y <- results[,value]
  
  if (value != "delta.AICc") {
    v <- results[,variance]
    # ylim <- c(min(y-v), max(y+v))
    ylim <- c(0, 1)
  } else {
    ylim <- c(min(y, na.rm=TRUE), max(y, na.rm=TRUE))
  }
  
  
  plot(results$rm, y, col='white', ylim=ylim, ylab=value, xlab=xlab, axes=F, cex.lab=1.5)
  if (value=="delta.AICc") abline(h=2, lty=3)
  axis(1, at= unique(results$rm))
  axis(2)
  box()
  for (j in 1:length(unique(results$features))){
    s <- ((fc*rm)-fc+j)
    points(results$rm[seq(j, s, fc)], y[seq(j, s, fc)], type="l", col=col[j])
    if (!is.null(variance)) {
      arrows(results$rm[seq(j, s, fc)],
             y[seq(j, s, fc)] + v[seq(j, s, fc)],
             results$rm[seq(j, s, fc)],
             y[seq(j, s, fc)] - v[seq(j, s, fc)],
             code=3, length=.05, angle=90, col=col[j])
    }
  }
  points(results$rm, y, bg=col, pch=21)
  legend("topright", legend=unique(results$features), pt.bg=col, pch=21, bg='white', cex=1, ncol=2)
}