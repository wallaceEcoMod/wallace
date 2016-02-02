## -------------------------------------------------------------------- ##
## Define functions
## -------------------------------------------------------------------- ##

# clear current mapped features and plot occurrence locations
map_plotLocs <- function(locs, clearMarkers=TRUE, clearShapes=TRUE, clearImages=TRUE, fillColor='red', fillOpacity=0.2) {
  if (clearMarkers) proxy %>% clearMarkers()
  if (clearShapes) proxy %>% clearShapes()
  if (clearImages) proxy %>% clearImages()
  proxy %>% addCircleMarkers(data = locs, lat = ~latitude, lng = ~longitude,
                             radius = 5, color = 'red', fill = TRUE, 
                             fillColor = fillColor, fillOpacity = fillOpacity, 
                             weight = 2, popup = ~pop)
}

# zooms appropriately for any extent
smartZoom <- function(longi, lati) {
  lg.diff <- abs(max(longi) - min(longi))
  lt.diff <- abs(max(lati) - min(lati))
  if (lg.diff > 1) lg.diff <- 1
  if (lt.diff > 1) lt.diff <- 1
  c(min(longi-lg.diff), min(lati-lt.diff), max(longi+lg.diff), max(lati+lt.diff))
}

# for naming files
nameAbbr <- function(spname) {
  namespl <- strsplit(tolower(spname[1,1]), " ")
  genusAbbr <- substring(namespl[[1]][1], 1, 1)
  fullNameAbbr <- paste0(genusAbbr, "_", namespl[[1]][2])
  return(fullNameAbbr)
}

# make a minimum convex polygon as SpatialPolygons object
mcp <- function (xy) {
  xy <- as.data.frame(coordinates(xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  return(SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.bord))), 1))))
}

remDups <- function(df) {
  dups <- duplicated(df)
  df <- df[!dups,]
}

makeOccIcons <- function(width = 10, height = 10, ...) {
  occIcons <- c('H', 'O', 'P', 'U', 'F', 'M', 'I', 'L', 'A', 'X')
  files <- character(9)
  # create a sequence of png images
  for (i in 1:9) {
    f <- tempfile(fileext = '.png')
    png(f, width = width, height = height, bg = 'transparent')
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = occIcons[i], cex = min(width, height) / 8, col='red', ...)
    dev.off()
    files[i] <- f
  }
  files
}

popUpContent <- function(x) {
  as.character(tagList(
    tags$strong(paste("ID:", x['origID'])),
    tags$br(),
    tags$strong(paste("Latitude:", x['latitude'])),
    tags$strong(paste("Longitude:", x['longitude']))
  ))
}

BioClim_eval <- function (occs, bg.pts, occ.grp, bg.grp, env) {

  # RUN FULL DATA MODEL
  full.mod <- bioclim(env, occs)
  pred <- predict(env, full.mod)

  # CREATE HOLDERS FOR RESULTS
  AUC.TEST <- double()
  AUC.DIFF <- double()
  OR10 <- double()
  ORmin <- double()

  # SET NUMBER OF TEST BINS
  nk <- length(unique(occ.grp))

  for (k in 1:nk) {

    # SPLIT TEST AND TRAIN DATA
    test.pts <- occs[occ.grp == k, ]
    train.pts <- occs[occ.grp != k, ]
    backg.pts <- bg.pts[bg.grp != k, ]
    mod <- bioclim(env, train.pts)

    # GET AUC METRICS
    AUC.TEST[k] <- evaluate(p=test.pts, a=backg.pts, mod=mod, x=env)@auc
    AUC.TRAIN <- evaluate(p=train.pts, a=backg.pts, mod=mod, x=env)@auc
    AUC.DIFF[k] <- max(0, AUC.TRAIN - AUC.TEST[k])

    # GET PREDICTED VALUES AT OCCURRENCES FOR OMISSION RATE STATS
    train.pred <- predict(env, mod)
    p.train <- extract(train.pred, train.pts)
    p.test <- extract(train.pred, test.pts)

    # FIND THRESHOLD FOR OR10
    if (nrow(train.pts) < 10) {
      n90 <- floor(nrow(train.pts) * 0.9)
    } else {
      n90 <- ceiling(nrow(train.pts) * 0.9)
    }

    # GET OMISSION RATE STATS
    train.thr.10 <- rev(sort(p.train))[n90]
    OR10[k] <- mean(p.test < train.thr.10)
    ORmin[k] <- mean(p.test < min(p.train))
  }

  # COMPILE AND SUMMARIZE RESULTS
  stats <- as.data.frame(rbind(AUC.DIFF, AUC.TEST, OR10, ORmin))
  stats <- cbind(apply(stats, 1, mean), corrected.var(stats, nk), stats)
  colnames(stats) <- c("Mean", "Variance", paste("Bin", 1:nk))
  rownames(stats) <- c("AUC.DIFF", "AUC.TEST","OR10","ORmin")

  # THIS FORMAT FOR RETURNED DATA ATTEMPTS TO MATCH WHAT HAPPENS IN WALLACE ALREADY FOR ENMEVAL.
  return(list(models=full.mod, results=stats, predictions=stack(pred)))
}

# plot ENMeval stats based on user selection ("value")
evalPlot <- function(res, value) {
  fc <- length(unique(res$features))
  col <- rainbow(fc)
  rm <- length(unique(res$rm))
  xlab <- "Regularization Multiplier"
  
  if (value != "delta.AICc") {
    variance <- paste0('Var', strsplit(value, split='Mean')[[1]][2])
  }
  
  y <- res[,value]

  if (value != "delta.AICc") {
    v <- res[,variance]
    ylim <- c(min(y-v), max(y+v))  
  } else {
    ylim <- c(min(y, na.rm=TRUE), max(y, na.rm=TRUE))
  }
  
  
  plot(res$rm, y, col='white', ylim=ylim, ylab=value, xlab=xlab, axes=F, cex.lab=1.5)
  if (value=="delta.AICc") abline(h=2, lty=3)
  axis(1, at= unique(res$rm))
  axis(2)
  box()
  for (j in 1:length(unique(res$features))){
    s <- ((fc*rm)-fc+j)
    points(res$rm[seq(j, s, fc)], y[seq(j, s, fc)], type="l", col=col[j])
    if(!is.null(variance)){
      arrows(res$rm[seq(j, s, fc)], 
             y[seq(j, s, fc)] + v[seq(j, s, fc)], 
             res$rm[seq(j, s, fc)], 
             y[seq(j, s, fc)] - v[seq(j, s, fc)],
             code=3, length=.05, angle=90, col=col[j])
    }
  }
  points(res$rm, y, bg=col, pch=21)
  legend("topright", legend=unique(res$features), pt.bg=col, pch=21, bg='white', cex=1, ncol=2)
}

evalPlots <- function(results) {
  par(mfrow=c(3,2))
  fc <- length(unique(results$features))
  col <- rainbow(fc)
  rm <- length(unique(results$rm))
  plot(rep(1, times=fc), 1:fc, ylim=c(.5,fc+1), xlim=c(0,3), axes=F, ylab='', xlab='', cex=2, pch=21, bg=col)
  segments(rep(.8, times=fc), 1:fc, rep(1.2, times=fc), 1:fc, lwd=1, col=col)
  points(rep(1, times=fc), 1:fc, ylim=c(-1,fc+2), cex=2, pch=21, bg=col)
  text(x=rep(1.3, times=fc), y=1:fc, labels=unique(results$features), adj=0)
  text(x=1, y=fc+1, labels="Feature Classes", adj=.20, cex=1.3, font=2)
  eval.plot(results, legend=FALSE, value="delta.AICc")
  eval.plot(results, legend=FALSE, value="Mean.AUC", variance="Var.AUC")
  eval.plot(results, legend=FALSE, value="Mean.AUC.DIFF", variance="Var.AUC.DIFF")
  eval.plot(results, legend=FALSE, value="Mean.ORmin", variance="Var.ORmin")
  eval.plot(results, legend=FALSE, value="Mean.OR10", variance="Var.OR10")
}

# Bind csv and occ records
addCSVpts <- function(df, inFile.occs) {
  df <- rbind(df, inFile.occs)
  df <- remDups(df)
}


# Fix columns
fixcols <- function(cols, results) {
  colsadd <- cols[!(cols %in% colnames(results$data))]  # find colNames not included in results$data
  n <- length(colsadd)  # number of colNames not included

  # if there are colNames not included, add a new named col filled with NAs
  if (n > 0) {
    for (i in 1:n) {
      results$data <- cbind(results$data, NA)
      colnames(results$data)[ncol(results$data)] <- colsadd[i]
    }
  }
  return(results)
}


# Normalize function for raw predictions
normalize <- function(x) {
  valores <- values(x)
  pos <- which(!is.na(valores))
  valores[pos] <- (valores[pos] - min(valores[pos])) / (max(valores[pos]) - min(valores[pos]))
  values(x) <- valores
  return(x)
}

# make data.frame of lambdas vector from Maxent model object
lambdasDF <- function(mx) {
  lambdas <- mx@lambdas[1:(length(mx@lambdas)-4)]
  data.frame(var=sapply(lambdas, FUN=function(x) strsplit(x, ',')[[1]][1]),
             coef=sapply(lambdas, FUN=function(x) as.numeric(strsplit(x, ',')[[1]][2])),
             min=sapply(lambdas, FUN=function(x) as.numeric(strsplit(x, ',')[[1]][3])),
             max=sapply(lambdas, FUN=function(x) as.numeric(strsplit(x, ',')[[1]][4])),
             row.names=1:length(lambdas))
}
## pulls out all non-zero, non-redundant (removes hinge/product/threshold) predictor names
mxNonzeroPreds <- function(mx) {
  x <- lambdasDF(mx)
  #remove any rows that have a zero lambdas value (Second column)
  x <- x[(x[,2] != 0),]
  #remove any rows that have duplicate "var"s (hinges, quadratics)
  x <- unique(sub("\\^\\S*", "", x[,1]))
  x <- unique(sub("\\`", "", x))
  x <- unique(sub("\\'", "", x))
  x <- unique(sub("\\=\\S*", "", x))
  x <- unique(sub("\\(", "", x))
}

respCurv <- function(mod, i) {  # copied mostly from dismo
  v <- rbind(mod@presence, mod@absence)
  v.nr <- nrow(v)
  vi <- v[, i]
  vi.r <- range(vi)
  expand <- 10
  xlm <- 25
  vi.rx <- seq(vi.r[1]-expand, vi.r[2]+expand, length.out=xlm)
  mm <- v[rep(1:v.nr, xlm), ]
  mm[, i] <- rep(vi.rx, v.nr)
  mm[, -i] <- rep(colMeans(mm[,-i]), each=nrow(mm))
  p <- predict(mod, mm)
  plot(cbind(vi.rx, p[1:xlm]), type='l', ylim=0:1, col = 'red', lwd = 2,
       ylab = 'predicted value', xlab = names(v)[i])
  pres.r <- range(mod@presence[, i])
  abline(v = pres.r[1], col='blue')  # vertical blue lines indicate min and max of presence vals
  abline(v = pres.r[2], col='blue')
  abs.r <- range(mod@absence[, i])
  abline(v = abs.r[1], col='green') # vertical green lines indicate min and max of background vals
  abline(v = abs.r[2], col='green')
    #graphics::text(x = vals, y = pred, labels = row.names(mod@presence), pos = 3, offset = 1)
}
