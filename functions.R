## -------------------------------------------------------------------- ##
## Define functions
## -------------------------------------------------------------------- ##

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
    tags$strong(paste("Latitude:", x['lat'])),
    tags$strong(paste("Longitude:", x['lon']))
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
addCSVpts <- function(df) {
  df <- rbind(df, inFile.occs)
  df <- remDups(df)
}


# Fix columns
fixcols <- function(cols, results) {
  colsadd <- cols[!(cols %in% colnames(results$data))]
  n <- length(colsadd)

  if (n > 0) {
    for(i in 1:n) {
      results$data <- cbind(results$data, NA)
      colnames(results$data)[ncol(results$data)] <- colsadd[i]
    }
  }
  return(results)
}
