## -------------------------------------------------------------------- ##
## Define functions
## -------------------------------------------------------------------- ##

GCMlookup <- c(AC="ACCESS1-0", BC="BCC-CSM1-1", CC="CCSM4", CE="CESM1-CAM5-1-FV2",
              CN="CNRM-CM5", GF="GFDL-CM3", GD="GFDL-ESM2G", GS="GISS-E2-R",
              HD="HadGEM2-AO", HG="HadGEM2-CC", HE="HadGEM2-ES", IN="INMCM4",
              IP="IPSL-CM5A-LR", ME="MPI-ESM-P", MI="MIROC-ESM-CHEM", MR="MIROC-ESM",
              MC="MIROC5", MP="MPI-ESM-LR", MG="MRI-CGCM3", NO="NorESM1-M")

rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")

reverseLabels <- function(..., reverse_order = FALSE) {
  if (reverse_order) {
    function(type = "numeric", cuts) {
      cuts <- sort(cuts, decreasing = TRUE)
    }
  } else {
    labelFormat(...)
  }
}

## custom label format function
myLabelFormat = function(..., reverse_order = FALSE){
  if(reverse_order){
    function(type = "numeric", cuts){
      cuts <- sort(cuts, decreasing = T)
    }
  }else{
    labelFormat(...)
  }
}

# return the map center given the bounds
mapCenter <- function(bounds) {
  map_center <- c((bounds$west + bounds$east) / 2, (bounds$north + bounds$south) / 2)
  map_center <- round(map_center, digits=3)
  return(map_center)
}

# mapping controls
map_plotLocs <- function(map, locs, fillColor='red', fillOpacity=0.2) {
  if (is.null(locs)) return(map)
  map %>% addCircleMarkers(data = locs, lat = ~latitude, lng = ~longitude,
                           radius = 5, color = 'red', fill = TRUE,
                           fillColor = fillColor, fillOpacity = fillOpacity,
                           weight = 2, popup = ~pop)
}

# zoom to occ pts
zoom2Occs <- function(map, occs) {
  # map %>% clearShapes()
  lati <- occs[,3]
  longi <- occs[,2]
  z <- smartZoom(longi, lati)
  map %>% fitBounds(z[1], z[2], z[3], z[4])

  # this section makes letter icons for occs based on basisOfRecord
  #     occIcons <- makeOccIcons()
  #     iconList <- list(HUMAN_OBSERVATION=1, OBSERVATION=2, PRESERVED_SPECIMEN=3,
  #                      UNKNOWN_EVIDENCE=4, FOSSIL_SPECIMEN=5, MACHINE_OBSERVATION=6,
  #                      LIVING_SPECIMEN=7, LITERATURE_OCCURRENCE=8, MATERIAL_SAMPLE=9)
  #     values$origOccs$basisNum <- unlist(iconList[values$origOccs$basisOfRecord])
  #     proxy %>% addMarkers(data = values$origOccs, lat = ~latitude, lng = ~longitude,
  #                          layerId = as.numeric(rownames(values$origOccs)),
  #                          icon = ~icons(occIcons[basisNum]))
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

formatSpName <- function(spName) paste(strsplit(spName, split=' ')[[1]], collapse='_')

# make a minimum convex polygon as SpatialPolygons object
mcp <- function (xy) {
  xy <- as.data.frame(sp::coordinates(xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  return(sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(as.matrix(xy.bord))), 1))))
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
  full.mod <- dismo::bioclim(env, occs)
  pred <- dismo::predict(env, full.mod)

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
    mod <- dismo::bioclim(env, train.pts)

    # GET AUC METRICS
    AUC.TEST[k] <- dismo::evaluate(p=test.pts, a=backg.pts, mod=mod, x=env)@auc
    AUC.TRAIN <- dismo::evaluate(p=train.pts, a=backg.pts, mod=mod, x=env)@auc
    AUC.DIFF[k] <- max(0, AUC.TRAIN - AUC.TEST[k])

    # GET PREDICTED VALUES AT OCCURRENCES FOR OMISSION RATE STATS
    train.pred <- dismo::predict(env, mod)
    p.train <- raster::extract(train.pred, train.pts)
    p.test <- raster::extract(train.pred, test.pts)

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
  stats <- cbind(apply(stats, 1, mean), ENMeval::corrected.var(stats, nk), stats)
  colnames(stats) <- c("Mean", "Variance", paste("Bin", 1:nk))
  rownames(stats) <- c("AUC.DIFF", "AUC.TEST","OR10","ORmin")

  # THIS FORMAT FOR RETURNED DATA ATTEMPTS TO MATCH WHAT HAPPENS IN WALLACE ALREADY FOR ENMEVAL.
  return(list(models=list(full.mod), results=stats, predictions=raster::stack(pred)))
}

# plot ENMeval stats based on user selection ("value")
evalPlot <- function(res, value) {
  fc <- length(unique(res$features))
  col <- rainbow(fc)
  rm <- length(unique(res$rm))
  xlab <- "Regularization Multiplier"

  if (value != "delta.AICc") {
    variance <- paste0('Var', strsplit(value, split='Mean')[[1]][2])
  } else {
    variance <- NULL
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
    if (!is.null(variance)) {
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
  ENMeval::eval.plot(results, legend=FALSE, value="delta.AICc")
  ENMeval::eval.plot(results, legend=FALSE, value="Mean.AUC", variance="Var.AUC")
  ENMeval::eval.plot(results, legend=FALSE, value="Mean.AUC.DIFF", variance="Var.AUC.DIFF")
  ENMeval::eval.plot(results, legend=FALSE, value="Mean.ORmin", variance="Var.ORmin")
  ENMeval::eval.plot(results, legend=FALSE, value="Mean.OR10", variance="Var.OR10")
}

# borrowed from the plot method for bioclim in dismo v.1.1-1
bc.plot <- function(x, a=1, b=2, p=0.9, ...) {

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

# Reset values
resetV <- function(x) {

  namesV <- names(x)[-(1:2)]
  for(i in namesV){
    x[[i]] <- NULL
  }
  brk <- paste(rep('------', 14), collapse='')
  x$polyID <- 0
  x$polyErase <- FALSE
  x$log <- c(paste('***WELCOME TO WALLACE***', brk,
                'Please find messages for the user in this log window.', brk, sep='<br>'))
}
