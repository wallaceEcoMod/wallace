## -------------------------------------------------------------------- ##
## Define functions
## -------------------------------------------------------------------- ##

####################### #
# UI ####
####################### #

uiTop <- function(modPkg, pkgDes) {
  list(span(modPkg, id="rpkg"),
       span(paste(':', pkgDes), id="pkgDes"),
       br())
}

uiBottom <- function(modAuthors=NULL, pkgName=NULL, pkgAuthors) {
  if (is.null(modAuthors)) {
    list(span(pkgName, id = "rpkg"), "references", br(),
         div(paste('Package Developers:', pkgAuthors), id="pkgDes"),
         a("CRAN", href = file.path("http://cran.r-project.org/web/packages", pkgName, "index.html"), target = "_blank"),
         " | ",
         a("documentation", href = file.path("https://cran.r-project.org/web/packages", pkgName, paste0(pkgName, ".pdf")), target = "_blank"))
  } else {
    if (is.null(pkgName)) {
      list(div(paste('Module Developers:', modAuthors), id="pkgDes"))
    } else {
      list(div(paste('Module Developers:', modAuthors), id="pkgDes"),
           span(pkgName, id = "rpkg"), "references", br(),
           div(paste('Package Developers:', pkgAuthors), id="pkgDes"),
           a("CRAN", href = file.path("http://cran.r-project.org/web/packages", pkgName, "index.html"), target = "_blank"),
           " | ",
           a("documentation", href = file.path("https://cran.r-project.org/web/packages", pkgName, paste0(pkgName, ".pdf")), target = "_blank")
      )
    }
  }
}

####################### #
# LOG WINDOW ####
####################### #

# initialize log window text
logInit <- function() {
  intro <- '***WELCOME TO WALLACE***'
  brk <- paste(rep('------', 14), collapse='')
  expl <- 'Please find messages for the user in this log window.'
  return(c(paste(intro, brk, expl, brk, sep='<br>')))
}

# add text to log
writeLog <- function(mp, ..., type = 'default') {
  if (type == "default") {
    pre <- "> "
  } else if (type == 'error') {
    pre <- '<font color="red"><b>! ERROR</b></font> : '
  } else if (type == 'warning') {
    pre <- '<font color="orange"><b>! WARNING</b></font> : '
  }
  
  args <- list(pre, ...)
  newEntries <- paste(args, collapse = ' ')
  isolate({mp$logs <- paste(mp$logs, newEntries, sep = '<br>')})
}

####################### #
# MISC ####
####################### #

# for naming files
nameAbbr <- function(spname) {
  namespl <- strsplit(tolower(spname), " ")
  genusAbbr <- substring(namespl[[1]][1], 1, 1)
  fullNameAbbr <- paste0(genusAbbr, "_", namespl[[1]][2])
  return(fullNameAbbr)
}

formatSpName <- function(spName) paste(strsplit(spName, split=' ')[[1]], collapse='_')

fileNameNoExt <- function(f) {
  sub(pattern = "(.*)\\..*$", replacement = "\\1", f)
}

####################### #
# MAPPING ####
####################### #

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
  
  
  ## this section makes letter icons for occs based on basisOfRecord
  # makeOccIcons <- function(width = 10, height = 10, ...) {
  #   occIcons <- c('H', 'O', 'P', 'U', 'F', 'M', 'I', 'L', 'A', 'X')
  #   files <- character(9)
  #   # create a sequence of png images
  #   for (i in 1:9) {
  #     f <- tempfile(fileext = '.png')
  #     png(f, width = width, height = height, bg = 'transparent')
  #     par(mar = c(0, 0, 0, 0))
  #     plot.new()
  #     points(.5, .5, pch = occIcons[i], cex = min(width, height) / 8, col='red', ...)
  #     dev.off()
  #     files[i] <- f
  #   }
  #   files
  # }
  # occIcons <- makeOccIcons()
  # iconList <- list(HUMAN_OBSERVATION=1, OBSERVATION=2, PRESERVED_SPECIMEN=3,
  #                  UNKNOWN_EVIDENCE=4, FOSSIL_SPECIMEN=5, MACHINE_OBSERVATION=6,
  #                  LIVING_SPECIMEN=7, LITERATURE_OCCURRENCE=8, MATERIAL_SAMPLE=9)
  # values$origOccs$basisNum <- unlist(iconList[values$origOccs$basisOfRecord])
  # proxy %>% addMarkers(data = values$origOccs, lat = ~latitude, lng = ~longitude,
  #                      layerId = as.numeric(rownames(values$origOccs)),
  #                      icon = ~icons(occIcons[basisNum]))
}

# zooms appropriately for any extent
smartZoom <- function(longi, lati) {
  lg.diff <- abs(max(longi) - min(longi))
  lt.diff <- abs(max(lati) - min(lati))
  if (lg.diff > 1) lg.diff <- 1
  if (lt.diff > 1) lt.diff <- 1
  c(min(longi-lg.diff), min(lati-lt.diff), max(longi+lg.diff), max(lati+lt.diff))
}

popUpContent <- function(x) {
  lat <- round(as.numeric(x['latitude']), digits = 2)
  lon <- round(as.numeric(x['longitude']), digits = 2)
  as.character(tagList(
    tags$strong(paste("occID:", x['occID'])),
    tags$br(),
    tags$strong(paste("Latitude:", lat)),
    tags$strong(paste("Longitude:", lon))
  ))
}

####################### #
# COMP 3 ####
####################### #

remEnvsValsNA <- function(rvs) {
  withProgress(message = "Checking for points with NA values...", {
    occsVals <- raster::extract(rvs$envs, rvs$occs[c('longitude', 'latitude')])
    na.rowNums <- which(rowSums(is.na(occsVals)) > 1)
    
    if (length(na.rowNums) == length(occsVals)) {
      rvs %>% writeLog(type = 'error', 'No localities overlay with environmental predictors. 
                        All localities may be marine -- please redo with terrestrial occurrences.')
      return()
    }
    
    if (length(na.rowNums) > 0) {
      occs.notNA <- rvs$occs[-na.rowNums,]
      rvs %>% writeLog(type = 'warning', 'Removed records without environmental values with occIDs: "',
                        paste(rvs$occs[na.rowNums,]$occID, collapse=', '), ".")
      return(occs.notNA)
    }
    
    return(rvs$occs)
  })
}

####################### #
# COMP 4 ####
####################### #

# make a minimum convex polygon as SpatialPolygons object
mcp <- function(xy) {
  xy <- as.data.frame(sp::coordinates(xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  return(sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(as.matrix(xy.bord))), 1))))
}

####################### #
# COMP 5 ####
####################### #

comp5_map <- function(map, occs, occsGrp) {
  if (is.null(occsGrp)) return()
  # colors for partition symbology
  newColors <- gsub("FF$", "", rainbow(max(occsGrp)))  
  partsFill <- newColors[occsGrp]
  map %>%
    clearMarkers() %>%
    map_plotLocs(occs, fillColor = partsFill, fillOpacity = 1) %>%
    addLegend("bottomright", colors = newColors,
              title = "Partition Groups", labels = sort(unique(occsGrp)),
              opacity = 1, layerId = 'leg')
}

####################### #
# COMP 6 ####
####################### #

BioClim_eval <- function (occs, bg.pts, occ.grp, bg.grp, env) {
  
  # RUN FULL DATA MODEL
  full.mod <- dismo::bioclim(env, occs)
  pred <- dismo::predict(env, full.mod)
  occPredVals <- matrix(dismo::predict(full.mod, full.mod@presence))
  colnames(occPredVals) <- 'BIOCLIM'
  
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
  stats <- as.data.frame(rbind(AUC.TEST, AUC.DIFF, ORmin, OR10))
  stats <- cbind(apply(stats, 1, mean), ENMeval::corrected.var(stats, nk), stats)
  colnames(stats) <- c("Mean", "Variance", paste("Bin", 1:nk))
  rownames(stats) <- c("test.AUC", "diff.AUC","test.orMTP","test.or10pct")
  
  preds <- raster::stack(pred)
  names(preds) <- "BIOCLIM"
  
  # THIS FORMAT FOR RETURNED DATA ATTEMPTS TO MATCH WHAT HAPPENS IN WALLACE ALREADY FOR ENMEVAL.
  return(list(models=list(BIOCLIM=full.mod), results=stats, 
              predictions=preds, occVals=occPredVals))
}

thresh <- function(modOccVals, type) {
  # remove all NA
  modOccVals <- na.omit(modOccVals)
  if (type == 'mtp') {
    # apply minimum training presence threshold
    x <- min(modOccVals)
  } else if (type == 'p10') {
    # Define 10% training presence threshold
    if (length(modOccVals) < 10) {  # if less than 10 occ values, find 90% of total and round down
      n90 <- floor(length(modOccVals) * 0.9)
    } else {  # if greater than or equal to 10 occ values, round up
      n90 <- ceiling(length(modOccVals) * 0.9)
    }
    x <- rev(sort(modOccVals))[n90]  # apply 10% training presence threshold over all models
  }
  return(x)
}

####################### #
# COMP 7 ####
####################### #

# plot ENMeval stats based on user selection ("value")
evalPlot <- function(res, value) {
  fc <- length(unique(res$features))
  col <- rainbow(fc)
  rm <- length(unique(res$rm))
  xlab <- "Regularization Multiplier"
  
  if (value != "delta.AICc") {
    variance <- gsub('avg', 'var', value)
  } else {
    variance <- NULL
  }
  
  y <- res[,value]
  
  if (value != "delta.AICc") {
    v <- res[,variance]
    # ylim <- c(min(y-v), max(y+v))
    ylim <- c(0, 1)
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
mxNonzeroCoefs <- function(mx) {
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

getVals <- function(r, type='raw') {
  v <- raster::values(r)
  # remove NAs
  v <- v[!is.na(v)]
  if (type == 'logistic' | type == 'cloglog') v <- c(v, 0, 1)  # set to 0-1 scale
  return(v)
}

####################### #
# COMP 8 ####
####################### #

GCMlookup <- c(AC="ACCESS1-0", BC="BCC-CSM1-1", CC="CCSM4", CE="CESM1-CAM5-1-FV2",
               CN="CNRM-CM5", GF="GFDL-CM3", GD="GFDL-ESM2G", GS="GISS-E2-R",
               HD="HadGEM2-AO", HG="HadGEM2-CC", HE="HadGEM2-ES", IN="INMCM4",
               IP="IPSL-CM5A-LR", ME="MPI-ESM-P", MI="MIROC-ESM-CHEM", MR="MIROC-ESM",
               MC="MIROC5", MP="MPI-ESM-LR", MG="MRI-CGCM3", NO="NorESM1-M")

reverseLabels <- function(..., reverse_order = FALSE) {
  if (reverse_order) {
    function(type = "numeric", cuts) {
      cuts <- sort(cuts, decreasing = TRUE)
    }
  } else {
    labelFormat(...)
  }
}

comp8_map <- function(map, ras, polyXY, bgShpXY, rasVals, rasCols, 
                      legTitle, clearID = NULL) {
  
  # if thresholded, plot with two colors
  if (grepl('thresh', names(ras))) {
    rasPal <- c('gray', 'blue')
    map %>% addLegend("bottomright", colors = c('gray', 'blue'),
                      title = "Thresholded Suitability", labels = c("predicted absence", "predicted presence"),
                      opacity = 1, layerId = 'leg')
  } else {
    rasPal <- colorNumeric(rasCols, rasVals, na.color='transparent')
    legPal <- colorNumeric(rev(rasCols), rasVals, na.color='transparent')
    map %>% addLegend("bottomright", pal = legPal, title = legTitle,
                      values = rasVals, layerId = 'leg',
                      labFormat = reverseLabels(2, reverse_order=TRUE))
  }
  map %>% 
    clearMarkers() %>% 
    clearShapes() %>%
    removeImage(clearID) %>%
    addRasterImage(ras, colors = rasPal, opacity = 0.7, 
                   group = 'c7', layerId = 'rProj') %>%
    addPolygons(lng=polyXY[,1], lat=polyXY[,2], layerId="projExt", fill = FALSE,
                weight=4, color="green", group='c8')
  
  for (shp in bgShpXY()) {
    map %>%
      addPolygons(lng=shp[,1], lat=shp[,2], fill = FALSE,
                  weight=4, color="red", group='c8')  
  }
}

drawToolbarRefresh <- function(map) {
  map %>% leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) %>%
    leaflet.extras::addDrawToolbar(
      targetGroup='draw',
      polylineOptions = FALSE,
      rectangleOptions = FALSE,
      circleOptions = FALSE,
      markerOptions = FALSE)
}

####################### #
# SESSION CODE ####
####################### #

makeCap <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
getSpName <- function() deparse(substitute(input$spName))
printVecAsis <- function(x) {
  if (is.character(x)) {
    if (length(x) == 1) {
      return(paste0("\'", x, "\'"))
    } else {
      return(paste0("c(", paste(sapply(x, function(a) paste0("\'", a, "\'")), collapse=", "), ")"))
    }
  } else {
    if (length(x) == 1) {
      return(x)
    } else {
      return(paste0("c(", paste(x, collapse=", "), ")"))
    }
  }
}
