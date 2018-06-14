## -------------------------------------------------------------------- ##
## Define functions
## -------------------------------------------------------------------- ##

####################### #
# UI ####
####################### #
uiTop <- function(mod_INFO) {
  modName <- mod_INFO$modName
  pkgName <- mod_INFO$pkgName
  pkgTitl <- mod_INFO$pkgTitl
  
  ls <- list(div(paste("Module: ", modName), id="mod"))
  
  if(!is.null(pkgName)) {
    for(i in 1:length(pkgName)) {
      ls <- c(ls, list(span(pkgName[i], id="rpkg"),
                       span(paste(':', pkgTitl[i]), id="pkgDes"),
                       br()))
    }  
  }
  
  ls <- c(ls, list(HTML('<hr>')))
  return(ls)
}

uiBottom <- function(mod_INFO) {
  modAuts <- mod_INFO$modAuts
  pkgName <- mod_INFO$pkgName 
  pkgAuts <- mod_INFO$pkgAuts
  
  ls <- list(div(paste('Module Developers:', modAuts), id="pkgDes"))
  
  if(!is.null(pkgName)) {
    for(i in 1:length(pkgName)) {
      ls <- c(ls, list(span(pkgName[i], id = "rpkg"), "references", br(),
                       div(paste('Package Developers:', pkgAuts[i]), id="pkgDes"),
                       a("CRAN", href = file.path("http://cran.r-project.org/web/packages", 
                                                  pkgName[i], "index.html"), target = "_blank"), " | ", 
                       a("documentation", href = file.path("https://cran.r-project.org/web/packages", 
                                                           pkgName[i], paste0(pkgName[i], ".pdf")), target = "_blank"), br()))
    }
  }
  return(ls)
}

infoGenerator <- function(pkgName, modName , modAuts) {
  pkgInfo <- sapply(pkgName, packageDescription, simplify = FALSE)
  pkgTitl <- sapply(pkgInfo, function(x) x$Title)
  # remove square brackets and spaces before commas
  pkgAuts <- sapply(pkgInfo, function(x) gsub("\\s+,", ",", gsub("\n|\\[.*?\\]", "", x$Author)))
  # remove parens and spaces before commas
  pkgAuts <- sapply(pkgAuts, function(x) gsub("\\s+,", ",", gsub("\\(.*?\\)", "", x)))
  list(modName = modName,
       modAuts = modAuts,
       pkgName = pkgName,
       pkgTitl = pkgTitl,
       pkgAuts = pkgAuts)
}

####################### #
# MISC ####
####################### #

# retrieves the species name for use internally in non-shiny functions
spName <- function(sp) {
  if(class(sp) == "list") {
    name <- sp$occs$taxon_name[1]
  }
  if("data.frame" %in% class(sp)) {
    name <- sp$taxon_name[1]
  }
  return(paste(strsplit(as.character(name), "_")[[1]], collapse = " "))  
}

# either prints a message to console or makes a progress bar in the shiny app
# the entry of the first param "logs" turns on shiny functionality
smartProgress <- function(logs, message, expr) {
  if(!is.null(logs)) {
    withProgress(message = message, expr)
  } else {
    message(message)
    expr
  }
}

formatSpName <- function(spName) {
  spName <- as.character(spName)
  spl <- strsplit(spName, split=' ')
  if(length(spl[[1]]) > 1) {
    paste(spl[[1]], collapse='_')
  } else {
    spName
  }
}

# for naming files
nameAbbr <- function(spname) {
  namespl <- strsplit(tolower(spname), " ")
  genusAbbr <- substring(namespl[[1]][1], 1, 1)
  fullNameAbbr <- paste0(genusAbbr, "_", namespl[[1]][2])
  return(fullNameAbbr)
}

fileNameNoExt <- function(f) {
  sub(pattern = "(.*)\\..*$", replacement = "\\1", f)
}

writeSpp <- function(spp, sp, dir) {
  if(!is.null(spp[[sp]]$occs)) write.csv(spp[[sp]]$occs, file.path(dir, paste0(sp, "_occs.csv")))
  if(!is.null(spp[[sp]]$bg)) write.csv(spp[[sp]]$bg, file.path(dir, paste0(sp, "_bg.csv")))
  if(!is.null(spp[[sp]]$procEnvs$bgMask)) raster::writeRaster(spp[[sp]]$procEnvs$bgMask, file.path(dir, paste0(sp, "_bgMask.tif")), bylayer = TRUE)
}

# add text to log
writeLog <- function(logs, ..., type = 'default') {
  if (is.null(logs)) {
    if (type == "default") {
      pre <- "> "
    } else if (type == 'error') {
      pre <- 'ERROR: '
    } else if (type == 'warning') {
      pre <- 'WARNING: '
    }  
    newEntries <- paste(pre, ..., collapse = "")
    message(newEntries)
    return()
  }
  
  if (type == "default") {
    pre <- "> "
  } else if (type == 'error') {
    pre <- '<font color="red"><b>! ERROR</b></font> : '
  } else if (type == 'warning') {
    pre <- '<font color="orange"><b>! WARNING</b></font> : '
  }
  newEntries <- paste(pre, ..., collapse = "")
  logs(paste(logs(), newEntries, sep = '<br>'))
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

# map occurrences with the Wallace default symbology
map_occs <- function(map, occs, fillColor = 'red', fillOpacity = 0.2, customZoom = NULL) {
  map %>%
    addCircleMarkers(data = occs, lat = ~latitude, lng = ~longitude, 
                     radius = 5, color = 'red', fill = TRUE, fillColor = fillColor, 
                     fillOpacity = fillOpacity, weight = 2, popup = ~pop)
  if(is.null(customZoom)) {
    map %>% zoom2Occs(occs)
  } else {
    map %>% zoom2Occs(customZoom)
  }
}

# map all background polygons
mapBgPolys <- function(map, bgShpXY) {
  for (shp in bgShpXY) {
    map %>%
      addPolygons(lng = shp[,1], lat = shp[,2], fill = FALSE,
                  weight = 4, color="red", group='proj')
  }
}

clearAll <- function(map) {
  map %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% clearControls()
}

# zoom to occ pts
zoom2Occs <- function(map, occs) {
  # map %>% clearShapes()
  lat <- occs["latitude"]
  lon <- occs["longitude"]
  z <- smartZoom(lon, lat)
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

####################### #
# OBTAIN OCCS ####
####################### #

popUpContent <- function(x) {
  lat <- round(as.numeric(x['latitude']), digits = 2)
  lon <- round(as.numeric(x['longitude']), digits = 2)
  as.character(tagList(
    tags$strong(paste("occID:", x['occID'])),
    tags$br(),
    tags$strong(paste("Latitude:", lat)),
    tags$br(),
    tags$strong(paste("Longitude:", lon)),
    tags$br(),
    tags$strong(paste("Year:", x['year'])),
    tags$br(),
    tags$strong(paste("Inst. Code:", x['institutionCode'])),
    tags$br(),
    tags$strong(paste("Country:", x['country'])),
    tags$br(),
    tags$strong(paste("State/Prov.:", x['stateProvince'])),
    tags$br(),
    tags$strong(paste("Locality:", x['locality'])),
    tags$br(),
    tags$strong(paste("Elevation:", x['elevation'])),
    tags$br(),
    tags$strong(paste("Basis of Record:", x['basisOfRecord']))
  ))
}

####################### #
# COMP 3 ####
####################### #

remEnvsValsNA <- function(occs, occsEnvsVals, logs) {
  withProgress(message = "Checking for points with NA values...", {
    na.rowNums <- which(rowSums(is.na(occsEnvsVals)) > 1)
    
    if (length(na.rowNums) == length(occsEnvsVals)) {
      logs %>% writeLog(type = 'error', 'No localities overlay with environmental predictors. 
                        All localities may be marine -- please redo with terrestrial occurrences.')
      return()
    }
    
    if (length(na.rowNums) > 0) {
      occs.notNA <- occs[-na.rowNums,]
      logs %>% writeLog(type = 'warning', 'Removed records without environmental values with occIDs: ',
                        paste(occs[na.rowNums,]$occID, collapse=', '), ".")
      return(occs.notNA)
    }
    
    return(occs)
  })
  }

####################### #
# PROCESS ENVS ####
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
# MODEL ####
####################### #

maxentPredTransform <- function(results, curModel, bgMask, predType, shinyLogs = NULL) {
  pargs <- paste0("outputformat=", predType) 
  smartProgress(shinyLogs, message = paste0("Generating ", predType, " prediction for model ", curModel, "..."), {
    transPred <- dismo::predict(results$models[[curModel]], bgMask, args=pargs)
  })  
  return(transPred)
}

####################### #
# VISUALIZE ####
####################### #

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

# retrieve the value range for a prediction raster for plotting
getRasterVals <- function(r, type='raw') {
  v <- raster::values(r)
  # remove NAs
  v <- v[!is.na(v)]
  if(type == 'logistic' | type == 'cloglog') v <- c(v, 0, 1)  # set to 0-1 scale
  return(v)
}

getAllThresh <- function(occPredVals) {
  # remove all NA
  occPredVals <- na.omit(occPredVals)
  # apply minimum training presence threshold
  mtp <- min(occPredVals)
  # Define 10% training presence threshold
  if (length(occPredVals) < 10) {  # if less than 10 occ values, find 90% of total and round down
    n90 <- floor(length(occPredVals) * 0.9)
  } else {  # if greater than or equal to 10 occ values, round up
    n90 <- ceiling(length(occPredVals) * 0.9)
  }
  p10 <- rev(sort(occPredVals))[n90]  # apply 10% training presence threshold over all models
  return(list(mtp = mtp, p10 = p10))
}

####################### #
# PROJECT ####
####################### #

reverseLabels <- function(..., reverse_order = FALSE) {
  if (reverse_order) {
    function(type = "numeric", cuts) {
      cuts <- sort(cuts, decreasing = TRUE)
    }
  } else {
    labelFormat(...)
  }
}

####################### #
# SESSION CODE ####
####################### #

makeCap <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
getSpName <- function() deparse(substitute(input$spName))
printVecAsis <- function(x, asChar = FALSE) {
  if (is.character(x)) {
    if (length(x) == 1) {
      return(paste0("\'", x, "\'"))
    } else {
      if(asChar == FALSE) {
        return(paste0("c(", paste(sapply(x, function(a) paste0("\'", a, "\'")), collapse=", "), ")"))
      } else {
        return(paste0("(", paste(sapply(x, function(a) paste0("\'", a, "\'")), collapse=", "), ")"))
      }
    }
  } else {
    if (length(x) == 1) {
      return(x)
    } else {
      if(asChar == FALSE) {
        return(paste0("c(", paste(x, collapse=", "), ")"))  
      } else {
        return(paste0("(", paste(x, collapse=", "), ")"))
      }
    }
  }
}


