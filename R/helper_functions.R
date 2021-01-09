####################### #
# MISC ####
####################### #

# retrieves the species name for use internally in non-shiny functions
#' @export
spName <- function(sp) {
  if (is.null(sp)) {
    return("species")
  } else {
    return(paste(strsplit(as.character(sp), "_")[[1]], collapse = " "))
  }
}

# either prints a message to console or makes a progress bar in the shiny app
# the entry of the first param "logs" turns on shiny functionality
#' @export
smartProgress <- function(logs, message, expr) {
  if(!is.null(logs)) {
    withProgress(message = message, expr)
  } else {
    message(message)
    expr
  }
}

#' @export
formatSpName <- function(spNames) {
  spNames <- as.character(spNames)
  # separate by space
  spNames.fmt <- sapply(spNames, function(x) strsplit(x, split=' '))
  # put underscores in
  spNames.fmt <- sapply(spNames.fmt, function(x) paste(x, collapse='_'))
  return(spNames.fmt)
}

#' @export
fileNameNoExt <- function(f) {
  sub(pattern = "(.*)\\..*$", replacement = "\\1", f)
}

#' @export
writeSpp <- function(spp, sp, dir) {
  if(!is.null(spp[[sp]]$occs)) write.csv(spp[[sp]]$occs, file.path(dir, paste0(sp, "_occs.csv")))
  if(!is.null(spp[[sp]]$bg)) write.csv(spp[[sp]]$bg, file.path(dir, paste0(sp, "_bg.csv")))
  if(!is.null(spp[[sp]]$procEnvs$bgMask)) raster::writeRaster(spp[[sp]]$procEnvs$bgMask, file.path(dir, paste0(sp, "_bgMask.tif")), bylayer = TRUE)
}

#' Add text to a logger
#'
#' @param logger The logger to write the text to. Can be NULL or a function
#' @param ... Messages to write to the logger
#' @param type One of "default", "error", "warning"
#' @export
writeLog <- function(logger, ..., type = 'default') {
  if (is.null(logger)) {
    if (type == 'error') {
      stop(paste0(..., collapse = ""), call. = FALSE)
    } else if (type == 'warning') {
      warning(paste0(..., collapse = ""), call. = FALSE)
    } else {
      message(paste0(..., collapse = ""))
    }
  } else if (is.function(logger)) {
    if (type == "default") {
      pre <- "> "
    } else if (type == 'error') {
      shinyalert::shinyalert("Please, check window-Log for more information (**)",
                             type = "error")
      pre <- '> <font color="red"><b>! ERROR</b></font> : '
    } else if (type == 'warning') {
      shinyalert::shinyalert("Please, check window-Log for more information (**)",
                             type = "warning")
      pre <- '> <font color="orange"><b>! WARNING</b></font> : '
    }
    newEntries <- paste0('<br>', pre, ..., collapse = "")
    logger(paste0(logger(), newEntries))
  } else {
    warning("Invalid logger type")
  }
  invisible()
}

# Highlight species name in Windows Log
#' @export
hlSpp <- function(scientificName) {
  if (is.null(scientificName)) {
    return("")
  } else if (grepl("_", scientificName)) {
    scientificName <- gsub("_", " ", scientificName)
    boldSpp <- paste0('<font color="#003300"><b><i>', scientificName, '</i> | </b></font>')
    return(boldSpp)
  }
}

####################### #
# MAPPING ####
####################### #

# map occurrences with the Wallace default symbology
#' @export
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
#' @export
mapBgPolys <- function(map, bgShpXY, color = "blue", group = "proj") {
  for (shp in bgShpXY) {
    map %>%
      addPolygons(lng = shp[,1], lat = shp[,2], fill = FALSE,
                  weight = 4, color = color, group = group)
  }
}

#' @export
clearAll <- function(map) {
  map %>% clearMarkers() %>% clearShapes() %>% clearImages() %>%
    clearControls() %>% removeLayersControl()
}

# zoom to occ pts
#' @export
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
#' @export
smartZoom <- function(longi, lati) {
  lg.diff <- abs(max(longi) - min(longi))
  lt.diff <- abs(max(lati) - min(lati))
  if (lg.diff > 1) lg.diff <- 1
  if (lt.diff > 1) lt.diff <- 1
  c(min(longi-lg.diff), min(lati-lt.diff), max(longi+lg.diff), max(lati+lt.diff))
}

# zooms appropriately for any polygon
#' @export
polyZoom <- function(xmin, ymin, xmax, ymax, fraction) {
  x <- (xmax - xmin) * fraction
  y <- (ymax - ymin) * fraction
  x1 <- xmin - x
  x2 <- xmax + x
  y1 <- ymin - y
  y2 <- ymax + y
  return(c(x1, y1, x2, y2))
}

####################### #
# OBTAIN OCCS ####
####################### #
#' @export
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
#' @export
remEnvsValsNA <- function(occs, occsEnvsVals, sppName, logger) {
  withProgress(message = "Checking for points with NA values...", {
    na.rowNums <- which(rowSums(is.na(occsEnvsVals)) > 1)
    if (length(na.rowNums) == length(occsEnvsVals)) {
      logger %>% writeLog(
        type = 'error',
        hlSpp(sppName), paste0('No localities overlay with environmental ',
        'predictors. All localities may be marine -- please redo with ',
        'terrestrial occurrences.')
      )
      return()
    }
    if (length(na.rowNums) > 0) {
      occs.notNA <- occs[-na.rowNums,]
      logger %>% writeLog(
        type = 'warning',
        hlSpp(sppName), 'Removed records without environmental values with occIDs: ',
        paste(occs[na.rowNums,]$occID, collapse=', '), ".")
      return(occs.notNA)
    }

    # # check to see if any cells are NA for one or more rasters but not all,
    # # then fix it
    # n <- raster::nlayers(envs)
    # z <- raster::getValues(envs)
    # z.rs <- rowSums(is.na(z))
    # z.i <- which(z.rs < n & z.rs > 0)
    # if(length(z.i) > 0) {
    #   envs[z.i] <- NA
    #   warning(paste0("Environmental raster grid cells (n = ", length(z.i), ") found with NA values for one or more but not all variables. These cells were converted to NA for all variables.\n"), immediate. = TRUE)
    # }

    return(occs)
  })
  }

####################### #
# PROCESS ENVS ####
####################### #

# make a minimum convex polygon as SpatialPolygons object
#' @export
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
#' @export
maxentJARversion <- function() {
  if (is.null(getOption('dismo_rJavaLoaded'))) {
    # to avoid trouble on macs
    Sys.setenv(NOAWT=TRUE)
    if ( requireNamespace('rJava') ) {
      rJava::.jpackage('dismo')
      options(dismo_rJavaLoaded=TRUE)
    } else {
      stop('rJava cannot be loaded')
    }
  }
  mxe <- rJava::.jnew("meversion")
  v <- try(rJava::.jcall(mxe, "S", "meversion"))
  return(v)
}

####################### #
# VISUALIZE ####
####################### #
#' @export
predictMaxnet <- function(mod, envs, clamp, type) {
  requireNamespace("maxnet", quietly = TRUE)
  envs.n <- raster::nlayers(envs)
  envs.pts <- raster::getValues(envs) %>% as.data.frame()
  mxnet.p <- predict(mod, envs.pts, type = type,
                     clamp = clamp)
  envs.pts[as.numeric(row.names(mxnet.p)), "pred"] <- mxnet.p
  pred <- raster::rasterFromXYZ(cbind(raster::coordinates(envs),
                                      envs.pts$pred),
                                res = raster::res(envs),
                                crs = raster::crs(envs))
  return(pred)
}

#' @export
evalPlots <- function(evalOut) {
  par(mfrow=c(3,2))
  fc <- length(unique(evalOut@features))
  col <- rainbow(fc)
  rm <- length(unique(evalOut@rm))
  plot(rep(1, times=fc), 1:fc, ylim=c(.5,fc+1), xlim=c(0,3), axes=F, ylab='', xlab='', cex=2, pch=21, bg=col)
  segments(rep(.8, times=fc), 1:fc, rep(1.2, times=fc), 1:fc, lwd=1, col=col)
  points(rep(1, times=fc), 1:fc, ylim=c(-1,fc+2), cex=2, pch=21, bg=col)
  text(x=rep(1.3, times=fc), y=1:fc, labels=unique(evalOut@features), adj=0)
  text(x=1, y=fc+1, labels="Feature Classes", adj=.20, cex=1.3, font=2)
  ENMeval::eval.plot(evalOut, legend=FALSE, value="delta.AICc")
  ENMeval::eval.plot(evalOut, legend=FALSE, value="Mean.AUC", variance="Var.AUC")
  ENMeval::eval.plot(evalOut, legend=FALSE, value="Mean.AUC.DIFF", variance="Var.AUC.DIFF")
  ENMeval::eval.plot(evalOut, legend=FALSE, value="Mean.ORmin", variance="Var.ORmin")
  ENMeval::eval.plot(evalOut, legend=FALSE, value="Mean.OR10", variance="Var.OR10")
}

# make data.frame of lambdas vector from Maxent model object
#' @export
lambdasDF <- function(mx, alg) {
  if (alg == "maxent.jar") {
    lambdas <- mx@lambdas[1:(length(mx@lambdas)-4)]
    data.frame(var = sapply(lambdas, FUN = function(x) strsplit(x, ',')[[1]][1]),
               coef = sapply(lambdas, FUN = function(x) as.numeric(strsplit(x, ',')[[1]][2])),
               row.names=1:length(lambdas))
  } else if (alg == "maxnet") {
    lambdas <- mx$betas
    data.frame(var = names(lambdas),
               coef = lambdas,
               row.names = 1:length(lambdas))
  }
}
## pulls out all non-zero, non-redundant (removes hinge/product/threshold) predictor names
#' @export
mxNonzeroCoefs <- function(mx, alg) {
  if (alg == "maxent.jar") {
    x <- lambdasDF(mx, alg)
    #remove any rows that have a zero lambdas value (Second column)
    x <- x[(x[,2] != 0),]
    #remove any rows that have duplicate "var"s (hinges, quadratics, product)
    x <- unique(sub("\\^\\S*", "", x[,1]))
    x <- unique(sub("\\`", "", x))
    x <- unique(sub("\\'", "", x))
    x <- unique(sub("\\=\\S*", "", x))
    x <- unique(sub("\\(", "", x))
    x <- unique(unlist(strsplit(x, split = "\\*")))
    x <- sort(x)
  } else if (alg == "maxnet") {
    x <- lambdasDF(mx, alg)
    #remove any rows that have a zero lambdas value (Second column)
    x <- x[(x[,2] != 0),]
    #remove any rows that have duplicate "var"s (hinges, quadratics, product)
    x <- unique(sub("\\^\\S*", "", x[,1]))
    x <- unique(sub("\\I", "", x))
    x <- unique(sub("\\hinge", "", x))
    x <- unique(sub("\\categorical", "", x))
    x <- unique(sub("\\)\\:\\S*", "", x))
    x <- unique(sub("\\(", "", x))
    x <- unique(unlist(strsplit(x, split = "\\:")))
    x <- sort(x)
  }
}

#' @export
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
#' @export
getRasterVals <- function(r, type='raw') {
  v <- raster::values(r)
  # remove NAs
  v <- v[!is.na(v)]
  if(type == 'logistic' | type == 'cloglog') v <- c(v, 0, 1)  # set to 0-1 scale
  return(v)
}

# getAllThresh <- function(occPredVals) {
#   # remove all NA
#   occPredVals <- na.omit(occPredVals)
#   # apply minimum training presence threshold
#   min.thr <- min(occPredVals)
#   # Define 10% training presence threshold
#   if (length(occPredVals) < 10) {  # if less than 10 occ values, find 90% of total and round down
#     pct10 <- ceiling(length(occPredVals) * 0.1)
#   } else {  # if greater than or equal to 10 occ values, round up
#     pct10 <- floor(length(occPredVals) * 0.1)
#   }
#   pct10.thr <- sort(occPredVals)[pct10]  # apply 10% training presence threshold over all models
#   return(list(mtp = min.thr, p10 = pct10.thr))
# }


####################### #
# PROJECT ####
####################### #
#' @export
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
#' @export
makeCap <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
#' @export
getSpName <- function() deparse(substitute(input$spName))
#' @export
printVecAsis <- function(x, asChar = FALSE) {
  if (is.character(x)) {
    if (length(x) == 1) {
      return(paste0("\'", x, "\'"))
    } else {
      if (asChar == FALSE) {
        return(paste0("c(", paste(sapply(x, function(a) paste0("\'", a, "\'")), collapse = ", "), ")"))
      } else {
        return(paste0("(", paste(sapply(x, function(a) paste0("\'", a, "\'")), collapse = ", "), ")"))
      }
    }
  } else {
    if (length(x) == 1) {
      return(x)
    } else {
      if (asChar == FALSE) {
        return(paste0("c(", paste(x, collapse = ", "), ")"))
      } else {
        return(paste0("(", paste(x, collapse = ", "), ")"))
      }
    }
  }
}

#####################
# Download utlities #
#####################
#' @export
convert_list_cols <- function(x) {
  dplyr::mutate_if(.tbl = x,
                   .predicate = function(col) inherits(col, "list"),
                   .funs = function(col) {
                     vapply(col,
                            jsonlite::toJSON,
                            character(1L))
                   })
}
#' @export
write_csv_robust <- function(x, ...) {
  write.csv(convert_list_cols(x), ...)
}
