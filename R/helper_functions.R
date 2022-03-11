####################### #
# MISC #
####################### #
#' @title alfred.printVecAsis
#' @description For internal use. Print vector as character string
#' @param x vector
#' @param asChar exclude c notation at the begging of string
#' @export
alfred.printVecAsis <- function(x, asChar = FALSE) {
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

####################### #
# SHINY LOG #
####################### #

#' @title alfred.fmtSpN
#' @description For internal use. Format species name with underscore
#' @param spN Species name
#' @export
alfred.fmtSpN <- function(spN) {
  spN <- as.character(spN)
  # separate by space
  spN.fmt <- sapply(spN, function(x) strsplit(x, split = ' '))
  # put underscores in
  spN.fmt <- sapply(spN.fmt, function(x) paste(x, collapse = '_'))
  return(spN.fmt)
}

#' @title alfred.hlSpp
#' @description For internal use. Green and bold species name in Windows Log
#' @param spN Species name
#' @export
alfred.hlSpp <- function(spN) {
  if (is.null(spN)) {
    return("")
  } else if (grepl("_", spN)) {
    spN <- gsub("_", " ", spN)
    boldSpp <- paste0('<font color="#003300"><b><i>', spN, '</i> | </b></font>')
    return(boldSpp)
  }
}

#
#' @title alfred.smartProgress
#' @description For internal use. Either prints a message to console or makes
#' a progress bar in the shiny app the entry of the first param "logs" turns on
#' shiny functionality
#' @param logs Wallace logger
#' @param message A single-element character vector; the message to be displayed to the user.
#' @param expr The work to be done.
#' @export
alfred.smartProgress <- function(logs, message, expr) {
  if(!is.null(logs)) {
    withProgress(message = message, expr)
  } else {
    message(message)
    expr
  }
}

# retrieves the species name for use internally in non-shiny functions
#' @export
spName <- function(sp) {
  if (is.null(sp)) {
    return("species")
  } else {
    return(paste(strsplit(as.character(sp), "_")[[1]], collapse = " "))
  }
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
      shinyalert::shinyalert("Please, check window-Log for more information ",
                             type = "error")
      pre <- '> <font color="red"><b>! ERROR</b></font> : '
    } else if (type == 'warning') {
      shinyalert::shinyalert("Please, check window-Log for more information ",
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

####################### #
# MAPPING #
####################### #

#' @export
clearAll <- function(map) {
  map %>% clearMarkers() %>% clearShapes() %>% clearImages() %>%
    clearControls() %>% removeLayersControl()
}

# zoom to occ pts
#' @export
zoom2Occs <- function(map, occs) {
  lat <- occs["latitude"]
  lon <- occs["longitude"]
  lg.diff <- abs(max(lon) - min(lon))
  lt.diff <- abs(max(lat) - min(lat))
  if (lg.diff > 1) lg.diff <- 1
  if (lt.diff > 1) lt.diff <- 1
  z <- c(min(lon - lg.diff), min(lat - lt.diff),
         max(lon + lg.diff), max(lat + lt.diff))
  map %>% fitBounds(z[1], z[2], z[3], z[4])

  ## this section makes letter icons for occs based on basisOfRecord
  # makeOccIcons <- function(width = 10, height = 10, ...) {
  #   occIcons <- c('H', 'O', 'P', 'U', 'F', 'M', 'I', 'L', 'A', 'X')
  #   files <- character(9)
  #   # create a sequence of png images
  #   for (i in 1:9) {
  #     f <- tempfile(fileext = '.png')
  #     png(f, width = width, height = height, bg = 'transparent')
  #     graphics::par(mar = c(0, 0, 0, 0))
  #     plot.new()
  #     graphics::points(.5, .5, pch = occIcons[i], cex = min(width, height) / 8, col='red', ...)
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
# OBTAIN OCCS #
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
# ENV DATA #
####################### #
#' @export
remEnvsValsNA <- function(occs, occsEnvsVals, sppName, logger) {
  withProgress(message = "Checking for points with NA values and in same cells...", {
    na.rowNums <- which(rowSums(is.na(occsEnvsVals[, -1])) >= 1)
    if (length(na.rowNums) == nrow(occsEnvsVals)) {
      logger %>% writeLog(
        type = 'error',
        alfred.hlSpp(sppName), paste0('No localities overlay with environmental ',
                               'predictors. For example, all localities may be marine -- please redo with ',
                               'terrestrial occurrences.')
      )
      return()
    }
    if (length(na.rowNums) > 0) {
      logger %>% writeLog(
        type = 'warning',
        alfred.hlSpp(sppName), 'Removed records without environmental values with occIDs: ',
        paste(sort(occs[na.rowNums, "occID"]), collapse = ', '), ".")
      occs <- occs[-na.rowNums, ]
      occsEnvsVals <- occsEnvsVals[-na.rowNums, ]
    }
    # Remove same cell duplicates
    occs.dups <- duplicated(occsEnvsVals[, 1])
    if (sum(occs.dups) > 0) {
      logger %>%
        writeLog(type = 'warning',
                 alfred.hlSpp(sppName), "Removed ", sum(occs.dups), " localities that ",
                 "shared the same grid cell. occIDs: ",
                 paste(sort(occs[occs.dups, "occID"]), collapse = ', '), ".")
      occs <- occs[!occs.dups, ]
      occsEnvsVals <- occsEnvsVals[!occs.dups, ]
    }
    return(list(occs = occs, occsEnvsVals = occsEnvsVals))
  })
}

####################### #
# VISUALIZE & PROJECT #
####################### #
# retrieve the value range for a prediction raster for plotting
#' @export
getRasterVals <- function(r, type='raw') {
  v <- raster::values(r)
  # remove NAs
  v <- v[!is.na(v)]
  if(type == 'logistic' | type == 'cloglog') v <- c(v, 0, 1)  # set to 0-1 scale
  return(v)
}

## pulls out all non-zero, non-redundant (removes hinge/product/threshold) predictor names
#' @export
mxNonzeroCoefs <- function(mx, alg) {
  if (alg == "maxent.jar") {
    lambdas <- mx@lambdas[1:(length(mx@lambdas)-4)]
    x <- data.frame(var = sapply(lambdas, FUN = function(x) strsplit(x, ',')[[1]][1]),
                    coef = sapply(lambdas, FUN = function(x) as.numeric(strsplit(x, ',')[[1]][2])),
                    row.names = 1:length(lambdas))
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
    lambdas <- mx$betas
    x <- data.frame(var = names(lambdas),
                    coef = lambdas,
                    row.names = 1:length(lambdas))
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
predictMaxnet <- function(mod, envs, clamp, type) {
  requireNamespace("maxnet", quietly = TRUE)
  envs.n <- raster::nlayers(envs)
  envs.pts <- raster::getValues(envs) %>% as.data.frame()
  mxnet.p <- stats::predict(mod, envs.pts, type = type,
                            clamp = clamp)
  envs.pts[as.numeric(row.names(mxnet.p)), "pred"] <- mxnet.p
  pred <- raster::rasterFromXYZ(cbind(raster::coordinates(envs),
                                      envs.pts$pred),
                                res = raster::res(envs),
                                crs = raster::crs(envs))
  return(pred)
}

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

##################### #
# DOWNLOAD #
##################### #
#' @export
write_csv_robust <- function(x, ...) {
  a <- dplyr::mutate_if(.tbl = x,
                        .predicate = function(col) inherits(col, "list"),
                        .funs = function(col) {
                       vapply(col,
                              jsonlite::toJSON,
                              character(1L))
                          })
  utils::write.csv(a, ...)
}
