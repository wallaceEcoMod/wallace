####################### #
# MISC ####
####################### #

# retrieves the species name for use internally in non-shiny functions
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

formatSpName <- function(spNames) {
  spNames <- as.character(spNames)
  # separate by space
  spNames.fmt <- sapply(spNames, function(x) strsplit(x, split = ' '))
  # put underscores in
  spNames.fmt <- sapply(spNames.fmt, function(x) paste(x, collapse = '_'))
  return(spNames.fmt)
}

# add text to log
#' @export
writeLog <- function(logs, ..., type = 'default') {
  if (is.null(logs)) {
    if (type == 'error') {
      stop(paste0(..., collapse = ""), call.=FALSE)
      return()
    } else if (type == 'warning') {
      warning(paste0(..., collapse = ""), call.=FALSE)
      return()
    }
    message(paste0(..., collapse = ""))
    return()
  }

  if (type == 'default') {
    pre <- "> "
  } else if (type == 'error') {
    pre <- '> <font color="red"><b>! ERROR</b></font> : '
  } else if (type == 'warning') {
    pre <- '> <font color="orange"><b>! WARNING</b></font> : '
  }
  newEntries <- paste0(pre, ..., collapse = "")
  logs(paste0(logs(), newEntries, '', sep = '<br>'))
}

####################### #
# MAPPING ####
####################### #

clearAll <- function(map) {
  map %>% clearMarkers() %>% clearShapes() %>% clearImages() %>%
    clearControls() %>% removeLayersControl()
}

# zoom to occ pts
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
remEnvsValsNA <- function(occs, occsEnvsVals, sppName, logger) {
  withProgress(message = "Checking for points with NA values and in same cells...", {
    na.rowNums <- which(rowSums(is.na(occsEnvsVals[, -1])) >= 1)
    if (length(na.rowNums) == nrow(occsEnvsVals)) {
      logger %>% writeLog(
        type = 'error',
        hlSpp(sppName), paste0('No localities overlay with environmental ',
                               'predictors. For example, all localities may be marine -- please redo with ',
                               'terrestrial occurrences.')
      )
      return()
    }
    if (length(na.rowNums) > 0) {
      logger %>% writeLog(
        type = 'warning',
        hlSpp(sppName), 'Removed records without environmental values with occIDs: ',
        paste(sort(occs[na.rowNums, "occID"]), collapse = ', '), ".")
      occs <- occs[-na.rowNums, ]
      occsEnvsVals <- occsEnvsVals[-na.rowNums, ]
    }
    # Remove same cell duplicates
    occs.dups <- duplicated(occsEnvsVals[, 1])
    if (sum(occs.dups) > 0) {
      logger %>%
        writeLog(type = 'warning',
                 hlSpp(sppName), "Removed ", sum(occs.dups), " localities that ",
                 "shared the same grid cell. occIDs: ",
                 paste(sort(occs[occs.dups, "occID"]), collapse = ', '), ".")
      occs <- occs[!occs.dups, ]
      occsEnvsVals <- occsEnvsVals[!occs.dups, ]
    }
    return(list(occs = occs, occsEnvsVals = occsEnvsVals))
  })
}

####################### #
# VISUALIZE ####
####################### #

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

# retrieve the value range for a prediction raster for plotting
getRasterVals <- function(r, type='raw') {
  v <- raster::values(r)
  # remove NAs
  v <- v[!is.na(v)]
  if(type == 'logistic' | type == 'cloglog') v <- c(v, 0, 1)  # set to 0-1 scale
  return(v)
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


