source("functions.R")

comp1_gbifOcc <- function(spName, occNum) {
  # query GBIF based on user input, remove duplicate records
  writeLog("...Searching GBIF...")
  values$spname <- spName  # record species name
  results <- occ_search(scientificName = spName, limit = occNum, hasCoordinate = TRUE)
  
  # Control species not found
  if (results$meta$count == 0) {
    writeLog(paste('* No records found for ', spName, ". Please check the spelling."))
  }
  
  if (results$meta$count != 0) {
    cols <- c('name','decimalLongitude','decimalLatitude',
              'institutionCode','country', 'stateProvince',
              'locality', 'elevation', 'basisOfRecord')
    results <- fixcols(cols, results)
    locs.in <- results$data[!is.na(results$data[,3]),][,cols]
    locs <- remDups(locs.in)
    names(locs)[1:3] <- c('species','longitude', 'latitude')
    locs$origID <- row.names(locs)
    locs$pop <- unlist(apply(locs, 1, popUpContent))
    # add locs to values list and copy
    values$gbifoccs <- locs
    values$gbifoccs <- remDups(values$gbifoccs)
    values$df <- values$gbifoccs
    #       sinkFalse(paste0("map(interior = FALSE)\n",
    #                       "points(df$lon, df$lat, col = 'red', bg = 'blue', pch = 21, cex = 1)"),
    #                 "Plot the occurrence data:")
    
    
    inName <- isolate(spName)
    nameSplit <- length(unlist(strsplit(inName, " ")))
    
    if (nameSplit == 1 && !is.null(locs)) {
      x <- paste("* Please input both genus and species names. More than one species with this genus was found.")
    } else {if (nameSplit == 1 && is.null(locs)) {
      x <- paste("* Please input both genus and species names.")
    } else {if (nameSplit != 1 && is.null(locs)) {
      x <- paste0('* No records found for ', inName, ". Please check the spelling.")
    } else {if (nameSplit != 1 && !is.null(locs)) {
      x <- paste('* Total GBIF records for', values$gbifoccs[1,1], 'returned [', nrow(locs.in),
                 '] out of [', results$meta$count, '] total (limit 500).
                   Duplicated records removed [', nrow(locs.in) - nrow(locs), "]: Remaining records [", nrow(locs), "].")
    }}}}
    writeLog(x)
  }
}
