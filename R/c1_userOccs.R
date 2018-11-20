#' @title c1_userOccs
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param csvPath
#' @param csvName
#' @param shinyLogs
# @keywords
#'
# @examples
#'
#'
# @return 
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
# @note

# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.

#' @export

c1_userOccs <- function(csvPath, csvName, shinyLogs = NULL) {
  
  # read in csv
  csv <- read.csv(csvPath, header = TRUE)
  
  # check to make sure all column names are correct
  if (!all(c('taxon_name', 'longitude', 'latitude') %in% names(csv))) {
    shinyLogs %>% writeLog(type = "error", 'Please input CSV file with columns
                           "taxon_name", "longitude", "latitude".')
    return()
  }
  
  # subset to just records with non-NA latitude and longitude
  csv.xy <- csv %>% dplyr::filter(!is.na(latitude) & !is.na(longitude))
  
  # get all species names
  occs <- csv.xy %>% dplyr::filter(!grepl("bg_", taxon_name))
  spNames <- trimws(as.character(unique(occs$taxon_name)))
  
  if (nrow(csv.xy) == 0) {
    shinyLogs %>% writeLog(type = 'warning', 'No records with coordinates found 
                           in ', csvName, ".")
    return()
  }
  
  # put species into a list in the same form as spp
  occsList <- list()
  for (i in spNames) {
    sp.occs <- csv.xy %>% dplyr::filter(taxon_name == i)
    # add occID field if it doesn't exist
    if(!("occID" %in% names(sp.occs))) sp.occs$occID <- row.names(sp.occs)
    # add all cols to match dbOccs if not already there
    for (col in c("country", "state_province", "locality", "year", "record_type", 
                  "institution_code", "elevation", "uncertainty")) {  
      if (!(col %in% names(sp.occs))) sp.occs[,col] <- NA
    }
    # add popup field
    sp.occs$pop <- unlist(apply(sp.occs, 1, popUpContent))
    n <- formatSpName(i)
    
    # subset to just records with latitude and longitude
    occsXY <- sp.occs[!is.na(sp.occs$latitude) & !is.na(sp.occs$longitude),]
    
    # round longitude and latitude with 5 digits
    occsXY['longitude'] <- round(occsXY['longitude'], 5)
    occsXY['latitude'] <- round(occsXY['latitude'], 5)
    
    dups <- duplicated(occsXY[,c('longitude','latitude')])
    occs <- occsXY[!dups,]
    
    occsList[[n]] <- list(orig = sp.occs, cleaned = occs)
    
    # subset by key columns and make id and popup columns
    dupsRem <- nrow(sp.occs) - nrow(occs)
    
    shinyLogs %>% writeLog("Data for ", em(i), " uploaded from ", csvName, ": 
                           Duplicated records removed [", dupsRem, "]. Remaining 
                           records [", nrow(occs), "].")
    
    # look for background records
    sp.bg <- csv.xy %>% dplyr::filter(taxon_name == paste0("bg_", i))
    # if they exist, load them into occsList for the current species
    if(nrow(sp.bg) > 0) {
      occsList[[n]]$bg <- sp.bg
      shinyLogs %>% writeLog("Data for ", em(i), " uploaded from ", csvName, ": ", 
                        nrow(sp.bg), " background records.")
    }
  }
  
  return(occsList)
}
