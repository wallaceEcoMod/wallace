

c1_userOccs <- function(csvPath, csvName, logs = NULL, shiny = FALSE) {
  
  # read in csv
  csv <- read.csv(csvPath, header = TRUE)
  
  # check to make sure all column names are correct
  if (!all(c('taxon_name', 'longitude', 'latitude') %in% names(csv))) {
    logs %>% writeLog(type = "error", 'Please input CSV file with columns "taxon_name", "longitude", "latitude".')
    return()
  }
  
  # get all species names
  spNames <- trimws(as.character(unique(csv[,1])))
  
  # subset to just records with non-NA latitude and longitude
  occs <- csv %>% dplyr::filter(!is.na(latitude) & !is.na(longitude))
  
  if (nrow(occs) == 0) {
    logs %>% writeLog(type = 'warning', 'No records with coordinates found in ', csvName, ".")
    return()
  }
  
  # put species into a list in the same form as spp
  occsList <- list()
  for (i in spNames) {
    x <- occs %>% dplyr::filter(taxon_name == i)
    # add occID field
    x$occID <- row.names(x)
    # add all cols to match dbOccs if not already there
    for (col in c("year", "institution_code", "country", "state_province",
                  "locality", "elevation", "record_type")) {  
      if (!(col %in% names(x))) x[,col] <- NA
    }
    # add popup field
    x$pop <- unlist(apply(x, 1, popUpContent))
    n <- formatSpName(i)
    occsList[[n]] <- list(occs = x)
  }
  
  logs %>% writeLog("User-specified CSV file ", csvName, " with total of ", 
                    length(spNames), " species and ", nrow(occs), " records 
                    with coordinates was uploaded.")
  
  return(occsList)
}
