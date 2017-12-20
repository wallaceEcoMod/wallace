

c1_userOccs <- function(csvPath, csvName, logs = NULL, shiny = FALSE) {
  
  # read in csv
  csv <- read.csv(csvPath, header = TRUE)
  
  name <- trimws(as.character(csv$taxon_name[1]))
  
  if (!all(c('taxon_name', 'longitude', 'latitude') %in% names(csv))) {
    logs %>% writeLog(type = "error", 'Please input CSV file with columns 
                        "taxon_name", "longitude", "latitude".')
    return()
  }
  
  
  # subset to just records with first species name, and non-NA latitude and longitude
  occs <- csv %>% 
    dplyr::filter(taxon_name == name) %>%
    dplyr::filter(!is.na(latitude) & !is.na(longitude))
  
  if (nrow(occs) == 0) {
    logs %>% writeLog(type = 'warning', 'No records with coordinates found in', 
                      csvName, "for", name, ".")
    return()
  }
  
  logs %>% writeLog("User-specified CSV file", csvName, "with total of", 
                    nrow(occs), "records with coordinates was uploaded.")
  
  for (col in c("year", "institution_code", "country", "state_province",
                "locality", "elevation", "record_type")) {  # add all cols to match origOccs if not already there
    if (!(col %in% names(occs))) occs[,col] <- NA
  }
  
  occs$occID <- row.names(occs)  # add col for IDs
  occs$pop <- unlist(apply(occs, 1, popUpContent))  # add col for map marker popup text
  
  return(occs)
}