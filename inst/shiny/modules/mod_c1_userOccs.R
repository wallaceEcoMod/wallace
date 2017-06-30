
userOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userCSV"), label = "Upload Occurrence CSV")
  )
}

userOccs_MOD <- function(input, output, session, logs, occs) {
  
  readOccsCSV <- reactive({
    req(input$userCSV)
    
    csv <- read.csv(input$userCSV$datapath)
    
    spName <- trimws(as.character(csv$name[1]))
    
    if (!all(c('name', 'longitude', 'latitude') %in% names(csv))) {
      logs %>% writeLog('<font color="red"><b>! ERROR</b></font> : Please input CSV file with columns "name", "longitude", "latitude".')
      return()
    }
    
    
    # subset to just records with first species name, and non-NA latitude and longitude
    uoccs <- csv %>% 
      dplyr::filter(name == spName) %>%
      dplyr::filter(!is.na(latitude) & !is.na(longitude))
      
    if (nrow(uoccs) == 0) {
      logs %>% writeLog('<font color="orange"><b>! WARNING</b></font> : No records with coordinates found in', input$userCSV$name, "for", spName, ".")
      return()
    }
    
    logs %>% writeLog("> User-specified CSV file", input$userCSV$name, "with total of", nrow(uoccs),
                            "records with coordinates was uploaded.")
    
    for (col in c("year", "institutionCode", "country", "stateProvince",
                  "locality", "elevation", "basisOfRecord")) {  # add all cols to match origOccs if not already there
      if (!(col %in% names(uoccs))) uoccs[,col] <- NA
    }
    
    uoccs$origID <- row.names(uoccs)  # add col for IDs
    uoccs$pop <- unlist(apply(uoccs, 1, popUpContent))  # add col for map marker popup text
    
    occs(uoccs)
    return(uoccs)
  })
  
  return(readOccsCSV)
}
