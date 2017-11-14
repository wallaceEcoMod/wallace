queryPaleoDb_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("occDb"), "Choose Database",
                 choices = list("PaleobioDB" = 'PaleobioDB',
                                "Neotoma" = 'neotoma'), inline = TRUE),
    radioButtons(ns("timeInterval"), "Choose time interval",
                 choices = list("Last Glacial Maximum" = 'LGM',
                                "Holocene" = 'Holo'), inline = TRUE),
    tags$div(title='Examples: Canis lupus, Crocuta crocuta',
             textInput(ns("spName"), label = "Enter species scientific name", 
                       placeholder = 'format: Genus species')),
    tags$div(title='Maximum number of occurrences recovered from databases. Downloaded records are not sorted randomly: rows are always consistent between downloads.',
             sliderInput(ns("occNum"), "Set maximum number of occurrences", 
                         min = 1, max = 500, value = 100))
  )
}

queryPaleoDb_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occs <- c1_queryPaleoDb(input$spName, input$occDb, input$occNum, input$timeInterval, logs, shiny=TRUE)
    
    if (is.null(occs)) return()
    
    # LOAD vals ####
    vals$occDb <- input$occDb
    vals$spName <- input$spName
    vals$occNum <- input$occNum
    
    # MAPPING ####
    map %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearImages() %>%
      addCircleMarkers(data = occs, lat = ~latitude, lng = ~longitude, radius = 5, 
                       color = 'red', fill = TRUE, fillColor = 'red', 
                       fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
      zoom2Occs(occs)
    
    # RETURN ####
    return(occs)
  })
}
