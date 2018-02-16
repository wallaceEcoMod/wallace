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
    occsTbl <- c1_queryPaleoDb(input$spName, input$occDb, input$occNum, input$timeInterval, logs, shiny=TRUE)
    
    if (is.null(occsTbl)) return()
    
    # LOAD INTO SPP ####
    n <- formatSpName(occsTbl$taxon_name)
    # if species name is already in list, overwrite it
    if(!is.null(spp[[n]])) spp[[n]] <- NULL
    # add two copies of occs dataset -- "occs" will be altered during session,
    # while "occsOrig" will be preserved in this state
    # rmm is the range model metadata object
    spp[[n]] <- list(occData = list(occs = occsTbl, occsOrig = occsTbl),
                     rmm = rangeModelMetadata::rangeModelMetadataTemplate())
    
    # METADATA ####
    spp[[n]]$rmm$data$occurrence$taxa <- n
    spp[[n]]$rmm$data$occurrence$dataType <- "presence only"
    spp[[n]]$rmm$data$occurrence$presenceSampleSize <- nrow(occsTbl)
    spp[[n]]$rmm$data$occurrence$sources <- input$occDb
    spp[[n]]$rmm$code$wallaceSettings$occNum <- input$occNum
    # for now, just put yearMin and yearMax as the time interval?
    spp[[n]]$rmm$data$occurrence$yearMin <- input$timeInterval
    spp[[n]]$rmm$data$occurrence$yearMax <- input$timeInterval
    
    # RETURN ####
    return(occsTbl)
  })
}
