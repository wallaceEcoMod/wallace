queryPaleoDb_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("occsDb"), "Choose Database",
                 choices = list("PaleobioDB" = 'PaleobioDB',
                                "Neotoma" = 'neotoma'), inline = TRUE),
    radioButtons(ns("timeInterval"), "Choose time interval",
                 choices = list("Last Glacial Maximum" = 'LGM',
                                "Holocene" = 'Holo'), inline = TRUE),
    tags$div(title='Examples: Canis lupus, Crocuta crocuta',
             textInput(ns("spName"), label = "Enter species scientific name", 
                       placeholder = 'format: Genus species')),
    tags$div(title='Maximum number of occurrences recovered from databases. Downloaded records are not sorted randomly: rows are always consistent between downloads.',
             sliderInput(ns("occsNum"), "Set maximum number of occurrences", 
                         min = 1, max = 500, value = 100))
  )
}

queryPaleoDb_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occsTbls <- c1_queryPaleoDb(input$spName, 
                               input$occsDb, 
                               input$occsNum, 
                               input$timeInterval, 
                               logs, shiny=TRUE)
    
    req(occsTbls)
    
    # LOAD INTO SPP ####
    occsOrig <- occsTbls$orig
    occs <- occsTbls$cleaned
    n <- formatSpName(occs$taxon_name)
    # if species name is already in list, overwrite it
    if(!is.null(spp[[n]])) spp[[n]] <- NULL
    # add two copies of occs dataset -- "occs" will be altered during session,
    # while "occsOrig" will be preserved in this state
    # rmm is the range model metadata object
    spp[[n]] <- list(occs = occs, occData = list(occsOrig = occsOrig, occsCleaned = occs),
                     rmm = rangeModelMetadata::rangeModelMetadataTemplate())
    
    # METADATA ####
    spp[[n]]$rmm$data$occurrence$taxa <- n
    spp[[n]]$rmm$data$occurrence$dataType <- "presence only"
    spp[[n]]$rmm$data$occurrence$presenceSampleSize <- nrow(occsTbl)
    spp[[n]]$rmm$data$occurrence$sources <- input$occsDb
    spp[[n]]$rmm$code$wallaceSettings$occsNum <- input$occsNum
    # for now, just put yearMin and yearMax as the time interval?
    spp[[n]]$rmm$data$occurrence$yearMin <- input$timeInterval
    spp[[n]]$rmm$data$occurrence$yearMax <- input$timeInterval
    
    # RETURN ####
    return(occs)
  })
}

queryPaleoDb_INFO <- infoGenerator(modName = "Query Database (Paleo)",
                                   modAuts = "Sara Varela, Jamie Kass",
                                   pkgName = c("paleobioDB", "neotoma"))

