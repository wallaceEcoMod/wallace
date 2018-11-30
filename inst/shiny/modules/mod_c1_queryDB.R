queryDb_UI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$div(title = "text",
             radioButtons(ns("occsDb"), label = "Choose Database",
                          choices = c("GBIF" = 'gbif', 
                                      "VertNet" = 'vertnet', 
                                      "BISON" = 'bison',
                                      "BIEN" = 'bien'), 
                          inline = TRUE)),
    conditionalPanel(sprintf("input['%s'] == 'gbif'", ns("occsDb")),
                     checkboxInput(ns("doCitations"), 
                                   label = 'Include Data Source Citations', 
                                   value = FALSE),
                     conditionalPanel(sprintf("input['%1$s'] == 'gbif' & 
                                              input['%2$s'] == true",
                                              ns("occsDb"), ns("doCitations")),
                                      splitLayout(textInput(ns('gbifUser'),
                                                            'GBIF User ID',
                                                            value=NULL),
                                                  textInput(ns('gbifEmail'),
                                                            'GBIF email',
                                                            value=NULL),
                                                  textInput(ns('gbifPW'),
                                                            'GBIF password',
                                                            value=NULL)))),
    tags$div(title = 'Examples: Felis catus, Canis lupus, Nyctereutes procyonoides',
             textInput(ns("spName"), label = "Enter species scientific name", 
                       placeholder = 'format: Genus species')),
    tags$div(title = 'Maximum number of occurrences recovered from databases. 
             Downloaded records are not sorted randomly: 
             rows are always consistent between downloads.',
             numericInput(ns("occsNum"), "Set maximum number of occurrences", 
                          value = 100, min = 1))
  )
}

queryDb_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occsTbls <- c1_queryDb(input$spName, input$occsDb, input$occsNum, 
                           input$doCitations, input$gbifUser, input$gbifEmail,
                           input$gbifPW, shinyLogs)
    req(occsTbls)
    
    # LOAD INTO SPP ####
    occsOrig <- occsTbls$orig
    occs <- occsTbls$cleaned
    n <- formatSpName(occs$taxon_name)
    # if species name is already in list, overwrite it
    if(!is.null(spp[[n]])) spp[[n]] <- NULL
    # add two copies of occs dataset -- higher level occs will be
    # altered during session, while occData$occsCleaned is preserved in the
    # post-download cleaned state; occsOrig is the raw download
    # rmm is the range model metadata object
    spp[[n]] <- list(occs = occs, 
                     occData = list(occsOrig = occsOrig, occsCleaned = occs),
                     rmm = rangeModelMetadata::rmmTemplate())
    
    # METADATA ####
    spp[[n]]$rmm$data$occurrence$taxa <- n
    spp[[n]]$rmm$data$occurrence$dataType <- "presence only"
    spp[[n]]$rmm$data$occurrence$presenceSampleSize <- nrow(occs)
    spp[[n]]$rmm$code$wallaceSettings$occsNum <- input$occsNum
    spp[[n]]$rmm$code$wallaceSettings$occsRemoved <- input$occsNum - nrow(occsTbls$cleaned)
     # store citations with occCite, or just report the database if users are 
     # too lame to use bridgetree
    if(input$doCitations){
      # DOUBLE CHECK THIS DOESN"T NEED TO BE VECTORIZED!!
      spp[[n]]$rmm$data$occurrence$sources <- occsTbls$citations
    } else {
      spp[[n]]$rmm$data$occurrence$sources <- input$occsDb
    }  
    # RETURN ####
    # output the table
    return(occs)
  })
}

queryDb_MAP <- function(map, session) {
  occs <- spp[[curSp()]]$occData$occsCleaned
  map %>% clearAll() %>%
    addCircleMarkers(data = occs, lat = ~latitude, lng = ~longitude, 
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red", 
                     fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
    zoom2Occs(occs)
}

queryDb_INFO <- infoGenerator(modName = "Query Database (Present)",
                              modAuts = "Jamie M. Kass, Bruno Vilela, Gonzalo E. 
                                Pinilla-Buitrago, Hannah Owens, Cory Merow, Robert P.
                                Anderson",
                              pkgName = "spocc")

