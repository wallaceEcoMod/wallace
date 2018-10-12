

queryDb_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    #radioButtons(ns("occsDb"), "Choose Database", # Jamie's way, but doesn't work with conditional panel
    radioButtons("occsDb", "Choose Database",
                 choices = list("GBIF" = 'gbif',
                                "BIEN" = 'bien',
                                "VertNet" = 'vertnet',
                                "BISON" = 'bison'), inline = TRUE),
    # CM >>
    # add checkbox for data sources
    # this currently doesn't work. however, if you replace ns("occsDb") wiht "occsDb" above, and similarly for doCitations below, it does. but i figure we don't want to 
    conditionalPanel(
      condition="input.occsDb == 'gbif'", # bien citations not working yet | input.occsDb == 'bien'",
        #paste0("input.",ns("occsDb")," == 'gbif'"),
      #checkboxInput(ns('doCitations'),'Include Data Source Citations', value=T)), # Jamie's way, but doesn't work with conditional panel
      checkboxInput('doCitations','Include Data Source Citations', value=T)),
    
    conditionalPanel(
      condition="input.occsDb == 'gbif' & input.doCitations == true",
        #paste0("input.",ns("occsDb")," == 'gbif' & input.",ns("doCitations")," == true"),
      splitLayout(textInput('gbifUser','GBIF User ID',value=NULL),
                  textInput('gbifEmail','GBIF email',value=NULL),
                  textInput('gbifPW','GBIF password',value=NULL))),
    #CM<<
    tags$div(title='Examples: Felis catus, Canis lupus, Nyctereutes procyonoides',
             textInput(ns("spName"), label = "Enter species scientific name", 
                       placeholder = 'format: Genus species')),
    tags$div(title='Maximum number of occurrences recovered from databases. 
             Downloaded records are not sorted randomly: 
             rows are always consistent between downloads.',
             numericInput(ns("occsNum"), "Set maximum number of occurrences", 
                          value = 100, min = 1)))#,
    # CM >>
    # tags$div(title='This info is required by GBIF to enable metadata download. 
    #          Wallace does not save or use this for anything else.',
    #          textInput(ns("gbifUser"), label = " ", 
    #                    placeholder = ' ')),
    # tags$div(title='This info is required by GBIF to enable metadata download. 
    #          Wallace does not save or use this for anything else.',
    #          textInput(ns("gbifEmail"), label = " ", 
    #                    placeholder = ' ')),
    # tags$div(title='This info is required by GBIF to enable metadata download. 
    #          Wallace does not save or use this for anything else.',
    #          textInput(ns("gbifPW"), label = " ", 
    #                    placeholder = '')))
    #CM<<
  
}

queryDb_MOD <- function(input, output, session) {
  reactive({
    # CM >>
    # for testing
    # input=list(spName='Alliaria petiolata',occsDb='gbif',occsNum=50)
    # shinyLogs=NULL
    # spp=list(NULL)
    # n=1
    #CM<<
    
    # FUNCTION CALL ####
    # CM >>
    occsTbls <- c1_queryDb(input$spName,
                           input$occsDb,
                           input$occsNum,
                           input$doCitations,
                           input$gbifUser, 
                           input$gbifEmail,
                           input$gbifPW,
                           shinyLogs)
    # occsTbls <- c1_queryDb(input$spName, 
    #                        input$occsDb, 
    #                        input$occsNum, 
    #                        shinyLogs)
    #CM<<
    
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
    spp[[n]] <- list(occs = occs, occData = list(occsOrig = occsOrig, occsCleaned = occs),
                     rmm = rangeModelMetadata::rmmTemplate())
    
    # METADATA ####
    spp[[n]]$rmm$data$occurrence$taxa <- n
    spp[[n]]$rmm$data$occurrence$dataType <- "presence only"
    spp[[n]]$rmm$data$occurrence$presenceSampleSize <- nrow(occs)
    spp[[n]]$rmm$code$wallaceSettings$occsNum <- input$occsNum
    #CM >>
     # store citations with occCite, or just report the database if users are too lame to use bridgetree
    if(input$doCitations){
      # DOUBLE CHECK THIS DOESN"T NEED TO BE VECTORIZED!!
      spp[[n]]$rmm$data$occurrence$sources <- occsTbls$citations
    } else {
      spp[[n]]$rmm$data$occurrence$sources <- input$occsDb
    }  
    #CM<<
    
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
                              modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson, Hannah Owens, Cory Merow",
                              pkgName = "spocc")

