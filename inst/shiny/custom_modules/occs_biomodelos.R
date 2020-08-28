occs_biomodelos_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$div(title = 'Examples: Marmosa xerophila, Tremarctos ornatus',
             textInput(ns("spNameBM"), label = "Enter species scientific name",
                       placeholder = 'format: Genus species')),
    tags$div(passwordInput(ns("keyBM"), label = "Enter API Key", value = "")),
    actionButton(ns("goBiomodelosOccs"), "Query Biomodelos")
  )
}

occs_biomodelos_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp

  observeEvent(input$goBiomodelosOccs, {
    # FUNCTION CALL ####
    occsTbls <- occs_biomodelos(input$spNameBM, input$keyBM, logger)

    req(occsTbls)

    # LOAD INTO SPP ####
    occsOrig <- occsTbls$orig
    occs <- occsTbls$cleaned
    sp <- formatSpName(input$spNameBM)
    # if species name is already in list, overwrite it
    if (!is.null(spp[[sp]])) spp[[sp]] <- NULL
    # add two copies of occs dataset -- "occs" will be altered during session,
    # while "occsOrig" will be preserved in this state
    # rmm is the range model metadata object
    spp[[sp]] <- list(occs = occs,
                      occData = list(occsOrig = occsOrig,
                                     occsCleaned = occs),
                      rmm = rangeModelMetadata::rmmTemplate(),
                      rmd = list())

    # METADATA ####
    spp[[sp]]$rmm$data$occurrence$taxon <- sp
    spp[[sp]]$rmm$data$occurrence$dataType <- "presence only"
    spp[[sp]]$rmm$data$occurrence$presenceSampleSize <- nrow(occs)
    # spp[[sp]]$rmm$data$occurrence$yearMin <- paste(min(occs$late_age), "mya")
    # spp[[sp]]$rmm$data$occurrence$yearMax <- paste(max(occs$early_age), "mya")
    spp[[sp]]$rmm$code$wallace$occsRemoved <- nrow(occsOrig) - nrow(occs)
    spp[[sp]]$rmm$data$occurrence$sources <- "Biomodelos"

    common$update_component(tab = "Map")
  })

  return(list(
    save = function() {
      list(
        spNameBM = input$spNameBM,
      )
    },
    load = function(state) {
      updateTextInput(session, "spNameBM", value = state$spNameBM)
    }
  ))

}

occs_biomodelos_module_map <- function(map, common) {
  spp <- common$spp
  curSp <- common$curSp
  occs <- spp[[curSp()]]$occData$occsCleaned
  map %>% clearAll() %>%
    addCircleMarkers(data = occs, lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                     fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
    zoom2Occs(occs)
}

occs_biomodelos_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    occs_biomodelos_knit = species$rmm$data$occurrence$sources == "Biomodelos"
  )
}

