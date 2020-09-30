mask_temp_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Upload Temporal Rasters (**)", class = "stepText"), br(), br(),
    fileInput(ns("tempRasters"),
              label = "Upload environmental rasters for masking distribution map",
              multiple = TRUE),
    actionButton(ns("goTempRasters"), "Load (**)", class = "tempRast"), br(),
    tags$hr(),
    span("Step 2:", class = "step"),
    span("Land Cover Mask (**)", class = "stepText"), br(),
    uiOutput(ns("curTempRastersUI")),
    textInput(ns("yearInput"),
              label = paste0("Type the years to be used for extracting ",
                             "environmental data, separated by commas")),
    actionButton(ns('goProjectArea'), "Project")
  )
}

mask_temp_module_server <- function(input, output, session, common) {

  spp <- common$spp
  curSp <- common$curSp
  logger <- common$logger


  observeEvent(input$goTempRasters, {
    curSp <- common$curSp
    if (is.null(curSp())) {
      logger %>% writeLog(type = 'error', "Upload some occs or userSDM (**).")
      return()
    }

    if (is.null(input$tempRasters$datapath)) {
      logger %>% writeLog(type = 'warning',
                          "Wait until data is uploaded or specify rasters(**)")
      return()
    }

    ppRasters <- envs_userEnvs(rasPath = input$tempRasters$datapath,
                               rasName = input$tempRasters$name,
                               logger = logger)

    # LOAD INTO SPP ####
    spp[[curSp()]]$postProc$rasters <- ppRasters
  })

  output$curTempRastersUI <- renderUI({
    req(curSp(), spp[[curSp()]]$postProc$rasters)
    if(!is.null(spp[[curSp()]]$postProc$rasters)) {
      n <- c(names(spp[[curSp()]]$postProc$rasters))
    } else {
      n <- NULL
    }
    ppRastersNameList <- setNames(as.list(n), n)
    shinyWidgets::pickerInput("selTempRaster",
                              label = "Select/deselect environmental rasters",
                              choices = ppRastersNameList,
                              multiple = TRUE,
                              selected = ppRastersNameList)
  })

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO SPP ####

    # METADATA ####
  })

  output$result <- renderText({
    # Result
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))

}

mask_temp_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

mask_temp_module_map <- function(map, common) {
  # Map logic
}

mask_temp_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    mask_temp_knit = species$rmm$code$wallace$someFlag,
    var1 = species$rmm$code$wallace$someSetting1,
    var2 = species$rmm$code$wallace$someSetting2
  )
}

