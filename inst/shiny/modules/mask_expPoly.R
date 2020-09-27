mask_expPoly_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Choose Input Polygon (**)", class = "stepText"), br(), br(),
    selectInput(ns('polyExpSel'), label = "Select method (**)",
                choices = list("Draw polygon(**)" = 'expDraw',
                               "User-specified polygon(**)" = 'expUser')),
    conditionalPanel(sprintf("input['%s'] == 'expUser'", ns("polyExpSel")),
                     fileInput(ns("polyExpShp"),
                               label = paste0('Upload polygon in shapefile (.shp, .shx, .dbf) or ',
                                              'CSV file with field order (longitude, latitude)'),
                               accept = c(".csv", ".dbf", ".shx", ".shp"), multiple = TRUE)),
    conditionalPanel(sprintf("input['%s'] == 'expDraw'", ns("polyExpSel")),
                     p("Draw a polygon (**)")),
    actionButton(ns("goInputPoly"), "Create(**)"), br(),
    tags$hr(),
    span("Step 2:", class = "step"),
    span("Choose action (**)", class = "stepText"), br(),
    p("Add or remove polygon (**)"),
    tags$div(
      title = "Add guidance text here (**)",
      radioButtons(ns("actExpPoly"), "",
                   choices = list("Add polygon" = 'addPoly',
                                  "Remove polygon" = 'remPoly'), inline = TRUE),
      actionButton(ns('goActionPoly'), "Mask (**)"))
  )
}

mask_expPoly_module_server <- function(input, output, session, common) {

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

mask_expPoly_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

mask_expPoly_module_map <- function(map, common) {
  # Map logic
}

mask_expPoly_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    mask_expPoly_knit = species$rmm$code$wallace$someFlag,
    var1 = species$rmm$code$wallace$someSetting1,
    var2 = species$rmm$code$wallace$someSetting2
  )
}

