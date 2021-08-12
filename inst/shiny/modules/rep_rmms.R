rep_rmms_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    strong("Download metadata CSV file"), br(), br(),
    downloadButton('dlRMM', 'Download Metadata')
  )
}

rep_rmms_module_server <- function(input, output, session, common) {}
