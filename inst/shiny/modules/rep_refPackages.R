rep_refPackages_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    strong("Download List of References"), br(), br(),
    strong("Select download file type"),
    selectInput('refFileType', label = "",
                choices = list("PDF", "HTML", "Word")),
    downloadButton('dlrefPackages', 'Download References')
  )
}

rep_refPackages_module_server <- function(input, output, session, common) {}
