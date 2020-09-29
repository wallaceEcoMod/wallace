rep_markdown_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    strong("Select download file type"),
    selectInput('rmdFileType', label = "",
                choices = list("Rmd", "PDF", "HTML", "Word")),
    downloadButton('dlRMD', 'Download Session Code')
  )
}

rep_markdown_module_server <- function(input, output, session, common) {}
