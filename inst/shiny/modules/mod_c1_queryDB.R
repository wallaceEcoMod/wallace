source("R/c1_queryDb.R", chdir = TRUE)

queryDb_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("occDb"), "Choose Database",
                 choices = list("GBIF" = 'gbif',
                                "VertNet" = 'vertnet',
                                "BISON" = 'bison'), inline = TRUE),
    tags$div(title='Examples: Felis catus, Canis lupus, Nyctereutes procyonoides',
             textInput(ns("spName"), label = "Enter species scientific name", placeholder = 'format: Genus species')),
    tags$div(title='Maximum number of occurrences recovered from databases. Downloaded records are not sorted randomly: rows are always consistent between downloads.',
             sliderInput(ns("occNum"), "Set maximum number of occurrences", min = 1, max = 500, value = 100))
  )
}

queryDb_MOD <- function(input, output, session, rvs) {
  reactive({
    c1_queryDb(input$spName, input$occDb, input$occNum, rvs)
  })
}
