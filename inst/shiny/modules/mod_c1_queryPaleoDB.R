queryDb_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("occDb"), "Choose Database",
                 choices = list("PaleobioDB" = 'PaleobioDB',
                                "Neotoma" = 'neotoma'), inline = TRUE),
    radioButtons(ns("timeInterval"), "Choose time interval",
                 choices = list("Last Glacial Maximum" = 'LGM',
                                "Holocene" = 'Holo'), inline = TRUE),
    tags$div(title='Examples: Canis lupus, Crocuta crocuta',
             textInput(ns("spName"), label = "Enter species scientific name", 
                       placeholder = 'format: Genus species')),
    tags$div(title='Maximum number of occurrences recovered from databases. Downloaded records are not sorted randomly: rows are always consistent between downloads.',
             sliderInput(ns("occNum"), "Set maximum number of occurrences", 
                         min = 1, max = 500, value = 100))
  )
}

queryDb_MOD <- function(input, output, session, rvs) {
  reactive({
    c1_queryPaleoDb(input$spName, input$occDb, input$occNum, input$timeInterval, rvs)
  })
}
