library(shiny)
library(shinyIncubator)
#source("helpers.R")
library(dismo)
library(rgbif)

library(maps)
library(mapproj)

shinyServer(function(input, output) {

  output$gbif.txt1 <- renderText({
    if (input$goName == 0) 
      return()
    input$goName
    name <- isolate(input$gbif.name)
    paste0('There are ', gbif.search()[[1]], ' total records for ', name, '.')
  })
  
  output$gbif.txt2 <- renderText({
    if (input$goName == 0) 
      return()
    input$goLim
    lim <- isolate(input$gbif.lim)
    paste0('Retrieved ', nrow(gbif.search()[[2]]), ' records out of ', ' requested.')
  })
  
  output$gbif.map <- renderPlot({
    if (input$goName == 0) 
      return()
    df <- gbif.search()[[2]]
    if (nrow(df) > 0)
      map("world", fill=TRUE, col="white", bg="lightblue", 
          xlim=c(min(df[,4]) - 5, max(df[,4]) + 5), 
          ylim=c(min(df[,3]) - 5, max(df[,3]) + 5), mar=c(0,0,0,0))
      points(df[,c(4,3)], col='red')
  })
  
  gbif.search <- reactive({
    if (input$goName == 0 & input$goLim == 0) 
      return()
    isolate({
      x <- occ_search(scientificName = input$gbif.name, limit = input$gbif.lim, 
                      fields = 'minimal', hasCoordinate = TRUE)
      count <- x$meta$count
      df <- x$data[!is.na(x$data[,3]),]
      list(count, df)
    })
  })
  


  
})

