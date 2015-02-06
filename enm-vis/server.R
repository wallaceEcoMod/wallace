library(shiny)
library(shinyIncubator)
#source("helpers.R")
library(dismo)
library(rgbif)

library(ggplot2)


shinyServer(function(input, output) {

  output$gbif.txt <- renderUI({
    if (input$goName == 0) 
      return()
    input$goName
    x <- gbif.search()
    name <- isolate(input$gbifName)
    str1 <- paste('There are', x$meta$count, 'total records for', name, '(limit 500 displayed).')
    str2 <- "Please enter bounding box coordinates for the analysis."
    HTML(paste(str1, str2, sep='<br/>'))
  })
  
  
  output$gbif.map <- renderPlot({
    if (input$goName == 0) 
      return()
    x <- gbif.search()
    df <- x$data[!is.na(x$data[,3]),]
    if (nrow(df) > 0) {
      mapWorld <- borders('world', colour='white', fill='white')
      mp <- ggplot() + mapWorld + 
        theme(panel.background = element_rect(fill = 'lightblue')) +
        geom_point(aes(x = df[,4], y = df[,3]), color = 'blue', size = 3) +
        coord_cartesian(xlim=c(min(df[,4]) - 5, max(df[,4]) + 5), 
                        ylim=c(min(df[,3]) - 5, max(df[,3]) + 5))
      mp
    }
  })
  
  gbif.search <- reactive({
    if (input$goName == 0) 
      return()
    isolate({
      occ_search(scientificName = input$gbifName, limit = 500, 
                 fields = 'minimal', hasCoordinate = TRUE)
    })
  })
  


  
})

