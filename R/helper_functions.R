# add text to log
writeLog <- function(logs, ..., type = 'default') {
  if (is.null(logs)) {
    if (type == "default") {
      pre <- "> "
    } else if (type == 'error') {
      pre <- 'ERROR: '
    } else if (type == 'warning') {
      pre <- 'WARNING: '
    }  
    newEntries <- paste(pre, ..., collapse = "")
    message(newEntries)
    return()
  }
  
  if (type == "default") {
    pre <- "> "
  } else if (type == 'error') {
    pre <- '<font color="red"><b>! ERROR</b></font> : '
  } else if (type == 'warning') {
    pre <- '<font color="orange"><b>! WARNING</b></font> : '
  }
  newEntries <- paste(pre, ..., collapse = "")
  logs(paste(logs(), newEntries, sep = '<br>'))
}

popUpContent <- function(x) {
  lat <- round(as.numeric(x['latitude']), digits = 2)
  lon <- round(as.numeric(x['longitude']), digits = 2)
  as.character(tagList(
    tags$strong(paste("occID:", x['occID'])),
    tags$br(),
    tags$strong(paste("Latitude:", lat)),
    tags$strong(paste("Longitude:", lon))
  ))
}

smartProgress <- function(logs, message, expr) {
  if(!is.null(logs)) {
    withProgress(message = message, expr)
  } else {
    message(message)
    expr
  }
}

