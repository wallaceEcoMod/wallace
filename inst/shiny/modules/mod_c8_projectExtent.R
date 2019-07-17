projExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    div("Step 1:", id = "step"),
    div("Choose Study Region (**)", id = "stepText"), br(), br(),
    selectInput(ns('projExt'), label = "Select method (**)",
                choices = list("User-specified(**)" = 'pjUser',
                               "Draw polygon(**)" = 'pjDraw',
                               "Same extent (**)" = 'pjCur')),
    conditionalPanel(
      sprintf("input['%s'] == 'pjUser'", ns("projExt")),
      fileInput(ns("userBgShp"),
                label = paste0('Upload polygon in shapefile (.shp, .shx, .dbf) or ',
                               'CSV file with field order (longitude, latitude)'),
                accept = c(".csv", ".dbf", ".shx", ".shp"),
                multiple = TRUE),
      tags$div(title = paste0('Buffer area in degrees (1 degree = ~111 km). Exact',
                              ' length varies based on latitudinal position.'),
               numericInput(ns("userPjBuf"),
                            label = "Study region buffer distance (degree)",
                            value = 0, min = 0, step = 0.5))),
    conditionalPanel(
      sprintf("input['%s'] == 'pjDraw'", ns("projExt")),
      tagList("Draw a polygon and select buffer distance(**)", br(), br(),
              tags$div(
                title = paste0('Buffer area in degrees (1 degree = ~111 km). Exact',
                               ' length varies based on latitudinal position.'),
                numericInput(ns("drawPjBuf"),
                             label = "Study region buffer distance (degree)",
                             value = 0, min = 0, step = 0.5))
      )
    ),
    conditionalPanel(
      sprintf("input['%s'] == 'pjCur'", ns("projExt")),
      tagList('You will use the same extent (**)')
    )
  )
}
