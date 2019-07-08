# 
# profileOccs_UI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     # CM: what are we doing with this?
#     # tags$div(checkboxInput(ns("spThinAllSp"), label = "Batch for all species?", value = TRUE),
#     tags$div(
#       title='Create a profile characterizing occurrence records for cleaning and filtering.',
#       checkboxGroupInput(ns("grades"),
#                          label = strong("Choose grades to keep"),
#                          choices = list("A: xxx" = "A",
#                                         "B: xxx" = "B",
#                                         "C: xxx" = "C",
#                                         "D: xxx" = "D",
#                                         "E: xxx" = "E",
#                                         "F: xxx" = "F",
#                                         "G: xxx" = "G",
#                                         "H: xxx" = "H")),
#       checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE)
#     )
#   )
# }
# 
# profileOccs_resultsUI <- function(id) {
#   ns <- NS(id)
#   uiOutput(ns('profileOccsResults'))
# }
# 
# profileOccs_MOD <- function(input, output, session) {
#   reactive({
#     # loop over all species if batch is on
#     if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()
# 
#     for(sp in spLoop) {
#       # FUNCTION CALL ####
#       occs.prof <- c2_profileOccs(sp.name = sp,
#                                   sp.table = spp[[sp]]$occs,
#                                   x.field = "longitude",
#                                   y.field = "latitude",
#                                   t.field = "year",
#                                   l.field = "locality",
#                                   # c.field = "country",
#                                   # e.field = "elevation",
#                                   r.env = envs.global[[spp[[sp]]$envs]],
#                                   shinyLogs)
#       req(occs.prof)
# 
#       occs.prof$occ_full_profile <- occs.prof$occ_full_profile[order(occs.prof$occ_full_profile$occID),]
# 
#       # LOAD INTO SPP ####
#       # record present occs before thinning (this may be different from occData$occOrig)
#       spp[[sp]]$procOccs$occsProf <- occs.prof
# 
#       # PLOTS ####
#       output$profileOccsResults <- renderUI({
#         full.qaqc <- occs.prof$occ_full_profile
#         options <- list(scrollX = TRUE, sDom  = '<"top">rtp<"bottom">')
#         output$profOccsTbl <- DT::renderDataTable(full.qaqc %>% dplyr::select(-pop), options = options)
#         output$profOccsBarplot <- renderPlot({
#           proposed.color.grading <- data.frame(row.names = c('A','B','C','D','E','F','G','H'),
#                                                color.qgrade = c('#4575b4','#74add1',
#                                                                 '#abd9e9','#e0f3f8',
#                                                                 '#fee090','#fdae61',
#                                                                 '#f46d43','#d73027'))
#           color.to.plot<-as.character(sapply(full.qaqc$quality.grade,
#                                              function (x) proposed.color.grading[x,'color.qgrade']))
# 
#           full.qaqc$grade.color <- color.to.plot
#           ggplot2::ggplot(ggplot2::aes(x=quality.grade,fill=quality.grade), data = full.qaqc) +
#             ggplot2::geom_bar()
#         })
# 
#         tabsetPanel(
#           tabPanel("Occurrence Profiling Results",
#                    tagList(
#                      DT::dataTableOutput(session$ns('profOccsTbl'))
#                    )),
#           tabPanel("Occurrence Profiling Barplot",
#                    tagList(
#                      plotOutput(session$ns('profOccsBarplot'))
#                    ))
#         )
#       })
# 
#       # METADATA ####
#       # decide later on metadata
#       # spp[[sp]]$rmm$code$wallaceSettings$thinDistKM <- input$thinDist
#     }
#   })
# }
# 
# profileOccsClean_MOD <- function(input, output, session) {
#   reactive({
#     # loop over all species if batch is on
#     #if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()
#     print(input$grades)
#     spLoop <- curSp()
#     for(sp in spLoop) {
#       # FUNCTION CALL ####
#       #########
#       # make sure some have been selected
#       # CM: how do I put a shinylog here?
#       if (is.null(input$grades)) {
#         shinyLogs %>% writeLog(type = 'error',
#                                'You must select some grades to keep.')
#         return()
#       }
#       occs=occs()
#       # CM: i think occs should have all the grades with it
#       # check this uses the right formats
#       print(class(input$grades))
#       print('       ')
#       print(input$grades)
#       print('       ')
#       print(unlist(input$grades) )
# 
#       keep= print(occs.thin$occ_short_profile$quality.grade) %in%
#         unlist(input$grades)
#       print(keep)
#       # Test that at least some presences pass the specified test
#       if(length(keep)==0) {
#         shinyLogs %>% writeLog(type = 'error',
#                               'You must select some grades to keep.')
#         return()
#       }
# 
#       occsClean=occs[keep,]
# 
#       shinyLogs %>% writeLog(
#         em(spName(occs)), ": Removing dirty occurrences")
# 
#       ########
# 
#       # LOAD INTO SPP ####
#       spp[[sp]]$occs <- occs.clean
# 
#       # METADATA ####
#       # decide later on metadata
#       # spp[[sp]]$rmm$code$wallaceSettings$thinDistKM <- input$thinDist
#       # CM: add a vector of which grades/tests kept
#     }
#   })
# }
# 
# occProfile_MAP <- function(map, session) {
#   updateTabsetPanel(session, 'main', selected = 'Map')
#   req(spp[[curSp()]]$procOccs$occsProf)
#   grades <- spp[[curSp()]]$procOccs$occsProf$occ_full_profile$quality.grade
#   grades2 <- spp[[curSp()]]$procOccs$occsProf$occ_full_profile$quality.grade
#   print(grades)
#   print(grades2)
#   # colors for partition symbology
#   n <- length(unique(grades))
#   newColors <- gsub("FF$", "", rainbow(n))
#   partsFill <- newColors[match(grades, sort(unique(grades)))]
# 
#   map %>% clearAll() %>%
#     map_occs(occs(), fillColor = partsFill, fillOpacity = 1) %>%
#     addLegend("bottomright", colors = newColors,
#               title = "Profile Grades", labels = sort(unique(grades)),
#               opacity = 1)
# }
# 
# profileOccs_INFO <- infoGenerator(modName = "Profile Occurrences",
#                                modAuts = "Pep Serra, Cory Merow, Jamie M. Kass",
#                                pkgName = "occProfileR")
# 
