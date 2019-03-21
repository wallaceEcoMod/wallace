# 
# sample workflow with module functions
# occs <- c1_queryDb("meles meles", "gbif", 100, doCitations = FALSE)
# sp="Meles_meles"
# envs <- c3_worldclim(10, 1:19)
# occs.thin <- c2_profileOccs(sp.name = sp,
#                             sp.table = occs[[1]]$cleaned,
#                             x.field = "longitude",
#                             y.field = "latitude",
#                             t.field = "year",
#                             l.field = "locality",
#                             # c.field = "country",
#                             # e.field = "elevation",
#                             r.env = envs)
# 
# for(sp in spLoop) {
#   # FUNCTION CALL ####
#   #########
#   # make sure some have been selected
#   # CM: how do I put a shinylog here?
#   # if (is.null(grades)) {
#   #   shinyLogs %>% writeLog(type = 'error', 
#   #                          'You must select some grades to keep.')
#   #   return()
#   # }
#   # CM: i think occs should have all the grades with it
#   # check this uses the right formats
#   print(occs.thin$occ_short_profile$quality.grade)
#   print('       ')
#   print(class(input$grades))
#   print('       ')
#   print(input$grades)
#   print('       ')
#   print(unlist(input$grades) )
#   
#   keep=occs$occ_short_profile %in% unlist(input$grades) 
#   # Test that at least some presences pass the specified test
#   if(length(keep)==0) {
#     shinyLogs %>% writeLog(type = 'error',
#                            '#You must select some grades to keep.')
#     return()
#   }
#   
#   occsClean=occs[keep,]
#   
#   shinyLogs %>% writeLog( 
#     em(spName(occs)), ": Removing dirty occurrences")
#   
#   ########
#   
#   # LOAD INTO SPP ####
#   spp[[sp]]$occs <- occs.clean 
# }
# 
# #occs.thin <- c2_thinOccs(as.data.frame(occs$cleaned), 100)
# ext <- c4_bgExtent(occs.thin, envs, "minimum convex polygon", 1)
# bgMsk <- c4_bgMask(occs.thin, envs, ext)
# bg <- c4_bgSample(occs.thin, bgMsk, 1000)
# grps <- c5_partitionOccs(occs.thin, bg, "block")
# m <- runMaxent(occs.thin, bg, grps$occ.grp, grps$bg.grp, bgMsk, 1:3, 1, c("L", "LQ"), TRUE, "maxnet")
