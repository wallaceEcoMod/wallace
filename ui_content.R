library(shiny)

tab1content <- tags$div(span(strong("rgbif"), style = "color:purple; font-size:18pt"), br(),
                        span(em("Interface to the Global Biodiversity Information Facility API"), style = "font-size:10pt"), br(),
                        span(em("Developers of rgbif: Scott Chamberlain, Karthik Ram, Vijay Barve, Dan Mcglinn"), style = "font-size:10pt"), br(),
                        span(em("Other packages used: ggplot2"), style = "color:gray; font-size:8pt"), br(),
                        a("CRAN homepage", href = "http://cran.r-project.org/web/packages/rgbif/index.html", target="_blank"),
                        br(),
                        a("rgbif documentation @ GBIF", href = "http://www.gbif.org/resource/81747", target="_blank"),
                        br(),
                        "The first step is to download occurrence data (e.g. from GBIF; duplicate 
                                records are removed). After acquiring these points, it is useful to examine 
                                them on a map.",
                        br(),
                        "Such datasets can contain errors; as a preliminary method of data-cleaning, 
                                here the user can specify records to be removed. Additionally, the user can 
                                download the records as a CSV file.",
                        br(),
                        "Future versions will allow the user to download occurrence records from other 
                                databases, as well as upload their own occurrence records as an alternate option."
                        )
                        #span(em("<Not currently functional>"), style = "color:gray; font-size:10pt")

tab2content <- tags$div(span(strong("spThin"), style = "color:purple; font-size:18pt"), br(),
                        span(em("Spatial Thinning of Species Occurrence Records"), style = "font-size:10pt"), br(),
                        #span(em("Developers of spThin:"), style = "font-size:10pt"), br(),
                        span(em("Links to"), a("software note", href = "http://onlinelibrary.wiley.com/doi/10.1111/ecog.01132/abstract", target="_blank"), "and",
                             a("CRAN", href = "http://cran.r-project.org/web/packages/spThin/index.html", target="_blank"), style = "font-size:10pt"), br(),
                        span("Citation:  Aiello-Lammens, M. E., Boria, R. A., Radosavljevic, A., Vilela, B. 
                                     and Anderson, R. P. (2015; Early View), spThin: an R package for spatial thinning of species 
                                     occurrence records for use in ecological niche models.", 
                             style = "font-size:9pt"), br(),
                        span(em("Other packages used: ggplot2"), style = "color:gray; font-size:8pt"),
                        br(), br(),
                        "Datasets of occurrence records typically suffer from the effects of biased sampling 
                                across geography. spThin implements one way to reduce the effects of such biases, by 
                                spatial thinning that removes occurrence records less than a user-specified distance 
                                from other records. The user can download the thinned records as a CSV file. This step 
                                is optional.",
                        br(), br())
tab3contentA <- tags$div(span(em("Packages used: ggplot2, sp, rgeos"), style = "color:gray; font-size:8pt"), 
                        br(), br(),
                        "The user then chooses which environmental variables to use as
                                predictors. These data are in raster form.
                                For this demonstration, WorldClim bioclimatic variables are made available at 3 
                                resolutions. For Maxent and many other niche/distribution modeling approaches, 
                                selection of a study region is critical because it defines the pixels whose 
                                environmental values are compared with those of the pixels holding occurrence 
                                records of the species (Anderson & Raza 2010; Barve et al. 2011). As one way to do so, the user can choose a 
                                bounding box or minimum convex polygon around the occurrence records, as well as 
                                buffer distance for either.", 
                        br(),
                        br(),
                        "Future versions will include other sets of environmental variables, as well as 
                                allow users to upload their own sets of environmental variables and designate a 
                                shapefile indicating a custom study region.", 
                        br(), br(),
                        a("Worldclim homepage", href = "http://worldclim.org", target="_blank"),
                        br(), br())
tab3contentB <-tags$div(span("References:", style = "font-size:9pt"), br(),
                        span("Anderson, R.P. & A. Raza. (2010). The effect of the extent 
                                             of the study region on GIS models of species geographic 
                                             distributions and estimates of niche evolution: 
                                             preliminary tests with montane rodents (genus Nephelomys) 
                                             in Venezuela. Journal of Biogeography, 37: 1378–1393.", style = "font-size:9pt"), br(),
                        span("Barve, N., V. Barve, A. Jiménez-Valverde, A. Lira-Noriega, 
                                             S.P. Maher, A.T. Peterson, J. Soberón & F. Villalobos. 
                                             (2011), The crucial role of the accessible area in 
                                             ecological niche modeling and species distribution 
                                             modeling. Ecological Modeling, 222: 1810–1819.", style = "font-size:9pt"))
                        
                        
tab4content <- tags$div(span(strong("ENMeval"), style = "color:purple; font-size:18pt"), br(),
                        span(em("Automated Runs and Evaluations of Ecological Niche Models"), style = "font-size:10pt"), br(),
                        #span(em("Developers of ENMeval:"), style = "font-size:10pt"), br(),
                        span(em("Links to"), a("software note", href = "http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12261/abstract", target="_blank"), "and",
                             a("CRAN", href = "http://cran.r-project.org/web/packages/ENMeval/index.html", target="_blank"), style = "font-size:10pt"), br(),
                        span("Citation:  Muscarella, R., Galante, P. J., Soley-Guardia, M., Boria, R. A., Kass, J. M., 
                                     Uriarte, M., Anderson, R. P. (2014), ENMeval: An R package for conducting spatially 
                                     independent evaluations and estimating optimal model complexity for Maxent ecological 
                                     niche models. Methods in Ecology and Evolution, 5: 1198–1205.", 
                             style = "font-size:9pt"), br(),
                        span(em("Other packages used: ggplot2, raster, dismo"), style = "color:gray; font-size:8pt"), br(),
                        br(),
                        "The output of niche/distribution models varies greatly depending on model settings, 
                                in particular those affecting the level of model complexity. This step automates the 
                                tedious building of a suite of candidate models with differing limitations on complexity. 
                                Furthermore, it quantifies their performance on test records. These metrics can aid the
                                user in selecting optimal settings.",
                        br(),
                        br(),
                        "Future versions will include other options for partitioning occurrence data. 
                                Furthermore, future development of ENMeval or similar packages could provide similar 
                                implementations with other algorithms.",
                        br(), br())
tab5content <- tags$div(span(em("Packages used: raster"), style = "color:gray; font-size:8pt"), br(),
                        br(),
                        "View the prediction rasters. You can download them individually and
                                import into a GIS for further analysis.",
                        br(), br())

tab6contentA <- tags$div(img(src = 'img/jamie.jpg', height=100, hspace = 10, align = 'left'),
                         img(src = 'img/gc_logo.jpg', height=100, hspace = 5),
                         img(src = 'img/city_logo.jpg', height=50), 
                         br(), br(),
                         img(src = 'img/matt.jpg', height=100, hspace = 10),
                         img(src = 'img/UConn_logo.tiff', width=100),
                         br(), br(),
                         img(src = 'img/bruno.jpg', height=100, hspace = 10),
                         img(src = 'img/ufg.jpg', width=100, hspace = 5), 
                         img(src = 'img/uah.jpg', width=140),
                         br(), br(),
                         img(src = 'img/bob.jpg', height=100, hspace = 10),
                         img(src = 'img/aarhus.jpg', height=50), 
                         br(), br(),
                         img(src = 'img/rob.jpg', height=100, hspace = 10),
                         img(src = 'img/city_logo.jpg', height=50, hspace = 5),
                         img(src = 'img/gc_logo.jpg', height=100))

tab6contentB <- tags$div(h5("Jamie M. Kass is a coauthor of ENMeval and a PhD student at CUNY Graduate Center and City College of New York. He
                                          is currently supported by a CUNY Science Scholarship.", align = 'left'),
                         br(), br(), br(),
                         h5("Matthew Aiello-Lammens is the lead author of spThin and a post-doctoral researcher at University of Connecticut.
                                          He is currently supported by NSF DEB-1046328.", align = 'left'),
                         br(), br(), br(),
                         h5("Bruno Vilela is a coauthor of spThin and a PhD student at the Federal University of Goiás (Brazil) and in 
                                          Ecology, Conservation and Restoration of Ecosystems at the University of Alcalá (Spain). He is currently
                                          supported by a CAPES grant for doctoral studies.", align = 'left'),
                         br(), br(), br(),
                         h5("Robert Muscarella is the lead author of ENMeval and a post-doctoral researcher at Aarhus University (Denmark). 
                                          He is currently supported by NSF DEB-1311367 and NSF DBI-1401312.", align = 'left'),
                         br(), br(), br(),
                         h5("Robert P. Anderson is a coauthor of spThin and ENMeval, and a Professor of Biology at City College of New York CUNY. This work was
                                          supported by NSF DEB-1119915.", align = 'left')
)