### **Module:** ***Occurrence Density Grid***

**BACKGROUND**

The ordination scatterplots with ellipses provided in the previous Module: *Environmental Ordination* are a standard way to represent niches. However, they do not convey which part of the environmental space is occupied more densely by the species, nor do they delineate the availability of environmental conditions present within the background extent. In contrast, the Module: *Occurrence Density Grid* calculates and plots both.

**IMPLEMENTATION**

This module implements an estimation of the occurrence density for each region of the reduced environmental space (of the PCA) using the technique detailed in Broennimann et al. (2012) and implemented with the <a href="https://www.rdocumentation.org/packages/ecospat/versions/3.3/topics/ecospat.grid.clim.dyn" target="_blank">ecospat.grid.clim.dyn</a> function in the R package ecospat (Di Cola et al. 2017). The environmental space of the full extent is represented by a 100 x 100 grid generated using the scores from the first two axes of the PCA (where the x and y-axes correspond to PC1 and PC2, respectively). Then, the densities of occurrence and background points are estimated for each pixel in this environmental space using a kernel density approach (function <a href="https://rdrr.io/cran/ks/man/kde.html" target="_blank">kde</a> in R package `ks`), and plotted with the ecospat.plot.niche function (cor = FALSE), where darker areas represent higher occurrence density (Calenge 2006; see Gerstner et al. 2018, Fig. 2 for an example). In the plots, areas within solid lines represent all environmental conditions available in the background extent, and areas within dashed lines represent the 50% most frequent ones.  In the current implementation for Wallace, only PC1 and PC2 are used to calculate the occurrence density grid and thus niche overlap.

Users can download a .png image of the density grid.

**REFERENCES**

Broennimann, O., Fitzpatrick, M.C., Pearman, P.B., Petitpierre, B., Pellissier, L., Yoccoz, N.G., Thuiller, W., Fortin, M.J., Randin, C., Zimmermann, N.E., Graham, C.H., & Guisan, A. (2012). Measuring ecological niche overlap from occurrence and spatial environmental data. *Global Ecology and Biogeography*, 21(4), 481-497. <a href="https://doi.org/10.1111/j.1466-8238.2011.00698.x" target="_blank">DOI: 10.1111/j.1466-8238.2011.00698.x</a>  

Calenge, C. (2006). The package adehabitat for the R software: tool for the analysis of space and habitat use by animals. *Ecological Modelling*, 197, 1035. <a href="https://doi.org/10.1016/j.ecolmodel.2006.03.017" target="_blank">DOI: 10.1016/j.ecolmodel.2006.03.017</a>  

Di Cola, V., Broennimann, O., Petitpierre, B., Breiner, F.T., d’Amen, M., Randin, C., Engler, R., Pottier, J., Pio, D., Dubuis, A., Pellissier, L., Mateo, R.G., Hordijk, W., Salamin, N., & Guisan, A. (2017). ecospat: an R package to support spatial analyses and modeling of species niches and distributions. *Ecography*, 40(6), 774-787. <a href="https://doi.org/10.1111/ecog.02671" target="_blank">DOI: 10.1111/ecog.02671</a>  

Duong, T. (2022). ks: Kernel Smoothing. R package version 1.13.5, <a href="https://CRAN.R-project.org/package=ks" target="_blank">CRAN</a>  

Gerstner, B.E., Kass, J.M., Kays, R., Helgen, K.M., & Anderson, R.P. (2018). Revised distributional estimates for the recently discovered olinguito (Bassaricyon neblina), with comments on natural and taxonomic history. *Journal of Mammalogy*, 99(2), 321-332. <a href="https://doi.org/10.1093/jmammal/gyy012" target="_blank">DOI: 0.1093/jmammal/gyy012</a>  
