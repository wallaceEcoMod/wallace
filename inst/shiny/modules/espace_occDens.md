### **Module:** ***Occurrence Density Grid***

**BACKGROUND**

Ordination scatterplots with ellipses in Module: *Environmental Ordination* are a standard way to represent niches. However, they do not represent which part of the environmental space is occupied more densely by the species, nor do they show the availability of environmental conditions present within the background extent. The Module: *Occurrence Density Grid* calculates and plots both of these.

**IMPLEMENTATION**

The module implements an estimation of the occurrence density for each region of the reduced environmental space (of the PCA) using the technique detailed in Broennimann et al. (2012) and implemented with the ecospat.grid.clim.dyn function of the R package `ecospat` (Di Cola et al. 2017). The environmental space of the full extent is first represented by a 100 x 100 grid generated using the scores from the PCA. Then, the densities of occurrence and background records are estimated for each pixel in this environmental space using a kernel density approach (function kernelUD in R package `adehabitatHR`), where darker areas represent higher density. In the plots, areas within solid lines represent all environmental conditions available in the background extent. Areas within dashed lines represent the 50% most frequent environmental conditions.

**REFERENCES**

Broennimann, O., Fitzpatrick, M.C., Pearman, P.B., Petitpierre, B., Pellissier, L., Yoccoz, N.G., Thuiller, W., Fortin, M.J., Randin, C., Zimmermann, N.E., Graham, C.H., & Guisan, A. (2012) Measuring ecological niche overlap from occurrence and spatial environmental data. *Global Ecology and Biogeography*, 21(4), 481-497. <a href="https://doi.org/10.1111/j.1466-8238.2011.00698.x" target="_blank">https://doi.org/10.1111/j.1466-8238.2011.00698.x</a> 

Di Cola, V., Broennimann, O., Petitpierre, B., Breiner, F.T., d'Amen, M., Randin, C., Engler, R., Pottier, J., Pio, D., Dubuis, A., Pellissier, L., Mateo, R.G., Hordijk, W., Salamin, N., & Guisan, A. (2017). ecospat: an R package to support spatial analyses and modeling of species niches and distributions. *Ecography*, 40(6), 774-787. <a href="https://doi.org/10.1111/ecog.02671" target="_blank">https://doi.org/10.1111/ecog.02671</a>
