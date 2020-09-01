### **Module: Occurrence Density Grid**

**BACKGROUND**

The scatterplots with inertia ellipse in Module Principal Components Analysis is a standard representation of the environmental niches. However, it does not represent in a straightforward manner which part of the environmental space is populated more densely by the species, and do not show the availability of environmental conditions present within the background extent. The Occurrence Density Module calculates and plots these two features. 

**IMPLEMENTATION**

The module implements a density estimation of the occurrence density for each region of the reduced environmental space using the ecospat.grid.clim.dyn function of the R package ecospat (Broennimann et al. 2016). The environmental space is first gridded in 100 x 100 pixels. Then an occurrence density is estimated for each pixel using a kernel density approach (function kernelUD in R package adehabitatHR). Using the same kernel density approach, the density of environmental conditions in the background extent are also calculated. In the plots, areas within solid lines represent all environmental conditions available in the background extent. Areas within dashed lines represent the 50% most frequent environmental conditions. Darker areas indicate higher densities of occurrences for the focal species.

**REFERENCES**

Broennimann, O., Fitzpatrick, M.C., Pearman, P.B., Petitpierre, B., Pellissier, L., Yoccoz, N.G., Thuiller, W., Fortin, M.J., Randin, C., Zimmermann, N.E., Graham, C.H. & Guisan, A. (2012) Measuring ecological niche overlap from occurrence and spatial environmental data. Global Ecology and Biogeography, 21, 481-497.
Broennimann, O., Di Cola V. and Guisan, A. (2016). ecospat: Spatial Ecology Miscellaneous Methods. R package version 2.1.1. https://CRAN.R-project.org/package=ecospat

