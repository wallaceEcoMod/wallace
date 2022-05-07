### **Module:** ***Environmental Ordination***

**BACKGROUND**

Although the *Hutchinsonian* niche theoretically can be defined in an *n*-dimensional environmental space, it is convenient to reduce the dimensionality. To this end, Wallace currently uses an ordination approach called Principal Component Analysis (PCA) that maximizes the variation contained in the original predictors into fewer ones. Ordination analyses are commonly used in ecology to study species’ niches (e.g. Blonder et al. 2018, Broennimann et al. 2012, Navarro et al. 2015).

**IMPLEMENTATION**

The PCA is implemented in *Wallace* using the R package `ade4`. The PCA is calibrated over the whole set of background points (i.e. PCAenv in Broennimann et al. 2012). Scores along the two first principal components axes are calculated for the occurrences of the selected species and plotted in the environmental space (points in the left panel). The size of ellipses represent 1.5 times the inertia (i.e. the equivalent of variance in ordination techniques) of the scores of the species along each principal component axes and provide a convenient visualization of the position and breadth of the niches. A correlation circle is plotted in the right panel to help with the interpretation of principal component axes. The lengths and directions of arrows (i.e. Eigen vectors) show the contribution of original predictors to each principal components. 

**REFERENCES**

Blonder, B., Morrow, C.B., Maitner, B., Harris, D.J., Lamanna, C., Violle, C., Enquist, B.J., & Kerkhoff, A.J. (2018). New approaches for delineating n‐dimensional hypervolumes. *Methods in Ecology and Evolution*, 9(2), 305-319. <a href="https://doi.org/10.1111/2041-210X.12865" target="_blank">https://doi.org/10.1111/2041-210X.12865</a>

Broennimann, O., Fitzpatrick, M.C., Pearman, P.B., Petitpierre, B., Pellissier, L., Yoccoz, N.G., Thuiller, W., Fortin, M.J., Randin, C., Zimmermann, N.E., Graham, C.H., & Guisan, A. (2012) Measuring ecological niche overlap from occurrence and spatial environmental data. *Global Ecology and Biogeography*, 21, 481-497. <a href="https://doi.org/10.1111/j.1466-8238.2011.00698.x" target="_blank">https://doi.org/10.1111/j.1466-8238.2011.00698.x</a>

Dray, S., & Dufour, A-B. (2007). The ade4 Package: Implementing the Duality Diagram for Ecologists. *Journal of Statistical Software*, 22(4), 1–20. <a href="https://doi.org/10.18637/jss.v022.i04" target="_blank">https://doi.org/10.18637/jss.v022.i04</a>

Navarro, J., Cardador, L., & Brown, R. (2015). Spatial distribution and ecological niches of non-breeding planktivorous petrels. *Scientific Reports*, 5, 12164. <a href="https://doi.org/10.1038/srep12164" target="_blank">https://doi.org/10.1038/srep12164</a>
