### **Module: Principal Components Analysis**

**BACKGROUND**

Although the Hutchinsonian niche can be theoretically defined in an n-dimensional environmental space, it is convenient to reduce the dimensionality for the ease of interpretation and for computational efficiency. To this end, Wallace currently uses a Principal Component Analysis (PCA) that maximize the variation contained in the original predictors into a limited set of meaningful predictors. PCA are commonly used in ecology to study environmental niches (e.g. Navarro et al. 2015, Broennimann et al. 2012)

**IMPLEMENTATION**

The PCA is implemented in Wallace using the R package ade4. The PCA is calibrated over the whole set of background points (i.e. PCAenv in Broennimann et al. 2012). Scores along the two first principal components axes are calculated for the occurrences of the selected species and plotted in the environmental space (points in the left panel). The size of ellipses represent 1.5 times the inertia (i.e. the equivalent of variance in ordination techniques) of the scores of the species along each principal component axes and provide a convenient visualization of the position and breadth of the niches. A correlation circle is plotted in the right panel to help with the interpretation of principal component axes. The lengths and directions of arrows (i.e. Eigen vectors) show the contribution of original predictors to each principal components. 

**REFERENCES**

Navarro, J. et al. Spatial distribution and ecological niches of non-breeding planktivorous petrels. Sci. Rep. 5, 12164; doi: 10.1038/srep12164 (2015).
Broennimann, O., Fitzpatrick, M.C., Pearman, P.B., Petitpierre, B., Pellissier, L., Yoccoz, N.G., Thuiller, W., Fortin, M.J., Randin, C., Zimmermann, N.E., Graham, C.H. & Guisan, A. (2012) Measuring ecological niche overlap from occurrence and spatial environmental data. Global Ecology and Biogeography, 21, 481-497.

