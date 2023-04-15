### **Module:** ***Environmental Ordination***

**BACKGROUND**

Although the *Hutchinsonian* niche theoretically can be defined in an *n*-dimensional environmental space, it is convenient to reduce the dimensionality. To this end, Wallace currently uses an ordination approach called Principal Component Analysis (PCA) that maximizes the variation contained in the original predictors into fewer dimensions. Ordination analyses are commonly used in ecology to study species’ niches (e.g., Blonder et al. 2018, Broennimann et al. 2012, Navarro et al. 2015).

**IMPLEMENTATION**

The PCA is implemented in Wallace using the 
<a href="https://www.rdocumentation.org/packages/ade4/versions/1.7-19/topics/dudi.pca" target="_blank">dudi.pca</a> function in the R package `ade4` (Thioulouse et al. 2018), conducting the ordination via a correlation matrix (with the arguments “center” and “scale” equal to TRUE). The scatterplot and biplot for the correlation circle both use type 2 scaling (Legendre and Legendre 2012, Chapter 9), where distances between occurrence points in the ordination plot are not approximations of Euclidean distances but rather Mahalanobis ones. This scaling gives equal weight to all environmental predictors irrespective of their values, which especially makes sense when variables have different units. Although the PCA is always calculated over the whole set of background environmental values (i.e., PCAenv in Broennimann et al. 2012), users can select between two different plotting options (which do not affect the way the PCA is made). The option “Occurrences only” shows just the occurrence records in the rotated ordination space with ellipses to delineate them, and “Occurrences + Background” shows the occurrence records and their ellipses on top of the background points in this space. The numbers for the x-axis and y-axis correspond to the scores on the respective PC axes selected (x = 1, y = 2 means that the first principal component [PC1] is plotted along the x-axis and the second [PC2] on the y-axis). Typically, PC1 is plotted against PC2 because they have the highest variance explained, but other combinations may be of interest as well. The sizes of the ellipses, which are delineated around the occurrence records, represent 1.5 times the inertia (i.e., eigenvalue, the equivalent of variance in ordination techniques) of the species’ scores along each principal component. These ellipses provide a convenient visualization of the position and breadth of the species’ niches. In the top right corner of the plot, ‘d’ indicates the vertical or horizontal distance between two lines of the grid (gray lines) as an indication of the values of the scores along the axes.

In the PCA Correlation Circle tab, a correlation circle is plotted to help interpret the principal component axes. Loadings are the correlations between each principal component axis and each original predictor. For a given variable, the heads of the arrows correspond to the coordinates of the loadings of this variable for PC1 and PC2. The lengths and directions of the arrows thus show the contributions of the original predictors to each principal component, or PC1 x PC2 axes. A long arrow with a narrow angle (i.e., low slope) would indicate the variable has high correlation with PC1, whereas a long arrow with a wide angle (i.e., high slope) would be more correlated with PC2. Short arrows would indicate low correlations with both axes.  Arrows in opposite directions indicate variables with negative correlations, reflective of an axis that characterizes a contrast between the two variables.

The PCA screeplot in the corresponding tab shows what is termed the inertia for each ordination axis (see above). An axis with high inertia indicates that it contributes a lot to explaining the total variance in the dataset and to explaining distances among points in the scatter plot. Formally, inertia values correspond to eigenvalues in the matrix on which the PCA was calculated. The purpose of the screeplot is to help the user choose which ordination axes to focus on for interpretation and any downstream analyses—generally those with the greatest inertia.

A Results summary comprises the final tab. This documentation includes the inertia for each principal component, the loadings of each variable in each principal component, and other details. 

Users have the option to download a zip file containing .png files of the scatterplots, correlational circle and screeplot, and a .txt file of the summary table.


**REFERENCES**

Blonder, B., Morrow, C.B., Maitner, B., Harris, D.J., Lamanna, C., Violle, C., Enquist, B.J., & Kerkhoff, A.J. (2018). New approaches for delineating n‐dimensional hypervolumes. *Methods in Ecology and Evolution*, 9(2), 305-319. <a href="https://doi.org/10.1111/2041-210X.12865" target="_blank">DOI: 10.1111/2041-210X.12865</a>  

Broennimann, O., Fitzpatrick, M.C., Pearman, P.B., Petitpierre, B., Pellissier, L., Yoccoz, N.G., Thuiller, W., Fortin, M.J., Randin, C., Zimmermann, N.E., Graham, C.H., & Guisan, A. (2012) Measuring ecological niche overlap from occurrence and spatial environmental data. *Global Ecology and Biogeography*, 21, 481-497. <a href="https://doi.org/10.1111/j.1466-8238.2011.00698.x" target="_blank">DOI: 10.1111/j.1466-8238.2011.00698.x</a>  

Dray, S., & Dufour, A-B. (2007). The ade4 Package: Implementing the Duality Diagram for Ecologists. *Journal of Statistical Software*, 22(4), 1–20. <a href="https://doi.org/10.18637/jss.v022.i04" target="_blank">DOI: 10.18637/jss.v022.i04</a>  

Legendre, P., & Legendre, L. (2012). Ordination in reduced space. *Numerical Ecology*, 2, 425-520. <a href="https://doi.org/10.1016/B978-0-444-53868-0.50009-5" target="_blank">DOI: 10.1016/B978-0-444-53868-0.50009-5</a>  

Navarro, J., Cardador, L., & Brown, R. (2015). Spatial distribution and ecological niches of non-breeding planktivorous petrels. *Scientific Reports*, 5, 12164. <a href="https://doi.org/10.1038/srep12164" target="_blank">DOI: 10.1038/srep12164</a>  

Thioulouse, J., Dray, S., Dufour, A., Siberchicot, A., Jombart, T., Pavoine, S. (2018). Multivariate Analysis of Ecological Data with ade4. *Springer*. <a href="https://doi.org/10.1007/978-1-4939-8850-1" target="_blank">DOI: 10.1007/978-1-4939-8850-1</a>  
