### **Module:** ***Calculate Environmental Similarity***

**BACKGROUND**  

When transferring a niche/distributional model to new conditions, the new region/time period frequently contains environmental values and combinations that are different than those where it was made (termed “non-analog conditions”). Realistic use of such transfers requires detecting any non-analog conditions and assessing the degree to which extrapolation in environmental space affects the prediction (Williams and Jackson 2007, Fitzpatrick and Hargrove 2009, Anderson 2013). Multivariate environmental similarity surfaces (MESS) constitute one way to address these issues (Elith et al. 2010). High positive values (cooler colors on the map) indicate increasing similarity with the conditions used to train the model, and low negative values (warmer colors on the map) indicate increasing difference.

**IMPLEMENTATION** 

This module relies on the `mess()` function from the R package `dismo` (Hijmans et al. 2020), which calculates a multidimensional similarity surface (Elith et al. 2010).

Users must first select a model. Depending on the ENMeval (Kass et al. 2021) settings selected in **Component: Build and Evaluate Niche Model**, there may be multiple choices for Maxent. Users then draw a polygon on the map to delineate the area for performing the MESS calculations. Once the polygon has been drawn, “Select” chooses this extent, and “Calculate” runs the MESS analysis for the selected extent. Users can download the MESS map as either raster grid types for analysis (.asc, .grd and .tif), or as an image file (.png).


**REFERENCES**

Anderson R.P. (2013). A framework for using niche models to estimate impacts of climate change on species distributions. Annals of the New York Academy of Sciences, 1297(1), 8-28. <a href="https://doi.org/10.1111/nyas.12264" target="_blank">DOI: 10.1111/nyas.12264</a>

Elith, J., Kearney, M., Phillips, S. (2010). The art of modelling range-shifting species. Methods in Ecology and Evolution, 1(4), 330-342. <a href="https://doi.org/10.1111/j.2041-210X.2010.00036.x" target="_blank">DOI: 10.1111/j.2041-210X.2010.00036.x</a>

Fitzpatrick, M. C., Hargrove, W. W. (2009). The projection of species distribution models and the problem of non-analog climate. Biodiversity and Conservation, 18, 2255. <a href="https://doi.org/10.1007/s10531-009-9584-8" target="_blank">DOI: 10.1007/s10531-009-9584-8</a>

Hijmans, R.J., Phillips, S., Leathwick, J., Elith, J. (2020). dismo: Species Distribution Modeling. R package version 1.3-3. <a href="https://CRAN.R-project.org/package=dismo" target="_blank">CRAN</a> 

Kass, J., Muscarella, R., Galante, P. J., Bohl, C. L., Pinilla-Buitrago, G. E., Boria, R. A., Soley-Guardia, M., Anderson, R. P. (2021). ENMeval: Automated Tuning and Evaluations of Ecological Niche Models. R package version 2.0 <a href="https://CRAN.R-project.org/package=ENMeval" target="_blank">CRAN</a>

Williams, J. W., & Jackson, S. T. (2007). Novel climates, no-analog communities, and ecological surprises. Frontiers in Ecology and the Environment, 5(9), 475-482. <a href="https://doi.org/10.1890/070037" target="_blank">DOI: 10.1890/070037</a>

