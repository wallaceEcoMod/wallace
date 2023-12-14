### **Module:** ***Transfer to New Extent***

**BACKGROUND**  

In simple terms, applying or “transferring” a niche/distributional model to a region or time period different from the ones used to make the model involves making a prediction based on the model and the new values of the predictor variables. In reality, however, researchers should be cognizant of many possible pitfalls, including non-analog conditions (e.g., requiring extrapolation in environmental space; see **Component: Build and Evaluate Niche Model**) and heterogeneity in the effects of species interactions (Fitzpatrick and Hargrove 2009; Anderson 2013).

**IMPLEMENTATION** 

This module relies on functionality for model prediction grids from the R package dismo (Hijmans et al. 2020).  

Users must first select a model (Step 1). Depending on the ENMeval settings selected in **Component: Build and Evaluate Niche Model**, there may be multiple choices for Maxent (Kass et al. 2021). Users have two options to delimit the transfer extent area. First, users draw a polygon on the map to delineate the area desired for the new prediction. Once the polygon has been drawn, “Create” chooses this extent for all transfer operations. Alternatively, users can upload a preexisting polygon by using the User-specified polygon option. The polygon can be uploaded as a shapefile (must include .shp, .shx. .dbf) or as a CSV file with field order (longitude, latitude).  
  
“Transfer” (Step 2) calculates the modeled response for the predictor variable values for each cell of the selected extent and plots the prediction on the map. Users can download the transferred prediction as either raster grid types for analysis (.asc, .grd and .tif), or as an image file (.png).  

Users may choose a thresholding rule to convert the continuous prediction to a binary one (0s and 1s), which can be interpreted as presence/absence or suitable/unsuitable. Please see guidance text for **Component: Build and Evaluate Niche Model** for more details on thresholding rules. After the model and threshold selections are made, the prediction can be plotted on the map. Users can download the thresholded transfer prediction as either raster grid types for analysis (.grd and .asc), or as an image file (.png).
  
NOTE: To reset the selected spatial transfer extent, a red *Reset* button is available at the bottom of the page.
 

**REFERENCES**

Anderson R.P. (2013). A framework for using niche models to estimate impacts of climate change on species distributions. Annals of the New York Academy of Sciences, 1297(1), 8-28. <a href="https://doi.org/10.1111/nyas.12264" target="_blank">DOI: 10.1111/nyas.12264</a>

Fitzpatrick, M. C., & Hargrove, W. W. (2009). The projection of species distribution models and the problem of non-analog climate. Biodiversity and Conservation, 18, 2255. <a href="https://doi.org/10.1007/s10531-009-9584-8" target="_blank">DOI: 10.1007/s10531-009-9584-8</a>

Hijmans, R.J., Phillips, S., Leathwick, J., Elith, J. (2020). dismo: Species Distribution Modeling. R package version 1.3-3. <a href="https://CRAN.R-project.org/package=dismo" target="_blank">CRAN</a> 

Kass, J., Muscarella, R., Galante, P. J., Bohl, C. L., Pinilla-Buitrago, G. E., Boria, R. A., Soley-Guardia, M., Anderson, R. P. (2021). ENMeval: Automated Tuning and Evaluations of Ecological Niche Models. R package version 2.0 <a href="https://CRAN.R-project.org/package=ENMeval" target="_blank">CRAN</a>


