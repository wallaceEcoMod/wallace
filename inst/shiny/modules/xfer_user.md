### **Module:** ***Transfer to User-Specified Environments***

**BACKGROUND**

In simple terms, applying or “transferring” a niche/distributional model to a region or time period different from the ones used to make the model involves making a prediction based on the model and the new values of the predictor variables. In reality, however, researchers should be cognizant of many possible pitfalls, including non-analog conditions (e.g., requiring extrapolation in environmental space; see **Component: Build and Evaluate Niche Model**) and heterogeneity in the effects of species interactions (Fitzpatrick and Hargrove 2009; Anderson 2013).

**IMPLEMENTATION**

Users must begin by selecting a model. Depending on the ENMeval (Kass et al. 2021) settings selected in **Component: Build and Evaluate Niche Model**, there may be multiple choices for Maxent. For Step 1, users then choose their study region. This is done by drawing a polygon, choosing to use the same extent as the model prediction, or uploading a polygon. The uploaded polygon must be a shapefile (include .shp, .shx, and .dbf) or as a CSV file with field order (longitude, latitude). Once the study region has been delimited, “Create” chooses this extent for all transfer operations. 

In Step 2, upload of the user-specified environments occurs. Note, the data must be uploaded in single-file format (i.e. .tif, .asc) and all rasters must be the same extent and resolution (cell size). Also, the user-provided rasters must be named exactly the same as the original environmental data used in **Component: Obtain Environmental Data**. If users used WorldClim or EcoClimate data to make the model, user-provided bioclimatic variables for this module need to be named using the prefix “bio” followed by two digits (use zero for the first nine variables, e.g., bio01). Users can check the names of the variables used to make the model by referencing the Step 2 section of the Module menu above the “Browse…” input raster button (Ex: Your files must be named as:  bio02, bio05, bio08, bio10).
 
“Transfer” calculates the modeled response for the predictor variable values for each cell of the selected extent and plots the prediction on the map. Users can download the transfer prediction as either raster grid types for analysis (.asc, .grd and .tif), or as an image file (.png).
Users also may choose a thresholding rule to convert the continuous prediction to a binary one (0s and 1s), which can be interpreted as presence/absence or suitable/unsuitable. See guidance text for **Component: Build and Evaluate Niche Model** for more details on thresholding rules. After the model and thresholding selections are made, the prediction can be plotted on the map. Users can download the thresholded prediction as either raster grid types for analysis (.grd and .asc) or as an image file (.png).


**REFERENCES**

Anderson, R.P. (2013). A framework for using niche models to estimate impacts of climate change on species distributions. Annals of the New York Academy of Sciences, 1297(1), 8-28. <a href="https://doi.org/10.1111/nyas.12264" target="_blank">DOI: 10.1111/nyas.12264</a>

Fitzpatrick, M.C., & Hargrove, W.W. (2009). The projection of species distribution models and the problem of non-analog climate. Biodiversity and Conservation, 18, 2255. <a href="https://doi.org/10.1007/s10531-009-9584-8" target="_blank">DOI: 10.1007/s10531-009-9584-8</a>

Kass, J., Muscarella, R., Galante, P.J., Bohl, C.L., Pinilla-Buitrago, G.E., Boria, R.A., Soley-Guardia, M., & Anderson, R.P. (2021). ENMeval: Automated Tuning and Evaluations of Ecological Niche Models. R package version 2.0 <a href="https://CRAN.R-project.org/package=ENMeval" target="_blank">CRAN</a> 

