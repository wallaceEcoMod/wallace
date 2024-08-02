### **Module:** ***Transfer to New Time***

**BACKGROUND**  

In simple terms, applying or “transferring” a niche/distributional model to a region or time period different from the ones used to make the model involves making a prediction based on the model and the new values of the predictor variables. In reality, however, researchers should be cognizant of many possible pitfalls, including non-analog conditions (e.g., requiring extrapolation in environmental space; see **Component: Build and Evaluate Niche Model**) and heterogeneity in the effects of species interactions (Fitzpatrick and Hargrove 2009; Anderson 2013). 

To predict to different times, datasets describing environmental variables in these times are needed. <a href="https://www.ipcc-data.org/guidelines/pages/gcm_guide.html" target="_blank">Global circulation models</a> (GCMs) provide estimates for climate for both the past and future. Various GCMs may have disparate estimates because they are based on different assumptions. Wallace currently uses future climate data based on the user’s selection of WorldClim or ecoClimate source variables. For WorldClim, CMIP6 downscaled future climate projections are available via WorldClim v2.1. Some climate projections use the four Representative Concentration Pathways (RCPs) available (RCP2.6, RCP4.5, RCP6.0, and RCP8.5), which span a range of climate change scenarios from different greenhouse gas emission outcomes. Others use Shared Socio-economic Pathways (SSPs; 126, 245, 370 and 585). More information on climate change models can be found <a href="https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained?fbclid=IwAR3uHOtK48LMz1jbJ5_N4kyEsPtL1aKMHZC3KV7Y_ewddFlGvVY89ho7u9Q" target="_blank">here</a> (Hausfather 2019).  

**IMPLEMENTATION** 

This module relies on functionality for model prediction grids from the R package `dismo` (Hijmans et al. 2020) and `geodata` for accessing climate data ((Hijmans et al. 2024).
Users must first select a model. Depending on the ENMeval (Kass et al. 2021) settings selected in **Component: Build and Evaluate Niche Model**, there may be multiple choices for Maxent. For Step 1, users choose their study region. This is done by drawing a polygon, choosing to use the same extent as the model prediction, or uploading a polygon. The uploaded polygon must be a shapefile (include .shp, .shx, and .dbf) or a CSV file with field order (longitude, latitude). Once the study region has been delimited, “Create” chooses this extent for all transfer operations. 

In Step 2, users must then select a time period to transfer and the source of variables (Worldclim or ecoClimate). The WorldClim option allows users to select a time period (year 2050 or 2070), and also a GCM and RCP that estimates the future climate. Note, three GCMs (FIO-ESM-2-0, GFDL-ESM4, & HadGEM3-GC31-LL) are excluded due to unavailibility for all SSP options. Using ecoClimate lets users select the Atmospheric Oceanic General Circulation Model and the temporal scenario (2080-2100 at different RCPs, Holocene, or LGM).

“Transfer” calculates the modeled response for the predictor variable values for each cell of the selected extent and plots the prediction on the map. Users can download the prediction as either raster grid types for analysis (.asc, .grd and .tif), or as an image file (.png).

Users may choose a thresholding rule to convert the continuous prediction to a binary one (0s and 1s), which can be interpreted as presence/absence or suitable/unsuitable. Please see guidance text for **Component: Build and Evaluate Niche Model** for more details on thresholding rules. After the model and threshold selections are made, the prediction can be plotted on the map. Users can download the prediction as either raster grid types for analysis (.grd and .asc), or as an image file (.png).


**REFERENCES**

Anderson, R.P. (2013). A framework for using niche models to estimate impacts of climate change on species distributions. Annals of the New York Academy of Sciences, 1297(1), 8-28. <a href="https://doi.org/10.1111/nyas.12264" target="_blank">DOI: 10.1111/nyas.12264</a>

Fitzpatrick, M.C., & Hargrove, W.W. (2009). The projection of species distribution models and the problem of non-analog climate. Biodiversity and Conservation, 18, 2255. <a href="https://doi.org/10.1007/s10531-009-9584-8" target="_blank">DOI: 10.1007/s10531-009-9584-8</a>

Hausfather, Z. (2019). CMIP6: the next generation of climate models explained. CarbonBrief. <a href="https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained" target="_blank">www.carbonbrief.org</a>

Hijmans, R.J., Phillips, S., Leathwick, J., & Elith, J. (2020). dismo: Species Distribution Modeling. R package version 1.3-3. <a href="https://CRAN.R-project.org/package=dismo" target="_blank">CRAN</a> 

Hijmans, R.J., et al. (2024). geodata: Download Geographic Data. R package version 0.6-2. <a href="https://CRAN.R-project.org/package=geodata" target="_blank">CRAN</a>

Kass, J., Muscarella, R., Galante, P.J., Bohl, C.L., Pinilla-Buitrago, G.E., Boria, R.A., Soley-Guardia, M., & Anderson, R.P. (2021). ENMeval: Automated Tuning and Evaluations of Ecological Niche Models. R package version 2.0 <a href="https://CRAN.R-project.org/package=ENMeval" target="_blank">CRAN</a>

