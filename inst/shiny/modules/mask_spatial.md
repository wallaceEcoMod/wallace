### **Module: Mask by Shapefile**

**BACKGROUND**

Range models may include areas where the species does not exist. Expert knowledge can improve the realism of range models by removing unsuitable regions from a model - for example, a shape file of rivers and streams can be used to mask out model predictions on land for freshwater fish.  Or, a shapefile of cloud forest extent can be used to mask out any non-cloud forest areas from the model prediction for a mammal found only in cloud forest 


**IMPLEMENTATION**

These analyses require the following data:

Shapefile: With environmental information relevant to the species’ distribution. *NOTE: You must upload all three files (.shp, .shx, .dbf)
SDM: This can be a continuous or thresholded sdm. You can make the model in wallace or upload the sdm in in the User SDM component.

**REFERENCES**

Merow, C., P. J. Galante, C. Babich Morrow, J. M. Kass, A. Moore, B. E. Gerstner, V. Grisales-Betancur, G. E. Pinilla- Buitrago, E. A. Noguera-Urbano, J. Velasquez-Tibatá, R. P. Anderson, M. E. Blair. Operationalizing expert knowledge in species' range estimates using diverse data types: the R package maskRangeR. https://cmerow.github.io/maskRangeR/
