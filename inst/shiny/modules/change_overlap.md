### **Module: Ratio Overlap**

**BACKGROUND**

It is helpful to calculate the proportion overlap of a species’ range
with other features, for example different land cover classes, protected areas, habitat types, or ecoregions, different types of threats (any user-defined georeferenced polygon). 

**IMPLEMENTATION**

Wallace calculates proportion overlap using the changeRangeR function ratioOverlap. Shapefiles uploaded can be separated by any fields’ categories in a shapefile’s attribute table. 

These analyses require the following data:

Shapefile: With environmental information relevant to the species’ distribution. *NOTE: You must upload all three files (.shp, .shx, .dbf)
SDM: This can be a continuous or thresholded sdm. You can make the model in wallace or upload the sdm in in the User SDM component.



**REFERENCES**

Merow, C., Galante, P., Gerstner, B., Johnson, B., Kass, J.M., Paz, A., Rosauer, D., Serra, P., Anderson, R.P., Blair, M. "changeRangeR: Translating species’ distributions into conservation metrics". In prep.



