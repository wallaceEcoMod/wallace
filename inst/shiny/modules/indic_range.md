### **Module: Calculate Area Metrics**

**BACKGROUND**

Calculating the area for range size is as simple as multiplying the number of cells in a binary raster by the resolution (in km squared). This method is most accurate when your raster is projected in an equal-area projection (such as UTM), but you can also get an estimate from an unprojected raster here.

IUCN’s EOO is defined as the area contained within the shortest imaginary (continuous) boundary drawn to encompass all the known (current) occurrences of a taxon, excluding vagrant localities. This measure may exclude discontinuities or disjunctions within the overall distribution of a taxon (e.g., large areas of unsuitable habitat, but see AOO below). The EOO is typically measured by drawing a minimum convex polygon (MCP, also called a convex hull) around occurrence localities, but this may include many large
areas of obviously unsuitable or unoccupied habitat, making a convex hull around a thresholded SDM more appropriate (Kass et al.). 

Within the calculated EOO area, users can calculate the sum of 2x2 km grid cells to calculate the upper bounds of IUCN’s area of occupancy or AOO. AOO is intended to account for unsuitable or unoccupied habitats that may be included in the EOO calculations. 

It is important to follow the guidelines of the relevant IUCN SSC SG when calculating and contributing EOO or AOO measurements to enable consistency across assessments. AOO should be calculated with a standard grid cell size of 2 km (a cell area of 4 km2) in order to ensure consistency and comparability of results in IUCN assessments.   


**IMPLEMENTATION**

In this module you can calculate areas from a wallace model, a wallace transferred model, a masked model (made in the Mask component), or a user provided SDM.
“Range Size” calculates the area in km squared for the species’ range as defined by a binary raster.
Users can calculate IUCN’s EOO via two options 1) MCP/convex hull around occurrence localities, 2) MCP/convex hull area of a binary SDM.
Users can calculate AOO either 1) with occurrence points, 2) from the pre-masked thresholded SDM, and 3) from the masked thresholded SDM.

These analyses require the following data, depending on the module:

SDM: This must be a binary/thresholded sdm. You can make the model in wallace or upload the sdm in in the User SDM component.

Occurrence records: You must have searched for and plotted species' occurrence data, or uploaded a user-specified csv file for your species in the Occ Data Component. The name of the sdm raster should match the scientific name of the occurrence records.


**REFERENCES**

IUCN Standards and Petitions Committee. 2019. Guidelines for Using the IUCN Red List Categories and Criteria. Version 14. Prepared by the Standards and Petitions Committee.

Merow, C., Galante, P., Gerstner, B., Johnson, B., Kass, J.M., Paz, A., Rosauer, D., Serra, P., Anderson, R.P., Blair, M. “changeRangeR: Translating species’ distributions into conservation metrics”. In prep.

Kass et al spiny pocket mice reference.

