### **Module: Remove/Add by expert opinion**

**BACKGROUND**

Range models may omit areas of presence, or include areas where the species does not exist. Expert knowledge can improve the realism of range models, either by adding suitable regions to a model or removing unsuitable ones.

**IMPLEMENTATION**

These analyses require the following data:

Expert defined area: This may be an area to add or remove to the species’ distribution. In wallace, you can draw a polygon or upload a polygon area to add or remove from the sdm.
SDM: This can be a continuous or thresholded sdm. You can make the model in wallace or upload the sdm in in the User SDM component.
Note: You can only remove a polygon from a continuous model prediction. You can add or remove a polygon to a thresholded model prediction. 
Lets try the last Mask module

**REFERENCES**

Merow, C., P. J. Galante, C. Babich Morrow, J. M. Kass, A. Moore, B. E. Gerstner, V. Grisales-Betancur, G. E. Pinilla- Buitrago, E. A. Noguera-Urbano, J. Velasquez-Tibatá, R. P. Anderson, M. E. Blair. Operationalizing expert knowledge in species' range estimates using diverse data types: the R package maskRangeR. https://cmerow.github.io/maskRangeR/c
