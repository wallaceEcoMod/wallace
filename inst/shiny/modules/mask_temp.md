### **Module: Temporal Extract**

**BACKGROUND**

When creating SDMs, best practice dictates using environmental data that temporally match the occurrence records. However, this is not always realistic. Often, many occurrence records are from museum specimens that do not have relevant environmental data, e.g. forest cover. In such cases, recent occurrence records may be temporally matched to recent environmental data in order create a mask of values outside of observed ranges. This mask can be applied to the SDM to remove those geographic areas that are unsuitable for the species.

**IMPLEMENTATION**

In wallace, a data-driven determination of thresholds for masking is implemented using recent records with a simple, conservative methodology that can be used for many species with limited recent records, and also will prove useful for processing of expert range maps or other pre-existing range estimates that do not take into account human modifications of the environment.  Dated occurrence records can be temporally matched with the dated environmental rasters (e.g. forest cover) to obtain values per occurrence.  The values at occurrence records are shown in a results table, where the user can decide what quantile of environmental value to use as bounds to mask a model for a given year to produce an estimate of the species' range for that year that takes into account the value threshold.   

These analyses require the following data:

SDM: This can be a continuous or thresholded sdm. You can make the model in wallace or upload the sdm in in the User SDM component.

Recent environmental data: For example, forest cover data, that include in the filename the year of the data.  

Dated occurrence records: To use this module, you must have already uploaded a user-specified csv file for your species that includes a column “year” with the years for occurrence data. These years should match the environmental data that you upload.

**REFERENCES**

Merow, C., P. J. Galante, C. Babich Morrow, J. M. Kass, A. Moore, B. E. Gerstner, V. Grisales-Betancur, G. E. Pinilla- Buitrago, E. A. Noguera-Urbano, J. Velasquez-Tibatá, R. P. Anderson, M. E. Blair. Operationalizing expert knowledge in species' range estimates using diverse data types: the R package maskRangeR. https://cmerow.github.io/maskRangeR/
