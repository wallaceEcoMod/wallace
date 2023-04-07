### **Module:** ***Query PaleobioDB Database*** 

**BACKGROUND**

The Paleobiology Database (<a href="paleobiodb.org" target="_blank">paleobiodb.org</a>) is a non-governmental, non-profit public resource for paleontological data. It has been organized and operated by a multi-disciplinary, multi-institutional, international group of paleobiological researchers. Its purpose is to provide global, collection-based occurrence and taxonomic data for organisms of all geological ages, as well as data services to allow easy access to data for independent development of analytical tools, visualization software, and applications of all types. The Database’s broader goal is to encourage and enable data-driven collaborative efforts that address large-scale paleobiological questions (Varela et al. 2011).

**IMPLEMENTATION**

This module relies on the R package `paleobioDB` to download occurrence data from the Quaternary (Holocene and the Pleistocene with its four ages; Varela et al. 2015). Users need to specify the scientific name and the maximum number of occurrences to download.

The records used in downstream analyses in Wallace are filtered to remove those without georeferences (latitude/longitude coordinates) and that have exact duplicate coordinates as other records (including the same number of decimal places). The "Occurrences” tab displays all the filtered records with several key fields: name, longitude, latitude, country, early_age, and late_age. The original and filtered records are available for download as .csv files in the “Save” tab.

**REFERENCES**

Varela, S., González-Hernández, J., Sgarbi, L. F., Marshall, C., Uhen, M. D., Peters, S., & McClennen, M. (2015). paleobioDB: an R package for downloading, visualizing and processing data from the Paleobiology Database. *Ecography*, 38(4), 419–425. <a href="https://doi.org/10.1111/ecog.01154" target="_blank">DOI: 10.1111/ecog.01154</a>

Varela, S., Lobo, J. M., & Hortal, J. (2011). Using species distribution models in paleobiogeography: A matter of data, predictors and concepts. *Palaeogeography, Palaeoclimatology, Palaeoecology*, 310(3), 451–463. <a href="https://doi.org/10.1016/j.palaeo.2011.07.021" target="_blank">DOI: 10.1016/j.palaeo.2011.07.021</a>
