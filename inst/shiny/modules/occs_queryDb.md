### **Module:** ***Query Database*** 
This guidance text will be updated 

**BACKGROUND**  

Over the past two decades, the worldwide biodiversity informatics community has achieved remarkable progress. Many millions of species occurrence records are now available online through various databases, including a substantial subset of records with assigned georeferences (e.g., latitude/longitude coordinates; Gaiji et al. 2013; Peterson et al. 2015; Walters and Scholes 2017). These data document the presence of a species at particular points in space and time, along with other useful metadata fields when available (e.g., institution, specimen/observation number, elevation). The origin of much of this information is specimens in research collections at natural history museums and herbaria, although newer data sources such as citizen-science initiatives are rapidly outpacing traditional ones for new records of some taxonomic groups (Sullivan et al. 2009).

**IMPLEMENTATION** 

By default, this module relies on the R package `spocc`, which provides streamlined access to many species occurrence databases, some of which aggregate data from myriad providers (e.g., individual museums or citizen-science initiatives). Currently, users can choose among three of the largest databases: <a href="http://www.gbif.org" target="_blank">GBIF</a>, <a href="http://www.vertnet.org" target="_blank">VertNet*</a>, and <a href="https://bison.usgs.gov" target="_blank">BISON</a>. Note that as implemented at present, users must choose only one of these databases, and any later download overwrites previous ones.

When GBIF has been selected, an alternative option exists to receive data source citations in addition to the records themselves. In this case, the module will use the R package `occCite` instead of `spocc`. Whereas `spocc` uses the `occ_search()` function from the `rgbif` package to perform a streamlined search of the GBIF database, the `occCite` package instead uses `occ_download()` from the `rgbif` package. While this requires the user to enter their GBIF login information (which must be set up beforehand), the search has two advantages. First, it returns a DOI that can be used to cite the downloaded dataset when publishing manuscripts and other descriptions of research results. Second, it has no hard limit on the number of occurrences that can be obtained (which is set at 100,000 in searches conducted with `occ_search()`.

For all options in this module, records used in downstream analyses in *Wallace* are filtered to remove those without georeferences (latitude/longitude coordinates) and  that have exact duplicate coordinates of other records (including the same number of decimal places). The "Occs Tbl" tab displays all the filtered records with several key fields: name, longitude, latitude, year, institutionCode, country, stateProvince, locality, elevation, and basisOfRecord (standard field names from GBIF). In contrast, the records available for download as a .csv file have all original fields and include records without georeferences.

(*) Vertnet option is not available currently because it is not supported by `spocc` at present.

**REFERENCES**

Gaiji, S., Chavan, V., Ariño, A. H., Otegui, J., Hobern, D., Sood, R., & Robles, E. (2013). Content assessment of the primary biodiversity data published through GBIF network: status, challenges and potentials. *Biodiversity Informatics*. 8: 94-172.

Peterson, A. T., Soberón, J., & Krishtalka, L. (2015). A global perspective on decadal challenges and priorities in biodiversity informatics. *BMC Ecology*. 15: 15.

Sullivan, B. L., Wood, C. L., Iliff, M. J., Bonney, R. E., Fink, D., & Kelling, S. (2009). eBird: A citizen-based bird observation network in the biological sciences. *Biological Conservation*. 142: 2282-2292.

Walters, M., and Scholes, R. J., (Eds.). (2017). The GEO Handbook on Biodiversity Observation Networks. Springer International Publishing. Link: http://link.springer.com/book/10.1007/978-3-319-27288-7



