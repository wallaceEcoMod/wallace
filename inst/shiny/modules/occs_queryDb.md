### **Module:** ***Query Present Database*** 

**BACKGROUND**  

Over the past two decades, the worldwide biodiversity informatics community has achieved remarkable progress. Many millions of species occurrence records are now available online through various databases, including a substantial subset of records with assigned georeferences (e.g., latitude/longitude coordinates; Gaiji et al. 2013; Walters and Scholes 2017; Anderson et al. 2020). These data document the presence of a species at particular points in space and time, along with other useful metadata fields when available (e.g., institution, specimen/observation number, elevation). The origin of much of this information is specimens in research collections at natural history museums and herbaria, although newer data sources such as citizen-science initiatives are rapidly outpacing traditional ones for new records of some taxonomic groups (Sullivan et al. 2009).

**IMPLEMENTATION** 

By default, this module relies on the R package `spocc`, which provides streamlined access to many species occurrence databases, some of which aggregate data from myriad providers (e.g., individual museums or citizen-science initiatives) (Chamberlain et al. 2021). Currently, users can choose among two of the largest databases: <a href="http://www.gbif.org" target="_blank">GBIF</a> and <a href="http://www.vertnet.org" target="_blank">VertNet</a>. Note that as implemented at present, users must choose only one of these databases, and any later download overwrites previous ones.

When GBIF has been selected, an alternative option exists to receive data source citations in addition to the records themselves. In this case, the module will use the R package `occCite` instead of `spocc`. Whereas `spocc` uses the `occ_search()` function from the `rgbif` package to perform a streamlined search of the GBIF database (Chamberlain et al. 2021), the `occCite` package instead uses `occ_download()` from the `rgbif` package (Owens et al. 2021). While this requires the user to enter their GBIF login information (which must be set up beforehand), the search has two advantages. First, it returns a DOI that can be used to cite the downloaded dataset when publishing manuscripts and other descriptions of research results. Second, it has no hard limit on the number of occurrences that can be obtained (which is set at 100,000 in searches conducted with `occ_search()`.

For all options in this module, records used in downstream analyses in *Wallace* are filtered to remove those without georeferences (latitude/longitude coordinates) and that have exact duplicate coordinates of other records (including the same number of decimal places). The "Occurrences" tab displays all the filtered records with several key fields: scientific_name, longitude, latitude, country, state_province, locality, year, record_type, catalog_number, institution_code, elevation, (standard field names from GBIF), and uncertainty. The records are available for download as a .csv file with all original fields and include records without georeferences, or as the cleaned table shown in the “Occurrences” tab. Additionally, the user can choose to retain only records that have an estimate of the uncertainty of the georeference, which can be critical for assessing whether or not the record is of sufficient quality for a given analysis (Anderson et al. 2020).

**REFERENCES**

Anderson, R.P., Araújo, M.B., Guisan, A., Lobo, J.M., Martínez-Meyer, E., Peterson, A.T., & Soberón, J.M.. (2020). Optimizing biodiversity informatics to improve information flow, data quality, and utility for science and society. *Frontiers of Biogeography*, 12(3), e47839. <a href="https://doi.org/10.21425/F5FBG47839" target="_blank">DOI: 10.21425/F5FBG47839</a>  

Chamberlain, S., Ram, K., & Hart, T. (2021). spocc: Interface to Species Occurrence Data Sources. R package version 1.2.0. <a href="https://github.com/ropensci/spocc" target="_blank">GitHub</a>  

Gaiji, S., Chavan, V., Ariño, A.H., Otegui, J., Hobern, D., Sood, R., & Robles, E. (2013). Content assessment of the primary biodiversity data published through GBIF network: status, challenges and potentials. *Biodiversity Informatics*, 8(2), 94-172. <a href="https://doi.org/10.17161/bi.v8i2.4124" target="_blank">DOI: 10.17161/bi.v8i2.4124</a>  

Owens, H.L., Merow, C., Maitner, B.S., Kass, J.M., Barve, V., & Guralnick, R.P., (2021). occCite: Tools for querying and managing large biodiversity occurrence datasets. *Ecography*, 44(8), 1228-1235. <a href="https://doi.org/10.1111/ecog.05618" target="_blank">DOI: 10.1111/ecog.05618</a>  

Sullivan, B.L., Wood, C.L., Iliff, M.J., Bonney, R.E., Fink, D., & Kelling, S. (2009). eBird: A citizen-based bird observation network in the biological sciences. *Biological Conservation*, 142(10), 2282-2292. <a href="https://doi.org/10.1016/j.biocon.2009.05.006" target="_blank">DOI: 10.1016/j.biocon.2009.05.006</a>  

Walters, M., & Scholes, R. J. (2017). The GEO Handbook on Biodiversity Observation Networks. Springer International Publishing. <a href="https://doi.org/10.1007/978-3-319-27288-7" target="_blank">DOI: 10.1007/978-3-319-27288-7</a>  



