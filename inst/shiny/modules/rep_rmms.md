### **Module:** ***Metadata***

**BACKGROUND**

Over the decade of the 2010s, scientific practice increasingly emphasized documentation and reproducibility. In biodiversity science, the area of modeling species niches/distributions has advanced rapidly in this regard via the emergence of various kinds of community-driven standards (see Fitzpatrick et al. 2021 for an overview). These include checklists for data and model reporting (Feng et al. 2019), standardized metadata frameworks (RMMS, Merow et al. 2019; occCite, Owens et al., 2021), and detailed protocols for reporting (ODMAP, Zurell et al. 2020). These tools facilitate the implementation of best-practice guidelines to assess the quality of a model, indicating whether it meets minimal standards for applied biodiversity uses (Araújo et al. 2019; Sofaer et al. 2019). Heavily leveraging `ENMeval 2.0` and `rangeModelMetadata`, *Wallace* now uses Range Modeling Metadata Standards (RMMS) data objects (which also form the basis of ODMAP reporting) and allows the user to download them as a CSV file (or a ZIP file for multiple species). *Wallace* promotes documentation and downstream assessment of modeling quality by allowing users to download extensive information that includes sources of input data, methodological decisions, and results. One option for the documentation (see Module: *Download Session Code*) is a file that can be re-run in R to reproduce the analyses (if re-run on exactly the same versions of R and dependent packages). Many intermediate and advanced users of R likely will find this file useful as a template for modification. Additionally, *Wallace* now provides citations of the particular R packages (and their versions) used in a given analysis (Module: *Reference Packages*).

*Wallace* generates and provides a variety of metadata objects that facilitate documentation and reproducibility. `ENMeval 2.0` (Kass et al. 2021) now generates or populates standard metadata for model-building and evaluations, via the Range Model Metadata Standards (RMMS; Merow et al. 2021). Harnessing this functionality, *Wallace* now records the user’s methodological decisions (e.g., parameter settings) and stores them in a RMMS object. This object is available for download at any stage of the analysis.

**IMPLEMENTATION**

Metadata from the Wallace analyses can be downloaded as a zip file via the `rangeModelMetadata` package (Merow et al. 2021). The RMMS includes eight fields; authorship, studyObjective, data, dataPrep (data preparation), model, prediction, assessment, and code; to catalog the modeling process. As a Wallace analysis progresses, these fields autofill to maintain data. (Merow et al. 2019).

**REFERENCES**

Araújo, M.B., Anderson, R.P., Barbosa, A.M., Beale, C.M., Dormann, C.F., Early, R., Garcia, R.A., Guisan, A., Maiorano, L., Naimi, B., O’Hara, R.B., Zimmermann, N.E., & Rahbek, C. (2019). Standards for distribution models in biodiversity assessments. *Science Advances*, 5, 1. <a href="https://doi.org/10.1126/sciadv.aat4858" target="_blank">DOI: 10.1126/sciadv.aat4858</a>

Feng, X., Park, D.S., Walker, C., Peterson, A.T., Merow, C., & Papeş, M. (2019). A checklist for maximizing reproducibility of ecological niche models. *Nature Ecology & Evolution*, 3, 1382–1395. <a href="https://doi.org/10.1038/s41559-019-0972-5" target="_blank">DOI: 10.1038/s41559-019-0972-5</a>

Fitzpatrick, F.C., Lachmuth, S., Haydt, N.T. (2021). The ODMAP protocol: a new tool for standardized reporting that could revolutionize species distribution modeling. *Ecography*, 44(7), 1067-1070.<a href="https://doi.org/10.1111/ecog.05700" target="_blank">DOI: 10.1111/ecog.05700</a>

Kass, J.M, Muscarella, R., Galante, P.J, Bohl, C.L., Pinilla-Buitrago, G.E., Boria, R.A., Soley-Guardia, M., & Anderson, R.P. (2021). ENMeval 2.0: Redesigned for customizable and reproducible modeling of species’ niches and distributions. *Methods in Ecology and Evolution*, 12(9), 1602– 1608. <a href="https://doi.org/10.1111/2041-210X.13628" target="_blank">DOI: 10.1111/2041-210X.13628</a>

Merow, C., Maitner, B.S., Owens, H.L., Kass, J.M., Enquist, B.J., Jetz, W., Guralnick, R.P. (2019). Species’ range model metadata standards: RMMS. *Global Ecology and Biogeography*, 28(12), 1912–1924. <a href="https://doi.org/10.1111/geb.12993" target="_blank">DOI: 10.1111/geb.12993</a>

Merow, C.,  Maitner, B., Owens, H., Kass, J., Enquist, B., Guralnik, R.,  Zurrell, D., & Koenig, C. (2021).rangeModelMetadata: Provides Templates for Metadata Files Associated with Species Range Models. R package v1.0.4. <a href="https://CRAN.R-project.org/package=rangeModelMetadata" target="_blank">CRAN</a> 

Owens, H.L., Merow, C., Maitner, B.S., Kass, J.M., Barve, V., Guralnick, R.P., (2021). occCite: Tools for querying and managing large biodiversity occurrence datasets. *Ecography*, 44(8), 1228-1235. <a href="https://doi.org/10.1111/ecog.05618" target="_blank">DOI: 10.1111/ecog.05618</a>

Sofaer, H.R., Jarnevich, C.S., Pearse, I.S., Smyth, R.L, Auer, S., Cook, G.L., Edwards, T.C., Guala, G.F., Howard, T.G., Morisette, J.T., & Hamiliton, H. (2019). Development and delivery of species distribution models to inform decision-making. *BioScience*, 69(7), 544–557. <a href="https://doi.org/10.1093/biosci/biz045" target="_blank">DOI: 10.1093/biosci/biz045</a>

Zurell, D., et al. (2020). A standard protocol for reporting species distribution models. *Ecography*, 43(9), 1261–1277. <a href="https://doi.org/10.1111/ecog.04960" target="_blank">DOI: 10.1111/ecog.04960</a>
