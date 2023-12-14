### **Module:** ***Session Code***

**BACKGROUND**

Over the decade of the 2010s, scientific practice increasingly emphasized documentation and reproducibility. In biodiversity science, the area of modeling species niches/distributions has advanced rapidly in this regard via the emergence of various kinds of community-driven standards (see Fitzpatrick et al. 2021 for an overview). These include checklists for data and model reporting (Feng et al. 2019), standardized metadata frameworks (RMMS, Merow et al. 2019; occCite, Owens et al., 2021), and detailed protocols for reporting (ODMAP, Zurell et al. 2020). These tools facilitate the implementation of best-practice guidelines to assess the quality of a model, indicating whether it meets minimal standards for applied biodiversity uses (Araújo et al. 2019; Sofaer et al. 2019). Heavily leveraging `ENMeval 2.0` and `rangeModelMetadata`, *Wallace* now uses Range Modeling Metadata Standards (RMMS) data objects (which also form the basis of ODMAP reporting) and allows the user to download them as a CSV file (or a ZIP file for multiple species). *Wallace* promotes documentation and downstream assessment of modeling quality by allowing users to download extensive information that includes sources of input data, methodological decisions, and results. One option for the documentation (see Module: *Download Session Code*) is a file that can be re-run in R to reproduce the analyses (if re-run on exactly the same versions of R and dependent packages). Many intermediate and advanced users of R likely will find this file useful as a template for modification. Additionally, *Wallace* now provides citations of the particular R packages (and their versions) used in a given analysis (Module: *Reference Packages*).

Via the *Session Code* module, the user can download files that document the analyses run in a given *Wallace* session (including executable code that can reproduce them).  This functionality supports reproducible science (Merow et al. 2019; Zurell et al., 2020; Fitzpatrick et al. 2021).

**IMPLEMENTATION**

Here, the user can download documented code that corresponds to the analyses run in the current session of *Wallace*. Multiple formats are available for download (.Rmd [R Markdown], .pdf, .html, or .doc). The .Rmd format is an executable R script file that will reproduce the analysis when run in an R session; it is composed of plain text and R code “chunks”. Extended functionality for R Markdown files exists in RStudio. Simply open the .Rmd in RStudio, click on “Run” in the upper-right corner, and run chunk by chunk or all at once. To learn more details, see the RStudio tutorial.

The *Wallace* session code .Rmd file is composed of a chain of code chunks with module functions that are for internal use in *Wallace*. Each of these functions corresponds to a single module that the user ran during the session. To see the internal code for these module functions, click on the links in the .Rmd file. Users are encouraged to write custom code in the .Rmd directly to modify their analysis, and even modify the module function code to further customize.

If more than one species was modeled, the markdown session code is organized by each species’ analysis. If any multispecies analysis was run (from the **Characterize Environmental Space** component), it will appear at the end of the session code, after the individual species analysis. 

Notes
To generate a PDF of your session code, it is essential you have a working version of TeX installed. For Mac OS, download MacTeX <a href="https://tug.org/mactex/" target="_blank">here</a>. For Windows, please perform the following steps:  

1. Download and Install MiKTeX <a href="https://miktex.org/download" target="_blank">here</a>.  
2. Run `Sys.getenv("PATH")` in RStudio. This command returns the path where RStudio is trying to find pdflatex.exe. In Windows (64-bit), it should return `C:\Program Files\MiKTeX 2.9\miktex\bin\x64\pdflatex.exe`. If pdflatex.exe is not located in this location, RStudio gives the error code “41”.  
3. To set the path variable, run the following in RStudio:  
`d <- "C:/Program Files/MiKTeX 2.9/miktex/bin/x64/"`  
`Sys.setenv(PATH=paste(Sys.getenv("PATH"), d, sep=";"))`  


**REFERENCES**

Araújo, M.B., Anderson, R.P., Barbosa, A.M., Beale, C.M., Dormann, C.F., Early, R., Garcia, R.A., Guisan, A., Maiorano, L., Naimi, B., O’Hara, R.B., Zimmermann, N.E., & Rahbek, C. (2019). Standards for distribution models in biodiversity assessments. *Science Advances*, 5, 1. <a href="https://doi.org/10.1126/sciadv.aat4858" target="_blank">DOI: 10.1126/sciadv.aat4858</a>

Feng, X., Park, D.S., Walker, C., Peterson, A.T., Merow, C., & Papeş, M. (2019). A checklist for maximizing reproducibility of ecological niche models. *Nature Ecology & Evolution*, 3, 1382–1395. <a href="https://doi.org/10.1038/s41559-019-0972-5" target="_blank">DOI: 10.1038/s41559-019-0972-5</a>

Fitzpatrick, F.C., Lachmuth, S., & Haydt, N.T. (2021). The ODMAP protocol: a new tool for standardized reporting that could revolutionize species distribution modeling. *Ecography*, 44(7), 1067-1070.<a href="https://doi.org/10.1111/ecog.05700" target="_blank">DOI: 10.1111/ecog.05700</a>

Kass, J.M, Muscarella, R., Galante, P.J, Bohl, C.L., Pinilla-Buitrago, G.E., Boria, R.A., Soley-Guardia, M., & Anderson, R.P. (2021). ENMeval 2.0: Redesigned for customizable and reproducible modeling of species’ niches and distributions. *Methods in Ecology and Evolution*, 12(9), 1602– 1608. <a href="https://doi.org/10.1111/2041-210X.13628" target="_blank">DOI: 10.1111/2041-210X.13628</a>

Merow, C., Maitner, B.S., Owens, H.L., Kass, J.M., Enquist, B.J., Jetz, W., & Guralnick, R.P. (2019). Species’ range model metadata standards: RMMS. *Global Ecology and Biogeography*, 28(12), 1912–1924. <a href="https://doi.org/10.1111/geb.12993" target="_blank">DOI: 10.1111/geb.12993</a>

Owens, H.L., Merow, C., Maitner, B.S., Kass, J.M., Barve, V., & Guralnick, R.P., (2021). occCite: Tools for querying and managing large biodiversity occurrence datasets. *Ecography*, 44(8), 1228-1235. <a href="https://doi.org/10.1111/ecog.05618" target="_blank">DOI: 10.1111/ecog.05618</a>

Sofaer, H.R., Jarnevich, C.S., Pearse, I.S., Smyth, R.L, Auer, S., Cook, G.L., Edwards, T.C., Guala, G.F., Howard, T.G., Morisette, J.T., & Hamiliton, H. (2019). Development and delivery of species distribution models to inform decision-making. *BioScience*, 69(7), 544–557. <a href="https://doi.org/10.1093/biosci/biz045" target="_blank">DOI: 10.1093/biosci/biz045</a>

Zurell, D., et al. (2020). A standard protocol for reporting species distribution models. *Ecography*, 43(9), 1261–1277. <a href="https://doi.org/10.1111/ecog.04960" target="_blank">DOI: 10.1111/ecog.04960</a>
