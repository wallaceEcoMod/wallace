### **Module:** ***Bio-ORACLE Marine Variables***

**BACKGROUND**

Bio-ORACLE provides global environmental layers for the marine realm, designed for species distribution and niche modeling in the ocean (Tyberghein et al. 2012; Assis et al. 2018). Version 3 (Assis et al. 2024) derives its layers from Copernicus Marine Service model products and offers, for each variable, long-term statistics (mean, minimum, maximum, range, long-term minimum and maximum) over decadal "baseline" periods (2000-2020), at the sea surface and at the sea floor (benthic layers at the mean, minimum or maximum depth of each cell). A static terrain dataset supplies bathymetry and seafloor topography. All layers are global at 0.05 degree (3 arcmin, ~5.5 km) resolution and are undefined (NA) over land, so that downstream *Wallace* steps (background extent, masking, modeling) are restricted to the ocean.

Variables available in this module (Bio-ORACLE codes in parentheses):

Sea water temperature (thetao), Sea water salinity (so), Chlorophyll concentration (chl), Dissolved oxygen (o2), Nitrate (no3), Phosphate (po4), Silicate (si), pH (ph), Phytoplankton carbon (phyc), Dissolved iron (dfe), Sea water speed (sws), Sea water direction (swd); surface only: Mixed layer depth (mlotst), Sea ice concentration (siconc), Sea ice thickness (sithick), Air temperature (tas); terrain: Bathymetry mean/min/max (bathymetry_mean, bathymetry_min, bathymetry_max), Slope (slope), Terrain ruggedness index (terrain_ruggedness_index), Topographic position index (topographic_position_index).

Layer names in *Wallace* follow the pattern `variable_statistic_depth` (e.g. `thetao_mean_surf`, `so_range_mean`); terrain layers keep their Bio-ORACLE name (e.g. `bathymetry_mean`).

**IMPLEMENTATION**

This module relies on the R package `rerddap` (Chamberlain & Mendelssohn 2024) to request layers from the Bio-ORACLE ERDDAP server (https://erddap.bio-oracle.org/erddap/). Users choose the variables, the depth layer (sea surface or benthic), the statistic and the resolution. Resolutions coarser than the native 0.05 degree are obtained server-side with an ERDDAP "stride" (every n-th cell is sampled; no averaging is applied), which keeps downloads fast, and the grid is snapped to whole-degree boundaries. The last available baseline decade (2010-2020) is used. Downloaded layers are cached in the session's temporary folder, so re-loading the same selection in a session is fast.

Because occurrences on land have NA values in every Bio-ORACLE layer, they are removed when the variables are loaded (the log window reports how many). Choose the sea-surface depth layer for pelagic species and a benthic layer for species living on or near the sea floor.

The "Batch" option will load the environmental variables selected at the chosen resolution for all the species that have occurrence data uploaded.

**REFERENCES**

Assis, J., Fernández Bejarano, S.J., Salazar, V.W., Schepers, L., Gouvêa, L., Fragkopoulou, E., Leclercq, F., Vanhoorne, B., Tyberghein, L., Serrão, E.A., Verbruggen, H., & De Clerck, O. (2024). Bio-ORACLE v3.0. Pushing marine data layers to the CMIP6 Earth System Models of climate change research. *Global Ecology and Biogeography*, 33(4), e13813. <a href="https://doi.org/10.1111/geb.13813" target="_blank">DOI: 10.1111/geb.13813</a>

Assis, J., Tyberghein, L., Bosch, S., Verbruggen, H., Serrão, E.A., & De Clerck, O. (2018). Bio-ORACLE v2.0: Extending marine data layers for bioclimatic modelling. *Global Ecology and Biogeography*, 27(3), 277-284. <a href="https://doi.org/10.1111/geb.12693" target="_blank">DOI: 10.1111/geb.12693</a>

Chamberlain, S., & Mendelssohn, R. (2024). rerddap: General Purpose Client for 'ERDDAP' Servers. R package. <a href="https://CRAN.R-project.org/package=rerddap" target="_blank">CRAN</a>

Tyberghein, L., Verbruggen, H., Pauly, K., Troupin, C., Mineur, F., & De Clerck, O. (2012). Bio-ORACLE: a global environmental dataset for marine species distribution modelling. *Global Ecology and Biogeography*, 21(2), 272-281. <a href="https://doi.org/10.1111/j.1466-8238.2011.00656.x" target="_blank">DOI: 10.1111/j.1466-8238.2011.00656.x</a>
