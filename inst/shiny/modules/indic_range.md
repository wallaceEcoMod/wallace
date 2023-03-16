### **Module: Calculate Area Metrics**

**BACKGROUND**

Calculating the area for range size is as simple as multiplying the number of cells in a binary raster by the resolution (in km^2^). This method is most accurate when your raster is projected in an equal-area projection, which best preserves area relationships and has units in meters instead of degrees, but you can also get an estimate from an unprojected raster here (which should be interpreted with caution).

IUCN’s extent of occurrence (EOO) is defined as the area contained within the shortest imaginary (continuous) boundary drawn to encompass all the known (current) occurrences of a taxon, excluding vagrant localities (IUCN 2022). This measure may exclude discontinuities or disjunctions within the overall distribution of a taxon (e.g., large areas of unsuitable habitat, but see AOO below). The EOO is typically measured by drawing a minimum convex polygon (MCP, also called a convex hull) around occurrence localities, but this may include many large areas of obviously unsuitable or unoccupied habitat, making a convex hull around a thresholded SDM potentially more appropriate (Kass et al. 2021). 

Within the EOO area, users can calculate the sum of 2x2 km grid cells to calculate the upper bounds of IUCN’s area of occupancy or AOO. AOO is intended to account for unsuitable or unoccupied habitats that may be included in the EOO calculations. 

It is important to follow the guidelines of the relevant IUCN Species Survival Commission Specialist Group when calculating and contributing EOO or AOO measurements to enable consistency and comparability across assessments (IUCN 2022). 

**IMPLEMENTATION**

In this module, you can calculate areas from a model generated in `wallace` (Wallace SDM, Module: *Map Prediction* in **Component: Visualize Model Results**), a model transferred in time or space in `wallace` (Transferred SDM, **Component: Transfer**), a Masked SDM (**Component: Mask Prediction**), or a model uploaded by the User (User uploaded SDM; uploaded in the **Mask Prediction** component in the *Upload User Prediction* module, see Module: *Upload User Prediction* guidance text for information on uploaded user SDMs).  
Important: These analyses require a binary/thresholded sdm. 

“Range Size” calculates the area in km squared for the species’ range as defined by a binary raster.
Users can calculate IUCN’s EOO via two options 1) MCP/convex hull around occurrence localities, 2) MCP/convex hull area of a binary SDM.
Users can calculate AOO either 1) with occurrence points, 2) from the pre-masked thresholded SDM, and 3) from the masked thresholded SDM.

To calculate EOO or AOO using the occurrence points option with an uploaded user prediction, you must have searched for and plotted species' occurrence data, or uploaded a user-specified csv file for your species in the **Occ Data** component prior to uploading the SDM. The name of the SDM raster should match the scientific name of the occurrence records, otherwise Wallace will recognize the SDM as a distinct species.

Metrics appear in the Results tab.

---

**CONTEXTO**

Calcular el área para el tamaño del rango es tan simple como multiplicar el número de celdas en un ráster binario por la resolución (en km cuadrados). Este método es más preciso cuando su ráster se proyecta en una proyección de áreas equivalentes, que preserva mejor las relaciones de área y tiene unidades en metros en lugar de grados, pero aquí también puede obtener una estimación de un ráster no proyectado (que debe interpretarse con precaución).

La extensión de ocurrencia (EOO) de la UICN se define como el área contenida dentro del límite imaginario (continuo) más corto trazado para abarcar todas las ocurrencias conocidas (actuales) de un taxón, excluyendo las localidades errantes (UICN 2022). Esta medida puede excluir discontinuidades o disyunciones dentro de la distribución general de un taxón (p. ej., grandes áreas de hábitat inadecuado, pero consulte AOO a continuación). El EOO generalmente se mide dibujando un polígono convexo mínimo (MCP, también llamado casco convexo) alrededor de las localidades de ocurrencia, pero esto puede incluir muchas áreas grandes de hábitat obviamente inadecuado o desocupado, lo que hace que un casco convexo alrededor de un SDM umbral sea potencialmente más apropiado ( Kass et al. 2021).

Dentro del área EOO, los usuarios pueden calcular la suma de celdas de cuadrícula de 2x2 km para calcular los límites superiores del área de ocupación de la UICN o AOO. AOO tiene por objeto dar cuenta de los hábitats inadecuados o desocupados que pueden incluirse en los cálculos de EOO.

Es importante seguir las directrices del Grupo de Especialistas de la Comisión de Supervivencia de Especies de la UICN pertinente al calcular y aportar mediciones de EOO o AOO para permitir la coherencia y la comparabilidad entre las evaluaciones (UICN 2022).

**IMPLEMENTACIÓN**

En este módulo, puede calcular áreas a partir de un modelo generado en `wallace` (Wallace SDM, Module: *Map Prediction* en **Component: Visualize Model Results**), un modelo transferido en el tiempo o en el espacio en `wallace` (Transferred SDM, **Component: Transfer**), un Masked SDM (**Component: Mask Prediction**) o un modelo cargado por el usuario (User uploaded SDM; cargado en el componente **Mask Prediction** en el módulo *Upload User Prediction*, consulte Módulo: *Upload User Prediction* para obtener información sobre los SDM cargados por el usuario).
 Importante: Estos análisis requieren un sdm binario/umbral.

"Tamaño del rango" calcula el área en km cuadrados para el rango de la especie según lo define un ráster binario.
Los usuarios pueden calcular el EOO de la UICN a través de dos opciones 1) MCP/casco convexo alrededor de las localidades de ocurrencia, 2) MCP/área de casco convexo de un SDM binario.
Los usuarios pueden calcular AOO 1) con puntos de ocurrencia, 2) desde el SDM con umbral preenmascarado y 3) desde el SDM con umbral enmascarado.

Para calcular EOO o AOO utilizando la opción de puntos de ocurrencia con una predicción de usuario cargada, debe haber buscado y graficado los datos de ocurrencia de especies, o debe haber cargado un archivo csv especificado por el usuario para su especie en el componente **Occ Data** antes de cargar el SDM. El nombre del ráster de SDM debe coincidir con el nombre científico de los registros de ocurrencia; de lo contrario, Wallace reconocerá al SDM como una especie distinta.

Las métricas aparecen en la pestaña Results.

---

**REFERENCES**

Galante, P.J., Chang, S., Paz, A., Aiello-Lammens, M., Gerstner, B.E., Johnson, B.A., Kass, J.M., Merow, C., Noguera-Urbano, E.A., Pinilla-Buitrago, G.E., and Blair, M.E. (2023). changeRangeR: an R package for reproducible biodiversity change metrics from species distribution estimates. *Conservation Science & Practice*, 5(1), e12863. <a href="https://doi.org/10.1111/csp2.12863" target="_blank">DOI:10.1111/csp2.12863</a> 

IUCN. (2022). Guidelines for using the IUCN red list categories and criteria. Version 15.1. IUCN Retrieved from <a href="https://www.iucnredlist.org/resources/redlistguidelines" target="_blank">www.iucnredlist.org/resources/redlistguidelines</a>

Kass, J.M., Meenan, S.I., Tinoco, N., Burneo, S.F., & Anderson, R.P. (2021). Improving area of occupancy estimates for parapatric species using distribution models and support vector machines. *Ecological Applications*, 31(1). <a href="https://doi.org/10.1002/eap.2228" target="_blank">DOI:10.1002/eap.2228</a>
