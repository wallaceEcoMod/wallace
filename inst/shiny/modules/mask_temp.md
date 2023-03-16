### **Module: Temporal Extract**

**BACKGROUND**

When creating SDMs, best practice dictates using environmental predictors that temporally match the occurrence records. However, this is not always realistic. Often, many occurrence records are from museum specimens that do not have relevant environmental data, e.g. forest cover or do not have information on collection date. In such cases, recent occurrence records may be temporally matched to recent environmental data in order to create a mask of values outside of observed ranges (Gavrutenko et al. 2021). This mask can be applied to the SDM to remove those geographic areas that are unsuitable for the species.  Such a data-driven determination of thresholds for masking can be used for many species with limited recent records, and may also prove useful for processing of expert range maps or other pre-existing range estimates that do not take into account human modifications of the environment. 

An expert may have knowledge of habitat requirements but only possess a rough estimate of the quality of habitat required to distinguish suitable from unsuitable areas, e.g., the critical threshold of the percentage of forest cover in a particular area (grid cell) necessary to qualify as suitable. In this case, known presence locations of the species can be used to guide the expert’s estimate of this threshold by using forest cover data available in those pixels (time-matched to correspond to the species’ observation). Reliability of the data-driven thresholding will depend in part on how well occurrences match environmental data, in terms of resolution (georeferencing error vs. spatial resolution) and timing (Merow et al. 2022).


**IMPLEMENTATION**

This module relies on the R package `maskRangeR` to implement two approaches to generating thresholds and masking SDMs using raster datasets that delimit suitable/occupied locations from unsuitable/unoccupied ones: 

1. In 'expert-driven thresholding', an expert with a priori knowledge of requirements for a given filter to distinguish suitable conditions from unsuitable ones can apply that filter to mask an SDM using this module using raster dataset(s) describing the conditions. For example, an expert may estimate that a grid cell must contain at least 50% forest to be suitable for a forest-obligate species. 

2. In 'data-driven thresholding',  dated occurrence records can be temporally matched with dated environmental rasters (e.g. forest cover) to obtain values per occurrence. The values at occurrence records are shown in the Results tab, where the user can decide what quantile of environmental value to use as lower and upper bounds to mask a model for a given year and produce an estimate of the species' range for that year that takes into account the value threshold (Gavrutenko et al. 2021). 

These analyses require the following data:

SDM: This can be a continuous or thresholded sdm. You can use a model generated in `wallace` (Wallace SDM, Module: *Map Prediction* in **Component: Visualize Model Results**), a model transferred in time or space in `wallace` (Transferred SDM, **Component: Transfer**), or use a model uploaded by the User (User uploaded SDM, this component). You can also make an additional mask on a previously Masked SDM. Note that if you wish to make more than one mask, the SDM tab will default to Masked SDM to update the mask. This means the previous mask will be lost if it is not saved before proceeding. 

Recent environmental data: In raster file format. Must include in the filename the year of the dataset.  

Dated occurrence records: To use the data-driven analysis part of this module, you must also upload in **Component: Occ Data** a user-specified csv file for your species that includes a column “year” denoting the years for your occurrence data. These years should match the years for the environmental data that you upload in this module.  
 
Users can compare how different choices of thresholds may result in different calculations of range size and changes in range size over time and view a line graph and table of those changes in **Component: Indicators** in the *Calculate Change over Time* module.

---

**CONTEXTO**

Al crear SDM, las mejores prácticas dictan el uso de predictores ambientales que coincidan temporalmente con los registros de ocurrencia. Sin embargo, esto no siempre es realista. Frecuentemente, muchos registros de ocurrencia provienen de especímenes de museo que no tienen datos ambientales relevantes, p. ej., cobertura forestal, o no tienen información sobre la fecha de recolección. En tales casos, los registros de ocurrencia recientes pueden coincidir temporalmente con datos ambientales recientes para crear una máscara de valores fuera de los rangos observados (Gavrutenko et al. 2021). Esta máscara se puede aplicar al SDM para eliminar aquellas áreas geográficas que no son aptas para la especie. Tal determinación basada en datos de los umbrales para el enmascaramiento se puede utilizar para muchas especies con registros recientes limitados, y también puede resultar útil para el procesamiento de mapas de distribución de expertos u otras estimaciones de distribución preexistentes que no tienen en cuenta las modificaciones humanas del medio ambiente.

Un experto puede tener conocimiento de los requisitos del hábitat, pero solo poseer una estimación aproximada de la calidad del hábitat requerida para distinguir las áreas adecuadas de las no adecuadas, por ejemplo, el umbral crítico del porcentaje de cobertura forestal en un área particular (cuadrícula) necesario para calificar como adecuado. En este caso, las ubicaciones de presencia conocidas de la especie se pueden usar para guiar la estimación del experto de este umbral mediante el uso de datos de cobertura forestal disponibles en esos píxeles (coincidencia temporal para corresponder a la observación de la especie). La confiabilidad del umbral basado en datos dependerá en parte de qué tan bien las ocurrencias coincidan con los datos ambientales, en términos de resolución (error de georreferenciación versus resolución espacial) y tiempo (Merow et al. 2022).

**IMPLEMENTACIÓN**

Este módulo se basa en el paquete de R `maskRangeR` para implementar dos enfoques para generar umbrales y enmascarar SDM utilizando conjuntos de datos ráster que delimitan las ubicaciones adecuadas/ocupadas de las inadecuadas/desocupadas:

1. En la 'definición de umbrales impulsada por expertos', un experto con conocimiento a priori de los requisitos de un filtro determinado para distinguir las condiciones adecuadas de las inadecuadas puede aplicar ese filtro para enmascarar un SDM utilizando este módulo usando conjuntos de datos ráster que describen las condiciones. Por ejemplo, un experto puede estimar que una celda de cuadrícula debe contener al menos un 50 % de bosque para ser adecuada para una especie obligada por el bosque.

2. En el 'umbral basado en datos', los registros de ocurrencia fechados pueden coincidir temporalmente con rásteres ambientales fechados (p. ej., cobertura forestal) para obtener valores por ocurrencia. Los valores en los registros de ocurrencia se muestran en la pestaña Results, donde el usuario puede decidir qué cuantil de valor ambiental usar como límites inferior y superior para enmascarar un modelo para un año determinado y producir una estimación del rango de la especie para ese año que tiene en cuenta el valor umbral (Gavrutenko et al. 2021).

Estos análisis requieren los siguientes datos:

SDM: Puede ser un SDM continuo o con umbral. Se puede usar un modelo generado en `wallace` (Wallace SDM, Module: *Map Prediction* en **Component: Visualize Model Results**), un modelo transferido en el tiempo o en el espacio en `wallace` (Transferred SDM, **Component: Transfer**), o usar un modelo cargado por el Usuario (User uploaded SDM, este componente). También puede hacer una máscara adicional en un SDM previamente enmascarado. Tenga en cuenta que si desea crear más de una máscara, la pestaña SDM se establecerá de forma predeterminada en SDM enmascarado para actualizar la máscara. Esto significa que la máscara anterior se perderá si no se guarda antes de continuar.

Datos ambientales recientes: En formato de archivo ráster. Debe incluir en el nombre del archivo el año del conjunto de datos.

Registros de ocurrencia fechados: para usar la parte de análisis basado en datos de este módulo, también debe cargar en **Component: Occ Data** un archivo csv especificado por el usuario para su especie que incluye una columna "año" que indica los años para sus datos de ocurrencia. Estos años deben coincidir con los años de los datos ambientales que carga en este módulo.
 
Los usuarios pueden comparar cómo las diferentes opciones de umbrales pueden resultar en diferentes cálculos del tamaño del rango y los cambios en el tamaño del rango a lo largo del tiempo y ver un gráfico de líneas y una tabla de esos cambios en **Componente: Indicators** en el Módulo *Calculate Change over Time*.

---

**REFERENCES**

Gavrutenko, M., Gerstner, B.E., Kass, J.M., Goodman, S.M. & Anderson, R.P. (2021). Temporal matching of occurrence localities and forest cover data helps improve range estimates and predict climate change vulnerabilities. *Global Ecology and Conservation*, 27, e01569. <a href="https://doi.org/10.1016/j.gecco.2021.e01569" target="_blank">DOI:10.1016/j.gecco.2021.e01569</a>

Merow, C., Galante, P.J., Kass, J.M., Aiello-Lammens, M., Babich Morrow, C., Gerstner, B.E., Grisales-Betancur, V., Moore, A., Noguera-Urbano, E.A., Pinilla- Buitrago, G.E., Velasquez-Tibatá, J., Anderson, R.P., Blair, M.E. (2022). Operationalizing expert knowledge in species' range estimates using diverse data types. *Frontiers of Biogeography*, 14(2), e53589. <a href="https://doi.org/10.21425/F5FBG53589" target="_blank">DOI:10.21425/F5FBG53589</a>
