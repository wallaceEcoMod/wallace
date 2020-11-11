### **Module:** ***User-specified Occurrences*** 

**BACKGROUND**  

Users may want to use species occurrence data that they have compiled themselves in the *Wallace* analysis. Such data must have assigned georeferences in decimal degrees (i.e., latitude/longitude coordinates).

**IMPLEMENTATION** 

Users may upload a .csv file of occurrence records with rows of localities, and the first three fields must be named "species_name" (the species name, which must be the same for all rows and should have genus and species separated by a space), "longitude", and "latitude" (in that order, with those names). Other fields can appear after these mandatory fields, but they are not used in the analysis.
