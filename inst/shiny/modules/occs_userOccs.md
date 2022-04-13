### **Module:** ***User-specified Occurrences*** 

**BACKGROUND**  

Users may want to use species occurrence data that they have compiled themselves in the *Wallace* analysis. Such data must have assigned georeferences in decimal degrees (i.e., latitude/longitude coordinates).

**IMPLEMENTATION** 

Users may upload a .csv file of occurrence records with rows of localities. The first field must be named “scientific_name” and include the species’ name in binomial nomenclature; it must have the genus and species separated by a space, with the genus capitalized. The next two fields must be “longitude”, and “latitude” (in that order, with those names). Other fields can appear after these mandatory fields, but they are not used in the analysis. Users can use other formats (different delimiter and decimal separators) by checking the box under the file selection option and filling in the prompts.
After clicking “Load”, the localities will appear on the map. The log window will indicate the species name, the uploaded file name, if any duplicate records were removed, and how many records remain. If the file contained more than one species, they will all be listed in the log window. The user can also upload multiple files in the same Wallace session. Only one species’ occurrences will be shown at a time. To see the others, toggle among species using the species drop-down menu.
