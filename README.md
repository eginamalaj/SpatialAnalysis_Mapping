# Spatial Analysis and Mapping with R

This file contains an example of how R can be used for spatial analysis.

The results are part of the publication:

**Malaj, E.**, Liber, K., and Morrissey, C. (2020). Spatial distribution of agricultural pesticide use and 
predicted wetland exposure in the Canadian Prairie Pothole Region. *Science of the Total Environment*, 718, 
https://doi.org/10.1016/j.scitotenv.2019.134765.

The goal in that paper was to estimate the spatial distribution of pesticide use patterns in the Canadian Prairies. Here, I only give an example on the methodology for spatial calculations using herbicides. In the paper, two other pesticide groups (i.e., fungicides and insecticides) were also included.

To run this analysis the following files are needed from the `/data` file:
1. A csv files with the data `data_herb` (here amound of herbicides used for each crop)
2. Shapefile of provincial boundaries for the Canadian Prairies `prov_bound`
3. landUseRaster is a `.RData` file (lu_rs) as a `RasterBrick` where each layer represent % for one crop

To generate Pesticide Use Density (PUD) for herbicides % crop were multiplied with the amount of herbicides per crop and then summed to generate a map of the Prairies like presented below.


![Herbicide](https://user-images.githubusercontent.com/54320408/94349046-3f7aa600-fffe-11ea-98be-c1b7b3a26933.png)
