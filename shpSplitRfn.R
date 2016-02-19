################################################################################
# shpSplitR
#
# A function that takes one shape file and splits it into many using unique
# ID's in an attribute field.
#
# Intended usage is to split a shapefile of many sites/plots into separate 
# shapefiles of each site/plot.

# Inputs are:
#   shpdir = full path to original shapefile
#   shp = name of original shapefile to split. NOTE no ".shp" suffix
#   shp.ID = field name in the attribute table to base split on
#
# Bart Huntley

rm(list=ls())


library(raster)
library(rgdal)
library(maptools)
library(sp)



#################INPUTS#########################################################

wkdir <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\cloud_QA"
imdir <- "W:\\usgs\\113075"
shpdir <- paste0(wkdir, "\\", "QAshapes")
shp <- "4Bores_60_MGA50" ##No .shp suffix
shp.ID <- "Bore"


shpSplitR <- function(shpdir, shp, shp.ID){
  setwd(shpdir)
  
  data <-  readOGR(dsn = ".", shp)
  
  unique <- unique(data@data[,shp.ID])
  sites <- as.character(unique)
  
  for(i in 1:length(sites)){
    tmp <- data[data@data[,shp.ID] == sites[i], ]
    writeOGR(tmp, dsn=getwd(), sites[i], driver="ESRI Shapefile",
             overwrite_layer=TRUE)
    }
}


shpSplitR(shpdir, shp, shp.ID)







