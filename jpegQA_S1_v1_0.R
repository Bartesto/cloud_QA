################################################################################
# Stage 1 of jpeg query process
# 
# This code takes user supplied shapefiles of plots/sites to create small
# jpeg views for visual QA of cloud impacts. 
# 
# The user will create a folder in the working/analysis directory called 
# "QAshapes" and store all plot/site shapefiles for analysis here.
# 
# The code then:
#   creates a folder per shapefile
#   uses each shapefile to create a "zoomed" in view of all available/processed
#   USGS Landsat imagery
#   the degree of "zoom" can be controlled by the buffer input (m)
#
# The proceedure from this point is to use Windows Explorer and the extra large
# icon view to asses each jpeg for cloud. Any jpeg with cloud can be deleted 
# from this folder. Remaining cloud free jpegs will then be used for Stage 2 of 
# the jpeg query process.
#
# Bart Huntley


rm(list=ls())

library(raster)
library(rgdal)
library(maptools)



#################INPUTS#########################################################
## Directories
wkdir <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\clouds"
imdir <- "W:\\usgs\\113075"
shpdir <- paste0(wkdir, "\\", "QAshapes")
# This directory could change needs to be cleaned - grows quickly
tmpdir <- "C:\\Users\\barth\\AppData\\Local\\Temp\\RtmpiqxKEG\\raster"
# Buffer
buff <- 1000

red <- 3
green <- 2
blue <- 1
combo <- "321"

#################IMAGE NAMES/FOLDERS############################################
setwd(imdir)

# get everything
allfiles <- list.files(recursive = TRUE)
# get only those that end in .pre
result <- allfiles[grepl("*pre.ers", allfiles)]
# get only image date folders file paths
result <- result[!grepl("ecw*", result)]#remove display folder
## limit to just few folders for testing
#result <- result[1:5]
#get just folders
fold <- substr(result, 1, 8)

################SHAPE FILE NAMES################################################
setwd(shpdir)

shpfiles <- list.files(pattern = "*.shp")
shpnames <- unlist(strsplit(shpfiles, split = "\\."))
shpnames <- shpnames[c(TRUE,FALSE)] #this returns the odd indexes

################################################################################



for(i in 1:length(shpnames)){
  setwd(wkdir)
  shp <- readOGR(dsn = shpdir, shpnames[i])
  ext <- extent(shp)
  ext <- ext + buff
  folder <- paste0("QA_jpegs_", shpnames[i], "_", Sys.Date())
  if(!file.exists(folder)){ dir.create(folder)}
  outdir <- paste0(wkdir,"\\", folder)
  for(j in 1:length(result)){
    setwd(paste0(imdir, "\\", fold[j]))
    f <- list.files(pattern = '*pre.ers')
    date <- as.Date(substring(f, 12, 17),"%d%m%y")
    jname <- paste0(date, "-", combo, ".jpeg")
    fname <- paste0(wkdir, "\\", folder, "\\", jname)
    br <- raster(f, band = red)
    br <- crop(br, ext)
    bg <- raster(f, band = green)
    bg <- crop(bg, ext)
    bb <- raster(f, band = blue)
    bb <- crop(bb, ext)
    b <- brick(br, bg, bb)
    jpeg(filename = fname)
    plotRGB(b, 1, 2, 3, axes = FALSE) 
    plot(shp, add = TRUE, lwd = 1, border = "green")
    dev.off()
    tmp.list <- list.files(path = tmpdir, full.names = TRUE)
    file.remove(tmp.list)#Dangerous be careful
  }
  
}

