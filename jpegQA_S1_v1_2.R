################################################################################
# Stage 1 of jpeg query process
# 
# This code takes user supplied shapefiles of plots/sites to create small
# jpeg views for visual QA of cloud impacts. 
# 
# The user will create a folder in the working/analysis directory called 
# "QAshapes" and store all plot/site shapefiles for analysis here.
#
# The user must also decide on a band combination. Although the inputs are 
# labelled red, green and blue you must choose what band is displayed in these 
# colour spaces. For example it could be 321 or 543 for argument sake. The user
# must decide on a band combination and then perform a stretch (in ERMapper) 
# that will work for all Landsat sensors. This is to obtain the min and max 
# values for each of the colour spaces (i.e. rmin, rmax inputs etc) which will 
# then be applied to each scene for a consistent look.
# 
# The code then:
#   * creates a folder per shapefile
#   * uses each shapefile to create a "zoomed" in view of all available/processed
#   USGS Landsat imagery (the degree of "zoom" can be controlled by the buffer 
#   input (m))
#   * the stretch is applied and the result saved to jpeg
#   
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
wkdir <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\cloud_QA"
imdir <- "W:\\usgs\\113075"
shpdir <- paste0(wkdir, "\\", "QAshapes")

# Buffer
buff <- 1500

red <- 5
green <- 4
blue <- 3
combo <- "543"
rmin <- 5
rmax <- 210
gmin <- 10
gmax <- 100
bmin <- 10
bmax <- 95

#################IMAGE NAMES/FOLDERS############################################
setwd(imdir)

# get everything
allfiles <- list.files(recursive = TRUE)

# get only those that end in .pre and in date folders
result <- allfiles[grepl("*pre.ers", allfiles)]
ind1 <- grepl("^[[:digit:]]",result)
result <- result[ind1]

# # get only image date folders file paths
# result <- result[!grepl("ecw*", result)]#remove display folder

# limit to just few folders for testing
#result <- result[1:100]

# get just folders
fold <- substr(result, 1, 8)

# get beginning and end dates for names
beg <- fold[1]
end <- fold[length(fold)]

################SHAPE FILE NAMES################################################
setwd(shpdir)

shpfiles <- list.files(pattern = "*.shp")
shpfiles <- shpfiles[!grepl("xml", shpfiles)] #xml handler
shpnames <- unlist(strsplit(shpfiles, split = "\\."))
shpnames <- shpnames[c(TRUE,FALSE)] #this returns the odd indexes

################################################################################



for(i in 1:length(shpnames)){
  setwd(wkdir)
  shp <- readOGR(dsn = shpdir, shpnames[i])
  ext <- extent(shp)
  ext <- ext + buff
  folder <- paste0("QA_jpegs_stretchtest_", shpnames[i], "_", beg, "-", end)
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
    br <- stretch(br, minv = rmin, maxv = rmax)
    bg <- raster(f, band = green)
    bg <- crop(bg, ext)
    bg <- stretch(bg, minv = gmin, maxv = gmax)
    bb <- raster(f, band = blue)
    bb <- crop(bb, ext)
    bb <- stretch(bb, minv = bmin, maxv = bmax)
    b <- brick(br, bg, bb)
    jpeg(filename = fname, width = 842, height = 870)
    plotRGB(b, 1, 2, 3, axes = FALSE) 
    plot(shp, add = TRUE, lwd = 2, border = "green")
    dev.off()
  }
  
}

