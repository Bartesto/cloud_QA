################################################################################
# jpegR
#
# Function:
# 1. obtains list of imagery folders to access. Can be from one of 2 sources. 
#    Either from auto cloud QA'd output (fmaskcheckR) or from all available 
#    imagery output (checkR).
# 2. obtains individual site shps from QAshapes.
# 3. creates a small jpeg of the individual sites for each image date as per step
#    1, for the puposes of visual QA for cloud.
# 4. jpegs based on band combination from user input and buffered by desired
#    amount.
# 5. all outputs are placed in appropriately named folders per site in the working
#    directory.
#
# parameters
# wkdir - working directory where your shp file is located
# imdir - directory where imagery is located to path/row level
# shp - shp file name with no extension
# shp.ID - attribute field of shp file that contains unique site IDs
# buff - how much to buffer the site out (metres). Is a way to control zoom for
#        jpegs
# red - what satellite band do you require in red channel
# green - what satellite band do you require in green channel
# blue - what satellite band do you require in blue channel
# fmask - have fmask cloud masks been created (fmaskR)
#
# usage
# use this function to create small jpegs for each site to enable examination 
# for cloud/shadow. Can proceed with or without utilising cloud masks generated 
# from using fmaskR. Output is placed in individual folders per site with the 
# intention that a user will perform a visual inspection for cloud/shadow and 
# delete any affected jpegs. Edited folders will be used later to determine which 
# imagery should be used to extract index/band values from. 
#
# see html doco for more details
#
# Bart Huntley 10/08/16


jpegR <- function(wkdir, imdir, shp, shp.ID, buff, red, green, blue, fmask){
  setwd(wkdir)
  source("Z:\\DOCUMENTATION\\Software\\R\\R_Scripts\\cloudQA_extract\\processingFunctions.R")
  
  ## libraries #################################################################
  library(raster)
  library(rgdal)
  library(maptools)
  
  ## Get correct list of data ##################################################
  if(fmask == "y"){
    foldsToDo <- fmaskcheckR(wkdir, imdir, shp, shp.ID)
  } else {
    foldsToDo <- checkR(wkdir, imdir, shp, shp.ID)
  }
  
  ## Get shp names to iterate through ##########################################
  shpdir <- paste0(wkdir, "\\", "QAshapes")
  shpfiles <- list.files(path = shpdir, pattern = "*.shp")
  shpfiles <- shpfiles[!grepl("xml", shpfiles)] #xml handler
  shpnames <- unlist(strsplit(shpfiles, split = "\\."))
  shpnames <- shpnames[c(TRUE,FALSE)] #this returns the odd indexes
  goodnames <- paste0("site_", shpnames)
  
  ## Find CRS of sample raster for reproject of shp###########################
  setwd(paste0(imdir, "\\", foldsToDo[[1]][1]))
  testD <- list.files(pattern = "*pre.ers")
  testD <- testD[!grepl("xml", testD)] #xml handler
  proj <- crs(raster(testD))
  
  ## Create combo variable #####################################################
  combo <- paste0(red, green, blue)
  
  for(i in 1:length(shpnames)){
    setwd(wkdir)
    shp <- readOGR(dsn = shpdir, shpnames[i])
    shp_t <- spTransform(shp, proj)
    ext <- extent(shp_t)
    ext <- ext + buff
    beg <- foldsToDo[[i]][1]
    end <- foldsToDo[[i]][length(foldsToDo[[i]])]
    folder <- paste0("QA_jpegs_", goodnames[i], "_", beg, "-", end)
    if(!file.exists(folder)){ dir.create(folder)}
    outdir <- paste0(wkdir,"\\", folder)
    
    for(j in 1:length(foldsToDo[[i]])){
      setwd(paste0(imdir, "\\", foldsToDo[[i]][j]))
      f <- list.files(pattern = '*pre.ers')
      date <- as.Date(substring(f, 12, 17),"%d%m%y")
      jname <- paste0(date, "-", combo, ".jpeg")
      fname <- paste0(wkdir, "\\", folder, "\\", jname)
      br <- raster(f, band = red)
      br <- crop(br, ext)
      br <- stretch(br)
      bg <- raster(f, band = green)
      bg <- crop(bg, ext)
      bg <- stretch(bg)
      bb <- raster(f, band = blue)
      bb <- crop(bb, ext)
      bb <- stretch(bb)
      b <- brick(br, bg, bb)
      jpeg(filename = fname, width = 842, height = 870)
      plotRGB(b, 1, 2, 3, axes = FALSE) 
      plot(shp_t, add = TRUE, lwd = 2, border = "green")
      dev.off()
    }

    
}
}
  
  
wkdir <- "C:\\temp\\R_development\\wkdir"
imdir <- "C:\\temp\\R_development\\imdir"
shp <- "site_test" ##No .shp suffix
shp.ID <- "id"
buff <- 2000
red <- 5
green <- 4
blue <- 2
fmask <- "y"


jpegR(wkdir, imdir, shp, shp.ID, buff, red, green, blue, fmask)


