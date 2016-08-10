################################################################################
# checkR
#
# Function:
# 1. creates QAshapes folder
# 2. ensures leap year folders are correctly named
# 3. splits shp files into single site shapes
# 4. builds a list of image date folders to pass on to jpegR function
#   4a. checkR does not perform auto cloud detection and returns all available 
#       imagery folders
# 5. also outputs "DateSiteList" RData file
#
# parameters
# wkdir - working directory where your shp file is located
# imdir - directory where imagery is located to path/row level
# shp - shp file name with no extension
# shp.ID - attribute field of shp file that contains unique site IDs
#
# usage
# used in the jpegR function to collate a list of all available imagery folders
# to pass on to jpegR function to produce jpegs per site for cloud QA. Created 
# as an alternate option to creating fmask cloud masks and allowing purely 
# visual QA of all imagery per site. Could be important if fmask cloud masks 
# contain type 1 or type 2 errors.
#
# this function is contained in the source file "processingFunctions.R" for use 
# in the jpegR function.
#
# see html doco for more details
#
# Bart Huntley 10/08/16


checkR <- function(wkdir, imdir, shp, shp.ID){
  ## Create QAshapes working folder for shapefiles #############################
  setwd(wkdir)
  if(!file.exists("QAshapes")){ dir.create("QAshapes")}
  shpdir <- paste0(wkdir, "\\", "QAshapes")
  
  
  ## libraries #################################################################
  library(raster)
  library(rgdal)
  library(maptools)
  
  ## QA leap file folders for correct date #####################################
  leapR <- function(imdir){
    setwd(imdir)
    allfiles <- list.files(recursive = TRUE)
    result <- allfiles[grepl("*pre.ers", allfiles)]
    result <- result[!grepl("^[a-zA-Z]", result)]
    #date for folder
    fold <- substr(result, 1, 8)
    fdate <- as.character(as.Date(fold, "%Y%m%d"))
    #date for image
    ldate <- as.character(as.Date(substr(result, 21, 26), "%d%m%y"))
    #find mismatch
    bad.fold.dates <- setdiff(fdate, ldate)
    #correct the folder names and path
    corr.fold.dates <- as.Date(bad.fold.dates, "%Y-%m-%d") - 1
    corr.fold <- gsub("-", "", as.character(corr.fold.dates))
    new.name <- paste0(imdir, "\\", corr.fold)
    #old folder names and path to correct
    bad.fold <- gsub("-", "", as.character(bad.fold.dates))
    old.name <- paste0(imdir, "\\", bad.fold)
    #rename folders
    file.rename(old.name, new.name)
  }
  
  leapR(imdir)
  
  
  ## Handle shape file to single parts (sites) #################################
  shpSplitR <- function(wkdir, shpdir, shp, shp.ID){
    setwd(wkdir)
    data <-  readOGR(dsn = ".", shp)
    unique <- unique(data@data[,shp.ID])
    sites <- as.character(unique)
    setwd(shpdir)
    for(i in 1:length(sites)){
      tmp <- data[data@data[,shp.ID] == sites[i], ]
      writeOGR(tmp, dsn=getwd(), sites[i], driver="ESRI Shapefile",
               overwrite_layer=TRUE)
    }
  }
  
  shpSplitR(wkdir, shpdir, shp, shp.ID)
  
  
  ## Get shp names to iterate through ##########################################
  shpfiles <- list.files(path = shpdir, pattern = "*.shp")
  shpfiles <- shpfiles[!grepl("xml", shpfiles)] #xml handler
  shpnames <- unlist(strsplit(shpfiles, split = "\\."))
  shpnames <- shpnames[c(TRUE,FALSE)] #this returns the odd indexes
  
  ## Build empty list as long as number of sites ###############################
  DateSiteList <- vector("list", length(shpnames))
  goodnames <- paste0("site_", shpnames, "_dates")
  names(DateSiteList) <- goodnames
  
  ## Get all folders with imagery ##############################################
  allfiles <- list.files(path = imdir, pattern = "*pre.ers", recursive = TRUE)
  
  ## Get only those that end in .pre and in date folders #######################
  ind <- grepl("^[[:digit:]]", allfiles)
  allfiles <- allfiles[ind]
  allFolds <- substr(allfiles, 1, 8)
  for(i in 1:length(shpnames)){
    DateSiteList[[i]] <- allFolds
  }
  setwd(wkdir)
  save(DateSiteList, file = "DateSiteList.RData")
  return(DateSiteList)
}



wkdir <- "C:\\temp\\R_development\\wkdir"
imdir <- "C:\\temp\\R_development\\imdir"
shp <- "site_test" ##No .shp suffix
shp.ID <- "id"

checked <- checkR(wkdir, imdir, shp, shp.ID)










