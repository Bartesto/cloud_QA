################################################################################
# Stage 2 of jpeg query process

# This code is to be run after Stage 1 is complete. The starting point should
# be a working directory that has separate folders containing cloud free jpegs
# and a "QAshapes" folder with the original plots/sites used to create the 
# jpegs. 
# 
# The code: 
#   copies the QA shapefiles into the appropriate folder
#   assembles QA'd imagery folder list
#   uses this folder list to run a version of stackR for index/band extraction
#   and writes this to csv file
#   and finally amalgamates all individual csv files into one complete csv
#   file in the working directory
#
# The final output csv contains all available image dates that have been 
# processed and corresponding extracts for all QA'd plots/sites. Where cloud
# has been found an NA will be present.
#
#                             NOTE
# At this stage it is recommended that after the initail run of this process
# on the majority of imagery, further updates are run on only image dates not
# contained in the initial run.
#
# Bart Huntley


rm(list=ls())

library(raster)
library(rgdal)
library(maptools)
library(stringr)
library(sp)



#################INPUTS#########################################################
## Directories
wkdir <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\cloud_QA"
imdir <- "W:\\usgs\\113075"
shpdir <- paste0(wkdir, "\\", "QAshapes")
zone <- 50
shp.ID <- "Bore"
option <- "i35"
pr <- 11375

#################Get names of QA folders########################################
setwd(wkdir)

list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      full.names=FALSE, ignore.case=FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}

directories <- list.dirs()
qind <- grep("QA_jpegs", directories)
directories <- directories[qind]

#################Get full list of image folders#################################
setwd(wkdir)


allimfolds <- list.dirs(imdir)
allimfolds <- as.Date(allimfolds, "%Y%m%d")
allimfolds <- allimfolds[!is.na(allimfolds)]

beg <- gsub("-", "", as.character(allimfolds[1]))
end <- gsub("-", "", as.character(allimfolds[length(allimfolds)]))


#################Copy shape files to relevant QA folders########################
setwd(shpdir)

shpfiles <- list.files(pattern = "*.shp")
shpfiles <- shpfiles[!grepl("xml", shpfiles)]
shpnames <- unlist(strsplit(shpfiles, split = "\\."))
shpnames <- shpnames[c(TRUE,FALSE)] #this returns the odd indexes


for(i in 1:length(shpnames)){
  shp.i <- shpnames[i]
  pattern <- paste0("^", shp.i, "\\.")
  shps.i <- list.files(pattern = pattern)
  from.i <- paste0(shpdir,"\\", shps.i)
  pattern2 <- paste0(shp.i, "_")
  to.i <- paste0(wkdir, "\\", directories[grep(pattern2, directories)])
  file.copy(from=from.i, to=to.i, recursive = FALSE, overwrite = TRUE, 
            copy.mode = TRUE)
}


#################Extract site data from QA'd date folders#######################
qadlist <- vector("list", length(directories))


for(h in 1:length(directories)){
  setwd(paste0(wkdir, "\\", directories[h]))
  jpegs <- list.files(pattern = "*.jpeg")
  qadates <- substr(jpegs, 1, 10)
  qafolds <- str_replace_all(qadates, "[^[:alnum:]]", "")
  qadlist[[h]] <- qafolds
}

# Run check to make sure leap year folder names are correct
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

for(j in 1:length(directories)){  
  setwd(paste0(wkdir, "\\", directories[j]))
  prj<-CRS(paste("+proj=utm +zone=",zone,
                 " +south +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", 
                 sep=""))
  shp.j <- strsplit(list.files(pattern = "*.shp"), split = "\\.")[[1]][1]
  
  ###
  sitesSHP <- readOGR(dsn = ".", shp.j)
  rnames <- as.character(sitesSHP@data[, shp.ID])
  rownames(sitesSHP@data) <- rnames
  namesSHP <- rownames(sitesSHP@data)#Cool. Finds row names from IDvar supplied 
  
  
  # 
  # sitesSHP <- readShapePoly(shp.j, IDvar = shp.ID, proj4string = prj)
  # namesSHP<-rownames(sitesSHP@data)#Cool. Finds row names from IDvar supplied   
  
  #obtain file paths for all .pre
  good.folds <- qadlist[[j]]
  good.paths <- paste0(imdir, "\\", good.folds)
  
  date<- as.Date(good.folds, "%Y%m%d")
  
  #construct satellite designator
  sat <- numeric(length(good.paths))
  for(h in 1:length(good.paths)){
    setwd(good.paths[h])
    pre_im <- list.files(pattern = "*.pre$")
    sat[h] <- substr(pre_im, 2, 2)
  }
  results.a <- as.data.frame(matrix(ncol=length(namesSHP), dimnames=list(NULL, namesSHP)))
  options <- c("i35", "ndvi", "b1", "b2", "b3", "b4", "b5", "b6")
  preferred <- paste0("USG_utm", zone, "pre.ers")
  
  
  
  for (k in 1:length(good.folds)){                
    dir.i <- good.paths[k]
    setwd(dir.i)
    files.i <- list.files(pattern="pre.ers")#all pre.ers
    
    if(length(files.i) > 1){
      data.i <- files.i[grepl("USG", files.i)]
    } else { data.i <- list.files(pattern="pre.ers")}#preferentially USG if 2
    
    b1R.i<-raster(data.i, band=1)
    b2R.i<-raster(data.i, band=2)
    b3R.i<-raster(data.i, band=3)
    b4R.i<-raster(data.i, band=4)
    b5R.i<-raster(data.i, band=5)
    b6R.i<-raster(data.i, band=6)
    if(option == "i35"){
      b3X.i<-extract(b3R.i, sitesSHP)
      b5X.i<-extract(b5R.i, sitesSHP)
      m3.i <- unlist(lapply(b3X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
      m5.i <- unlist(lapply(b5X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
      i35.i<-(m3.i+m5.i)/2
      out<-i35.i
    } else if (option == "ndvi"){
      b3X.i<-extract(b3R.i, sitesSHP)
      b4X.i<-extract(b4R.i, sitesSHP)
      m3.i <- unlist(lapply(b3X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
      m4.i <- unlist(lapply(b4X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
      ndvi.i<-(m4.i-m3.i)/(m4.i+m3.i)
      out<-ndvi.i
    } else if (option == "b1"){
      b1X.i<-extract(b1R.i, sitesSHP)
      m1.i <- unlist(lapply(b1X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
      out<-m1.i
    } else if (option == "b2"){
      b2X.i<-extract(b2R.i, sitesSHP)
      m2.i <- unlist(lapply(b2X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
      out<-m2.i
    } else if (option == "b3"){
      b3X.i<-extract(b3R.i, sitesSHP)
      m3.i <- unlist(lapply(b3X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
      out<-m3.i
    } else if (option == "b4"){
      b4X.i<-extract(b4R.i, sitesSHP)
      m4.i <- unlist(lapply(b4X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
      out<-m4.i
    } else if (option == "b5"){
      b5X.i<-extract(b5R.i, sitesSHP)
      m5.i <- unlist(lapply(b5X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
      out<-m5.i
    } else {
      b6X.i<-extract(b6R.i, sitesSHP)
      m6.i <- unlist(lapply(b6X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
      out<-m6.i
    }              
    
    
    results.a<- rbind(results.a, out)
    
  }       
  #setwd(dir)
  results.a <- as.data.frame(results.a[-1, ])
  names(results.a) <- namesSHP
  results.a<-cbind(date, sat, results.a)
  #shpname.i <- str_split(shp.j, "\\.")[[1]][1]
  shpname.i <- shp.j
  setwd(paste0(wkdir, "\\", directories[j]))
  write.csv(file=paste0(pr,"_", option, "_", shpname.i, "_", beg, "-", end, ".csv"), 
            x=results.a)
}


#################Combine all results in one data source#########################
setwd(wkdir)
library(dplyr)

resultslist <- vector("list", length(directories))

dpaths <- paste0(wkdir, "\\", directories)

for(l in 1:length(dpaths)){
  setwd(dpaths[l])
  dat <- read.csv(list.files(pattern = c(option, ".csv")), stringsAsFactors = FALSE)
  dat <- dat[,c(-1, -3)]
  dat$date <- as.Date(dat$date, "%Y-%m-%d")
  resultslist[[l]] <- dat
}



alldat <- data.frame(date = allimfolds)

for(m in 1:length(resultslist)){
  alldat <- left_join(alldat, resultslist[[m]], "date")
}
setwd(wkdir)
write.csv(alldat, file = paste0(pr, "_", option, "_QA_", beg, "-", end, ".csv"))
