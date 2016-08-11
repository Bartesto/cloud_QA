################################################################################
# extractR
#
# Function:
# 1. copies individual site shape files to QA jpeg folders
# 2. obtains dates of remaining jpegs in QA folders
# 3. uses dates from 2. to extract data for each site shape file
# 4. writes results of 3. to a .csv file within each QA folder
# 5. combines all individual site extracts into one .csv with NA's indicating
#    data removed due to cloud or complete striping.
#
# parameters
# wkdir - working directory where your shp file is located
# imdir - directory where imagery is located to path/row level
# shp - shp file name with no extension
# shp.ID - attribute field of shp file that contains unique site IDs
# option - choice for extracted values. One of "i35", "ndvi", "b1", "b2", "b3", 
#          "b4", "b5", "b6".
# addsen - should sensor designator be added to final csv output, "y" or "n".
#
# NOTE: adding sensor to final csv adds a column and thereby may disrupt
# downstream analysis where correct columns have been indexed by number not 
# name. Leave as "n" if unsure or make sure other analysis is drawing from 
# correct columns
#
# usage
# the intention is to run jpegR prior to extractR. jpegR will create QA folders 
# of jpegs. These need to be edited for cloudy images (delete cloud affected 
# jpegs) prior to using extractR. extractR will then based on the dates of jpegs 
# left in QA folders, extract desired band/index values for each site. Results 
# are output to csv per site and saved to individual QA folders, then combined 
# in one csv, saved to working directory.
#



extractR <- function(wkdir, imdir, shp, shp.ID, option, addsen = "n"){
  
  ## fp for shp folder #########################################################
  shpdir <- paste0(wkdir, "\\", "QAshapes")
  
  ## libraries #################################################################
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  
  ## Get names of QA folders ###################################################
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
  
  ## Get full list of image folders ############################################
  allimfolds <- list.dirs(imdir)
  allimfolds <- as.Date(allimfolds, "%Y%m%d")
  allimfolds <- allimfolds[!is.na(allimfolds)]
  
  beg <- gsub("-", "", as.character(allimfolds[1]))
  end <- gsub("-", "", as.character(allimfolds[length(allimfolds)]))
  
  ## Get all sat sensor nums ###################################################
  allsats <- numeric(length(allimfolds))
  for(a in 1:length(allimfolds)){
    setwd(paste0(imdir, "\\", gsub("-", "", as.character(allimfolds)))[a])
    pre_im <- list.files(pattern = "*.pre$")
    allsats[a] <- substr(pre_im, 2, 2)
  }
  
  ## Copy shape files to relevant QA folders ###################################
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
    pattern2 <- paste0("site_", shp.i)
    to.i <- paste0(wkdir, "\\", directories[grep(pattern2, directories)])
    file.copy(from=from.i, to=to.i, recursive = FALSE, overwrite = TRUE, 
              copy.mode = TRUE)
  }
  
  ## get QA'd date folders per site ############################################
  qadlist <- vector("list", length(directories))
  names(qadlist) <- directories
  
  
  for(h in 1:length(directories)){
    setwd(paste0(wkdir, "\\", directories[h]))
    jpegs <- list.files(pattern = "*.jpeg")
    qadates <- substr(jpegs, 1, 10)
    qafolds <- str_replace_all(qadates, "[^[:alnum:]]", "")
    qadlist[[h]] <- qafolds
  }

  ## Extract values and right to csv's #########################################  
  for(j in 1:length(qadlist)){  
    setwd(paste0(wkdir, "\\", directories[j]))
    
    ## shp file biz
    shp.j <- strsplit(list.files(pattern = "*.shp"), split = "\\.")[[1]][1]
    sitesSHP <- readOGR(dsn = ".", shp.j)
    rnames <- as.character(sitesSHP@data[, shp.ID])
    rownames(sitesSHP@data) <- rnames
    namesSHP <- rownames(sitesSHP@data)
    
    ## obtain file paths for all .pre
    good.folds <- qadlist[[j]]
    good.paths <- paste0(imdir, "\\", good.folds)
    
    ## dates
    date<- as.Date(good.folds, "%Y%m%d")
    
    ## construct satellite designator
    sat <- numeric(length(good.paths))
    for(h in 1:length(good.paths)){
      setwd(good.paths[h])
      pre_im <- list.files(pattern = "*.pre$")
      sat[h] <- substr(pre_im, 2, 2)
    }
    
    ## empty df for results and options vector
    results.a <- as.data.frame(matrix(ncol=length(namesSHP), dimnames=list(NULL, namesSHP)))
    options <- c("i35", "ndvi", "b1", "b2", "b3", "b4", "b5", "b6")
    
    ## extract
    for (k in 1:length(good.folds)){                
      dir.i <- good.paths[k]
      setwd(dir.i)
      files.i <- list.files(pattern="pre.ers")#all pre.ers
      
      if(length(files.i) > 1){
        data.i <- files.i[grepl("USG", files.i)]
      } else { data.i <- list.files(pattern="pre.ers")}#preferentially USG if 2
      
      pr <- substr(data.i, 5, 9)
      
      b1R.i<-raster(data.i, band=1)
      b2R.i<-raster(data.i, band=2)
      b3R.i<-raster(data.i, band=3)
      b4R.i<-raster(data.i, band=4)
      b5R.i<-raster(data.i, band=5)
      b6R.i<-raster(data.i, band=6)
      
      ## transform shp to raster
      sitesSHPt <- spTransform(sitesSHP, crs(b1R.i))
      
      if(option == "i35"){
        b3X.i<-extract(b3R.i, sitesSHPt)
        b5X.i<-extract(b5R.i, sitesSHPt)
        m3.i <- unlist(lapply(b3X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        m5.i <- unlist(lapply(b5X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        i35.i<-(m3.i+m5.i)/2
        out<-i35.i
      } else if (option == "ndvi"){
        b3X.i<-extract(b3R.i, sitesSHPt)
        b4X.i<-extract(b4R.i, sitesSHPt)
        m3.i <- unlist(lapply(b3X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        m4.i <- unlist(lapply(b4X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        ndvi.i<-(m4.i-m3.i)/(m4.i+m3.i)
        out<-ndvi.i
      } else if (option == "b1"){
        b1X.i<-extract(b1R.i, sitesSHPt)
        m1.i <- unlist(lapply(b1X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        out<-m1.i
      } else if (option == "b2"){
        b2X.i<-extract(b2R.i, sitesSHPt)
        m2.i <- unlist(lapply(b2X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        out<-m2.i
      } else if (option == "b3"){
        b3X.i<-extract(b3R.i, sitesSHPt)
        m3.i <- unlist(lapply(b3X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        out<-m3.i
      } else if (option == "b4"){
        b4X.i<-extract(b4R.i, sitesSHPt)
        m4.i <- unlist(lapply(b4X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        out<-m4.i
      } else if (option == "b5"){
        b5X.i<-extract(b5R.i, sitesSHPt)
        m5.i <- unlist(lapply(b5X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        out<-m5.i
      } else {
        b6X.i<-extract(b6R.i, sitesSHPt)
        m6.i <- unlist(lapply(b6X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
        out<-m6.i
      }              
      
      
      results.a<- rbind(results.a, out)
      
    } 
    
    ## write out results per shp file
    results.a <- as.data.frame(results.a[-1, ])
    names(results.a) <- namesSHP
    results.a<-cbind(date, sat, results.a)
    shpname.i <- shp.j
    setwd(paste0(wkdir, "\\", directories[j]))
    write.csv(file=paste0(pr,"_", option, "_", shpname.i, "_", beg, "-", end,
                          ".csv"), x=results.a)
              
  }
  
  ## Combine all results in one data source ####################################
  setwd(wkdir)
 
  ## empty list for storage
  resultslist <- vector("list", length(directories))
  
  ## populate list with all csv data
  dpaths <- paste0(wkdir, "\\", directories)
  for(l in 1:length(dpaths)){
    setwd(dpaths[l])
    dat <- read.csv(list.files(pattern = c(option, ".csv")), 
                    stringsAsFactors = FALSE)
    dat <- dat[,c(-1, -3)]
    dat$date <- as.Date(dat$date, "%Y-%m-%d")
    resultslist[[l]] <- dat
  }
  
  
  ## combine all data into 1 csv
  if(addsen == "n"){
    alldat <- data.frame(date = allimfolds)
  } else {
    alldat <- data.frame(date = allimfolds, sensor = allsats)
  }
  
  for(m in 1:length(resultslist)){
    alldat <- left_join(alldat, resultslist[[m]], "date")
  }
  setwd(wkdir)
  write.csv(alldat, file = paste0(pr, "_", option, "_QA_", beg, "-", end,
                                  ".csv"))
  

}

wkdir <- "C:\\temp\\R_development\\wkdir"
imdir <- "C:\\temp\\R_development\\imdir"
shp <- "site_test" ##No .shp suffix
shp.ID <- "id"
option = "i35"
addsen <- "n"

extractR(wkdir, imdir, shp, shp.ID, option, addsen)
