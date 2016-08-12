fmaskR <- function(imdir, Aenv = "myenv", pyPath){
  start <- Sys.time()
  setwd(imdir)
  
  ## Helper functions
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
  
  ## Sensor specific arguments
  ref5 <- paste("gdal_merge.py -separate -of HFA -co COMPRESSED=YES -o ref.img", 
                "L*_B[1,2,3,4,5,7].TIF")
  thermal5 <- paste("gdal_merge.py -separate -of HFA -co COMPRESSED=YES -o",
                    "thermal.img L*_B6.TIF")
  
  ref7 <- paste("gdal_merge.py -separate -of HFA -co COMPRESSED=YES -o", 
                "ref.img L*_B[1,2,3,4,5,7].TIF")
  thermal7 <- paste("gdal_merge.py -separate -of HFA -co COMPRESSED=YES -o", 
                    "thermal.img L*_B6_VCID_?.TIF")
  
  ref8 <- paste("gdal_merge.py -separate -of HFA -co COMPRESSED=YES -o", 
                "ref.img LC8*_B[1-7,9].TIF")
  thermal8 <- paste("gdal_merge.py -separate -of HFA -co COMPRESSED=YES -o", 
                    "thermal.img LC8*_B1[0,1].TIF")
  
  ## Generic arguments
  envArg <- paste("activate", Aenv, "&")
  pyArg <- paste("python", pyPath)
  anglesArg <- paste("fmask_usgsLandsatMakeAnglesImage.py -m *_MTL.txt -t", 
                     "ref.img -o angles.img")
  saturationArg <- paste("fmask_usgsLandsatSaturationMask.py -i", 
                         "ref.img -m *_MTL.txt -o saturationmask.img")
  toaArg <- paste("fmask_usgsLandsatTOA.py -i ref.img -m *_MTL.txt -z", 
                  "angles.img -o toa.img")
  cloudArg <- paste("fmask_usgsLandsatStacked.py -t thermal.img -a", 
                    "toa.img -m *_MTL.txt -z angles.img -s saturationmask.img",
                    "-o cloud.img") 
  
  ## create cloud masks using fmask
  folders <- list.dirs()
  for(i in 1:length(folders)){
    setwd(paste0(imdir, "\\", folders[i]))
    
    ## message
    print(paste("Working in folder", folders[i]))
    
    ## check for existence of old MTL txt files and remove
    mtl <- list.files(pattern = "MTL.txt")
    if(length(mtl) > 0){file.remove(mtl)}
    
    ## check for existence of cloud mask
    cmask <- list.files(pattern = "cloud.img")
    if(length(cmask) == 0){
      
      ## for name of cloud mask output
      usgsname <- paste0(substr(list.files(pattern = ".pre.ers"), 1 , 9),
                         substr(list.files(pattern = ".pre.ers"), 11 , 17))
      
      ## determine if file unzipped if no unzip it
      zipped <- list.files(pattern = ".tar.gz")
      files <- list.files(pattern = ".TIF")
      if(length(files) == 0){untar(zipped)}
      
      ## get sensor for specific arguments
      sensor <- substr(zipped, 3,3)
      
      ## setup .img arguments
      # ref.img
      if (sensor == "5") {
        refimg <- paste(envArg, pyArg, ref5)  
      } else if (sensor == "7") {
        refimg <- paste(envArg, pyArg, ref7)
      } else
        refimg <- paste(envArg, pyArg, ref8)
      # thermal.img
      if (sensor == "5") {
        thermalimg <- paste(envArg, pyArg, thermal5) 
      } else if (sensor == "7") {
        thermalimg <- paste(envArg, pyArg, thermal7) 
      } else
        thermalimg <- paste(envArg, pyArg, thermal8)
      # angles.img
      anglesimg <- paste(envArg, pyArg, anglesArg)
      # saturationmask.img
      satimg <- paste(envArg, pyArg, saturationArg)
      # toa.img
      toaimg <- paste(envArg, pyArg, toaArg)
      # cloud.img
      cloudimg <- paste(envArg, pyArg, cloudArg)
      # all .img arguments in a list
      imgList <- list(refimg, thermalimg, anglesimg, satimg, toaimg, cloudimg)
      
      ## process using fmask python calls through shell
      for(k in 1:length(imgList)){
        shell(imgList[k])
      }
      
      ## clean up unzipped files
      notneeded <- list.files(pattern = ".TIF|GCP.txt") 
      file.remove(notneeded)
      
      ## clean up other img rasters - adjust if you want to keep
      xtraimg <- list.files(pattern = "angles|saturationmask|toa|thermal|ref")
      file.remove(xtraimg)
      
      ## rename .img files
      imgs <- list.files(pattern = ".img")
      newimgs <- paste(usgsname, imgs, sep = "_")
      file.rename(imgs, newimgs)
      
      ## message
      print(paste("Finished with folder", folders[i]))
      
    }
    
  }
  
  ## time stats
  end <- Sys.time()
  tot <- end - start
  
  ## completion message
  setwd(imdir)
  completedmasks <- list.files(pattern = "cloud.img", recursive = TRUE)
  
  print(paste("I have made", length(completedmasks)/2, 
              "cloud masks and it took me", round(tot, 2), "hours"))
}

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
Aenv = "myenv"
pyPath = paste0("C:\\Users\\barth\\AppData\\Local\\Continuum\\Miniconda2\\envs",
                "\\myenv\\Scripts\\fmask_expandWildcards.py")
buff <- 2000
red <- 5
green <- 4
blue <- 2
fmask <- "y"

start <- Sys.time()
fmaskR(imdir, Aenv, pyPath)
jpegR(wkdir, imdir, shp, shp.ID, buff, red, green, blue, fmask)
extractR(wkdir, imdir, shp, shp.ID, option, addsen)
end <- Sys.time()

total <- end - start
total