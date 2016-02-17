################################################################################
# When imagery is processed with the Python scripts it incorrectly names the 
# date folder when it is a leap year. This function checks through a path/row 
# folder and compares the date of imagery to the folder date name to find 
# mismatches. It then subtracts a day and renames the incorrectly named folder.
# Argument required is the full path to the path/row you wish to process.
#
# Bart Huntley

rm(list=ls())


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

imdir <- imdir <- "W:\\usgs\\113075"

leapR(imdir)
