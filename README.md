# cloud_QA
Various ideas to ease the pain of QA'ing imagery for cloud

The functions in this repository are parts of a processing stream to extract band/index values from processed, cloud QA'd USGS Landsat scenes.

Imagery needs to have been previously downloaded (e.g. <http://earthexplorer.usgs.gov/>) and processed per DPaW current practices using CSIRO scripts. These scripts compile the individual band .tif files into an ERMapper raster (.ers format), correct them to TOA (digital numbers not reflectance) and project them to utm crs. Naming convention is l5ut11382m_080199_USG_utm50pre.ers. 

Certain pattern matching does occur throughout these functions so if hacking this for other scenarios/imagery/formats then these matches will have to be altered. Sensor number ("l5..."), path/row ("...11382...") and scene date ("...080199...") are all important extracted variables as is "...pre.ers". Bear these in mind when naming own imagery and conversion to other purposes will be easier.

Imagery prior to these functions being run is stored in a pathrow/scene date folder structure. For e.g. C:/.../11382/19990108

***jpegR.R***

This function checks scene date folders and either:

1. returns a list of scene dates with imagery present, or
2. performs a check with a shapefile (ESRI) of individual sites against a pre-made cloud mask (see cloud_fmask repo and fmaskR.R) for cloud then returns a list of scene dates of imagery not impacted by cloud.

Scene dates are then used to create small jpegs covering the location of individual sites and stores them in separate folders (by individual site) for visual cloud QA. QA is done by simply deleting any jpeg that shows cloud impacting the site.

The option of chosing the precheck against cloudmasks exists as sometimes the fmask process can create type 1 or type 2 errors. It is up to the user to determine how many type 1 errors (false positives) would impact the quality of a dense time series analysis. Complete control is to chose the alternative whereby the user will have to delete every cloudy jpeg during the visual QA. There's always a trade off!

See "Using jpegR.Rmd" for full details and usage.

***extractR.R***

This function obtains the dates of the left over jpegs, deems these as QA'd for cloud and then extracts band/index values per site for scenes from those dates and compiles them into one csv file. The csv file will be structured with sites across the columns and dates as the rows. NA's indicate cloud or a site falling completely within a stripe of missing data (e.g. Landsat 7 post 2003).

See Using "extractR.Rmd" for full details and usage.

***checkR.R*** and ***fmaskcheckR.R*** are both helper functions written to be included in jpegR.R. In perhaps a moment of madness (remains to be seen) these have been put into "processingFunctions.R" source script which is called from within jpegR.R. Overcomplification? Perhaps but development was occuring quickly and I didn't want to start having to update little functions present in larger functions with small changes and bug fixes.

***leapRfn.R*** and ***shpSplitRfn.R*** are also small helper functions but were not included into the source script.

See separate "Using....Rmd" docos for details if needed.
