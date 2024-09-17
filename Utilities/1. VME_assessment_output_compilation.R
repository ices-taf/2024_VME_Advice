
#-------------------------------------------------------------------------------------
# Assessment script ICES VME advice
# last update August 2024 by Karin van der Reijden
# Script based on VME assessment scripts from Anna Downies and Daniel van Denderen.
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Notes:
# The outputs are generated following the ICES VME benchmark
# workshop and ADGVME2 (December 2022). The workshop report can be found here: xxx 

# Before running the script, please download all  
# VME and VMS restricted data - these data are on the ICES Sharepoint
# they can be obtained by contacting the chairs of the ICES working  
# group on Deep Water Ecology: 
# https://www.ices.dk/community/groups/pages/wgdec.aspx

#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
# step 1 - assessment year, rlibraries and folder structure
#------------------------------------------------------------------------------------- 

# Set data call years to work with for VME and VMS data
datacallyear     <- 2024   # VME data call year with latest data
datacallyear_VMS <- 2023   # VMS data call year with latest data

# Set some other years
assyearprev <- 2022
PubYearPrevAdvice <- 2023

# Set main work/path directory  
#pathdir <- getwd()
pathdir <- "D:/VME/VME2024"

# Set path to geopackage for all related GIS data products
geopack <- paste(pathdir,"EUVME_Assessment_2022.gpkg",sep="/")
geopackORIG <- "D:/VME/ADGVME2Assessment_AD_30032023_ORIG/ADGVME2Assessment_AD_30032023/EUVME_Assessment_2022.gpkg"

# R libraries used
source(paste(pathdir,"Utilities/Libraries_VMEadvice.R",sep="/"))  


#-------------------------------------------------------------------------------------  



#-------------------------------------------------------------------------------------
# step 2 - run scenarios and options to obtain VME polygons
#-------------------------------------------------------------------------------------

### ~~~ Already run previously - these and 2020 polygons are read in from geopackage later
### ~~~ Rewrite Scenario codes to output directly to geopackage, with year affixed to filename
### ~~~ for future compatibility

# create folder for the VME datacall year
dir.create(paste(pathdir,"2-Data processing",paste("VME_polygons",datacallyear,sep="_"),sep="/"))


# run scenario 1 - option 1 & 2 (warnings are okay)
source(paste(pathdir,"Utilities/Scenario_A.R",sep="/"))
source(paste(pathdir,"Utilities/Scenario_B.R",sep="/"))

# run scenario 2 - option 1& 2 (warnings are okay)
refyear       <- 2009:datacallyear_VMS-1   # specify years to estimate sar threshold
SAR_threshold <- 0.43                  # SAR threshold value
source(paste(pathdir,"Utilities/Scenario_C.R",sep="/"))
source(paste(pathdir,"Utilities/Scenario_D.R",sep="/"))
#
# # run the combined S1-O2 + S2-O1 scenario (warnings are okay)
# # termed S2-O3 in all scripts
source(paste(pathdir,"Utilities/Scenario_E.R",sep="/"))

rm(list=setdiff(ls(), c("datacallyear","datacallyear_VMS","pathdir","pathdir_nogit", "geopack", "geopackORIG", "assyearprev", "PubYearPrevAdvice")))

#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
# step 3 - load fishable domains and obtain fishing layers 
#-------------------------------------------------------------------------------------

### ~~~ These are all read in from the geopackage - footprints are from 2021 advice
### ~~~ fished areas calculated separately and included in geopackage
### ~~~ Edit and use Neil's functions and save outputs to geopackage 
### ~~~ for future compatibility

## This part is kept in because we may once need to update the fishable domain. For now, we don't need to.
# # # load EU + NEAFC polygons with fishable domain
# # source(paste(pathdir,"Utilities/Footprint_current.R",sep="/")) # replace by the geopack-layer EUfootprint

# obtain fished area based on the latest VMS datacall
refyear_fished <- 2009:2011
newyear_fished <- (datacallyear_VMS-1):(datacallyear_VMS-3)
source(paste(pathdir,"Utilities/Fishedarea_newVMS.R",sep="/"))


#------------------------------------------------------------------------------------
# step 4 - prepare all outputs to run the rmarkdowns 
#-------------------------------------------------------------------------------------
# Note:
# The script will provide all outputs based on the data call year
# it will overwrite any previous version 
# to return to a previous year, change the data call year above and re-run the below

source(paste(pathdir,"Utilities/2. VME_Assessment_compile_spatial_data_layers.R",sep="/"))  #warnings OK
save.image(file = paste(pathdir,"2-Data processing/Map_layers_workspace.RData",sep="/"))

load(file=paste(pathdir,"2-Data processing/Map_layers_workspace.RData",sep="/"))
# compile output for table 1 of the RMarkdown
source(paste(pathdir,"Utilities/4. Compile_table1_underlyingdata.R",sep="/"))  

#------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# step 5 - run the RMD file from the working folder
#------------------------------------------------------------------------------------

#  EU_VME.RMD
rmarkdown::render("VME2024_Advice.Rmd", 
                  output_file = paste0("VME2024_EU_Advice",Sys.Date (),".html")
)




