#-------------------------------------------------------------------------------------
# Work around of VME-database update
#-------------------------------------------------------------------------------------

## Read in VME database (version 29-08-2024)
vmedb <- read.csv(paste(pathdir,paste(
  "1-Input data/VME data repository/VME observations and csquares/VME_observations_datacall_",
  datacallyear,"_eu_29082024.csv",sep=""),sep="/"), header=T,sep=";",row.names = NULL,stringsAsFactors = FALSE)

## Change the insert date of sample "SeaRover18_540"
vmedb$InsDateTime <- ifelse(vmedb$Sample == "SeaRover18_540", "2024-03-29 17:57:10.000", vmedb$InsDateTime)

## Save VME database 
write.csv(vmedb, file = paste(pathdir,paste(
  "1-Input data/VME data repository/VME observations and csquares/VME_observations_datacall_",
  datacallyear,"_eu.csv",sep=""),sep="/"), row.names = FALSE)
