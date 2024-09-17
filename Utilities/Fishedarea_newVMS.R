#
# estimate fished area based on latest VMS data  - this might be a too heavy calculation with all ecoregions
# 
# load grid
load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  

# load depths
#load(paste(pathdir,"1-Input data/Region_depth_prelim.RData",sep="/"))
depth <- read.csv(paste(pathdir,"1-Input data/eco_bathymetry_v2/Extended_ICES_area_EMODNET_GEBCO_Combined.csv",sep="/")) # 1646903 obs

# get region max > 200 m and min < 1000
depth <- subset(depth, !(Depth_max  < 200)) # 1301346 obs
depth <- subset(depth, !(Depth_min  > 1000)) # 331580 obs
Reg_w <- subset(bargrid, bargrid@data$csquares %in% depth$csquares)

## Create sf and validate all polygons
regwsf <- st_as_sf(Reg_w)
regwsf <- st_make_valid(regwsf)
regwsf <- st_transform(regwsf, 4326)

# get fishing data - mobile and static
vmsreg1  <- readRDS(paste(pathdir,paste("1-Input data/VMS data repository/All_VMS_datacall",datacallyear_VMS,".rds",sep=""),sep="/"))
vmsreg2  <- readRDS(paste(pathdir,paste("1-Input data/VMS data repository/SARdata_2009_2022.rds",sep=""),sep="/"))
#vmsreg <- read.csv(paste(pathdir_nogit, paste("VMS data repository/adhoc_VMS_datacall",datacallyear_VMS,".csv",sep=""),sep="/"))

## Merge fishing data sources
nam_fished <- c(paste("Static",2009:2022,sep="_"))
vmsreg1 <- vmsreg1[, c("c.square", nam_fished)]
vmsreg <- merge(vmsreg1, vmsreg2, all.x=T, by="c.square")

# get area fished reference years
# get c-sq with mobile fishing
nam_fished      <- c(paste("SAR_total",refyear_fished,sep="_"))
indexcol_fished <- which(names(vmsreg) %in% nam_fished) 
vmsreg$mobref   <- rowSums(vmsreg[indexcol_fished],na.rm=T)
vmsreg$mobref[vmsreg$mobref > 0] <- 1
vmsreg$mobref[is.na(vmsreg$mobref)] <- 0

# get c-sq with static fishing  
nam_fished <- c(paste("Static",refyear_fished,sep="_"))
indexcol_fished <- which(names(vmsreg) %in% nam_fished) 
vmsreg$statref <- rowSums(vmsreg1[indexcol_fished],na.rm=T)
vmsreg$statref[vmsreg$statref > 0] <- 1
vmsreg$statref[is.na(vmsreg$statref)] <- 0

# get combined
vmsreg$combref <- vmsreg$statref + vmsreg$mobref
vmsreg$combref[vmsreg$combref > 0] <- 1

# get area fished latest years
# get c-sq with mobile fishing
nam_fished      <- c(paste("SAR_total",newyear_fished,sep="_"))
indexcol_fished <- which(names(vmsreg) %in% nam_fished) 
vmsreg$mobnew         <- rowSums(vmsreg[indexcol_fished],na.rm=T)
vmsreg$mobnew[vmsreg$mobnew > 0] <- 1
vmsreg$mobnew[is.na(vmsreg$mobnew)] <- 0

# get c-sq with static fishing  
nam_fished <- c(paste("Static",newyear_fished,sep="_"))
indexcol_fished <- which(names(vmsreg) %in% nam_fished) 
vmsreg$statnew <- rowSums(vmsreg[indexcol_fished],na.rm=T)
vmsreg$statnew[vmsreg$statnew > 0] <- 1
vmsreg$statnew[is.na(vmsreg$statnew)] <- 0

# get combined
vmsreg$combnew <- vmsreg$statnew + vmsreg$mobnew
vmsreg$combnew[vmsreg$combnew > 0] <- 1

# get average SAR for all years combined
all_SAR <- c(paste("SAR_total",2009:(datacallyear_VMS-1),sep="_"))
indexcol_fished <- which(names(vmsreg) %in% all_SAR) 
vmsreg$AVSAR <- rowMeans(vmsreg[indexcol_fished],na.rm=T)

# combine with >200 -  <1000 m region
regwsf <- cbind(regwsf, vmsreg[match(regwsf$csquares,vmsreg$c.square), c("mobref","statref","combref",
                                                                         "mobnew","statnew","combnew", "AVSAR")])
# for mobile fishing
Ref_mobile   <- subset(regwsf, mobref == 1)
Ref_mobile   <- st_union(st_make_valid(Ref_mobile))
Ref_mobile   <- st_cast(Ref_mobile, "POLYGON")
New_mobile   <- subset(regwsf, mobnew == 1)
New_mobile   <- st_union(st_make_valid(New_mobile))
New_mobile   <- st_cast(New_mobile, "POLYGON")

# for static fishing
Ref_static   <- subset(regwsf, statref == 1)
Ref_static   <- st_union(st_make_valid(Ref_static))
Ref_static   <- st_cast(Ref_static, "POLYGON")
New_static   <- subset(regwsf, statnew == 1)
New_static   <- st_union(st_make_valid(New_static))
New_static   <- st_cast(New_static, "POLYGON")

# for combined fishing
Ref_comb   <- subset(regwsf, combref == 1)
Ref_comb   <- st_union(st_make_valid(Ref_comb))
Ref_comb   <- st_cast(Ref_comb, "POLYGON")
New_comb   <- subset(regwsf, combnew == 1)
New_comb   <- st_union(st_make_valid(New_comb))
New_comb   <- st_cast(New_comb, "POLYGON")

# for average SAR layer
AVSAR <- regwsf[,c("csquares", "AVSAR")]

# and clean
rm(Reg_w, regwsf, depth,vmsreg,indexcol_fished,nam_fished,refyear_fished,bargrid)

## Write to new geopackage, just to be sure
st_write(Ref_mobile, dsn="2-Data processing/fisheriesdata.gpkg", layer=paste0("ref_mobile_", datacallyear_VMS), append=F)
st_write(Ref_static, dsn="2-Data processing/fisheriesdata.gpkg", layer=paste0("ref_static_", datacallyear_VMS), append=F)
st_write(Ref_comb, dsn="2-Data processing/fisheriesdata.gpkg", layer=paste0("ref_comb_", datacallyear_VMS), append=F)
st_write(New_mobile, dsn="2-Data processing/fisheriesdata.gpkg", layer=paste0("new_mobile_", datacallyear_VMS), append=F)
st_write(New_static, dsn="2-Data processing/fisheriesdata.gpkg", layer=paste0("new_static_", datacallyear_VMS), append=F)
st_write(New_comb, dsn="2-Data processing/fisheriesdata.gpkg", layer=paste0("new_comb_", datacallyear_VMS), append=F)
st_write(AVSAR, dsn="2-Data processing/fisheriesdata.gpkg", layer=paste0("AVSAR_", datacallyear_VMS), append=F)
