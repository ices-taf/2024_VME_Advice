################################################
#### code to compile underlying data Table 1 ##
################################################

# load footprint workspace
#load(paste(pathdir,"2-Data processing/Map_layers_workspace.RData",sep="/"))  

# select all new c-squares
VMEgrid_old$uni  <- paste(VMEgrid_old$csquares,VMEgrid_old$VME_Class)  
VMEgrid_tot      <- VMEgrid_new 
VMEgrid_new$uni  <- paste(VMEgrid_new$csquares,VMEgrid_new$VME_Class)
VMEgrid_new      <- VMEgrid_new[!(VMEgrid_new$uni %in% VMEgrid_old$uni ),]

# load all regions
load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  

# load depths
#load(paste(pathdir,"1-Input data/Region_depth_prelim.RData",sep="/"))
depth <- read.csv(paste(pathdir,"1-Input data/eco_bathymetry_v2/Extended_ICES_area_EMODNET_GEBCO_Combined.csv",sep="/"))
names(depth)

# get area 400-800
# IREG <- subset(depth,!(depth$min_depth_emodnet  > 800))
# IREG <- subset(IREG, !(IREG$max_depth_emodnet  < 400)) 
IREG <- subset(depth,!(depth$Depth_min  > 800))
IREG <- subset(IREG, !(IREG$Depth_max  < 400)) 
IREG$within <- 1  # if TRUE
depth <- cbind(depth,IREG[match(depth$csquares,IREG$csquares),c("within")])
colnames(depth)[ncol(depth)] <- "de4_8"
depth$de4_8[is.na(depth$de4_8)] <- 0 # if not TRUE

# get area 200-400  
IREG <- subset(depth,!(depth$Depth_min  > 400))
IREG <- subset(IREG, !(IREG$Depth_max  < 200)) 
# IREG <- subset(depth,!(depth$min_depth_emodnet  > 400))
# IREG <- subset(IREG, !(IREG$max_depth_emodnet  < 200)) 
IREG$within <- 1  # if TRUE
depth <- cbind(depth,IREG[match(depth$csquares,IREG$csquares),c("within")])
colnames(depth)[ncol(depth)] <- "de2_4"
depth$de2_4[is.na(depth$de2_4)] <- 0 # if not TRUE

# IREG <- subset(depth,depth$max_depth_emodnet > 800)
IREG <- subset(depth,depth$Depth_max > 800)
IREG$within <- 1  # if TRUE
depth <- cbind(depth,IREG[match(depth$csquares,IREG$csquares),c("within")])
colnames(depth)[ncol(depth)] <- "de8"
depth$de8[is.na(depth$de8)] <- 0 # if not TRUE

depth$cat <- ifelse(depth$de4_8 ==1,"de4_8",NA)
depth$cat <- ifelse(depth$de2_4 ==1 & depth$de4_8 ==0,"de2_4",depth$cat)
depth$cat <- ifelse(depth$de8 ==1 & depth$de4_8 ==0,"de8",depth$cat)

# get depth ranges
load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  
bargrid <- subset(bargrid,bargrid@data$csquares %in% depth$csquares)
#bargrid <- cbind(bargrid,depth[match(bargrid@data$csquares,depth$csquares),c("cat","mean_depth_emodnet")])
bargrid <- cbind(bargrid,depth[match(bargrid@data$csquares,depth$csquares),c("cat","Depth_mean")])
bargrid <- subset(bargrid,!(is.na(bargrid@data$cat)))

# select all c-squares
bargrid <- cbind(bargrid,as.data.frame(VMEgrid_old)[match(bargrid@data$csquares,VMEgrid_old$csquares),c("VME_Class")])
colnames(bargrid@data)[ncol(bargrid@data)] <- "Exist_VMEs"
bargrid <- cbind(bargrid,as.data.frame(VMEgrid_new)[match(bargrid@data$csquares,VMEgrid_new$csquares),c("VME_Class")])
colnames(bargrid@data)[ncol(bargrid@data)] <- "New_VMEs"
bargrid <- cbind(bargrid,as.data.frame(VMEgrid_tot)[match(bargrid@data$csquares,VMEgrid_tot$csquares),c("VME_Class")])
colnames(bargrid@data)[ncol(bargrid@data)] <- "Tot_VMEs"

bargrid_sf <- st_as_sf(bargrid)

# get element data
grid_over    <- st_intersects(bargrid_sf,Elements)
idx          <- as.data.frame(grid_over)
idx$csquare  <- bargrid_sf$csquares[idx$row.id]
idx$element  <- Elements$type[idx$col.id]

#now get the area of overlap - subsetting – otherwise it takes too long to run
bargrid_sf_sub <- bargrid_sf[unique(idx$row.id),]
elements_sub   <- Elements[unique(idx$col.id),]
arealoverlap   <- st_intersection(bargrid_sf_sub,elements_sub)
arealoverlap$areasize <- as.numeric(st_area(st_make_valid(arealoverlap)))
arealoverlap <- subset(arealoverlap,arealoverlap$areasize > 0 )

dat          <- data.frame(csquare = unique(idx$csquare))
banks        <- subset(arealoverlap,arealoverlap$type =='Bank')
dat$banks[dat$csquare %in% unique(banks$csquares)] <- 1
corMd        <- subset(arealoverlap,arealoverlap$type =='Coral mound')
dat$corMd[dat$csquare %in% unique(corMd$csquares)] <- 1
seaMt        <- subset(arealoverlap,arealoverlap$type =='Seamount')
dat$seaMt[dat$csquare %in% unique(seaMt$csquares)] <- 1
bargrid <- cbind(bargrid,dat[match(bargrid@data$csquares,dat$csquare),
                             c("banks","corMd","seaMt")])

# now add fishing layers
grid_over    <- st_intersection(bargrid_sf,New_mobile)
grid_over$size <- as.numeric(st_area(st_make_valid(grid_over)))
grid_over <- subset(grid_over,grid_over$size >0)
bargrid@data$FareaM <- ifelse(bargrid@data$csquares %in% grid_over$csquares, 1, NA)

grid_over    <- st_intersection(bargrid_sf,New_static)
grid_over$size <- as.numeric(st_area(st_make_valid(grid_over)))
grid_over <- subset(grid_over,grid_over$size >0)
bargrid@data$FareaS <- ifelse(bargrid@data$csquares %in% grid_over$csquares, 1, NA)

grid_over    <- st_intersection(bargrid_sf,New_comb)
grid_over$size <- as.numeric(st_area(st_make_valid(grid_over)))
grid_over <- subset(grid_over,grid_over$size >0)
bargrid@data$FareaC <- ifelse(bargrid@data$csquares %in% grid_over$csquares, 1, NA)

## now add which c-squares are 100% within the old VME polygon
#tt <- st_intersection(VMEgrid_new,st_make_valid(scen11_prev))
#tt$area <- st_area(st_make_valid(tt))/10^6
#tt$frac <- as.numeric(tt$area/tt$area_sqkm)
#tt <- subset(tt,tt$frac> 0.9)
#bargrid@data$scen11[bargrid@data$csquares %in% unique(tt$csquares)] <- 1

#tt <- st_intersection(VMEgrid_new,st_make_valid(scen12_prev))
#tt$area <- st_area(st_make_valid(tt))/10^6
#tt$frac <- as.numeric(tt$area/tt$area_sqkm)
#tt <- subset(tt,tt$frac> 0.9)
#bargrid@data$scen12[bargrid@data$csquares %in% unique(tt$csquares)] <- 1

#tt <- st_intersection(VMEgrid_new,st_make_valid(scen21_prev))
#tt$area <- st_area(st_make_valid(tt))/10^6
#tt$frac <- as.numeric(tt$area/tt$area_sqkm)
#tt <- subset(tt,tt$frac> 0.9)
#bargrid@data$scen21[bargrid@data$csquares %in% unique(tt$csquares)] <- 1

#tt <- st_intersection(VMEgrid_new,st_make_valid(scen22_prev))
#tt$area <- st_area(st_make_valid(tt))/10^6
#tt$frac <- as.numeric(tt$area/tt$area_sqkm)
#tt <- subset(tt,tt$frac> 0.9)
#bargrid@data$scen22[bargrid@data$csquares %in% unique(tt$csquares)] <- 1

#tt <- st_intersection(VMEgrid_new,st_make_valid(scen23_prev))
#tt$area <- st_area(st_make_valid(tt))/10^6
#tt$frac <- as.numeric(tt$area/tt$area_sqkm)
#tt <- subset(tt,tt$frac> 0.9)
#bargrid@data$scen23[bargrid@data$csquares %in% unique(tt$csquares)] <- 1

tt <- st_intersection(VMEgrid_new,st_make_valid(clos_eu))
tt$area <- as.numeric(st_area(st_make_valid(tt))/10^6)
tt$frac <- tt$area/as.numeric(tt$area_sqkm)
tt <- subset(tt,tt$frac> 0.9)
bargrid@data$clos_eu[bargrid@data$csquares %in% unique(tt$csquares)] <- 1

grid_over    <- st_intersection(bargrid_sf,shape_ices_EEZ)
grid_over$size <- as.numeric(st_area(st_make_valid(grid_over)))
grid_over <- subset(grid_over,grid_over$size >0)
bargrid@data$EUassess <- ifelse(bargrid@data$csquares %in% grid_over$csquares, 1, NA)

table1 <- bargrid@data 

save(table1,file = paste(pathdir,"2-Data processing/Table1_underlyingdata.RData",sep="/"))
