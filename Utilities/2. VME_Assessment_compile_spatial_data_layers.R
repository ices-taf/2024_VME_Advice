##------------------------------------------------------------------------------------
# script to obtain and calculate all spatial data layers for rmarkdown
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
# load current EU VME closures 
clos_eu <- st_read(geopack,layer='EU_Closures_ImplementingAct') %>% 
  st_make_valid()

##-------------------------------------------------------------------------------------

# obtain polygons for geographic areas

# Combined ICES ecoregions and EU waters (based on member EEZs)
shape_ices_EEZ <- st_read(geopack,layer='EUVME_Assessment_Extent') %>% 
  st_make_valid()
##-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# get depths

# # EUVME depth based on EmodNet 2018
# load(paste(pathdir,"1-Input data/Region_depth_EUVME.RData",sep="/"))
# # some of the scripts use depth$csquare, some depth$csquares, so making both - this needs cleaning up in the code.
# depth$csquare <- depth$csquares

# Get 400-800m zone
Reg_depth <- st_read(geopack,layer='EU_Depth_400_800') %>% 
  st_make_valid()

### ~~~ This is deprecated but keeping for now in case need to make a new depth zone

# IREG <- subset(depth,!(depth$min_depth_emodnet > 800))
# IREG <- subset(IREG, !(IREG$max_depth_emodnet  < 400)) 
# IREG$within <- 1  # if TRUE
# depth <- cbind(depth,IREG[match(depth$csquare,IREG$csquare),c("within")])
# colnames(depth)[ncol(depth)] <- "within"
# depth$within[is.na(depth$within)] <- 0 # if not TRUE
# depth <- cbind(depth,bargrid@data[match(depth$csquares,bargrid@data$csquares),c(2,3)])
#depth_EUVME <- depth

#depth <- read.csv(paste(pathdir_nogit,"EU/Depth/Extended_ICES_area_EMODNET_GEBCO_Combined.csv",sep="/"))

# depth <- depth %>% 
#   mutate(within = case_when(Depth_min < 800 & Depth_max > 400 ~ 1, TRUE ~ 0)) %>% 
#   left_join(bargrid@data, by=c("csquares")) %>% 
#   select(-c(area_sqkm, long, lat))

# create polygon of 400-800m depths 
# Reg_w <- subset(bargrid, bargrid@data$csquares %in% depth$csquares)
# Reg_w <- cbind(Reg_w,depth[match(Reg_w@data$csquares,depth$csquares),c("within")])
# colnames(Reg_w@data)[ncol(Reg_w@data)] <- "within"
# Reg_w <- subset(Reg_w,Reg_w@data$within == 1)
# Reg_w <- raster::aggregate(Reg_w)
# Reg_w <- gUnaryUnion(Reg_w)
# Reg_depth   <- st_as_sf(Reg_w)
# Reg_depth <-  st_transform(Reg_depth, "EPSG:4326")  
#st_write(Reg_depth,"Shapefiles/Reg_depth_EUVME_redo_01122022.shp")

##-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# load VME polygons from last assessment (assyearprev) all and within EU DR
#-------------------------------------------------------------------------------------
# some of the previous polygons have been dissolved and have no IDs, there is a 
# conditional loop to break them into polygons and give them ids because it is 
# needed further along the script

## All polygons
scen11_prev  <- st_read(geopackORIG,layer=paste("Scenario1_option1",assyearprev,sep='_')) %>% 
  st_make_valid()

scen12_prev  <- st_read(geopackORIG,layer=paste("Scenario1_option2",assyearprev,sep='_')) %>% 
  st_make_valid()

scen21_prev  <- st_read(geopackORIG,layer=paste("Scenario2_option1",assyearprev,sep='_')) %>% 
  st_make_valid()

scen22_prev  <- st_read(geopackORIG,layer=paste("Scenario2_option2",assyearprev,sep='_')) %>% 
  st_make_valid()

scen23_prev  <- st_read(geopackORIG,layer=paste("Scenario2_option3",assyearprev,sep='_'))  %>% 
  st_make_valid()


## Within EU Depth range
scen11_prev_dpt  <- st_read(geopackORIG,layer=paste("Sc1Op1_dzone_",assyearprev,sep='_')) %>% 
  st_make_valid()

scen12_prev_dpt  <- st_read(geopackORIG,layer=paste("Sc1Op2_dzone_",assyearprev,sep='_')) %>% 
  st_make_valid()

scen21_prev_dpt  <- st_read(geopackORIG,layer=paste("Sc2Op1_dzone_",assyearprev,sep='_')) %>% 
  st_make_valid()


scen22_prev_dpt  <- st_read(geopackORIG,layer=paste("Sc2Op2_dzone_",assyearprev,sep='_')) %>% 
  st_make_valid()

scen23_prev_dpt  <- st_read(geopackORIG,layer=paste("Sc2Op3_dzone_",assyearprev,sep='_'))  %>% 
  st_make_valid()

#-------------------------------------------------------------------------------------  
# load VME polygons from datacallyear
#-------------------------------------------------------------------------------------
scen11  <- st_read(dsn= paste0(pathdir, "/2-Data processing/VME_polygons_2024"), layer="Scenario1_option1") %>% 
  st_make_valid()

scen12  <- st_read(dsn= paste0(pathdir, "/2-Data processing/VME_polygons_2024"), layer="Scenario1_option2") %>% 
  st_make_valid()

scen21  <- st_read(dsn= paste0(pathdir, "/2-Data processing/VME_polygons_2024"), layer="Scenario2_option1") %>% 
  st_make_valid()

scen22  <- st_read(dsn= paste0(pathdir, "/2-Data processing/VME_polygons_2024"), layer="Scenario2_option2") %>% 
  st_make_valid()

scen23  <- st_read(dsn= paste0(pathdir, "/2-Data processing/VME_polygons_2024"), layer="Scenario2_option3") %>% 
  st_make_valid()


## Clip polygons from the assessment year to depth zone
scen11_dpt <- scen11  %>%
                st_make_valid %>%
                select(-id) %>%
                st_intersection(Reg_depth) %>%
                st_collection_extract(type= "POLYGON") %>%
                mutate(id=row_number())
scen11_dpt %>% 
  st_make_valid()%>%
  st_write(geopack,layer=paste('Sc1Op1_dzone_',datacallyear,sep='_'),append=FALSE)


scen12_dpt <- scen12 %>%
                st_make_valid() %>%
                select(-id) %>%
                st_intersection(Reg_depth) %>%
                st_collection_extract(type= "POLYGON") %>%
                mutate(id=row_number())
scen12_dpt %>% 
  st_make_valid()%>%
  st_write(geopack,layer=paste('Sc1Op2_dzone_',datacallyear,sep='_'),append=FALSE)

scen21_dpt <- scen21 %>%
                st_make_valid() %>%
                select(-id) %>%
                st_intersection(Reg_depth) %>%
                st_collection_extract(type= "POLYGON") %>%
                mutate(id=row_number())
scen21_dpt %>% 
  st_make_valid()%>%
  st_write(geopack,layer=paste('Sc2Op1_dzone_',datacallyear,sep='_'),append=FALSE)

scen22_dpt <- scen22 %>%
                st_make_valid() %>%
                select(-id) %>%
                st_intersection(Reg_depth) %>%
                st_collection_extract(type= "POLYGON") %>%
                mutate(id=row_number())
scen22_dpt %>% 
  st_make_valid()%>%
  st_write(geopack,layer=paste('Sc2Op2_dzone_',datacallyear,sep='_'),append=FALSE)

scen23_dpt <- scen23 %>%
                st_make_valid() %>%
                select(-id) %>%
                st_intersection(Reg_depth) %>%
                st_collection_extract(type= "POLYGON") %>%
                mutate(id=row_number())
scen23_dpt %>% 
  st_make_valid()%>%
  st_write(geopack,layer=paste('Sc2Op3_dzone_',datacallyear,sep='_'),append=FALSE)

##-------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------- 
# load VME physical elements
Elements <- st_read(geopack,layer='VME_Elements') %>%
  st_make_valid()

##-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Create VMEgrid_old
#-------------------------------------------------------------------------------------
# # load all available VMEs from previous year (datacallyear - 1) or previous assessment - could be datacallyear -2
# VME <- read.csv(paste(pathdir,paste(
#   "1-Input data/VME data repository/VME observations and csquares/VME_csquares_datacall_",
#   assyearprev,".csv",sep=""),sep="/"),header=T,sep=",",row.names = NULL)
# 
# # VME <- read.csv(paste(pathdir_nogit,paste(
# #   "VME data repository/VME observations and csquares/VME_csquares_datacall_",
# #   datacallyear-2,".csv",sep=""),sep="/"),header=T,sep=",",row.names = NULL)
# 
# VME <- as.data.frame(VME)
# colnames(VME)[1] <- "CSquare"
# #VME <- VME[,-1]
# 
# # create VME spatial grid
# load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  
# VMEgrid       <- subset(bargrid,bargrid@data$csquares %in% unique(VME$CSquare))
# VMEgrid       <- cbind(VMEgrid, VME[match(VMEgrid@data$csquares,VME$CSquare), c("VME_Class")])
# colnames(VMEgrid@data)[ncol(VMEgrid)] <- "VME_Class"
# VMEgrid       <- st_as_sf(subset(VMEgrid,!(is.na(VMEgrid@data$VME_Class))))
# 
# VMEgrid_old <- VMEgrid %>% mutate(VME_Class_Lab = factor(case_when(
#                                                                     VME_Class==3  ~ "VME Habitat" ,
#                                                                     VME_Class==2  ~ "High VME Index" ,
#                                                                     VME_Class==1  ~ "Med VME Index" ,
#                                                                     VME_Class==0  ~ "Low VME Index"), 
#                                                           levels=c("VME Habitat","High VME Index","Med VME Index","Low VME Index")),
#                                   EU = case_when(lengths(st_intersects(st_centroid(VMEgrid), st_make_valid(shape_ices_EEZ))) > 0 ~ 1,
#                                                  TRUE ~0),
#                                   Depthzone = case_when(lengths(st_intersects(st_centroid(VMEgrid), st_make_valid(Reg_depth))) > 0 ~ 1,
#                                                TRUE ~0))
# VMEgrid_old %>%
#   st_write(geopack,layer=paste('VMEgrid',assyearprev,sep='_'),append=FALSE)
# 
# VMEgrid_old %>%
#   st_drop_geometry() %>%
#   write.csv(file = paste(pathdir,"2-Data processing/VMEgrid_old_data.RData",sep="/"))

VMEgrid_old <- st_read(geopackORIG, 
                       layer= paste0("VMEgrid_", assyearprev))
VMEgrid_old %>%
  st_write(geopack,layer=paste('VMEgrid',assyearprev,sep='_'), append=FALSE)

VMEgrid_old %>%
  st_drop_geometry() %>%
  write.csv(file = paste(pathdir,"2-Data processing/VMEgrid_old_data.RData",sep="/"))

#-------------------------------------------------------------------------------------
# load latest VME information (datacallyear)
VME <- read.csv(paste(pathdir,paste(
  "1-Input data/VME data repository/VME observations and csquares/VME_csquares_datacall_",
  datacallyear,"_eu.csv",sep=""),sep="/"),header=T,sep=",",row.names = NULL)
VME <- as.data.frame(VME)
#VME <- VME[,-1]

# create VME spatial grid
load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  
VMEgrid       <- subset(bargrid,bargrid@data$csquares %in% unique(VME$CSquare))
VMEgrid       <- cbind(VMEgrid, VME[match(VMEgrid@data$csquares,VME$CSquare), c("VME_Class")])
colnames(VMEgrid@data)[ncol(VMEgrid)] <- "VME_Class"
VMEgrid       <- st_as_sf(subset(VMEgrid,!(is.na(VMEgrid@data$VME_Class))))
VMEgrid_new <- VMEgrid %>%
  full_join(VMEgrid_old %>%
              st_drop_geometry() %>%
              select(csquares,VME_Class) %>%
              rename(VME_Class_Old=VME_Class),
            by='csquares') 
VMEgrid_new <- VMEgrid_new %>% 
  mutate(VME_Class_Lab = factor(case_when(
    VME_Class==3  ~ "VME Habitat" ,
    VME_Class==2  ~ "High VME Index" ,
    VME_Class==1  ~ "Med VME Index" ,
    VME_Class==0  ~ "Low VME Index"), 
    levels=c("VME Habitat","High VME Index","Med VME Index","Low VME Index")),
    EU = case_when(lengths(st_intersects(st_centroid(VMEgrid_new), st_make_valid(shape_ices_EEZ))) > 0 ~ 1,
                   TRUE ~0),
    Depthzone = case_when(lengths(st_intersects(st_centroid(VMEgrid_new), st_make_valid(Reg_depth))) > 0 ~ 1,
                          TRUE ~0),
    Change = factor(case_when(
      is.na(VME_Class_Old) ~ "New" ,
      VME_Class==VME_Class_Old  ~ "Same" ,
      VME_Class>VME_Class_Old  ~ "Up" ,
      VME_Class<VME_Class_Old  ~ "Down",
      is.na(VME_Class) & VME_Class_Old %in% c(0:3) ~ "Removed"), 
      levels=c("New","Up","Same","Down", "Removed")))
# the above gives a warning (Problem with `mutate()` column 'EU' / 'DepthZone') but output is OK 


VMEgrid_new %>%
  st_write(geopack,layer=paste('VMEgrid',datacallyear,sep='_'), append=FALSE)

VMEgrid_new %>%
  st_drop_geometry() %>%
  write.csv(file = paste(pathdir,"2-Data processing/VMEgrid_new_data.RData",sep="/"))

VMEgrid_new %>% ## new code to write csv with changes in VME index.
  st_drop_geometry() %>%
  filter(Change %in% c("New", "Up", "Down", "Removed")) %>% 
  write.csv(file = paste0(pathdir,"/2-Data processing/ChangesVME_Index.csv"))

# ## Load the extra squares that were filtered out of the 2022 data ## not sure about this data source, so not kept in
# VMEgrid_new_filtered <- st_read(geopack,layer='VMEgrid_2022_filteredout') %>% 
#    st_make_valid() %>%
#   rename(csquares=csquars) %>%
#   mutate(VME_Class_Lab=factor(case_when(Conf_Nm=="VME" ~"VME Habitat",
#                                         Conf_Nm=="Medium" ~"Med VME Index"),
#                                        levels=levels(VMEgrid_new$VME_Class_Lab)))
 
#-------------------------------------------------------------------------------------
# load Fishing layers latest year with available data (datacallyear_VMS)
#load(paste(pathdir,paste("2-Data processing/Fishing_layers",datacallyear_VMS,sep="_"),"Fishing_workspace.RData",sep="/"))

EUFootp <- st_read(geopack,layer='Footprint_all') %>% 
  st_make_valid()

# New_comb <- st_read(geopack,layer='VME_All_Gears_New_Area') %>% 
#   st_make_valid()
New_comb <- st_read(paste0(pathdir, "/2-Data processing/fisheriesdata.gpkg"), layer=paste0("new_comb_", datacallyear_VMS))
 
# New_mobile <- st_read(geopack,layer='VME_Mobile_New_Area') %>% 
#   st_make_valid()
New_mobile <- st_read(paste0(pathdir, "/2-Data processing/fisheriesdata.gpkg"), layer=paste0("new_mobile_", datacallyear_VMS))
 
# New_static <- st_read(geopack,layer='VME_Static_New_Area') %>% 
#   st_make_valid()
New_static <- st_read(paste0(pathdir, "/2-Data processing/fisheriesdata.gpkg"), layer=paste0("new_static_", datacallyear_VMS)) 

# New_SAR <- st_read(geopack,layer='VME_Assessment_SAR') %>% 
#   st_make_valid()
New_SAR <- st_read(paste0(pathdir, "/2-Data processing/fisheriesdata.gpkg"), layer=paste0("AVSAR_", datacallyear_VMS))

#-------------------------------------------------------------------------------------
# load VME database summary used for the popup tables in the rmarkdown
# source(paste(pathdir,"Utilities/VME Database summary.R",sep="/"))  # (warnings are okay)
 source(paste(pathdir,"Utilities/3. VME Database summary EU VME List.R",sep="/"), echo = TRUE)  # (warnings are okay)


# save.image(file = paste(pathdir,"2-Data processing/Map_layer_workspace.RData",sep="/"))     

