## script to create VMEgridDREU.rds

load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  

# get all long x lat at 0.25 c-square format
tt <- bargrid@data
tt$long <- round(tt$long, digits = 4)
tt$lat <- round(tt$lat, digits = 4)

tt1 <- tt
tt2 <- tt
tt3 <- tt
tt4 <- tt

tt1$long <- tt$long - 0.05/4
tt1$lat  <- tt$lat - 0.05/4
tt2$long <- tt$long +  0.05/4
tt2$lat  <- tt$lat +  0.05/4
tt3$long <- tt$long -  0.05/4
tt3$lat  <- tt$lat +  0.05/4
tt4$long <- tt$long +  0.05/4
tt4$lat  <- tt$lat - 0.05/4
tt1$coords <- paste(tt1$long,tt1$lat)
tt2$coords <- paste(tt2$long,tt2$lat)
tt3$coords <- paste(tt3$long,tt3$lat)
tt4$coords <- paste(tt4$long,tt4$lat)

# add VMEs
VME <- read.csv(paste(pathdir,paste(
  "1-Input data/VME data repository/VME observations and csquares/VME_csquares_datacall_",
  datacallyear,"_eu.csv",sep=""),sep="/"),header=T,sep=",",row.names = NULL)
VME <- as.data.frame(VME)
colnames(VME)[1] <- "CSquare"
#VME <- VME[,-1]

# create VME spatial grid
VMEgrid       <- subset(bargrid,bargrid@data$csquares %in% unique(VME$CSquare))
VMEgrid       <- cbind(VMEgrid, VME[match(VMEgrid@data$csquares,VME$CSquare), c("VME_Class")])
colnames(VMEgrid@data)[ncol(VMEgrid)] <- "VME_Class"
VMEgrid       <- subset(VMEgrid,!(is.na(VMEgrid@data$VME_Class)))

## Cut VMEgrid to EU depth range 400-800m
DREU <- st_read(geopackORIG, "EU_Depth_400_800")

##-- first remove all gridcells not near the DR
VMEgridDR <- data.frame()
for(iPoly in 1:17){
  DREUPoly <- DREU[iPoly,]
  a <- st_bbox(DREUPoly)
  VMEg <- subset(VMEgrid@data, long >= a[1]-0.5 & long <= a[3]+0.5)
  VMEg <- subset(VMEg, lat >= a[2]-0.5 & lat <= a[4]+0.5)
  VMEgridDR <- rbind(VMEgridDR, VMEg)
}

VMEgrid2 <- subset(VMEgrid, VMEgrid@data$csquares %in% VMEgridDR$csquares)
VMEgridsf <- st_as_sf(VMEgrid2)
VMEgridsf <- st_transform(VMEgridsf, crs=3035)
VMEgridsf <- st_buffer(VMEgridsf, dist=-50)
VMEgridsf <- st_transform(VMEgridsf, crs=4326)
VMEDR <- st_intersection(DREU, VMEgridsf) # obtain VMEs in the EU Depth range

VMEgrid  <- subset(VMEgrid, VMEgrid@data$csquares %in% VMEDR$csquares)
saveRDS(VMEgrid, file=paste0(pathdir, "/1-Input data/VMEgridDREU.rds"))

VMEgridsf <- st_as_sf(VMEgrid)#, 
st_write(VMEgridsf, dsn=paste0(pathdir, "/1-Input data"), layer="VMEgridDREU", 
         driver="ESRI Shapefile", append=FALSE)

## create new "quar_grid" that only covers the EU depth range
nam <- c("south","north1","north2","north3","north4")
DREU <- st_read(geopackORIG, "EU_Depth_400_800")


for (iGrid in 1:5){
  load(paste(pathdir,paste(paste("1-Input data/Region_0.25_csquare_grid",nam[iGrid],sep="_"),".RData",sep=""),sep="/"))
  
  QG_grid <- quar_grid[1,]
  QG_grid <- QG_grid[-1,]
  for(iPoly in 1:17){
    DREUPoly <- DREU[iPoly,]
    a <- st_bbox(DREUPoly)
    QGa <- subset(quar_grid, long >= a[1]-0.5 & long <= a[3]+0.5)
    QGa <- subset(QGa, lat >= a[2]-0.5 & lat <= a[4]+0.5)
    QG_grid <- rbind(QG_grid, QGa)
  } # end iPoly
  saveRDS(QG_grid, file=paste0(pathdir, "/1-Input data/QG_grid", iGrid, ".rds"))
} # end iGrid
  
QG_grid <- readRDS(file=paste0(pathdir, "/1-Input data/QG_grid1.rds"))
for(iGrid in 2:5){
  QG_gridx <- readRDS(file=paste0(pathdir, "/1-Input data/QG_grid", iGrid, ".rds"))
  
  if(nrow(QG_gridx) > 0) {
    QG_gridx <- spTransform(QG_gridx, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    QG_grid <- rbind(QG_grid, QG_gridx)
  }
  if(nrow(QG_gridx)== 0){
    print(paste0("Grid ", iGrid, " does not provide new grid cells."))
  }
}
## because data is only QG_grid1, we'll keep to this data set.

