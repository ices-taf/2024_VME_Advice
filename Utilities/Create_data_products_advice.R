# Set data call years to work with for VME and VMS data
datacallyear     <- 2024   # VME data call year with latest data
datacallyear_VMS <- 2023   # VMS data call year with latest data
# Create a variable for the previous year
# assyearprev <- datacallyear-1
assyearprev <- 2022
PubYearPrevAdvice <- 2023

# Set main work/path directory  
#pathdir <- getwd()
pathdir <- "D:/VME/ADGVME2Assessment_AD_30032023"

# Set path to geopackage for all related GIS data products
geopack <- paste(pathdir,"EUVME_Assessment_2022.gpkg",sep="/")
geopackORIG <- "D:/VME/ADGVME2Assessment_AD_30032023_ORIG/ADGVME2Assessment_AD_30032023/EUVME_Assessment_2022.gpkg"

# R libraries used
source(paste(pathdir,"Utilities/Libraries_VMEadvice.R",sep="/"))  
library(openxlsx)
library(basemaps)
library(ggplot2)
library(terra)

### Create CSV and XLSX files of VME polygons within EU depth range.
##-- Scenario A
scenario_A <- st_read(geopack,paste('Sc1Op1_dzone_',datacallyear,sep='_'))
ScenA = data.frame()
for(iID in 1:nrow(scenario_A)){
  print(paste0("This is polygon " , iID))
  poly = subset(scenario_A, id == iID)
  poly = st_cast(poly, "POLYGON")
  if(nrow(poly) == 1){
    poly2 = st_cast(poly, "POINT")
    poly2$Polygon_Number = iID
    poly2$Coordinate_Order = 1:nrow(poly2)
    poly2$Coordinate_ID = paste(poly2$id, poly2$Coordinate_Order, sep="_")
    poly2$Longitude = round(st_coordinates(poly2)[,1], digits=3)
    poly2$Latitude = round(st_coordinates(poly2)[,2], digits=3)
    poly2 = st_drop_geometry(poly2)
    poly2$id = NULL
    ScenA = rbind(ScenA, poly2)} else {
      poly$id = paste0(iID, letters[1:nrow(poly)])
      for(iSub in 1:nrow(poly)){
        sub = poly[iSub,]
        sub = st_cast(sub, "POINT")
        sub$Polygon_Number = sub$id
        sub$Coordinate_Order = 1:nrow(sub)
        sub$Coordinate_ID = paste(sub$Polygon_Number, sub$Coordinate_Order, sep="_")
        sub$Longitude = round(st_coordinates(sub)[,1], digits=3)
        sub$Latitude = round(st_coordinates(sub)[,2], digits=3)
        sub = st_drop_geometry(sub)
        sub$id = NULL
        ScenA = rbind(ScenA, sub)}}
  } # end iID-loop
write.csv(ScenA, file=paste0(pathdir, "/data_products/ScenarioA.csv"), row.names=F)
write.xlsx(ScenA, file=paste0(pathdir, "/data_products/ScenarioA.xlsx"))

##-- Scenario B
scenario_B <- st_read(geopack,paste('Sc1Op2_dzone_',datacallyear,sep='_'))
ScenB = data.frame()
for(iID in 1:nrow(scenario_B)){
  print(paste0("This is polygon " , iID))
  poly = subset(scenario_B, id == iID)
  poly = st_cast(poly, "POLYGON")
  if(nrow(poly) == 1){
    poly2 = st_cast(poly, "POINT")
    poly2$Polygon_Number = iID
    poly2$Coordinate_Order = 1:nrow(poly2)
    poly2$Coordinate_ID = paste(poly2$id, poly2$Coordinate_Order, sep="_")
    poly2$Longitude = round(st_coordinates(poly2)[,1], digits=3)
    poly2$Latitude = round(st_coordinates(poly2)[,2], digits=3)
    poly2 = st_drop_geometry(poly2)
    poly2$id = NULL
    ScenB = rbind(ScenB, poly2)} else {
      poly$id = paste0(iID, letters[1:nrow(poly)])
      for(iSub in 1:nrow(poly)){
        sub = poly[iSub,]
        sub = st_cast(sub, "POINT")
        sub$Polygon_Number = sub$id
        sub$Coordinate_Order = 1:nrow(sub)
        sub$Coordinate_ID = paste(sub$Polygon_Number, sub$Coordinate_Order, sep="_")
        sub$Longitude = round(st_coordinates(sub)[,1], digits=3)
        sub$Latitude = round(st_coordinates(sub)[,2], digits=3)
        sub = st_drop_geometry(sub)
        sub$id = NULL
        ScenB = rbind(ScenB, sub)}}
} # end iID-loop
write.csv(ScenB, file=paste0(pathdir, "/data_products/ScenarioB.csv"), row.names=F)
write.xlsx(ScenB, file=paste0(pathdir, "/data_products/ScenarioB.xlsx"))

##-- Scenario C
scenario_C <- st_read(geopack,paste('Sc2Op1_dzone_',datacallyear,sep='_'))
ScenC = data.frame()
for(iID in 1:nrow(scenario_C)){
  print(paste0("This is polygon " , iID))
  poly = subset(scenario_C, id == iID)
  poly = st_cast(poly, "POLYGON")
  if(nrow(poly) == 1){
    poly2 = st_cast(poly, "POINT")
    poly2$Polygon_Number = iID
    poly2$Coordinate_Order = 1:nrow(poly2)
    poly2$Coordinate_ID = paste(poly2$id, poly2$Coordinate_Order, sep="_")
    poly2$Longitude = round(st_coordinates(poly2)[,1], digits=3)
    poly2$Latitude = round(st_coordinates(poly2)[,2], digits=3)
    poly2 = st_drop_geometry(poly2)
    poly2$id = NULL
    ScenC = rbind(ScenC, poly2)} else {
      poly$id = paste0(iID, letters[1:nrow(poly)])
      for(iSub in 1:nrow(poly)){
        sub = poly[iSub,]
        sub = st_cast(sub, "POINT")
        sub$Polygon_Number = sub$id
        sub$Coordinate_Order = 1:nrow(sub)
        sub$Coordinate_ID = paste(sub$Polygon_Number, sub$Coordinate_Order, sep="_")
        sub$Longitude = round(st_coordinates(sub)[,1], digits=3)
        sub$Latitude = round(st_coordinates(sub)[,2], digits=3)
        sub = st_drop_geometry(sub)
        sub$id = NULL
        ScenC = rbind(ScenC, sub)}}
} # end iID-loop
write.csv(ScenC, file=paste0(pathdir, "/data_products/ScenarioC.csv"), row.names=F)
write.xlsx(ScenC, file=paste0(pathdir, "/data_products/ScenarioC.xlsx"))

##-- Scenario D
scenario_D <- st_read(geopack,paste('Sc2Op2_dzone_',datacallyear,sep='_'))
ScenD = data.frame()
for(iID in 1:nrow(scenario_D)){
  print(paste0("This is polygon " , iID))
  poly = subset(scenario_D, id == iID)
  poly = st_cast(poly, "POLYGON")
  if(nrow(poly) == 1){
    poly2 = st_cast(poly, "POINT")
    poly2$Polygon_Number = iID
    poly2$Coordinate_Order = 1:nrow(poly2)
    poly2$Coordinate_ID = paste(poly2$id, poly2$Coordinate_Order, sep="_")
    poly2$Longitude = round(st_coordinates(poly2)[,1], digits=3)
    poly2$Latitude = round(st_coordinates(poly2)[,2], digits=3)
    poly2 = st_drop_geometry(poly2)
    poly2$id = NULL
    ScenD = rbind(ScenD, poly2)} else {
      poly$id = paste0(iID, letters[1:nrow(poly)])
      for(iSub in 1:nrow(poly)){
        sub = poly[iSub,]
        sub = st_cast(sub, "POINT")
        sub$Polygon_Number = sub$id
        sub$Coordinate_Order = 1:nrow(sub)
        sub$Coordinate_ID = paste(sub$Polygon_Number, sub$Coordinate_Order, sep="_")
        sub$Longitude = round(st_coordinates(sub)[,1], digits=3)
        sub$Latitude = round(st_coordinates(sub)[,2], digits=3)
        sub = st_drop_geometry(sub)
        sub$id = NULL
        ScenD = rbind(ScenD, sub)}}
} # end iID-loop
write.csv(ScenD, file=paste0(pathdir, "/data_products/ScenarioD.csv"), row.names=F)
write.xlsx(ScenD, file=paste0(pathdir, "/data_products/ScenarioD.xlsx"))

##-- Scenario E
scenario_E <- st_read(geopack,paste('Sc2Op3_dzone_',datacallyear,sep='_'))
ScenE = data.frame()
for(iID in 1:nrow(scenario_E)){
  print(paste0("This is polygon " , iID))
  poly = subset(scenario_E, id == iID)
  poly = st_cast(poly, "POLYGON")
  if(nrow(poly) == 1){
    poly2 = st_cast(poly, "POINT")
    poly2$Polygon_Number = iID
    poly2$Coordinate_Order = 1:nrow(poly2)
    poly2$Coordinate_ID = paste(poly2$id, poly2$Coordinate_Order, sep="_")
    poly2$Longitude = round(st_coordinates(poly2)[,1], digits=3)
    poly2$Latitude = round(st_coordinates(poly2)[,2], digits=3)
    poly2 = st_drop_geometry(poly2)
    poly2$id = NULL
    ScenE = rbind(ScenE, poly2)} else {
      poly$id = paste0(iID, letters[1:nrow(poly)])
      for(iSub in 1:nrow(poly)){
        sub = poly[iSub,]
        sub = st_cast(sub, "POINT")
        sub$Polygon_Number = sub$id
        sub$Coordinate_Order = 1:nrow(sub)
        sub$Coordinate_ID = paste(sub$Polygon_Number, sub$Coordinate_Order, sep="_")
        sub$Longitude = round(st_coordinates(sub)[,1], digits=3)
        sub$Latitude = round(st_coordinates(sub)[,2], digits=3)
        sub = st_drop_geometry(sub)
        sub$id = NULL
        ScenE = rbind(ScenE, sub)}}
} # end iID-loop
write.csv(ScenE, file=paste0(pathdir, "/data_products/ScenarioE.csv"), row.names=F)
write.xlsx(ScenE, file=paste0(pathdir, "/data_products/ScenarioE.xlsx"))

### Create maps of VME polygons
## obtain spatial data
shape_ices_EEZ <- st_read(geopack,layer='EUVME_Assessment_Extent') %>% 
  st_make_valid()
Reg_depth <- st_read(geopack,layer='EU_Depth_400_800') %>% 
  st_make_valid()
scenario_A <- st_read(geopack,paste('Sc1Op1_dzone_',datacallyear,sep='_'))
scenario_B <- st_read(geopack,paste('Sc1Op2_dzone_',datacallyear,sep='_'))
scenario_C <- st_read(geopack,paste('Sc2Op1_dzone_',datacallyear,sep='_'))
scenario_D <- st_read(geopack,paste('Sc2Op2_dzone_',datacallyear,sep='_'))
scenario_E <- st_read(geopack,paste('Sc2Op3_dzone_',datacallyear,sep='_'))

## Fix MULTIPOLYGON-IDs
scA = scenario_A[0,]
for(iPoly in 1:nrow(scenario_A)){
  sub = scenario_A[iPoly,]
  sub = st_cast(sub, "POLYGON")
  if(nrow(sub) == 1){
    scA = rbind(scA, sub)
  } else {
  sub$id = paste0(iPoly, letters[1:nrow(sub)])
  scA = rbind(scA, sub)
  }} # end
st_write(obj=scA, paste0(pathdir, "/data_products/Shapefile_scA.shp"))

scB = scenario_B[0,]
for(iPoly in 1:nrow(scenario_B)){
  sub = scenario_B[iPoly,]
  sub = st_cast(sub, "POLYGON")
  if(nrow(sub) == 1){
    scB = rbind(scB, sub)
  } else {
    sub$id = paste0(iPoly, letters[1:nrow(sub)])
    scB = rbind(scB, sub)
  }} # end
st_write(obj=scB, paste0(pathdir, "/data_products/Shapefile_scB.shp"))

scC = scenario_C[0,]
for(iPoly in 1:nrow(scenario_C)){
  sub = scenario_C[iPoly,]
  sub = st_cast(sub, "POLYGON")
  if(nrow(sub) == 1){
    scC = rbind(scB, sub)
  } else {
    sub$id = paste0(iPoly, letters[1:nrow(sub)])
    scC = rbind(scC, sub)
  }} # end
st_write(obj=scC, paste0(pathdir, "/data_products/Shapefile_scC.shp"))

scD = scenario_B[0,]
for(iPoly in 1:nrow(scenario_D)){
  sub = scenario_D[iPoly,]
  sub = st_cast(sub, "POLYGON")
  if(nrow(sub) == 1){
    scD = rbind(scD, sub)
  } else {
    sub$id = paste0(iPoly, letters[1:nrow(sub)])
    scD = rbind(scD, sub)
  }} # end
st_write(obj=scD, paste0(pathdir, "/data_products/Shapefile_scD.shp"))

scE = scenario_E[0,]
for(iPoly in 1:nrow(scenario_E)){
  sub = scenario_E[iPoly,]
  sub = st_cast(sub, "POLYGON")
  if(nrow(sub) == 1){
    scE = rbind(scE, sub)
  } else {
    sub$id = paste0(iPoly, letters[1:nrow(sub)])
    scE = rbind(scE, sub)
  }} # end
st_write(obj=scE, paste0(pathdir, "/data_products/Shapefile_scE.shp"))


CS <- subset(shape_ices_EEZ, Eco_EEZ == "Celtic Seas")
BI <- subset(shape_ices_EEZ, Eco_EEZ == "Bay of Biscay and the Iberian Coast")
CSdepth <- st_intersection(CS, Reg_depth)
BIdepth <- st_intersection(BI, Reg_depth)
A_CS <- st_intersection(CS, st_make_valid(scA))
A_BI <- st_intersection(BI, st_make_valid(scA))
B_CS <- st_intersection(CS, st_make_valid(scB))
B_BI <- st_intersection(BI, st_make_valid(scB))
C_CS <- st_intersection(CS, st_make_valid(scC))
C_BI <- st_intersection(BI, st_make_valid(scC))
D_CS <- st_intersection(CS, st_make_valid(scD))
D_BI <- st_intersection(BI, st_make_valid(scD))
E_CS <- st_intersection(CS, st_make_valid(scE))
E_BI <- st_intersection(BI, st_make_valid(scE))

#plot(st_geometry(subset(shape_ices_EEZ, Eco_EEZ == "Celtic Seas")))
CS <- st_transform(CS, 4326)
BI <- st_transform(BI, 4326)
extCS <- st_bbox(st_buffer(CS, 100000))
extBI <- st_bbox(st_buffer(BI, 100000))

windowsFonts(A = windowsFont("Arial"))
CSmap <- basemap_terra(extCS, map_service = "esri", map_type = "world_ocean_base")
CSmap <- project(CSmap, "epsg:4326")
BImap <- basemap_terra(extBI, map_service = "esri", map_type = "world_ocean_base")
BImap <- project(BImap, "epsg:4326")

depthcol <- rgb(red=255, green=255, blue=0, alpha=80, maxColorValue = 255)
AssAreaCol <- rgb(red=139, green=0, blue=0, alpha=40, maxColorValue = 255) 

### Figure Celtic Seas
pdf(paste0(pathdir, "/data_products/Scenario_A.pdf"),       # File name
    width = 8, height = 11, # Width and height in inches
    onefile = TRUE,
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4") 
par(mar=c(0,0,0,0))
plotRGB(CSmap)
plot(st_geometry(CS), col=AssAreaCol, border="darkred", lwd=0.2, add=T)
plot(st_geometry(CSdepth), col=depthcol, add=T, border="yellow")
plot(st_geometry(A_CS), col="grey", border="dimgrey", add=T)
text(st_coordinates(st_centroid(A_CS)), labels=st_centroid(A_CS)$id, adj=c(0.5, 0.5), offset=0, cex=0.4)
mtext(side=3, line=-2, adj=0, text="Scenario A")

par(mar=c(0,0,0,0))
plotRGB(BImap)
plot(st_geometry(BI), col=AssAreaCol, border="darkred", lwd=0.2, add=T)
plot(st_geometry(BIdepth), col=depthcol, add=T, border="yellow")
plot(st_geometry(A_BI), col="grey", border="dimgrey", add=T)
text(st_coordinates(st_centroid(A_BI)), labels=st_centroid(A_BI)$id, adj=c(0.5, 0.5), offset=0, cex=0.4)
mtext(side=3, line=-2, adj=0, text="Scenario A")
dev.off()

### Figure Scenario B
pdf(paste0(pathdir, "/data_products/Scenario_B.pdf"),       # File name
    width = 8, height = 11, # Width and height in inches
    onefile = TRUE,
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4") 
par(mar=c(0,0,0,0))
plotRGB(CSmap)
plot(st_geometry(CS), col=AssAreaCol, border="darkred", lwd=0.2, add=T)
plot(st_geometry(CSdepth), col=depthcol, add=T, border="yellow")
plot(st_geometry(B_CS), col="grey", border="dimgrey", add=T)
text(st_coordinates(st_centroid(B_CS)), labels=st_centroid(B_CS)$id, adj=c(0.5, 0.5), offset=0, cex=0.4)
mtext(side=3, line=-2, adj=0, text="Scenario B")

par(mar=c(0,0,0,0))
plotRGB(BImap)
plot(st_geometry(BI), col=AssAreaCol, border="darkred", lwd=0.2, add=T)
plot(st_geometry(BIdepth), col=depthcol, add=T, border="yellow")
plot(st_geometry(B_BI), col="grey", border="dimgrey", add=T)
text(st_coordinates(st_centroid(B_BI)), labels=st_centroid(B_BI)$id, adj=c(0.5, 0.5), offset=0, cex=0.4)
mtext(side=3, line=-2, adj=0, text="Scenario B")
dev.off()

### Figure Celtic Seas
pdf(paste0(pathdir, "/data_products/Scenario_C.pdf"),       # File name
    width = 8, height = 11, # Width and height in inches
    onefile = TRUE,
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4") 
par(mar=c(0,0,0,0))
plotRGB(CSmap)
plot(st_geometry(CS), col=AssAreaCol, border="darkred", lwd=0.2, add=T)
plot(st_geometry(CSdepth), col=depthcol, add=T, border="yellow")
plot(st_geometry(C_CS), col="grey", border="dimgrey", add=T)
text(st_coordinates(st_centroid(C_CS)), labels=st_centroid(C_CS)$id, adj=c(0.5, 0.5), offset=0, cex=0.4)
mtext(side=3, line=-2, adj=0, text="Scenario C")

par(mar=c(0,0,0,0))
plotRGB(BImap)
plot(st_geometry(BI), col=AssAreaCol, border="darkred", lwd=0.2, add=T)
plot(st_geometry(BIdepth), col=depthcol, add=T, border="yellow")
plot(st_geometry(C_BI), col="grey", border="dimgrey", add=T)
text(st_coordinates(st_centroid(C_BI)), labels=st_centroid(C_BI)$id, adj=c(0.5, 0.5), offset=0, cex=0.4)
mtext(side=3, line=-2, adj=0, text="Scenario C")
dev.off()

### Figure Celtic Seas
pdf(paste0(pathdir, "/data_products/Scenario_D.pdf"),       # File name
    width = 8, height = 11, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4") 
par(mar=c(0,0,0,0))
plotRGB(CSmap)
plot(st_geometry(CS), col=AssAreaCol, border="darkred", lwd=0.2, add=T)
plot(st_geometry(CSdepth), col=depthcol, add=T, border="yellow")
plot(st_geometry(D_CS), col="grey", border="dimgrey", add=T)
text(st_coordinates(st_centroid(D_CS)), labels=st_centroid(D_CS)$id, adj=c(0.5, 0.5), offset=0, cex=0.4)
mtext(side=3, line=-2, adj=0, text="Scenario D")

par(mar=c(0,0,0,0))
plotRGB(BImap)
plot(st_geometry(BI), col=AssAreaCol, border="darkred", lwd=0.2, add=T)
plot(st_geometry(BIdepth), col=depthcol, add=T, border="yellow")
plot(st_geometry(D_BI), col="grey", border="dimgrey", add=T)
text(st_coordinates(st_centroid(D_BI)), labels=st_centroid(D_BI)$id, adj=c(0.5, 0.5), offset=0, cex=0.4)
mtext(side=3, line=-2, adj=0, text="Scenario D")
dev.off()

### Figure Celtic Seas
pdf(paste0(pathdir, "/data_products/Scenario_E.pdf"),       # File name
    width = 8, height = 11, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4") 
par(mar=c(0,0,0,0))
plotRGB(CSmap)
plot(st_geometry(CS), col=AssAreaCol, border="darkred", lwd=0.2, add=T)
plot(st_geometry(CSdepth), col=depthcol, add=T, border="yellow")
plot(st_geometry(E_CS), col="grey", border="dimgrey", add=T)
text(st_coordinates(st_centroid(E_CS)), labels=st_centroid(E_CS)$id, adj=c(0.5, 0.5), offset=0, cex=0.4)
mtext(side=3, line=-2, adj=0, text="Scenario E")

par(mar=c(0,0,0,0))
plotRGB(BImap)
plot(st_geometry(BI), col=AssAreaCol, border="darkred", lwd=0.2, add=T)
plot(st_geometry(BIdepth), col=depthcol, add=T, border="yellow")
plot(st_geometry(E_BI), col="grey", border="dimgrey", add=T)
text(st_coordinates(st_centroid(E_BI)), labels=st_centroid(E_BI)$id, adj=c(0.5, 0.5), offset=0, cex=0.4)
mtext(side=3, line=-2, adj=0, text="Scenario E")
dev.off()

