# Create figure for in main advice document
# Karin van der Reijden

# Set main work/path directory  
pathdir <- "D:/VME/ADGVME2Assessment_AD_30032023"
setwd("D:/VME/ADGVME2Assessment_AD_30032023")

# Set path to geopackage for all related GIS data products
geopack <- paste(pathdir,"EUVME_Assessment_2022.gpkg",sep="/")
geopackORIG <- "D:/VME/ADGVME2Assessment_AD_30032023_ORIG/ADGVME2Assessment_AD_30032023/EUVME_Assessment_2022.gpkg"

# R libraries used
source(paste(pathdir,"Utilities/Libraries_VMEadvice.R",sep="/"))  
library(basemaps)
library(ggplot2)
library(terra)

## obtain spatial data
shape_ices_EEZ <- st_read(geopack,layer='EUVME_Assessment_Extent') %>% 
  st_make_valid()
Reg_depth <- st_read(geopack,layer='EU_Depth_400_800') %>% 
  st_make_valid()

## create subsets for the regions
CS <- subset(shape_ices_EEZ, Eco_EEZ == "Celtic Seas")
BI <- subset(shape_ices_EEZ, Eco_EEZ == "Bay of Biscay and the Iberian Coast")
CSdepth <- st_intersection(CS, Reg_depth)
BIdepth <- st_intersection(BI, Reg_depth)

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

depthcol <- rgb(red=255, green=255, blue=0, alpha=100, maxColorValue = 255)
AssAreaCol <- rgb(red=139, green=0, blue=0, alpha=50, maxColorValue = 255) 

### Figure Celtic Seas
tiff(filename="Figure1_CS.tiff", height=1000, width=810, res=100)
par(mar=c(0,0,0,0))
plotRGB(CSmap)
plot(st_geometry(CS), col=AssAreaCol, border="darkred", lwd=0.2, add=T)
plot(st_geometry(CSdepth), col=depthcol, add=T, border="yellow")

text("Galway", x=-8.55, y=53.4, srt=0, family="A")
text("Mayo", x=-9.2, y=54, srt=0, family="A")
text("Donegal", x=-7, y=54.8, srt=0, family="A")
text("Kerry", x=-9.3, y=52.3, srt=0, family="A")
text("Cork", x=-8.45, y=52, srt=0, family="A")

text("Rockall Bank", x=-16, y=56.5, srt=0, family="A", font=2)
text("Rockall \nTrough", x=-13, y=55.5, srt=0, family="A", font=2)
text("Hebrides \nTerrace", x=-7.5, y=56.6, family="A", font=2)
text("Porcupine \nBank", x=-14, y=52.3, srt=0, family="A", font=2)
text("Porcupine \nSeabight", x=-12.8, y=50.7, family="A", font=2)
text("Goban \nSpur", x=-12.5, y=49, family="A", font=2)
dev.off()

### Figure Bay of Biscay and the Iberian Coast
tiff(filename="Figure1_BI.tiff", height=1000, width=812, res=100)
par(mar=c(0,0,0,0))
plotRGB(BImap)
plot(st_geometry(BI), col=AssAreaCol, border="darkred", lwd=0.2, add=T)
plot(st_geometry(BIdepth), col=depthcol, add=T, border="yellow")
text("Gulf of \nCadiz", x=-7.3, y=36.5, srt=0, family="A")
text("Gorringe \nBank", x=-11.8, y=37.05, srt=0, family="A", font=2)
text("Galicia \nBank", x=-12.35, y=42.7, srt=0, family="A", font=2)
text("Le Danois \nBank", x=-4.9, y=44.5, srt=0, family="A", font=2)
text("Bay of \nBiscay", x=-4.8, y=46, srt=0, family="A")
dev.off()