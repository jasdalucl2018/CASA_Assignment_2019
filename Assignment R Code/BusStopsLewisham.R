#Code Author Jason Dalrymple jhdalrymple@Gmail.com

# Date November 19 
#code to work out whether there is a pattern in the bus stops of Lewisham 

#Note - much of this code comes from CASA0005: Geographic Information Systems and Science (19/20) - practical book 
# available at https://andrewmaclachlan.github.io/CASA0005repo/analysing-spatial-patterns.html

# the code assumes that all files have been loaded to the local drive 
#======================================

#Confirmj working directory
getwd()

#1. load relevant libraries  
library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)

#2 obtain London Borough Boundary Data 

EW <- geojson_read(
  "https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_4.geojson",
  what = "sp")

#3. Pull out london using grep and the regex wildcard for’start of the string’ (^) to to look for the bit of the district code that relates to London (E09) from the ‘lad15cd’ column in the data slot of our spatial polygons dataframe
BoroughMap <- EW[grep("^E09",EW@data$lad15cd),]
#4.plot it using the base plot function
qtm(BoroughMap)
summary(BoroughMap)

#4. CRS for project if BNG sotransform borough maop to BNG
BNG = "+init=epsg:27700"
BoroughMapBNG <- spTransform(BoroughMap,BNG)

#===
#5. Get geojson file containing bus stop data from local file
  
BusStops <- geojson_read("/Users/jasondalrymple/Desktop/UCL laptop  LBU 19_9_26/A. Modules/CASA0005 GIS&S/RCodeForAssignment/CodeForBusses/bus routes/BusStopGeoJsonFile.geojson", 
                          what = "sp")
# code is available from the following site https://github.com/jasdalucl2018/CASA_Assignment_2019/blob/master/geojson%20files%20/BusStopGeoJsonFile.geojson

#6. Run summary data
summary(BusStops)

#7 set up an EPSG string for projection 
BNG = "+init=epsg:27700"
WGS = "+init=epsg:4326"
BusStopsBNG <- spTransform(BusStops, BNG)
summary(BusStopsBNG)

#8. plot location of all bus stops in London
#plot the bus stops in the city
tmap_mode("view")
tm_shape(BoroughMapBNG) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BusStopsBNG) +
  tm_dots(col = "blue")

#====
#9 load bus routes
BusRoutes <- geojson_read("/Users/jasondalrymple/Desktop/UCL laptop  LBU 19_9_26/A. Modules/CASA0005 GIS&S/RCodeForAssignment/CodeForBusses/bus routes/BusRoutesGeoJsonFile.geojson", 
                         what = "sp")

# code is available from the following site https://github.com/jasdalucl2018/CASA_Assignment_2019/blob/master/geojson%20files%20/BusRoutesGeoJsonFile.geojson
#9.1 summary bus route data to confirm loaded
summary(BusRoutes)

#9.2 set up an EPSG string for projection 
BNG = "+init=epsg:27700"
WGS = "+init=epsg:4326"
BusRoutesBNG <- spTransform(BusRoutes, BNG)
summary(BusRoutesBNG)

#10 Plot the bus stops in the city 

tmap_mode("view")
tm_shape(BoroughMapBNG) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BusStopsBNG) +
  tm_dots(col = "blue")

#11 - there are unlikely to be any duplicate bus stops but just in case 
#remove duplicates
BusStopsBNG <- remove.duplicates(BusStopsBNG)

#A12.Extract data for Lewisham 
LBLBorough <- BoroughMapBNG[BoroughMapBNG@data$lad15nm=="Lewisham",]

#A12.1Confirm code has extracted correct borough has been pulled out
tm_shape(LBLBorough) +
  tm_polygons(col = NA, alpha = 0.5)

#A12.2 clip BusStopsBNG to extract Lewisham / Southwark only 
BusStopSub_LBL <- BusStopsBNG[LBLBorough,]

#A12.3 check that it's worked
tmap_mode("view")

##A12.4 Plot dots on interactive viewing m,ap with TMap
tm_shape(LBLBorough) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BusStopSub_LBL) +
  tm_dots(col = "blue")

#A13.1 Create obsertation Window for spatstat analysis within
#now set a window as the borough boundary
window <- as.owin(LBLBorough)
plot(window)

#A13.2 create  create a point pattern (ppp) object.
BusStopSub_LBL.ppp <- ppp(x=BusStopSub_LBL@coords[,1],y=BusStopSub_LBL@coords[,2],window=window)

#A13.3 check bus co-ordinates coordinates
BusStopSub_LBL@coords[,1]

#A13.4 Review new objects in window
plot(BusStopSub_LBL.ppp,pch=16,cex=0.5, main="Bus Stations Lewisham")

#A14. Point pattern analysis -Kernel Density Estimation from ppp object using the density function
plot(density(BusStopSub_LBL.ppp, sigma = 500))

#14a export KDE plot to shapefile for further analysis in QGIS
writeSpatialShape(BusStopSub_LBL,"LBL_KDE_map")

#A14b

#A16. Confirm Complete Spatial Randomness
teststats <- quadrat.test(BusStopSub_LBL.ppp, nx = 6, ny = 6)
teststats

plot(BusStopSub_LBL.ppp,pch=16,cex=0.5, main="Bus Stations in Lewisham")
plot(teststats, add=T, col = "red")

#A17.Undertake Ripley's K on Data 
Ripleys_K_Test_on_Lewisham_BusStops <- Kest(BusStopSub_LBL.ppp, correction="border")
plot(Ripleys_K_Test_on_Lewisham_BusStops)

#A18 DBSCAN on Data 

library(raster)
install.packages("fpc")
library(fpc)
library(plyr)
install.packages("OpenStreetMap")
library(OpenStreetMap)

#18.1 first extract the points from the spatial points data frame
BusStopSub_LBLPoints <- data.frame(BusStopSub_LBL@coords[,1:2])

#18.2 now run the dbscan analysis
db <- fpc::dbscan(BusStopSub_LBLPoints, eps = 700, MinPts = 4)
#18.3 now plot the results
plot(db, BusStopSub_LBLPoints, main = "DBSCAN Output for London Borough of Lewisham Bus Network", frame = F)
plot(LBLBorough, add=T)

#18.4 plotting with GGplot2
library(ggplot2)
db
db$cluster



# adding cluster info back into our dataframe
BusStopSub_LBLPoints$cluster <- db$cluster
# Next we are going to create some convex hull polygons to wrap around the points
# Use the ddply function in the plyr package to get the convex hull coordinates

chulls <- ddply(BusStopSub_LBLPoints, .(cluster), 
                function(df) df[chull(df$coords.x1, df$coords.x2), ])

#note remove 0 as this is not a cluster point - ie points need to be greater than  1 (>1)
chulls <- subset(chulls, cluster>=1)

# Now create a ggplot2 object from our data
dbplot <- ggplot(data=BusStopSub_LBLPoints, 
                 aes(coords.x1,coords.x2, colour=cluster, fill=cluster)) 

#add the points in
dbplot <- dbplot + geom_point()

#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=cluster), 
                                alpha = 0.5) 

# now plot, setting the coordinates to scale correctly and as a black and white plot 

dbplot + theme_bw() + coord_equal()

###add a basemap
##First get the bbox in lat long for Lewisham
latlong <- "+init=epsg:4326" 
BoroughWGS <-spTransform(LBLBorough, CRS(latlong))
BoroughWGS@bbox

