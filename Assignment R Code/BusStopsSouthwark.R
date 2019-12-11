#Code Author Jason Dalrymple jhdalrymple@Gmail.com

# Date November 19 
#code to work out whether there is a pattern in the bus stops of Southwark 

#Note - much of this code comes from CASA0005: Geographic Information Systems and Science (19/20) - practical book 
# available at https://andrewmaclachlan.github.io/CASA0005repo/analysing-spatial-patterns.html
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

#9 load bus routes -from local file
BusRoutes <- geojson_read("/Users/jasondalrymple/Desktop/UCL laptop  LBU 19_9_26/A. Modules/CASA0005 GIS&S/RCodeForAssignment/CodeForBusses/bus routes/BusRoutesGeoJsonFile.geojson", 
                         what = "sp")
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

#A12.Extract data for Southwark 
LBSBorough <- BoroughMapBNG[BoroughMapBNG@data$lad15nm=="Southwark",]

#A12.1Confirm code has extracted correct borough has been pulled out
tm_shape(LBSBorough) +
  tm_polygons(col = NA, alpha = 0.5)

#A12.2 clip BusStopsBNG to extract Southwark only 
BusStopSub_LBS <- BusStopsBNG[LBSBorough,]

#A12.3 check that it's worked
tmap_mode("view")

##A12.4 Plot dots on interactive viewing m,ap with TMap
tm_shape(LBSBorough) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BusStopSub_LBS) +
  tm_dots(col = "green")

#A13.1 Create obsertation Window for spatstat analysis within
#now set a window as the borough boundary
window <- as.owin(LBSBorough)
plot(window)

#A13.2 create  create a point pattern (ppp) object.
BusStopSub_LBS.ppp <- ppp(x=BusStopSub_LBS@coords[,1],y=BusStopSub_LBS@coords[,2],window=window)

#A13.3 check bus coordinates
BusStopSub_LBS@coords[,1]

#A13.4 Review new objects in window
plot(BusStopSub_LBS.ppp,pch=16,cex=0.5, main="Bus Stations Southwark")

#A14. Point pattern analysis -Kernel Density Estimation from ppp object using the density function
plot(density(BusStopSub_LBS.ppp, sigma = 500))


#A15 Quadrat Analysis - are the points ‘complete spatial randomness’ 

#A15.1 First plot the points
plot(BusStopSub_LBS.ppp,pch=16,cex=0.5, main="Bus Stations Southwark")
#A15.2 count the points in that fall in a 6 x 6 grid overlaid across the window
plot(quadratcount(BusStopSub_LBS.ppp, nx = 6, ny = 6),add=T,col="red")

#A15.3 Establish if there is an association between dots
#A15.4 run the quadrat count
Qcount<-data.frame(quadratcount(BusStopSub_LBS.ppp, nx = 6, ny = 6))
#A15.5 put the results into a data frame
QCountTable <- data.frame(table(Qcount$Freq, exclude=NULL))
#1A5.6 view the data frame
QCountTable
QCountTable <- QCountTable[-nrow(QCountTable),]

#A15.7 Check the data type in the first column - if it is factor, convert to numeric
class(QCountTable[,1])
#A15.8 convert to numberic
vect<- as.numeric(levels(QCountTable[,1]))
vect <- vect[1:6]
QCountTable[,1] <- vect

#A16. Confirm Complete Spatial Randomness
teststats <- quadrat.test(BusStopSub_LBS.ppp, nx = 6, ny = 6)
teststats

plot(BusStopSub_LBS.ppp,pch=16,cex=0.5, main="Bus Stations in Southwark")
plot(teststats, add=T, col = "red")


#A17.Undertake Ripley's K on Data 
Ripleys_K_Test_on_Southwark_BusStops <- Kest(BusStopSub_LBS.ppp, correction="border")
plot(Ripleys_K_Test_on_Southwark_BusStops)

#A18 DBSCAN on Data 

library(raster)
install.packages("fpc")
library(fpc)
library(plyr)
install.packages("OpenStreetMap")
library(OpenStreetMap)

#18.1 first extract the points from the spatial points data frame
BusStopSub_LBSPoints <- data.frame(BusStopSub_LBS@coords[,1:2])

#18.2 now run the dbscan analysis
db <- fpc::dbscan(BusStopSub_LBSPoints, eps = 700, MinPts = 4)
#18.3 now plot the results
plot(db, BusStopSub_LBSPoints, main = "DBSCAN Output for London Borough of Southwark Bus Network", frame = F)
plot(LBSBorough, add=T)

#18.4 plotting with GGplot2
library(ggplot2)
db
db$cluster



# adding cluster info back into our dataframe
BusStopSub_LBSPoints$cluster <- db$cluster
# Next we are going to create some convex hull polygons to wrap around the points
# Use the ddply function in the plyr package to get the convex hull coordinates

chulls <- ddply(BusStopSub_LBSPoints, .(cluster), 
                function(df) df[chull(df$coords.x1, df$coords.x2), ])

#note remove 0 as this is not a cluster point - ie points need to be greater than  1 (>1)
chulls <- subset(chulls, cluster>=1)

# Now create a ggplot2 object from our data
dbplot <- ggplot(data=BusStopSub_LBSPoints, 
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
BoroughWGS <-spTransform(LBSBorough, CRS(latlong))
BoroughWGS@bbox

