Date  4/12/19
Author: JHDalrymple - jhdalrymple@gmail.com

This code has been written to perform a number of regression tests on LSOA IMD data pertaining to 
employment and transport access

codes refering to path names have been written for APPLE Mac, and assume that files have already been loaded to local directories 


Source of code: much of the code is derived from here https://andrewmaclachlan.github.io/CASA0005repo/gwr-and-spatially-lagged-regression.html
==========
#1. load libraries
library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)

#2 confirm working director 

getwd()

#3 download statistical boundary data - 

download.file("https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip", destfile="prac9_data/statistical-gis-boundaries-london.zip")

#4. load relevant  CVS file containing the London Data set

ward <- read_csv("/Users/jasondalrymple/Desktop/UCL laptop  LBU 19_9_26/A. Modules/CASA0005 GIS&S/course work_1/R Code /LondonData2.csv")


LondonWardsss <- readOGR("/Users/jasondalrymple/Desktop/UCL laptop  LBU 19_9_26/A. Modules/CASA0005 GIS&S/Practicals/19_12_4 Pr9 Spatial Regression/Practical_9/statistical-gis-boundaries-london/ESRI/London_Ward_CityMerged.shp")

#5 convert it to a simple features object
LondonWardsssSF <- st_as_sf(LondonWardsss)

#6 check coordinate reference system
LondonWardsssSF

#7 Proj4 string tells me it's in wgs84, so Convert to British National Grid
BNG = "+init=epsg:27700"
LondonWardsssSFBNG <- st_transform(LondonWardsssSF, BNG)

#8 check the data
qtm(LondonWardsssSFBNG)
colnames(LondonWardsssSFBNG)

- 
# 9 read in  data fom local drive
  
LondonWardProfiles <- read_csv("/Users/jasondalrymple/Desktop/CASA2019_Assignment/CSV files/Route2_IMDvsEmployment.csv")

#10 check all of the columns have been read in correctly
str(LondonWardProfiles)

=======
colnames(LondonWardProfiles)

#selected columns will be  "Transport Access Scores"  and  "IMS Score" 

#10 review dataset to determine if selected columns contains erroneous data 

#10.1 - employment rate "IMS Score"
unique(LondonWardProfiles$"IMS Score")

#10.2 "Transport Access Scores"  
unique(LondonWardProfiles$"Transport Access Scores")

#10.3 Removing erroneous data/ confirming that there are no missing values
LondonWardProfiles<-na.omit(LondonWardProfiles)

#10.4.1 Confirm that data contains all numeric values - employment rate 

as.numeric(unique(LondonWardProfiles$"IMS Score"))

#10.4.2 Confirm that data contains all numeric values - Transport Accessibility score 

as.numeric(unique(LondonWardProfiles$"Transport Access Scores"))
  

#11 check all of the columns have been read in correctly
str(LondonWardProfiles)

#12 merge boundaries and data
LonWardProfiles <- left_join(LondonWardsssSFBNG, LondonWardProfiles, by = c("GSS_CODE" = "Ward No"))

#13 view variables 
tmap_mode("view")
## tmap mode set to interactive viewing
#14.1 remember we are looking at average transport access scores 
qtm(LonWardProfiles, fill = "Transport Access Scores", borders = NULL)

#14.2 Employment rate 
qtm(LonWardProfiles, fill = "IMS Score", borders = NULL)

#15 Question nil hypothesis - there is no connection between unemployment and access to trasport 
#15.1 perform regression on employment rate and access to  transport 

q <- qplot(x = `Transport Access Scores`, y = `IMS Score`, data=LonWardProfiles)

#16 plot with a regression line - note, I've added some jitter here as the x-scale is rounded
q + stat_smooth(method="lm", se=FALSE, size=1) + geom_jitter()


#17 run the linear regression model and store its outputs in an object called model1
model1 <- lm(`IMS Score` ~ `Transport Access Scores`, data = LonWardProfiles)

#18 how the summary of those outputs
summary(model1)

model1_res <- tidy(model1)
summary(model1_res)
