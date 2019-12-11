====
#November 18 2019

#Author Jason Dalrymple - jhdalrymple€googlemail.com
  
#This code has been written to test the statistical connection between a) Average Public Transport Accessibility score (2014)` and 
  # Employment rate (16 64) (2011)` - for the wards that are within scope of Bakerloo Line ROUTE 1. baseed on data available from the London DataStore. The resulting regression graph is available at the following website
  http://rpubs.com/Jasdal19/551497


# Data files for this code are all available in the following github https://github.com/jasdalucl2018/CASA2019_Assignment/blob/master/RCode4StatisticalAnalysis.R
  .. although the code is written for APPLE to call up the files from my local drive. Pathname code will need to be amended for windows machines.
.....The London Data Set is available from the London Data store as well as the following github https://github.com/jasdalucl2018/CASA2019_Assignment/blob/master/LondonData.csv

#This code forms part of the paper written on 
"An Examination as to whether the right route has been selected for the proposed Bakerloo Line Extension."


#Source of the code - much of the code is based on code made avaible by Mirco Musolesi  m.musolesi@ucl.ac.uk as part of 
UCL's Principles of Spatial Analysis Practicals Module 2018 - 2019 GEOG0014. For further information please contact 



====
#1 Load all required libraries
library(sf)
library(rgdal)
library(tmap)
library(tmaptools)
library(plyr)
library(tidyverse)  
library(ggplot2)
  
#2. Load CVS file containing the London Data set
  
ward <- read_csv("/Users/jasondalrymple/Desktop/UCL laptop  LBU 19_9_26/A. Modules/CASA0005 GIS&S/Coursework_Methodology reperformed/CSV/Route1_IMDvsEmployment.csv")



#this code helps to clear up data from characters to numbers. 
ward <- read_csv("/Users/jasondalrymple/Desktop/UCL laptop  LBU 19_9_26/A. Modules/CASA0005 GIS&S/Coursework_Methodology reperformed/CSV/Route1_IMDvsEmployment.csv",
                        col_types = cols(
                          `Ward No` = col_character(),
                          `WardName` = col_character(),
                          `Transport Access Scores` = col_number(),
                          `IMS Score` = col_number()))                  
                        

#3. Select columns to analyse

colnames(ward)

#selected columns will be [3] "Transport Access Scores"  and [4] "IMS Score" 

#4 review dataset to determine if selected columns contains erroneous data 

#4.1 "Transport Access Scores"
unique(ward$"Transport Access Scores")

#4.2 "IMS Score"  
unique(ward$"IMS Score")

#4.3 Removing erroneous data/ confirming that there are no missing values
ward<-na.omit(ward)

#4.4.1 Confirm that data contains all numeric values - Transport Access Scores 

as.numeric(ward$"Transport Access Scores")

#4.4.2 Confirm that data contains all numeric values - IMS Scores 

as.numeric(ward$"IMS Score")

#5. Calculate the average scores - Transport Access Score
#5.1 A Transport Access Scores
Route1Col_3<-c(as.numeric(ward$"Transport Access Scores"))
mean(Route1Col_3)


#5,2 B  Average Enmployment Access Score ( IMS Score)

Route1Col_4<-c(as.numeric(ward$"IMS Score"))
mean(Route1Col_4)

#6. investigate if there is a correlation between Route 1 a) Transport Access Scores and b) Employment IMD scores 
#6.1 create dataset

Xvar<-c(Route1Col_3)
Yvar<-c(Route1Col_4)

data_ward<-data.frame(x=Xvar, y=Yvar)

#6.2 Print initial results 


cor(data_ward)

#6.3Rename column names to reflect underlining data

colnames(data_ward) <- c("Transport Access Score", "Employment IMD Score")

#6.4Print result

cor(data_ward)

#7 PLot Points
TransportAccessScore <- c(Route1Col_3)
EmploymentIMD <- c(Route1Col_4)

#Plot the data
plot(TransportAccessScore, EmploymentIMD, col="red")

#8 Conduct & Plot Regression Analysisplot 
ggplot(ward, 
       aes(x = as.numeric(ward$"Transport Access Scores")
           , y = as.numeric(ward$"IMS Score")
       )) + geom_point(shape=1)+geom_smooth(method=lm)+xlab("2011 Employment Rate for 16 to 64 year olds")+ylab("2014 Av Public Transportation Access Scores")




