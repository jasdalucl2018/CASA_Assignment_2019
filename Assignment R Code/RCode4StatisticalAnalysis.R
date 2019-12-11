====
#November 18 2019

#Author Jason Dalrymple - jhdalrymple€googlemail.com
  
#This code has been written to test the statistical connection between a) Average Public Transport Accessibility score (2014)` and 
  # Employment rate (16 64) (2011)` - based on data available from the London DataStore. The resulting regression graph is available at the following website
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
  
ward <- read_csv("/Users/jasondalrymple/Desktop/UCL laptop  LBU 19_9_26/A. Modules/CASA0005 GIS&S/course work_1/R Code /LondonData2.csv")

#3. Select columns to analyse

colnames(ward)

#selected columns will be [27] "Employment rate (16 64) (2011)"  and [65] "Average Public Transport Accessibility score (2014)" 

#4 review dataset to determine if selected columns contains erroneous data 

#4.1 "Employment rate (16 64) (2011)"
unique(ward$"Employment rate (16 64) (2011)")

#4.2 "Average Public Transport Accessibility score (2014)"  
unique(ward$"Average Public Transport Accessibility score (2014)")

#4.3 Removing erroneous data/ confirming that there are no missing values
ward<-na.omit(ward)

#4.4.1 Confirm that data contains all numeric values - employment rate 

as.numeric(ward$"Employment rate (16 64) (2011)")

#4.4.2 Confirm that data contains all numeric values - Transport Accessibility score 

as.numeric(ward$"Average Public Transport Accessibility score (2014)")

#5. Calculate the average scores - for employment 
#5.1 A Employment rate (16 64) (2011)
ward_col_27<-c(as.numeric(ward$"Employment rate (16 64) (2011)"))
mean(ward_col_27)


#5.1 B Average Public Transport Accessibility Score
ward_col_65<-c(as.numeric(ward$"Average Public Transport Accessibility score (2014)"))
mean(ward_col_65)

#6. investigate if there is a correlation between a) Employment rate (16 64) (2011) (col 27) and b)Average Public Transport Accessibility score (2014) - col 65
#6.1 create dataset


Xvar<-c(ward_col_27)
Yvar<-c(ward_col_65)

data_ward<-data.frame(x=Xvar, y=Yvar)

#6.2 Print initial results 


cor(data_ward)

#6.3Rename column names to reflect underlining data

colnames(data_ward) <- c("(2011) Employment Rate 16 to 64", "(2014) Average Transport Accessibility Scores")

#6.4Print result

cor(data_ward)

#7 PLot Points
EmploymentRate <- c(ward_col_27)
AvTransportAccessScores <- c(ward_col_65)
#Plot the data
plot(EmploymentRate, AvTransportAccessScores, col="red")

#8 Conduct & Plot Regression Analysisplot 
ggplot(ward, 
       aes(x = as.numeric(ward$"Employment rate (16 64) (2011)")
           , y = as.numeric(ward$"Average Public Transport Accessibility score (2014)")
       )) + geom_point(shape=1)+geom_smooth(method=lm)+xlab("2011 Employment Rate for 16 to 64 year olds")+ylab("2014 Av Public Transportation Access Scores")

======

#9 Question nil hypothesis - there is no connection between unemployment and access to trasport 
#9 perform regression on employment rate and access to  transport 

q <- qplot(x = `Average Public Transport Accessibility score (2014)`, y = `Employment rate (16 64) (2011)`, data=ward)

q <- qplot(x = `Employment rate (16 64) (2011)`, y = `Average Public Transport Accessibility score (2014)`, data=ward)


#10 plot with a regression line - note, I've added some jitter here as the x-scale is rounded
q + stat_smooth(method="lm", se=FALSE, size=1) + geom_jitter()


#11 run the linear regression model and store its outputs in an object called model1
model1 <- lm(`Employment rate (16 64) (2011)` ~ `Average Public Transport Accessibility score (2014)`, data = ward)

#12 how the summary of those outputs
summary(model1)

model1_res <- tidy(model1)
summary(model1_res)

