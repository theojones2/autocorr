#Inspired by this discussion http://slatestarcodex.com/2016/04/02/beware-regional-scatterplots/#comment-342220
#This Script Needs "ape" package, "gstat", "sp", and "nlime" 
#Load packages 
library(ape)
library(gstat)
library(sp)
library(nlme)

#Load data
#Data comes from http://slatestarcodex.com/2016/01/06/guns-and-states/
#and https://inkplant.com/code/state-latitudes-longitudes
data <- read.csv("state_factors with latlon csv.csv", row.names = 1)
#Create total deaths row
data["TotalDead"] <- data["GunMurder"] + data["Suicide"]

#Use the method in http://www.ats.ucla.edu/stat/r/faq/morans_i.htm
#to generate Moran's I 

#First create a distance matrix
data.dists <- as.matrix(dist(cbind(data$Longitude, data$Latitude)))
#take inverse of the matrix values 
data.dists.inv <- 1/data.dists
#replace diagonal entries with zero
diag(data.dists.inv) <- 0

#Get Moran's I 
Moran.I(data$TotalDead, data.dists.inv)
#Significant autocorrlation p = 5.158247e-06

#Corelation befor adjustment 
mod.cor <- gls(PGun~TotalDead,data=data)
summary(mod.cor)
#r = -0.975

#refit model with correlation structure (based on both) using gaussian correlation 
#method discussed in 
#https://beckmw.wordpress.com/2013/01/07/breaking-the-rules-with-spatial-correlation/
#resp is linear combination of explanatory variables plus an error term that is normally distributed
resp <- 1+4*data[["TotalDead"]]-3*data[["PGun"]]-2*data$Latitude+rnorm(51)
#latlon is the sum of latitude and longitude. This is used in the gaussian corelation
#as a location metric. Not as exact as using lat and lon as seperate variables 
#but simplifies the correlation 
data$latlon <- data$Latitude + data$Longitude
mod.cor <- gls(resp~PGun+TotalDead,data=data,correlation=corGaus(form=~latlon,nugget=TRUE))
summary(mod.cor)
# r = -0.255

