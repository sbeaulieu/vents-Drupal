# R script point-in-polygon to check older version EEZ categories used in Vents Database against categories in VLIZ World EEZ Ver 11 from Nov 2019
# I couldn't figure out how to do reverse geocode using R package mregions, nor could I figure out how to login to LifeWatch web services
library(rgdal)
setwd("C:/Users/sbeaulieu/Desktop")
vents <- read.csv("VentdataSortedclean_20200321.csv")
coordinates(vents) <- c("Longitude", "Latitude")
EEZs <- readOGR(".", "eez_v11") # takes a minute
proj4string(vents) <- proj4string(EEZs)
inside.EEZ <- !is.na(over(vents, as(EEZs, "SpatialPolygons")))
# use 'over' again, this time with EEZs as a SpatialPolygonsDataFrame
# object, to determine which EEZ (if any) contains each sighting, and
# store the EEZ name as an attribute of the vents data
vents$EEZ <- over(vents, EEZs)$GEONAME
# write the augmented vents dataset to CSV
write.csv(vents, "vents-by-EEZ.csv", row.names=FALSE)