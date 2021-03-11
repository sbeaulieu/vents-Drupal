# quick script to compute distance from named vent to all other named vents
# this simply appends columns to the original table
# it'd be even better to input the named vent and return a data table with the 10 closest vent Name.ID, Latitude, Longitude, Maximum.or.Single.Reported.Depth
# file on Stace's GitHub should be equivalent to Ver 3.4 in Pangaea
# https://github.com/sbeaulieu/vents-Drupal/raw/master/vent_fields_all_20200325cleansorted.csv

library(tidyverse)
library(geosphere)


vents_all <- read_csv("https://github.com/sbeaulieu/vents-Drupal/raw/master/vent_fields_all_20200325cleansorted.csv")
# Name.ID "EPR, 9 50'N" 9.83,-104.29
# Name.ID "Pescadero Basin, Auka" 23.9567,-108.8618
# Name.ID "Snail" 12.9533,143.62
# it'd be better to filter on Name.ID, acquire Latitude and Longitude as input to query the entire vents_all

p1 <- matrix(data = c(vents_all$Longitude, vents_all$Latitude), nrow = 721, ncol = 2, byrow = FALSE,
       dimnames = NULL)

pEPR <- c(-104.29,9.83)
km_from_EPR950 <- distHaversine(p1, pEPR, r=6378.137)

pPesc <- c(-108.8618,23.9567)
km_from_Pesc <- distHaversine(p1, pPesc, r=6378.137)

pSnail <- c(143.62,12.9533)
km_from_Snail <- distHaversine(p1, pSnail, r=6378.137)

vents_all_with_dist_EPR950_Pesc_Snail <- mutate(vents_all, km_from_EPR950, km_from_Pesc, km_from_Snail)
write.csv(vents_all_with_dist_EPR950_Pesc_Snail, "vents_all_with_dist_EPR950_Pesc_Snail.csv")
