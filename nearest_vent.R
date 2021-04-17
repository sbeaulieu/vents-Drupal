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

##Function to produce .csv file including the distance to the ten closest 'active, confirmed'  vent fields. 
##The vent_name_input needs to exactly match the Name.ID from vents_all e.g. "Pescadero Basin, Auka" (see example below)
##vent_field_matrix is a matrix 
closest_vents<- function(vent_name_input){
  vent_field_matrix<-vents_all %>% filter(Name.ID == vent_name_input) %>% ##Filters for vent_name_input 
    select(Longitude, Latitude) %>% ##Selecting Long, Lat 
    as.matrix(.) ## as a matrix so that it can be used in geosphere::distHaversine
  km_from_ventfield<-distHaversine(as.matrix(select(vents_all, Longitude, Latitude)), vent_field_matrix, r=6378.137) ##calculating the distance
  closest_ten_vents<- vents_all %>% mutate(km_from_ventfield) %>% arrange(km_from_ventfield) %>% ##Adding a km_from_ventfield column to vents_all, arranging in ascending order
    select(Name.ID, Activity, Latitude, Longitude, Maximum.or.Single.Reported.Depth,km_from_ventfield) %>% 
    filter(Activity== "active, confirmed") %>% ##Filtering based on Activity so that only 'active, confirmed' sites are included
    head(., n=10) ##Getting the top 10 vent fields
  write.csv(closest_ten_vents, file=paste("closest_vent_distance_",vent_name_input,".csv")) ##Saving as a .csv with the vent_name_input in file name 
  return(closest_ten_vents) ##Returning closest_ten_vents so that you can inspect the results. 
}

##Example
Pesc_closest_vents<-closest_vents("Pescadero Basin, Auka") ##Produces "closest_vent_distance_Pescadero Basin, Auka .csv"
EPR_closest_vents<-closest_vents("EPR, 9 50'N")
Mariana_closest_vents<-closest_vents("Snail")
