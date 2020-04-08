# R script to clean CSV file output from InterRidge Vents Database ver3.4 for deposit into Pangaea
# Input: vent_fields_all CSV file note the date accessed
# Output: vent_fields_all_clean CSV file
library(dplyr)

# database version 3.4 completed on 2020-03-25 should be 721 total count
Ventdata <- read.csv("C:/Users/sbeaulieu/Desktop/vent_fields_all_20200325.csv")
# Ventdata <- read.csv("C:/Users/sbeaulieu/Desktop/vent_fields_all_032020.csv") # prior to last edits to the database

# exclude 5 columns MGDS, HostRock, Deposit, VolcanoNumber, NodeID
Ventdata %>% select(-MGDS_FeatureID.lowest.in.hierarchy, -Host.Rock, -Deposit.Type, -Volcano.Number, -Node.ID) -> Ventdata

# change factor columns to character columns
Ventdata %>% mutate_if(is.factor, as.character) -> Ventdata

# # the following lines commented bc only needed for vent_fields_all_032020.csv prior to last edits to the database
#
# # get rid of line breaks in a few character columns
# Ventdata$Notes.on.Vent.Field.Description <- sapply(Ventdata$Notes.on.Vent.Field.Description, function(x) { gsub("[\n]", "", x) })
# Ventdata$Discovery.References..text. <- sapply(Ventdata$Discovery.References..text., function(x) { gsub("[\n]", "", x) })
# Ventdata$Other.References..text. <- sapply(Ventdata$Other.References..text., function(x) { gsub("[\n]", "", x) })
#
# # replace the one vent field name that we know has problem with character encoding
# Ventdata %>% mutate(Name.ID=replace(Name.ID, Name.Alias.es.=="Aegir", "Aegir")) -> Ventdata
#
# # Dorado Outcrop is (the only) missing National.Jurisdiction Costa Rica
# Ventdata %>% mutate(National.Jurisdiction=replace(National.Jurisdiction, Name.ID =="Dorado Outcrop", "Costa Rica")) -> Ventdata
#
# # Isafjardardjup bay is (the only listing) missing Ocean (location is at the northern border of the IHO sea area for North Atlantic Ocean)
# Ventdata %>% mutate(Ocean=replace(Ocean, Name.ID =="Isafjardardjup bay", "N. Atlantic")) -> Ventdata
#
# # in the year discovered column, change NotApplicable to NotProvided
# # first see which this applies to
# DiscvNotApplicable <- Ventdata %>% filter(Year.and.How.Discovered..if.active..visual.confirmation.is.listed.first. == "NotApplicable")
# Ventdata %>% mutate(Year.and.How.Discovered..if.active..visual.confirmation.is.listed.first. = replace(Year.and.How.Discovered..if.active..visual.confirmation.is.listed.first., Year.and.How.Discovered..if.active..visual.confirmation.is.listed.first. == "NotApplicable", "NotProvided")) -> Ventdata

# the following lines needed for vent_fields_all_20200325.csv

# should be 20 columns
# 11 required columns, count should be 721: Name ID, Activity, Latitude, Longitude, Ocean, Region, National Jurisdiction, Maximum or Single Reported Depth, Tectonic setting, Year and How Discovered,	Discovery References
# 5 optional character columns so far we are not filling blanks: Name Alias, Vent Sites, Notes on Vent Field Description, Notes Relevant to Biology, Other References
# 1 optional numeric column with NAs: Minimum Depth
# 2 columns for which blanks (NA or "") are expected depending upon column Activity: Maximum Temperature, Max Temperature Category
# 1 column for which blanks (NA) are expected depending upon column Tectonic setting: Full Spreading Rate

# check 721 Name ID must be unique
nameID <- Ventdata %>% filter(Name.ID != "")
nrow(nameID)
unique(nameID$Name.ID)

# check 721 lat and lon and min and max
nrow(Ventdata %>% filter(!is.na(Latitude)))
nrow(Ventdata %>% filter(!is.na(Longitude)))
noNAs <- Ventdata$Latitude[which(!is.na(Ventdata$Latitude))]
min(noNAs)
max(noNAs)
noNAs <- Ventdata$Longitude[which(!is.na(Ventdata$Longitude))]
min(noNAs)
max(noNAs)

# check that 5 optional character columns do not have NAs
nrow(Ventdata %>% filter(is.na(Name.Alias.es.)))
nrow(Ventdata %>% filter(is.na(Vent.Sites)))
nrow(Ventdata %>% filter(is.na(Notes.on.Vent.Field.Description)))
nrow(Ventdata %>% filter(is.na(Notes.Relevant.to.Biology)))
nrow(Ventdata %>% filter(is.na(Other.References..text.)))

# check that 1 optional numeric column has NAs: Minimum Depth
nrow(Ventdata %>% filter(is.na(Minimum.Depth)))

# active confirmed should have MaxTemp numeric and/or MaxTempCategory character
# Higa and Mata Tolu are active confirmed with MaxTemp so remove "NotProvided" in MaxTempCategory
# Havre, Hinepuia volcanic center, Luso, "Mariana Trough, 15.5 N", Ribeira Quente, Tangaroa volcano are active confirmed withOUT MaxTemp so ADD "NotProvided" in MaxTempCategory
Ventdata %>% mutate(Max.Temperature.Category=replace(Max.Temperature.Category, Name.ID=="Higa", "")) -> Ventdata
Ventdata %>% mutate(Max.Temperature.Category=replace(Max.Temperature.Category, Name.ID=="Mata Tolu", "")) -> Ventdata
Ventdata %>% mutate(Max.Temperature.Category=replace(Max.Temperature.Category, Name.ID=="Havre", "NotProvided")) -> Ventdata
Ventdata %>% mutate(Max.Temperature.Category=replace(Max.Temperature.Category, Name.ID=="Hinepuia volcanic center", "NotProvided")) -> Ventdata
Ventdata %>% mutate(Max.Temperature.Category=replace(Max.Temperature.Category, Name.ID=="Luso", "NotProvided")) -> Ventdata
Ventdata %>% mutate(Max.Temperature.Category=replace(Max.Temperature.Category, Name.ID=="Mariana Trough, 15.5 N", "NotProvided")) -> Ventdata
Ventdata %>% mutate(Max.Temperature.Category=replace(Max.Temperature.Category, Name.ID=="Ribeira Quente", "NotProvided")) -> Ventdata
Ventdata %>% mutate(Max.Temperature.Category=replace(Max.Temperature.Category, Name.ID=="Tangaroa volcano", "NotProvided")) -> Ventdata

# Duanqiao is active inferred so remove "NotApplicable"from MaxTempCategory
Ventdata %>% mutate(Max.Temperature.Category=replace(Max.Temperature.Category, Name.ID=="Duanqiao", "")) -> Ventdata

# inactive Ashadze-3, Brown Bear Seamount, "central Manus Basin, Bismarck Sea", Mata Nima, Petersburgskoe fill "NotApplicable" in MaxTempCategory
Ventdata %>% mutate(Max.Temperature.Category=replace(Max.Temperature.Category, Name.ID=="Ashadze-3", "NotApplicable")) -> Ventdata
Ventdata %>% mutate(Max.Temperature.Category=replace(Max.Temperature.Category, Name.ID=="Brown Bear Seamount", "NotApplicable")) -> Ventdata
Ventdata %>% mutate(Max.Temperature.Category=replace(Max.Temperature.Category, Name.ID=="central Manus Basin, Bismarck Sea", "NotApplicable")) -> Ventdata
Ventdata %>% mutate(Max.Temperature.Category=replace(Max.Temperature.Category, Name.ID=="Mata Nima", "NotApplicable")) -> Ventdata
Ventdata %>% mutate(Max.Temperature.Category=replace(Max.Temperature.Category, Name.ID=="Petersburgskoe", "NotApplicable")) -> Ventdata

# check Activity should be 304 active confirmed, 362 active inferred, 55 inactive
# database is comprehensive for active confirmed and active inferred
# database is NOT comprehensive for inactive; previous versions of the database had 56 inactive bc we updated Vema Fracture Zone to active inferred
activeConf <- Ventdata %>% filter(Activity == "active, confirmed")
nrow(activeConf)
activeInf <- Ventdata %>% filter(Activity == "active, inferred")
nrow(activeInf)
inactive <- Ventdata %>% filter(Activity == "inactive")
nrow(inactive)

# 2 of the active inferred have MaxTemp (Atlantis II Deep and Discovery Deep)
activeInfMaxTemp <- activeInf %>% filter(!is.na(Maximum.Temperature))

# check min and max Maximum Temperature
noNAs <- Ventdata$Maximum.Temperature[which(!is.na(Ventdata$Maximum.Temperature))]
min(noNAs)
max(noNAs)

# because there are 55 inactive, there should be 55 NotApplicable in MaxTempCategory
nrow(inactive %>% filter(Max.Temperature.Category == "NotApplicable"))

# if no Region put NotProvided. There are 5 of these: Isafjardardjup bay, Espalamaca, El Hierro, Dorado Outcrop, Prony Bay
Ventdata %>% mutate(Region = replace(Region, Region == "", "NotProvided")) -> Ventdata

# we used script Ventdata_EEZcheck.R to check national jurisdiction in vent_fields_all_032020.csv prior to last edits to the database
# category labels in vents database ver3.4 are from VLIZ World EEZ v8, but all assignments were checked against VLIZ World_EEZ_v11_20191118
# we only needed to change 2 listings: 1 (EPR, southern 16 N segment) for Mexico and 1 (Vityaz megamullion) for Mauritius
Mexico <- Ventdata %>% filter(National.Jurisdiction == "Mexico")
Mauritius <- Ventdata %>% filter(National.Jurisdiction == "Mauritius")

# 2 missing Maximum or Single Reported Depth (Bataan and Leyte)
# filled with NA because even though not provided we don't want to mix character with numeric
missingDepth <- Ventdata %>% filter(is.na(Maximum.or.Single.Reported.Depth))

# check min and max Maximum or Single Reported Depth
noNAs <- Ventdata$Maximum.or.Single.Reported.Depth[which(!is.na(Ventdata$Maximum.or.Single.Reported.Depth))]
min(noNAs)
max(noNAs)

# Isafjardardjup bay Tectonic.setting is NotProvided
Ventdata %>% mutate(Tectonic.setting=replace(Tectonic.setting, Name.ID =="Isafjardardjup bay", "NotProvided")) -> Ventdata

# check min and max Full Spreading Rate
noNAs <- Ventdata$Full.Spreading.Rate..mm.a.[which(!is.na(Ventdata$Full.Spreading.Rate..mm.a.))]
min(noNAs)
max(noNAs)

# count of spreading rate values 547 is 4 less than count of tectonic setting mid-ocean ridge 404 + back-arc spreading center 147
# because 4 back-arc missing sprd rate (Kulo Lasi, "Parece Vela Ridge, Philippine Sea", "Philippine Ridge, West Philippine Basin", "Starfish Seamount, Tinakula")
# filled with NA because don't want to mix character with numeric
nrow(Ventdata %>% filter(Tectonic.setting == "mid-ocean ridge"))
nrow(Ventdata %>% filter(Tectonic.setting == "back-arc spreading center"))
nrow(Ventdata %>% filter(!is.na(Full.Spreading.Rate..mm.a.)))

# Manuk Island need to fill NotProvided Year.and.How.Discovered
Ventdata %>% mutate(Year.and.How.Discovered..if.active..visual.confirmation.is.listed.first. = replace(Year.and.How.Discovered..if.active..visual.confirmation.is.listed.first., Name.ID == "Manuk Island", "NotProvided")) -> Ventdata
# any rows left missing year?
nrow(Ventdata %>% filter(Year.and.How.Discovered..if.active..visual.confirmation.is.listed.first. == ""))

# 6 are missing Discovery Reference, fill with NotProvided. Other references is optional.
Ventdata %>% mutate(Discovery.References..text. = replace(Discovery.References..text., Discovery.References..text. == "", "NotProvided")) -> Ventdata
# any rows left missing Discovery Ref?
nrow(Ventdata %>% filter(Discovery.References..text. == ""))

# next sort alphabetically by region then latitude N to S
Ventdata %>% arrange(Region, desc(Latitude)) -> Ventdatacleansorted
write.csv(Ventdatacleansorted, "c:/Users/sbeaulieu/Desktop/vent_fields_all_20200325cleansorted.csv")
