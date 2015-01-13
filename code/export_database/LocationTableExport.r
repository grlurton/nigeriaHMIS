setwd('J://Project/phc/nga/dhis')

library(stringr)

FacilitiesMatched <- read.csv('matchedFacilities.csv' , stringsAsFactors = FALSE)
StateMatched <- read.csv('LocationsExport.csv' , stringsAsFactors = FALSE)
HierarchyData <- read.csv('HierarchyData.csv' , stringsAsFactors = FALSE)
SDIFacilities <- read.csv('J://temp/phc/facilities.csv' , stringsAsFactors = FALSE)
MetaData <- read.csv(file = 'MetadataUnitsRaw.csv' , stringsAsFactors = FALSE)

##This Data set gets OrgUnits with at least one associated data set or data recorded

DataUnits <- read.csv('UnitsWithData.csv', stringsAsFactors = FALSE)$x
ReportUnits <- unique(as.character(read.csv('OrgUnitxDataSets.csv')$as.character.orgUnits))

UnitsToMatch <- unique(c(DataUnits , ReportUnits))

##Cleaning facilities from DHIS to only keep active facilities and get duplicates out
FacilitiesToUse <- subset(MetaData , UnitId %in% UnitsToMatch & UnitLevel > 4 , 
                          select = c(UnitName , UnitLevel , UnitId))
FacilitiesToUse <- FacilitiesToUse[!duplicated(FacilitiesToUse) , ]


###Getting SDI Data about matched facilities 
FacilitiesWithData <- merge(FacilitiesMatched , 
                            SDIFacilities , by.x = 'SDIFacility' , by.y = 'facility_name')
FacilitiesWithData <- subset(FacilitiesWithData , 
                             select = c(SDIFacility , DHISFacility , UnitId ,
                                        basic_em_obstetric_service , comp_em_obstetric_service , 
                                        vaccine_service , coordinates , fac_type , urbanicity ,
                                        ownership , datayear , datasource, nid))

###Compiling the two
FacilitiesComplete <- merge(FacilitiesToUse , FacilitiesWithData , by = 'UnitId' , all.x = TRUE)

##making hierarchy of facilities
FacilitiesFull <- merge(FacilitiesComplete , HierarchyData , 
                        by.x = 'UnitId' , by.y = 'Level5ID' , all.x = TRUE)

##It appears some facilities have two parents. We remove them for simplicity
dupl <- table(as.character(FacilitiesFull$UnitId)) > 1
drop <- unique(as.character(FacilitiesFull$UnitId))[dupl]

FacilitiesFull <- subset(FacilitiesFull , !(UnitId %in% drop))

###We know have the list of facilities we want to keep

##We only keep the indicators we need
FacilitiesFull <- subset(FacilitiesFull , select = c(SDIFacility , UnitName , UnitId , UnitLevel , 
                                                             basic_em_obstetric_service , comp_em_obstetric_service , 
                                                             vaccine_service , coordinates , fac_type , urbanicity , 
                                                             ownership , datayear , datasource, nid , Level4ID , Level4 ))

##Adding facilities of level 6
FacilitiesFull <- merge(FacilitiesFull , HierarchyData , by.x = 'UnitId' , by.y = 'Level6ID' , all.x = TRUE)
FacilitiesFull <- subset(FacilitiesFull , select = c(UnitName , SDIFacility , UnitId , UnitLevel , 
                                                     basic_em_obstetric_service , comp_em_obstetric_service , 
                                                     vaccine_service , coordinates , fac_type , urbanicity , 
                                                     ownership , datayear , datasource, nid , Level4ID.x , Level4.x , Level5ID ,
                                                     Level5))


##We want the Ward to be the parents
FacilitiesFull$parentID <- as.character(FacilitiesFull$Level4ID)
FacilitiesFull$parentID[!is.na(FacilitiesFull$Level5ID)] <- as.character(FacilitiesFull$Level5ID[!is.na(FacilitiesFull$Level5ID)])

FacilitiesFull$parentName <- as.character(FacilitiesFull$Level4.x)
FacilitiesFull$parentName[!is.na(FacilitiesFull$Level5)] <- as.character(FacilitiesFull$Level5[!is.na(FacilitiesFull$Level5)])

FacilitiesFull$coordinates <- as.character(FacilitiesFull$coordinates)
#Trick to have a good number of coordinates because str_split does not split the NA values...
FacilitiesFull$coordinates[is.na(FacilitiesFull$coordinates)] <-  'a,b'

SplitCoords <- unlist(str_split(as.character(FacilitiesFull$coordinates) , pattern = ','))
NCoords <- length(SplitCoords)

FacilitiesFull$Latitude <- SplitCoords[2*seq(NCoords/2)-1]
FacilitiesFull$Longitude <- SplitCoords[2*seq(NCoords/2)]

FacilitiesFull$Latitude[FacilitiesFull$Latitude == 'a'] <- FacilitiesFull$Longitude[FacilitiesFull$Longitude == 'b'] <- NA
FacilitiesFull$Latitude <- as.numeric(FacilitiesFull$Latitude)
FacilitiesFull$Longitude <- as.numeric(FacilitiesFull$Longitude)

NFac <- nrow(FacilitiesFull)

attach(FacilitiesFull)
FacilitiesFinal <- data.frame(location_id = UnitId , location_name = UnitName , location_name_alt = SDIFacility ,
                              location_level = UnitLevel , location_latitude = Latitude , location_longitude = Longitude ,
                              location_start = character(NFac) , location_end = character(NFac) , location_type = fac_type ,
                              location_owner = ownership, location_urbanicity = urbanicity , 
                              location_service_basic_emergency_obstetric_service = basic_em_obstetric_service ,
                              location_service_complete_emergency_obstetric_sevice = comp_em_obstetric_service ,
                              location_service_vaccine = vaccine_service , location_service_year = datayear , 
                              location_service_source = datasource , location_service_nid = nid , 
                              location_parent_id = parentID , location_parent_name = parentName , stringsAsFactors = FALSE)
detach(FacilitiesFull)

rm(SDIFacilities , FacilitiesComplete , FacilitiesFull , FacilitiesMatched , FacilitiesToUse ,
   FacilitiesWithData , DataUnits , NCoords , NFac , ReportUnits , SplitCoords , UnitsToMatch ,
   drop , dupl)


#################
##### Including Wards in the hierarchy

Wards <- subset(MetaData , UnitLevel == 4)
WardsParents <- merge(Wards , HierarchyData , by.x = 'UnitId' , by.y = 'Level4ID')
WardsParents <- subset(WardsParents , select  = c(UnitId , UnitName , UnitLevel , 
                                                  Level3 , Level3ID))

WardsParents <- WardsParents[!duplicated(WardsParents),]
NWards <- nrow(WardsParents)

attach(WardsParents)
WardsFinal <- data.frame(location_id = UnitId , location_name = UnitName , location_name_alt = character(NWards) ,
                              location_level = UnitLevel , 
                              location_latitude = character(NWards) , location_longitude = character(NWards) ,
                              location_start = character(NWards) , location_end = character(NWards) , 
                              location_type  = character(NWards) ,
                              location_owner = character(NWards), location_urbanicity = character(NWards) , 
                              location_service_basic_emergency_obstetric_service = character(NWards) ,
                              location_service_complete_emergency_obstetric_sevice = character(NWards) ,
                              location_service_vaccine = character(NWards) , location_service_year = character(NWards) , 
                              location_service_source = character(NWards) , location_service_nid = character(NWards) , 
                              location_parent_id = Level3ID , location_parent_name = Level3 , stringsAsFactors = FALSE)
detach(WardsParents)

rm(NWards , Wards , WardsParents)
####States and LGA + GeopoliticalZones

locationSDI <- read.csv('J://temp/phc/phc_location_names.csv' , stringsAsFactors = FALSE)
locationSDI <- subset(locationSDI , select = c('state' , 'geopoliticalzone'))
locationSDI <- locationSDI[!duplicated(locationSDI) ,]
locationSDI$state[locationSDI$state == 'Akwa Ibom'] <- 'Akwa-Ibom'


geopoliticalZone <- merge(StateMatched ,  locationSDI , by.x = 'IDBName' , by.y = 'state' , all.x = TRUE)
geopoliticalZone$ParentName <- as.character(geopoliticalZone$ParentName)
geopoliticalZone$ParentName[!is.na(geopoliticalZone$geopoliticalzone)] <- 
  as.character(geopoliticalZone$geopoliticalzone[!is.na(geopoliticalZone$geopoliticalzone)])
  
StateFinal <- subset(geopoliticalZone , select = c(IDBName , DHISName , UnitId , ParentName , ParentId , Level))

zones <- unique(as.character(geopoliticalZone$geopoliticalzone))
zones <- zones[!is.na(zones)]
geoplev <- data.frame(IDBName =  zones, 
                      DHISName = zones , 
                      UnitId = paste('Geop' , seq(length(zones)) , sep = ''),
                      ParentName = 'Nigeria National' ,
                      ParentId = 's5DPBsdoE8b' , 
                      Level = 'Geopolitical Zone' , stringsAsFactors = FALSE)
nat <- data.frame(IDBName = 'Nigeria National' , 
                  DHISName = 'Nigeria National' , 
                  UnitId = 's5DPBsdoE8b',
                  ParentName = NA ,
                  ParentId = NA , 
                  Level = 'National' , stringsAsFactors = FALSE)

for(zone in geoplev$DHISName){
  StateFinal$ParentId[StateFinal$ParentName == zone] <- 
    geoplev$UnitId[geoplev$DHISName == zone]
}


StateFinal <- rbind(StateFinal , geoplev , nat)


NStates <- nrow(StateFinal)
attach(StateFinal)
StateOut <- data.frame(location_id = UnitId , location_name = DHISName , location_name_alt = character(NStates) ,
                         location_level = Level , 
                         location_latitude = character(NStates) , location_longitude = character(NStates) ,
                         location_start = character(NStates) , location_end = character(NStates) , 
                         location_type  = character(NStates) ,
                         location_owner = character(NStates), location_urbanicity = character(NStates) , 
                         location_service_basic_emergency_obstetric_service = character(NStates) ,
                         location_service_complete_emergency_obstetric_sevice = character(NStates) ,
                         location_service_vaccine = character(NStates) , location_service_year = character(NStates) , 
                         location_service_source = character(NStates) , location_service_nid = character(NStates) , 
                         location_parent_id = ParentId , location_parent_name = ParentName , stringsAsFactors = FALSE)
detach(StateFinal)

LocationTable <- rbind(StateOut , WardsFinal , FacilitiesFinal)
LocationTable$location_level[LocationTable$location_level == '4'] <- 'Ward'
LocationTable$location_level[LocationTable$location_level == '5'] <- 'Facility'
LocationTable$location_level[LocationTable$location_level == '6'] <- 'Facility2'
table(LocationTable$location_level)

###Put Capital letters for initial letters of multiple word 
simpleCap <- function(x) {
  s <- strsplit(x$location_name, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

out <- ddply(LocationTable , .(location_id), simpleCap )
LocationTable$location_name <- out$V1

write.csv(LocationTable , 'table_location.csv' , row.names = FALSE)
