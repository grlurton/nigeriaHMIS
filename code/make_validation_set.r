setwd('J://Project/phc/nga/dhis')

OrgUnitsHierarchy <- read.csv('HierarchyData.csv')
DHISLGA <- readShapePoly("LGAMap.shp")

#Select hierarchy of LGA Available in DHIS as of now
AvailableHierarchy <- subset(OrgUnitsHierarchy , Level3 %in% DHISLGA$UnitName)

##Voir comment obtenir osm data directement
osmData <- readShapePoints("C://Users/grlurton/Documents/NGAMap/NigeriaPointsEdit2.shp")

#####################################gi
#####Retrieve Usable OSM layer#######
#####################################

##Crop data to fit the zones we have in DHIS
osmCrop <- over(osmData , DHISLGA)

## Merge to have LGA in osm data
osmData@data <- cbind(osmData@data , DHISLGA = osmCrop$UnitName)

##Drop points with no name
osmNigeria <- osmData[!is.na(osmData$DHISLGA) ,]

rm(osmData , osmCrop , OrgUnitsHierarchy)

##Prepare osm names to be used in the matching

osmNigeria@data$name <- gsub('\\{|\\}' , '-' , osmNigeria@data$name)

ehealthdata <- osmNigeria[osmNigeria$source == 'ehealthafrica.org' &
                            osmNigeria$amenity == 'hospital' &
                            !is.na(osmNigeria$source) & !is.na(osmNigeria$amenity),]

##Match eHealth with datasets from DHIS
matchEhealthALL <- data.frame(ehealth = character() , data = character())
for(i in seq(nrow(ehealthdata))){
  out <- data.frame(ehealth = character() , data = character())
  mm <- grep(ehealthdata$name[i] , MatchStrat1$match , 
             ignore.case = FALSE , value = TRUE)
  if (length(mm) > 0){
    out <- data.frame(ehealth = ehealthdata$name[i] , data = mm)
    matchEhealthALL <- rbind(matchEhealthALL , out)
  }
}

rm(mm , i , out , LGA , LGAAlt , LGAData , LGADataAlt , NatFeatures)





## Only keep matches with facilities in Kano region 
## and that are matched on osm localities
dfValidCorresp <- subset(matchEhealthALL , substr(data , 1 ,2) == 'kn')
dfValidCorresp <- subset(dfValidCorresp ,
                         data %in% UniqueMatch(dfValidCorresp , dfValidCorresp$data))
dfValidCorresp <- subset(dfValidCorresp , 
                         ehealth %in% UniqueMatch(dfValidCorresp , dfValidCorresp$ehealth))


ValidationData <- ehealthdata[ehealthdata$name %in% dfValidCorresp$ehealth ,]



###################
######Data from SDI
###################


SDIData <- read.csv("J://temp/phc/facilities.csv" , stringsAsFactors = FALSE)
matchedSDI <- read.csv("matchedFacilities.csv" , stringsAsFactors = FALSE)

SDIData <- subset(SDIData , facility_name %in% matchedSDI$SDIFacility)

##get LGA of SDI facilities
SDIData <- merge(SDIData , matchedSDI , by.x = 'facility_name' , by.y = 'SDIFacility')
length(unique(SDIData$UnitId))

SDIData5 <- merge(SDIData , AvailableHierarchy , by.x = 'UnitId' , by.y = 'Level5ID')
SDIData6 <- merge(SDIData , AvailableHierarchy , by.x = 'UnitId' , by.y = 'Level6ID')

SDIData <- rbind(SDIData5 , SDIData6)

nn <- nrow(SDIData)
SDIData <- data.frame(name = SDIData$facility_name , 
                      lat = as.numeric(
                        unlist(strsplit(as.character(SDIData$coordinates) , ','))[2*seq(1:nn/2)-1]) ,
                      long = as.numeric(
                        unlist(strsplit(as.character(SDIData$coordinates) , ','))[2*seq(1:nn/2)]) ,
                      lga = SDIData$Level3)

SDIData <- subset(SDIData ,!is.na(long) & !is.na(lat) )

SDILGAcrop <- data.frame(name = character() ,
                         lat = numeric() ,
                         UnitName = character() ,
                         country = character())

tot <- 0
SDIData$lga <- as.character(SDIData$lga)
for (LGA in unique(SDIData$lga)){
  print(LGA)
  LGABorder <- DHISLGA[DHISLGA$UnitName == LGA ,]
  SDIinLGA <- SDIData[SDIData$lga == LGA , ]
  print(nrow(SDIinLGA))
  coordinates(SDIinLGA) = ~long+lat
  inLGA <- over(SDIinLGA , LGABorder)
  out <- cbind(SDIinLGA@data , inLGA)
  print(nrow(out))
  tot <- tot + nrow(out)
  print(tot)
  SDILGAcrop <- rbind(SDILGAcrop , out)
}

SDIDataShp <- subset(SDILGAcrop , !is.na(UnitName))
SDIDataShp <- merge(SDIDataShp , SDIData , by = 'name')


coordinates(SDIDataShp) = ~long+lat


plot(DHISLGA)
plot(SDIDataShp , add = T , col = 'blue')
plot(ehealthdata , add = T , col = 'red')
title(main = 'Validation Set')


### Crop SDI sur les LGA from DHIS sur lesquels ils ont ete matches