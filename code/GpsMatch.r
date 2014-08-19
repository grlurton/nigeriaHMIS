library(maptools)
library(stringr)
library(rgeos)
library(osmar)
library(plyr)

setwd('J://Project/phc/nga/dhis')

orgUnits <- read.csv('ExtractOrgUnitsRaw.csv')
hierarch <- read.csv('HierarchyData.csv')
DHISLGA <- readShapePoly("LGAMap.shp")

##Voir comment obtenir osm data directement
osmData <- readShapePoints("C://Users/grlurton/Documents/NGAMap/NigeriaPointsEdit2.shp")

par(mfrow = c(2,2))
plot(DHISLGA , col = "grey")
plot(DHISLGA , col = "grey")
plot(osmData , col = 'darkred' , add = TRUE , cex = 0.5)
  
osmCropped <- over(osmData , DHISLGA)
osmData@data <- cbind(osmData@data , DHISLGA = osmCropped$UnitName)
osmDataCroppedALL <- osmData[!is.na(osmData$DHISLGA) ,]

plot(DHISLGA , col = "grey")
plot(osmDataCroppedALL , col = 'darkred' , add = TRUE, cex = 0.5)

### Reduire aux noms de localites

osmDataCropped <- osmDataCroppedALL[!is.na(osmDataCroppedALL$place) ,]
plot(DHISLGA , col = "grey")
plot(osmDataCropped , col = 'darkred' , add = TRUE, cex = 0.5)

###Matching function

Match <- function(orgUnits , osmData){
  out <- data.frame(index = character() , place = character() , 
                    match = character() , long = character() , lat = character())
  names <- as.character(osmData$name)
  facilities <- as.character(orgUnits)
  nosm <- nrow(osmData@coords)
  if (nosm >0){
    for(i in 1:length(names)){
      match <- facilities[grep(x = facilities , pattern = names[i] , 
                               ignore.case = TRUE)]
      nmatch <- length(match)
      if (nmatch > 0){
        outWRK <- data.frame(index = rep(i , nmatch) , 
                          place = rep(names[i] , nmatch) , 
                          match = match ,
                          long = osmData@coords[i] ,
                          lat = osmData@coords[nosm + i]
                          )
        out <- rbind(out , outWRK)
      }
    }
  }
  out
}

osmDataCropped@data$name <- paste(' ' , osmDataCropped@data$name , ' ' , sep = '')

MatchsCoords <- data.frame(index = character() , place = character() , 
                  match = character() , 
                  long = character() , lat = character() ,
                  lga = character())
LGAIndex <- 1
for(LGAIndex in 1:length(unique(hierarch$Level3))){
  LGA <- unique(as.character(hierarch$Level3))[LGAIndex]
  print(paste(LGAIndex , LGA , sep = ' - '))
  osmWRK<- osmDataCropped[osmDataCropped$DHISLGA == LGA ,]
  hierarchWRK <- subset(hierarch , Level3 == LGA)
  out <- Match(hierarchWRK$Level5 , osmWRK)
  if (nrow(out) > 0){
    out$lga <- LGA
    MatchsCoords <- rbind(MatchsCoords , out)
  }
  LGAIndex <- LGAIndex+1
}
rm(LGAIndex , LGA , osmWRK , hierarchWRK , out)
  
write.csv(MatchsCoords , "CoordsMatchsRaw.csv")

coordinates(MatchsCoords) = ~long+lat

lgaTest <- unique(as.character(hierarch$Level3))[8]
plotLGATest <- MatchsCoords[MatchsCoords@data$lga %in% lgaTest , ]
plotLGATest@data <- merge(plotLGATest@data  , hierarch ,
                          by.x = 'lga' , by.y = 'Level3' , all.y = FALSE)
par(mfrow = c(1,1))
plot(plotLGATest , col = plotLGATest$Level4)
plot(DHISLGA , col = "grey")
plot(MatchsCoords , col = 'darkblue' , add = TRUE , cex = 0.5)


##Creer validation set

##Get dataset of facilities from ehealth

ehealthdata <- osmDataCroppedALL@data[osmDataCroppedALL$source == 'ehealthafrica.org' & 
                                 osmDataCroppedALL$amenity == 'hospital' &
                                 !is.na(osmDataCroppedALL$source) & 
                                 !is.na(osmDataCroppedALL$amenity),]

##Match these datasets with datasets from DHIS

matchEhealthALL <- data.frame(ehealth = character() , data = character())
for(i in seq(nrow(ehealthdata))){
  out <- data.frame(ehealth = character() , data = character())
  mm <- grep(ehealthdata$name[i] , hierarch$Level5 , ignore.case = FALSE , value = TRUE)
  if (length(mm) > 0){
    out <- data.frame(ehealth = ehealthdata$name[i] , data = mm)
  }
  matchEhealthALL <- rbind(matchEhealthALL , out)
}

## Only keep matches with facilities in Kano region 
## and that are matched on osm localities
matchEhealth <- subset(matchEhealthALL , substr(data , 1 ,2) == 'kn' & 
                        data %in% MatchsCoords$match)

##Only keep uniquely matched ehealth facilities
duplicates <- function(data , indexVar){
  aa <- ddply(data , .(indexVar) , function(x) nrow(x))
  subset(aa , V1 > 1)
}

fromDHIS <- MatchsCoords@data[MatchsCoords$match %in% matchEhealth$data ,]
fromDHIS$match <- as.character(fromDHIS$match)

dup <- duplicates(matchEhealth , matchEhealth$ehealth)
dup <- rbind(dup , duplicates(matchEhealth , matchEhealth$data))
dup <- rbind(dup , 
             duplicates(fromDHIS , fromDHIS$match))


matchEhealth <- subset(matchEhealth , !(ehealth %in% dup$index) &
                         !(data %in% dup$index) )

CompareDHIS <- MatchsCoords[MatchsCoords$match %in% matchEhealth$data ,]
CompareEhealth <- osmDataCroppedALL[osmDataCroppedALL$name %in% matchEhealth$ehealth ,]




coordsData <- data.frame(match = CompareDHIS$match ,
                         latData = CompareDHIS@coords[,1],
                         longData = CompareDHIS@coords[,2])
coordsData <- merge(coordsData , matchEhealth , by.x = 'match' , by.y  = 'data')


eHealthData <- data.frame(name = CompareEhealth$name ,
                         lateHealth = CompareEhealth@coords[,1],
                         longeHealth = CompareEhealth@coords[,2])



Compare <- merge(eHealthData , coordsData , by.x = 'name' , by.y = 'ehealth')

par(mfrow = c(1,1))
plot(CompareDHIS)
plot(DHISLGA , col = "grey" , add = TRUE)
plot(DHISLGA[substr(DHISLGA$UnitName , 1 ,2) == 'kn' ,] , col = "white" , add = TRUE)
plot(CompareEhealth , add = TRUE , col = 'red')
plot(CompareDHIS, add = TRUE)
segments(Compare$lateHealth , Compare$longeHealth ,
        Compare$latData , Compare$longData , col = 'orange' , lwd = 2)


library(raster)
library(ggplot2)
dist <- pointDistance(cbind(Compare$lateHealth , Compare$longeHealth), 
              cbind(Compare$latData , Compare$longData), 
              lonlat = TRUE, allpairs=FALSE) /1000
sum(dist <5)/length(dist)
mean(dist)


##Quelques chiffres illustratifs
orgUnitDataSets <- read.csv("OrgUnitxDataSets.csv")
DataSetsUnit <- merge(orgUnitDataSets , orgUnits , 
                      by.x = 'as.character.orgUnits.' , by.y = 'UnitId')

length(unique(hierarch$Level5[hierarch$Level5 %in% DataSetsUnit$UnitName]))
length(unique(MatchsCoords$match[MatchsCoords$match %in% DataSetsUnit$UnitName]))

ReportingFacilities <- hierarch$Level5[hierarch$Level5 %in% DataSetsUnit$UnitName]
MatchReporting <- MatchsCoords[MatchsCoords@data$match %in% ReportingFacilities ,]

table(orgUnitDataSets$DataSetsName)

UniqueMatch <- function(data , indexVar){
  aa <- ddply(data , .(indexVar) , function(x) nrow(x))
  subset(aa , V1 == 1)
}

MatchData <- MatchsCoords@data
MatchReportData <- MatchReporting@data

nrow(UniqueMatch(MatchData , MatchData$match))
nonEquivocal <- UniqueMatch(MatchData , MatchData$match)
nrow(UniqueMatch(MatchReportData , MatchReportData$match))

##How many matched facility in total (match + Kano)

matchEhealthAdd <- subset(matchEhealthALL , substr(data , 1 ,2) == 'kn' & 
                         !(data %in% MatchData$match))

NonEquivEhealth <- UniqueMatch(matchEhealthAdd , matchEhealthAdd$data)

length(unique(hierarch$Level3))
length(unique(hierarch$Level3[hierarch$Level3 %in% DHISLGA$UnitName]))

length(unique(hierarch$Level5[hierarch$Level3 %in% DHISLGA$UnitName]))


