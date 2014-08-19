library(maptools)
library(stringr)
library(rgeos)
library(osmar)
library(plyr)
library(raster)
library(ggplot2)

setwd('J://Project/phc/nga/dhis')

#orgUnits <- read.csv('ExtractOrgUnitsRaw.csv')
OrgUnitsHierarchy <- read.csv('HierarchyData.csv')
DHISLGA <- readShapePoly("LGAMap.shp")

#Select hierarchy of LGA Available in DHIS as of now
AvailableHierarchy <- subset(OrgUnitsHierarchy , Level3 %in% DHISLGA$UnitName)

##Voir comment obtenir osm data directement
osmData <- readShapePoints("C://Users/grlurton/Documents/NGAMap/NigeriaPointsEdit2.shp")

#####################################
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

###Matching function
MatchSimple <- function(orgUnits , osmData){
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

MatchOver <- function(hierarch , osmDataCropped){
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
    out <- MatchSimple(hierarchWRK$Level5 , osmWRK)
    if (nrow(out) > 0){
      out$lga <- LGA
      MatchsCoords <- rbind(MatchsCoords , out)
    }
    LGAIndex <- LGAIndex+1
  }
  coordinates(MatchsCoords) = ~long+lat
  print(paste('Number of Matches :' , nrow(MatchsCoords) , sep = ' '))
  MatchsCoords
}

UniqueMatch <- function(data , indexVar){
  aa <- ddply(data , .(indexVar) , function(x) nrow(x))
  c(as.character(subset(aa , V1 == 1)[,1]))
}

##Strategy 1 - match simply on units considered places by OSM 
osmStrategy1 <- osmNigeria[!is.na(osmNigeria$place) ,]
osmStrategy1@data$name <- paste(' ' , osmStrategy1@data$name , ' ' , sep = '')
osmStrategy1@data$name <- gsub('  ' , ' ' , osmStrategy1@data$name)
MatchStrat1 <- MatchOver(AvailableHierarchy , osmStrategy1)
MatchStrat1 <- MatchStrat1[MatchStrat1$match %in% 
                             UniqueMatch(MatchStrat1@data , MatchStrat1@data$match),
                           ]

plot(MatchStrat1)

##Strategy 2 - match simply on all units in OSM 
osmStrategy2 <- osmNigeria
osmStrategy2@data$name <- paste(' ' , osmStrategy2@data$name , ' ' , sep = '')
osmStrategy2@data$name <- gsub('  ' , ' ' , osmStrategy2@data$name)
MatchStrat2 <- MatchOver(AvailableHierarchy , osmStrategy2)
MatchStrat2 <- MatchStrat2[MatchStrat2$match %in% 
                             UniqueMatch(MatchStrat2@data , MatchStrat2@data$match),
                           ]

plot(MatchStrat2)

##Strategy 3 - simplify osm names by taking out natural feature indication
osmStrategy3 <- osmNigeria
NatFeatures <- 'River|Hill|Forest Reserve|Native Area'
osmStrategy3@data$name <- gsub(NatFeatures , '' , osmStrategy3@data$name)
osmStrategy3@data$name <- paste(' ' , osmStrategy3@data$name , ' ' , sep = '')
osmStrategy3@data$name <- gsub('  ' , ' ' , osmStrategy3@data$name)
MatchStrat3 <- MatchOver(AvailableHierarchy , osmStrategy3)
MatchStrat3 <- MatchStrat3[MatchStrat3$match %in% 
                             UniqueMatch(MatchStrat3@data , MatchStrat3@data$match),
                           ]


############################
###Make Validation Set######
############################

##Get dataset of facilities from ehealth

ehealthdata <- osmNigeria[osmNigeria$source == 'ehealthafrica.org' &
                            osmNigeria$amenity == 'hospital' &
                            !is.na(osmNigeria$source) & !is.na(osmNigeria$amenity),]

idbData <- read.csv("J://temp/phc/facilities.csv" , )
nn <- nrow(idbData)
idbData <- data.frame(name = idbData$facility_name , 
                      lat = as.numeric(
                        unlist(strsplit(as.character(idbData$coordinates) , ','))[2*seq(1:nn/2)-1]) ,
                      long = as.numeric(
                        unlist(strsplit(as.character(idbData$coordinates) , ','))[2*seq(1:nn/2)]) )

idbData <- subset(idbData , !is.na(long) & !is.na(lat))

coordinates(idbData) = ~long+lat



plot(DHISLGA)
plot(idbData , add = T)
plot(ehealthdata , add = T , col = 'red')
title(main = 'Validation Set')

####
### Match idb et DHIS pour facility / state / LGA
### check les facilities sont dans le bon LGA







##Match these datasets with datasets from DHIS => faire une fonction
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




ValidationData <- ehealthdata[ehealthdata$name %in% matchEhealth$ehealth ,]



##################################################
########## Validate Matching #####################
##################################################

Validation <- function(TestedSet , ValidationSet){
  ValidData <- ValidationSet@data
  ValidData <- merge(ValidData , matchEhealth , by.x = 'name' , by.y = 'ehealth' , all.y = FALSE)
  ValidData$data <- as.character(ValidData$data)
  TestedSet <- TestedSet[TestedSet@data$match %in% ValidData$data ,]
  
  TestedCoords <- data.frame(match = TestedSet$match ,
                           latData = TestedSet@coords[,1],
                           longData = TestedSet@coords[,2])
  TestedCoords <- merge(TestedCoords , ValidData , by.x = 'match' , by.y  = 'data')
  
  
  ValidCoords <- data.frame(match = ValidationSet$name ,
                            lateHealth = ValidationSet@coords[,1],
                            longeHealth = ValidationSet@coords[,2])
  ValidCoords <- merge(ValidCoords , matchEhealth  , by.x = 'match' , by.y = 'ehealth' , all.y = FALSE)
  
  Compare <- merge(ValidCoords , TestedCoords , by.x = 'data' , by.y = 'match') 
  dist <- pointDistance(cbind(Compare$lateHealth , Compare$longeHealth), 
                        cbind(Compare$latData , Compare$longData), 
                        lonlat = TRUE, allpairs=FALSE) /1000
  min5 <- sum(dist <5)/length(dist)
  meanDist <- mean(dist)
  data.frame(min5 , meanDist)
}

CompareSet1 <- Validation(MatchStrat1 , ValidationData)
CompareSet2 <- Validation(MatchStrat2 , ValidationData)
CompareSet3 <- Validation(MatchStrat3 , ValidationData)




#par(mfrow = c(1,1))
#plot(CompareDHIS)
#plot(DHISLGA , col = "grey" , add = TRUE)
#plot(DHISLGA[substr(DHISLGA$UnitName , 1 ,2) == 'kn' ,] , col = "white" , add = TRUE)
#plot(CompareEhealth , add = TRUE , col = 'red')
#plot(CompareDHIS, add = TRUE)
#segments(CompareSet$lateHealth , CompareSet$longeHealth ,
#         CompareSet$latData , CompareSet$longData , col = 'orange' , lwd = 2)