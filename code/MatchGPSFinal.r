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
Locations <- read.csv('table_location.csv')
Facilities <- Locations$location_id[Locations$location_level == 'Facility']

#Select hierarchy of LGA Available in DHIS as of now
AvailableHierarchy <- subset(OrgUnitsHierarchy , #Level3 %in% DHISLGA$UnitName &
                               Level5ID %in% Facilities)

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

##Strategy 2 - match simply on all units in OSM 
osmStrategy2 <- osmNigeria
osmStrategy2@data$name <- paste(' ' , osmStrategy2@data$name , ' ' , sep = '')
osmStrategy2@data$name <- gsub('  ' , ' ' , osmStrategy2@data$name)
MatchStrat2 <- MatchOver(AvailableHierarchy , osmStrategy2)
MatchStrat2 <- MatchStrat2[MatchStrat2$match %in% 
                             UniqueMatch(MatchStrat2@data , MatchStrat2@data$match),
                           ]

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

##Strategy 4 - From Strategy 4 - if name has multiple match, in a < 10 km2 zone, put
## in the middle
osmStrategy4 <- osmNigeria
NatFeatures <- 'River|Hill|Forest Reserve|Native Area'
osmStrategy4@data$name <- gsub(NatFeatures , '' , osmStrategy4@data$name)
osmStrategy4@data$name <- paste(' ' , osmStrategy4@data$name , ' ' , sep = '')
osmStrategy4@data$name <- gsub('  ' , ' ' , osmStrategy4@data$name)
MatchStrat4 <- MatchOver(AvailableHierarchy , osmStrategy4)

Centroids <- data.frame(facility = character(), 
                        place = character() ,
                        centroidlat = numeric(),
                        centroidlong = numeric() ,
                        lat = numeric() ,
                        long = numeric())
for (place in unique(MatchStrat3$place)){
  MatchesforPlace <- MatchStrat3[MatchStrat3$place == place , ]
  if (nrow(MatchesforPlace@data) > 1){
    for (facility in unique(MatchesforPlace$match)){
      MatchFacPlace <- MatchStrat3[MatchesforPlace$match == facility , ]
      if(nrow(MatchesforPlace@data) > 1){
        nmatches <- nrow(MatchesforPlace@data)
        centroid <- gCentroid(MatchesforPlace)
        out <- data.frame(facility = rep(facility , nmatches), 
                          place = rep(place , nmatches) ,
                          centroidlat = rep(centroid@coords[,1] , nmatches) ,
                          centroidlong = rep(centroid@coords[,2] , nmatches) ,
                          lat = MatchesforPlace@coords[,1] ,
                          long = MatchesforPlace@coords[,2]
                          )
        Centroids <- rbind(Centroids , out)
        print('1 centroid de plus')
      }
    }
  }
}

test <- subset(Centroids , place == ' Aba ')
plot(test$lat , test$long , col = test$facility)
points(test$centroidlat , test$centroidlong  , col = test$facility)



##Strategy 5 - If multiple facilities have been found in a ward, attribute those in
## the same wards to variations in the convex zone

##Strategy Combine - combining first 3 strategies incrementally
osmStrategyC1 <- osmNigeria[!is.na(osmNigeria$place) ,]
osmStrategyC1@data$name <- paste(' ' , osmStrategyC1@data$name , ' ' , sep = '')
osmStrategyC1@data$name <- gsub('  ' , ' ' , osmStrategyC1@data$name)
MatchStratC1 <- MatchOver(AvailableHierarchy , osmStrategyC1)
MatchStratC1 <- MatchStratC1[MatchStratC1$match %in% 
                             UniqueMatch(MatchStratC1@data , MatchStratC1@data$match),
                             ]
MatchStratC1@data$MatchingStage <- 'Stage 1'


##Step 2

HierarchyStep2 <- subset(AvailableHierarchy , !(Level5 %in% MatchStratC1$match))
osmStrategyC2 <- osmNigeria
osmStrategyC2@data$name <- paste(' ' , osmStrategyC2@data$name , ' ' , sep = '')
#osmStrategyC2 <- osmStrategyC2[!(osmStrategyC2@data$name %in% 
#                                as.character(MatchStratC1$place)), ]
osmStrategyC2@data$name <- gsub('  ' , ' ' , osmStrategyC2@data$name)
MatchStratC2 <- MatchOver(HierarchyStep2 , osmStrategyC2)
MatchStratC2 <- MatchStratC2[MatchStratC2$match %in% 
                             UniqueMatch(MatchStratC2@data , MatchStratC2@data$match), 
                             ]
MatchStratC2@data$MatchingStage <- 'Stage 2'                           
MatchStratC2 <- spRbind(MatchStratC2 , MatchStratC1)

##Step 3

HierarchyStep3 <- subset(AvailableHierarchy , !(Level5 %in% MatchStratC2$match))
osmStrategyC3 <- osmNigeria
osmStrategyC3@data$name <- gsub(NatFeatures , '' , osmStrategyC3@data$name)
osmStrategyC3@data$name <- paste(' ' , osmStrategyC3@data$name , ' ' , sep = '')
#osmStrategyC3 <- osmStrategyC3[!(osmStrategyC3@data$name %in% 
#                                   as.character(MatchStratC2$place)), ]
osmStrategyC3@data$name <- gsub('  ' , ' ' , osmStrategyC3@data$name)
MatchStratC3 <- MatchOver(HierarchyStep3 , osmStrategyC3)
MatchStratC3 <- MatchStratC3[MatchStratC3$match %in% 
                               UniqueMatch(MatchStratC3@data , MatchStratC3@data$match), 
                             ]
MatchStratC3@data$MatchingStage <- 'Stage 3'                           
MatchStratC3 <- spRbind(MatchStratC3 , MatchStratC2)
                             
                             
                             
rm(osmStrategy1 , osmStrategy2 , osmStrategy3 , osmStrategy4 , osmStrategyC1 ,
   osmStrategy42 , osmStrategyC3)


##################################################
########## Validate Matching #####################
##################################################

ValidationSet <- readShapePoints('ValidationSet.shp')

Validation <- function(TestedSet , ValidationSet){
  ValidData <- ValidationSet@data
  ValidData$DHISName <- as.character(ValidData$DHISName)
  TestedSet <- TestedSet[TestedSet@data$match %in% ValidData$DHISName ,]
  
  TestedCoords <- data.frame(match = TestedSet$match ,
                             latData = TestedSet@coords[,1],
                             longData = TestedSet@coords[,2])
  TestedCoords <- merge(TestedCoords , ValidData , by.x = 'match' , by.y  = 'DHISName')
  
  ValidCoords <- data.frame(match = ValidationSet$DHISName ,
                            Source = ValidationSet$Source ,
                            lateHealth = ValidationSet@coords[,1],
                            longeHealth = ValidationSet@coords[,2])
  Compare <- merge(ValidCoords , TestedCoords , by = 'match') 
  dist <- pointDistance(cbind(Compare$lateHealth , Compare$longeHealth), 
                        cbind(Compare$latData , Compare$longData), 
                        lonlat = TRUE, allpairs=FALSE) /1000
  out <- cbind(Compare , dist)
  out
}

CompareSet1 <- Validation(MatchStratC1 , ValidationSet)
CompareSet2 <- Validation(MatchStratC2 , ValidationSet)
CompareSet3 <- Validation(MatchStratC3 , ValidationSet)

ValidationStatistics <- function(ValidationOutput){
  min5 <- sum(ValidationOutput$dist <5)/length(ValidationOutput$dist)
  meanDist <- mean(ValidationOutput$dist)
  data.frame(min5 , meanDist) 
}

plotResults <- function(data , State){
  dataPlot <- subset(data , substr(data$match , 1 ,2) == State)
  coordinates(dataPlot) =~ long+lat
  plot(dataPlot)
  plot(DHISLGA , col = "grey" , add = TRUE)
  plot(DHISLGA[substr(DHISLGA$UnitName , 1 ,2) == State ,] , 
       col = "white" , add = TRUE)
  plot(dataPlot , add = TRUE , col = 'red')
  dataPlot <- subset(data , substr(data$match , 1 ,2) == State)
  coordinates(dataPlot) =~ latData + longData
  plot(dataPlot, add = TRUE)
  segments(dataPlot$lateHealth , dataPlot$longeHealth ,
           dataPlot$latData , dataPlot$longData , col = 'orange' , lwd = 2)
}

plotGroupped <- function(data){
  par(mfrow = c(2,4))
  for(State in unique(substr(data$match , 1 ,2))){
    plotResults(data , State)
  }
}

DiganosticElements <- function(data){
  stat <- ddply(data , .(Source.x) , ValidationStatistics)
  print(stat)
  plotGroupped(data)
}

DiganosticElements(CompareSet1)
DiganosticElements(CompareSet2)
DiganosticElements(CompareSet3)
#### essayer de mapper plus de facilities de Kano (avec la fonction faite pour mapper)