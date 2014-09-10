library(reshape2)

MatchedFacilities <- readShapePoints('FacilitiesGPS.shp')
FacilitiesData <- read.csv('MetadataUnitsRaw.csv')

FacilitiesData$GroupName <- gsub(' ' , '_'  , FacilitiesData$GroupName)

FacilitiesData$Dummy <- TRUE

FacilitiesDataLarge <- dcast(FacilitiesData , UnitId + UnitName + UnitLevel ~ GroupName)


FullMatchedFacilities <- MatchedFacilities
FullMatchedFacilities@data <- merge(FullMatchedFacilities@data , FacilitiesDataLarge ,
                                    by.x = 'facilityID' , by.y = 'UnitId',
                                    sort = FALSE)

NomsDim <- colnames(FullMatchedFacilities@data)
write.csv(NomsDim , "colShapes.csv" , row.names = FALSE)

writePointsShape(FullMatchedFacilities, "FacilitiesFull")
