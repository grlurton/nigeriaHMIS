setwd('J://Project/phc/nga/dhis')

library(plyr)

AlexData <- read.csv('J://Project/phc/nga/output/database.csv' , stringsAsFactors = FALSE)

Indicators <- read.csv('table_indicators.csv' , stringsAsFactors = FALSE)
Locations <- read.csv('table_location.csv' , stringsAsFactors = FALSE)
DeReports <- read.csv('RepxDE.csv' , stringsAsFactors = FALSE)

hierarchy <- read.csv('HierarchyData.csv' , stringsAsFactors = FALSE)

tt <- merge(Indicators , DeReports , by.x = 'indicator_name' , by.y = 'dataElements' , all.x = FALSE)
head(tt)
IndicsExport <- as.character(tt$indicator_ID[tt$DataSetsName == 'NHMIS Monthly Summary (version 2013)'])

Data <- read.csv('Data.csv')

DataUse <- subset(Data , orgUnit %in% Locations$location_id & dataElement %in% IndicsExport)

DataUse$DataYear <- substr(as.character(DataUse$period) , 1 , 4)
DataUse <- subset(DataUse , select = c(orgUnit , dataElement , value , DataYear))

DataLogical <- subset(DataUse , value %in% c('false' , 'true'))
DataNumeric <- subset(DataUse , !is.na(as.numeric(as.character(value))))

#AverageData <- ddply(DataNumeric , .(dataElement , orgUnit , DataYear) , function(x) mean(as.numeric(x$value) , 
#                                                                                          na.rm = TRUE) ,
#         unction(data , hierarchy  , level){
  library(plyr)
  dataFacil1 <- merge(hierarchy  , data , by.y = 'orgUnit' , by.x = 'Level6ID')
  subset(dataFacil1 , select = c(dataElement , value , DataYear , 
                                Level2ID , Level3ID , Level4ID , Level5ID , Level6ID))
  dataFacil2 <- merge(hierarchy  , data , by.y = 'orgUnit' , by.x = 'Level5ID')
  subset(dataFacil2 , select = c(dataElement , value , DataYear , 
                                Level2ID , Level3ID , Level4ID , Level5ID , Level6ID))
  dataFacil <- rbind(dataFacil1 , dataFacil2)
  dataWard <- merge(hierarchy  , data , by.y = 'orgUnit' , by.x = 'Level4ID')
  subset(dataFacil , select = c(dataElement , value , DataYear , 
                                Level2ID , Level3ID , Level4ID , Level5ID , Level6ID))
  dataLGA <- merge(hierarchy  , data , by.y = 'orgUnit' , by.x = 'Level3ID')
  subset(dataFacil , select = c(dataElement , value , DataYear , 
                                Level2ID , Level3ID , Level4ID , Level5ID , Level6ID))
  dataState <- merge(hierarchy  , data , by.y = 'orgUnit' , by.x = 'Level2ID')
  subset(dataFacil , select = c(dataElement , value , DataYear , 
                                Level2ID , Level3ID , Level4ID , Level5ID , Level6ID))
  print('data extracted')
  if(level == 'Facility'){
    dataCollapse <- dataFacil
    dataCollapse$mergeOn <- dataCollapse$orgUnit
    print(length(unique(dataCollapse$mergeOn)))
  }
  if(level == 'Ward'){
    dataCollapse <- rbind(dataWard , dataFacil)
    dataCollapse$mergeOn <- dataCollapse$Level4ID
    print(length(unique(dataCollapse$mergeOn)))
  }
  if(level == 'LGA'){
    dataCollapse <- rbind(dataWard , dataFacil , dataLGA)
    dataCollapse$mergeOn <- dataCollapse$Level3ID
    print(length(unique(dataCollapse$mergeOn)))
  }
  if(level == 'State'){
    dataCollapse <- rbind(dataWard , dataFacil , dataLGA , dataState)
    dataCollapse$mergeOn <- dataCollapse$Level2ID
    print(length(unique(dataCollapse$mergeOn)))
  }
  out <- ddply(dataCollapse , .(dataElement , mergeOn , DataYear) , function(x) mean(as.numeric(x$value) , 
                                                                                     na.rm = TRUE) , 
               .progress  = 'text')
  colnames(out) <- c('indicator_id' , 'location_id' , 'value_year' , 'value')
  Nvalues <- nrow(out)
  out <- data.frame(indicator_id = out$indicator_id ,
                    location_id = out$location_id , 
                    value_year = out$value_year , 
                    value = out$value ,
                    value_type = rep('raw' , Nvalues) ,
                    value_uci = character(Nvalues),
                    value_lci = character(Nvalues) , 
                    value_se = character(Nvalues) ,
                    value_sample_size = character(Nvalues) ,
                    value_note = character(Nvalues) ,
                    value_source = character(Nvalues) ,
                    value_nid = rep('151858' , Nvalues) ,  stringsAsFactors = FALSE
  )
  out
}

DataCollapseState <- CollapseData(DataNumeric , hierarchy , 'State')
DataCollapseLGA <- CollapseData(DataNumeric , hierarchy , 'LGA')




colnames(AlexData) <- c('location_id' , 'value_year' , 'value' , 'value_lci' , 'value_uci' , 'value_note' ,
                        'indicator_id' , 'value_type' , 'value_source' , 'value_se' , 'value_sample_size' , 
                        'value_nid')


outIndics <- rbind(DataCollapseState , DataCollapseLGA , AlexData)

outIndics$value_type[outIndics$value_type == 'Raw'] <- 'raw'


write.csv(outIndics , 'table_values.csv')
