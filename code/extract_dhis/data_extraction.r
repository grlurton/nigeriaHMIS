library(RCurl)
library(XML)
library(zoo)

setwd('J://Project/phc/nga/dhis')

DfDataSets <- read.csv('nigeria_dataSets.csv')
DfOrgUnit <- read.csv('nigeria_orgunits.csv')
DfDataElements <- read.csv('nigeria_data_elements.csv')

GetReport <- function(Site , DateStart , DateEnd){
  url <- paste('https://dhis2nigeria.org.ng/api/dataValueSets.xml?' , 'dataSet=MZ8Z16lPJoP&' ,
               'dataSet=JwcqPVkjUWE&' , 'dataSet=zTii0mzVwQs&', 'dataSet=zAoBXwRpQ7X&', 'dataSet=lyVV9bPLlVy&', 
               'dataSet=SCGPsnXy6IZ&', 
               'dataSet=USY6TIGc4JS&', 'dataSet=XpbmnAxeUA8&', 'dataSet=xIgRQKDjAdv&', 'dataSet=NoSIZ9aEz4c&', 
               'orgUnit=' , Site , 
               '&startDate=' , DateStart , '&endDate=' , DateEnd, sep = '')
  #print(Site)
  
  response<-getURL(url , userpwd="grlurton:Glurton29" , httpauth = 1L , 
                   header=FALSE , ssl.verifypeer = FALSE)
  
  if(substr(response , 1 , 5) == "<?xml"){
    ParsedPage <- xmlParse(response)
    root <- xmlRoot(ParsedPage)
    
    dataElement <- unlist(as.character(xmlSApply(root, xmlGetAttr, "dataElement")))
    period <- unlist(as.character(xmlSApply(root, xmlGetAttr, "period")))
    orgUnit <- unlist(as.character(xmlSApply(root , xmlGetAttr , "orgUnit")))
    value <- unlist(as.character(xmlSApply(root , xmlGetAttr , "value")))
    categoryOptionCombo <- unlist(as.character(xmlSApply(root , xmlGetAttr , "categoryOptionCombo")))
    storedBy <- unlist(as.character(xmlSApply(root , xmlGetAttr , "storedBy")))
    lastUpdate <-unlist(as.character(xmlSApply(root , xmlGetAttr , "lastUpdated")))
    comment <- unlist(as.character(xmlSApply(root , xmlGetAttr , "comment")))
    followUp <- unlist(as.character(xmlSApply(root , xmlGetAttr  , "followUp")))
    
    out <- data.frame(dataElement , period , orgUnit , value , categoryOptionCombo , storedBy  , 
                      lastUpdate , comment , followUp) 
    return(out)
  }
} 
  
##Running  the function on unit x reports

OrgUnits <- read.csv('ExtractOrgUnitsRaw.csv')
ids <- unique(as.character(OrgUnits$UnitId))

#SampleSites <- sample(seq(1, nrow(DfOrgUnit)), 100 , replace = FALSE)

hash <- matrix( seq(signif(length(ids) , digits = 3)) , ncol = 100)

Data <- data.frame(dataElement = character() , period = character() , orgUnit = character() ,
                   categoryOptionCombo = character() , value = character() , Report = character())

TimeStart <- Sys.time()
for (i in seq(nrow(hash))){
  unitIds <- paste(ids[hash[i,]],collapse='&orgUnit=')
  extract <- GetReport(unitIds , '2008-01-01' , '2014-08-01')
  Data <- rbind(Data , extract)
  Time <- Sys.time()
  remaining <- (nrow(hash) - i)*((Time - TimeStart) / i)
  print(paste(i , ' itérations - Temps restant : ' , remaining , sep = ''))
} 

write.csv(Data , 'Data.csv')

UnitsWithData <- unique(Data$orgUnit)
write.csv(UnitsWithData , 'UnitsWithData.csv' , row.names = FALSE)



ReportsSites <- merge(Reports , subset(DfOrgUnit , select = c(unitName , unitLevel , unitid) ) , 
                      by.x = 'orgUnit' , by.y = 'unitid' , all.y = FALSE)
ReportsComplete <- merge(ReportsSites, subset(DfDataElements , select = c(DEName , DEid) ) , 
                         by.x = 'dataElement' , by.y = 'DEid' , all.y = FALSE)
ReportsFinal <- subset(ReportsComplete  , select = c(period , unitName , unitLevel , DEName , value , Report))



write.csv(ReportsComplete , 'FullData.csv')
write.csv(ReportsFinal , 'Data.csv')