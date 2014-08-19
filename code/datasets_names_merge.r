library(XML)
library(plyr)
library(RCurl)

DataSets <- read.csv('nigeria_dataSets.csv')

GetDatElements <- function(Report){
  url <- paste('https://dhis2nigeria.org.ng/api/dataSets/' , Report , '.xml' , sep = '')
  print(url)
  
  response<-getURL(url,userpwd="grlurton:Glurton29",httpauth = 1L, header=FALSE,ssl.verifypeer = FALSE)
  print('got Response')
  ParsedPage <- xmlParse(response)
  print('Parsedok')
  root <- xmlRoot(ParsedPage)
  
  if (!is.null(root[['dataElements']])){
    dataElements <- xmlSApply(root[['dataElements']] , xmlGetAttr , 'name')
  }
#  orgUnits <- xmlSApply(root[['organisationUnits']] , xmlGetAttr , 'name')
  
  out <- data.frame(dataElements)
}

GetOrgUnitsReports <- function(Report){
  url <- paste('https://dhis2nigeria.org.ng/api/dataSets/' , Report , '.xml' , sep = '')
  print(url)
  
  orgUnits <- NA
  
  response<-getURL(url,userpwd="grlurton:Glurton29",httpauth = 1L, header=FALSE,ssl.verifypeer = FALSE)
  print('got Response')
  ParsedPage <- xmlParse(response)
  print('Parsedok')
  root <- xmlRoot(ParsedPage)
  
  if (!is.null(root[['organisationUnits']])){
    orgUnits <- xmlSApply(root[['organisationUnits']] , xmlGetAttr , 'id')
  }
  
  data.frame(as.character(orgUnits))
}

dsFull <- ddply(DataSets[!(DataSets$DataSetsid %in% c('xIgRQKDjAdv')) , ] , 
                .(DataSetsName) , function(x) GetDatElements(x$DataSetsid) , .progress = 'text')

dsorgUnit <- ddply(DataSets , .(DataSetsName) , function(x) GetOrgUnitsReports(x$DataSetsid) , .progress = 'text')
write.csv(dsorgUnit , 'OrgUnitxDataSets.csv')

write.csv(dsFull , 'RepxDE.csv')
