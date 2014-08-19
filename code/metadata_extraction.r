library(RCurl)
library(XML)


setwd('J://Project/phc/nga/dhis')

###Get Orgunits

##Did not manage to get number of pages from xml, so hardcoding the Npages
Npages <- 958

DfOrgUnit <- data.frame(unitName =  character() , unitLevel =  character() ,
                        unitPage = character() , unitid = character())
i <- 1
while(i <= Npages){
  print(i)
  if (i %in% c(460 , 827 , 829 , 833 , 834 ,836 ,841 ,842 , 845 , 848) ){
    i <- i+1 }  ## Some pages will not load. Could be problematic because orgunits are ordered by state
  else {
    OrgUnits <-paste('https://dhis2nigeria.org.ng/api/organisationUnits.xml?page=' , i , sep = '')
    PageOrgUnits<-getURL(OrgUnits,userpwd="grlurton:Glurton29", ssl.verifypeer = FALSE , httpauth = 1L)
    ParsedPage <- xmlParse(PageOrgUnits)
    root <- xmlRoot(ParsedPage)
    unitName <- xmlSApply(root[['organisationUnits']] , xmlGetAttr , 'name')
    unitLevel <- xmlSApply(root[['organisationUnits']] , xmlGetAttr ,  "level")
    unitPage <- paste(xmlSApply(root[['organisationUnits']] , xmlGetAttr , "href") , "xml" , sep = '.')
    unitid <- xmlSApply(root[['organisationUnits']] , xmlGetAttr , "id")
    temp <- data.frame(unitName , unitLevel , unitPage , unitid)
    DfOrgUnit <- rbind(DfOrgUnit , temp)
    print(paste(i , 'done' , sep = ' '))
    i <- i+1
  }
}

write.csv(DfOrgUnit , 'nigeria_orgunits.csv')

###Get Data Elements

Npages <- 15

DfDataElements <- data.frame(DEName =  character() , DELevel =  character() ,
                             DEPage = character() , DEid = character())
i <- 1
while(i <= Npages){
  print(i)
  DataElements <-paste('https://dhis2nigeria.org.ng/api/dataElements.xml?page=' , i , sep = '')
  PageDE<-getURL(DataElements,userpwd="grlurton:Glurton29", ssl.verifypeer = FALSE , httpauth = 1L)
  ParsedPage <- xmlParse(PageDE)
  root <- xmlRoot(ParsedPage)
  DEName <- xmlSApply(root[['dataElements']] , xmlGetAttr , 'name')
  DEid <- xmlSApply(root[['dataElements']] , xmlGetAttr , "id")
  temp <- data.frame(DEName , DEid)
  DfDataElements <- rbind(DfDataElements , temp)
  print(paste(i , 'done' , sep = ' '))
  i <- i+1
}

write.csv(DfDataElements , 'nigeria_data_elements.csv')


###Get Data Sets ===> Merge with the code data sets and data elements and org units

DataSets <-'https://dhis2nigeria.org.ng/api/dataSets.xml'
PageDataSets<-getURL(DataSets,userpwd="grlurton:Glurton29", ssl.verifypeer = FALSE , httpauth = 1L)
ParsedPage <- xmlParse(PageDataSets)
root <- xmlRoot(ParsedPage)
DataSetsName <- xmlSApply(root[['dataSets']] , xmlGetAttr , 'name')
DataSetsPage <- paste(xmlSApply(root[['dataSets']] , xmlGetAttr , "href") , "xml" , sep = '.')
DataSetsid <- xmlSApply(root[['dataSets']] , xmlGetAttr , "id")
DfDataSets <- data.frame(DataSetsName , DataSetsPage , DataSetsid)

write.csv(DfDataSets , 'nigeria_dataSets.csv')