library(RCurl)
library(XML)


setwd('J://Project/phc/nga/dhis')

Nigeria <-'https://dhis2nigeria.org.ng/api/organisationUnits/s5DPBsdoE8b.xml'

##Function to extract children from a given page + adress of children
getChildren <- function(url){
  ParentName <- xmlGetAttr(root , "name")
  ParentId <- xmlGetAttr(root , "id")
  if(length(root[['children']]) > 0){
    UnitName <- xmlSApply(root[['children']] , xmlGetAttr , "name")
    UnitId <- xmlSApply(root[['children']] , xmlGetAttr , "id")
    UnitAdress <- xmlSApply(root[['children']] , xmlGetAttr , "href")
  }
  else{
    UnitAdress <- UnitName <- UnitAdress <- UnitId<- NA
  } 
  out <- data.frame(UnitName , UnitId ,  UnitAdress , ParentName , ParentId)
  out
}

getMetadata<- function(root){
  UnitLevel <- xmlGetAttr(root , "level")
  UnitName <- xmlGetAttr(root , "name")
  UnitId <- xmlGetAttr(root , "id")
  if(length(root[['organisationUnitGroups']]) > 0){
    GroupName <- xmlSApply(root[['organisationUnitGroups']] , xmlGetAttr , "name")
  }
  else{
    GroupName <- NA
  } 
  out <- data.frame(UnitName , UnitLevel , UnitId , GroupName)
  out
}

##Run Function

#Initiate parameters

pagesToRead <- c(Nigeria)
pagesRead <- c()
orgUnits <- data.frame(unitName =  character() , UnitId =  character() ,
                       UnitAdress = character() , ParentName = character() , ParentId = character())
MetaData <- data.frame(UnitName = character() , UnitLevel = character() , 
                      UnitId = character() , GroupName = character())
countp <- 0
total <- 0

StartTime <- Sys.time()
while (length(pagesToRead) > 0){
  extractOrg <- data.frame(unitName =  character() , UnitId =  character() ,
                           UnitAdress = character() , ParentName = character() , ParentLevel = character() ,
                           ParentId = character())
  extractMeta <- data.frame(UnitName = character() , GroupName = character())
  pagesJustRead <- c()  
  for(url in pagesToRead){
    print(url)
    Page<-getURL(url,userpwd="grlurton:Glurton29",
                 ssl.verifypeer = FALSE , httpauth = 1L)
    if(substr(Page , 1 , 5) == "<?xml"){
      ParsedPage <- xmlParse(Page)
      root <- xmlRoot(ParsedPage)
      extractOrg <- rbind(extractOrg , getChildren(root))
      extractMeta <- rbind(extractMeta , getMetadata(root))
    }
    pagesJustRead <- c(pagesJustRead , url)
    countp <- countp + 1
    print(paste(countp , 'Units done' , ',' , length(pagesToRead) - (countp - total) , 
                'remaining this round',sep = ' '))
  }
  print('Merging to general output')
  orgUnits <- rbind(orgUnits , extractOrg)
  MetaData <- rbind(MetaData , extractMeta)

  child_adress <- paste(as.character(extractOrg$UnitAdress[!is.na(extractOrg$UnitAdress)]) ,
                        "xml" , sep = '.')
  
  pagesRead <- c(pagesRead , pagesJustRead)
  total <- length(pagesRead)
  
  pagesToRead <- c(pagesToRead , child_adress)
  pagesToRead <- pagesToRead[!(pagesToRead %in% pagesRead )]
  
  print(paste("Pages read =" , total , sep = " " ))
  print(paste("Pages to Read" , length(pagesToRead) , sep = " "))
}
StopTime <- Sys.time()

write.csv(MetaData , 'MetadataUnitsRaw.csv' , row.names = FALSE)
write.csv(orgUnits , 'ExtractOrgUnitsRaw.csv', row.names = FALSE)
