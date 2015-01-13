setwd('J://Project/phc/nga/dhis')

MetaData <- read.csv(file = 'MetadataUnitsRaw.csv')[,-1]
OrgUnits <- read.csv(file = 'ExtractOrgUnitsRaw.csv')[,-1]


HierarchData <- data.frame(toMatch = 'ng Federal Government')
i <- 2
for (i in 2:max(MetaData$UnitLevel)){
  print(i)
  HierarchData <- merge(HierarchData , 
                        subset(OrgUnits , select = c(UnitName  , ParentName, UnitId)) , 
                        by.x = 'toMatch' , by.y = 'ParentId')
  print(colnames(HierarchData))
  colnames(HierarchData)[2*(i-1)] <- 'toMatch'
  colnames(HierarchData)[2*(i-1)+1] <- paste('Level' , i  , 'ID', sep = '')
  colnames(HierarchData)[1] <- paste('Level' , i-1 , sep = '')
  print('new names')
  print(colnames(HierarchData))
  print(nrow(HierarchData))
  i <- i+1
}

write.csv(HierarchData , 'HierarchyData.csv' ,row.names = FALSE)