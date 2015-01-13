ttt <- read.csv('LocationsExport.csv')

ttt2 <- merge(ttt , ttt , by.x = 'DHISName' , by.y = 'ParentName')

ttt2$longLGA <- paste(ttt2$IDBName.x , ttt2$IDBName.y , sep = '_')

ttt3 <- merge(ttt , ttt2 , by.x = 'UnitId' , by.y = 'UnitId.y' , all.x = TRUE)

tttfinal <- subset(ttt3 , select =  c(UnitId , IDBName , longLGA , DHISName.x,  ParentName.x,	ParentId,	Level))
colnames(tttfinal) <- c('UnitId' , 'IDBName' , 'longLGA' , 'DHISName',  'ParentName',  'ParentId',	'Level')

write.csv(tttfinal , 'LocationLongLGA.csv')
