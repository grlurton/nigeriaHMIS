library(osmar)
library(maptools)

setwd('J://Project/phc/nga/dhis')

NigeriaShp <-  readShapePoly('LGAMap.shp')
Nigeriabbox <- bbox(NigeriaShp)
#Nigeriabbox <- corner_bbox(Nigeriabbox[1,1], Nigeriabbox[2,1], Nigeriabbox[1,2], Nigeriabbox[2,2])

url <- paste("http://www.overpass-api.de/api/xapi?" , 'node' , '[bbox=' ,
             Nigeriabbox[1,1], ',' ,Nigeriabbox[2,1], ',' , Nigeriabbox[1,2], ',' , Nigeriabbox[2,2] ,
             ']' , '[@meta]' ,
             sep = '')
             
response <- getURL(url, .encoding = "UTF-8")       
resp <- xmlParse(response)
osmardata <- as_osmar(resp)
shapefile <- as_sp(osmardata, what="points")


library(reshape2)


tt <- osmardata$nodes$tags
tt$k <- as.character(tt$k)
tt$k <- gsub(':' , '_'  ,tt$k)
tt$k <- gsub('/' , '_'  ,tt$k)
tt$k <- gsub(' ' , '_'  ,tt$k)
tt$k <- gsub('__' , '_'  ,tt$k)
length(unique(tt$k))

reshTags <- dcast(tt , id ~ k)

colnames(reshTags)[1] <- colnames(shapefile@data)[1] <- 'idtoMatch'

shapefile@data <- merge(shapefile@data , reshTags , by = 'idtoMatch' ,
                        sort = FALSE)

table(shapefile@data$amenity)

plot(NigeriaShp)
plot(shapefile , col = factor(shapefile@data$amenity) , add = TRUE)

