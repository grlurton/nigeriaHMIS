library(osmar)
library(maptools)

setwd('J://Project/phc/nga/dhis')
src <- osmsource_api()

NigeriaShp <-  readShapePoly('LGAMap.shp')
Nigeriabbox <- bbox(NigeriaShp)
Nigeriabbox <- corner_bbox(Nigeriabbox[1,1], Nigeriabbox[2,1], Nigeriabbox[1,2], Nigeriabbox[2,2])

NigeriaOSM <- get_osm(Nigeriabbox , source = src)



download.file("http://download.geofabrik.de/africa/nigeria-latest.osm.bz2","nigeria-latest.osm.bz2")

## for now decompression should be done by hand in preferred software

src <- osmsource_osmosis(file = "nigeria-latest.osm/nigeria-latest.osm")
Nigeria <- get_osm(Nigeriabbox, src)
haha <- get_osm(complete_file(), source = osmsource_file("nigeria-latest.osm/nigeria-latest.osm"))
