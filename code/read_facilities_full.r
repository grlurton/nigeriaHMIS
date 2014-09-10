library(maptools)

FacilitiesFull <- read.shapefile('FacilitiesFull')
dimnames <- read.csv('colShapes.csv')

colnames(FacilitiesFull@data) <- dimnames$x