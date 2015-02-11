format_GPS <- function(org_units_description){
  org_units_description$coordinates <- gsub('\\[|\\]', '' ,
                                            org_units_description$coordinates)
  splitcoord <- unlist(strsplit(org_units_description$coordinates ,
                                "," , fixed = TRUE))
  coords <- length(splitcoord)
  org_units_coords <- data.frame(org_unit_ID = character() ,
                                 long = character() ,
                                 lat = character()
  )
  if(coords == 2){
    org_units_coords <- data.frame(org_unit_ID = org_units_description$org_unit_ID ,
                                   long = splitcoord[1] ,
                                   lat = splitcoord[2])
  }
  if(coords > 2){
    org_units_coords <- data.frame(org_unit_ID = unique(org_units_description$org_unit_ID),
                                   long = splitcoord[2*(1:(length(splitcoord)/2))-1] ,
                                   lat = splitcoord[2*(1:(length(splitcoord)/2))]
    )
  }
  org_units_coords
}

make_shapefiles <- function(formatted_coordinates){
  points_shapefile <- poly_shapefile<-  data.frame(org_unit_ID = character() ,
                                                   long = character() ,
                                                   lat = character()
  )

  for(i in 1:length(formatted_coordinates)){
    if(nrow(formatted_coordinates[[i]]) == 1){
      points_shapefile <- rbind(points_shapefile , formatted_coordinates[[i]])
    }
    if(nrow(formatted_coordinates[[i]]) > 1){
      poly_shapefile <- rbind(poly_shapefile , formatted_coordinates[[i]])
    }
  }

  points_shapefile$lat <- as.numeric(as.character(points_shapefile$lat))
  points_shapefile$long <- as.numeric(as.character(points_shapefile$long))
  coordinates(points_shapefile) = ~ long+lat

  print('points ok')

  poly_shapefile$lat <- as.numeric(as.character(poly_shapefile$lat))
  poly_shapefile$long <- as.numeric(as.character(poly_shapefile$long))
  ShapeData <- data.frame(org_unit_ID = unique(poly_shapefile$org_unit_ID) )
  poly_shapefile <- convert.to.shapefile(poly_shapefile, ShapeData, "org_unit_ID", 5)

  list(points_shapefile , poly_shapefile)
}

extract_geolocalisation <- function(org_units_description){
  formatted_gps <- dlply(org_units_description , .(org_unit_ID) ,
                         function(org_units_description){
                           tabGPS(org_units_description)
                           },
                         .progress = 'text')

  out <- make_shapefiles(formatted_gps)

  out
}
