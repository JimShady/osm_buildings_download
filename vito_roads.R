rm(list=ls())

list.of.packages <- c("RPostgreSQL", "osmdata", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(RPostgreSQL)
library(osmdata)
library(lubridate)

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="vito_eu_emissions", user="james", password="brianclough", host="10.0.4.240")

# New plan!

# Import the points to PostGIS Make a line from the points
# proecss_vita_inc_canyons.sql

# Take the centre-point of each line, and join it with
# the classification data, putting that value back in postgresql 
# linked to the line

#land_coverage <- raster('/home/james/mounts/James/Mini Projects/sean_eu_nox_roads/g100_clc12_V18_5a/g100_clc12_V18_5.tif')
#proj4string(land_coverage) <- CRS("+init=epsg:3035")

#centroid_attributes              <- dbGetQuery(con, paste0("SELECT   id,
#                                                                    st_x(geo_centroid) as x,
#                                                                    st_y(geo_centroid) as y
#                                                           FROM     eu_nox_points"))

#coordinates(centroid_attributes) <- ~x + y
#proj4string(centroid_attributes) <- CRS("+init=epsg:3035")

#centroid_attributes$land_type    <- extract(land_coverage, centroid_attributes)

#centroid_attributes              <- data.frame(centroid_attributes)

#dbWriteTable(con, "temp_land_cover", value = centroid_attributes, append = TRUE, row.names = FALSE)

#dbGetQuery(con, paste0("UPDATE  eu_nox_points
#                       SET     land_type = temp_land_cover.land_type
#                       FROM     temp_land_cover
#                       WHERE    eu_nox_points.id = temp_land_cover.id"))

#dbGetQuery(con, paste0("DROP TABLE temp_land_cover"))

#print(paste0("extracted land type and put back into table"))

api_list <- c('http://overpass-api.de/api/interpreter',
              'https://lz4.overpass-api.de/api/interpreter',
              'https://z.overpass-api.de/api/interpreter',
              #'http://overpass.osm.rambler.ru/cgi/interpreter',
              'https://overpass.kumi.systems/api/interpreter')

while (dbGetQuery(con,
                  paste0("SELECT COUNT(*) FROM eu_nox_points WHERE land_type IN (1,2,3) AND canyon IS NULL")) > 0){
  
  id <- dbGetQuery(con,
                   paste0("SELECT id FROM eu_nox_points WHERE land_type IN (1,2,3) AND canyon IS NULL OFFSET floor(random()*(25-10+1))+10 LIMIT 1"))
  
  api_to_use <- sample(1:length(api_list), 1)
  
  set_overpass_url(api_list[api_to_use]) 
  
  # Bbox info to get the buildings data
  
  bbox_attributes                 <- dbGetQuery(con,
                                                paste0("SELECT  
                                                            st_xmin(st_extent(st_transform(st_buffer(the_geom, 100, 'endcap=flat join=round'), 4326))) as bbox_min_x,
                                                            st_ymin(st_extent(st_transform(st_buffer(the_geom, 100, 'endcap=flat join=round'), 4326))) as bbox_min_y,
                                                            st_xmax(st_extent(st_transform(st_buffer(the_geom, 100, 'endcap=flat join=round'), 4326))) as bbox_max_x,
                                                            st_ymin(st_extent(st_transform(st_buffer(the_geom, 100, 'endcap=flat join=round'), 4326))) as bbox_max_y
                                                            FROM    eu_nox_points
                                                            WHERE   id = ", id$id))
 
  buildings                       <- opq(bbox = c(bbox_attributes$bbox_min_x,
                                                  bbox_attributes$bbox_min_y,
                                                  bbox_attributes$bbox_max_x,
                                                  bbox_attributes$bbox_max_y)) %>% 
    add_osm_feature(key = 'building') %>%
    osmdata_sp()
  
  height  <- c()
  levels  <- c()
  height_counter <- 1
  levels_counter <- 1
  
  if ( "building:height" %in% names(buildings$osm_points))        { height[height_counter] <- mean(as.numeric(as.character(buildings$osm_points$`building:height`)), na.rm=T); 
  height_counter <- height_counter+1}
  if ( "building:height" %in% names(buildings$osm_lines))         { height[height_counter] <- mean(as.numeric(as.character(buildings$osm_lines$`building:height`)), na.rm=T);
  height_counter <- height_counter+1}
  if ( "building:height" %in% names(buildings$osm_polygons))      { height[height_counter] <- mean(as.numeric(as.character(buildings$osm_polygons$`building:height`)), na.rm=T);
  height_counter <- height_counter+1}
  if ( "building:height" %in% names(buildings$osm_multilines))    { height[height_counter] <- mean(as.numeric(as.character(buildings$osm_multilines$`building:height`)), na.rm=T);
  height_counter <- height_counter+1}
  if ( "building:height" %in% names(buildings$osm_multipolygons)) { height[height_counter] <- mean(as.numeric(as.character(buildings$osm_multipolygons$`building:height`)), na.rm=T);
  height_counter <- height_counter+1}
  
  if ( "building:levels" %in% names(buildings$osm_points))        { levels[levels_counter] <- mean(as.numeric(as.character(buildings$osm_points$`building:levels`)), na.rm=T);
  levels_counter <- levels_counter+1}
  if ( "building:levels" %in% names(buildings$osm_lines))         { levels[levels_counter] <- mean(as.numeric(as.character(buildings$osm_lines$`building:levels`)), na.rm=T);
  levels_counter <- levels_counter+1 }
  if ( "building:levels" %in% names(buildings$osm_polygons))      { levels[levels_counter] <- mean(as.numeric(as.character(buildings$osm_polygons$`building:levels`)), na.rm=T);
  levels_counter <- levels_counter+1 }
  if ( "building:levels" %in% names(buildings$osm_multilines))    { levels[levels_counter] <- mean(as.numeric(as.character(buildings$osm_multilines$`building:levels`)), na.rm=T);
  levels_counter <- levels_counter+1}
  if ( "building:levels" %in% names(buildings$osm_multipolygons)) { levels[levels_counter] <- mean(as.numeric(as.character(buildings$osm_multipolygons$`building:levels`)), na.rm=T);
  levels_counter <- levels_counter+1}
  
  
  if (!is.null(height)) { height <- mean(height) }
  
  if (!is.null(levels)) { levels <- 3.5 * mean(levels) + 9.625 + 2.265*(mean(levels)/25) }
  
  if (!is.null(levels) & !is.null(height)) {
    dbGetQuery(con, paste0("UPDATE eu_nox_points SET canyon = ", (height+levels)/2, " WHERE id = ", id$id))
    print(paste0('Completed ', id$id , ' using API ', api_to_use, ' at ', Sys.time(), ' set as ', (height+levels)/2))  }
  
  if (!is.null(levels) & is.null(height)) {
    dbGetQuery(con, paste0("UPDATE eu_nox_points SET canyon = ", levels, " WHERE id = ", id$id))
    print(paste0('Completed ID ', id$id , ' using API ', api_to_use, ' at ', Sys.time(), ' set as ', levels))  }
  
  if (is.null(levels) & !is.null(height)) {
    dbGetQuery(con, paste0("UPDATE eu_nox_points SET canyon = ", height, " WHERE id = ", id$id))
    print(paste0('Completed ', id$id , ' using API ', api_to_use, ' at ', Sys.time(), ' set as', height))
  }
  
  if (is.null(levels) & is.null(height)) {
    dbGetQuery(con, paste0("UPDATE eu_nox_points SET canyon = ", 999, " WHERE id = ", id$id))
    print(paste0('Completed ', id$id , ' using API ', api_to_use, ' at ', Sys.time(), ' no buildings found so set as 999'))  }
  
  Sys.sleep(5)
  
}

print(paste0('script broke while using API', api_to_use))
