library(sf)
library(tidyverse)
library(tmap)
library(osmdata)

# read points from shp
points <- read.table("coordinates.txt", header=T, sep="\t") %>% 
  mutate(type = ifelse(Typ=="agricultural", "Agricultural landscape", "Near highway")) %>% 
  st_as_sf(coords=c("Longitude","Latitude"), crs=4326)

# load road data from OSM
highways <- opq(bbox = st_bbox(points)) %>%
  add_osm_feature(key = "highway", value = "motorway") %>%
  osmdata_sf() %>% 
  "$"("osm_lines")
roads1 <- opq(bbox = st_bbox(points)) %>%
  add_osm_feature(key = "highway", value = "trunk") %>%
  osmdata_sf() %>% 
  "$"("osm_lines")
roads2 <- opq(bbox = st_bbox(points)) %>%
  add_osm_feature(key = "highway", value = "primary") %>%
  osmdata_sf() %>% 
  "$"("osm_lines")
roads3 <- opq(bbox = st_bbox(points)) %>%
  add_osm_feature(key = "highway", value = "secondary") %>%
  osmdata_sf() %>% 
  "$"("osm_lines")
roads4 <- opq(bbox = st_bbox(points)) %>%
  add_osm_feature(key = "highway", value = "tertiary") %>%
  osmdata_sf() %>% 
  "$"("osm_lines")
obce <- read_sf("GIS/obce_body.shp") %>% 
  st_transform(4326) %>% 
  st_crop(st_bbox(points))
obce_p<- read_sf("GIS/obce_polygony.shp") %>% 
  st_transform(4326) %>% 
  st_crop(st_bbox(points))

# make the main map
tmap_options(check.and.fix = TRUE)
tm_shape(roads2, bbox = st_bbox(points)) + tm_lines(col="grey50", lwd = 2) +
  tm_shape(roads3) + tm_lines(col="grey50", lwd = 2) + 
  tm_shape(roads4) + tm_lines(col="grey") + 
  tm_shape(highways) + tm_lines(col="black", lwd=4) +
  tm_shape(roads1) + tm_lines(col="black", lwd=4) +
  #tm_shape(obce %>% filter(POCET_OBYV > 10000)) + tm_dots(col="grey50", size=1) +
  tm_shape(obce_p%>% filter(POCET_OBYV > 10000)) + tm_fill(col="grey50") +
  #tm_shape(obce_p%>% filter(POCET_OBYV > 10000)) + tm_text("NAZ_OBEC", auto.placement = TRUE, remove.overlap = TRUE) +
  tm_shape(points) +
  tm_bubbles(col="type", size=0.4, palette=c("chartreuse4", "coral4")) 
tmap_save(filename = "map.png")

st_write(points, "GIS/recorders.shp")
st_write(points %>% st_transform(5514), "GIS/recorders_JTSK.shp")

bb <- read_sf("GIS/kraje.shp") %>% 
  st_transform(4326) %>% 
  st_bbox
highways_CR <- opq(bbox = bb) %>%
  add_osm_feature(key = "highway", value = c("motorway")) %>%
  osmdata_sf() %>% 
  "$"("osm_lines")
st_write(highways_CR, "GIS/highways.shp")
