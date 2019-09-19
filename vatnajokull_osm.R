library(osmdata)
library(sf)
library(dplyr)

is <- opq (bbox = c( -18.9, 64.84, -14.0, 64.1)) %>%
  add_osm_feature(key = "natural", value = "glacier") %>% 
  osmdata_sf() %>% unique_osmdata()
