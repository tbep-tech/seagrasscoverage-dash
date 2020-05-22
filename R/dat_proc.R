library(tidyverse)
library(sf)
library(tbeptools)
library(mapview)

prj <- 4326


# 2016 data
fl <- 'https://opendata.arcgis.com/datasets/f0ecff0cf0de491685f8fb074adb278b_20.geojson'
# import file
dat_raw <- st_read(fl)

segs <- st_read('~/Desktop/TBEP/GISboundaries/GISboundaries/TBEP_Bay_Segments_Correct_Projection.shp') %>% 
  st_transform(crs = prj)

# 9113 is patchy, 9116 is continuous
dat_clp <- dat_raw[segs, ] %>%  
  filter(FLUCCS_CODE %in% c(9113, 9116))

stations <- st_as_sf(stations, coords = c('Longitude', 'Latitude'), crs = prj)

mapview(dat_clp) + mapview(stations)

save(dat_clp, file = 'data/dat_clp.RData', compress = 'xz')
