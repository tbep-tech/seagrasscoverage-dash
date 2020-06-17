library(tidyverse)
library(sf)
library(tbeptools)
library(mapview)
library(doParallel)
library(foreach)

prj <- 4326

urls <- list(
  `2018` = 'https://opendata.arcgis.com/datasets/e7e0a72a33274ca3a3ce39629e2de250_0.geojson',
  `2016` = 'https://opendata.arcgis.com/datasets/f0ecff0cf0de491685f8fb074adb278b_20.geojson',
  `2014` = 'https://opendata.arcgis.com/datasets/f530f972ded749adb1c6b20c2651e7f9_18.geojson',
  `2012` = 'https://opendata.arcgis.com/datasets/619bd267e4c54e70968abd86eb92318e_17.geojson',
  `2010` = 'https://opendata.arcgis.com/datasets/82153be25a3340a0abdb3ec713425f29_16.geojson',
  `2008` = 'https://opendata.arcgis.com/datasets/4ddc60a8c9f845a2912c4e7cb14a3b7b_15.geojson',
  `2006` = 'https://opendata.arcgis.com/datasets/5a72bbd64bc9486696fa0bc47ca4e30c_13.geojson', 
  `2004` = 'https://opendata.arcgis.com/datasets/bb6b117c8eab40209d8125c3c95f6150_12.geojson', 
  `2001` = 'https://opendata.arcgis.com/datasets/e2ce063712f34654a4f371240f541479_11.geojson', 
  `1999` = 'https://opendata.arcgis.com/datasets/e27b6e5148514f29a1f1483813297fd7_10.geojson', 
  `1996` = 'https://opendata.arcgis.com/datasets/38f62dd9b6e5482888b2c0bb51716b6e_9.geojson',
  `1994` = 'https://opendata.arcgis.com/datasets/a2fb9d100cfd441cbdd24b16a3b0ce53_8.geojson',
  `1992` = 'https://opendata.arcgis.com/datasets/ea9fab53f2f74236b0cba8980dffe363_7.geojson',
  `1990` = 'https://opendata.arcgis.com/datasets/bcc955216c62468c9a6dafffc0545a40_6.geojson',
  `1988` = 'https://opendata.arcgis.com/datasets/092df867ece945b787557c9a7cf811d8_5.geojson'
  ) %>% 
  enframe 

# setup parallel backend
ncores <- detectCores() - 1 
cl <- makeCluster(ncores)
registerDoParallel(cl)
strt <- Sys.time()

res <- foreach(i = 1:nrow(urls), .packages = c('tidyverse', 'sf')) %dopar% {
  
  sink('log.txt')
  cat(i, 'of', nrow(urls), '\n')
  print(Sys.time()-strt)
  sink()
  
  prj <- 4326

  # import file
  dat_raw <- urls[i, ] %>% 
    pull(value) %>% 
    .[[1]] %>% 
    st_read
  
  # clip bounaries
  segs <- st_read('~/Desktop/TBEP/GISboundaries/GISboundaries/TBEP_Bay_Segments_Correct_Projection.shp') %>% 
    st_transform(crs = prj)
  
  if('FLUCCSCODE' %in% names(dat_raw))
    dat_raw <- dat_raw %>% 
      rename(FLUCCS_CODE = FLUCCSCODE)
  
  # crop by watershed and select fluccs
  # 9113 is patchy, 9116 is continuous
  dat_crp <- dat_raw %>%
    st_transform(crs = prj) %>%
    .[segs, ] %>% 
    filter(FLUCCS_CODE %in% c(9113, 9116)) %>% 
    select(OBJECTID, FLUCCS_CODE) %>% 
    st_buffer(dist = 0)

  # name assignment and save
  flnm <- paste0('sgdat', urls$name[i])
  assign(flnm, dat_crp)
  save(list = flnm, file = paste0('data/', flnm, '.RData'), compress = 'xz')

}

# 
# # 2016 data
# fl <- 'https://opendata.arcgis.com/datasets/f0ecff0cf0de491685f8fb074adb278b_20.geojson'
# # import file
# dat_raw <- st_read(fl)
# 
# segs <- st_read('~/Desktop/TBEP/GISboundaries/GISboundaries/TBEP_Bay_Segments_Correct_Projection.shp') %>% 
#   st_transform(crs = prj)
# 
# # 9113 is patchy, 9116 is continuous
# dat_clp <- dat_raw[segs, ] %>%  
#   filter(FLUCCS_CODE %in% c(9113, 9116))
# 
# stations <- st_as_sf(stations, coords = c('Longitude', 'Latitude'), crs = prj)
# 
# mapview(dat_clp) + mapview(stations)
# 
# save(dat_clp, file = 'data/dat_clp.RData', compress = 'xz')
