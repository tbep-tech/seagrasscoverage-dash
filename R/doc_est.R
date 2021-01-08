
library(tidyverse)
library(sf)
library(maptools)
library(raster)
library(tbeptools)

source('../sgdepth_manu/R/funcs.R')

# from fawda123/sgdepth_manu repo
data(tb_bathy)
data(tb_seg)

# preprocessed coverage layers
data(sgdat1988)
data(sgdat1990)
data(sgdat1992)
data(sgdat1994)
data(sgdat1996)
data(sgdat1999)
data(sgdat2001)
data(sgdat2004)
data(sgdat2006)
data(sgdat2008)
data(sgdat2010)
data(sgdat2012)
data(sgdat2014)
data(sgdat2016)
data(sgdat2018)

# globals
grid_spc <- 0.01
pts_spc <- 0.0005
rad <- 0.02
grid_seed <- 4321
prj <- 4326
set.seed(grid_seed)

# sample grid where doc is estimated
pts <- grid_est(tb_seg, spacing = grid_spc) 

# dense sample grid to convert sg polygons to points
sgpts <- grid_est(tb_seg, pts_spc) %>% 
  st_as_sf %>% 
  st_set_crs(prj) %>% 
  mutate(
    id = 1:nrow(.)
  )

alldoc <- list(
  `1988` = sgdat1988,
  `1990` = sgdat1990,
  `1992` = sgdat1992,
  `1994` = sgdat1994,
  `1996` = sgdat1996,
  `1999` = sgdat1999,
  `2001` = sgdat2001,
  `2004` = sgdat2004,
  `2006` = sgdat2006,
  `2008` = sgdat2008,
  `2010` = sgdat2010,
  `2012` = sgdat2012,
  `2014` = sgdat2014,
  `2016` = sgdat2016,
  `2018` = sgdat2018
  ) %>%
  enframe('yr', 'data') %>%
  mutate(
    data = purrr::pmap(list(yr, data), function(yr, data){
      
      cat(yr, '\t')
      
      # sample seagrass polygon for points in seagrass
      # then add depth
      sgpa <- st_intersection(sgpts, data) %>% 
        st_set_geometry(NULL) %>% 
        dplyr::select(id, FLUCCS_CODE) %>% 
        left_join(sgpts, ., by = 'id') %>% 
        mutate(
          Depth = raster::extract(tb_bathy, .), 
          Seagrass = case_when(
            FLUCCS_CODE == '9116' ~ 'Continuous', 
            FLUCCS_CODE == '9113' ~ 'Discontinuous', 
            T ~ NA_character_
          ), 
          Seagrass = factor(Seagrass)
        ) %>% 
        dplyr::select(Depth, Seagrass) %>% 
        as('Spatial')
      
      # crs needs to be removed because of nonsense
      sgpa@proj4string <- CRS()
      
      # get doc estimates
      ests <- suppressMessages(doc_est_grd(pts, sgpa, radius = rad, out_sens = T, remzero = F, trace = F))
      
      out <- ests %>% 
        st_as_sf() %>% 
        st_set_crs(prj)
      
      return(out)
      
    })
  )

tmp <- unnest(alldoc, 'data') %>% 
  st_as_sf() %>% 
  st_set_crs(prj) %>% 
  st_intersection(tbseg) %>% 
  st_set_geometry(NULL)

ggplot(tmp, aes(x = yr, y = z_cmed)) + 
  geom_boxplot() +
  facet_wrap(~bay_segment, ncol = 1)#, scales = 'free_y')


