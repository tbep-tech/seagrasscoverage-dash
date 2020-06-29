library(tidyverse)
library(sf)
library(tbeptools)
library(mapview)
library(doParallel)
library(foreach)
library(units)

# download raw data, clip, and save ---------------------------------------

prj <- 4326

urls <- list(
  `2018` = 'https://opendata.arcgis.com/datasets/8d0d473468924423bf0f1682aaca790f_0.geojson',
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
  
  # original in NAD83_HARN_Florida_West_ftUS, transform to wgs84
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
    filter(FLUCCS_CODE %in% c(9113, 9116, 7210, 9121)) %>% 
    select(OBJECTID, FLUCCS_CODE) %>% 
    st_buffer(dist = 0)

  # name assignment and save
  flnm <- paste0('sgdat', urls$name[i])
  assign(flnm, dat_crp)
  save(list = flnm, file = paste0('data/', flnm, '.RData'), compress = 'xz')

}

# area change for sankey --------------------------------------------------

# load all data
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

prj <- 4326

flcat <- list(
  code = c('7210', '9113', '9116', '9121'),
  name = c('sand', 'patchy', 'cont.', 'algae')
)

allsg <- list(
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
    data = purrr::map(data, function(x){

      x <- x %>%
        mutate(
          FLUCCS_CODE = factor(FLUCCS_CODE, levels = flcat$code, labels = flcat$name)
        ) %>%
        select(OBJECTID, Category = FLUCCS_CODE)


      st_crs(x) <- prj

      return(x)

    })
  )

# year to eval
inds <- crossing(
  segs = tbseg$bay_segment,
  yr = allsg$yr 
)

chgdat <- NULL
for(i in 1:nrow(inds)){

  # year pairs
  yr1 <- inds[i, ] %>% pull(yr)
  yr2 <- inds[i + 1, ] %>% pull(yr)

  if(yr1 == '2018')
    next()
  
  # segment
  seg <- inds[i, ] %>% pull(segs)
  
  toclp <- tbseg %>% 
    filter(bay_segment %in% !!seg)
  
  cat(paste(seg, yr1, yr2, sep = ', '), '\t')

  a <- allsg %>%
    filter(yr %in% !!yr1) %>%
    pull(data) %>%
    .[[1]] %>%
    .[toclp, ] %>%
    st_transform(crs = 26917) %>%
    select(Category) %>%
    st_union(by_feature=TRUE) %>%
    mutate(Category = paste0(Category, ', ', yr1))
  b <- allsg %>%
    filter(yr %in% !!yr2) %>%
    pull(data) %>%
    .[[1]] %>%
    .[toclp, ] %>%
    st_transform(crs = 26917) %>%
    select(Category) %>%
    st_union(by_feature=TRUE) %>%
    mutate(Category = paste0(Category, ', ', yr2))

  st_agr(a) = "constant"
  st_agr(b) = "constant"

  # get full union
  op1 <- st_difference(a,st_union(b))
  op2 <- st_difference(b, st_union(a)) %>%
    rename(Category.1 = Category)
  op3 <- st_intersection(a, b)

  union <- bind_rows(op1, op2, op3) %>%
    mutate(
      yr = unique(na.omit(sub('^.*\\,\\s([0-9]+)$', '\\1', Category))),
      yr.1 = unique(na.omit(sub('^.*\\,\\s([0-9]+)$', '\\1', Category.1))),
      Category = ifelse(is.na(Category), paste0('other, ', yr), as.character(Category)),
      Category.1 = ifelse(is.na(Category.1), paste0('other, ', yr.1), as.character(Category.1)),
      Acres = st_area(.),
      Acres = set_units(Acres, 'acres'),
      Acres = as.numeric(Acres)
    ) %>%
    select(-yr, -yr.1) %>%
    st_set_geometry(NULL) %>%
    select(Category.1, Category, Acres) %>%
    group_by(Category.1, Category) %>%
    summarise(Acres = sum(Acres)) %>%
    ungroup %>%
    select(source = Category, target = Category.1, value = Acres) %>%
    data.frame(stringsAsFactors = F)

  union$bay_segment <- seg
  chgdat <- bind_rows(chgdat, union)

}

save(chgdat, file = 'data/chgdat.RData', compress = 'xz')