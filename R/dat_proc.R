library(tidyverse)
library(sf)
library(tbeptools)
library(mapview)
library(doParallel)
library(foreach)
library(units)

# download raw data, clip, and save ---------------------------------------

prj <- 4326

# all zipped files on amazon s3
# downloaded from here https://data-swfwmd.opendata.arcgis.com/
fls <- c('88', '90', '92', '94', '96', '99', '01', '04', '06', '08', '10', '12', '14', '16', '18', '20') %>% 
  paste0('https://swfwmd-seagrass.s3.amazonaws.com/sg', ., '.zip')

for(i in 1:length(fls)){
  
  cat(i, 'of', length(fls), '\n')
  
  ## import file
  
  # download from s3, unzip
  tmpdir <- here('data/tmp')
  tmpzip <- here('data/tmp/tmp.zip')
  dir.create(tmpdir)
  download.file(fls[i], destfile = tmpzip)
  unzip(tmpzip, exdir = tmpdir)
  
  # import shapefile
  toimp <- list.files(tmpdir, pattern = '\\.shp$', full.names = T)
  dat_raw <- st_read(toimp, quiet = T)
  
  # delete files
  unlink(tmpdir, recursive = T)
  
  if(any(c('FLUCCS_CODE', 'FLUCCS_COD') %in% names(dat_raw)))
    names(dat_raw) <- gsub('^FLUCCS\\_COD$|^FLUCCS\\_CODE$', 'FLUCCSCODE', names(dat_raw))

  # clip bounaries
  segs <- st_read('~/Desktop/TBEP/GISboundaries/GISboundaries/TBEP_Bay_Segments_Correct_Projection.shp') %>% 
    st_transform(crs = prj)
  
  # crop by watershed and select fluccs
  # 9113 is patchy, 9116 is continuous
  dat_crp <- dat_raw %>%
    st_transform(crs = prj) %>%
    .[segs, ] %>% 
    filter(FLUCCS_CODE %in% c(9113, 9116, 7210, 9121)) %>% 
    select(OBJECTID, FLUCCS_CODE) %>% 
    st_buffer(dist = 0)

  # name assignment and save
  # name assignment and save
  flnm <- gsub('^sg|\\.zip$', '', basename(fls[i])) %>% 
    as.numeric
  if(flnm > 80){
    flnm <- paste0('19', flnm)
  } else {
    flnm <- paste0('20', sprintf("%02d", flnm))
  }
  flnm <- paste0('sgdat', flnm)
  assign(flnm, dat_crp)
  save(list = flnm, file = here('data', paste0('/', flnm, '.RData')), compress = 'xz')
  
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
data(sgdat2020)

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
  `2018` = sgdat2018,
  `2020` = sgdat2020
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

  if(yr1 == '2020')
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

# simplify polygons for mapping -------------------------------------------

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
data(sgdat2020)

prj <- 4326

flcat <- list(
  code = c('7210', '9113', '9116', '9121'),
  name = c('sand', 'patchy', 'cont.', 'algae')
)

list(
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
  `2018` = sgdat2018,
  `2020` = sgdat2020
  ) %>%
  enframe('yr', 'data') %>%
  mutate(
    data = purrr::pmap(list(yr, data), function(yr, data){
      
      x <- data %>%
        mutate(
          FLUCCS_CODE = factor(FLUCCS_CODE, levels = flcat$code, labels = flcat$name)
        ) %>%
        select(OBJECTID, Category = FLUCCS_CODE)
      
      st_crs(x) <- prj
      
      x <- st_simplify(x, dTolerance = 0.0001)
      
      # name assignment and save
      flnm <- paste0('sgdat', yr, 'simp')
      assign(flnm, x)
      save(list = flnm, file = paste0('data/', flnm, '.RData'), compress = 'xz')
      
      return(x)
      
    })
  )

# bar plot data for segments ----------------------------------------------

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
data(sgdat2020)

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
  `2018` = sgdat2018,
  `2020` = sgdat2020
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

segs <- tbseg$bay_segment

bardat <- NULL
for(i in seq_along(segs)){
  
  cat(i, '\n')
  
  seg <- segs[i]
  toclp <- tbseg %>% 
    filter(bay_segment %in% !!seg)
  
  clp <- allsg %>%
    mutate(
      data = purrr::map(data, function(x){
        
        x <- st_intersection(toclp, x) %>%
          mutate(
            area = st_area(.)
          ) %>% 
          st_set_geometry(NULL)
        
        return(x)
        
      })
    ) %>% 
    unnest('data') %>%
    group_by(yr, Category) %>%
    summarise(Acres = sum(area)) %>%
    ungroup %>%
    mutate(
      Acres = set_units(Acres, 'acres'), 
      bay_segment = seg
      )
  
  bardat <- rbind(bardat, clp)
  
}

save(bardat, file = 'data/bardat.RData', compress = 'xz')
