library(tidyverse)
library(tbeptools)
otb <- tbseg %>% 
  filter(bay_segment %in% 'OTB')


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

inds <- allsg$yr

toplo <- NULL
for(i in seq_along(inds)){
  
  if(i == length(inds))
    break()
  
  ind1 <- inds[i]
  ind2 <- inds[i + 1]
  
  cat(paste(ind1, ind2, sep = ', '), '\t')
  
  a <- allsg %>% 
    filter(yr %in% !!ind1) %>% 
    pull(data) %>% 
    .[[1]] %>% 
    .[otb, ] %>% 
    st_transform(crs = 26917) %>% 
    select(Category) %>% 
    st_union(by_feature=TRUE) %>% 
    mutate(Category = paste0(Category, ', ', ind1))
  b <- allsg %>% 
    filter(yr %in% !!ind2) %>% 
    pull(data) %>% 
    .[[1]] %>% 
    .[otb, ] %>% 
    st_transform(crs = 26917) %>% 
    select(Category) %>% 
    st_union(by_feature=TRUE) %>% 
    mutate(Category = paste0(Category, ', ', ind2))
  
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
  
  toplo <- bind_rows(toplo, union)
  
}

# # can I arrange by factors?
# toplo2 <- toplo %>% 
#   mutate(
#     srccat = gsub('\\,\\s[0-9]+$', '', source), 
#     srcyr = gsub('^.*,\\s([0-9])*$', '\\1', source), 
#     trgcat = gsub('\\,\\s[0-9]+$', '', target),
#     trgyr = gsub('\\,\\s([0-9]+)$', '\\1', target)
#   )

nodes <- data.frame(name=c(as.character(toplo$source), as.character(toplo$target)) %>% unique())

toplo$IDsource=match(toplo$source, nodes$name)-1 
toplo$IDtarget=match(toplo$target, nodes$name)-1


# nodes$name <- factor(nodes$name, levels = c('continuous', 'patchy', 'other', 'attached algae', 'sand'))

out <- sankeyNetwork(Links = toplo[, -c(1, 2)], Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", height = 600, width = 2000,
                     sinksRight=FALSE, units = 'acres', nodeWidth = 20, fontSize = 8, nodePadding =0)

out

