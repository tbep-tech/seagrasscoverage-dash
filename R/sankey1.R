library(tidyverse)
library(tbeptools)
otb <- tbseg %>% 
  filter(bay_segment %in% 'OTB')
allsg


a <- allsg$data[[1]][otb, ] %>% 
  st_transform(crs = 26917) %>% 
  # group_by(Category) %>% 
  # summarise() %>% 
  select(Category) %>% 
  st_union(by_feature=TRUE)
b <- allsg$data[[15]][otb, ] %>% 
  st_transform(crs = 26917) %>% 
  # group_by(Category) %>% 
  # summarise() %>% 
  select(Category) %>% 
  st_union(by_feature=TRUE)




st_agr(a) = "constant" #to avoid warnings, but see https://github.com/r-spatial/sf/issues/406
st_agr(b) = "constant"

#Operations


op1 <- st_difference(a,st_union(b)) #notice the use of st_union()

op2 <- st_difference(b, st_union(a)) %>% 
  rename(Category.1 = Category)#notice the order of b and a and st_union()

op3 <- st_intersection(a, b) #notice the order of b and a


union <- bind_rows(op1, op2, op3) %>% 
  mutate(
    Category = ifelse(is.na(Category), 'other', as.character(Category)), 
    Category.1 = ifelse(is.na(Category.1), 'other', as.character(Category.1))
    ) %>% 
  mutate(
    Acres = st_area(.), 
    Acres = set_units(Acres, 'acres'),
    Acres = as.numeric(Acres)
  ) %>% 
  st_set_geometry(NULL)

sumdat <- union %>% 
  select(Category.1, Category, Acres) %>% 
  group_by(Category.1, Category) %>% 
  summarise(Acres = sum(Acres)) %>% 
  ungroup %>% 
  select(source = Category, target = Category.1, value = Acres) %>% 
  data.frame(stringsAsFactors = F) 
sumdat$target <- paste(sumdat$target, " ", sep="")

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(sumdat$source), as.character(sumdat$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
sumdat$IDsource=match(sumdat$source, nodes$name)-1 
sumdat$IDtarget=match(sumdat$target, nodes$name)-1

out <- sankeyNetwork(Links = sumdat, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", height = 400, width = 800,
                     sinksRight=FALSE, units = 'acres', nodeWidth=40, fontSize=13, nodePadding=5)

out

