# get change over time by complete union
chgfun <- function(crpsel){
  
  yrs <- crpsel %>% 
    pull(yr)
  
  out <- NULL
  for(i in seq_along(yrs)){
    
    # year pairs
    yr1 <- yrs[i] 
    yr2 <- yrs[i + 1] 
    
    if(yr1 == '2018')
      next()

    cat(paste(yr1, yr2, sep = ', '), '\t')
    
    a <- crpsel %>%
      filter(yr %in% !!yr1) %>%
      pull(data) %>%
      .[[1]] %>%
      st_transform(crs = 26917) %>%
      select(Category) %>%
      st_union(by_feature=TRUE) %>%
      mutate(Category = paste0(Category, ', ', yr1))
    b <- crpsel %>%
      filter(yr %in% !!yr2) %>%
      pull(data) %>%
      .[[1]] %>%
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
    
    out <- bind_rows(out, union)
    
  }
  
  return(out)
  
}

# sankey plot function
sanplofun <- function(toplo, nodepd = 100){
  
  nodes <- data.frame(name = c(as.character(toplo$source), as.character(toplo$target))) %>% 
    unique() %>% 
    mutate(
      group = gsub('\\,\\s[0-9]+$', '', name), 
      group = factor(group, levels = c("algae", "cont.", "patchy", "other", "sand"))
    )
  
  # cols <- c('brown', 'darkgreen', 'dodgerblue', 'green', 'tan')
  cols <- c("#A52A2A", "#006400", "#1E90FF", "#00FF00", "#D2B48C")
  names(cols) <- c("algae", "cont.", "other", "patchy", "sand")
  
  cols <- 'd3.scaleOrdinal() .domain(["algae", "cont.", "patchy", "other", "sand"]) .range(["#A52A2A", "#006400", "#00FF00", "#1E90FF", "#D2B48C"])'
  
  toplo$IDsource=match(toplo$source, nodes$name)-1 
  toplo$IDtarget=match(toplo$target, nodes$name)-1
  
  out <- sankeyNetwork(Links = toplo[, -c(1, 2)], Nodes = nodes, NodeGroup = "group", colourScale = cols,
                       Source = "IDsource", Target = "IDtarget",
                       Value = "value", NodeID = "name", 
                       sinksRight=F, units = 'acres', nodeWidth = 20, fontSize = 12, nodePadding = nodepd, iterations = 3)
  
  return(out)
  
}

# bar plot function, plotly
barplofun <- function(toplo, cols, fontin = 'Lato Light'){
  
  p <- plot_ly(toplo, x = ~yr, y= ~Acres, color = ~Category, text = ~paste0(yr, ', ', Category, ', ', round(Acres, 0), ' acres'),
               hoverinfo = 'text',  colors = cols, alpha = 0.7) %>%
    add_bars() %>%
    layout(
      yaxis = list(title = 'Acres', gridcolor = '#ECECEC'),
      xaxis = list(title = ''),
      legend = list(x = 0, y = 1.1),
      barmode = 'stack',
      showlegend = T,
      font = list(family = fontin),
      plot_bgcolor = '#FFFFFF'
    )
  
  return(p)
  
}

# reactable table from bar pots
bartabfun <- function(tab){
  
  out <- reactable(tab,
    columns = list(
      yr = colDef(name = 'Year'),
      Total = colDef(name = 'Total (acres)')
    ),
    defaultColDef = colDef(
      footerStyle = list(fontWeight = "bold"),
      format = colFormat(digits = 0, separators = TRUE),
      resizable = TRUE
    ),
    filterable = T,
    defaultPageSize = 12
  )
  
  return(out)
  
}