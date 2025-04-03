#network graph actors

dataframe.network <- nigeria.merged %>%   
  select(actor_group1, actor_group2)  %>%
  mutate(
    actor_group1 = case_when(
      str_detect(actor_group1, "State Forces") ~ "Staatliche Sicherheitskräfte",
      str_detect(actor_group1, "Rebel Groups") ~ "Rebellengruppen",
      str_detect(actor_group1, "Political Militias") ~ "Politische Milizen",
      str_detect(actor_group1,"Identity Militias" ) ~ "Identitätsmilizen",
      str_detect(actor_group1, "Rioters") ~ "Aufständische",
      str_detect(actor_group1, "Protesters") ~ "Protestierende",
      str_detect(actor_group1, "Civilians") ~ "Zivilsten",
      str_detect(actor_group1, "External/Other Forces") ~"Externe/Sontsige Akteure")) %>%
  mutate(
    actor_group2 = case_when(
      str_detect(actor_group2, "State Forces") ~ "Staatliche Sicherheitskräfte",
      str_detect(actor_group2, "Rebel Groups") ~ "Rebellengruppen",
      str_detect(actor_group2, "Political Militias") ~ "Politische Milizen",
      str_detect(actor_group2,"Identity Militias" ) ~ "Identitätsmilizen",
      str_detect(actor_group2, "Rioters") ~ "Aufständische",
      str_detect(actor_group2, "Protesters") ~ "Protestierende",
      str_detect(actor_group2, "Civilians") ~ "Zivilsten",
      str_detect(actor_group2, "External/Other Forces") ~"Externe/Sontsige Akteure")
  )

# Network data
edges <- dataframe.network %>%  
  mutate(  
    from = pmin(actor_group1, actor_group2),    
    to = pmax(actor_group1, actor_group2)        
  ) %>%  
  group_by(from, to) %>%  
  summarise(weight = n(), .groups = 'drop')    

# create network 
#set seed so it doesnt change with reloads
network <- graph_from_data_frame(edges, directed = FALSE)    
set.seed(123)
layout <- layout_with_fr(network, weights = E(network)$weight)    

# actor colors
colors <- c("#E41A1C",    
            "lightblue",    
            "#4DAF4A",    
            "blue",    
            "#FF7F00",    
            "#FFFF33",    
            "#A65628",    
            "#F781BF")    

  
actor.types <- unique(c(edges$from, edges$to))  
actor.colors <- setNames(rep_len(colors, length(actor.types)), actor.types)  

# node size with scaling
node.strength <- strength(network, mode = "all")  
V(network)$size <- (node.strength / max(node.strength)) * 30 + 5    

# node colors  
V(network)$color <- actor.colors[V(network)$name]  

# plotting network
plot.network <- plot(network,   
                     layout = layout,    
                     vertex.size = V(network)$size,    
                     vertex.color = V(network)$color,    
                     vertex.label = NA,     
                     edge.width = (E(network)$weight / max(E(network)$weight)) * 5 + 1,     
                     main = "Gewaltinteraktion zwischen den Akteuren")    

# graph legend  
legend("topright",   
       legend = names(actor.colors),   
       col = actor.colors,   
       pch = 19,     
       cex = 0.8,   
       title = "Akteure")  
