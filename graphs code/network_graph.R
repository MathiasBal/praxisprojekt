data_network <- nigeria.merged %>%   
  select(actor_group1, actor_group2)  

# Netzwerk-Daten 
edges <- data_network %>%  
  mutate(  
    from = pmin(actor_group1, actor_group2),    
    to = pmax(actor_group1, actor_group2)        
  ) %>%  
  group_by(from, to) %>%  
  summarise(weight = n(), .groups = 'drop')    

# Netzwerk erstellen  
network <- graph_from_data_frame(edges, directed = FALSE)    
set.seed(123)    #Verhindert dass sich die Position bei jedem Neuladen ändert
layout <- layout_with_fr(network, weights = E(network)$weight)    


colors <- c("#E41A1C",    
            "lightblue",    
            "#4DAF4A",    
            "blue",    
            "#FF7F00",    
            "#FFFF33",    
            "#A65628",    
            "#F781BF")    

# Farben den Akteuren zuweisen  
actor_types <- unique(c(edges$from, edges$to))  
actor_colors <- setNames(rep_len(colors, length(actor_types)), actor_types)  

# Knotengröße anhand der Summe der Gewichtungen (node strength)  
node_strength <- strength(network, mode = "all")  
V(network)$size <- (node_strength / max(node_strength)) * 30 + 5   # Skalierung  

# Knotenfarben zuweisen  
V(network)$color <- actor_colors[V(network)$name]  

# Netzwerk plotten  
plot(network,   
     layout = layout,    
     vertex.size = V(network)$size,    
     vertex.color = V(network)$color,    
     vertex.label = NA,   # Keine Labels direkt an den Knoten  
     edge.width = (E(network)$weight / max(E(network)$weight)) * 5 + 1,     
     main = "Gewaltinteraktion zwischen den Akteuren")    

# Legende  
legend("topright",   
       legend = names(actor_colors),   
       col = actor_colors,   
       pch = 19,   # Kreissymbol für die Legende  
       cex = 0.8,   
       title = "Akteure")  
