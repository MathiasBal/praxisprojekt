
library(igraph)   # Paket für die Erstellung und Analyse von Netzwerken
library(dplyr)    

data_network <- nigeria.merged %>%  
  select(actor_group1, actor_group2) 

# Daten für das Netzwerk vorbereiten
edges <- data_network %>%
  mutate(
    # Stelle sicher, dass die Reihenfolge der Gruppen immer gleich ist
    # (damit "A → B" und "B → A" nicht als zwei verschiedene Verbindungen gezählt werden)
    from = pmin(actor_group1, actor_group2),  # Die alphabetisch kleinere Gruppe wird immer "from"
    to = pmax(actor_group1, actor_group2)     # Die alphabetisch größere Gruppe wird immer "to"
  ) %>%
  group_by(from, to) %>%  # Gruppiere die Daten nach den Akteursgruppen (jede Verbindung nur einmal zählen)
  summarise(weight = n(), .groups = 'drop')  # Zähle, wie oft jede Verbindung vorkommt für das Gewicht der Kante


network <- graph_from_data_frame(edges, directed = FALSE)  # directed = FALSE Es ist ein UNGERICHTETES Netzwerk (A ↔ B ist dasselbe wie B ↔ A)


set.seed(123)  #Setzt einen Zufallswert, damit das Layout jedes Mal gleich bleibt (sonst würde es sich zufällig ändern)

layout <- layout_with_fr(network, weights=E(network)$weight)  
# Wendet den Fruchterman-Reingold-Algorithmus an, um das Netzwerk schön anzuordnen
# Die Knoten werden so platziert, dass eng verbundene Gruppen nah beieinander liegen
# Die Kanten-Gewichte beeinflussen, wie nah die Knoten aneinander sind


plot(network, 
     layout = layout,   # Verwende das definierte Layout
     vertex.size = 10,  # Größe der Knoten (Punkte) festlegen
     vertex.label.cex = 0.9,  # Größe der Beschriftungen der Knoten
     vertex.label.color = "black",  # Farbe der Beschriftungen für bessere Lesbarkeit
     edge.width = (E(network)$weight / max(E(network)$weight)) * 5 + 1,  
     # Die Dicke der Kanten hängt von der Anzahl der Interaktionen ab (mehr Interaktionen = dickere Linie)
     # Gewicht wird so skaliert, dass die dickste Kante ca. 5-mal dicker ist als die dünnste
     main = "Interaction between different actor groups")  

                                           
