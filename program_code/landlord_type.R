install.packages("forcats")
library(forcats)
### type of landlord
landlords <- pricedrivers %>% 
  select(landlord_type)
landlords$landlord_type <- factor(landlords$landlord_type, levels = c("1", "2", "3", "4", "5"),
                                  labels = c("Privater Vermieter",
                                             "Genossenschaft",
                                             "Städtischer Vermieter",
                                             "Wohnungsbaugesellschaft",
                                             "keine Angabe"))

### count of bars
landlords_count <- landlords %>% 
  group_by(landlord_type) %>% 
  summarise(count = n())

### graph different landlord types
ggplot(data = landlords, aes(x = fct_infreq(landlord_type))) +
  geom_bar(fill = "skyblue") +
  labs(x = "Vermietertyp", y = "Anzahl") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  geom_text(data = landlords_count, aes(y = count, label = count),
            size = 3.5, vjust = -0.5)