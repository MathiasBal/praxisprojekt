# fatatlities per year

# count deaths by year
fatalities_summary <- nigeria.merged %>%
  filter(!is.na(year)) %>% 
  group_by(year) %>% 
  summarise(total_fatalities = sum(fatalities, na.rm = TRUE))


# graph
ggplot(fatalities_summary, aes(x = as.factor(year), y = total_fatalities)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_fatalities), vjust = -0.5, size = 3.5) +
  labs(title = "Anzahl der Todesfälle pro Jahr in Nigeria",
       x = "Jahr",
       y = "Todesfälle") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(hjust = 0.5, size = 10))
