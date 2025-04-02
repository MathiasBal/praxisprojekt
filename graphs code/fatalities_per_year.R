# fatatlities per year


# count deaths by year
fatalities.summary <- nigeria.merged %>%
  filter(!is.na(year) & year != 2025) %>% 
  group_by(year) %>% 
  summarise(total_fatalities = sum(fatalities, na.rm = TRUE))


# graph
ggplot(fatalities.summary, aes(x = as.factor(year), y = total_fatalities)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Todesfälle pro Jahr in Nigeria",
       x = "Jahr",
       y = "Anzahl der Todesfälle") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(hjust = 0.5, size = 10)) +
  scale_x_discrete(breaks = c(1997, 2000, 2005, 2010, 2015, 2020, 2024))

