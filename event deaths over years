library(ggplot2)
library(dplyr)

# Aggregation
nigeria_event_deaths <- nigeria.no.nas %>%
  group_by(year, event_type) %>%
  summarise(total_fatalities = sum(fatalities))

# Stacked Area Chart
plot <- ggplot(nigeria_event_deaths, aes(x = year, y = total_fatalities, fill = event_type)) +
  geom_area(alpha = 0.7) +
  scale_x_continuous(breaks = seq(min(nigeria_event_deaths$year), max(nigeria_event_deaths$year), by = 1)) +
  
  labs(title = "Distribution of Fatalities by Event Type Over the Years",
       x = "Year",
       y = "Total Fatalities",
       fill = "Event Type")+
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


print(plot)


