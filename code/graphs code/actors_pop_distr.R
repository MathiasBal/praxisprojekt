
dataframe.actors.population <- nigeria.merged


dataframe.actors.population <- dataframe.actors.population %>% filter(!is.na(population_best)) %>%
  
mutate(
  actor_group1 = case_when(
    str_detect(actor_group1, "State Forces") ~ "Staatliche Sicherheitskräfte",
    str_detect(actor_group1, "Rebel Groups") ~ "Rebellengruppen",
    str_detect(actor_group1, "Political Militias") ~ "Politische Milizen",
    str_detect(actor_group1,"Identity Militias" ) ~ "Identitätsmilizen",
    str_detect(actor_group1, "Rioters") ~ "Aufständische",
    str_detect(actor_group1, "Protesters") ~ "Protestierende",
    str_detect(actor_group1, "Civilians") ~ "Zivilsten",
    str_detect(actor_group1, "External/Other Forces") ~"Externe/Sontsige Akteure")
  )


ggplot(dataframe.actors.population, aes(x = actor_group1, y = population_best)) +
  geom_boxplot(outlier.shape = NA, fill = "#779776", alpha = 0.6) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 125000)) + 
  labs(title = "Zusammenhang zwischen Akteursgruppen und der Bevölkerungsanzahl am Ereignisort",
       x = "Akteure",
       y = "Bevölkerung am Ereignisort") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
