#association between actors and population size

#removing nas for population size and translating actor groups
dataframe.actors.population <- nigeria.restructured %>% 
  filter(!is.na(population_best)) %>%
  mutate(
    actor_group1 = case_when(
      str_detect(actor1, "military forces of nigeria|police forces of nigeria|government of nigeria|
                 nigeria customs service|NSCDC: Nigeria Security and Civil Defence Corps|
                 national drug law enforcement agency|nigeria immigration service|multinational joint task force|
                 military forces of chad|military forces of niger|military forces of cameroon") ~ "Staatliche Sicherheitskräfte",
      str_detect(actor1, "boko haram|islamic state west africa province|ansaru|islamic state sahel province") ~ "Rebellengruppen",
      str_detect(actor1, "unidentified armed group|indigenous peoples of biafra|OPC: Oodua Peoples Congress|
                 ijaw freedom fighters|yoruba nation agitators|NDLM: Niger Delta Liberation Movement") ~ "Politische Milizen",
      str_detect(actor1, "communal militia|ethnic militia|fulani ethnic militia|tiv ethnic militia|
                 jukun ethnic militia|irigwe ethnic militia|alago ethnic militia|gbagyi ethnic militia|
                 yoruba ethnic militia|ambu ethnic militia|eki ethnic militia") ~ "Identitätsmilizen",
      str_detect(actor1, "rioters") ~ "Aufständische",
      str_detect(actor1, "protesters|nigeria labour congress") ~ "Protestierende",
      str_detect(actor1, "civilians") ~ "Zivilisten",
      str_detect(actor1, "private security forces|african nature investors|KSVG: Katsina State Vigilance Group|
                 ebube agu corps|amotekun corps|zamfara state community protection guards") ~ "Externe/sonstige Akteure",
      TRUE ~ "Externe/sonstige Akteure")
  )


# boxplot
ggplot(dataframe.actors.population, aes(x = actor_group1, y = population_best)) +
  geom_boxplot(outlier.shape = NA, fill = "#779776", alpha = 0.6) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 125000)) + 
  labs(title = "Zusammenhang zwischen Akteursgruppen und der Bevölkerungsanzahl am Ereignisort",
       x = "Akteure",
       y = "Bevölkerung am Ereignisort") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
