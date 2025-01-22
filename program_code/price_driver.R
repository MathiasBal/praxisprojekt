# Annahme: Der DataFrame "anf_park_ws24" ist bereits im Arbeitsspeicher
# "Preistreiber" berechnen

pricedrivers <- anf.prak.ws24 %>%
  filter(net_rent_per_qm > avg_comparative_rent)

# 2 DFs zum ausrechnen wer seit wann zu viel zahlt

pd_mieterhöhung <- pricedrivers %>% 
  filter(!is.na(rent_increase_year)) %>% 
  select("rent_increase_year", "rent_increase_month", "start_lease_year")
pd_mieterhöhung <- pd_mieterhöhung %>% 
  mutate("overrent_start" = as.factor(pd_mieterhöhung[["rent_increase_year"]]))

pd_anfangsmiete <- pricedrivers %>% 
  filter(is.na(rent_increase_year) & is.na(rent_increase_month)) %>% 
  select("rent_increase_year", "rent_increase_month", "start_lease_year")
pd_anfangsmiete <- pd_anfangsmiete %>% 
  mutate("overrent_start" = as.factor(pd_anfangsmiete[["start_lease_year"]])) 

pd_miete_jahr <- rbind(pd_anfangsmiete, pd_mieterhöhung)

ggplot(data = pd_miete_jahr, aes(x = overrent_start)) +
  geom_bar(fill = "green", color = "black") +
  labs(
    title = "Anzahl der erschienenen Preistreiber pro Jahr",
    x = "Jahr",
    y = "Anzahl"
  ) +
  theme_minimal()
