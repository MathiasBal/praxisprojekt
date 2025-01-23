# Annahme: Der DataFrame "anf_park_ws24" ist bereits im Arbeitsspeicher
# "Preistreiber" berechnen

pricedrivers <- anf.prak.ws24 %>%
  filter(net_rent_per_qm > avg_comparative_rent)
