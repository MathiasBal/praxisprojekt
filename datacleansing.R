# Libraries
install.packages("stringr")
library(stringr)

# Data Prepping
anf.park.ws24$bj <- floor(anf.park.ws24$bj)
wl <- anf.park.ws24$WL
wl <- str_replace(wl, "beste", "3")
wl <- str_replace(wl, "gute", "2")
wl <- str_replace(wl, "durchschnittliche", "1")
anf.park.ws24 <- transform(anf.park.ws24, WL = as.integer(WL))

# Third Normform
apt <- data.frame(
  apt_id = integer(),                           # primary key
  net_rent_per_qm = anf.park.ws24$nmqm,         # Nettokaltmiete je Quadratmeter in Euro
  space_per_qm = anf.park.ws24$wfl.gekappt,     # Wohnfläche in Quadratmetern
  year_of_construction = anf.park.ws24$bj,      # Baujahr
  landlord_type = anf.park.ws24$vermietertyp,   # Vermietertyp
  contract_type = anf.park.ws24$art.vertrag,    # Vertragsart
  residential_area = anf.park.ws24$WL,          # Wohnlage
  is_central = anf.park.ws24$Zentral,           # Zentral (ja/nein)
  is_new_contract = anf.park.ws24$Neuvertrag,   # Neuvertrag (ja/nein)
  house_type = anf.park.ws24$Haustyp,           # Haustyp
  building_type = anf.park.ws24$Gebäudetyp      # Gebäudetyp
)

district <- data.frame(
  district_no = anf.park.ws24$bezirk.nr,        # primary key
  district_name = anf.park.ws24$SBez            # Stadtbezirk Name
)

rent_index <- data.frame(
  rent_index_id = integer(),                    # primary key
  district_no = district$district_no,           # foreign key zu district_no$district_no
  avg_comparative_rent = anf.park.ws24$ovm21    # Mittlere Ortsübliche Vergleichsmiete gemäß Mietspiegel 2021
)

rental_agreement <- data.frame(
  rental_agreement_id = integer(),  # primary key
  apt_id = apt$id,          # Fremdschlüssel zu wohnung$wohnung_id
  rent_increase_month = anf.park.ws24$mieterhöhung_monat,  # Mieterhöhung Monat
  rent_increase_year = anf.park.ws24$mieterhöhung_jahr,   # Mieterhöhung Jahr
  start_lease_month = anf.park.ws24$beginn_mietverh_monat,  # Beginn Mietverhältnis Monat
  start_lease_year = anf.park.ws24$beginn_mietverh_jahr    # Beginn Mietverhältnis Jahr
)

year_of_construction_first_occupancy <- data.frame(
  year_of_construction = apt$year_of_construction,   # primary key Baujahr
  first_occupancy = anf.park.ws24$erstbezug          # Erstbezug Jahr
)
