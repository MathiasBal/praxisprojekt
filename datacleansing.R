# Libraries
install.packages("stringr")
library(stringr)

# Data Prepping
head(anf.park.ws24)
anf.park.ws24$bj <- floor(anf.park.ws24$bj)
wl <- anf.park.ws24$WL
wl <- str_replace(wl, "beste", "3")
wl <- str_replace(wl, "gute", "2")
wl <- str_replace(wl, "durchschnittliche", "1")
anf.park.ws24 <- transform(anf.park.ws24, WL = as.integer(WL))
