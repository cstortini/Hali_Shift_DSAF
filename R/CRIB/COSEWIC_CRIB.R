polcrib<-read.csv("Data/CRIB/crib_pollock.csv")
hercrib<-read.csv("Data/CRIB/crib_herring.csv")
polcrib$ToE.year<-2015+(-log(polcrib$E.Time.of.climate.emergence)/0.033) #calculate raw ToE's from standardized
hercrib$ToE.year<-2015+(-log(hercrib$E.Time.of.climate.emergence)/0.033) #calculate raw ToE's from standardized
