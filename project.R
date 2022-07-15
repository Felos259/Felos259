#install.packages("readxl") 
library(readxl)

#install.packages("xlsx")
library("xlsx")

#install.packages("tidyverse")
library("tidyverse")

#install.packages("tidytext")
library("tidytext")


#rohdaten <- read_excel(file.choose())

# daten werden aus dem System ausgelesen als rohdaten
rohdaten <- read_excel("data/Ladevorgangs-Übersicht.xlsx", na = "NA")


daten <- subset(rohdaten, select=c('Ladepunkt ID', 'Zugangsmedium: Name', 'Beginn', 'Ende', 'Energie nach Korrektur', 'Dauer nach Korrektur'))
daten_clean <- na.omit(daten)

#Möglichkeit die verschiedenen ingenieurbüros zuzuweisen wird dem datensatz hinzugefügt 
daten_clean["identifier"] <- substring(daten_clean$'Zugangsmedium: Name', first = 1, last = 10)
    

#subsets mit den speziellen Angaben der Firmen werden eingespeichert         
datenBuH <-  daten_clean %>%
    filter(grepl('BuH GmbH', daten_clean$identifier))

datenIBBuH <-  daten_clean %>%
    filter(grepl('IB BuH', daten_clean$identifier) & !grepl('IB BuH NRW', daten_clean$identifier))

datenIBBuHNRW <-  daten_clean %>%
    filter(grepl('IB BuH NRW', daten_clean$identifier))

datenSonstige <- daten_clean %>%
  filter(!grepl('BuH', daten_clean$identifier))


#Datei wird erstellt
write.xlsx(rohdaten, file="Ladestatistik.xlsx", sheetName="Rohdaten")
write.xlsx(datenBuH, file="Ladestatistik.xlsx", sheetName="BuH GmbH", append=TRUE)
write.xlsx(datenIBBuH, file="Ladestatistik.xlsx", sheetName="IB BuH GmbH", append=TRUE)
write.xlsx(datenIBBuHNRW, file="Ladestatistik.xlsx", sheetName="IB BuH NRW", append=TRUE)
write.xlsx(datenSonstige, file="Ladestatistik.xlsx", sheetName="Sonstige", append=TRUE)
