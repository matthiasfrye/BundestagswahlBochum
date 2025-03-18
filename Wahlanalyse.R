# /************************************************************************************/
# /************************************************************************************/
# /************************************************************************************/
# /**                                                                                **/
# /** Project Analyse Bundestagswahl für Bochum                                      **/
# /**                                                                                **/
# /** Combined R Script and R Markdown                                               **/
# /**                                                                                **/
# /** generate .pdf and .Rmd files with                                              **/
# /**    rmarkdown::render("Wahlanalyse.R",clean=FALSE)                              **/
# /**                                                                                **/
# /************************************************************************************/
# /************************************************************************************/
# /************************************************************************************/
#' ---
#' title: "Analyse der Bundestagswahl 2025 für die Grünen in Bochum "
#' author: "Matthias Frye"
#' date: "`r Sys.Date()`"
#' output: 
#'   pdf_document: 
#'       toc: yes
#'       number_sections: yes
#'       toc_depth: 3
#'       fig_width: 7 
#'       fig_height: 6
#' lang: de
#' urlcolor: blue
#' ---
#'
#'
#'
# /************************************************************************************/
# /************************************************************************************/
# /************************************************************************************/
# /**                                                                                **/
#' # Zusammenfassung
#' 
#' Die Grünen haben in Bochum leicht bessere Ergebnisse als im Bund erzielt, mussten jedoch gegenüber 2021 ebenfalls Verluste hinnehmen,
#' die absolut gesehen vor allem in der Stadtmitte hoch waren. Die Grünen haben in Bochum vor allem an die Linke verloren haben und die Schwäche der SPD nicht nutzen konnten.
#' Die Grünen haben vergleichsweise bessere Ergebnisse in Stimmbezirken mit hoher Wahlbeteiligung erzielt. 
#' 
#' Die Analyse der Stimmbezirke der Stadt Bochum in Kombination mit ausgewählten soziodemographischen Faktoren aus dem Sozialbericht 2024 
#' zeigt auf, dass die Grünen in Bochum bei Haushalten mit Kindern insgesamt besonders schlecht abgeschnitten haben, allerdings hier im Vergleich zu 2021 keine 
#' weiteren Verluste hinnehmen müssen. Die Grünen haben ihre stärksten Ergebnisse in Gebieten mit niedriger Arbeitslosigkeit und hohem Bildungsstand erzielt.
#' 
#' Briefkasten- und Haustürwahlkampf hatten einen schwachen, positiven Effekt auf das Wahlergebnis, allerdings muss dieses Ergebnis aufgrund der
#' Datenqualität mit einer gewissen Vorsicht betrachtet werden.
#' 
#' 
#' # Einleitung und Methode
#' Angeregt durch die Detailanalyse der Bundestagswahl 2025 in [Zeit Online](https://www.zeit.de/politik/deutschland/2025-02/wahlergebnisse-wahlkreise-bundestagswahl-daten-grafik)
#' werden die Ergebnisse für Bochum unter Berücksichtigung ausgewählter soziodemographischer Faktoren analysiert.
#' Dazu werden die Zweitstimmen der Stimmbezirke (Quelle: [www.regioit.de](https://wahlen.regioit.de/3/bt2025/05911000/praesentation/ergebnis.html?wahl_id=102&stimmentyp=1&id=ebene_3_id_2))
#' mit ausgewählten Daten des [Sozialberichts 2024](https://www.bochum.de/C125830C0042AB74/vwContentByKey/W2DB29JD041BOCMDE/$File/BochumerSozialbericht2024.pdf) der Stadt Bochum kombiniert.
#' Um die Auswirkungen des Wahlkampfs zu analysieren, wurden die Aktivitäten des Briefwahl- und Haustürwahlkampfs im Straßenverzeichnis mit der Wahlkampf-App abgeglichen und mit den Wahlergebnissen verknüoft.
#' 
#' Die Ergebnisse der Briefwahllokale wurden dabei auf die Stimmbezirke proportional zur Zahl der Wahlberechtigten verteilt,
#' um auswertbare Daten auf Ebene der Stimmbezirke zu erhalten. Dies stellt eine vereinfachende Annahme dar, da naturgemäß die Briefwahlanteile der Stimmbezirke innerhalb eines 
#' Kommunalwahlbezirks variieren.
#' 
#' Die Daten des Sozialberichts liegen nur auf Stadtteilebene (statistische Bezirke) vor und wurden jeweils allen Stimmbezirken eines Stadtteils zugeordnet.
#' Daraus ergibt sich eine weitere Ungenauigkeit, zudem die statistischen Bezirke von den Kommunalwahlbezirken geringfügig abweichen. Auf eine aufwändige Zuordnung der einzelnen Stimmbezirke
#' zu statistischen Bezirken wurde aufgrund des unverhältnismäßig hohen Aufwands verzichtet. 
#' 
#' Beim Wahlkampf wurde die Anzahl der Gebäude je Straßenabschnitt anhand der Gebäudedaten in OpenStreetMap ermittelt. 
#' 
#'  
# /************************************************************************************/
# /************************************************************************************/
# /************************************************************************************/
#' 
#+ echo=FALSE, warning=FALSE, message=FALSE,include=FALSE

#load tidyverse and other packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dslabs")) install.packages("dslabs")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("titanic")) install.packages("titanic")
if (!require("gtools")) install.packages("gtools")
if (!require("rvest")) install.packages("rvest")
if (!require("purrr")) install.packages("purrr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("tidyr")) install.packages("tidyr")
if (!require("scales")) install.packages("scales")
if (!require("tidytext")) install.packages("tidytext")
if (!require("pdftools")) install.packages("pdftools")
if (!require("mosaic")) install.packages("mosaic")
if (!require("readr")) install.packages("readr")
if (!require("gplots")) install.packages("gplots")
if (!require("readxl")) install.packages("readxl")
if (!require("rmarkdown")) install.packages("rmarkdown")
if (!require("sf")) install.packages("sf")
if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("geodata")) install.packages("geodata")
if (!require("tidygeocoder")) install.packages("tidygeocoder")
if (!require("st")) install.packages("st")
if (!require("osmdata")) install.packages("osmdata")
if (!require("osmextract")) install.packages("osmextract")

# now load the libraries
library(tidyverse) #load also ggplot2
library(dslabs)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(titanic)
library(gtools)
library(rvest)
library(purrr)
library(lubridate)
library(tidyr)
library(scales)
library(tidytext)
library(pdftools)


library(mosaic) 
library(readr)
library(gplots)
library(readxl)
library(rmarkdown)

library(sf)
library(rnaturalearth)
library(geodata)
library(tidygeocoder)
library(st)
library(osmdata)
library(osmextract)

# set working directory
setwd("~/Documents/GoogleDrive/Harvard/LearningR/BundestagswahlBochum")


# Wahlergebnise 2025
# gebiet-nr : Nummer des Wahlgebiets
# A : Wahlberechtigte insgesamt
# F : Gültige Zweitstimmen
# F1 : SPD
# F2 : CDU
# F3 : Grüne
# F4 : FDP
# F5 : AfD
# F6 : Linke
# F12 : Volt

filename <- "Open-Data-05911000-Wahl-zum-Deutschen-Bundestag-Wahlbezirk.csv"
btw25 <- read_delim(filename, delim=";", escape_double = FALSE, trim_ws = TRUE)
 

# # Gesamtergebnis prüfen
# btw25 %>%  
#   summarize(Berechtigt=sum(A, na.rm=T),
#             Gültig=sum(F),
#             SPD=sum(F1), 
#             CDU=sum(F2),
#             AfD=sum(F5),
#             Grüne=sum(F3),
#             Linke=sum(F6),
#             Volt=sum(F12))

# Daten laden und Kommunalbezirk, Stadtbezirk berechnen
dat25 <- btw25  %>%
        mutate(Wahl=wahl,
               Stimmbezirk=`gebiet-nr`,
               Berechtigt=A,
               Gültig=F,
               SPD=F1,
               CDU=F2,
               Grüne=F3,
               FDP=F4,
               AfD=F5,
               Linke=F6,
               Volt=F12,
               Kommunalbezirk=ifelse(
                 Stimmbezirk >= 9000,
                 substring(as.character(Stimmbezirk), 2, 3), 
                 substring(as.character(Stimmbezirk), 1, 2)),
               Stadtbezirk=substring(Kommunalbezirk, 1, 1)) %>% 
       select(Wahl,Stimmbezirk,Kommunalbezirk,Stadtbezirk,Berechtigt,Gültig,SPD,CDU,FDP,Grüne,AfD,Linke,Volt)

# # Gesamtergebnis prüfen
# dat25 %>%  
# summarize(Gültig=sum(Gültig),
#           SPD=sum(SPD), 
#           CDU=sum(CDU),
#           AfD=sum(AfD),
#           Grüne=sum(Grüne),
#           Linke=sum(Linke),
#           Volt=sum(Volt))

# Summe der gültigen Stimmen der Wahllokale und der Stimmen aus der Briefwahl bilden
Kommunalbezirk25 <- dat25 %>% 
  group_by(Kommunalbezirk) %>%
  summarize(Berechtigt_lokal=sum(ifelse(Stimmbezirk < 9000,Berechtigt,0)),
            Gültig_kommunal=sum(ifelse(Stimmbezirk >= 9000,Gültig,0)),
            SPD_kommunal=sum(ifelse(Stimmbezirk >= 9000,SPD,0)),
            CDU_kommunal=sum(ifelse(Stimmbezirk >= 9000,CDU,0)),
            FDP_kommunal=sum(ifelse(Stimmbezirk >= 9000,FDP,0)),
            Grüne_kommunal=sum(ifelse(Stimmbezirk >= 9000,Grüne,0)),
            AfD_kommunal=sum(ifelse(Stimmbezirk >= 9000,AfD,0)),
            Linke_kommunal=sum(ifelse(Stimmbezirk >= 9000,Linke,0)),
            Volt_kommunal=sum(ifelse(Stimmbezirk >= 9000,Volt,0)),
            .groups = "keep")

# # Überprüfung der Korrektheit der Aggregation
# Kommunalbezirk25 %>% filter(Kommunalbezirk=="10")
# dat25 %>%  filter((Kommunalbezirk=="10") & (Stimmbezirk >= 9000))
# dat25 %>%  filter((Kommunalbezirk=="10") & (Stimmbezirk < 9000))

# Verteilung der Briefwahlstimmen eines jeden Kommunalbezirks auf die Stimmen der Wahllokale
dat25 <- dat25 %>%
  left_join(Kommunalbezirk25, by = "Kommunalbezirk") %>%
  mutate(Gültig_all = ifelse(Stimmbezirk < 9000, Gültig + Berechtigt/Berechtigt_lokal * Gültig_kommunal, 0),
         SPD_all = ifelse(Stimmbezirk < 9000, SPD + Berechtigt/Berechtigt_lokal * SPD_kommunal, 0),
         CDU_all = ifelse(Stimmbezirk < 9000, CDU + Berechtigt/Berechtigt_lokal * CDU_kommunal, 0),
         FDP_all = ifelse(Stimmbezirk < 9000, FDP + Berechtigt/Berechtigt_lokal * FDP_kommunal, 0),
         Grüne_all = ifelse(Stimmbezirk < 9000, Grüne + Berechtigt/Berechtigt_lokal * Grüne_kommunal, 0),
         AfD_all = ifelse(Stimmbezirk < 9000, AfD + Berechtigt/Berechtigt_lokal * AfD_kommunal, 0),
         Linke_all = ifelse(Stimmbezirk < 9000, Linke + Berechtigt/Berechtigt_lokal * Linke_kommunal, 0),
          Volt_all = ifelse(Stimmbezirk < 9000, Volt + Berechtigt/Berechtigt_lokal * Volt_kommunal, 0))


# # Überprüfung der Korrektheit der Verteilung
# dat25 %>%  filter(Kommunalbezirk=="10") %>%
#   group_by(Kommunalbezirk) %>%
#   summarize(Berechtigt=sum(Berechtigt, na.rm=T),
#             Gültig=sum(Gültig),SPD=sum(SPD),CDU=sum(CDU),AfD=sum(AfD),Grüne=sum(Grüne),Linke=sum(Linke),Volt=sum(Volt),
#             Gültig_all=sum(Gültig_all),SPD_all=sum(SPD_all), CDU_all=sum(CDU_all),AfD_all=sum(AfD_all),Grüne_all=sum(Grüne_all) ,Linke_all=sum(Linke_all),Volt_all=sum(Volt_all),
#             .groups = "keep")

# # Vergleich der Gesamtergebnisse
# dat25 %>%  
#   summarize(Berechtigt=sum(Berechtigt, na.rm=T),
#             Gültig=sum(Gültig_all),
#             SPD=sum(SPD_all), 
#             CDU=sum(CDU_all),
#             AfD=sum(AfD_all),
#             Grüne=sum(Grüne_all),
#             Linke=sum(Linke_all),
#             Volt=sum(Volt_all),
#             .groups = "keep")
  
# Wahlergebnisse 2021
# gebiet-nr : Nummer des Wahlgebiets
# A : Wahlberechtigte insgesamt
# F : Gültige Zweitstimmen
# F1 : CDU
# F2 : SPD
# F3 : FDP
# F4 : AfD
# F5 : Grüne
# F6 : Linke
# F27 : Volt
filename <- "Open-Data-Bundestagswahl936.csv"
btw21 <- read_delim(filename, delim=";", escape_double = FALSE, trim_ws = TRUE)

# # Gesamtergebnis prüfen
# btw21 %>%  
#   summarize(Gültig=sum(F),
#             SPD=sum(F1), 
#             CDU=sum(F2),
#             Grüne=sum(F5),
#             FDP=sum(F3),
#             Linke=sum(F6),
#             Volt=sum(F12))

# Daten laden und Kommunalbezirk berechnen
dat21 <- btw21 %>%
  mutate(Wahl=wahl,
         Stimmbezirk=`gebiet-nr`,
         Berechtigt=A,
         Gültig=F,
         SPD=F2,
         CDU=F1,
         Grüne=F5,
         FDP=F3,
         AfD=F4,
         Linke=F6,
         Volt=F27,
         Kommunalbezirk=ifelse(
           Stimmbezirk >= 9000,
           substring(as.character(Stimmbezirk), 2, 3), 
           substring(as.character(Stimmbezirk), 1, 2)),
         Stadtbezirk=substring(Kommunalbezirk, 1, 1)) %>% 
  select(Wahl,Stimmbezirk,Kommunalbezirk,Stadtbezirk,Berechtigt,Gültig,SPD,CDU,FDP,Grüne,AfD,Linke,Volt)

# Summe der gültigen Stimmen der Wahllokale und der Stimmen aus der Briefwahl bilden
Kommunalbezirk21 <- dat21 %>% 
  group_by(Kommunalbezirk) %>%
  summarize(Berechtigt_lokal=sum(ifelse(Stimmbezirk < 9000,Berechtigt,0)),
            Gültig_kommunal=sum(ifelse(Stimmbezirk >= 9000,Gültig,0)),
            SPD_kommunal=sum(ifelse(Stimmbezirk >= 9000,SPD,0)),
            CDU_kommunal=sum(ifelse(Stimmbezirk >= 9000,CDU,0)),
            FDP_kommunal=sum(ifelse(Stimmbezirk >= 9000,FDP,0)),
            Grüne_kommunal=sum(ifelse(Stimmbezirk >= 9000,Grüne,0)),
            AfD_kommunal=sum(ifelse(Stimmbezirk >= 9000,AfD,0)),
            Linke_kommunal=sum(ifelse(Stimmbezirk >= 9000,Linke,0)),
            Volt_kommunal=sum(ifelse(Stimmbezirk >= 9000,Volt,0)),
            .groups = "keep")

# # Überprüfung der Korrektheit der Aggregation
# Kommunalbezirk21 %>% filter(Kommunalbezirk=="10")
# dat21 %>%  filter((Kommunalbezirk=="10") & (Stimmbezirk >= 9000)) %>% 
#   summarise(across(c(Berechtigt, Gültig, SPD, CDU, FDP, Grüne, AfD, Linke, Volt), sum))
# dat21 %>%  filter((Kommunalbezirk=="10") & (Stimmbezirk < 9000)) %>% 
#   summarise(Gültig=sum(Gültig))



# Verteilung der Briefwahlstimmen eines jeden Kommunalbezirks auf die Stimmen der Wahllokale
dat21 <- dat21 %>%
  left_join(Kommunalbezirk21, by = "Kommunalbezirk") %>%
  mutate(Gültig_all = ifelse(Stimmbezirk < 9000, Gültig + Berechtigt/Berechtigt_lokal * Gültig_kommunal, 0),
         SPD_all = ifelse(Stimmbezirk < 9000, SPD + Berechtigt/Berechtigt_lokal * SPD_kommunal, 0),
         CDU_all = ifelse(Stimmbezirk < 9000, CDU + Berechtigt/Berechtigt_lokal * CDU_kommunal, 0),
         FDP_all = ifelse(Stimmbezirk < 9000, FDP + Berechtigt/Berechtigt_lokal * FDP_kommunal, 0),
         Grüne_all = ifelse(Stimmbezirk < 9000, Grüne + Berechtigt/Berechtigt_lokal * Grüne_kommunal, 0),
         AfD_all = ifelse(Stimmbezirk < 9000, AfD + Berechtigt/Berechtigt_lokal * AfD_kommunal, 0),
         Linke_all = ifelse(Stimmbezirk < 9000, Linke + Berechtigt/Berechtigt_lokal * Linke_kommunal, 0),
         Volt_all = ifelse(Stimmbezirk < 9000, Volt + Berechtigt/Berechtigt_lokal * Volt_kommunal, 0))



# # Überprüfung der Korrektheit der Verteilung         
# dat21 %>%  filter(Kommunalbezirk=="10") %>%
#   group_by(Kommunalbezirk) %>%
#   summarize(Gültig=sum(Gültig),SPD=sum(SPD),CDU=sum(CDU),AfD=sum(AfD),Grüne=sum(Grüne),Linke=sum(Linke),Volt=sum(Volt),
#             Gültig_all=sum(Gültig_all),SPD_all=sum(SPD_all), CDU_all=sum(CDU_all),AfD_all=sum(AfD_all),Grüne_all=sum(Grüne_all) ,Linke_all=sum(Linke_all),Volt_all=sum(Volt_all),
#             .groups = "keep")
# 
# # Vergleich der Gesamtergebnisse
# dat21 %>%  
#   summarize(Gültig_all=sum(Gültig_all),
#             SPD=sum(SPD_all), 
#             CDU=sum(CDU_all),
#             AfD=sum(AfD_all),
#             Grüne=sum(Grüne_all),
#             Linke=sum(Linke_all),
#             Volt=sum(Volt_all),
#             .groups = "keep")

dat21a <- dat21 %>%
  filter(Stimmbezirk < 9000) %>%
  mutate(Berechtigt_a21=Berechtigt, Gültig_a21=Gültig_all, Gültig_p21 = Gültig_a21/Berechtigt*100,
         SPD_a21=SPD_all, CDU_a21=CDU_all, FDP_a21=FDP_all, Grüne_a21=Grüne_all, 
         AfD_a21=AfD_all, Linke_a21=Linke_all, Volt_a21=Volt_all,
         SPD_p21=SPD_all/Gültig_all*100, CDU_p21=CDU_all/Gültig_all*100, FDP_p21=FDP_all/Gültig_all*100, Grüne_p21=Grüne_all/Gültig_all*100, 
         AfD_p21=AfD_all/Gültig_all*100, Linke_p21=Linke_all/Gültig_all*100, Volt_p21=Volt_all/Gültig_all*100) %>%
  select(Stimmbezirk,
         Berechtigt_a21, Gültig_a21, Gültig_p21, 
         SPD_a21,CDU_a21,FDP_a21,Grüne_a21,AfD_a21,Linke_a21,Volt_a21,
         SPD_p21,CDU_p21,FDP_p21,Grüne_p21,AfD_p21,Linke_p21,Volt_p21)
  
btw <- dat25 %>%
  filter(Stimmbezirk < 9000) %>%
  mutate(Berechtigt_a25=Berechtigt, Gültig_a25=Gültig_all, Gültig_p25 = Gültig_a25/Berechtigt*100,
         SPD_a25=SPD_all, CDU_a25=CDU_all, FDP_a25=FDP_all, Grüne_a25=Grüne_all, 
         AfD_a25=AfD_all, Linke_a25=Linke_all, Volt_a25=Volt_all,
         SPD_p25=SPD_all/Gültig_all*100, CDU_p25=CDU_all/Gültig_all*100, FDP_p25=FDP_all/Gültig_all*100, Grüne_p25=Grüne_all/Gültig_all*100, 
         AfD_p25=AfD_all/Gültig_all*100, Linke_p25=Linke_all/Gültig_all*100, Volt_p25=Volt_all/Gültig_all*100) %>%
  select(Stimmbezirk,Kommunalbezirk,Stadtbezirk,
         Berechtigt_a25, Gültig_a25, Gültig_p25, 
         SPD_a25,CDU_a25,FDP_a25,Grüne_a25,AfD_a25,Linke_a25,Volt_a25,
         SPD_p25,CDU_p25,FDP_p25,Grüne_p25,AfD_p25,Linke_p25,Volt_p25) %>%
  left_join(dat21a, by = "Stimmbezirk") %>%
  mutate(Berechtigt_ad = Berechtigt_a25 - Berechtigt_a21,
         Gültig_ad = Gültig_a25 - Gültig_a21,
         Gültig_pd = Gültig_p25 - Gültig_p21,
         SPD_ad = SPD_a25 - SPD_a21,
         SPD_pd = SPD_p25 - SPD_p21,
         CDU_ad = CDU_a25 - CDU_a21,
         CDU_pd = CDU_p25 - CDU_p21,
         FDP_ad = FDP_a25 - FDP_a21,
         FDP_pd = FDP_p25 - FDP_p21,
         Grüne_ad = Grüne_a25 - Grüne_a21,
         Grüne_pd = Grüne_p25 - Grüne_p21,
         AfD_ad = AfD_a25 - AfD_a21,
         AfD_pd = AfD_p25 - AfD_p21,
         Linke_ad = Linke_a25 - Linke_a21,
         Linke_pd = Linke_p25 - Linke_p21,
         Volt_ad = Volt_a25 - Volt_a21,
         Volt_pd = Volt_p25 - Volt_p21)

         
# # Prüfung Ergebnisse 2025
# btw %>% summarize(Berechtigt=sum(Berechtigt_a25, na.rm=T),
#                   Gültig=sum(Gültig_a25),
#                   SPD=sum(SPD_a25), 
#                   CDU=sum(CDU_a25),
#                   AfD=sum(AfD_a25),
#                   Grüne=sum(Grüne_a25),
#                   Linke=sum(Linke_a25),
#                   Volt=sum(Volt_a25))
# 
# # Prüfung Ergebnisse 2021
# btw %>% summarize(Berechtigt=sum(Berechtigt_a21, na.rm=T),
#                   Gültig=sum(Gültig_a21),
#                   SPD=sum(SPD_a21), 
#                   CDU=sum(CDU_a21),
#                   AfD=sum(AfD_a21),
#                   Grüne=sum(Grüne_a21),
#                   Linke=sum(Linke_a21),
#                   Volt=sum(Volt_a21))
# 
#   
# # Prüfung Differenz
# btw %>% summarize(Berechtigt_ad=sum(Berechtigt_ad, na.rm=T),
#                   Gültig_ad=sum(Gültig_ad),
#                   SPD_ad=sum(SPD_ad), 
#                   CDU_ad=sum(CDU_ad),
#                   Grüne_ad=sum(Grüne_ad))
# 
# btw %>% filter(Stimmbezirk==1001) %>%
#   select(Berechtigt_a25, Berechtigt_a21, Berechtigt_ad,
#          Gültig_a25, Gültig_a21, Gültig_ad,
#          Gültig_p25, Gültig_p21, Gültig_pd)
# btw %>% filter(Stimmbezirk==1001) %>%
#   select(SPD_a25, SPD_a21, SPD_ad,
#          SPD_p25, SPD_p21, SPD_pd)
# 

# Mapping der Kreiswahlbezirke auf statistische Gebiete
filename <- "KWB_SB.csv"
kwb_sb <- read_delim(filename, delim=";", escape_double = FALSE, trim_ws = TRUE)

kwb_sb <- kwb_sb %>% 
  mutate(Kommunalbezirk=substring(Kommunalwahlbezirk,1,2),
         Statistikbezirk=substring(Statistikbezirk,4,100)) %>%
  select(Kommunalbezirk, Statistikbezirk)

btw <- btw %>%
  left_join(kwb_sb, by = "Kommunalbezirk")

# Statistikbereich prüfen
btw$Kommunalbezirk[1:20]
btw$Statistikbezirk[1:20]

# Sozialbericht laden
filename <- "BochumerSozialbericht2024.pdf"
txt <- pdf_text(filename)


# Rohdaten für Ausländische_Bevölkerung aus Tabelle im PDF-Format abrufen
Ausländische_Bevölkerung <- txt[27]

# Zeilenumbrüche "\n" als Trennzeichen verwenden
tab <- str_split(Ausländische_Bevölkerung, "\n")
# Liste von 1
tab <- tab[[1]]

#Spaltennamen
the_names <- c("Statistikbezirk","b","c","Ausländische_Bevölkerung")

#Daten extrahieren
Ausländische_Bevölkerung <- tab[10:45] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>% #Split at two or more spaces
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)

#Ergebnisse anpassen
Ausländische_Bevölkerung <- Ausländische_Bevölkerung %>%
  mutate(Ausländische_Bevölkerung = Ausländische_Bevölkerung/10) %>%
  select(Statistikbezirk, Ausländische_Bevölkerung)

# Zu Wahlergebnis ergänzen
btw <- btw %>%
  left_join(Ausländische_Bevölkerung, by = "Statistikbezirk")

# # Ausländische_Bevölkerung prüfen
# btw$Kommunalbezirk[1:20]
# btw$Ausländische_Bevölkerung[1:20]

# Rohdaten für Alt_Jung_Quotient aus Tabelle im PDF-Format abrufen
Alt_Jung_Quotient <- txt[35]

# Zeilenumbrüche "\n" als Trennzeichen verwenden
tab <- str_split(Alt_Jung_Quotient, "\n")
# Liste von 1
tab <- tab[[1]]

#Spaltennamen
the_names <- c("Statistikbezirk","b","c","Alt_Jung_Quotient")

#Daten extrahieren
Alt_Jung_Quotient <- tab[14:49] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>% #Split at two or more spaces
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)

#Ergebnisse anpassen
Alt_Jung_Quotient <- Alt_Jung_Quotient %>%
  select(Statistikbezirk, Alt_Jung_Quotient)

# Zu Wahlergebnis ergänzen
btw <- btw %>%
  left_join(Alt_Jung_Quotient, by = "Statistikbezirk")

# # Alt_Jung_Quotient prüfen
# btw$Kommunalbezirk[1:20]
# btw$Alt_Jung_Quotient[1:20]

# Rohdaten für Arbeitslose aus Tabelle im PDF-Format abrufen
Arbeitslose <- txt[46]

# Zeilenumbrüche "\n" als Trennzeichen verwenden
tab <- str_split(Arbeitslose, "\n")
# Liste von 1
tab <- tab[[1]]

#Spaltennamen
the_names <- c("Statistikbezirk","b","Arbeitslose")

#Daten extrahieren
Arbeitslose <- tab[7:42] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>% #Split at two or more spaces
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)

#Ergebnisse anpassen
Arbeitslose <- Arbeitslose %>%
  mutate(Arbeitslose = Arbeitslose/10) %>%
  select(Statistikbezirk, Arbeitslose)

# Zu Wahlergebnis ergänzen
btw <- btw %>%
  left_join(Arbeitslose, by = "Statistikbezirk")

# # Arbeitslose prüfen
# btw$Kommunalbezirk[1:20]
# btw$Arbeitslose[1:20]


# Rohdaten für Haushalte mit Kindern aus Tabelle im PDF-Format abrufen
Haushalte_mit_Kindern <- txt[93]

# Zeilenumbrüche "\n" als Trennzeichen verwenden
tab <- str_split(Haushalte_mit_Kindern, "\n")
# Liste von 1
tab <- tab[[1]]

#Spaltennamen
the_names <- c("Statistikbezirk","b","Haushalte_mit_Kindern")

#Daten extrahieren
Haushalte_mit_Kindern <- tab[8:43] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>% #Split at two or more spaces
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)

#Ergebnisse anpassen
Haushalte_mit_Kindern <- Haushalte_mit_Kindern %>%
  mutate(Haushalte_mit_Kindern = Haushalte_mit_Kindern/10) %>%
  select(Statistikbezirk, Haushalte_mit_Kindern)

# Zu Wahlergebnis ergänzen
btw <- btw %>%
  left_join(Haushalte_mit_Kindern, by = "Statistikbezirk")

# # Haushalte_mit_Kindern prüfen
# btw$Kommunalbezirk[1:20]
# btw$Haushalte_mit_Kindern[1:20]



# Rohdaten für Gymnasialempfehlung aus Tabelle im PDF-Format abrufen
Gymnasialempfehlung <- txt[135]

# Zeilenumbrüche "\n" als Trennzeichen verwenden
tab <- str_split(Gymnasialempfehlung, "\n")
# Liste von 1
tab <- tab[[1]]

#Spaltennamen
the_names <- c("Statistikbezirk","b","c", "Gymnasialempfehlung")

#Daten extrahieren
Gymnasialempfehlung <- tab[21:48] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>% #Split at two or more spaces
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)

# Ergebnisse anpassen
Gymnasialempfehlung <- Gymnasialempfehlung %>%
  mutate(Gymnasialempfehlung = Gymnasialempfehlung/10) %>%
  select(Statistikbezirk, Gymnasialempfehlung)

# Zu Wahlergebnis ergänzen
btw <- btw %>%
  left_join(Gymnasialempfehlung, by = "Statistikbezirk")

# # Gymnasialempfehlung prüfen
# btw$Kommunalbezirk[1:20]
# btw$Gymnasialempfehlung[1:20]


###### Geodaten laden


#load stadtbezirke as shapefile
stadtbezirke <- st_read("Stadtbezirke/Stadtbezirke.shp")
# convert to sf object
sf_stadtbezirke <- st_transform(stadtbezirke, crs = 4326)



# Wahllokale mit Adressen

# # Geocodieren
# filename <- "opendata-wahllokale.csv"
# wahllokale <- read_delim(filename, delim=";", escape_double = FALSE, trim_ws = TRUE)
# 
# # einige Adressen korrigieren
# wahllokale$`Wahlraum-Adresse`[wahllokale$`Bezirk-Nr`==1301] <- "Clemensstr. 17, 44789 Bochum"
# wahllokale$`Wahlraum-Adresse`[wahllokale$`Bezirk-Nr`==6501] <- "Gaußstr. 139, 44879 Bochum "
# wahllokale$`Wahlraum-Adresse`[wahllokale$`Bezirk-Nr`==6503] <- "Gaußstr. 139, 44879 Bochum "
# 
# # Geocode the addresses
# Wahllokale_geocoded <- wahllokale %>%
#    geocode(
#      address = `Wahlraum-Adresse`, # Column containing addresses
#      method = "osm",              # Use OpenStreetMap's Nominatim API
#      full_results = FALSE         # Return only latitude and longitude
#    )
# # save as file
# write.csv(Wahllokale_geocoded, "Wahllokale_geocoded.csv", row.names = FALSE)


filename <- "Wahllokale_geocoded.csv"
wahllokale <- read_csv(filename)

# rename district key
names(wahllokale)[names(wahllokale) == "Bezirk-Nr"] <- "Stimmbezirk"
wahllokale <- wahllokale %>% select(Stimmbezirk, lat, long)

# Zu Wahlergebnis ergänzen
btw <- btw %>%
  left_join(wahllokale, by = "Stimmbezirk")


###### OpenStreeMap für Gebäude laden

# # Convert pbf into gpkg - needed only once
# osm_gpkg <- oe_vectortranslate("./nordrhein-westfalen-latest.osm.pbf",
#                                extra_tags = c("building", "addr:housenumber", "addr:street", "addr:city",
#                                                 "building:flats", "building:levels", "height"),
#                                layer="multipolygons")
# 
# # Read gpkg file
# nrw_data <- st_read("./nordrhein-westfalen-latest.gpkg", layer = "multipolygons")
# nrw_filtered_data <- nrw_data[!is.na(nrw_data$addr_city) &
#                             !is.na(nrw_data$addr_street) &
#                             !is.na(nrw_data$addr_housenumber) &
#                             !is.na(nrw_data$building), ]
# bochum_data <- nrw_filtered_data[nrw_filtered_data$addr_city=="Bochum",]
# 
# 
# # Write the multipolygons layer to a new GeoPackage
# st_write(bochum_data, "./bochum_address.gpkg", layer = "multipolygons", delete_layer = TRUE)


# read gpkg file and convert to sf
bochum_data <- sf::st_as_sf(st_read("./bochum_address.gpkg", layer="multipolygons"))

# Nur Wohngebäude filtern
bochum_residential <- bochum_data[bochum_data$building %in% c("yes", "apartments","house",
                                                              "semidetached_house", "detached", "residential", 
                                                              "dormitory", "bungalow", "tiny_house"),]


#### Im Straßenverzeichnis jede Straßenabschnitt finden und die Anzahl Gebäude berechnen

# Straße umwandeln Gretchenstr. in Gretchenstraße und Gretchen-Str. in Gretchen-Straße
replacements <- c("str\\." = "straße", "Str\\." = "Straße")

# Anzeige der Häuder
# ggplot() +
#   geom_sf(data = filtered_buildings, fill = "lightblue", color = "black") +
#   theme_minimal() 

# Ensure addr:housenumber is numeric where possible
bochum_residential$housenumber <- as.numeric(gsub("[^0-9]", "", bochum_residential$addr_housenumber))  # Remove non-numeric characters

# Wahlkampfdaten einlesen

# Wahlkampf <- read_excel("Wahlkampf.xlsx")
Wahlkampf <- read_excel("Strassenverzeichnis_Bundestagswahl_2025_Stand_23-02-25 korrigiert.xlsx", 
                        col_types = c("text", "text", "numeric", 
                                      "numeric", "numeric", "skip", "skip", 
                                      "numeric", "skip", "skip", "skip", 
                                      "skip", "skip", "skip", "skip", "text", 
                                      "skip", "text", "skip", "skip", "skip", 
                                      "skip", "skip", "skip", "skip", "skip"))

# xxx this is not yet working properly - needs checking
suppressWarnings({
  Wahlkampf <- Wahlkampf %>% 
    mutate(`Hausnr. gerade von` = parse_number(`Hausnr. gerade von`))
})


# Für jeden Straßenabschnitt in Wahlkampf zählen, wieviele Gebäude es gibt 
Wahlkampf <- Wahlkampf %>%
  rowwise() %>%
  mutate( Gebäude_gerade = ifelse(is.na(`Hausnr. gerade von`) | `Hausnr. gerade von` == "unbebaut",0,
                                sum(bochum_residential$addr_street == str_replace_all(Straßenname, replacements) & #
                                     bochum_residential$housenumber %% 2 == 0 &
                                     bochum_residential$housenumber >= as.integer(`Hausnr. gerade von`) &
                                     bochum_residential$housenumber <= as.integer(`Hausnr. gerade bis`))),
          Gebäude_ungerade = ifelse(is.na(`Hausnr. ungerade von`),0,
                                sum(bochum_residential$addr_street == str_replace_all(Straßenname, replacements) & #
                                     bochum_residential$housenumber %% 2 == 1 &
                                     bochum_residential$housenumber >= as.integer(`Hausnr. ungerade von`) &
                                     bochum_residential$housenumber <= as.integer(`Hausnr. ungerade bis`))),
          Gebäude= Gebäude_gerade + Gebäude_ungerade,
          Häuserkampf = ifelse(!is.na(abgehakt), ifelse(tolower(abgehakt)=="ja", Gebäude, 0),0),
          Haustür = ifelse(is.na(Wahlkampf),0,ifelse(Wahlkampf=="HTWK",Häuserkampf,0)),
          Briefkasten = ifelse(is.na(Wahlkampf),0,ifelse(Wahlkampf=="BKWK",Häuserkampf,0)) 
  ) %>%
  ungroup()


# Nach Stimmbezirk aggregieren
Wahlkampf <- Wahlkampf %>% group_by(Stimmbezirk) %>%
  summarize(Gebäude=sum(Gebäude),
            Haustür=sum(Haustür),
            Briefkasten=sum(Briefkasten),
            .groups = "keep")

# Zu Wahlergebnis ergänzen
btw <- btw %>%
  left_join(Wahlkampf, by = "Stimmbezirk")






############################# Analyse der Daten #########################
#' \newpage
#' # Analyse der Daten
#'  
#' Im Folgenden werden die Analyseergebnisse vorgestellt. Zunächst wird das Gesamtergebnis für die Stadt Bochum und 
#' die Stimmenvetielung in der Stadt Bochum dargestellt.
#' Anschließend werden die Veränderungen der Stimmanteile der Parteien gegenüber der Bundestagswahl 2021 miteinander in Bezug gesetzt, 
#' um Hinweise auf Wählerwanderungen zu erhalten.
#' Im Anschluss werden Zusammenhänge der Ergebnisse der Grünen mit soziodemographischen Merkmalen ermittelt.
#' Zuletzt werden die Auswirkungen des Wahlkampfs untersucht.
#' 

#' ## Gesamtergebnis Stadt Bochum
#' Die hier gezeigten Werte zeigen die Ergebnisse bei den Zweitstimmen in der Stadt Bochum, also Wahlkreis Bochum I und Teiles des Wahlkreises Bochum II.
#' 
#+ echo=FALSE, fig.width=7, fig.height=5

totals <- btw %>%
  summarize(
    Gültig = sum(Gültig_a25), 
    SPD = sum(SPD_a25)/Gültig,
    CDU = sum(CDU_a25)/Gültig,
    FDP = sum(FDP_a25)/Gültig,
    AfD = sum(AfD_a25)/Gültig,
    Linke = sum(Linke_a25)/Gültig,
    Grüne = sum(Grüne_a25)/Gültig,
    Volt = sum(Volt_a25)/Gültig,
    Sonstige = (Gültig - sum(SPD_a25, CDU_a25, FDP_a25+ AfD_a25+Linke_a25+Grüne_a25+Volt_a25))/Gültig
  ) %>%
  pivot_longer(cols = everything(), names_to = "Party", values_to = "Total")

# Create the bar chart
p_absolut <- totals %>% filter(Party != "Gültig") %>%
  mutate(Party = factor(Party,levels = c("SPD","CDU","AfD","Grüne","Linke","FDP","Volt","Sonstige"))) %>%
 ggplot( aes(x = Party, y = Total, fill = Party)) +
  geom_bar(stat = "identity", show.legend=FALSE) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1), # Format y-axis as percentages
    expand = expansion(mult = c(0, 0.1)) # Add 10% padding to the top
  ) +
  geom_text(aes(label = percent(Total, accuracy = 0.1)), vjust = -0.5, size = 3) + # Add numbers on top of bars
  labs(x = "",y = "Zweistimmenanteil") +
  theme_minimal() +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "black", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="green", "FDP"="#FFCC00","Volt"="purple"))



totals <- btw %>%
  summarize(
    Gültig = sum(Gültig_a25), 
    SPD = sum(SPD_ad)/Gültig,
    CDU = sum(CDU_ad)/Gültig,
    FDP = sum(FDP_ad)/Gültig,
    AfD = sum(AfD_ad)/Gültig,
    Linke = sum(Linke_ad)/Gültig,
    Grüne = sum(Grüne_ad)/Gültig,
    Volt = sum(Volt_ad)/Gültig,
    Sonstige = (sum(Gültig_a25-SPD_a25-CDU_a25-FDP_a25-AfD_a25-Linke_a25-Grüne_a25-Volt_a25)-
                sum(Gültig_a21-SPD_a21-CDU_a21-FDP_a21-AfD_a21-Linke_a21-Grüne_a21-Volt_a21))/Gültig
    ) %>%
  pivot_longer(cols = everything(), names_to = "Party", values_to = "Total")

# Create the bar chart
p_diff <- totals %>% filter(Party != "Gültig") %>%
  mutate(Party = factor(Party,levels = c("SPD","CDU","AfD","Grüne","Linke","FDP","Volt","Sonstige"))) %>%
  ggplot( aes(x = Party, y = Total, fill = Party)) +
  geom_bar(stat = "identity", show.legend=FALSE) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1), # Format y-axis as percentages
    expand = expansion(mult = c(0, 0.1)) # Add 5% padding to the top
  ) +
  geom_text(aes(label = percent(Total, accuracy = 0.1)), vjust = -0.5, size = 3) + # Add numbers on top of bars
  labs(x = "",y = "Vergleich 2021") +
  theme_minimal() +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "black", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="green", "FDP"="#FFCC00","Volt"="purple"))

# Anordnen der Plots in einem 1x2-Raster 
grid.arrange(p_absolut, p_diff, ncol = 1, heights = c(1.5, 1))



#' 
#' Die SPD bleibt in der Stadt Bochum knapp in Führung. Die Ergebnisse der Grünen liegen über dem Bundesdurchschnitt (11,6 %).
#' Wie im Bund verlieren alle Ampelparteien Stimmenanteile gegenüber 2021.
#' 
#' ### Auswertung nach Briefwahl und Wahllokal
#'
#+ echo=FALSE, fig.width=7, fig.height=2


totals <- dat25 %>%
  summarize(
    `Gültig gesamt`= sum(Gültig), 
    Briefwahl = sum(ifelse(Stimmbezirk > 9000,Gültig,0)),
    Wahllokal =  `Gültig gesamt` - Briefwahl,
    ) %>%
  pivot_longer(cols = everything(), names_to = "Wahltyp", values_to = "Total")

# Create the bar chart
totals %>%
  mutate(
    Wahltyp = factor(Wahltyp, levels = c("Briefwahl", "Wahllokal", "Gültig gesamt" )),
    Percentage = Total / Total[Wahltyp == "Gültig gesamt"] * 100
  ) %>%
  ggplot(aes(x = Wahltyp, y = Total)) +
  geom_bar(stat = "identity", fill = "#888888", show.legend = FALSE, width = .7) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), 
    expand = expansion(mult = c(0, 0.2))  # Add 20% padding to the right
  ) +
  geom_text(
    aes(label = paste0(format(floor(Total), big.mark = ".", decimal.mark = ","), 
                       "\n(", round(Percentage, 0), "%)")),
    hjust = -.3, # Center text vertically within the bars
    size = 3
  ) +  # Add percentages for specific bars
  labs(x = "", y = "") +
  theme_minimal() +
   coord_flip()





#' 
#' Die folgende Grafik zeigt die Wahlergebnisse aus der Briefwahl im Vergleich zu den Stimmen, die am Wahltag in den 
#' Wahllokalen abgegeben wurden.
#' 
#+ echo=FALSE, fig.width=7, fig.height=6

totals <- dat25 %>%
  filter(Stimmbezirk<9000) %>%
  summarize(
    Sonstige = sum(Gültig-SPD-CDU-FDP-AfD-Linke-Grüne-Volt)/sum(Gültig),
    Gültig = sum(Gültig), 
    SPD = sum(SPD)/Gültig,
    CDU = sum(CDU)/Gültig,
    FDP = sum(FDP)/Gültig,
    AfD = sum(AfD)/Gültig,
    Linke = sum(Linke)/Gültig,
    Grüne = sum(Grüne)/Gültig,
    Volt = sum(Volt)/Gültig
  ) %>%
  pivot_longer(cols = everything(), names_to = "Party", values_to = "Total")

# Create the bar chart
p1 <- totals %>% filter(Party != "Gültig") %>%
  mutate(Party = factor(Party,levels = c("SPD","CDU","AfD","Grüne","Linke","FDP","Volt","Sonstige"))) %>%
  ggplot( aes(x = Party, y = Total, fill = Party)) +
  geom_bar(stat = "identity", show.legend=FALSE) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1), # Format y-axis as percentages
    expand = expansion(mult = c(0, 0.1)) # Add 5% padding to the top
  ) +
  geom_text(aes(label = percent(Total, accuracy = 0.1)), vjust = -0.5, size = 3) + # Add numbers on top of bars
  labs(x = "",y = "Stimmenanteil im Wahllokal") +
  theme_minimal() +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "black", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="green", "FDP"="#FFCC00","Volt"="purple"))


totals <- dat25 %>%
  filter(Stimmbezirk>9000) %>%
  summarize(
    Sonstige = sum(Gültig-SPD-CDU-FDP-AfD-Linke-Grüne-Volt)/sum(Gültig),
    Gültig = sum(Gültig), 
    SPD = sum(SPD)/Gültig,
    CDU = sum(CDU)/Gültig,
    FDP = sum(FDP)/Gültig,
    AfD = sum(AfD)/Gültig,
    Linke = sum(Linke)/Gültig,
    Grüne = sum(Grüne)/Gültig,
    Volt = sum(Volt)/Gültig,
  ) %>%
  pivot_longer(cols = everything(), names_to = "Party", values_to = "Total")

# Create the bar chart
p2 <- totals %>% filter(Party != "Gültig") %>%
  mutate(Party = factor(Party,levels = c("SPD","CDU","AfD","Grüne","Linke","FDP","Volt","Sonstige"))) %>%
  ggplot( aes(x = Party, y = Total, fill = Party)) +
  geom_bar(stat = "identity", show.legend=FALSE) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1), # Format y-axis as percentages
    expand = expansion(mult = c(0, 0.1)) # Add 5% padding to the top
  ) +
  geom_text(aes(label = percent(Total, accuracy = 0.1)), vjust = -0.5, size = 3) + # Add numbers on top of bars
  labs(x = "",y = "Stimmenanteil Briefwahll") +
  theme_minimal() +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "black", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="green", "FDP"="#FFCC00","Volt"="purple"))


# Anordnen der Plots in einem 1x2-Raster 
grid.arrange(p1, p2, ncol = 1)

#' 
#' Es gibt deutliche Unterschiede zwischen den Ergebnissen der Briefwahl und der Stimmabgabe in den Wahllokalen am Wahltag.
#' Dies kann auf unterschiedliche Wählerklientel sowie die unterschiedlichen Zeitpunkte der Stimmabgabe zurückzuführen sein. 
#' 
#' ### Stimmenverteilung im Stadtgebiet
#' 
#' Die folgenden Karten zeigen die prozentualen Stimmenanteile als räumliche Verteilung.
#' 
#+ echo=FALSE, fig.width=7, fig.height=8

geo_alpha1 <- .6
geo_alpha2 <- .9
geo_shape <- 16
geo_size <- .13
size_SPD <- 24.5 
size_CDU <- 23.3 
size_AfD <- 15.2 
size_Grüne <- 13.8 
size_Linke <- 11.5 
size_FDP <- 3.5 

p_CDU <- ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = CDU_p25), alpha = geo_alpha1, col="black", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  scale_size_continuous(range = c(0.1, geo_size*size_CDU)) +
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "CDU", size = 3.5)
p_SPD <- ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = SPD_p25), alpha = geo_alpha1, col="red2", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  scale_size_continuous(range = c(0.1, geo_size*size_SPD)) +
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "SPD", size = 3.5)
p_AfD <- ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = AfD_p25), alpha = geo_alpha2, col="#4169E1", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  scale_size_continuous(range = c(0.1, geo_size*size_AfD)) +
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "AfD", size = 3.5)
p_Grüne <- ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = Grüne_p25), alpha = geo_alpha2, col="forestgreen", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  scale_size_continuous(range = c(0.1, geo_size*size_Grüne))+ # Adjust point size range here
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "Grüne", size = 3.5)
p_Linke <- ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = Linke_p25), alpha = geo_alpha2, col="deeppink", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  scale_size_continuous(range = c(0.1, geo_size*size_Linke))+ # Adjust point size range here
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "Linke", size = 3.5)
p_FDP <- ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = FDP_p25), alpha = 1, col="#DDAA00", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  scale_size_continuous(range = c(0.1, geo_size*size_FDP))+ # Adjust point size range here
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "FDP", size = 3.5)
  
grid.arrange(p_CDU, p_SPD, p_AfD, p_Grüne, p_Linke, p_FDP, ncol = 2)


#'
#' Die räumliche Verteilung der Zweitstimmen zeigt, dass die Grünen und Linken besonders stark im Stadtzentrum sind,
#' während die CDU im Südwesten und die AfD im Osten und Westen der Stadt besser abschneiden.
#' 
#' ### Stimmengewinne beziehungsweise -verluste im Stadtgebiet
#' 
#' Gewinne der CDU, AfD und Linke, sowie Verluste von SPD, Grüne und FDP gegenüber 2021 werden auf den folgenden Karten 
#' anhand der absoluten Stimmenzahlen dargestellt.
#' 
#+ echo=FALSE, fig.width=7, fig.height=8

btw <- btw %>% 
  mutate(SPD_av = ifelse(SPD_ad < 0, SPD_ad, 0),
         SPD_ag = ifelse(SPD_ad > 0, SPD_ad, 0),
         CDU_av = ifelse(CDU_ad < 0, CDU_ad, 0),
         CDU_ag = ifelse(CDU_ad > 0, CDU_ad, 0),
         AfD_av = ifelse(AfD_ad < 0, AfD_ad, 0),
         AfD_ag = ifelse(AfD_ad > 0, AfD_ad, 0),
         FDP_av = ifelse(FDP_ad < 0, FDP_ad, 0),
         FDP_ag = ifelse(FDP_ad > 0, FDP_ad, 0),
         Grüne_av = ifelse(Grüne_ad < 0, Grüne_ad, 0),
         Grüne_ag = ifelse(Grüne_ad > 0, Grüne_ad, 0),
         Linke_av = ifelse(Linke_ad < 0, Linke_ad, 0),
         Linke_ag = ifelse(Linke_ad > 0, Linke_ad, 0))

geo_size <- .3
size_SPD <- 7.3
size_CDU <- 5.3
size_AfD <- 8.4
size_Grüne <- 3.8
size_Linke <- 6.7
size_FDP <- 5.2
size_plus <- .7

p_CDU <- 
  ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = CDU_ag), alpha = geo_alpha2, col="black", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "CDU Gewinne", size = 3.5) +
  scale_size_continuous(range = c(0.1, geo_size*size_CDU)) 

p_SPD <- 
  ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = -SPD_av), alpha = geo_alpha2, col="red2", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "SPD Verluste", size = 3.5) +
  scale_size_continuous(range = c(0.1, geo_size*size_SPD)) 


p_AfD <- 
  ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = AfD_ag), alpha = geo_alpha2, col="#4169E1", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "AfD Gewinne", size = 3.5) +
  scale_size_continuous(range = c(0.1, geo_size*size_AfD)) 

p_Grüne <-   ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = -Grüne_av), alpha = geo_alpha2, col="forestgreen", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "Grüne Verluste", size = 3.5) +
  scale_size_continuous(range = c(0.1, geo_size*size_Grüne)) 

p_Linke <-   ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = Linke_ag), alpha = geo_alpha2, col="deeppink", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "Linke Gewinne", size = 3.5) +
  scale_size_continuous(range = c(0.1, geo_size*size_Linke)) 

p_FDP <- 
  ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = -FDP_av), alpha = geo_alpha2, col="#DDAA00", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "FDP Verluste", size = 3.5) +
  scale_size_continuous(range = c(0.1, geo_size*size_FDP)) 

grid.arrange(p_CDU, p_SPD, p_AfD, p_Grüne, p_Linke, p_FDP, ncol = 2)


#' SPD, Grüne und FDP haben in allen Stimmbezirken Stimmen verloren, während AfD, Linke und CDU in nahezu allen
#' Bezirken Stimmen gewonnen haben. Die Veränderungen von SPD, CDU und FDP variieren wenig im Stadtgebiet. Die AfD hat vor allem 
#' im Westen und Osten der Stadt gewonnen. Grüne verlieren und Linke gewinnen besonders in der Stadtmitte.
#'
#' 
#' ## Wählerbewegungen
#' Um eine Einschätzung zu möglichen Wählerbewegungen zu erhalten, ohne auf Daten aus einer Nachbefragung zurückgreifen zu können,
#' wurden die Veränderungen der Stimmanteile der Grünen mit den Gewinnen und Verlusten der anderen Parteien in Beziehung gesetzt.
#' 
#' Die Analysen verwenden den sogenannten Korrelationskoeffizienten nach Pearson. 
#' Werte größer als 0,5 weisen auf einen starken Zusammenhang, Werte um 0,3 auf einen mittleren und Werte ab 0,1 auf einen
#' geringen Zusammenhang hin. Statistisch signifikante Zusammenhänge, die als gesichert angesehen werden dürfen, 
#' werden mit Sternen (`*`, `**` oder `***`) gekennzeichnet. 
#'
#' 
#+ echo=FALSE, fig.width=7, fig.height=6

# Korrelation der prozentualen Gewinne / Verluste 
c_SPD <- cor(btw$Grüne_pd, btw$SPD_pd, method = "pearson", use = "complete.obs")
c_SPD_p <- cor.test(btw$Grüne_pd, btw$SPD_pd, method = "pearson", use = "complete.obs")$p.value
c_SPD_s <- ifelse(c_SPD_p <= 0.001, "***", ifelse(c_SPD_p <= 0.01, "**", ifelse(c_SPD_p <= 0.05, "*", "")))

c_CDU <- cor(btw$Grüne_pd, btw$CDU_pd, method = "pearson", use = "complete.obs")
c_CDU_p <- cor.test(btw$Grüne_pd, btw$CDU_pd, method = "pearson", use = "complete.obs")$p.value
c_CDU_s <- ifelse(c_CDU_p <= 0.001, "***", ifelse(c_CDU_p <= 0.01, "**", ifelse(c_CDU_p <= 0.05, "*", "")))

c_Linke <- cor(btw$Grüne_pd, btw$Linke_pd, method = "pearson", use = "complete.obs")
c_Linke_p <- cor.test(btw$Grüne_pd, btw$Linke_pd, method = "pearson", use = "complete.obs")$p.value
c_Linke_s <- ifelse(c_Linke_p <= 0.001, "***", ifelse(c_Linke_p <= 0.01, "**", ifelse(c_Linke_p <= 0.05, "*", "")))

c_Volt <- cor(btw$Grüne_pd, btw$Volt_pd, method = "pearson", use = "complete.obs")
c_Volt_p <- cor.test(btw$Grüne_pd, btw$Volt_pd, method = "pearson", use = "complete.obs")$p.value
c_Volt_s <- ifelse(c_Volt_p <= 0.001, "***", ifelse(c_Volt_p <= 0.01, "**", ifelse(c_Volt_p <= 0.05, "*", "")))

c_AfD <- cor(btw$Grüne_pd, btw$AfD_pd, method = "pearson", use = "complete.obs")
c_AfD_p <- cor.test(btw$Grüne_pd, btw$AfD_pd, method = "pearson", use = "complete.obs")$p.value
c_AfD_s <- ifelse(c_AfD_p <= 0.001, "***", ifelse(c_AfD_p <= 0.01, "**", ifelse(c_AfD_p <= 0.05, "*", "")))

c_FDP <- cor(btw$Grüne_pd, btw$FDP_pd, method = "pearson", use = "complete.obs")
c_FDP_p <- cor.test(btw$Grüne_pd, btw$FDP_pd, method = "pearson", use = "complete.obs")$p.value
c_FDP_s <- ifelse(c_FDP_p <= 0.001, "***", ifelse(c_FDP_p <= 0.01, "**", ifelse(c_FDP_p <= 0.05, "*", "")))

c_SPD_CDU <- cor(btw$SPD_pd, btw$CDU_pd, method = "pearson", use = "complete.obs")
c_SPD_CDU_p <- cor.test(btw$SPD_pd, btw$CDU_pd, method = "pearson", use = "complete.obs")$p.value
c_SPD_CDU_s <- ifelse(c_SPD_CDU_p <= 0.001, "***", ifelse(c_SPD_CDU_p <= 0.01, "**", ifelse(c_SPD_CDU_p <= 0.05, "*", "")))

c_SPD_AfD <- cor(btw$SPD_pd, btw$AfD_pd, method = "pearson", use = "complete.obs")
c_SPD_AfD_p <- cor.test(btw$SPD_pd, btw$AfD_pd, method = "pearson", use = "complete.obs")$p.value
c_SPD_AfD_s <- ifelse(c_SPD_AfD_p <= 0.001, "***", ifelse(c_SPD_AfD_p <= 0.01, "**", ifelse(c_SPD_AfD_p <= 0.05, "*", "")))

# entsprechende Streudiagramme

p_CDU <- 
  ggplot(btw, aes(y = Grüne_pd, x = CDU_pd)) +
  geom_point(color = "black", size = .7) + labs(x = "CDU Veränderung (%)",y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$CDU_pd)),  # Horizontal center
            y = min(btw$Grüne_pd) +1 ,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_CDU, c_CDU_s), size = 3, color = "black" ) +
  theme_minimal() 

p_SPD <- 
  ggplot(btw, aes(y = Grüne_pd, x = SPD_pd)) +
  geom_point(color = "red2", size = .7) + labs(x = "SPD Veränderung (%)", y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "red2", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$SPD_pd)),  # Horizontal center
            y = min(btw$Grüne_pd) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_SPD,c_SPD_s), size = 3, color = "black" ) +
  theme_minimal() 

p_Linke <- 
  ggplot(btw, aes(y = Grüne_pd, x = Linke_pd)) +
  geom_point(color = "deeppink", size = .7) + labs(x = "Linke Veränderung (%)", y = "Grüne Veränderung (%)") +
  geom_smooth(method = "lm", formula = y ~ x, color = "deeppink", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Linke_pd)),  # Horizontal center
            y = min(btw$Grüne_pd) +1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Linke,c_Linke_s), size = 3, color = "black" ) +
  theme_minimal()   

p_Volt <- 
  ggplot(btw, aes(y = Grüne_pd, x = Volt_pd)) +
  geom_point(color = "purple", size = .7) + labs(x = "Volt Veränderung (%)", y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "purple", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Volt_pd)),  # Horizontal center
            y = min(btw$Grüne_pd) +1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Volt,c_Volt_s), size = 3, color = "black" ) +
  theme_minimal()  

p_AfD<- 
  ggplot(btw, aes(y = Grüne_pd, x = AfD_pd)) +
  geom_point(color = "#4169E1", size = .7) + labs(x = "AfD Veränderung (%)", y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "#4169E1", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$AfD_pd)),  # Horizontal center
            y = min(btw$Grüne_pd)+1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_AfD,c_AfD_s), size = 3, color = "black") +
  theme_minimal()  

p_FDP<- 
  ggplot(btw, aes(y = Grüne_pd, x = FDP_pd)) +
  geom_point(color = "#FFCC00", size = .7) + labs(x = "FDP Veränderung (%)",y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "#FFCC00", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$FDP_pd)),  # Horizontal center
            y = min(btw$Grüne_pd) +1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_FDP,c_FDP_s), size = 3, color = "black" ) +
  theme_minimal()   

# Anordnen der Plots in einem 2x3-Raster 
grid.arrange( p_CDU, p_SPD, p_Linke, p_Volt, p_AfD, p_FDP, ncol = 2)


#' Es zeigt sich ein sehr deutlicher Zusammenhang zwischen den Zugewinnen der Linken und den Verlusten der Grünen.
#' Die Linken gewinnen in den Stimmbezirken, wo die Grünen die größten Verluste erleiden.
#' 
#' Die SPD hat hingegen weniger stark verloren, wo die Grünen stark verloren haben und umgekehrt. Der Zusammenhang mit 
#' der SPD ist jedoch etwas schwächer. 
#' 
#' Ferner haben CDU und AFD tendenziell dort gewonnen, wo die Grünen weniger stark verloren haben, was möglicherweise durch die Verluste der SPD in diesen Bezirken erklärt werden kann.
#' 
#' Die FDP verliert ebenfalls weniger stark, , wo die Grünen stark verloren haben und umgekehrt. Volt gewinnt stärker, wo die 
#' Grünen stärker verloren haben. Die Zusammenhänge mit der FDP und Volt sind jedoch nicht stark ausgeprägt.
#' 
#+ echo=FALSE, fig.width=7, fig.height=2.2

p_SPD_CDU<- 
  ggplot(btw, aes(y = SPD_pd, x = CDU_pd)) +
  geom_point(color = "gray", size = .7) + labs(x = "CDU Veränderung (%)", y = "SPD +/- (%)") +
  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$CDU_pd)),  # Horizontal center
            y = min(btw$SPD_pd)+1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_SPD_CDU, c_SPD_CDU_s), size = 3, color = "black") +
  theme_minimal() +
  theme(plot.margin = margin(20, 0, 0, 0)) 

p_SPD_AfD<- 
  ggplot(btw, aes(y = SPD_pd, x = AfD_pd)) +
  geom_point(color = "gray", size = .7) + labs(x = "AfD Veränderung (%)", y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$AfD_pd)),  # Horizontal center
            y = min(btw$SPD_pd)+1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_SPD_AfD, c_SPD_AfD_s), size = 3, color = "black") +
  theme_minimal() +
  theme(plot.margin = margin(20, 0, 0, 0)) 
  

# Anordnen der Plots in einem 2x1-Raster 
grid.arrange( p_SPD_CDU, p_SPD_AfD, ncol = 2)

#' 
#' Ein starker Zusammenhang ist auch zwischen der SPD und der AfD zu sehen. Die AfD gewinnt dort stark, wo die SPD große Verluste hinnehmen muss.
#' 
#' ## Zusammenhang mit Wahlbeteiligung
#' 
#' In diesem Abschnitt werden die Zusammenhänge der Ergebnisse der Grünen mit der Wahlbeteiligung analysiert.
#' Die folgende Grafik setzt die Stimmanteile der Grünen und die Anteilsverluste der Grünen zur Wahlbeteiligung in Beziehung. 
#+ echo=FALSE, fig.width=7, fig.height=1.8


btw <- btw %>% 
  mutate(Wahlbeteiligung_25 = Gültig_a25 / Berechtigt_a25*100)

# Korrelation der prozentualen Werte 

c_Wahlbeteiligung <- cor(btw$Grüne_p25, btw$Wahlbeteiligung_25, method = "pearson", use = "complete.obs")
c_Wahlbeteiligung_p <- cor.test(btw$Grüne_p25, btw$Wahlbeteiligung_25, method = "pearson", use = "complete.obs")$p.value
c_Wahlbeteiligung_s <- ifelse(c_Wahlbeteiligung_p <= 0.001, "***", ifelse(c_Wahlbeteiligung_p <= 0.01, "**", ifelse(c_Wahlbeteiligung_p <= 0.05, "*", "")))

p1 <- ggplot(btw, aes(y = Grüne_p25, x = Wahlbeteiligung_25)) +
  geom_point(color = "gray", size = .7) + labs(x = "Wahlbeteiligung (%)", y = "Grüne Anteile (%) ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Wahlbeteiligung_25)),  # Horizontal center
            y = min(btw$Grüne_p25) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Wahlbeteiligung,c_Wahlbeteiligung_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(0, 0, 10, 0)) 

c_Wahlbeteiligung <- cor(btw$Grüne_pd, btw$Wahlbeteiligung_25, method = "pearson", use = "complete.obs")
c_Wahlbeteiligung_p <- cor.test(btw$Grüne_pd, btw$Wahlbeteiligung_25, method = "pearson", use = "complete.obs")$p.value
c_Wahlbeteiligung_s <- ifelse(c_Wahlbeteiligung_p <= 0.001, "***", ifelse(c_Wahlbeteiligung_p <= 0.01, "**", ifelse(c_Wahlbeteiligung_p <= 0.05, "*", "")))

p2 <- ggplot(btw, aes(y = Grüne_pd, x = Wahlbeteiligung_25)) +
  geom_point(color = "gray", size = .7) + labs(x = "Wahlbeteiligung (%)", y = "Grüne +/- (%) ") +
#  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Wahlbeteiligung_25)),  # Horizontal center
            y = min(btw$Grüne_pd) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Wahlbeteiligung,c_Wahlbeteiligung_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(0, 0, 10, 0)) 

# Anordnen der Plots in einem 2x1-Raster 
grid.arrange( p1, p2, ncol = 2)


#' Die Grünen haben bessere Ergebnisse in Stimmbezirken mit hoher Wahlbeteiligung erzielt. Es wurde jedoch kein 
#' Zusammenhang zwischen den Verlusten der Grünen und der Wahlbeteiligung festgestellt.  
#' 
#' \newpage
#' ## Zusammenhang mit soziodemographischen Faktoren
#' 
#' ### Analyse der Stimmenanteile
#' In diesem Abschnitt werden die Zusammenhänge der Ergebnisse der Grünen mit 
#' soziodemographischen Merkmalen untersucht, bevor im nächsten
#' Abschnitt betrachtet wird, wie die  Verluste der Grünen mit soziodemographischen 
#' Merkmalen zusammenhängen.
#' 
#+ echo=FALSE, fig.width=7, fig.height=7

# Korrelation der Stimmanteile 

c_Ausländische_Bevölkerung <- cor(btw$Grüne_p25, btw$Ausländische_Bevölkerung, method = "pearson", use = "complete.obs")
c_Ausländische_Bevölkerung_p <- cor.test(btw$Grüne_p25, btw$Ausländische_Bevölkerung, method = "pearson", use = "complete.obs")$p.value
c_Ausländische_Bevölkerung_s <- ifelse(c_Ausländische_Bevölkerung_p <= 0.001, "***", ifelse(c_Ausländische_Bevölkerung_p <= 0.01, "**", ifelse(c_Ausländische_Bevölkerung_p <= 0.05, "*", "")))

c_Alt_Jung_Quotient <- cor(btw$Grüne_p25, btw$Alt_Jung_Quotient, method = "pearson", use = "complete.obs")
c_Alt_Jung_Quotient_p <- cor.test(btw$Grüne_p25, btw$Alt_Jung_Quotient, method = "pearson", use = "complete.obs")$p.value
c_Alt_Jung_Quotient_s <- ifelse(c_Alt_Jung_Quotient_p <= 0.001, "***", ifelse(c_Alt_Jung_Quotient_p <= 0.01, "**", ifelse(c_Alt_Jung_Quotient_p <= 0.05, "*", "")))

c_Arbeitslose <- cor(btw$Grüne_p25, btw$Arbeitslose, method = "pearson", use = "complete.obs")
c_Arbeitslose_p <- cor.test(btw$Grüne_p25, btw$Arbeitslose, method = "pearson", use = "complete.obs")$p.value
c_Arbeitslose_s <- ifelse(c_Arbeitslose_p <= 0.001, "***", ifelse(c_Arbeitslose_p <= 0.01, "**", ifelse(c_Arbeitslose_p <= 0.05, "*", "")))

c_Haushalte_mit_Kindern <- cor(btw$Grüne_p25, btw$Haushalte_mit_Kindern, method = "pearson", use = "complete.obs")
c_Haushalte_mit_Kindern_p <- cor.test(btw$Grüne_p25, btw$Haushalte_mit_Kindern, method = "pearson", use = "complete.obs")$p.value
c_Haushalte_mit_Kindern_s <- ifelse(c_Haushalte_mit_Kindern_p <= 0.001, "***", ifelse(c_Haushalte_mit_Kindern_p <= 0.01, "**", ifelse(c_Haushalte_mit_Kindern_p <= 0.05, "*", "")))

c_Gymnasialempfehlung <- cor(btw$Grüne_p25, btw$Gymnasialempfehlung, method = "pearson", use = "complete.obs")
c_Gymnasialempfehlung_p <- cor.test(btw$Grüne_p25, btw$Gymnasialempfehlung, method = "pearson", use = "complete.obs")$p.value
c_Gymnasialempfehlung_s <- ifelse(c_Gymnasialempfehlung_p <= 0.001, "***", ifelse(c_Gymnasialempfehlung_p <= 0.01, "**", ifelse(c_Gymnasialempfehlung_p <= 0.05, "*", "")))





# entsprechende Streudiagramme
p_Ausländische_Bevölkerung <- 
  ggplot(btw, aes(y = Grüne_p25, x = Ausländische_Bevölkerung)) +
  geom_point(color = "gray", size = .7) + labs(x = "Ausländische Bevölkerung (%)", y = " ") +
#  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Ausländische_Bevölkerung)),  # Horizontal center
            y = max(btw$Grüne_p25) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Ausländische_Bevölkerung,c_Ausländische_Bevölkerung_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(0, 0, 10, 0)) 

p_Alt_Jung_Quotient <- 
  ggplot(btw, aes(y = Grüne_p25, x = Alt_Jung_Quotient)) +
  geom_point(color = "gray", size = .7) + labs(x = "Alt-Jung-Quotient ", y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Alt_Jung_Quotient)),  # Horizontal center
            y = max(btw$Grüne_p25) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Alt_Jung_Quotient,c_Alt_Jung_Quotient_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 10, 0)) 

p_Arbeitslose <- 
  ggplot(btw, aes(y = Grüne_p25, x = Arbeitslose)) +
  geom_point(color = "gray", size = .7) + labs(x = "Arbeitslose (%)", y = "Grüne Stimmenateile (%)") +
  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Arbeitslose)),  # Horizontal center
            y = max(btw$Grüne_p25) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Arbeitslose,c_Arbeitslose_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 10, 0)) 

p_Haushalte_mit_Kindern <- 
  ggplot(btw, aes(y = Grüne_p25, x = Haushalte_mit_Kindern)) +
  geom_point(color = "gray", size = .7) + labs(x = "Haushalte mit Kindern (%)", y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Haushalte_mit_Kindern)),  # Horizontal center
            y = max(btw$Grüne_p25) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Haushalte_mit_Kindern,c_Haushalte_mit_Kindern_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 10, 0)) 

p_Gymnasialempfehlung <- 
  btw %>% filter(!is.na(Gymnasialempfehlung)) %>%
  ggplot(aes(y = Grüne_p25, x = Gymnasialempfehlung)) +
  geom_point(color = "gray", size = .7) + labs(x = "Gymnasialempfehlung (%)", y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Gymnasialempfehlung, na.rm=T)),  # Horizontal center
            y = max(btw$Grüne_p25) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Gymnasialempfehlung,c_Gymnasialempfehlung_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 0, 0)) 

# Anordnen der Plots in einem 2x3-Raster 
grid.arrange(p_Ausländische_Bevölkerung, p_Alt_Jung_Quotient, p_Arbeitslose, 
             p_Haushalte_mit_Kindern, p_Gymnasialempfehlung, ncol = 2)


#' 
#' Der stärkste Zusammenhang besteht zwischen dem Abschneiden der Grünen und dem Anteil der Haushalte mit Kindern.
#' Die Grünen haben die besten Ergebnisse in Stimmbezirken mit einem geringen Anteil an Haushalten mit Kindern erzielt.
#' Die ebenfalls vorhandene Korrelation mit dem Alt-Jung-Quotienten deutet auf einen ähnlichen Zusammenhang.
#' 
#' Ein schwächerer Zusammenhang besteht zwischen guten Ergebnissen der Grünen und geringer Arbeitslosigkeit einerseits und 
#' häufiger Gymnasialempfehlung andererseits. Der Anteil der Empfehlungen für das Gymnasium wird als Indikator für das Bildungsniveau
#' genutzt.
#' 
#' ### Analyse der grünen Verluste  
#' In diesem Abschnitt werden die Zusammenhänge der Verluste der Stimmanteile der Grünen mit soziodemographischen Merkmalen betrachtet.
#' 
#+ echo=FALSE, fig.width=7, fig.height=7

# Korrelation der prozentualen Veränderungen 
c_Ausländische_Bevölkerung <- cor(btw$Grüne_pd, btw$Ausländische_Bevölkerung, method = "pearson", use = "complete.obs")
c_Ausländische_Bevölkerung_p <- cor.test(btw$Grüne_pd, btw$Ausländische_Bevölkerung, method = "pearson", use = "complete.obs")$p.value
c_Ausländische_Bevölkerung_s <- ifelse(c_Ausländische_Bevölkerung_p <= 0.001, "***", ifelse(c_Ausländische_Bevölkerung_p <= 0.01, "**", ifelse(c_Ausländische_Bevölkerung_p <= 0.05, "*", "")))

c_Alt_Jung_Quotient <- cor(btw$Grüne_pd, btw$Alt_Jung_Quotient, method = "pearson", use = "complete.obs")
c_Alt_Jung_Quotient_p <- cor.test(btw$Grüne_pd, btw$Alt_Jung_Quotient, method = "pearson", use = "complete.obs")$p.value
c_Alt_Jung_Quotient_s <- ifelse(c_Alt_Jung_Quotient_p <= 0.001, "***", ifelse(c_Alt_Jung_Quotient_p <= 0.01, "**", ifelse(c_Alt_Jung_Quotient_p <= 0.05, "*", "")))

c_Arbeitslose <- cor(btw$Grüne_pd, btw$Arbeitslose, method = "pearson", use = "complete.obs")
c_Arbeitslose_p <- cor.test(btw$Grüne_pd, btw$Arbeitslose, method = "pearson", use = "complete.obs")$p.value
c_Arbeitslose_s <- ifelse(c_Arbeitslose_p <= 0.001, "***", ifelse(c_Arbeitslose_p <= 0.01, "**", ifelse(c_Arbeitslose_p <= 0.05, "*", "")))

c_Haushalte_mit_Kindern <- cor(btw$Grüne_pd, btw$Haushalte_mit_Kindern, method = "pearson", use = "complete.obs")
c_Haushalte_mit_Kindern_p <- cor.test(btw$Grüne_pd, btw$Haushalte_mit_Kindern, method = "pearson", use = "complete.obs")$p.value
c_Haushalte_mit_Kindern_s <- ifelse(c_Haushalte_mit_Kindern_p <= 0.001, "***", ifelse(c_Haushalte_mit_Kindern_p <= 0.01, "**", ifelse(c_Haushalte_mit_Kindern_p <= 0.05, "*", "")))

c_Gymnasialempfehlung <- cor(btw$Grüne_pd, btw$Gymnasialempfehlung, method = "pearson", use = "complete.obs")
c_Gymnasialempfehlung_p <- cor.test(btw$Grüne_pd, btw$Gymnasialempfehlung, method = "pearson", use = "complete.obs")$p.value
c_Gymnasialempfehlung_s <- ifelse(c_Gymnasialempfehlung_p <= 0.001, "***", ifelse(c_Gymnasialempfehlung_p <= 0.01, "**", ifelse(c_Gymnasialempfehlung_p <= 0.05, "*", "")))

# entsprechende Streudiagramme
p_Ausländische_Bevölkerung <- 
  ggplot(btw, aes(y = Grüne_pd, x = Ausländische_Bevölkerung)) +
  geom_point(color = "gray", size = .7) + labs(x = "Ausländische Bevölkerung (%)", y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Ausländische_Bevölkerung)),  # Horizontal center
            y = max(btw$Grüne_pd) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Ausländische_Bevölkerung,c_Ausländische_Bevölkerung_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(0, 0, 10, 0)) 

p_Alt_Jung_Quotient <- 
  ggplot(btw, aes(y = Grüne_pd, x = Alt_Jung_Quotient)) +
  geom_point(color = "gray", size = .7) + labs(x = "Alt-Jung-Quotient ", y = " ") +
#  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Alt_Jung_Quotient)),  # Horizontal center
            y = max(btw$Grüne_pd) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Alt_Jung_Quotient,c_Alt_Jung_Quotient_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 10, 0)) 

p_Arbeitslose <- 
  ggplot(btw, aes(y = Grüne_pd, x = Arbeitslose)) +
  geom_point(color = "gray", size = .7) + labs(x = "Arbeitslose (%)", y = "Grüne Veränderung (%)") +
#  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Arbeitslose)),  # Horizontal center
            y = max(btw$Grüne_pd) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Arbeitslose,c_Arbeitslose_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 10, 0)) 

p_Haushalte_mit_Kindern <- 
  ggplot(btw, aes(y = Grüne_pd, x = Haushalte_mit_Kindern)) +
  geom_point(color = "gray", size = .7) + labs(x = "Haushalte mit Kindern (%)", y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Haushalte_mit_Kindern)),  # Horizontal center
            y = max(btw$Grüne_pd) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Haushalte_mit_Kindern,c_Haushalte_mit_Kindern_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 10, 0)) 

p_Gymnasialempfehlung <- 
  btw %>% filter(!is.na(Gymnasialempfehlung)) %>%
  ggplot(aes(y = Grüne_pd, x = Gymnasialempfehlung)) +
  geom_point(color = "gray", size = .7) + labs(x = "Gymnasialempfehlung (%)", y = " ") +
#  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Gymnasialempfehlung, na.rm=T)),  # Horizontal center
            y = max(btw$Grüne_pd) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Gymnasialempfehlung,c_Gymnasialempfehlung_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 0, 0)) 

# Anordnen der Plots in einem 2x3-Raster 
grid.arrange(p_Ausländische_Bevölkerung, p_Alt_Jung_Quotient, p_Arbeitslose, 
             p_Haushalte_mit_Kindern, p_Gymnasialempfehlung, ncol = 2)

#' 
#' In dieser Analyse ist ein Zusammenhang zwischen den Verlusten der Grünen und dem Anteil der Haushalte mit Kindern zu erkennen.
#' Die Verluste der Grünen sind am geringsten in Stimmbezirken mit einem hohen Anteil an Haushalten mit Kindern.
#' 
#' Ein schwacher Zusammenhang besteht zwischen den Verlusten der Grünen und dem Anteil von ausländischen Menschen an der Bevölkerung. 
#' Die Grünen haben in Gebieten mit einem hohen Ausländeranteil höhere Verluste zu verzeichnen.
#' 
#' ## Auswertung des Wahlkampfs
#' 
#' Im Folgenden werden der Haustür- und Briefkastenwahlkampf ausgewertet. 
#' Das vorrangige Ziel dieser Maßnahmen ist die Aktivierung der grünen Wähler. Daher wird untersucht,
#' ob die relativen Stimmverluste in Stimmbezirken, an denen mehr Gebäude im Wahlkampf abgedeckt wurden,
#' im Vergleich zu 2021 geringer ausfallen.
#' 
#' ### Umfang des Wahlkampfs
#' Die folgende Grafik zeigt die Anzahl der Gebäude in der Stadt Bochum gesamt, sowie die Anzahl der Gebäude, die im Briefkastenwahlkampf beziehungsweise
#' Haustürwahlkampf abgedeckt wurden.
#' 
#+ echo=FALSE, fig.width=7, fig.height=2


totals <- btw %>%
  summarize(
    `Gebäude gesamt`= sum(Gebäude), 
    Briefkastenwahlkampf = sum(Briefkasten),
    Haustürwahlkampf = sum(Haustür)) %>%
  pivot_longer(cols = everything(), names_to = "Aktivität", values_to = "Total")

# Create the bar chart
totals %>%
  mutate(
    Aktivität = factor(Aktivität, levels = c( "Haustürwahlkampf", "Briefkastenwahlkampf", "Gebäude gesamt")),
    Percentage = Total / Total[Aktivität == "Gebäude gesamt"] * 100
  ) %>%
  ggplot(aes(x = Aktivität, y = Total)) +
  geom_bar(stat = "identity", fill = "#888888", show.legend = FALSE, width = .7) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), 
    expand = expansion(mult = c(0, 0.2))  # Add 5% padding to the top
  ) +
  geom_text(
    aes(label = paste0(format(floor(Total), big.mark = ".", decimal.mark = ","), 
                       "\n(", round(Percentage, 0), "%)")),
    hjust = -0.3,
    size = 3
  ) +  # Add percentages for specific bars
  labs(x = "", y = "") +
  theme_minimal()+
  coord_flip()




#'
#' Die folgenden Karten zeigen die prozentuale Abdeckung der Gebäude im Wahlkampf und die
#' relativen Stimmenverluste.
#' 
#+ echo=FALSE, fig.width=7, fig.height=5

btw <- btw %>% 
  mutate(Grüne_rd = ifelse(Grüne_a21 != 0, (Grüne_a25 - Grüne_a21) / Grüne_a21*100, 0),
         Grüne_rg = ifelse(Grüne_rd > 0, Grüne_rd, 0),
         Grüne_rv = ifelse(Grüne_rd < 0, Grüne_rd, 0),
         Haustür_p = Haustür / Gebäude*100,
         Briefkasten_p = Briefkasten / Gebäude*100,
         Wahlkampf_p = (Haustür+Briefkasten) / Gebäude*100)

p_Brief <- ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = Briefkasten_p), alpha = geo_alpha2, col="black", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "Briefkastenwahlkampf", size = 3.5) +
  scale_size_continuous(range = c(0.1, max(btw$Briefkasten_p)*.03)) 

p_Haus <- ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = Haustür_p), alpha = geo_alpha2, col="black", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "Haustürwahlkampf", size = 3.5) +
  scale_size_continuous(range = c(0.1, max(btw$Haustür_p)*.03)) 

p_Wahlkampf <- ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = Wahlkampf_p), alpha = geo_alpha2, col="black", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "Wahlkampf gesamt", size = 3.5) +
  scale_size_continuous(range = c(0.1, max(btw$Wahlkampf_p)*.03)) 

p_Grüne <- ggplot(data = btw) +
  geom_sf(data = sf_stadtbezirke, alpha = 1, linewidth = .1, color = "black", fill = "#eeeeee") + 
  geom_point(aes(x = long, y = lat, size = -Grüne_rv), alpha = geo_alpha2, col="forestgreen", shape=geo_shape) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x = min(btw$long)+.08, y = min(btw$lat) - 0.015, label = "Grüne Verluste", size = 3.5) +
  scale_size_continuous(range = c(0.1, max(-btw$Grüne_rv)*.08)) 


grid.arrange(p_Brief, p_Haus, p_Wahlkampf, p_Grüne, ncol = 2)

#' 
#' Die relativen Stimmenverluste und die Briefwahl sind im Stadtgebiet recht gleichmäßig verteilt, während sich der Haustürwahlkampf
#' überwiegend auf die Stadtmitte konzentriert.
#' 
#' ### Wirksamkeit
#' Um eine Einschätzung der Wirksamkeit des Wahlkamps zu erhalten, wird im Folgenden der Zusammenhang der relativen Stimmverluste 
#' mit der Abdeckung der Gebäude im Wahlkampf untersucht.
#' 
#+ echo=FALSE, fig.width=7, fig.height=5

 
# Korrelation der prozentualen Veränderungen 
c_Haustür <- cor(btw$Grüne_rv, btw$Haustür_p, method = "pearson", use = "complete.obs")
c_Haustür_p <- cor.test(btw$Grüne_rv, btw$Haustür_p, method = "pearson", use = "complete.obs")$p.value
c_Haustür_s <- ifelse(c_Haustür_p <= 0.001, "***", ifelse(c_Haustür_p <= 0.01, "**", ifelse(c_Haustür_p <= 0.05, "*", "")))

c_Briefkasten <- cor(btw$Grüne_rv, btw$Briefkasten_p, method = "pearson", use = "complete.obs")
c_Briefkasten_p <- cor.test(btw$Grüne_rv, btw$Briefkasten_p, method = "pearson", use = "complete.obs")$p.value
c_Briefkasten_s <- ifelse(c_Briefkasten_p <= 0.001, "***", ifelse(c_Briefkasten_p <= 0.01, "**", ifelse(c_Briefkasten_p <= 0.05, "*", "")))

c_Wahlkampf <- cor(btw$Grüne_rv, btw$Wahlkampf_p, method = "pearson", use = "complete.obs")
c_Wahlkampf_p <- cor.test(btw$Grüne_rv, btw$Wahlkampf_p, method = "pearson", use = "complete.obs")$p.value
c_Wahlkampf_s <- ifelse(c_Wahlkampf_p <= 0.001, "***", ifelse(c_Wahlkampf_p <= 0.01, "**", ifelse(c_Wahlkampf_p <= 0.05, "*", "")))

p_Brief <-
  btw %>%
  ggplot(aes(y = Grüne_rv, x = Briefkasten_p)) +
  geom_point(color = "gray", size = .7) + labs(x = "Briefkasten (%)", y = "Stimmenverluste %") +
#  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text",
            x = mean(range(btw$Briefkasten_p, na.rm=T)),  # Horizontal center
            y = max(btw$Grüne_rv)+3,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Briefkasten,c_Briefkasten_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 0, 0))

p_Haus <-
  btw %>%
  ggplot(aes(y = Grüne_rv, x = Haustür_p)) +
  geom_point(color = "gray", size = .7) + labs(x = "Haustür (%)", y = "") +
#  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text",
            x = mean(range(btw$Haustür_p, na.rm=T)),  # Horizontal center
            y = max(btw$Grüne_rv)+3 ,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Haustür,c_Haustür_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 0, 0))

p_Wahlkampf <-
  btw %>%
  ggplot(aes(y = Grüne_rv, x = Wahlkampf_p)) +
  geom_point(color = "gray", size = .7) + labs(x = "Wahlkampf gesamt (%)", y = "") +
  geom_smooth(method = "lm", formula = y ~ x, color = "gray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text",
            x = mean(range(btw$Wahlkampf_p, na.rm=T)),  # Horizontal center
            y = max(btw$Grüne_rv)+3 ,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Wahlkampf,c_Wahlkampf_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 0, 0))


# Anordnen der Plots in einem Raster
grid.arrange(p_Brief, p_Haus, p_Wahlkampf, ncol = 2)

#' Beim Wahlkampf insgesamt zeigt sich ein schwacher Zusammenhang  mit den Wahlergebnissen. 
#' Stimmbezirke mit einem höheren Anteil an Gebäude, die im Wahlkampf abgedeckt wurden, zeigen geringere Stimmenverluste.
#' Bei der isolierten Betrachtung von Briefkasten- und Haustürwahlkampf lässt sich hingegen kein klarer Zusammenhang feststellen, was an der Datenqualität 
#' beziehungsweise dem vergleichsweisen geringen Anteil der Gebäude liegen kann, die im Haustürwahlkampf abgedeckt wurden. 
#' 
#' \newpage 
#' ### Simulation
#' 
#' Mit Hilfe einer linearen Regression wird im Folgenden simuliert, welches Wahlergebnis die Grüne in Bochum erzielt hätten, wenn
#' kein Briefkasten- und Haustürwahlkampf stattgefunden hätte. Die Stimmverluste wurden entsprechend der Stärke der Korrelation 
#' auf SPD, Linke, FDP und Volt verteilt.
#' 
#' Die folgende Grafik zeigt das simulierte Wahlergebnis und die Differenz zum tatsächlichen Wahlergebnis.
#' 
#+ echo=FALSE, fig.width=7, fig.height=4.5


# Lineare Regression Modell für relative Stimmverluste abhängig von Briefkasten und Haustürwahhlkampf
model <- lm(Grüne_rv ~ Wahlkampf_p, data = btw)

# Simulierte relative Stimmverluste je Stimmbezirk, abhängig von Aktivität in Stimmbezirk
btw <- btw %>% mutate(Grüne_rv_sim = Grüne_rv -
                        coef(model)["Wahlkampf_p"]* btw$Wahlkampf_p,
                      Grüne_sim = Grüne_a21 + Grüne_a21 * Grüne_rv_sim/100)

Grüne_sim <- sum(btw$Grüne_sim) / sum(btw$Gültig_a25)
sum_abs <- sum(abs(c( c_SPD, c_FDP, c_Linke, c_Volt)))
Grüne_diff <- - round(sum(btw$Grüne_a25 - btw$Grüne_sim))

totals <- btw %>%
  summarize(
    Gültig = sum(Gültig_a25), 
    SPD = (sum(SPD_a25) + round(c_SPD / sum_abs * Grüne_diff) )/Gültig,
    CDU = sum(CDU_a25)/Gültig,
    FDP = (sum(FDP_a25) + round(c_FDP / sum_abs * Grüne_diff) )/Gültig,
    AfD = sum(AfD_a25)/Gültig,
    Linke = (sum(Linke_a25) + round(c_Linke / sum_abs * Grüne_diff))/Gültig,
    Grüne = (sum(Grüne_a25) + Grüne_diff) /Gültig,
    Volt = (sum(Volt_a25) + round(c_Volt / sum_abs * Grüne_diff))/Gültig,
    Sonstige = 1 - sum(SPD, CDU, FDP, AfD, Linke, Grüne, Volt)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Party", values_to = "Total")

# Create the bar chart
p_absolute <- totals %>% filter(Party != "Gültig") %>%
  mutate(Party = factor(Party,levels = c("SPD","CDU","AfD","Grüne","Linke","FDP","Volt","Sonstige"))) %>%
  ggplot( aes(x = Party, y = Total, fill = Party)) +
  geom_bar( stat = "identity", show.legend = FALSE, , alpha = .6) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1), # Format y-axis as percentages
    expand = expansion(mult = c(0, 0.1)) # Add 10% padding to the top
  ) +
  geom_text(aes(label = percent(Total, accuracy = 0.1)), vjust = -0.5, size = 3) + # Add numbers on top of bars
  labs(x = "",y = "Zweistimmen (Simulaton) %") +
  theme_minimal() +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "black", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="green", "FDP"="#FFCC00","Volt"="purple"))
  


totals <- btw %>%
  summarize(
    Gültig = sum(Gültig_a25), 
    SPD = round(c_SPD / sum_abs * Grüne_diff)/Gültig,
    CDU = 0.00049,
    FDP = round(c_FDP / sum_abs * Grüne_diff)/Gültig,
    AfD = 0.00049,
    Linke = (c_Linke / sum_abs * Grüne_diff)/Gültig,
    Grüne = Grüne_diff/Gültig,
    Volt = round(c_Volt / sum_abs * Grüne_diff)/Gültig,
    Sonstige = 0.00049
  ) %>%
  pivot_longer(cols = everything(), names_to = "Party", values_to = "Total")

# Create the bar chart
p_diff <- totals %>% filter(Party != "Gültig") %>%
  mutate(Party = factor(Party,levels = c("SPD","CDU","AfD","Grüne","Linke","FDP","Volt","Sonstige"))) %>%
  ggplot( aes(x = Party, y = Total, fill = Party)) +
  geom_bar( stat = "identity", show.legend = FALSE, alpha = .6) +
  scale_y_continuous(
    labels = percent_format(accuracy = .1), # Format y-axis as percentages
    expand = expansion(mult = c(0, 0.5)) # Add 5% padding to the top
  ) +
  geom_text(aes(label = percent(Total, accuracy = 0.1)), vjust = -0.5, size = 3) + # Add numbers on top of bars
  labs(x = "",y = "Simulation +/-") +
  theme_minimal() +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "black", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="green", "FDP"="#FFCC00","Volt"="purple"))

# Anordnen der Plots in einem 1x2-Raster 
grid.arrange(p_absolute, p_diff, ncol = 1, heights = c(3, 1))


#' Nach dem Simulationsmodell hätten die Grünen ohne den Briefkasten- und Haustürwahlkampf 0,5% weniger Stimmanteile gehabt.

