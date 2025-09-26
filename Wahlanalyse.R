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
#' die absolut gesehen vor allem in der Stadtmitte hoch waren. Die Grünen haben in Bochum vor allem an die Linke verloren 
#' und konnten die Schwäche der SPD nicht nutzen. 
#' In Stimmbezirken mit hoher Wahlbeteiligung haben die Grünen vergleichsweise bessere Ergebnisse erzielt. 
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
#' In dieser Detailanalyse der Bundestagswahl 2025 werden die Ergebnisse der Grünen für Bochum 
#' unter Berücksichtigung regionaler und ausgewählter soziodemographischer Faktoren analysiert.
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
if (!require("eiPack")) install.packages("eiPack")
if (!require("grid")) install.packages("grid")
if (!require("ggforce")) install.packages("ggforce")



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
library(eiPack)
library(grid)
library(ggforce)

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
# F16 : BSW

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
#             Volt=sum(F12),
#             BSW=sum(F16,na.rm=T))


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
               BSW=F16,
               Kommunalbezirk=ifelse(
                 Stimmbezirk >= 9000,
                 substring(as.character(Stimmbezirk), 2, 3), 
                 substring(as.character(Stimmbezirk), 1, 2)),
               Stadtbezirk=substring(Kommunalbezirk, 1, 1)) %>% 
       select(Wahl,Stimmbezirk,Kommunalbezirk,Stadtbezirk,Berechtigt,Gültig,SPD,CDU,FDP,Grüne,AfD,Linke,Volt,BSW)

# # Gesamtergebnis prüfen
# dat25 %>%
# summarize(Gültig=sum(Gültig),
#           SPD=sum(SPD),
#           CDU=sum(CDU),
#           AfD=sum(AfD),
#           Grüne=sum(Grüne),
#           Linke=sum(Linke),
#           Volt=sum(Volt),
#           BSW=sum(BSW))


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
            BSW_kommunal=sum(ifelse(Stimmbezirk >= 9000,BSW,0)),
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
         Volt_all = ifelse(Stimmbezirk < 9000, Volt + Berechtigt/Berechtigt_lokal * Volt_kommunal, 0),
         BSW_all = ifelse(Stimmbezirk < 9000, BSW + Berechtigt/Berechtigt_lokal * BSW_kommunal, 0))


# # Überprüfung der Korrektheit der Verteilung
# dat25 %>%  filter(Kommunalbezirk=="10") %>%
#   group_by(Kommunalbezirk) %>%
#   summarize(Berechtigt=sum(Berechtigt, na.rm=T),
#             Gültig=sum(Gültig),SPD=sum(SPD),CDU=sum(CDU),AfD=sum(AfD),Grüne=sum(Grüne),Linke=sum(Linke),Volt=sum(Volt),
#             Gültig_all=sum(Gültig_all),SPD_all=sum(SPD_all), CDU_all=sum(CDU_all),AfD_all=sum(AfD_all),Grüne_all=sum(Grüne_all) ,Linke_all=sum(Linke_all),Volt_all=sum(Volt_all),BSW_all=sum(BSW_all),
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
#             BSW=sum(BSW_all),
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
# - : BSW
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
         BSW=0,
         Kommunalbezirk=ifelse(
           Stimmbezirk >= 9000,
           substring(as.character(Stimmbezirk), 2, 3), 
           substring(as.character(Stimmbezirk), 1, 2)),
         Stadtbezirk=substring(Kommunalbezirk, 1, 1)) %>% 
  select(Wahl,Stimmbezirk,Kommunalbezirk,Stadtbezirk,Berechtigt,Gültig,SPD,CDU,FDP,Grüne,AfD,Linke,Volt,BSW)

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
            BSW_kommunal=sum(ifelse(Stimmbezirk >= 9000,BSW,0)),
            .groups = "keep")

# # Überprüfung der Korrektheit der Aggregation
# Kommunalbezirk21 %>% filter(Kommunalbezirk=="10")
# dat21 %>%  filter((Kommunalbezirk=="10") & (Stimmbezirk >= 9000)) %>% 
#   summarise(across(c(Berechtigt, Gültig, SPD, CDU, FDP, Grüne, AfD, Linke, Volt,BSW), sum))
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
         Volt_all = ifelse(Stimmbezirk < 9000, Volt + Berechtigt/Berechtigt_lokal * Volt_kommunal, 0),
         BSW_all = ifelse(Stimmbezirk < 9000, BSW + Berechtigt/Berechtigt_lokal * BSW_kommunal, 0))



# # Überprüfung der Korrektheit der Verteilung         
# dat21 %>%  filter(Kommunalbezirk=="10") %>%
#   group_by(Kommunalbezirk) %>%
#   summarize(Gültig=sum(Gültig),SPD=sum(SPD),CDU=sum(CDU),AfD=sum(AfD),Grüne=sum(Grüne),Linke=sum(Linke),Volt=sum(Volt),
#             Gültig_all=sum(Gültig_all),SPD_all=sum(SPD_all), CDU_all=sum(CDU_all),AfD_all=sum(AfD_all),Grüne_all=sum(Grüne_all) ,Linke_all=sum(Linke_all),Volt_all=sum(Volt_all),BSW_all=sum(BSW_all),
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
#             BSW=sum(BSW_all),
#             .groups = "keep")

dat21a <- dat21 %>%
  filter(Stimmbezirk < 9000) %>%
  mutate(Berechtigt_a21=Berechtigt, 
         Gültig_a21=Gültig_all, 
         Gültig_p21 = Gültig_a21/Berechtigt*100,
         SPD_a21=SPD_all, 
         CDU_a21=CDU_all, 
         FDP_a21=FDP_all, 
         Grüne_a21=Grüne_all, 
         AfD_a21=AfD_all, 
         Linke_a21=Linke_all, 
         Volt_a21=Volt_all,
         BSW_a21=BSW_all,
         SPD_p21=SPD_all/Gültig_all*100, 
         CDU_p21=CDU_all/Gültig_all*100, 
         FDP_p21=FDP_all/Gültig_all*100, 
         Grüne_p21=Grüne_all/Gültig_all*100, 
         AfD_p21=AfD_all/Gültig_all*100, 
         Linke_p21=Linke_all/Gültig_all*100, 
         Volt_p21=Volt_all/Gültig_all*100,
         BSW_p21=BSW_all/Gültig_all*100) %>%
  select(Stimmbezirk,
         Berechtigt_a21, Gültig_a21, Gültig_p21, 
         SPD_a21,CDU_a21,FDP_a21,Grüne_a21,AfD_a21,Linke_a21,Volt_a21,BSW_a21,
         SPD_p21,CDU_p21,FDP_p21,Grüne_p21,AfD_p21,Linke_p21,Volt_p21, BSW_p21)
  
btw <- dat25 %>%
  filter(Stimmbezirk < 9000) %>%
  mutate(Berechtigt_a25=Berechtigt, 
         Gültig_a25=Gültig_all, 
         Gültig_p25 = Gültig_a25/Berechtigt*100,
         SPD_a25=SPD_all, 
         CDU_a25=CDU_all, 
         FDP_a25=FDP_all, 
         Grüne_a25=Grüne_all, 
         AfD_a25=AfD_all, 
         Linke_a25=Linke_all, 
         Volt_a25=Volt_all,
         BSW_a25=BSW_all,
         SPD_p25=SPD_all/Gültig_all*100, 
         CDU_p25=CDU_all/Gültig_all*100, 
         FDP_p25=FDP_all/Gültig_all*100, 
         Grüne_p25=Grüne_all/Gültig_all*100, 
         AfD_p25=AfD_all/Gültig_all*100, 
         Linke_p25=Linke_all/Gültig_all*100, 
         Volt_p25=Volt_all/Gültig_all*100,
         BSW_p25=BSW_all/Gültig_all*100) %>%
  select(Stimmbezirk,Kommunalbezirk,Stadtbezirk,
         Berechtigt_a25, Gültig_a25, Gültig_p25, 
         SPD_a25,CDU_a25,FDP_a25,Grüne_a25,AfD_a25,Linke_a25,Volt_a25,BSW_a25,
         SPD_p25,CDU_p25,FDP_p25,Grüne_p25,AfD_p25,Linke_p25,Volt_p25,BSW_p25) %>%
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
         Volt_pd = Volt_p25 - Volt_p21,
         BSW_ad = BSW_a25 - BSW_a21,
         BSW_pd = BSW_p25 - BSW_p21)

         
# # Prüfung Ergebnisse 2025
# btw %>% summarize(Berechtigt=sum(Berechtigt_a25, na.rm=T),
#                   Gültig=sum(Gültig_a25),
#                   SPD=sum(SPD_a25),
#                   CDU=sum(CDU_a25),
#                   AfD=sum(AfD_a25),
#                   Grüne=sum(Grüne_a25),
#                   Linke=sum(Linke_a25),
#                   Volt=sum(Volt_a25),
#                   BSW=sum(BSW_a25))
#
# 
# # Prüfung Ergebnisse 2021
# btw %>% summarize(Berechtigt=sum(Berechtigt_a21, na.rm=T),
#                   Gültig=sum(Gültig_a21),
#                   SPD=sum(SPD_a21),
#                   CDU=sum(CDU_a21),
#                   AfD=sum(AfD_a21),
#                   Grüne=sum(Grüne_a21),
#                   Linke=sum(Linke_a21),
#                   Volt=sum(Volt_a21),
#                   BSW=sum(BSW_a21))
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

# # Statistikbereich prüfen
# btw$Kommunalbezirk[1:20]
# btw$Statistikbezirk[1:20]

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

#load Stimmbezirke as shoapefile
stimmbezirke <- st_read("bochum_wahlen_bundestagswahl-2025_stimmbezirke/bochum_wahlen_bundestagswahl-2025_stimmbezirke.shp")
# convert to sf object
sf_stimmbezirke <- st_transform(stimmbezirke, crs = 4326)
names(sf_stimmbezirke)[names(sf_stimmbezirke) == "Stimmbez"] <- "Stimmbezirk"

# Join kommunalbezirk from btw to stimmbezirke and aggregate geometries of Kommunalbezirke
sf_kommunalbezirke <- sf_stimmbezirke %>%
  left_join((btw %>% select(Stimmbezirk, Kommunalbezirk)), by = "Stimmbezirk") %>%
  mutate(geometry = st_make_valid(geometry))  %>%
  group_by(Kommunalbezirk) %>%  # Group by Kommunalbezirk
  summarise(geometry = st_union(geometry))  # Combine geometries within each group

# read file with names of Kommunalbezirke
Kommunalbezirke <- read_excel("kommunalbezirke.xlsx")
Kommunalbezirke <- Kommunalbezirke %>%
  mutate(Kommunalname = substr(Kommunalname, 4, 100),
         Kommunalbezirk = as.character(Kommunalbezirk))

# join names of Kommunalbezirke to sf_kommunalbezirke
sf_kommunalbezirke <- sf_kommunalbezirke %>%
  left_join(Kommunalbezirke, by = "Kommunalbezirk")

# add center of geometry as point for labels
sf_kommunalbezirke <- cbind(sf_kommunalbezirke, st_coordinates(st_centroid(sf_kommunalbezirke$geometry)))

# shorten a few labels
sf_kommunalbezirke$Kommunalname[sf_kommunalbezirke$Kommunalname=="Innenstadt-Nord/Schmechtingwiese"] <- "Innenstadt-Nord"
sf_kommunalbezirke$Kommunalname[sf_kommunalbezirke$Kommunalname=="Langendreer-Nord/Ümmingen"] <- "Langendreer-Nord"

# adjust a few points for better label placement
sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="WAT-Mitte/Westenfeld"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="WAT-Mitte/Westenfeld"] + .001
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="WAT-Mitte/Westenfeld"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="WAT-Mitte/Westenfeld"] + .0005

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Günnigfeld/Südfeldmark"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Günnigfeld/Südfeldmark"] - .003
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Günnigfeld/Südfeldmark"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Günnigfeld/Südfeldmark"] + .003

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Höntrop-Nord"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Höntrop-Nord"] + .001
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Höntrop-Nord"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Höntrop-Nord"] + .0045

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Höntrop-Süd/Sevinghausen"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Höntrop-Süd/Sevinghausen"] - .0035
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Höntrop-Süd/Sevinghausen"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Höntrop-Süd/Sevinghausen"] + .0005

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Eppendorf/Munscheid"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Eppendorf/Munscheid"] + .004
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Eppendorf/Munscheid"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Eppendorf/Munscheid"] + .003

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Weitmar-Mitte"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Weitmar-Mitte"] + .003
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Weitmar-Mitte"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Weitmar-Mitte"] + .000

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Weitmar-Süd"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Weitmar-Süd"] + .000
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Weitmar-Süd"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Weitmar-Süd"] - .002

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Wiemelhausen"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Wiemelhausen"] - .001
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Wiemelhausen"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Wiemelhausen"] + .004

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Innenstadt-Südost"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Innenstadt-Südost"] + .003
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Innenstadt-Südost"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Innenstadt-Südost"] - .0025

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Innenstadt-Nord"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Innenstadt-Nord"] + .001
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Innenstadt-Nord"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Innenstadt-Nord"] - .000

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Altenbochum"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Altenbochum"] + .003
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Altenbochum"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Altenbochum"] - .0015

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Laer/Werne-West"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Laer/Werne-West"] - .002
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Laer/Werne-West"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Laer/Werne-West"] - .003

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Langendreer-Nord"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Langendreer-Nord"] - .001
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Langendreer-Nord"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Langendreer-Nord"] - .008

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Langendreer-Ost"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Langendreer-Ost"] - .001
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Langendreer-Ost"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Langendreer-Ost"] + .003

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Steinkuhl"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Steinkuhl"] + .000
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Steinkuhl"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Steinkuhl"] + .004

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Bärendorf"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Bärendorf"] + .000
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Bärendorf"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Bärendorf"] - .0015

sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Querenburg"] <- 
  sf_kommunalbezirke$Y[sf_kommunalbezirke$Kommunalname=="Querenburg"] + .000
sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Querenburg"] <- 
  sf_kommunalbezirke$X[sf_kommunalbezirke$Kommunalname=="Querenburg"] + .005


# replace "/" by " " and "-" by "- " in all Kommunalname for wrapping
sf_kommunalbezirke$Kommunalname <- str_replace_all(sf_kommunalbezirke$Kommunalname, "Langendreer", "Langen-dreer")
sf_kommunalbezirke$Kommunalname <- str_replace_all(sf_kommunalbezirke$Kommunalname, "/", " ")
sf_kommunalbezirke$Kommunalname <- str_replace_all(sf_kommunalbezirke$Kommunalname, "-", "- ")

# group btw results into btw_k by Kommunalbezirk
btw_k <- btw %>%
  group_by(Kommunalbezirk) %>%
  summarize(
    Gültig_a25 = sum(Gültig_a25), 
    SPD_a25 = sum(SPD_a25),
    CDU_a25 = sum(CDU_a25),
    FDP_a25 = sum(FDP_a25),
    AfD_a25 = sum(AfD_a25),
    Linke_a25 = sum(Linke_a25),
    Grüne_a25 = sum(Grüne_a25),
    Volt_a25 = sum(Volt_a25),
    SPD_p25 = SPD_a25 / Gültig_a25,
    CDU_p25 = CDU_a25 / Gültig_a25,
    FDP_p25 = FDP_a25 / Gültig_a25,
    AfD_p25 = AfD_a25 / Gültig_a25,
    Linke_p25 = Linke_a25 / Gültig_a25,
    Grüne_p25 = Grüne_a25 / Gültig_a25,
    Volt_p25 = Volt_a25 / Gültig_a25,
    Gültig_a21 = sum(Gültig_a21),
    SPD_a21 = sum(SPD_a21),
    CDU_a21 = sum(CDU_a21),
    FDP_a21 = sum(FDP_a21),
    AfD_a21 = sum(AfD_a21),
    Linke_a21 = sum(Linke_a21),
    Grüne_a21 = sum(Grüne_a21),
    Volt_a21 = sum(Volt_a21),
    SPD_p21 = SPD_a21 / Gültig_a21,
    CDU_p21 = CDU_a21 / Gültig_a21,
    FDP_p21 = FDP_a21 / Gültig_a21,
    AfD_p21 = AfD_a21 / Gültig_a21,
    Linke_p21 = Linke_a21 / Gültig_a21,
    Grüne_p21 = Grüne_a21 / Gültig_a21,
    Volt_p21 = Volt_a21 / Gültig_a21,
    SPD_pd = SPD_p25 - SPD_p21,
    CDU_pd = CDU_p25 - CDU_p21,
    FDP_pd = FDP_p25 - FDP_p21,
    AfD_pd = AfD_p25 - AfD_p21,
    Linke_pd = Linke_p25 - Linke_p21,
    Grüne_pd = Grüne_p25 - Grüne_p21,
    Volt_pd = Volt_p25 - Volt_p21
  )



# Wahllokale mit Adressen - not needed any more using shapefiles of Stimmbezirke 
#
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
#
#
# filename <- "Wahllokale_geocoded.csv"
# wahllokale <- read_csv(filename)
# 
# # rename district key
# names(wahllokale)[names(wahllokale) == "Bezirk-Nr"] <- "Stimmbezirk"
# wahllokale <- wahllokale %>% select(Stimmbezirk, lat, long)
# 
# # Zu Wahlergebnis ergänzen
# btw <- btw %>%
#   left_join(wahllokale, by = "Stimmbezirk")


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

# Anzeige der Häuser
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

# Convert `Hausnr. gerade von` into number ignoring "unbebaut"
suppressWarnings({
  Wahlkampf <- Wahlkampf %>% 
    mutate(`Hausnr. gerade von` = parse_number(`Hausnr. gerade von`))
})


# Für jeden Straßenabschnitt in Wahlkampf zählen, wieviele Gebäude es gibt 
Wahlkampf <- Wahlkampf %>%
  rowwise() %>%
  mutate( Gebäude_gerade = ifelse(is.na(`Hausnr. gerade von`),0,
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
  geom_bar(stat = "identity", show.legend=FALSE, color = "black", linewidth=0.2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1), # Format y-axis as percentages
    expand = expansion(mult = c(0, 0.1)) # Add 10% padding to the top
  ) +
  geom_text(aes(label = percent(Total, accuracy = 0.1)), vjust = -0.5, size = 3) + # Add numbers on top of bars
  labs(x = "",y = "Zweistimmenanteil") +
  theme_minimal() +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "#404040", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="limegreen", "FDP"="#FFCC00","Volt"="purple", "Sonstige"="darkgray"))



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
  geom_bar(stat = "identity", show.legend=FALSE, color = "black", linewidth=0.2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1), # Format y-axis as percentages
    expand = expansion(mult = c(0, 0.1)) # Add 5% padding to the top
  ) +
  geom_text(aes(label = percent(Total, accuracy = 0.1)), vjust = -0.5, size = 3) + # Add numbers on top of bars
  labs(x = "",y = "Vergleich 2021") +
  theme_minimal() +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "#404040", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="limegreen", "FDP"="#FFCC00","Volt"="purple", "Sonstige"="darkgray"))

# Anordnen der Plots in einem 1x2-Raster 
grid.arrange(p_absolut, p_diff, ncol = 1, heights = c(1.5, 1))



#' 
#' Die SPD bleibt in der Stadt Bochum knapp in Führung. Die Ergebnisse der Grünen liegen über dem Bundesdurchschnitt (11,6 %).
#' Wie im Bund verlieren alle Ampelparteien Stimmenanteile gegenüber 2021.
#' 
#' \newpage
#' ### Vergleich zum Bundesergebnis
#' 
#' Die folgende Grafik zeigt den Vergleich zum Ergebnis für Deutschland gesamt. Die helleren Werte stellen das Bundesergebnis dar.
#+ echo=FALSE, fig.width=7, fig.height=5

# Combine and prepare the data
totals <- btw %>%
  summarize(
    Result = "Bochum",
    Gültig = sum(Gültig_a25), 
    SPD = sum(SPD_a25) / Gültig,
    CDU = sum(CDU_a25) / Gültig,
    FDP = sum(FDP_a25) / Gültig,
    AfD = sum(AfD_a25) / Gültig,
    Linke = sum(Linke_a25) / Gültig,
    Grüne = sum(Grüne_a25) / Gültig,
    Volt = sum(Volt_a25) / Gültig,
    Sonstige = (Gültig - sum(SPD_a25, CDU_a25, FDP_a25, AfD_a25, Linke_a25, Grüne_a25, Volt_a25)) / Gültig
  ) %>%
  pivot_longer(cols = -Result, names_to = "Party", values_to = "Total")

national_data <- data.frame(
  Result = "Deutschland",
  Party = c("SPD", "CDU", "AfD", "Grüne", "FDP", "Linke", "Volt", "Sonstige"),
  Total = c(0.164, 0.286, 0.208, 0.116, 0.043, 0.088, 0.007, 0.088)
)

# Combine Bochum and Deutschland data
combined_data <- rbind(totals %>% filter(Party != "Gültig"), national_data)

# Ensure Party is a factor with consistent levels
combined_data <- combined_data %>%
  mutate(
    Party = factor(Party, levels = c("SPD", "CDU", "AfD", "Grüne", "Linke", "FDP", "Volt", "Sonstige")),
    Result = factor(Result, levels = c("Bochum", "Deutschland"))
  )

# Create the grouped bar chart
p_absolut <- combined_data  %>%
  ggplot( aes(x = Party, y = Total, fill = Party, alpha = Result)) +
  geom_bar(stat = "identity", show.legend=FALSE, position = position_dodge(width = .9), color = "black", linewidth=0.2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1), # Format y-axis as percentages
    expand = expansion(mult = c(0, 0.1)) # Add 10% padding to the top
  ) +
  geom_text(aes(label = percent(Total, accuracy = 0.1)), vjust = -0.5, size = 3,position = position_dodge(width = 0.9)) + # Add numbers on top of bars
  labs(x = "",y = "Zweistimmenanteil") +
  theme_minimal() +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "#404040", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="limegreen", "FDP"="#FFCC00","Volt"="purple", "Sonstige"="darkgray"))+
  scale_alpha_manual(values = c("Bochum" = 1, "Deutschland" = 0.5)) +
  theme(legend.position = "none") 

totals <- btw %>%
  summarize(
    Result = "Bochum",
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
  pivot_longer(cols = -Result, names_to = "Party", values_to = "Total")

national_data <- data.frame(
  Result = "Deutschland",
  Party = c("SPD", "CDU", "AfD", "Grüne", "FDP", "Linke", "Volt", "Sonstige"),
    Total = c(0.164 - 0.257, 0.286 - 0.242, 0.208 - 0.104, 0.116 - 0.147, 0.043 - 0.114, 0.088 - 0.049, 0.007-0.004, .137 - .086 + .002 -.001 -0.043 -0.007+0.004)
)

# Combine Bochum and Deutschland data
combined_data <- rbind(totals %>% filter(Party != "Gültig"), national_data)

# Ensure Party is a factor with consistent levels
combined_data <- combined_data %>%
  mutate(
    Party = factor(Party, levels = c("SPD", "CDU", "AfD", "Grüne", "Linke", "FDP", "Volt", "Sonstige")),
    Result = factor(Result, levels = c("Bochum", "Deutschland"))
  )

# Create the bar chart
p_diff <- combined_data  %>%
  ggplot( aes(x = Party, y = Total, fill = Party, alpha = Result)) +
  geom_bar(stat = "identity", show.legend=FALSE, position = position_dodge(width = 0.9), color = "black", linewidth=0.2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1), # Format y-axis as percentages
    expand = expansion(mult = c(0, 0.1)) # Add 5% padding to the top
  ) +
  geom_text(aes(label = percent(Total, accuracy = 0.1)), vjust = -0.5, size = 3, position = position_dodge(width = 0.9)) + # Add numbers on top of bars
  labs(x = "",y = "Vergleich 2021") +
  theme_minimal() +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "#404040", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="limegreen", "FDP"="#FFCC00","Volt"="purple", "Sonstige"="darkgray"))+
  scale_alpha_manual(values = c("Bochum" = 1, "Deutschland" = 0.5)) +
  theme(legend.position = "none") 

# Anordnen der Plots in einem 1x2-Raster 
grid.arrange(p_absolut, p_diff, ncol = 1, heights = c(1.5, 1))

#' Die Grünen haben in Bochum ein besseres Ergebnis als im Bundesdurchschnitt erzielt, jedoch etwas größere Verluste als im Bund hinnehmen müssen.
#' Die SPD ist in Bochum deutlich stärker als im Bundesdurchschnitt und erleidet in Bochum vergleichsweise geringere Verluste.
#' CDU und AfD bleiben in Bochum hinter den Bundesergebnissen zurück.
#' Die Linke gewinnt in Bochum stärker als im Bund und erzielt damit auch ein besseres Ergebnis in Bochum als in ganz Deutschland.
#' 
#' ### Auswertung nach Briefwahl und Wahllokal
#'
#+ echo=FALSE, fig.width=4, fig.height=2


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
    Wahltyp = factor(Wahltyp, levels = c("Gültig gesamt" , "Wahllokal", "Briefwahl")),
    Percentage = Total / Total[Wahltyp == "Gültig gesamt"] * 100
  ) %>%
  ggplot(aes(x = Wahltyp, y = Total)) +
  geom_bar(stat = "identity", fill = "#888888", show.legend = FALSE, width = .85, color = "black", linewidth=0.2) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), 
    expand = expansion(mult = c(0, 0.1))  # Add 10% padding to the top
  ) +
  geom_text(
    aes(label = paste0(format(floor(Total), big.mark = ".", decimal.mark = ","), 
                       " (", round(Percentage, 0), "%)")),
    vjust = -.5, # Center text vertically within the bars
    size = 3
  ) +  # Add percentages for specific bars
  labs(x = "", y = "") +
  theme_minimal() 





#' \newpage
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
  geom_bar(stat = "identity", show.legend=FALSE, color = "black", linewidth=0.2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1), # Format y-axis as percentages
    expand = expansion(mult = c(0, 0.1)) # Add 5% padding to the top
  ) +
  geom_text(aes(label = percent(Total, accuracy = 0.1)), vjust = -0.5, size = 3) + # Add numbers on top of bars
  labs(x = "",y = "Stimmenanteil im Wahllokal") +
  theme_minimal() +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "#404040", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="limegreen", "FDP"="#FFCC00","Volt"="purple", "Sonstige"="darkgray"))


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
  geom_bar(stat = "identity", show.legend=FALSE, color = "black", linewidth=0.2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1), # Format y-axis as percentages
    expand = expansion(mult = c(0, 0.1)) # Add 5% padding to the top
  ) +
  geom_text(aes(label = percent(Total, accuracy = 0.1)), vjust = -0.5, size = 3) + # Add numbers on top of bars
  labs(x = "",y = "Stimmenanteil Briefwahll") +
  theme_minimal() +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "#404040", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="limegreen", "FDP"="#FFCC00","Volt"="purple", "Sonstige"="darkgray"))


# Anordnen der Plots in einem 1x2-Raster 
grid.arrange(p1, p2, ncol = 1)

#' 
#' Es gibt deutliche Unterschiede zwischen den Ergebnissen der Briefwahl und der Stimmabgabe in den Wahllokalen am Wahltag.
#' Dies kann auf unterschiedliche Wählerklientel sowie die unterschiedlichen Zeitpunkte der Stimmabgabe zurückzuführen sein. 
#' 
#' \newpage
#' ### Stimmenverteilung im Stadtgebiet
#' 
#' Die folgenden Karten zeigen die prozentualen Stimmenanteile als räumliche Verteilung.
#' 
#+ echo=FALSE, fig.width=7, fig.height=8

sf_stimmbezirke_CDU <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, CDU_p25), by = "Stimmbezirk")
p_CDU <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_CDU, aes(fill = CDU_p25), linewidth = 0, color = "white") +
  scale_fill_gradient(low = "white", high = "#404040", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .2, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA, fontface = "bold") +
  labs(subtitle = "CDU", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      

sf_stimmbezirke_SPD <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, SPD_p25), by = "Stimmbezirk")
p_SPD <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_SPD, aes(fill = SPD_p25), linewidth = 0.001, color = "white") +
  scale_fill_gradient(low = "white", high = "red2", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .1, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA) +
  labs(subtitle = "SPD", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      
  

sf_stimmbezirke_AfD <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, AfD_p25), by = "Stimmbezirk")
p_AfD <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_AfD, aes(fill = AfD_p25), linewidth = 0.001, color = "white") +
  scale_fill_gradient(low = "white", high = "#4169E1", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .1, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA) +
  labs(subtitle = "AfD", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      


sf_stimmbezirke_Grüne <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, Grüne_p25), by = "Stimmbezirk")
p_Grüne <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_Grüne, aes(fill = Grüne_p25), linewidth = 0.001, color = "white") +
  scale_fill_gradient(low = "white", high = "limegreen", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .1, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA) +
  labs(subtitle = "Grüne", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      


sf_stimmbezirke_Linke <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, Linke_p25), by = "Stimmbezirk")
p_Linke <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_Linke, aes(fill = Linke_p25), linewidth = 0.001, color = "white") +
  scale_fill_gradient(low = "white", high = "deeppink", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .1, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA) +
  labs(subtitle = "Linke", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      


sf_stimmbezirke_FDP <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, FDP_p25), by = "Stimmbezirk")
p_FDP <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_FDP, aes(fill = FDP_p25), linewidth = 0.001, color = "white") +
  scale_fill_gradient(low = "white", high = "#DDAA00", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .1, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA) +
  labs(subtitle = "FDP", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      


grid.arrange(p_CDU, p_SPD, p_AfD, p_Grüne, p_Linke, p_FDP, ncol = 2)


#'
#' Die räumliche Verteilung der Zweitstimmen zeigt, dass die Grünen und Linken besonders stark im Stadtzentrum sind,
#' während die CDU im Südwesten und die AfD im Osten und Westen der Stadt besser abschneiden.
#' 
#' \newpage
#' ### Stimmengewinne beziehungsweise -verluste im Stadtgebiet
#' 
#' Gewinne der CDU, AfD und Linke, sowie Verluste von SPD, Grüne und FDP gegenüber 2021 werden auf den folgenden Karten 
#' anhand der Veränderung der Stimmenanteile dargestellt.
#' 
#+ echo=FALSE, fig.width=7, fig.height=8

btw <- btw %>% 
  mutate(SPD_pv = ifelse(SPD_pd < 0, SPD_pd, 0),
         SPD_pg = ifelse(SPD_pd > 0, SPD_pd, 0),
         CDU_pv = ifelse(CDU_pd < 0, CDU_pd, 0),
         CDU_pg = ifelse(CDU_pd > 0, CDU_pd, 0),
         AfD_pv = ifelse(AfD_pd < 0, AfD_pd, 0),
         AfD_pg = ifelse(AfD_pd > 0, AfD_pd, 0),
         FDP_pv = ifelse(FDP_pd < 0, FDP_pd, 0),
         FDP_pg = ifelse(FDP_pd > 0, FDP_pd, 0),
         Grüne_pv = ifelse(Grüne_pd < 0, Grüne_pd, 0),
         Grüne_pg = ifelse(Grüne_pd > 0, Grüne_pd, 0),
         Linke_pv = ifelse(Linke_pd < 0, Linke_pd, 0),
         Linke_pg = ifelse(Linke_pd > 0, Linke_pd, 0))

sf_stimmbezirke_CDU <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, CDU_pg), by = "Stimmbezirk")
p_CDU <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_CDU, aes(fill = CDU_pg), linewidth = 0, color = "white") +
  scale_fill_gradient(low = "white", high = "#404040", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .2, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA, fontface = "bold") +
  labs(subtitle = "CDU Gewinne", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      

sf_stimmbezirke_SPD <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, SPD_pv), by = "Stimmbezirk")
p_SPD <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_SPD, aes(fill = SPD_pv), linewidth = 0, color = "white") +
  scale_fill_gradient(high = "white", low = "red2", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .2, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA, fontface = "bold") +
  labs(subtitle = "SPD Verluste", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      

sf_stimmbezirke_AfD <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, AfD_pg), by = "Stimmbezirk")
p_AfD <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_AfD, aes(fill = AfD_pg), linewidth = 0, color = "white") +
  scale_fill_gradient(low = "white", high = "#4169E1", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .2, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA, fontface = "bold") +
  labs(subtitle = "AfD Gewinne", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      

sf_stimmbezirke_Grüne <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, Grüne_pv), by = "Stimmbezirk")
p_Grüne <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_Grüne, aes(fill = Grüne_pv), linewidth = 0, color = "white") +
  scale_fill_gradient(high = "white", low = "limegreen", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .2, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA, fontface = "bold") +
  labs(subtitle = "Grüne Verluste", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      

sf_stimmbezirke_Linke <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, Linke_pg), by = "Stimmbezirk")
p_Linke <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_Linke, aes(fill = Linke_pg), linewidth = 0, color = "white") +
  scale_fill_gradient(low = "white", high = "deeppink", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .2, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA, fontface = "bold") +
  labs(subtitle = "Linke Gewinne", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      


sf_stimmbezirke_FDP <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, FDP_pv), by = "Stimmbezirk")
p_FDP <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_FDP, aes(fill = FDP_pv), linewidth = 0, color = "white") +
  scale_fill_gradient(high = "white", low = "#DDAA00", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .2, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA, fontface = "bold") +
  labs(subtitle = "FDP Verluste", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      


grid.arrange(p_CDU, p_SPD, p_AfD, p_Grüne, p_Linke, p_FDP, ncol = 2)



#' SPD, Grüne und FDP haben in allen Stimmbezirken Stimmen verloren, während AfD, Linke und CDU in nahezu allen
#' Bezirken Stimmen gewonnen haben. Die Veränderungen von SPD, CDU und FDP variieren wenig im Stadtgebiet. Die AfD hat vor allem 
#' im Westen und Osten der Stadt gewonnen. Grüne verlieren und Linke gewinnen besonders in der Stadtmitte.
#'
#' 
#' ## Wählerwanderungen
#' Im Folgenden werden die Wählerwanderungen nach der [KOWAHL-Methode](https://www.staedtestatistik.de/arbeitsgemeinschaften/kosis/kowahl) bestimmt, die unter dem Dach
#' des Verbands Deutsche Städtestatistik entwickelt wurde. 
#'
#+ echo=FALSE, fig.width=7, fig.height=4



################################################################
#
# Wählerwanderung nach KOWAHL Methode mit eiPack
# partial model with all parties but FDP and Volt
#
################################################################

############################
# Calculate the model
############################

# btw_clean <- btw %>%
#   mutate(across(where(is.numeric), round)) %>%
#   mutate( Gültig_a21 = round(Gültig_a21 * Berechtigt_a25/Berechtigt_a21),
#           SPD_a21 = round(SPD_a21 * Berechtigt_a25/Berechtigt_a21),
#           CDU_a21 = round(CDU_a21 * Berechtigt_a25/Berechtigt_a21),
#           Grüne_a21 = round(Grüne_a21 * Berechtigt_a25/Berechtigt_a21),
#           FDP_a21 = round(FDP_a21 * Berechtigt_a25/Berechtigt_a21),
#           AfD_a21 = round(AfD_a21 * Berechtigt_a25/Berechtigt_a21),
#           Linke_a21 = round(Linke_a21 * Berechtigt_a25/Berechtigt_a21),
#           BSW_a21 = round(BSW_a21 * Berechtigt_a25/Berechtigt_a21),
#           Sonstige_a21 = Gültig_a21 - SPD_a21 - CDU_a21 - Grüne_a21 - FDP_a21 - AfD_a21 - Linke_a21 - BSW_a21,
#           Nichtwähler_a21 = Berechtigt_a25 - Gültig_a21, # use Berechtigt_a25 deliberately
#           Sonstige_a25 = Gültig_a25 - SPD_a25 - CDU_a25 - Grüne_a25 - FDP_a25 - AfD_a25 - Linke_a25 - BSW_a25,
#           Nichtwähler_a25 = Berechtigt_a25 - Gültig_a25) %>%
#   mutate(across(where(is.numeric), round)) %>%
#   mutate(across(where(is.numeric), as.integer)) %>%
#   select(SPD_a21, CDU_a21, Grüne_a21, FDP_a21, AfD_a21, Linke_a21, BSW_a21, Sonstige_a21, Nichtwähler_a21,
#          SPD_a25, CDU_a25, Grüne_a25, FDP_a25, AfD_a25, Linke_a25, BSW_a25, Sonstige_a25, Nichtwähler_a25)
# 
# 
# 
# # Calculate row totals for 2021 and 2025
# row_totals <- rowSums(btw_clean[, c("SPD_a21", "CDU_a21", "Grüne_a21", "FDP_a21", "AfD_a21", "Linke_a21", "BSW_a21", "Sonstige_a21", "Nichtwähler_a21")])
# column_totals <- rowSums(btw_clean[, c("SPD_a25", "CDU_a25", "Grüne_a25", "FDP_a25", "AfD_a25", "Linke_a25", "BSW_a25", "Sonstige_a25", "Nichtwähler_a25")])
# 
# # Check if totals match
# all(row_totals == column_totals)
# 
# # Define your RxC formula
# formula <- cbind(SPD_a21, CDU_a21, Grüne_a21, FDP_a21, AfD_a21, Linke_a21, BSW_a21, Sonstige_a21, Nichtwähler_a21) ~
#   cbind(SPD_a25, CDU_a25, Grüne_a25, FDP_a25, AfD_a25, Linke_a25, BSW_a25, Sonstige_a25, Nichtwähler_a25)
# 
# tune_btw <- tuneMD(formula,
#                    data = btw_clean,
#                    ntunes = 10,
#                    totaldraws = 250000)
# 
# # ei_result <- ei.MD.bayes(
# #   formula = formula,
# #   data = btw_clean,
# #   tune.list = tune_btw,
# #   sample = 100,      # Number of MCMC samples
# #   burnin = 5000,       # Number of burn-in iterations
# #   thin = 100,           # Thinning interval to reduce autocorrelation
# #   ret.mcmc = T,
# #   ret.beta = 'r',
# #   verbose =1000
# # )
# ei_result <- ei.MD.bayes(
#   formula = formula,
#   data = btw_clean,
#   tune.list = tune_btw,
#   sample = 10000,      # Number of MCMC samples
#   burnin = 250000,       # Number of burn-in iterations
#   thin = 2000,           # Thinning interval to reduce autocorrelation
#   ret.mcmc = T,
#   ret.beta = 'r',
#   verbose = 25000
# )
# 
# 
# transition_matrix <- ei_result$draws$Beta
# 
# # Extract parameter names
# param_names <- attr(transition_matrix, "dimnames")[[2]]
# 
# # Convert transition_matrix to data frame
# transition_df <- as.data.frame(transition_matrix)
# 
# # Add parameter names as column names
# colnames(transition_df) <- param_names
# 
# # Pivot longer to associate parameters with samples
# long_transition_df <- pivot_longer(
#   transition_df,
#   cols = everything(),
#   names_to = "Parameter",
#   values_to = "Sample"
# )
# 
# # Extract Origin and Destination Groups:
# long_transition_df <- long_transition_df %>%
#   mutate(
#     Destination = sub("beta\\.([^.]+)\\..*\\..*", "\\1", Parameter),  # Extract Destination group
#     Origin = sub("beta\\.[^.]+\\.([^.]+)\\..*", "\\1", Parameter)     # Extract Origin group
#   )
# 
# 
# # Summarize Posterior Distributions:
# voter_movements <- long_transition_df %>%
#   group_by(Origin, Destination) %>%
#   summarize(
#     Mean = mean(Sample),
#     .groups = "drop"
#   )
# 
# # normalize probabilities
# voter_movements_norm <- voter_movements %>%
#   group_by(Origin) %>%
#   mutate(Normalized_Mean = Mean / sum(Mean)) %>%
#   ungroup() %>%
#   select(Origin, Destination, Normalized_Mean)
# 
# means_matrix <- voter_movements_norm %>%
#   pivot_wider(names_from = Destination, values_from = Normalized_Mean) %>%
#   column_to_rownames("Origin")  # Set Origin as row names
# 
# write.table(means_matrix, file = "Wählerwanderung_BOCHUM_SPD_CDU_AfD_Grüne_Linke_BSW_FDP_Sonstige_md.txt",sep = "\t", row.names = TRUE, col.names = TRUE)



############################
# Read the model from file
############################

# read table means_matrix from Wählerwanderung_SPD_CDU_AfD_Grüne_Linke_Sonstige_1000.txt
means_matrix <- read.table(file = "Wählerwanderung_BOCHUM_SPD_CDU_AfD_Grüne_Linke_BSW_FDP_Sonstige_md.txt", sep = "\t", row.names = 1, header = TRUE)

# Extract mean draws for voters transitioning to Grüne
mean_to_Grüne <- means_matrix[c("SPD_a21", "CDU_a21", "AfD_a21", "BSW_a21", "FDP_a21", "Linke_a21", "Sonstige_a21", "Nichtwähler_a21"), "Grüne_a25"]

# calculate totals for 2021
totals_2021 <- btw %>% 
  summarise(Sonstige_a21 = sum(Gültig_a21 - SPD_a21 - CDU_a21 - Grüne_a21 - FDP_a21 - AfD_a21 - Linke_a21 - BSW_a21),
            SPD_a21 = sum(SPD_a21),
            CDU_a21 = sum(CDU_a21),
            AfD_a21 = sum(AfD_a21),
            BSW_a21 = sum(BSW_a21),
            FDP_a21 = sum(FDP_a21),
            Linke_a21 = sum(Linke_a21),
            Nichtwähler_a21 = sum(Berechtigt_a25 - Gültig_a21)) %>%
  select(SPD_a21, CDU_a21, AfD_a21, BSW_a21, FDP_a21, Linke_a21, Sonstige_a21, Nichtwähler_a21) %>%
  unlist()

# Multiply totals_2021 with mean_to_Grüne (element-wise multiplication)
voters_to_Grüne <- totals_2021 * mean_to_Grüne


# Extract posterior draws for voters transitioning from Grüne
mean_from_Grüne <- means_matrix["Grüne_a21", c("SPD_a25", "CDU_a25", "AfD_a25", "BSW_a25", "FDP_a25", "Linke_a25", "Sonstige_a25", "Nichtwähler_a25" ) ]

# calculate totals for 2021
Grüne_2021 <- btw %>% 
  summarise(Grüne_a21 = sum(Grüne_a21)) %>%
  pull()

# Multiply Grüne_2021 with mean_from_Grüne (element-wise multiplication)
voters_from_Grüne <- Grüne_2021 * mean_from_Grüne

movements = c(voters_to_Grüne["SPD_a21"] - voters_from_Grüne["SPD_a25"],
              voters_to_Grüne["CDU_a21"] - voters_from_Grüne["CDU_a25"],
              voters_to_Grüne["AfD_a21"] - voters_from_Grüne["AfD_a25"],
              voters_to_Grüne["BSW_a21"] - voters_from_Grüne["BSW_a25"],
              voters_to_Grüne["FDP_a21"] - voters_from_Grüne["FDP_a25"],
              voters_to_Grüne["Linke_a21"] - voters_from_Grüne["Linke_a25"],
              voters_to_Grüne["Sonstige_a21"] - voters_from_Grüne["Sonstige_a25"],
              voters_to_Grüne["Nichtwähler_a21"] - voters_from_Grüne["Nichtwähler_a25"])
movements <- as.numeric(movements)

green_diff <- btw %>% 
  summarise(green_diff = sum(Grüne_a25 - Grüne_a21)) %>%
  pull()

# calculate the regression model
simulated <- sum(movements) 
correction_factor = green_diff / simulated
movements <- movements * correction_factor
movements <- round(movements,-2) 



# Create the data frame
movements_df <- data.frame(
  party = c("SPD", "CDU", "AfD", "BSW", "FDP", "Linke", "Sonstige", "Nichtwähler"),
  voter = as.numeric(movements),
  row.names = NULL
)

# 1. Sortiere die Daten
movements_df <- movements_df %>%
  arrange(voter)

# 2. Erstelle einen Index für die y-Position
movements_df$y_index <- 1:nrow(movements_df)
num_winner <- sum(movements_df$voter > 0)
movements_df$y_index <- ifelse(movements_df$voter >=0, movements_df$y_index - num_winner - 1, movements_df$y_index )

# Partei trennen
movements_df$wrapped_party <- ifelse(movements_df$party=="Nichtwähler", str_wrap("Nicht- wähler", width = 5), movements_df$party)

green_width <- 1000

# Diagramm erstellen
movements_df %>% 
  ggplot(aes(y = y_index)) + 
  # Grünes Rechteck in der Mitte (für die Grünen)
  geom_shape(data = data.frame(x=c(-green_width, green_width, green_width, -green_width),
                               y=c(0.59, 0.59, 4.41, 4.41)),
             aes(x = x, y = y),
             fill = "limegreen",
             radius = .01)+

  # Add party names inside rectangles
  annotate(geom = "text", x = 0,  
           y = num_winner/2+1,
           label = "Grüne",
           color = "black",
           size = 4) +               
  
  # Farbige Rechtecke für Gewinne (links)
  geom_shape(data = . %>% filter(voter > 0) %>%
               reframe(data.frame(
                    x = c(-green_width*5, -green_width*3, -green_width*3, -green_width*5),
                    y = c(y_index - 0.4, y_index - 0.4, y_index + 0.4,y_index + 0.4),
                    party=party
                  ),
                  .by = party
                ),
             aes(x = x, y = y, fill=party),
             radius = .01)+
  
  # Add party names inside rectangles
  geom_text(data = . %>% filter(voter > 0 & party %in% c("Nichtwähler", "FDP") ),
            aes(x = -green_width*4,  
                y = y_index,
                label = wrapped_party),
            color = "black",
            size = 4) +               
  geom_text(data = . %>% filter(voter > 0 & !party %in% c("Nichtwähler", "FDP") ),
            aes(x = -green_width*4,  
                y = y_index,
                label = wrapped_party),
            color = "white",
            size = 4) +               

  # Farbige Rechtecke für Verluste (rechts)
    geom_shape(data = . %>% filter(voter < 0) %>%
               reframe(data.frame(
                 x = c(max(green_width*2 - movements_df$voter), max(green_width*4 - movements_df$voter), max(green_width*4 - movements_df$voter), max(green_width*2 - movements_df$voter)),
                 y = c(y_index - 0.4, y_index - 0.4, y_index + 0.4,y_index + 0.4),
                 party=party
               ),
               .by = party
               ),
             aes(x = x, y = y, fill=party),
             radius = .01)+
  
    # Add party names inside rectangles
  geom_text(data = . %>% filter(voter < 0 & party %in% c("Nichtwähler", "FDP") ),
            aes(x = max(green_width*3 - movements_df$voter),  
                y = y_index,
                label = wrapped_party),
            color = "black",
            size = 4) +               
  geom_text(data = . %>% filter(voter < 0 & !party %in% c("Nichtwähler", "FDP") ),
            aes(x = max(green_width*3 - movements_df$voter),  
                y = y_index,
                label = wrapped_party),
            color = "white",
            size = 4) +               
  
  # Pfeile für Wählergewinne 
  geom_segment(data = . %>% filter(voter > 0),
               aes(x = -green_width*2.7, xend = -green_width*2.7+voter,
                   yend = y_index,
                   color = party),
               arrow = arrow(length = unit(0.3, "cm"), type = "open", angle=1),
               linewidth = 10) +
  
  # Pfeile für Wählerverluste
  geom_segment(data = . %>% filter(voter < 0),
               aes(x = green_width*1.2, xend = green_width *1.2 - voter,
                   yend = y_index,
                   color = party),
               arrow = arrow(length = unit(0.3, "cm"), type = "open", angle=1),
               linewidth = 10) + 
   scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "black", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="limegreen", "FDP"="#FFCC00","BSW"="#5D0085", "Sonstige"="darkgray",
                                "Nichtwähler" = "beige")) +
  scale_color_manual(values = c( "SPD" = "red2", "CDU" = "black", "Linke"="deeppink",AfD="#4169E1", 
                                 "Grüne"="limegreen", "FDP"="#FFCC00","BSW"="#5D0085", "Sonstige"="darkgray",
                                 "Nichtwähler" = "beige")) +
  geom_text(data = . %>% filter(voter < 0),
            aes(x = green_width*1.3,
                y = y_index - 0.3,
                label = scales::comma(-voter)),
            hjust = 0,
            size = 4) +
  geom_text(data = . %>% filter(voter > 0),
            aes(x = -green_width*2.7,
                y = y_index - 0.3,
                label = scales::comma(voter)),
            hjust = 0,
            size = 4) +
  theme_minimal() +
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  theme(
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),           # Remove x axis labels
    axis.ticks.x = element_blank(),          # Remove x axis ticks
    panel.grid.major.x = element_blank(),    # Remove x axis grid lines
    panel.grid.minor.x = element_blank(),    # Remove minor grid lines if any
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  scale_x_continuous(limits = c(min(-green_width*5-movements_df$voter), max(green_width*6 - movements_df$voter) ))

  
#' Die Grünen haben erheblich Stimmen an die Linke verloren. Die Ergebnisse ähneln insofern den bundesweiten Wählerwanderungen, 
#' die infratest dimap anhand von Nachwahlbefragungen ermittelt hat.
#' 
#' \newpage
#' ## Zusammenhang mit Wahlbeteiligung
#' 
#' In diesem Abschnitt werden die Zusammenhänge der Ergebnisse der Grünen mit der Wahlbeteiligung analysiert.
#' Die folgende Grafik setzt die Stimmanteile der Grünen und die Anteilsverluste der Grünen zur Wahlbeteiligung in Beziehung. 
#' 
#' Die folgenden Analysen verwenden den sogenannten Korrelationskoeffizienten nach Pearson.
#' Werte größer als 0,5 weisen auf einen starken Zusammenhang, Werte um 0,3 auf einen mittleren und Werte ab 0,1 auf einen
#' geringen Zusammenhang hin. Statistisch signifikante Zusammenhänge, die als gesichert angesehen werden dürfen,
#' werden mit Sternen (`*`, `**` oder `***`) gekennzeichnet.
#' 
#+ echo=FALSE, fig.width=7, fig.height=3


btw <- btw %>% 
  mutate(Wahlbeteiligung_25 = Gültig_a25 / Berechtigt_a25*100)

# Korrelation der prozentualen Werte 

c_Wahlbeteiligung <- cor(btw$Grüne_p25, btw$Wahlbeteiligung_25, method = "pearson", use = "complete.obs")
c_Wahlbeteiligung_p <- cor.test(btw$Grüne_p25, btw$Wahlbeteiligung_25, method = "pearson", use = "complete.obs")$p.value
c_Wahlbeteiligung_s <- ifelse(c_Wahlbeteiligung_p <= 0.001, "***", ifelse(c_Wahlbeteiligung_p <= 0.01, "**", ifelse(c_Wahlbeteiligung_p <= 0.05, "*", "")))

p1 <- ggplot(btw, aes(y = Grüne_p25, x = Wahlbeteiligung_25)) +
  geom_point(color = "darkgray", size = .7) + labs(x = "Wahlbeteiligung (%)", y = "Grüne Anteile (%) ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkgray", se = FALSE, linewidth = 0.6) + # Add regression line
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
  geom_point(color = "darkgray", size = .7) + labs(x = "Wahlbeteiligung (%)", y = "Grüne +/- (%) ") +
#  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Wahlbeteiligung_25)),  # Horizontal center
            y = min(btw$Grüne_pd) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Wahlbeteiligung,c_Wahlbeteiligung_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(0, 0, 10, 0)) 

# Anordnen der Plots in einem 2x1-Raster 
grid.arrange( p1, p2, ncol = 2)


# Die Grünen haben bessere Ergebnisse in Stimmbezirken mit hoher Wahlbeteiligung erzielt. Es wurde jedoch kein 
# Zusammenhang zwischen den Verlusten der Grünen und der Wahlbeteiligung festgestellt.  
# 
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
  geom_point(color = "darkgray", size = .7) + labs(x = "Ausländische Bevölkerung (%)", y = " ") +
#  geom_smooth(method = "lm", formula = y ~ x, color = "darkgray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Ausländische_Bevölkerung)),  # Horizontal center
            y = max(btw$Grüne_p25) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Ausländische_Bevölkerung,c_Ausländische_Bevölkerung_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(0, 0, 10, 0)) 

p_Alt_Jung_Quotient <- 
  ggplot(btw, aes(y = Grüne_p25, x = Alt_Jung_Quotient)) +
  geom_point(color = "darkgray", size = .7) + labs(x = "Alt-Jung-Quotient ", y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkgray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Alt_Jung_Quotient)),  # Horizontal center
            y = max(btw$Grüne_p25) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Alt_Jung_Quotient,c_Alt_Jung_Quotient_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 10, 0)) 

p_Arbeitslose <- 
  ggplot(btw, aes(y = Grüne_p25, x = Arbeitslose)) +
  geom_point(color = "darkgray", size = .7) + labs(x = "Arbeitslose (%)", y = "Grüne Stimmenateile (%)") +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkgray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Arbeitslose)),  # Horizontal center
            y = max(btw$Grüne_p25) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Arbeitslose,c_Arbeitslose_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 10, 0)) 

p_Haushalte_mit_Kindern <- 
  ggplot(btw, aes(y = Grüne_p25, x = Haushalte_mit_Kindern)) +
  geom_point(color = "darkgray", size = .7) + labs(x = "Haushalte mit Kindern (%)", y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkgray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Haushalte_mit_Kindern)),  # Horizontal center
            y = max(btw$Grüne_p25) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Haushalte_mit_Kindern,c_Haushalte_mit_Kindern_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 10, 0)) 

p_Gymnasialempfehlung <- 
  btw %>% filter(!is.na(Gymnasialempfehlung)) %>%
  ggplot(aes(y = Grüne_p25, x = Gymnasialempfehlung)) +
  geom_point(color = "darkgray", size = .7) + labs(x = "Gymnasialempfehlung (%)", y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkgray", se = FALSE, linewidth = 0.6) + # Add regression line
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
#' \newpage
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
  geom_point(color = "darkgray", size = .7) + labs(x = "Ausländische Bevölkerung (%)", y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkgray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Ausländische_Bevölkerung)),  # Horizontal center
            y = max(btw$Grüne_pd) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Ausländische_Bevölkerung,c_Ausländische_Bevölkerung_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(0, 0, 10, 0)) 

p_Alt_Jung_Quotient <- 
  ggplot(btw, aes(y = Grüne_pd, x = Alt_Jung_Quotient)) +
  geom_point(color = "darkgray", size = .7) + labs(x = "Alt-Jung-Quotient ", y = " ") +
#  geom_smooth(method = "lm", formula = y ~ x, color = "darkgray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Alt_Jung_Quotient)),  # Horizontal center
            y = max(btw$Grüne_pd) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Alt_Jung_Quotient,c_Alt_Jung_Quotient_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 10, 0)) 

p_Arbeitslose <- 
  ggplot(btw, aes(y = Grüne_pd, x = Arbeitslose)) +
  geom_point(color = "darkgray", size = .7) + labs(x = "Arbeitslose (%)", y = "Grüne Veränderung (%)") +
#  geom_smooth(method = "lm", formula = y ~ x, color = "darkgray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Arbeitslose)),  # Horizontal center
            y = max(btw$Grüne_pd) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Arbeitslose,c_Arbeitslose_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 10, 0)) 

p_Haushalte_mit_Kindern <- 
  ggplot(btw, aes(y = Grüne_pd, x = Haushalte_mit_Kindern)) +
  geom_point(color = "darkgray", size = .7) + labs(x = "Haushalte mit Kindern (%)", y = " ") +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkgray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text", 
            x = mean(range(btw$Haushalte_mit_Kindern)),  # Horizontal center
            y = max(btw$Grüne_pd) + 1,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Haushalte_mit_Kindern,c_Haushalte_mit_Kindern_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 10, 0)) 

p_Gymnasialempfehlung <- 
  btw %>% filter(!is.na(Gymnasialempfehlung)) %>%
  ggplot(aes(y = Grüne_pd, x = Gymnasialempfehlung)) +
  geom_point(color = "darkgray", size = .7) + labs(x = "Gymnasialempfehlung (%)", y = " ") +
#  geom_smooth(method = "lm", formula = y ~ x, color = "darkgray", se = FALSE, linewidth = 0.6) + # Add regression line
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
#+ echo=FALSE, fig.width=5, fig.height=2


totals <- btw %>%
  summarize(
    `Gebäude gesamt`= sum(Gebäude), 
    Briefkastenwahlkampf = sum(Briefkasten),
    Haustürwahlkampf = sum(Haustür)) %>%
  pivot_longer(cols = everything(), names_to = "Aktivität", values_to = "Total")

# Create the bar chart
totals %>%
  mutate(
    Aktivität = factor(Aktivität, levels = c( "Gebäude gesamt", "Briefkastenwahlkampf", "Haustürwahlkampf")),
    Percentage = Total / Total[Aktivität == "Gebäude gesamt"] * 100
  ) %>%
  ggplot(aes(x = Aktivität, y = Total)) +
  geom_bar(stat = "identity", fill = "#888888", show.legend = FALSE, width = .85, color = "black", linewidth=0.2) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE), 
    expand = expansion(mult = c(0, 0.1))  # Add 10% padding to the top
  ) +
  geom_text(
    aes(label = paste0(format(floor(Total), big.mark = ".", decimal.mark = ","), 
                       " (", round(Percentage, 0), "%)")),
    vjust = -0.5,
    size = 3
  ) +  # Add percentages for specific bars
  labs(x = "", y = "") +
  theme_minimal()




#'
#' Die folgenden Karten zeigen die prozentuale Abdeckung der Gebäude im Wahlkampf und die
#' relativen Stimmenverluste.
#' 
#+ echo=FALSE, fig.width=7, fig.height=6

btw <- btw %>% 
  mutate(Grüne_rd = ifelse(Grüne_a21 != 0, (Grüne_a25 - Grüne_a21) / Grüne_a21*100, 0),
         Grüne_rg = ifelse(Grüne_rd > 0, Grüne_rd, 0),
         Grüne_rv = ifelse(Grüne_rd < 0, Grüne_rd, 0),
         Haustür_p = Haustür / Gebäude*100,
         Briefkasten_p = Briefkasten / Gebäude*100,
         Wahlkampf_p = (Haustür+Briefkasten) / Gebäude*100)

sf_stimmbezirke_Brief <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, Briefkasten_p), by = "Stimmbezirk")
p_Brief <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_Brief, aes(fill = Briefkasten_p), linewidth = 0, color = "white") +
  scale_fill_gradient(low = "white", high = "darkgray", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .2, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA, fontface = "bold") +
  labs(subtitle = "Briefkastenwahlkampf", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      

sf_stimmbezirke_Haus <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, Haustür_p), by = "Stimmbezirk")
p_Haus <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_Haus, aes(fill = Haustür_p), linewidth = 0, color = "white") +
  scale_fill_gradient(low = "white", high = "darkgray", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .2, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA, fontface = "bold") +
  labs(subtitle = "Haustürwahlkampf", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      


sf_stimmbezirke_Wahlkampf <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, Wahlkampf_p), by = "Stimmbezirk")
p_Wahlkampf <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_Wahlkampf, aes(fill = Wahlkampf_p), linewidth = 0, color = "white") +
  scale_fill_gradient(low = "white", high = "darkgray", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .2, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA, fontface = "bold") +
  labs(subtitle = "Wahlkampf gesamt", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      

sf_stimmbezirke_Grüne <- sf_stimmbezirke %>% left_join(btw %>% select(Stimmbezirk, Grüne_rv), by = "Stimmbezirk")
p_Grüne <- 
  ggplot(data = sf_kommunalbezirke) +
  geom_sf(data = sf_stimmbezirke_Grüne, aes(fill = Grüne_rv), linewidth = 0, color = "white") +
  scale_fill_gradient(high = "white", low = "limegreen", labels = scales::percent_format(scale = 1)) +
  geom_sf(fill=NA, linewidth = .2, color = "black") +
  geom_label(aes(x=X,y=Y,label = str_wrap(Kommunalname, width = 9)),
             color = "black", size=1, fill = alpha("white", 0.15), label.size = NA, fontface = "bold") +
  labs(subtitle = "Relative Stimmenverluste", fill = NULL) +
  theme_void() +
  theme( plot.subtitle = element_text(hjust = .1, vjust = -2),
         legend.text = element_text(size = 7),
         legend.key.size = unit(0.3, "cm"),
         legend.position = c(1, 0),       
         legend.justification = c(1, 0))      

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
  geom_point(color = "darkgray", size = .7) + labs(x = "Briefkasten (%)", y = "Stimmenverluste %") +
#  geom_smooth(method = "lm", formula = y ~ x, color = "darkgray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text",
            x = mean(range(btw$Briefkasten_p, na.rm=T)),  # Horizontal center
            y = max(btw$Grüne_rv)+3,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Briefkasten,c_Briefkasten_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 0, 0))

p_Haus <-
  btw %>%
  ggplot(aes(y = Grüne_rv, x = Haustür_p)) +
  geom_point(color = "darkgray", size = .7) + labs(x = "Haustür (%)", y = "") +
#  geom_smooth(method = "lm", formula = y ~ x, color = "darkgray", se = FALSE, linewidth = 0.6) + # Add regression line
  annotate( "text",
            x = mean(range(btw$Haustür_p, na.rm=T)),  # Horizontal center
            y = max(btw$Grüne_rv)+3 ,           # Top of the plot
            label = sprintf("Korrelation: %.2f%s", c_Haustür,c_Haustür_s), size = 3, color = "black" ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 0, 0, 0))

p_Wahlkampf <-
  btw %>%
  ggplot(aes(y = Grüne_rv, x = Wahlkampf_p)) +
  geom_point(color = "darkgray", size = .7) + labs(x = "Wahlkampf gesamt (%)", y = "") +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkgray", se = FALSE, linewidth = 0.6) + # Add regression line
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

c_SPD <- cor(btw$Grüne_pd, btw$SPD_pd, method = "pearson", use = "complete.obs")
c_CDU <- cor(btw$Grüne_pd, btw$CDU_pd, method = "pearson", use = "complete.obs")
c_Linke <- cor(btw$Grüne_pd, btw$Linke_pd, method = "pearson", use = "complete.obs")
c_Volt <- cor(btw$Grüne_pd, btw$Volt_pd, method = "pearson", use = "complete.obs")
c_AfD <- cor(btw$Grüne_pd, btw$AfD_pd, method = "pearson", use = "complete.obs")
c_FDP <- cor(btw$Grüne_pd, btw$FDP_pd, method = "pearson", use = "complete.obs")


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
  geom_bar( stat = "identity", show.legend = FALSE, , alpha = .8, color = "black", linewidth=0.2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1), # Format y-axis as percentages
    expand = expansion(mult = c(0, 0.1)) # Add 10% padding to the top
  ) +
  geom_text(aes(label = percent(Total, accuracy = 0.1)), vjust = -0.5, size = 3) + # Add numbers on top of bars
  labs(x = "",y = "Zweistimmen (Simulaton) %") +
  theme_minimal() +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "#404040", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="limegreen", "FDP"="#FFCC00","Volt"="purple", "Sonstige"="darkgray"))
  


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
  geom_bar( stat = "identity", show.legend = FALSE, alpha = .8, color = "black", linewidth=0.2) +
  scale_y_continuous(
    labels = percent_format(accuracy = .1), # Format y-axis as percentages
    expand = expansion(mult = c(0, 0.5)) # Add 5% padding to the top
  ) +
  geom_text(aes(label = percent(Total, accuracy = 0.1)), vjust = -0.5, size = 3) + # Add numbers on top of bars
  labs(x = "",y = "Simulation +/-") +
  theme_minimal() +
  scale_fill_manual(values = c( "SPD" = "red2", "CDU" = "#404040", "Linke"="deeppink",AfD="#4169E1", 
                                "Grüne"="limegreen", "FDP"="#FFCC00","Volt"="purple", "Sonstige"="darkgray"))

# Anordnen der Plots in einem 1x2-Raster 
grid.arrange(p_absolute, p_diff, ncol = 1, heights = c(3, 1))


#' Nach dem Simulationsmodell hätten die Grünen ohne den Briefkasten- und Haustürwahlkampf 0,5% weniger Stimmanteile gehabt.





