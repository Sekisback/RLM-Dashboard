# Testumgebung für ggplots

library(ggplot2)
library(tidyverse)
library(readxl)

rm(list=ls())

pfad <- "data/TLP_RLM-RAW-Daten_20250417_053227.xlsx"

# GLOBALE VARIABLEN ----
## ZFA Quellen ----
### Eigene ZFA PAULA ----
eigene_zfa <- c(
  "Gemeindewerke Halstenbek VNB",
  "Stadtwerke Clausthal-Zellerfeld GmbH VNB",
  "Stadtwerke Quickborn VNB",
  "Stadtwerke Südholstein GmbH VNB"
)

### Externe ZFA Kunde ----
ext_zfa_kunde <- c(
  "Schleswiger Stadtwerke GmbH VNB",
  "Stadtwerke Norderstedt VNB",
  "Stadtwerke Wedel VNB",
  "Stadtwerke Winsen (Luhe) VNB",
  "VersorgungsBetriebe Elbe GmbH VNB"
)

### Externe ZFA Dienstleister ----
ext_zfa_dienstleister <- c(
  "Stadtwerke Kaltenkirchen GmbH VNB",
  "Stadtwerke Nortorf AöR VNB"
)

### Chart Farben ----
farben <- c(
  # Statusfarben
  "W" = "green3",
  "V" = "gold",
  "E" = "dodgerblue",
  "F" = "red2",
  "G" = "darkorange",
  "N" = "gray60",
  
  # # OBIS-Farben
  # "1-0:1.29.0" = "royalblue",     # Strombezug iMS
  # "1-0:2.29.0" = "springgreen3",  # Einspeisung iMS
  # "1-1:1.29.0" = "darkorange2",   # Strombezug kmE
  # "1-1:2.29.0" = "deeppink3"      # Einspeisung kmE (optional)
  
  # OBIS-Farben (deutlich anders!)
  "1-0:1.29.0" = "mediumorchid",   # violett
  "1-0:2.29.0" = "deepskyblue4",  # sehr dunkles blaugrün
  "1-1:1.29.0" = "darkseagreen",  # pastellgrünlich
  "1-1:2.29.0" = "sienna3"        # warmes braun-rot
)


## DATEN LADEN ----
lade_daten <- function(pfad){
  # Liste aller Sheet in der Datei
  sheets <- excel_sheets(pfad)
  
  # Aus jedem Sheet den Inhalt laden  
  # und den Namen des sheets als erste Spalte einfügen
  daten <- lapply(sheets, function(sheet){
    
    # Aktuelles Sheet einlesen
    df <- read_excel(pfad, sheet = sheet)
    # DATEN (SheetName) als erste Spalte hinzufügen
    df <- bind_cols(tibble(DATEN = sheet), df)
    # Zurückgeben
    df
  })
  # Alle DataFrames zu einem zusammenfügen
  bind_rows(daten)
}

daten_aufbereiten <- function(df){

  # Excel-Serial-Datum umwandeln in Datum, mit Schaltjahr offset
  df <- df |> 
    mutate(
      VERTRAG_AB = case_when(
        !is.na(VERTRAG_AB) ~ as.Date(VERTRAG_AB -2, origin = "1900-01-01"),
        TRUE ~ as.Date(NA)
      ),
      VERTRAG_BIS = case_when(
        !is.na(VERTRAG_BIS) ~ as.Date(VERTRAG_BIS -2, origin = "1900-01-01"),
        TRUE ~ as.Date(NA)
      ),
      ABR_RELEVANT_VON = case_when(
        !is.na(ABR_RELEVANT_VON) ~ as.Date(ABR_RELEVANT_VON -2, origin = "1900-01-01"),
        TRUE ~ as.Date(NA)
      ),
      ABR_RELEVANT_BIS = case_when(
        !is.na(ABR_RELEVANT_BIS) ~ as.Date(ABR_RELEVANT_BIS -2, origin = "1900-01-01"),
        TRUE ~ as.Date(NA)
      )
    )
  
  # ZFA Quelle zuordnen
  df <- df |> 
    mutate(ZFA = case_when(
      OBIS %in% c("1-0:1.29.0", "1-0:2.29.0") ~ "meterpan",
      VNB %in% eigene_zfa ~ "PAULA",
      VNB %in% ext_zfa_kunde ~ "extern_kunde",
      VNB %in% ext_zfa_dienstleister ~ "extern_dienstleister",
      TRUE ~ NA_character_
    ))
  
  # Messsytem zuordnen anhand der OBIS
  df <- df |> 
    mutate(MESSSYSTEM = case_when(
      OBIS %in% c("1-0:1.29.0", "1-0:2.29.0") ~ "iMS",
      OBIS %in% c("1-1:1.29.0", "1-1:2.29.0") ~ "kME"
    ))
  
  return(df)
}

daten <- lade_daten(pfad)
df <- daten_aufbereiten(daten)
