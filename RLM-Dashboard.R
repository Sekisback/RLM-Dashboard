# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
#
# RLM-Vital-Signs Dashboard                                                 ----
#
# Author : Sascha Kornberger
# Datum  : 16.04.2025
# Version: 1.0.0
#
# History:
# 1.0.0  Funktion: 
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

# GLOBALE EINSTELLUNGEN ----
## Options ----
# Löscht alle Objekte aus dem aktuellen Arbeitsspeicher (Workspace)
rm(list = ls())
# Paketinstallation ohne Rückfrage zu Quellcode-Erzwingung
options(install.packages.check.source = "no")


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

# Datum gestern 
# TODO Entwicklung
#gestern <- (Sys.Date() - 1)
gestern <- (Sys.Date() - 2)

# BENÖTIGTE PAKETE ----
# Liste der Pakete
pakete <- c("shiny", "bs4Dash", "readxl", "DT", "tidyverse", "lubridate", "renv")

# Installiere fehlende Pakete ohne Rückfragen
installiere_fehlende <- pakete[!pakete %in% installed.packages()[, "Package"]]
if (length(installiere_fehlende) > 0) {
  install.packages(
    installiere_fehlende,
    repos = "https://cran.r-project.org",
    quiet = TRUE
  )
}

# Lade alle Pakete
invisible(lapply(pakete, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))


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


# DATEN AUFBEREITEN ----
# Zusätzliche Spalten hinzufügen um das filtern zu vereinfachen
daten_aufbereiten <- function(df_raw){

  # Spaltennamen aus den Datenframe extrahieren
  alle_spalten <- names(df_raw)
  # Finde alle Spaltennamen, die wie ein Datum aussehen: "TT.MM.JJJJ"
  datumsspalten <- alle_spalten[which(str_detect(alle_spalten, "^\\d{2}\\.\\d{2}\\.\\d{4}$"))]
  # Spaltennamen in ein Datum konvertieren
  datum_vektor <- dmy(datumsspalten)
  
  # Index der Spalte, die dem gestrigen Datum entspricht
  end_index <- which(datum_vektor == gestern)
  # Hole die Spaltennamen im Bereich 01.MM bis gestern
  spalten_bereich <- datumsspalten[1:end_index]
  
  # Zähle W, E, V, N, G, F pro Zeile im Bereich 01.MM bis gestern
  df <- df_raw |>
    mutate(
      W = rowSums(across(all_of(spalten_bereich), ~ as.integer(.x == "W")), na.rm = TRUE),
      E = rowSums(across(all_of(spalten_bereich), ~ as.integer(.x == "E")), na.rm = TRUE),
      V = rowSums(across(all_of(spalten_bereich), ~ as.integer(.x == "V")), na.rm = TRUE),
      G = rowSums(across(all_of(spalten_bereich), ~ as.integer(.x == "G")), na.rm = TRUE),
      N = rowSums(across(all_of(spalten_bereich), ~ as.integer(.x == "N")), na.rm = TRUE),
      F = rowSums(across(all_of(spalten_bereich), ~ as.integer(.x == "F")), na.rm = TRUE),
    )
  
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


# Pie-Chart erstellen ----
pie_chart <- function(df, zfa) {
  
  # Formatiere das Datum in Text "TT.MM.JJJJ"
  spalten_name <- format(gestern, "%d.%m.%Y")
  
  # Filtere nach PAULA und hole nur die Zielspalte
  df_gefiltert <- df |>
    filter(ZFA == zfa)
  
  zfa_werte <- as.data.frame(table(df_gefiltert[[spalten_name]]))
  colnames(zfa_werte) <- c("Wert", "Anzahl")
  
  # Definiere die Farben für bestimmte Werte
  farben <- c(
    "W" = "green3",
    "V" = "gold",
    "E" = "dodgerblue",
    "F" = "red2",
    "G" = "darkorange",
    "N" = "gray60"  # Optional
  )
  
  # Erstelle ein PieChart-Diagramm basierend auf dem DataFrame `zfa_werte`
  pie_chart <- ggplot(
    data = zfa_werte,
    # y: Anzahl *negativ* -> dadurch gegen den Uhrzeigersinn wie in Excel
    # fill: Farbkodierung nach Wert ("W", "E", "V" usw.)
    mapping =  aes(x = "", y = -Anzahl, fill = Wert)
  ) +
    
    # Erzeuge einen Balken pro Kategorie (W, E, V, ...)
    # stat = "identity": Y-Werte werden direkt verwendet
    # width = 1: volle Kreisbreite ohne Lücken
    # color:  Segmente optisch abgrenzen
    geom_bar(stat = "identity" , width = 1, color = "white" ) +
    
    # Transformiere Balken in ein Kreisdiagramm
    # start = 0 -> Start bei 12 Uhr (oben)
    coord_polar("y", start = 0) +
    
    # Manuelle Farbzuordnung für jede Kategorie
    # `na.translate = FALSE` unterdrückt Legenden-Eintrag für NA
    scale_fill_manual(values = farben, na.translate = FALSE) +
    
    # Beschriftung des Diagramms:
    # title: Oben über dem Kreis
    # fill: Titel der Legende
    labs(
      title = paste("Verteilung für ZFA:", zfa, "|", spalten_name),
      fill = spalten_name
    ) +
    
    # Entfernt Hintergrund, Achsen, Linien – klassischer Piechart-Stil
    theme_void()
    
  # Rückgabe
  return(pie_chart)
}


df_raw <- lade_daten("data/Dashboard_RLM-RAW-Daten_20250417_053227.xlsx")
df <- daten_aufbereiten(df_raw)


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
#                                  SHINY UI                                 ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
ui <- bs4DashPage(

  # Fullscreen-Mode
  fullscreen = TRUE,
  # Darkmode entfernen
  dark = NULL,
  # Helper entfernen
  help = NULL,

  ## Header ----
  header = bs4DashNavbar(
    # Fixiert die Navbar am oberen Rand des Fensters
    fixed = TRUE,

    ### Linker Bereich der Navbar (Buttons und Werte) ----
    leftUi = tagList(
      tags$li(
        class = "dropdown",
        tags$span(
          class = "navbar-text",
          style = "padding-left: 10px;",
          "RLM VITAL SIGNS"
        )
      )
    )
  ),

  # Sidebar (linker Bereich)
  sidebar = bs4DashSidebar(
    collapsed = TRUE,  # Sidebar startet eingeklappt
    status = "primary",
    elevation = 3,
    
    # Sidebar-Menü
    bs4SidebarMenu(
      id = "seite",  # input$seite reagiert auf Auswahl
      bs4SidebarMenuItem("Übersicht", tabName = "seite_uebersicht", icon = icon("chart-pie")),
      bs4SidebarMenuItem("Tabelle", tabName = "seite_tabelle", icon = icon("table")),
      bs4SidebarMenuItem("Einstellungen", tabName = "seite_einstellungen", icon = icon("cog"))
    )
  ),

  ## Body ----
  body = bs4DashBody(

    # Lädt CSS-Datei für eigenes Styling
    includeCSS(file.path(getwd(), "www/styles.css")),
    # Lädt benutzerdefinierten JavaScript-Code
    includeScript(file.path(getwd(), "www/custom.js")),

    bs4TabItems(
      bs4TabItem(tabName = "seite_uebersicht", fluidRow(
        column(3, div(class = "plot-shadow", plotOutput("plot_paula"))),
        column(3, div(class = "plot-shadow", plotOutput("plot_kunde"))),
        column(3, div(class = "plot-shadow", plotOutput("plot_dienstleister"))),
        column(3, div(class = "plot-shadow", plotOutput("plot_meterpan")))
      )),
      
      bs4TabItem(tabName = "seite_tabelle", fluidRow(
        box(title = "Datentabelle", width = 12, DTOutput("tabelle"))
      )),
      
      bs4TabItem(tabName = "seite_einstellungen", fluidRow(
        box(title = "Optionen", width = 12, "Platzhalter für Einstellungen")
      ))
    ),
  )
)


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
#                                SHINY SERVER                               ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
server <- function(input, output, session) {
  
  # Reaktive Daten – später austauschbar durch Dateiüberwachung
  df_reaktiv <- reactive({
    df_raw <- lade_daten("data/Dashboard_RLM-RAW-Daten_20250417_053227.xlsx")
    df <- daten_aufbereiten(df_raw)
    df
  })
  
  # Einzelne Piecharts für jede ZFA
  output$plot_paula <- renderPlot({
    pie_chart(df_reaktiv(), "PAULA")
  })
  
  output$plot_kunde <- renderPlot({
    pie_chart(df_reaktiv(), "extern_kunde")
  })
  
  output$plot_dienstleister <- renderPlot({
    pie_chart(df_reaktiv(), "extern_dienstleister")
  })
  
  output$plot_meterpan <- renderPlot({
    pie_chart(df_reaktiv(), "meterpan")
  })
  


  # Wenn die Sitzung endet, beende die App
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Starte die App im Browser; Shiny wählt automatisch einen freien Port
runApp(
  list(ui = ui, server = server),
  launch.browser = TRUE
)
