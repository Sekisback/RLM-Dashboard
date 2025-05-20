# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
#
# RLM-Vital-Signs Dashboard                                                 ----
#
# Author : Sascha Kornberger
# Datum  : 19.05.2025
# Version: 0.2.0
#
# History:
# 0.2.0   - alte Quelldateien werden gelöscht
# 0.1.0   
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
  "Gemeindewerke Halstenbek MSB",
  "Stadtwerke Clausthal-Zellerfeld GmbH MSB",
  "Stadtwerke Quickborn MSB",
  "Stadtwerke Südholstein GmbH MSB"
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
  
  # OBIS-Farben
  "1-0:1.29.0" = "mediumorchid",   # violett
  "1-0:2.29.0" = "deepskyblue4",  # sehr dunkles blaugrün
  "1-1:1.29.0" = "darkseagreen",  # pastellgrünlich
  "1-1:2.29.0" = "sienna3"        # warmes braun-rot
)


### Datum gestern ----
gestern <- (Sys.Date() - 1)

# BENÖTIGTE PAKETE ----
# Liste der Pakete
pakete <- c("shiny", "bs4Dash", "readxl", "DT", "tidyverse", "lubridate", 
            "scales", "base64enc")

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

# EEG-Logo laden und in Base64 konvertieren
logo_base64 <<- dataURI(file = "www/images/eeg-logo.png", mime = "image/png")


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
daten_aufbereiten <- function(df_raw, tage_max){

  # Spaltennamen aus den Datenframe extrahieren
  alle_spalten <- names(df_raw)
  # Finde alle Spaltennamen, die wie ein Datum aussehen: "TT.MM.JJJJ"
  datumsspalten <- alle_spalten[which(str_detect(alle_spalten, "^\\d{2}\\.\\d{2}\\.\\d{4}$"))]
  # Spaltennamen in ein Datum konvertieren
  datum_vektor <- dmy(datumsspalten)

  # Excel-Serial-Datum umwandeln in Datum, mit Schaltjahr offset
  df <- df_raw |> 
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
      MSB %in% eigene_zfa ~ "PAULA",
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


# Slider Datum
slider_datum <- function(tage_max) {
  datum <- as.Date(paste0(year(gestern), "-", month(gestern), "-", tage_max))
  format(datum, "%d.%m.%Y")
}


# CHARTS ERSTELLEN ----
pie_chart <- function(df, zfa, tage_max) {
  
  # Hole die Spalte basierend auf dem Slider Datum
  spalten_name <- slider_datum(tage_max)

  # Erstelle die Überschrift anhand der ZFA
  chart_title <- case_when(
    zfa == "PAULA"                ~ "Interne ZFA: PAULA",
    zfa == "extern_kunde"         ~ "Externe ZFA: Kunde",
    zfa == "extern_dienstleister" ~ "Externe ZFA: Dienstleister",
    zfa == "meterpan"             ~ "MSB MeterPan (iMS)",
    TRUE                          ~ paste("Unbekannte ZFA:", zfa)
  ) 
  
  # Filtere nach benötigtem Renderplot (Server)
  df_gefiltert <- df |>
    filter(ZFA == zfa)
  
  # Erstelle ein PieChart-Diagramm basierend auf dem DataFrame `df_gefiltert`
  ggplot(df_gefiltert, aes(x = "", fill = !!sym(spalten_name))) +
    geom_bar(stat = "count", color = "white") +
    coord_polar("y", start = 0, direction = -1) +
    scale_fill_manual(values = farben, na.translate = FALSE) +
    labs(
      title = chart_title,
      fill = NULL
    ) +
    theme_void() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 22,
        face = "bold",
        color = "grey44"),
      legend.position = "bottom",
      legend.justification = "right",
    )
  
}


# DONUT-CHART ERSTELLEN
donut_chart <- function(df, filter, tage_max) {
  
  # Hole die Spalte basierend auf dem Slider Datum
  spalten_name <- slider_datum(tage_max)
  
  # Erstelle die Überschrift anhand des Filters
  chart_title <- case_when(
    filter == "kME" ~ "Datenstand kME",
    filter == "iMS" ~ "Datenstand iMS",
    filter == "ein" ~ "Datenstand Einspeiser",
    filter == "aus" ~ "Datenstand Ausspeiser",
    TRUE ~ paste("Unbekannter FILTER:", filter)
  ) 
  
  df_gefiltert <- df |>
    filter(
      case_when(
        filter == "aus" ~ str_detect(DATEN, "WIRK_P"),
        filter == "ein" ~ str_detect(DATEN, "WIRK_M"),
        filter == "kME" ~ MESSSYSTEM == filter,
        filter == "iMS" ~ MESSSYSTEM == filter,
        TRUE ~ FALSE
      )
    )
  
  ggplot(df_gefiltert, aes(x = 2, fill = !!sym(spalten_name))) +
    geom_bar(stat = "count", color = "white") +
    coord_polar("y", start = 0, direction = -1) +
    xlim(c(1, 2.5)) + 
    scale_fill_manual(values = farben, na.translate = FALSE) +
    labs(
      title = chart_title,
      fill = NULL
    ) +
    theme_void() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 22,
        face = "bold",
        color = "grey44"),
      legend.position = "bottom",
      legend.justification = "right",
    )
}


# BAR-CHART OBIS ERSTELLEN
bar_chart_obis <- function(df){
  
  df_filtered <- df |> 
    filter(str_starts(DATEN, "GMSB")) |> 
    count(MSB, OBIS, name = "ANZAHL") |> 
    arrange(MSB) |> 
    filter(ANZAHL > 1)
  
  ggplot(df_filtered, aes(x = ANZAHL, y = MSB, fill = OBIS)) +
    geom_col(width = 0.8) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.01)))+
    scale_fill_manual(values = farben, na.translate = FALSE) +
    labs(
      x = element_blank(),
      y = element_blank(),
      title = "ANZAHL RLM MARKTLOKATIONEN JE MSB UND OBIS",
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(
        size = 22,
        face = "bold",
        color = "grey44"),
      legend.position = "bottom",
      legend.justification = "right",
      panel.grid.major.y = element_blank(),
    ) 
}


# BAR-CHART DATEN ERSTELLEN
bar_chart_daten <- function(df, tage_max){
  
  spalten_name <- slider_datum(tage_max)
  
  df_filtered <- df |> 
    filter(str_starts(DATEN, "GMSB")) |> 
    count(MSB, !!sym(spalten_name), name = "ANZAHL") |> 
    group_by(MSB) |>
    mutate(Prozent = ANZAHL / sum(ANZAHL) * 100) |> 
    filter(ANZAHL > 1)
  

  ggplot(df_filtered, aes(x = Prozent, y = MSB, fill = !!sym(spalten_name))) +
    geom_col(width = 0.8) +
    scale_fill_manual(values = farben, na.translate = FALSE) +
    scale_x_continuous(
      labels = percent_format(scale = 1),
      expand = expansion(mult = c(0, 0.01))) +
    theme_minimal(base_size = 14) +
    labs(
      x = element_blank(),
      y = element_blank(),
      title = "DATENSTAND JE MSB",
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(
        size = 22, 
        face = "bold",
        color = "grey44"),
      legend.position = "bottom",
      legend.justification = "right",
      panel.grid.major.y = element_blank()
    ) 
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
#                                  SHINY UI                                 ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
ui <- bs4DashPage(
  

  # Fullscreen-Mode
  fullscreen = FALSE,
  # Darkmode entfernen
  dark = NULL,
  # Helper entfernen
  help = NULL,

  ## Navbar ----
  header = bs4DashNavbar(
    # Fixiert die Navbar am oberen Rand des Fensters
    fixed = TRUE,
    
    title = dashboardBrand(
      title = "EEG-Energie",
      image = logo_base64
      ),

    ### Linker Bereich der Navbar ----
    leftUi = tagList(
      tags$li(
        class = "dropdown",
        tags$span(
          class = "navbar-text",
          style = "padding-left: 10px;",
          "RLM VITAL SIGNS"
        )
      )
    ),
    
    ### Rechter Bereich der Navbar ----
    rightUi = tagList(
      tags$li(
        # Definiert den Listeneintrag als Dropdown in der Navbar
        class = "dropdown",
        # Setzt Innenabstand: oben 
        style = "margin-top: -4px; padding-right: 10px",
        
        # Textfeld für Monat/Jahr-Anzeige
        tags$span(
          # ID für Monat/Jahr-Anzeige
          id = "navbar_date",
          # Styling für Navbar-Element
          class = "nav-link navbar-text"
        )
      ),
      
      tags$li(
        class = "dropdown",
        div(
          style = "width: 200px; padding-top: 8px; padding-right: 25px;",
          sliderInput(
            inputId = "tage_slider",
            label = NULL,
            min = 1,   # wird im Server überschrieben
            max = 31,  # wird im Server überschrieben
            value = 1, 
            step = 1,
            ticks = FALSE,
            width = "100%"
          )
        )
      )
    )
  ),

 
  ## Sidebar links ---- 
  sidebar = bs4DashSidebar(
    
    collapsed = TRUE,  # Sidebar startet eingeklappt
    status = "primary",
    elevation = 3,
    
    ### Sidebar-Menü ----
    bs4SidebarMenu(
      id = "seite",  # input$seite reagiert auf Auswahl
      bs4SidebarMenuItem("Übersicht", tabName = "seite_uebersicht", icon = icon("chart-pie")),
      bs4SidebarMenuItem("Tabelle", tabName = "seite_tabelle", icon = icon("table"))
      # bs4SidebarMenuItem("Einstellungen", tabName = "seite_einstellungen", icon = icon("cog"))
    ),
  ),

  ## Body ----
  body = bs4DashBody(

    # Lädt CSS-Datei für eigenes Styling
    includeCSS(file.path(getwd(), "www/styles.css")),
    # Lädt benutzerdefinierten JavaScript-Code
    includeScript(file.path(getwd(), "www/custom.js")),

    bs4TabItems(
      bs4TabItem(
        tabName = "seite_uebersicht", 
        ### Erste Reihe ----
        fluidRow(
          column(3, div(class = "plot-shadow", plotOutput("plot_paula", height = "350px"))),
          column(3, div(class = "plot-shadow", plotOutput("plot_kunde", height = "350px"))),
          column(3, div(class = "plot-shadow", plotOutput("plot_dienstleister", height = "350px"))),
          column(3, div(class = "plot-shadow", plotOutput("plot_meterpan", height = "350px")))
        ),
        ### Zweite Reihe ----
        fluidRow(
          column(6, div(class = "plot-shadow", plotOutput("plot_gmsb_obis", height = "410px"))),
          column(6, div(class = "plot-shadow", plotOutput("plot_gmsb_daten",height = "410px")))
        ),
        ### Dritte Reihe ----
        fluidRow(
          column(3, div(class = "plot-shadow", plotOutput("plot_kme", height = "350px"))),
          column(3, div(class = "plot-shadow", plotOutput("plot_ims", height = "350px"))),
          column(3, div(class = "plot-shadow", plotOutput("plot_ein", height = "350px"))),
          column(3, div(class = "plot-shadow", plotOutput("plot_aus", height = "350px")))
        ),
      ),
      
      ### Tabelle ----
      # TabItem mit NavTabs für die Tabellenseite erstellen
      bs4TabItem(
        tabName = "seite_tabelle", 
        fluidRow(
          box(
            title = "",
            width = 12,
            # NavTabs hinzufügen
            tabsetPanel(
              id = "tabellen_tabs",
              type = "pills",
              # Erster Tab: Haupttabelle
              tabPanel(
                "PAULA ZFA", 
                DTOutput("tabelle"),
                div(
                  style = "margin-top: 15px; text-align: right",
                  downloadButton(
                    "download_csv", 
                    "CSV herunterladen",
                    class = "btn btn-primary",
                    icon = icon("download")
                  )
                )
              ),
              # Zweiter Tab: Hier kannst du weitere Inhalte einfügen
              tabPanel(
                "Platzhalter", 
                p("Hier können weitere Informationen angezeigt werden.")
              ),
              # Dritter Tab: Beispiel für einen zusätzlichen Tab
              tabPanel(
                "Platzhalter", 
                p("Statistische Auswertungen der Daten.")
              )
            ),
            collapsible = FALSE
          )
        )
      ),

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
  
  # Aktuelle Datei, die verwendet wird
  aktuelle_datei <- reactiveVal("")
  
  # Letzte bekannte Datei für Notification-Zwecke
  letzte_datei <- reactiveVal("")
  
  # Timer für die Dateiüberwachung
  observe({
    invalidateLater(3000, session)
    
    # Finde alle passenden Dateien im Ordner
    dateien <- list.files(
      path = "data",
      pattern = ".*RLM-RAW-Daten.*\\.xlsx$",
      full.names = TRUE
    )
    
    # Wenn keine Dateien vorhanden sind, nichts tun
    if(length(dateien) == 0) return()
    
    # Ermittle neueste Datei
    neueste_datei <- dateien[which.max(file.info(dateien)$mtime)]
    
    # Prüfe, ob es eine neue Datei ist und setze dann erst aktuelle_datei
    if(neueste_datei != isolate(aktuelle_datei())) {
      aktuelle_datei(neueste_datei)
      letzte_datei(neueste_datei)
      
      # Ältere Dateien löschen
      alte_dateien <- setdiff(dateien, neueste_datei)
      file.remove(alte_dateien)
    }
  })
  
  # Lade die Daten aus der neuesten Datei - wird nur neu berechnet,
  # wenn sich aktuelle_datei oder tage_slider ändert
  df_reaktiv <- reactive({
    # Hole neueste Datei aus dem reaktiven Wert
    neueste_datei <- aktuelle_datei()
    
    # Stelle sicher, dass wir eine Datei haben
    req(neueste_datei)
    req(input$tage_slider)
    
    # Lade Daten und bereite sie auf
    df_raw <- lade_daten(neueste_datei)
    daten_aufbereiten(df_raw, tage_max = input$tage_slider)
  })
  
  # Slider-Datum aktualisieren
  observe({
    updateSliderInput(
      session,
      inputId = "tage_slider",
      max = day(gestern),
      value = day(gestern)
    )
  })
  
  # Anzeige Navbar Datum Datei
  observe({
    # Verwende den reaktiven Wert für die letzte Datei
    neueste_datei <- letzte_datei()
    req(neueste_datei)
    
    # Änderungsdatum der zuletzt geladenen Datei
    datei_info <- file.info(neueste_datei)
    mtime <- datei_info$mtime
    
    # Formatiere Datum + Uhrzeit
    mtime_text <- format(mtime, "%d.%m.%Y  %H:%M Uhr")
    
    # Schicke Text in die Navbar
    session$sendCustomMessage("updateNavbarDate", paste("Stand:", mtime_text))
  })
  

  ## AUFRUF CHARTS ----
  ### PIE-CHARTS ----
  output$plot_paula <- renderPlot({
    pie_chart(df_reaktiv(), "PAULA", input$tage_slider)
  })
  
  output$plot_kunde <- renderPlot({
    pie_chart(df_reaktiv(), "extern_kunde", input$tage_slider)
  })
  
  output$plot_dienstleister <- renderPlot({
    pie_chart(df_reaktiv(), "extern_dienstleister", input$tage_slider)
  })
  
  output$plot_meterpan <- renderPlot({
    pie_chart(df_reaktiv(), "meterpan", input$tage_slider)
  })
  
  ### BAR-CHARTS ----
  output$plot_gmsb_obis <- renderPlot({
    bar_chart_obis(df_reaktiv())
  })
  
  output$plot_gmsb_daten <- renderPlot({
    bar_chart_daten(df_reaktiv(), input$tage_slider)
  })
  
  ### DONUT-CHART ----
  output$plot_kme <- renderPlot({
    donut_chart(df_reaktiv(), "kME", input$tage_slider)
  })

  output$plot_ims <- renderPlot({
    donut_chart(df_reaktiv(), "iMS", input$tage_slider)
  })
  
  output$plot_ein <- renderPlot({
    donut_chart(df_reaktiv(), "ein", input$tage_slider)
  })
  
  output$plot_aus <- renderPlot({
    donut_chart(df_reaktiv(), "aus", input$tage_slider)
  })
  
  ## DATENTABELLE ----
  # Reaktive, gefilterte Tabelle
  tabelle_reaktiv <- reactive({
    df <- df_reaktiv()
    
    # Erstelle den Spaltennamen für das Datum
    tag <- sprintf("%02d", input$tage_slider)
    monat <- format(gestern, "%m")
    jahr <- format(gestern, "%Y")
    datumsspalte <- paste0(tag, ".", monat, ".", jahr)
    
    spalten_basis <- c("VNB", "KUNDE", "MSB", "VERTRAG", "ZAEHLPUNKT")
    spalten_anzeigen <- c(datumsspalte, spalten_basis)
    
    df |>
      filter(
        !!sym(datumsspalte) %in% c("V", "E", "F", "G", "N"),
        ZFA == "PAULA"
      ) |>
      select(all_of(spalten_anzeigen))
  })
  
  # Reaktive Tabelle anzeigen
  output$tabelle <- renderDT({
    tabelle_reaktiv()
  },
  rownames = FALSE,
  options = list(
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    scrollY = "calc(100vh - 300px)",
    scrollCollapse = TRUE,
    # Spaltenspezifische Formatierung hinzufügen
    columnDefs = list(
      # Index 0 bezieht sich auf die erste Spalte (Datum)
      list(
        className = 'dt-center', 
        targets = 0
      )
    )
  ))
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("ZFA_PAULA_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv2(tabelle_reaktiv(), file, row.names = FALSE, fileEncoding = "WINDOWS-1252")
    }
  )
  
  
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
