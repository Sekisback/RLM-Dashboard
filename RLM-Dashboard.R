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


## Benötigte Pakete ----
# Liste der Pakete
pakete <- c("shiny", "bs4Dash", "readxl", "DT", "tidyverse", "renv")

# Installiere fehlende Pakete ohne Rückfragen
installiere_fehlende <- pakete[!pakete %in% installed.packages()[, "Package"]]
if (length(installiere_fehlende) > 0) {
  install.ypackages(
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

ergebnis <- lade_daten("data/TPL_RLM-RAW-Daten_20250416_020844.xlsx")

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
  
  # Sidebar (linker Bereich) – wird hier deaktiviert
  sidebar = bs4DashSidebar(disable = TRUE),

  ## Body ----
  body = bs4DashBody(
    
    # Lädt CSS-Datei für eigenes Styling
    includeCSS(file.path(getwd(), "www/styles.css")),
    # Lädt benutzerdefinierten JavaScript-Code
    includeScript(file.path(getwd(), "www/custom.js")),
    
    tabsetPanel(
      id = "tabcard",
      tabPanel(
        title = "Tab 1",
        "Content 1",
      ),
      tabPanel(
        title = "Tab 2", 
        "Content 2"
      ),
      tabPanel(
        title = "Tab 3", 
        "Content 3"
      )
    ),
  )
)


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
#                                SHINY SERVER                               ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
server <- function(input, output, session) {
  
  
  
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
