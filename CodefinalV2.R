# Chargement des bibliothèques nécessaires
library(here)
library(tidyverse)
library(janitor)
library(shiny)
library(sf)

# Fonction pour configurer les chemins
setup_paths <- function() {
  data_dir <- "/Users/nadine/Desktop/Ecole/R/DataIDF"
  list(
    data_2018_s1 = file.path(data_dir, "2018_S1_NB_FER.txt"),
    data_2018_s2 = file.path(data_dir, "2018_S2_NB_FER.txt"),
    data_2019_s1 = file.path(data_dir, "2019_S1_NB_FER.txt"),
    data_2019_s2 = file.path(data_dir, "2019_S2_NB_FER.txt"),
    data_2020_s1 = file.path(data_dir, "2020_S1_NB_FER.txt"),
    data_2020_s2 = file.path(data_dir, "2020_S2_NB_FER.txt"),
    data_2021_s1 = file.path(data_dir, "2021_S1_NB_FER.txt"),
    data_2021_s2 = file.path(data_dir, "2021_S2_NB_FER.txt"),
    data_2022_s1 = file.path(data_dir, "2022_S1_NB_FER.txt"),
    data_2022_s2 = file.path(data_dir, "2022_S2_NB_FER.txt"),
    data_2023_s1 = file.path(data_dir, "2023_S1_NB_FER.txt"),
    data_2023_s2 = file.path(data_dir, "2023_S2_NB_FER.txt"),
    shapefile = file.path(data_dir, "REF_ZdA", "PL_ZDL_R_25_12_2024.shp")
  )
}

# Vérification des chemins
paths <- setup_paths()

invisible(lapply(paths, function(path) {
  if (!dir.exists(path) && !file.exists(path)) {
    warning(paste("Le chemin ou fichier suivant est manquant :", path))
  }
}))

# Chargement sécurisé des fichiers de données
safe_read_delim <- function(file_path) {
  tryCatch({
    if (!file.exists(file_path)) {
      warning(paste("Le fichier n'existe pas:", file_path))
      return(NULL)
    }
    read_delim(file_path, delim = "\t", col_types = cols()) %>% clean_names()
  }, error = function(e) {
    warning(paste("Erreur lors de la lecture du fichier:", file_path, "\nErreur:", e$message))
    return(NULL)
  })
}

# Fonction de nettoyage des données
clean_data <- function(df, year) {
  if (is.null(df)) {
    return(NULL)
  }
  
  if (!"date" %in% colnames(df)) {
    warning("La colonne 'date' est absente du fichier.")
    return(NULL)
  }
  
  df %>%
    mutate(
      year = year,
      date = as.Date(date, format = "%Y-%m-%d"),
      id_station = as.character(id_station),
      validation_count = as.numeric(validation_count)
    ) %>%
    filter(
      !is.na(date),
      validation_count >= 0
    ) %>%
    rename(
      station_id = id_station,
      station_name = libelle_arret
    ) %>%
    clean_names() %>%
    arrange(date)
}

# UI de l'application
ui <- fluidPage(
  titlePanel("Analyse des validations en Île-de-France"),
  sidebarLayout(
    sidebarPanel(
      h4("Sélection des paramètres"),
      dateRangeInput("ref_period", "Période de référence :", start = "2018-01-01", end = "2023-12-31"),
      selectInput("station_id", "Rechercher par ID de station :", choices = NULL, selected = NULL)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Carte interactive", leafletOutput("map", height = "600px")),
        tabPanel("Résumé Global", textOutput("global_summary"))
      )
    )
  )
)

# Serveur de l'application
server <- function(input, output, session) {
  # Initialisation des chemins
  paths <- setup_paths()
  
  # Chargement des données
  data <- reactive({
    all_years <- list(
      "2018_S1" = paths$data_2018_s1,
      "2018_S2" = paths$data_2018_s2,
      "2019_S1" = paths$data_2019_s1,
      "2019_S2" = paths$data_2019_s2,
      "2020_S1" = paths$data_2020_s1,
      "2020_S2" = paths$data_2020_s2,
      "2021_S1" = paths$data_2021_s1,
      "2021_S2" = paths$data_2021_s2,
      "2022_S1" = paths$data_2022_s1,
      "2022_S2" = paths$data_2022_s2,
      "2023_S1" = paths$data_2023_s1,
      "2023_S2" = paths$data_2023_s2
    )
    
    # Lecture et nettoyage des fichiers disponibles
    datasets <- lapply(names(all_years), function(year) {
      file <- all_years[[year]]
      if (is.null(file) || !file.exists(file)) {
        warning(paste("Aucun fichier trouvé pour l'année", year))
        return(NULL)
      }
      yearly_data <- safe_read_delim(file)
      clean_data(yearly_data, year = year)
    })
    
    # Combinez les données après le nettoyage
    combined_data <- bind_rows(datasets, .id = "year")
    if (is.null(combined_data) || nrow(combined_data) == 0) {
      warning("Aucune donnée combinée.")
      return(NULL)
    }
    
    combined_data
  })
  
  # Carte interactive
  output$map <- renderLeaflet({
    req(data())
    
    stations <- data() %>%
      filter(!is.na(station_id)) %>%
      distinct(station_id, .keep_all = TRUE)
    
    leaflet(data = stations) %>%
      addTiles() %>%
      addMarkers(
        ~lon, ~lat,
        popup = ~paste("Station ID:", station_id)
      )
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)
