# Configuration initiale et gestion des chemins
library(here)
library(tidyverse)
library(lubridate)
library(janitor)
library(shiny)
library(plotly)
library(sf)
library(leaflet)
library(knitr)      
library(kableExtra) 
library(rsconnect)
library(DT)

# Fonction pour gérer les chemins de fichiers
setup_paths <- function() {
  # Utilisation de here() pour gérer les chemins de manière portable
  data_dir <- here::here("data")
  
  paths <- list(
    data_2018 = file.path(data_dir, "data-rf-2018"),
    data_2019 = file.path(data_dir, "data-rf-2019"),
    data_2020 = file.path(data_dir, "data-rf-2020"),
    data_2021 = file.path(data_dir, "data-rf-2021"),
    data_2022 = file.path(data_dir, "data-rf-2022"),
    data_2023 = file.path(data_dir, "data-rf-2023"),
    shapefile = file.path(data_dir, "REF_ZdA", "PL_ZDL_R_25_12_2024.shp")
  )
  
  # Vérification de l'existence des dossiers
  for (path in paths) {
    if (!dir.exists(dirname(path))) {
      dir.create(dirname(path), recursive = TRUE)
    }
  }
  
  return(paths)
}

# Fonction pour charger les données de manière sécurisée
safe_read_delim <- function(file_path, type = "NB") {
  tryCatch({
    if (!file.exists(file_path)) {
      warning(paste("Le fichier n'existe pas:", file_path))
      return(NULL)
    }
    
    # Nettoyage des caractères nuls si nécessaire
    temp_file <- clean_null_chars_file(file_path)
    
    df <- read_delim(temp_file, delim = "\t", col_types = cols())
    df <- clean_names(df)
    df <- handle_malformed_file(df, type)
    
    if (type == "NB") {
      df <- clean_nb_data(df, year = as.numeric(substr(basename(file_path), 1, 4)))
    } else {
      df <- clean_profil_data(df, year = as.numeric(substr(basename(file_path), 1, 4)))
    }
    
    df <- harmonize_columns(df)
    return(df)
    
  }, error = function(e) {
    warning(paste("Erreur lors de la lecture du fichier:", file_path, "\nErreur:", e$message))
    return(NULL)
  })
}

# UI de l'application
ui <- fluidPage(
  titlePanel("Analyse des validations en Île-de-France"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Sélection des paramètres"),
      dateRangeInput(
        "ref_period",
        "Période de référence :",
        start = "2018-01-01",
        end   = "2023-12-31",
        min   = "2018-01-01",
        max   = "2023-12-31"
      ),
      dateRangeInput(
        "comp_period",
        "Période à comparer :",
        start = "2018-01-01",
        end   = "2023-12-31",
        min   = "2018-01-01",
        max   = "2023-12-31"
      ),
      checkboxInput("filter_holidays", "Inclure les jours fériés", value = TRUE),
      checkboxInput("filter_vacances", "Inclure les vacances scolaires", value = TRUE),
      
      h4("Choix de la station"),
      selectizeInput(
        "station_name",
        "Rechercher par nom de station :",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(
          placeholder = 'Tapez le nom d'une station',
                    onInitialize = I('function() { this.setValue(""); }')
                )
            ),
            selectInput(
                "station_id",
                "Rechercher par ID de station :",
                choices = NULL,
                selected = NULL
            )
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Carte interactive", leafletOutput("map", height = "600px")),
                tabPanel("Résumé Global",
                        textOutput("global_summary"),
                        DT::dataTableOutput("summary_table")
                ),
                tabPanel("Tendances Annuelles", plotlyOutput("annual_trends_plotly")),
                tabPanel("Tendances Mensuelles", plotlyOutput("monthly_trends_plotly")),
                tabPanel("Comparaison de Périodes", plotlyOutput("comparison_plotly")),
                tabPanel("Cumul des Validations", plotlyOutput("cumulative_plotly")),
                tabPanel("Heatmap", plotlyOutput("heatmap_validations")),
                tabPanel("Analyse Statistique",
                        verbatimTextOutput("t_test_result"),
                        verbatimTextOutput("anova_result")
                )
            )
        )
    )
)

# Server de l'application
        server <- function(input, output, session) {
          # Initialisation des chemins
          paths <- setup_paths()
          
          # Chargement initial des données
          data <- reactive({
            # Charger et combiner toutes les données ici
            # Utilisez safe_read_delim pour chaque fichier
            # Retournez les données combinées
          })
          
          # Le reste du code server reste identique à votre version originale,
          # mais utilisez data() au lieu de final_nb
          
          # Exemple de modification pour la carte
          output$map <- renderLeaflet({
            req(input$station_id)
            
            data_poly <- data() %>%
              filter(id_refa_lda == input$station_id)
            
            if (nrow(data_poly) == 0) {
              return(leaflet() %>%
                       addTiles() %>%
                       setView(lng = 2.3522, lat = 48.8566, zoom = 11) %>%
                       addPopups(lng = 2.3522, lat = 48.8566, 
                                 popup = "Sélectionnez une station"))
            }
            
            data_points <- st_centroid(data_poly)
            
            leaflet() %>%
              addTiles() %>%
              addMarkers(
                data = data_points,
                lng = ~st_coordinates(geometry)[,1],
                lat = ~st_coordinates(geometry)[,2],
                popup = ~paste(
                  "Station:", libelle_arret,
                  "<br>ID:", id_refa_lda
                )
              )
          })
          
          # Ajoutez ici le reste de votre code server...
        }
        
        # Lancement de l'application
        shinyApp(ui = ui, server = server)