# Chargement des bibliothèques
library(readr)
library(dplyr)
library(ggplot2)
library(shiny)
library(lubridate)
library(DT)
library(leaflet)

# Fonction pour examiner la structure d'un fichier
examine_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("Le fichier", file_path, "n'existe pas."))
  }
  cat("Examen du fichier:", file_path, "\n")
  data <- read_delim(file_path, delim = "\t", n_max = 5, show_col_types = FALSE)
  print(str(data))
  return(names(data))
}

# Fonction pour standardiser un dataframe
standardize_df <- function(df) {
  required_cols <- c("CODE_STIF_RES", "CODE_STIF_ARRET", "CODE_STIF_TRNS", 
                     "ID_REFA_LDA", "NB_VALD")
  for (col in required_cols) {
    if (col %in% names(df)) {
      if (col == "NB_VALD") {
        df[[col]] <- as.numeric(df[[col]])
      } else {
        df[[col]] <- as.character(df[[col]])
      }
    }
  }
  return(df)
}

# Fonction sécurisée pour charger un fichier
load_file_safely <- function(file_path) {
  if (!file.exists(file_path)) {
    warning(paste("Le fichier n'existe pas:", file_path))
    return(NULL)
  }
  
  tryCatch({
    df <- read_delim(file_path, delim = "\t", 
                     locale = locale(encoding = "UTF-8"), 
                     show_col_types = FALSE)
    df <- standardize_df(df)
    return(df)
  }, error = function(e) {
    warning(paste("Erreur lors du chargement de", file_path, ":", 
                  e$message, "Tentative avec UTF-16."))
    tryCatch({
      df <- read_delim(file_path, delim = "\t", 
                       locale = locale(encoding = "UTF-16"), 
                       show_col_types = FALSE)
      df <- standardize_df(df)
      return(df)
    }, error = function(e2) {
      warning(paste("Impossible de charger le fichier", file_path, ":", 
                    e2$message))
      return(NULL)
    })
  })
}

# Chargement des données et nettoyage
years <- 2018:2023
data_list <- list()

cat("Examen d'un fichier exemple:\n")
first_file <- paste0("./", years[1], "_S1_NB_FER.txt")
examine_file(first_file)

for (year in years) {
  cat("\nTraitement de l'année:", year, "\n")
  file_s1 <- paste0("./", year, "_S1_NB_FER.txt")
  file_s2 <- paste0("./", year, "_S2_NB_FER.txt")
  
  data_s1 <- load_file_safely(file_s1)
  if (is.null(data_s1)) {
    warning(paste("Fichier S1 pour", year, "non chargé"))
    next
  }
  
  data_s2 <- load_file_safely(file_s2)
  if (is.null(data_s2)) {
    warning(paste("Fichier S2 pour", year, "non chargé"))
    next
  }
  
  tryCatch({
    data_year <- bind_rows(data_s1, data_s2)
    data_year <- data_year %>% mutate(Year = year)
    data_list[[as.character(year)]] <- data_year
    cat("Fusion réussie pour", year, "\n")
  }, error = function(e) {
    warning(paste("Erreur lors de la fusion pour", year, ":", e$message))
  })
}

# Traitement des données
if (length(data_list) > 0) {
  data_all <- bind_rows(data_list)
  
  # Conversion des dates avec gestion d'erreurs
  data_all <- data_all %>%
    mutate(
      JOUR = case_when(
        !is.na(as.Date(as.character(JOUR), format = "%Y-%m-%d")) ~ 
          as.Date(as.character(JOUR), format = "%Y-%m-%d"),
        !is.na(as.Date(as.character(JOUR), format = "%d/%m/%Y")) ~ 
          as.Date(as.character(JOUR), format = "%d/%m/%Y"),
        TRUE ~ as.Date(NA)
      )
    )
  
  # Vérification des dates non converties
  if (sum(is.na(data_all$JOUR)) > 0) {
    warning(paste("Attention:", sum(is.na(data_all$JOUR)), 
                  "dates n'ont pas pu être converties"))
  }
  
  # Agrégation par ID_REFA_LDA, JOUR et CATEGORIE_TITRE
  data_agg <- data_all %>% 
    group_by(ID_REFA_LDA, JOUR, CATEGORIE_TITRE) %>% 
    summarise(NB_VALD = sum(NB_VALD, na.rm = TRUE), .groups = "drop")
  
  # Agrégation par jour
  data_agg_day <- data_agg %>% 
    group_by(JOUR) %>% 
    summarise(NB_VALD = sum(NB_VALD, na.rm = TRUE), .groups = "drop")
  
  # Définition des jours fériés (à compléter selon les besoins)
  jours_feries <- as.Date(c("2018-01-01", "2018-05-01", "2018-12-25"))
  
  # Ajout des informations sur les jours
  data_agg_day <- data_agg_day %>%
    mutate(
      holiday = ifelse(JOUR %in% jours_feries, "Férié", "Normal"),
      weekday = factor(weekdays(JOUR, abbreviate = FALSE), 
                       levels = c("lundi", "mardi", "mercredi", "jeudi", 
                                  "vendredi", "samedi", "dimanche"))
    )
  
  print("Traitement terminé avec succès!")
} else {
  stop("Aucune donnée n'a pu être chargée correctement")
}

# Données de localisation des gares (exemple)
locations_data <- tibble(
  Nom_Gare = c("Gare A", "Gare B", "Gare C"),
  Latitude = c(48.8566, 48.8575, 48.8588),
  Longitude = c(2.3522, 2.3430, 2.3490)
)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Analyse des Données de Validation - Tableau de Bord"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", 
                     "Sélectionner une période de référence",
                     start = min(data_agg_day$JOUR),
                     end = max(data_agg_day$JOUR)),
      
      dateRangeInput("compare_range", 
                     "Sélectionner une période à comparer",
                     start = min(data_agg_day$JOUR),
                     end = max(data_agg_day$JOUR)),
      
      selectInput("view_type", 
                  "Type de vue",
                  choices = c("Journalier" = "day",
                              "Hebdomadaire" = "week",
                              "Mensuel" = "month")),
      
      checkboxInput("show_holidays", 
                    "Afficher les jours fériés", 
                    value = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tendances", 
                 plotOutput("validation_plot"),
                 DTOutput("summary_table")),
        
        tabPanel("Comparaison", 
                 plotOutput("comparison_plot")),
        
        tabPanel("Carte", 
                 leafletOutput("map"),
                 DTOutput("station_table")),
        
        tabPanel("Statistiques",
                 verbatimTextOutput("statistics"))
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Données filtrées pour la période de référence
  filtered_data <- reactive({
    data_agg_day %>%
      filter(JOUR >= input$date_range[1], 
             JOUR <= input$date_range[2])
  })
  
  # Données filtrées pour la période de comparaison
  comparison_data <- reactive({
    data_agg_day %>%
      filter(JOUR >= input$compare_range[1], 
             JOUR <= input$compare_range[2])
  })
  
  # Graphique des tendances
  output$validation_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = JOUR, y = NB_VALD)) +
      geom_line(color = "steelblue") +
      geom_point(aes(color = holiday), 
                 show.legend = input$show_holidays) +
      scale_color_manual(values = c("Normal" = "steelblue", 
                                    "Férié" = "red")) +
      labs(title = "Tendances de Validation",
           x = "Date",
           y = "Nombre de validations") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Graphique de comparaison
  output$comparison_plot <- renderPlot({
    ggplot() +
      geom_line(data = filtered_data(), 
                aes(x = JOUR, y = NB_VALD, color = "Référence")) +
      geom_line(data = comparison_data(), 
                aes(x = JOUR, y = NB_VALD, color = "Comparaison")) +
      scale_color_manual(values = c("Référence" = "steelblue", 
                                    "Comparaison" = "red")) +
      labs(title = "Comparaison des périodes",
           x = "Date",
           y = "Nombre de validations",
           color = "Période") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Carte des gares
  output$map <- renderLeaflet({
    leaflet(locations_data) %>%
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, 
                 popup = ~paste0("<b>", Nom_Gare, "</b><br>",
                                 "Validations: ", format(1000, big.mark = " ")))
  })
  
  # Tableau récapitulatif
  output$summary_table <- renderDT({
    filtered_data() %>%
      group_by(weekday) %>%
      summarise(
        `Moyenne des validations` = mean(NB_VALD, na.rm = TRUE),
        `Maximum` = max(NB_VALD, na.rm = TRUE),
        `Minimum` = min(NB_VALD, na.rm = TRUE)
      ) %>%
      datatable(options = list(pageLength = 7))
  })
  
  # Statistiques descriptives
  output$statistics <- renderPrint({
    cat("Statistiques descriptives pour la période sélectionnée:\n\n")
    summary(filtered_data()$NB_VALD)
  })
}

# Lancement de l'application
shinyApp(ui, server)