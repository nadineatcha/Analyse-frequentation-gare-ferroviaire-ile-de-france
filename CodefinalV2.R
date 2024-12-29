# Chargement des bibliothèques
library(readr)
library(dplyr)
library(ggplot2)
library(shiny)
library(lubridate)
library(DT)

# Fonction pour examiner la structure d'un fichier
examine_file <- function(file_path) {
  cat("Examen du fichier:", file_path, "\n")
  data <- read_delim(file_path, delim = "\t", n_max = 5, show_col_types = FALSE)
  print(str(data))
  return(names(data))
}

# Fonction pour standardiser un dataframe
standardize_df <- function(df) {
  required_cols <- c("CODE_STIF_RES", "CODE_STIF_ARRET", "CODE_STIF_TRNS", "ID_REFA_LDA", "NB_VALD")
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
    df <- read_delim(file_path, delim = "\t", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
    df <- standardize_df(df)
    return(df)
  }, error = function(e) {
    warning(paste("Erreur lors du chargement de", file_path, ":", e$message, "Tentative avec UTF-16."))
    tryCatch({
      df <- read_delim(file_path, delim = "\t", locale = locale(encoding = "UTF-16"), show_col_types = FALSE)
      df <- standardize_df(df)
      return(df)
    }, error = function(e2) {
      warning(paste("Impossible de charger le fichier", file_path, ":", e2$message))
      return(NULL)
    })
  })
}

# Fonction pour nettoyer les données
clean_data <- function(data) {
  data <- data %>%
    filter_all(all_vars(!str_detect(., "\\?"))) %>% # Suppression des lignes contenant ?
    na.omit() %>%                                  # Suppression des lignes avec NA
    mutate(
      CODE_STIF_RES = as.character(CODE_STIF_RES),
      ID_REFA_LDA = as.character(ID_REFA_LDA)
    )
  return(data)
}

# Chargement des données
years <- 2018:2023
data_list <- list()

cat("Examen d'un fichier exemple:\n")
first_file <- paste0("./", years[1], "_S1_NB_FER.txt")
examine_file(first_file)

for (year in years) {
  cat("\nTraitement de l'année:", year, "\n")
  file_s1 <- paste0("./", year, "_S1_NB_FER.txt")
  file_s2 <- paste0("./", year, "_S2_NB_FER.txt")
  
  # Charger les fichiers avec la fonction sécurisée
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
    # Fusion des données pour l'année en cours
    data_year <- bind_rows(data_s1, data_s2)
    data_year <- data_year %>% mutate(Year = year)
    data_list[[as.character(year)]] <- data_year
    cat("Fusion réussie pour", year, "\n")
  }, error = function(e) {
    warning(paste("Erreur lors de la fusion pour", year, ":", e$message))
  })
}

if (length(data_list) > 0) {
  data_all <- bind_rows(data_list)
  
  data_all <- data_all %>%
    mutate(JOUR = as.Date(JOUR, format = "%d/%m/%Y"))
  
  data_agg <- data_all %>% 
    group_by(ID_REFA_LDA, JOUR, CATEGORIE_TITRE) %>% 
    summarise(NB_VALD = sum(NB_VALD, na.rm = TRUE), .groups = "drop")
  
  data_agg_day <- data_agg %>% 
    group_by(JOUR) %>% 
    summarise(NB_VALD = sum(NB_VALD, na.rm = TRUE), .groups = "drop")
  
  # Ajout des jours fériés
  jours_feries <- as.Date(c("2018-01-01", "2018-05-01", "2018-12-25"))
  data_agg_day <- data_agg_day %>%
    mutate(holiday = ifelse(JOUR %in% jours_feries, "Férié", "Normal"),
           weekday = factor(weekdays(JOUR), 
                            levels = c("lundi", "mardi", "mercredi", "jeudi", 
                                       "vendredi", "samedi", "dimanche")))
  
  print("Traitement terminé avec succès!")
} else {
  stop("Aucune donnée n'a pu être chargée correctement")
}

# UI
ui <- fluidPage(
  titlePanel("Analyse des Données de Validation - Tableau de Bord"),
  sidebarLayout(
    sidebarPanel(
      h4("Filtres"),
      dateRangeInput("date_range", "Sélectionner une période de référence",
                     start = "2018-01-01", end = "2018-12-31", language = "fr"),
      dateRangeInput("compare_range", "Sélectionner une période à comparer",
                     start = "2019-01-01", end = "2019-12-31", language = "fr"),
      selectInput("holiday_filter", "Filtrer par jour férié",
                  choices = c("Tous", "Jours fériés", "Jours normaux"),
                  selected = "Tous")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graphique des tendances", 
                 plotOutput("validation_plot"),
                 textOutput("data_info")),
        tabPanel("Comparaison des périodes", 
                 plotOutput("comparison_plot")),
        tabPanel("Tableau des données", 
                 DTOutput("data_table")),
        tabPanel("Statistiques résumées", 
                 verbatimTextOutput("stats_summary"))
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Filtrer les données en fonction des entrées utilisateur
  filtered_data <- reactive({
    data <- data_agg_day %>% filter(JOUR >= input$date_range[1], JOUR <= input$date_range[2])
    if (input$holiday_filter == "Jours fériés") {
      data <- data %>% filter(holiday == "Férié")
    } else if (input$holiday_filter == "Jours normaux") {
      data <- data %>% filter(holiday == "Normal")
    }
    return(data)
  })
  
  # Graphique des tendances
  output$validation_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = JOUR, y = NB_VALD, color = holiday)) +
      geom_line() +
      labs(title = "Tendances de Validation", x = "Date", y = "Nombre de Validations") +
      theme_minimal()
  })
  
  # Tableau des données
  output$data_table <- renderDT({
    datatable(filtered_data())
  })
  
  # Résumé statistique
  output$stats_summary <- renderPrint({
    summary(filtered_data())
  })
}

# Lancer l'application
shinyApp(ui, server)

