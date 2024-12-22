
library(readr)
library(dplyr)
library(sf)
library(data.table)
library(shiny)
library(lubridate)
library(DT)
data_s1 <- read_delim("./2018_S1_NB_FER.txt", delim =  "\t")  
data_s2 <- read_delim("./2018_S2_NB_Fer.txt", delim =  "\t")  


data_all = bind_rows(data_s1,data_s2)
# Vérification des valeurs manquantes
sum(is.na(data_all))
data_all <- na.omit(data_all)

data_agg = data_all %>% group_by(ID_REFA_LDA,JOUR,CATEGORIE_TITRE) %>% summarise(NB_VALD = sum(NB_VALD))


data_agg_day = data_agg %>% group_by(JOUR)  %>% 
  summarise(NB_VALD = sum(NB_VALD)) %>% 
  mutate(JOUR=as.Date(JOUR,format="%d/%m/%Y"))

data_agg_day

library(ggplot2)

ggplot() + geom_line(data=data_agg_day,aes(x=JOUR,y=NB_VALD))

str(data_agg_day)  
head(data_agg_day)
data_agg_day <- data_agg_day %>%
  mutate(weekday = weekdays(JOUR))  # Ajoute les jours de la semaine
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
data_agg_day <- data_agg_day %>%
  mutate(weekday = factor(weekdays(JOUR), 
                          levels = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")))
data_agg_day <- data_agg_day %>%
  mutate(weekday = weekdays(JOUR))

head(data_agg_day$weekday)
table(data_agg_day$weekday)

data_agg_day <- data_agg_day %>%
  mutate(weekday = format(JOUR, "%A")) 

head(data_agg_day)  

sum(data_agg_day$NB_VALD)

library(ggplot2)
#graphique de la tendance par jour de la semaine
ggplot(data_agg_day, aes(x = JOUR, y = NB_VALD)) +
  geom_line(color = "blue") +
  labs(title = "Nombre de validations par jour - 2018",
       x = "Date",
       y = "Nombre de validations") +
  theme_minimal()

#Graphique de la moyenne des validations par jour de la semaine
ggplot(data_agg_day, aes(x = weekday, y = NB_VALD)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Distribution des validations par jour de la semaine",
       x = "Jour de la semaine",
       y = "Nombre de validations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Agrégation par station
data_station_agg <- data_all %>%
  group_by(ID_REFA_LDA) %>%
  summarise(total_validations = sum(NB_VALD)) %>%
  arrange(desc(total_validations))  # Trier par nombre de validations décroissant

# Visualisation des premières stations avec le plus de passagers
head(data_station_agg)

# Agrégation par station
data_station_agg <- data_all %>%
  group_by(ID_REFA_LDA) %>%
  summarise(total_validations = sum(NB_VALD)) %>%
  arrange(desc(total_validations))  # Trier par nombre de validations décroissant

# Visualisation des premières stations avec le plus de passagers
head(data_station_agg)


library(ggplot2)

# Les 10 stations les plus fréquentées
top_stations <- head(data_station_agg, 10)


#  Graphique à barres pour afficher les 10 stations avec le plus de passagers
ggplot(top_stations, aes(x = reorder(ID_REFA_LDA, total_validations), y = total_validations)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "darkblue") +
  labs(title = "Top 10 des stations avec le plus de passagers",
       x = "Station (ID_REFA_LDA)",
       y = "Total des validations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Incliner les noms des stations pour plus de lisibilité

# Agrégation par mois
data_agg_month <- data_agg_day %>%
  mutate(month = format(JOUR, "%b")) %>%  # Extraire le mois au format abrégé
  group_by(month) %>%
  summarise(NB_VALD = sum(NB_VALD))

# Ajouter une colonne pour représenter les mois dans le bon ordre
data_agg_month <- data_agg_month %>%
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", 
                                          "jul", "aug", "sep", "oct", "nov", "dec")))

# Graphique de la tendance par mois
ggplot(data_agg_month, aes(x = month, y = NB_VALD)) +
  geom_line(group = 1, color = "green") +  # Ajout de group=1 pour relier les points
  labs(title = "Nombre de validations par mois - 2018", 
       x = "Mois", 
       y = "Nombre de validations") +
  theme_minimal()


str(data_agg_day)
jours_feries <- as.Date(c("2018-01-01", "2018-05-01", "2018-07-14", "2018-12-25"))
data_agg_day <- data_agg_day %>%
  mutate(holiday = ifelse(JOUR %in% jours_feries, "Férié", "Normal"))

table(data_agg_day$holiday)


ggplot(data_agg_day, aes(x = JOUR, y = NB_VALD, color = holiday)) +
  geom_line() +
  scale_color_manual(values = c("Férié" = "red", "Normal" = "blue")) +  # Définir des couleurs spécifiques
  labs(title = "Fréquentation entre jours fériés et jours normaux",
       x = "Date", 
       y = "Nombre de validations",
       color = "Type de jour") +
  theme_minimal()

# Graphique avec les jours fériés et jours normaux
ggplot(data_agg_day, aes(x = JOUR, y = NB_VALD, color = holiday)) +
  geom_line() +
  labs(title = "Fréquentation entre jours fériés et jours normaux",
       x = "Date", 
       y = "Nombre de validations") +
  theme_minimal()


# Exemple de données (remplacer par vos données réelles)
data_agg_day <- data.frame(
  JOUR = seq.Date(from = as.Date("2018-01-01"), by = "day", length.out = 365),
  NB_VALD = sample(1000:10000, 365, replace = TRUE)
)


# Supposons que `data_agg_day` est déjà disponible

# UI : Interface utilisateur
ui <- fluidPage(
  titlePanel("Analyse des Données de Validation - Tableau de Bord"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtres"),
      dateRangeInput("date_range", 
                     "Sélectionner une période de référence", 
                     start = "2018-01-01", 
                     end = "2018-12-31"),
      dateRangeInput("compare_range", 
                     "Sélectionner une période à comparer", 
                     start = "2018-01-01", 
                     end = "2018-12-31"),
      selectInput("holiday_filter", 
                  "Filtrer par jour férié", 
                  choices = c("Tous", "Jours fériés", "Jours normaux"), 
                  selected = "Tous")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Graphique des tendances", plotOutput("validation_plot")),
        tabPanel("Comparaison des périodes", plotOutput("comparison_plot")),
        tabPanel("Tableau des données", DTOutput("data_table")),
        tabPanel("Statistiques résumées", verbatimTextOutput("summary_stats"))
      )
    )
  )
)

# Server : Côté serveur
server <- function(input, output, session) {
  
  # Fonction pour filtrer les données
  filtered_data <- reactive({
    data_filtered <- data_agg_day %>%
      filter(JOUR >= input$date_range[1] & JOUR <= input$date_range[2])
    
    if (input$holiday_filter == "Jours fériés") {
      jours_feries <- as.Date(c("2018-01-01", "2018-05-01", "2018-07-14", "2018-12-25"))
      data_filtered <- data_filtered %>%
        mutate(holiday = ifelse(JOUR %in% jours_feries, "Férié", "Normal")) %>%
        filter(holiday == "Férié")
    } else if (input$holiday_filter == "Jours normaux") {
      jours_feries <- as.Date(c("2018-01-01", "2018-05-01", "2018-07-14", "2018-12-25"))
      data_filtered <- data_filtered %>%
        mutate(holiday = ifelse(JOUR %in% jours_feries, "Férié", "Normal")) %>%
        filter(holiday == "Normal")
    }
    
    return(data_filtered)
  })
  
  # Graphique des tendances
  output$validation_plot <- renderPlot({
    data_filtered <- filtered_data()
    
    ggplot(data_filtered, aes(x = JOUR, y = NB_VALD)) +
      geom_line(color = "blue") +
      labs(title = "Nombre de validations par jour", 
           x = "Date", 
           y = "Nombre de validations") +
      theme_minimal()
  })
  
  # Comparaison des périodes
  output$comparison_plot <- renderPlot({
    data_reference <- data_agg_day %>%
      filter(JOUR >= input$date_range[1] & JOUR <= input$date_range[2])
    
    data_comparison <- data_agg_day %>%
      filter(JOUR >= input$compare_range[1] & JOUR <= input$compare_range[2])
    
    ggplot() +
      geom_line(data = data_reference, aes(x = JOUR, y = NB_VALD), color = "blue", linetype = "dashed") +
      geom_line(data = data_comparison, aes(x = JOUR, y = NB_VALD), color = "red") +
      labs(title = "Comparaison des périodes sélectionnées", 
           x = "Date", 
           y = "Nombre de validations") +
      theme_minimal() +
      scale_color_manual(values = c("Période de Référence" = "blue", 
                                    "Période à Comparer" = "red"))
  })
  
  # Tableau interactif
  output$data_table <- renderDT({
    data_filtered <- filtered_data()
    datatable(data_filtered, options = list(pageLength = 10))
  })
  
  # Statistiques résumées
  output$summary_stats <- renderPrint({
    data_filtered <- filtered_data()
    summary(data_filtered$NB_VALD)
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)


