# api.R
library(plumber)
library(tidyverse)
source("App.R")  # Inclure votre code d'analyse R existant

#* @apiTitle API FlowSight pour l'analyse de trafic IDF

#* Obtenir les prédictions pour une station
#* @param station_id ID de la station
#* @param date Date au format YYYY-MM-DD
#* @get /predictions
function(station_id, date = Sys.Date()) {
  # Utiliser votre code R existant pour générer des prédictions
  predictions <- generate_predictions(station_id, as.Date(date))
  return(predictions)
}

#* Obtenir les stations à proximité
#* @param lat Latitude
#* @param lon Longitude
#* @get /nearby_stations
function(lat, lon) {
  # Code pour trouver les stations proches des coordonnées
  stations <- find_nearby_stations(as.numeric(lat), as.numeric(lon))
  return(stations)
}