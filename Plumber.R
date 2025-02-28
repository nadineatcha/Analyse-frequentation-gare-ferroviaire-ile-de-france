# Créez un fichier plumber.R
library(plumber)

#* @apiTitle API FlowSight pour l'analyse de trafic IDF

#* Obtenir les prédictions pour une station
#* @param station_id ID de la station
#* @param date Date au format YYYY-MM-DD
#* @get /predictions
function(station_id, date = Sys.Date()) {
  # Utiliser votre fonction existante
  predictions <- generate_predictions(station_id, as.Date(date))
  return(predictions)
}

#* Obtenir les stations à proximité
#* @param lat Latitude
#* @param lon Longitude
#* @get /nearby_stations
function(lat, lon) {
  # Utiliser votre fonction existante
  stations <- find_nearby_stations(as.numeric(lat), as.numeric(lon))
  return(stations)
}