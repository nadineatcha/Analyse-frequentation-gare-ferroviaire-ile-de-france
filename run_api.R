# Créez un fichier run_api.R
library(plumber)
source("API.R")  # Pour charger vos fonctions R existantes

# Démarrer le serveur API
plumb("plumber.R") %>% 
  pr_run(host="0.0.0.0", port=8000)