
 Analyse-frequentation-gare-ferroviaire-ile-de-france

## Description

Application Shiny développée pour analyser et visualiser les données de validation d'un système de transport. Elle permet de filtrer, agréger et explorer les données par jour, mois, station et type de jour (jour férié ou normal). L'application offre une série de graphiques interactifs pour examiner les tendances des validations et fournir des statistiques résumées.

Les principales fonctionnalités incluent :
- **Analyse des tendances par jour** : Visualisation des variations du nombre de validations au fil du temps.
- **Comparaison des périodes** : Permet de comparer deux périodes spécifiques pour observer les différences.
- **Distribution des validations par jour de la semaine** : Graphiques illustrant les validations en fonction du jour de la semaine.
- **Analyse par station** : Affichage des stations les plus fréquentées en fonction des validations.
- **Filtrage par jour férié ou normal** : Sélection des jours fériés et des jours normaux pour un comparatif.

## Installation

### Prérequis

- [R](https://cran.r-project.org/)
- [RStudio](https://posit.co/download/rstudio-desktop/) (facultatif, mais recommandé)

### Dépendances

Les principales bibliothèques utilisées dans ce projet sont :

- `shiny` : Pour la création de l'application web interactive.
- `ggplot2` : Pour la visualisation des graphiques.
- `dplyr` : Pour la manipulation des données.
- `lubridate` : Pour travailler avec les dates.
- `DT` : Pour la visualisation interactive des tableaux de données.
- `readr` : Pour lire les fichiers de données.

### Lancer l'application
Clonez ce projet sur votre machine locale :


git clone https://github.com/nadineatcha/Analyse-frequentation-gare-ferroviaire-ile-de-france
Ouvrez le dossier du projet dans RStudio (ou un IDE R de votre choix).

Exécutez le fichier app.R pour lancer l'application Shiny :

shiny::runApp()

# 1. Analyse des tendances de validation
Visualisation du nombre de validations par jour, avec la possibilité de voir les variations sur l'ensemble de la période ou sur des périodes spécifiques. Un graphique linéaire permet d'observer les tendances.

2. Comparaison de périodes
Vous pouvez sélectionner deux plages de dates et comparer le nombre de validations dans ces deux périodes via un graphique.

3. Distribution des validations par jour de la semaine
Un graphique en boîte permet d'analyser la distribution des validations par jour de la semaine (lundi à dimanche).

4. Analyse par station
Un graphique en barres permet de voir les stations les plus fréquentées selon le nombre de validations.

5. Filtrage par jour férié ou normal
Les utilisateurs peuvent filtrer les données pour ne voir que les jours fériés ou les jours normaux, permettant ainsi de comparer les différences dans les tendances de validation.


