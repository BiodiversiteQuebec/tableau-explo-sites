# tableau-explo-sites

Claire-Cécile Juhasz Octobre 2020

## Description
Explorateur pour les sites d'échantillonnage du Réseau d'Observation de la Biodiversité du Québec

##Les données incluent
- Occurences des espèces issues de COLEO
- Données météorologiques des sites d'échantillonnage issues de https://earthmap.org/
- Scénarios de changements climatiques (températures et précipitations) par région québécoise issus https://www.ouranos.ca/portraits-climatiques/

## Description des scripts
La diversité alpha des sites est obtenue grâce au script **make_local_data.R**.

L'application Shiny est lancée avec le script **app.R**. Plusieurs dépendances sont chargées automatiquement.

L'exécution du script requiert plusieurs librairies R, celles-ci peuvent être installées avec le lancement de **script_installation.r**.
