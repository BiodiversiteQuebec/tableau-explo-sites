# Températures et précipitations pour chaque cellule extraites de https://earthmap.org/
# Extraction des scénarios prévisionnels des changements climatiques associés à chaque région administrative (modifiées par Ouranos)
# https://www.ouranos.ca/portraits-climatiques/#/ #

library(tidyverse)
library(rcoleo)
library(sf)
library(raster)
library(geojsonio)
library(jsonlite)
library(rcoleo)
library(plyr)
library(dplyr)
library(purrr)
library(stringr)

# ------------------------------ #
#### Environmental data uniquement pour les cellules dans le prototype du TdeB ####
# ---------------------------- #

meteoCELLS <- list.files(path = "source_data/data_meteo_cells", full.names = TRUE)
nb <- nchar("source_data/data_meteo_cells/") # For counting the number of characters in the path

meteoCELLSdf <- data.frame()
for(i in 1:length(meteoCELLS)){
  path_cell <- stringr::str_sub(meteoCELLS[i], nb+1)
  path_break <- unlist(strsplit(path_cell, "-"))

  cell_id <- path_break[1]
  indic_meteo <- stringr::str_sub(path_break[2], 1, 4)

  tab <- readr::read_csv(meteoCELLS[[i]], col_names = FALSE, skip = 2)
  tab$cell_id <- cell_id
  tab$indic_meteo <- indic_meteo

  meteoCELLSdf <- rbind(meteoCELLSdf, tab)
}

names(meteoCELLSdf)[names(meteoCELLSdf) == "X1" | names(meteoCELLSdf) == "X2"] <- c("Month", "Value")
meteoCELLSdf$indic_meteo[meteoCELLSdf$indic_meteo == "Mean"] <- "Temp"

meteoCELLSdf$Month <- as.factor(meteoCELLSdf$Month)
meteoCELLSdf$cell_id <- as.numeric(meteoCELLSdf$cell_id)
meteoCELLSdf$indic_meteo <- as.factor(meteoCELLSdf$indic_meteo)

# sf objects
reg <- geojsonio::geojson_sf("source_data/data_ouranos/regions_simplified_Ouranos.geojson") # régions du Québec modifiées par Ouranos
#cellSHINY2 <- geojsonio::geojson_sf("local_data/cellsCoords2.geojson")
cellSHINY <- geojsonio::geojson_sf("local_data/cellsCoords3.geojson") # Cellules actuellement utilisées dans le TdeB "description des sites"
#cellSHINY$id <- 1:length(cellSHINY$IJ)

cent <- st_intersects(reg, st_centroid(cellSHINY))

RegCellsShiny <- data.frame()
for (i in 1:length(cent)){
  if(length(cent[[i]]) != 0){
    c <- data.frame(id = rep(i, length(cent[[i]])), cell_num = cent[[i]])
    RegCellsShiny <- rbind(RegCellsShiny, c)
  }
}

RegCellsShiny <- dplyr::left_join(RegCellsShiny, reg[, c("id", "Region")], by = "id")
names(RegCellsShiny)[names(RegCellsShiny) == "id"] <- "region_id"
RegCellsShiny <- RegCellsShiny[,-4]

cellSHINY$cell_num <- 1:length(cellSHINY$NOM)
RegCellsShiny <- dplyr::left_join(RegCellsShiny, cellSHINY[, -4], by = "cell_num")
#RegCellsShiny <- RegCellsShiny[, -5]
names(RegCellsShiny)[names(RegCellsShiny) == "NOM"] <- "cell_id"

# ------------------------------ #
#### ------ Récupération des scénarios climatiques pour chaque région ----- ####
# ------------------------------ #
scenario_meteo <- data.frame()
for (i in unique(RegCellsShiny$Region)){
  files <- list.files("source_data/data_ouranos",
                      pattern = i,
                      full.names = TRUE)
  for (j in files){
    data <- read.csv(j,
                     header = TRUE,
                     sep = ",",
                     encoding = "US-ASCII")
    if(stringr::str_detect(j, "Total") == TRUE){
      param <- "prec"
    }else{
      param <- "temp"
    }
    data$param_met <- param
    data$Region <- i

    # pour la continuité des iC dans le graphique des scénarios climatiques
    data$Hist.Min[data[1] == 2007] <- min(data$rcp45.Min[data[1] == 2007], data$rcp85.Min[data[1] == 2007])
    data$Hist.Max[data[1] == 2007] <- min(data$rcp45.Max[data[1] == 2007], data$rcp85.Max[data[1] == 2007])
    data$Obs[data[1] > 2007] <- NA

    scenario_meteo <- rbind(scenario_meteo, data)
  }

}
names(scenario_meteo)[1] <- "Annee"


#### Manip Coléo ####
# ----------------------------------------------------- #
#### Obtention des informations pour tous les sites ####
# --------------------------------------------------- #

all_sites <- do.call("rbind", get_sites()[[1]]$body)

# ----------------------------------------------------------- #
#### Obtention des coordonnées des sites d'échantillonnage ####
# ----------------------------------------------------------- #

k <- as.data.frame(do.call("rbind", all_sites$geom.coordinates))
all_sites <- all_sites %>%
  mutate(long_site = do.call("rbind", all_sites$geom.coordinates)[,1],
         lat_site = do.call("rbind", all_sites$geom.coordinates)[,2])

# Obtention de l'année d'ouverture des sites
all_sites$open_year <- do.call("rbind",strsplit(all_sites$opened_at, "-"))[,1]

# Association d'une couleur par type d'échantillonnage
all_sites$col <- c("#66CC00", "#000066", "#666600", "#003333", "#0066CC", "#FF9900", "#660000")[as.integer(as.factor(all_sites$type))]

# Récupération des régions associées au cellules
all_sites <- dplyr::left_join(all_sites, RegCellsShiny[, c("Region", "IJ")], by = c("cell.cell_code" = "IJ"))

# Creation de pop-ups
all_sites <- all_sites %>%
  mutate(popup_info = paste0("<b> Region</b> ",
                             Region,
                             "<br/>","<b> id_cellule</b> ",
                             cell_id,
                             "<br/>",
                             "<b> code_cellule</b> ",
                             cell.cell_code,
                             "<br/>",
                             "<b> nom_cellule</b> ",
                             cell.name,
                             "<br/>",
                             "<b> code_site</b> ",
                             site_code,
                             "<br/>",
                             "<b> type_echantillonnage</b> ",
                             type,
                             "<br/>",
                             "<b> annee_creation_site</b> ",
                             open_year))
# Nettoyage du DF
all_sites <- all_sites[, c("id", "cell_id", "site_code", "type", "opened_at", "cell.cell_code", "long_site", "lat_site", "open_year", "col", "popup_info")]
names(all_sites)[c(1, 4, 5, 6, 9)] <- c("site_id", "hab_type", "site_opened_at", "cell_code", "site_opened_year")

# ------------------------------- #
#### Observations des especes ####
# ----------------------------- #

# via get_gen() directement - Plus rapide
observations <- rcoleo::get_gen("/observations")
observations <- do.call("rbind.fill", observations[[1]])
observations <- observations[, c("id", "date_obs", "stratum", "campaign_id", "obs_species.id", "obs_species.taxa_name", "obs_species.variable", "obs_species.value")]
names(observations)[c(1, 5:8)] <- c("obs_id", "species_id", "name", "species_value_type", "species_value")

# Association du type de campagne pour chaque observations
# Préparation du DF campaigns
campaigns <- rcoleo::get_gen("/campaigns") # Identique à rcoleo::get_campaigns()
campaigns <- do.call("rbind.fill", campaigns[[1]])
campaigns <- campaigns[, c("id", "site_id", "type", "opened_at", "closed_at")]
names(campaigns)[c(1, 3:5)] <- c("campaign_id", "campaign_type", "campaign_opened_at", "campaign_closed_at")

# Tables joining
obsCamp <- dplyr::left_join(observations, campaigns, by = "campaign_id")

# Association avec le type d'indicateurs (= species category)
getSpecies <- rcoleo::get_species()
getSpecies <- do.call("rbind.fill", getSpecies[[1]])

obsCampCat <- dplyr::left_join(obsCamp, getSpecies[, c("name", "vernacular_fr", "rank", "category")], by = "name")

# Association du nom du site où l'observation a eu lieu
all_obs <- left_join(obsCampCat, all_sites, by = "site_id")

# Nettoyage du DF
# Noms de variables
# names(all_obs)[c(3, 4, 8, 18, 21:24)] <- c(, "site_open_year", "hab_type", "site_open_at", "cell_id", "cell_name")
# all_obs$hab_type <- as.character(all_obs$hab_type)
# all_obs$cell_id <- as.numeric(all_obs$cell_id)
# all_obs$lat_site <- as.numeric(all_obs$lat_site)

# Année d'observations
all_obs$obs_year <- as.factor(do.call("rbind",strsplit(as.character(all_obs$date_obs), "-"))[,1])

# Ajout de l'information régionale à all_obs
all_obs <- dplyr::left_join(all_obs, RegCellsShiny[, c("cell_id", "Region")], by = "cell_id")


# ------------------------------------------------ #
#### Compte des différents types d'indicateurs ####
# ---------------------------------------------- #
# Occurences totales et diversité alpha des indicateurs dans Coléo

indic_div_coleo <- all_obs %>%
  dplyr::group_by(category) %>%
  dplyr::summarise(freq_coleo = length(name),
                   alpha_coleo = length(unique(name)))
indic_div_coleo <- na.omit(indic_div_coleo)

# Occurences totales et diversité alpha des indicateurs par sites
indic_div_sites <- all_obs %>%
  dplyr::group_by(site_code, category) %>%
  dplyr::summarise(freq_site = length(name),
                   alpha_site = length(unique(name)))

essai <- split(indic_div_sites, indic_div_sites$site_code)

for(i in 1:length(essai)){
  diff_cat <- setdiff(indic_div_coleo$category, essai[[i]]$category)
  if(length(diff_cat) != 0){
    k <- data.frame(category = diff_cat)
    k$site_code <- unique(essai[[i]]$site_code)
    k$freq_site <- 0
    k$alpha_site <- 0
    k <- k[order(k$category),]

    essai[[i]] <- rbind(essai[[i]], k)
  }
}

indic_div_sites <- do.call(rbind, essai)
indic_div_sites <- indic_div_sites[!is.na(indic_div_sites$category),]
