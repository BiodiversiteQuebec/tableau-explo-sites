
rm(list = ls())
library(tidyverse)
library(rcoleo)
library(plyr)
library(dplyr)
library(purrr)
library(jsonlite)
library(geojsonio)
library(sf)

#source("functions.R")
source("make_local_data/Region_Ouranos.R")


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
  mutate(popup_info = paste0("<b> Région</b> ",
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

indic_count <- plyr::count(as.character(all_obs$category))
indic_count$prop <- round((indic_count$freq * 100)/sum(indic_count$freq), digits = 0)
names(indic_count)[1] <- "category"
indic_count <- na.omit(indic_count)

# -------------------------------------------------------- #
#### Diversité alpha pour chaque site vs. toute la BDD ####
# ------------------------------------------------------ #
