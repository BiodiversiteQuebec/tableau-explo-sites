# Températures et précipitations pour chaque cellule ont été extraits de https://earthmap.org/
# Extraction pour chaque cellule de COLEO, la région administrative (modifiées par Ouranos) & les scénarios prévisionnels des changements climatiques associés
# https://www.ouranos.ca/portraits-climatiques/#/ #

library(sf)
library(raster)
library(geojsonio)
library(rcoleo)
library(dplyr)
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
cellSHINY <- geojsonio::geojson_sf("local_data/cellsCoords2.geojson") # Cellules actuellement utilisées dans le TdeB "description des sites"
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

cellSHINY$cell_num <- 1:length(cellSHINY$Nom)
RegCellsShiny <- dplyr::left_join(RegCellsShiny, cellSHINY[, -4], by = "cell_num")
#RegCellsShiny <- RegCellsShiny[, -5]
names(RegCellsShiny)[names(RegCellsShiny) == "Nom"] <- "cell_id"


#### ------ Récupération des scénarios climatiques dépendemment des régions ----- ####
scenario_meteo <- data.frame()
for (i in unique(RegCellsShiny$Region)){
  files <- list.files("source_data/data_ouranos",
                      pattern = i,
                      full.names = TRUE)
  for (j in files){
    data <- read.csv(j, header = TRUE)
    if(stringr::str_detect(j, "précipitations")){
      param <- "prec"
    }else{
      param <- "temp"
    }
    data$param_met <- param
    data$Region <- i
    scenario_meteo <- rbind(scenario_meteo, data)
  }

}
names(scenario_meteo)[1] <- "Annee"

