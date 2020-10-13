# Extraction pour chaque cellule de COLEO, la région administrative (modifiées par Ouranos) #
# https://www.ouranos.ca/portraits-climatiques/#/ #

library(sf)
library(raster)
library(geojsonio)
library(rcoleo)
library(dplyr)


# ------------------------------------------------ #
#### Formatage des coord. des cellules --> geoJson pour extraction des données météo ####
# Via earthmap.org - https://earthmap.org/
# ---------------------------------------------- #
setwd("/home/claire/PostDoc_COLEO/shiny_site_explo/TdeB")
# # Obtention de toutes les cellules
# cells <- rcoleo::get_gen("/cells")
# cells <- do.call("rbind.fill", cells$body)
#
# # Conversion des polygones en spatial feature
# geom <- rcoleo::cl_to_sf(cells[,c(1, 7, 8)])
# names(geom)[1] <- "name"
#
# # Obtention des cellules uniquement présentes dans le prototype de la shiny app
# #cells : 134 198 151 165 119 149 160 157 169 186 141 446 495
# uniq_cells <- unique(all_obs$cell_id)
# coord_app_geojson <- geom[geom$name %in% uniq_cells,]
#
# coord_app_geojson <- geojson_json(coord_app_geojson)

# ------------------------------ #
#### Environmental data uniquement pour les cellules dans le prototype du TdeB ####
# ---------------------------- #

meteoCELLS <- list.files(path = "/home/claire/PostDoc_COLEO/shiny_site_explo/TdeB/data_meteo_cells/", full.names = TRUE)
nb <- nchar("/home/claire/PostDoc_COLEO/shiny_site_explo/TdeB/data_meteo_cells//") # For counting the nimber of characters in the path = 67
#lapply(funRcoleo, source)

meteoCELLSdf <- data.frame()
for(i in 1:length(meteoCELLS)){
  cell_id <- substr(meteoCELLS[[i]], start = nb+1, stop = nb+3)# j+1 / j+3
  indic_meteo <- substr(meteoCELLS[[i]], start = nb+5, stop = nb+8) # j+5 / j+8
  tab <- readr::read_csv(meteoCELLS[[i]], col_names = FALSE, skip = 2)
  tab$cell_id <- cell_id
  tab$indic_meteo <- indic_meteo

  meteoCELLSdf <- rbind(meteoCELLSdf, tab)
}

names(meteoCELLSdf)[c(1, 2)] <- c("Month", "Value")
meteoCELLSdf$indic_meteo[meteoCELLSdf$indic_meteo == "Mean"] <- "Temp"

meteoCELLSdf$Month <- as.factor(meteoCELLSdf$Month)
meteoCELLSdf$cell_id <- as.numeric(meteoCELLSdf$cell_id)
meteoCELLSdf$indic_meteo <- as.factor(meteoCELLSdf$indic_meteo)




setwd("/home/claire/PostDoc_COLEO/shiny_site_explo/TdeB/data_ouranos")

# sf objects
reg <- geojson_sf("/home/claire/PostDoc_COLEO/shiny_site_explo/TdeB/data_ouranos/regions_simplified_Ouranos.geojson") # régions du Québec modifiées par Ouranos
cellJSON <- geojsonio::geojson_sf("/home/claire/PostDoc_COLEO/shiny_site_explo/TdeB/cellsCOORD.geojson") # toutes les cellules existantes dans COLEO
cellSHINY <- geojsonio::geojson_sf("/home/claire/PostDoc_COLEO/shiny_site_explo/TdeB/ShinycellsCOORD.geojson") # Cellules actuellement utilisées dans le TdeB "description des sites"


plot(st_geometry(reg), col = "white", border = "grey")
plot(st_geometry(cellSHINY), border = "darkorange" , add = TRUE)
#
# class(reg)
# class(cellJSON)
# class(cellSHINY)

st_is_valid(reg)
#st_intersects(reg, cellJSON)
#j <- st_intersects(reg, cellSHINY)


st_centroid(cellSHINY)

plot(st_geometry(st_centroid(cellSHINY)),col = "red" , add = TRUE)

cent <- st_intersects(reg, st_centroid(cellSHINY))

RegCellsShiny <- data.frame()
for (i in 1:length(cent)){
  if(length(cent[[i]]) != 0){
    c <- data.frame(id = rep(i, length(cent[[i]])), cell_num = cent[[i]])
    RegCellsShiny <- rbind(RegCellsShiny, c)
  }
}

RegCellsShiny <- dplyr::left_join(RegCellsShiny, reg[, c(2, 4)], by = "id")
RegCellsShiny <- RegCellsShiny[,-4]

#cellSHINY <- as.data.frame(cellSHINY)

cellSHINY$cell_num <- 1:length(cellSHINY$name)
RegCellsShiny <- dplyr::left_join(RegCellsShiny, cellSHINY[, -2], by = "cell_num")
RegCellsShiny <- RegCellsShiny[, -5]
names(RegCellsShiny)[4] <- "cell_id"


#### ------ Récupération des scénarios climatiques dépendemment des régions ----- ####
scenario_meteo <- data.frame()
for (i in unique(RegCellsShiny$Region)){
  files <- list.files("/home/claire/PostDoc_COLEO/shiny_site_explo/TdeB/data_ouranos",
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
#
# head(scenario_meteo)
# summary(scenario_meteo)

