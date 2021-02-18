
library(sf)
library(tidyverse)
library(mapview)
library(raster)


# facultatifs, au moins pour ces étapes/script : retirer du dossier ??
lots_multiples_2020<-sf::st_read(dsn = "data/crotte", layer = "lots_multiples_2020")
lots_unitaire_2020<-sf::st_read(dsn = "data/crotte", layer = "lots_unitaires_2020")
mailles_2km_prospection_2020<-sf::st_read(dsn = "data/crotte", layer = "mailles_2km_prospection_2020")
ssmailles_200m_prospection_2020<-sf::st_read(dsn = "data/crotte", layer = "ss-mailles_200m_prospection_2020")



#######################
# importation et rastérisation des points d'observation lièvres après analyse génétique 
#######################


# importation des données brutes (2 fichiers Antagene), assemblage et préparation 
resultats_bruts_lots_plusieurs_crottes<-read_csv2("./data/crotte/resultats_bruts_lots_plusieurs_crottes.csv")
resultats_bruts_crottes_seules<-read_csv2("./data/crotte/resultats_bruts_crottes_seules.csv")

resultats_bruts_lots_plusieurs_crottes %>%
  select(id_lot, Conclusion) %>%
  rename(espece = Conclusion) -> plusieurs_crottes

resultats_bruts_crottes_seules %>%
  dplyr::rename(espece = `Assignation espèce`) %>%
  dplyr::select(id_lot, espece) %>%
  rbind(plusieurs_crottes) %>%
  dplyr::filter(!espece %in% "aucune",
         !is.na(espece),
         !is.na(id_lot)) %>%
  dplyr::mutate(timidus = if_else (espece %in% c("timidus seul","2 espèces", "timidus") , "1", "0"),
         europaeus = if_else (espece %in% c("europaeus seul","2 espèces", "europaeus") , "1", "0"),
         timidus = as.numeric(timidus),
         europaeus = as.numeric(europaeus)) %>%
  dplyr::select(-espece) -> crottes_assemblees


# importation des points de collecte 
points_collecte_lots_2020<-sf::st_read(dsn = "data/crotte", layer = "points_collecte_lots_2020")
points_collecte_lots_2020 %>% 
  st_cast("POINT") -> points_collecte_lots_2020 # ici cette fonction car la géométrie "multipoint" n'est pas acceptée lors de la rasterisation


# jointure : attention, 28 données avec timidus et europaeus en NA (correspondent certainement à celles filtrées au-dessus, mais j'ai pas vérifié) -> supprimés
points_collecte_lots_2020 %>%
  left_join(crottes_assemblees, by = c("id" = "id_lot")) %>%
  filter(!is.na(timidus)) -> points_collecte_espece 


# on rasterise les points timidus et europaeus
raster<-raster::raster("./data/rasters/alt_PNM_focal400.asc")
projection(raster)<-CRS("+init=EPSG:2154")
raster[]<-0
raster_timidus<-rasterize(points_collecte_espece, field="timidus", raster,  background=NA)
raster_europaeus<-rasterize(points_collecte_espece, field="europaeus", raster,  background=NA)


# on convertit le raster en polygones pour recuperer les centroides des mailles avec présence de chaque espece 
raster_timidus[raster_timidus==0]<-NA
polygon_timidus<-rasterToPolygons(raster_timidus)
timidus_hiver20<-as.data.frame(coordinates(polygon_timidus))
colnames(timidus_hiver20)<-c("x","y")

raster_europaeus[raster_europaeus==0]<-NA
polygon_europaeus<-rasterToPolygons(raster_europaeus)
europaeus_hiver20<-as.data.frame(coordinates(polygon_europaeus))
colnames(europaeus_hiver20)<-c("x","y")


# write.csv2(europaeus_hiver ,"coord.centroides_europaeus.csv")


#######################
# importation des traces 
#######################


# rastériser les traces -> ss-mailles prospectées (1 lorsque parcourue sur au moins 1m) 
traces <- sf::st_read(dsn = "data/trace", layer = "traces_prospection_2020")
# long_traces<-rasterize(traces, raster, field='id_serial', fun='length') # calcul de la longeur de transect par sous-maille très long : réalisé par le cluster et intégré dans output
long_traces<-raster::raster("./outputs/long_traces.asc")
projection(long_traces)<-CRS("+init=EPSG:2154")
traces <- long_traces
traces[traces == 0] <- NA
traces[traces > 0] <- 1 # mailles avec 1 si parcouru par l'observateur (même sur qqes mètres)



#######################
# préparation des données en présence-absence 
#######################

# on crée un dataframe avec les coordonnées des mailles 100m prospectées (centroïdes) et une colonne "pres" avec 0.
polygon_traces<-rasterToPolygons(traces)
coord_traces<-as.data.frame(coordinates(polygon_traces))
pres <- vector(length = dim(coord_traces)[1])
pres[]<-0
coord_traces <- cbind(coord_traces, pres)
colnames(coord_traces)<-c("x","y","pres")

# le tableau de coordonnées avec présences
pres_timidus <- vector(length = dim(timidus_hiver20)[1])
pres_timidus[]<-1
coord_timidus <- cbind(timidus_hiver20, pres_timidus)
colnames(coord_timidus)<-c("x","y","pres")

pres_europaeus <- vector(length = dim(europaeus_hiver20)[1])
pres_europaeus[]<-1
coord_europaeus <- cbind(europaeus_hiver20, pres_europaeus)
colnames(coord_europaeus)<-c("x","y","pres")

# on assemble les deux tableaux et on supprime les doublons (centroïdes avec lièvre et traces) en ne conservant  que les 1 
assemblage_timidus <- rbind(coord_timidus, coord_traces)
coord_timidus_presabs20<-assemblage_timidus[!duplicated(assemblage_timidus[,1:2]),]

assemblage_europaeus <- rbind(coord_europaeus, coord_traces)
coord_europaeus_presabs20<-assemblage_europaeus[!duplicated(assemblage_europaeus[,1:2]),]


myRespCoordp<-coord_europaeus_presabs20
myRespCoord<-myRespCoordp[,1:2]
myResp <- as.numeric(myRespCoordp[,3])
myRespName <- "europaeuspabs"

