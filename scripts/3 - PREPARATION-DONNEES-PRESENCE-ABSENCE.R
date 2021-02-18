
library(sf)
library(tidyverse)

#######################
# importation et rastérisation des points d'observation lièvres après analyse génétique 
#######################


# new février 2021
lots_multiples_2020<-sf::st_read(dsn = "data/crotte", layer = "lots_multiples_2020")
lots_unitaire_2020<-sf::st_read(dsn = "data/crotte", layer = "lots_unitaires_2020")
mailles_2km_prospection_2020<-sf::st_read(dsn = "data/crotte", layer = "mailles_2km_prospection_2020")
points_collecte_lots_2020<-sf::st_read(dsn = "data/crotte", layer = "points_collecte_lots_2020")
ssmailles_200m_prospection_2020<-sf::st_read(dsn = "data/crotte", layer = "ss-mailles_200m_prospection_2020")

resultats_lots_plusieurs_crottes<-read_csv2("./data/crotte/resultats_lots_plusieurs_crottes.csv")
resultats_crottes_seules<-read_csv2("./data/crotte/resultats_crottes_seules.csv")



crottes_lots <- readOGR(dsn="C:/Thibaut/SIG/PNM/lievres/data_hiver2019/final", layer="resultat_maille_lots_crottes") # ouverture du fichier ssmailles li?vres avec lots
projection(crottes_lots)<-CRS(pcs_l93)
crotte_seule <- readOGR(dsn="C:/Thibaut/SIG/PNM/lievres/data_hiver2019/final", layer="resultat_maille_crotte_seule") # ouverture du fichier ssmailles li?vres avec juste 1 crotte
projection(crotte_seule)<-CRS(pcs_l93)
crottes_assemblees <- merge(crottes_lots, crotte_seule, by.x="id", by.y="id") # on cr?e une nouvelle couche o? les colonnes crottes issues des lots et seules sont ajout?es. 
europaeus <- vector ("integer", length=nrow(crottes_assemblees@data)) # vecteur qui sera ajout? en tant que colonne au tableau
timidus <- vector ("integer", length=nrow(crottes_assemblees@data))
crottes_assemblees@data <- cbind (crottes_assemblees@data, europaeus, timidus)

crottes_assemblees@data$lots_Concl<-as.character(crottes_assemblees@data$lots_Concl) # transformation en charact?res puis enlever les NA, sinon ?a coince dans la fonction ifelse 
crottes_assemblees@data$lots_Concl[is.na(crottes_assemblees@data$lots_Concl)] <- "Nas"
crottes_assemblees@data$X1.crotte_A<-as.character(crottes_assemblees@data$X1.crotte_A)
crottes_assemblees@data$X1.crotte_A[is.na(crottes_assemblees@data$X1.crotte_A)] <- "Nas"

for (i in 1:nrow(crottes_assemblees)) # on rassemble les conditions pour indiquer 1 lorsque timidus pr?sent dans la maille (sinon 0). 
{ 
  if (crottes_assemblees@data$lots_Concl[i] == "timidus seul" | crottes_assemblees@data$lots_Concl[i] == "2 espèces" | crottes_assemblees@data$X1.crotte_A[i] == "timidus")
    crottes_assemblees@data$timidus[i] <- 1 else crottes_assemblees@data$timidus[i] <- 0
}


for (i in 1:nrow(crottes_assemblees)) # on rassemble les conditions pour indiquer 1 lorsque europaeus pr?sent dans la maille (sinon 0). 
{ 
  if (crottes_assemblees@data$lots_Concl[i] == "europaeus seul" | crottes_assemblees@data$lots_Concl[i] == "2 espèces" | crottes_assemblees@data$X1.crotte_A[i] == "europaeus")
    crottes_assemblees@data$europaeus[i] <- 1 else crottes_assemblees@data$europaeus[i] <- 0
}

timidus_lots<-crottes_assemblees[crottes_assemblees@data$timidus==1,] # on s?lectionne que timidus
timidus_lots@data<-cbind(timidus_lots@data, 1) 
raster_timidus_lots<-rasterize(timidus_lots, field="1", zone, background=0) # rast?risation timidus ? 100mx100m. Mais attention, les lots sont regroup?s par 200x200m. Ce qui veut dire qu'il y a une "duplication" de l'info sur 4 mailles 100x100m.

europaeus_lots<-crottes_assemblees[crottes_assemblees@data$europaeus==1,]
europaeus_lots@data<-cbind(europaeus_lots@data, 1) 
raster_europaeus_lots<-rasterize(europaeus_lots, field="1", zone, background=0) # rast?risation timidus ? 100mx100m. Mais attention, les lots sont regroup?s par 200x200m. Ce qui veut dire qu'il y a une "duplication" de l'info sur 4 mailles 100x100m.


# on vire les mailles 100x100 qui ne contiennent pas de crottes (d'apres le positionnement des points de prelevements). Car les lots sont à une échelle 200x200. 
crottes <- readOGR(dsn="D:/Thibaut/SIG/PNM/lievres/data_hiver2019/final", layer="loc_prelevements")
crottes@data<-cbind(crottes@data, 1)
raster_crottes<-rasterize(crottes, field="1", zone, background=0)
writeRaster (raster_crottes, filename="crottes.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) 
raster_timidus<-raster_crottes+raster_timidus_lots # addition des 2 raster afin ensuite de virer les cellules == 1 (pas de localisation de crottes)
raster_timidus[raster_timidus==1]<-0
raster_timidus[raster_timidus==2]<-1
writeRaster (raster_timidus, filename="timidus_bon.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # ?criture du raster final
raster_europaeus<-raster_crottes+raster_europaeus_lots
raster_europaeus[raster_europaeus==1]<-0
raster_europaeus[raster_europaeus==2]<-1

# penser ? v?rifier ce qui se passe en cas de lots o? les analyses n'ont pas fonctionn?... Ils ne figurent pas dans les fichiers crottes_lot et crottes_seule ? Si oui, ils devraient ?tre automatiquement ?limin?s du raster final (car prennent valeur 1). 13 crottes non assign?es dans le ficher 1 crotte et 2 lots sans assignation dans le fichier lot. 

# on convertit le raster en polygones afin de recuperer les centroides des mailles avec pr?sence de chaque esp?ce. 
timidus1<-raster_timidus
timidus1[timidus1==0]<-NA
polygon_timidus<-rasterToPolygons(timidus1)
timidus_hiver<-as.data.frame(coordinates(polygon_timidus))
colnames(timidus_hiver)<-c("x","y")

europaeus1<-raster_europaeus
europaeus1[europaeus1==0]<-NA
polygon_europaeus<-rasterToPolygons(europaeus1)
writeOGR(obj = polygon_europaeus, layer = "lots_europaeus", dsn = "D:/Thibaut/lievre/modele_niche/PNM/data2019", driver = "ESRI Shapefile")
europaeus_hiver19<-coordinates(polygon_europaeus)
colnames(europaeus_hiver19)<-c("x","y")
write.csv2(europaeus_hiver ,"coord.centroides_europaeus.csv")

# plot avec les donnees de lievre sur raster
myRespCoord<-coordinates(europaeus_hiver)
A<-dim(myRespCoord)
myResp <- as.numeric(rep(1,A[1]))
myRespName <- "europaeus_hiver_neige"
plot(T_hiver_PNM, main = "Occurrences li?vre d'Europe en hiver",plot.axes = { points(myRespCoord, pch = 16,col = "orange", cex=1)}, col = grey((20:1)/22))
Pres<-myRespCoord[myResp==1,]
points(Pres, pch = 16,col = "red", cex=0.5)



#######################
# importation des traces 
#######################


# rastériser les traces -> ss-mailles prospectées (1 lorsque parcourue sur au moins 1m) 
traces <- readOGR(dsn="C:/Thibaut/SIG/PNM/lievres/data_hiver2019/final", layer="loc_traces")
long_traces<-rasterize(traces, maskzone, field='name', fun='length') # calcul de la longeur de transect par sous-maille : très long (et espace stockage insuffisant -> passer par cluster)
long_traces<-raster("C:/Thibaut/lievre/modele_niche/PNM/data2019/long_traces.asc")
projection(long_traces)<-CRS(pcs_l93)
traces <- long_traces
traces[traces == 0] <- NA
traces[traces > 0] <- 1 # mailles avec 1 si parcouru par l'observateur (même sur qqes mètres)
writeRaster (traces, filename="traces.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final



#######################
# préparation des données en présence-absence 
#######################

# on crée un dataframe avec les coordonnées des ssmailles prospectées (centroïdes) et une colonne "pres" avec 0.
polygon_traces<-rasterToPolygons(traces)
coord_traces<-as.data.frame(coordinates(polygon_traces))
A <- dim(coord_traces)
pres <- vector(length = A[1])
pres[]<-0
coord_traces <- cbind(coord_traces, pres)
colnames(coord_traces)<-c("x","y","pres")

# le tableau de coordonnées avec présences
A <- dim(timidus_hiver)
pres_timidus <- vector(length = A[1])
pres_timidus[]<-1
coord_timidus <- cbind(timidus_hiver, pres_timidus)
colnames(coord_timidus)<-c("x","y","pres")

B <- dim(europaeus_hiver19)
pres_europaeus <- vector(length = B[1])
pres_europaeus[]<-1
coord_europaeus <- cbind(europaeus_hiver19, pres_europaeus)
colnames(coord_europaeus)<-c("x","y","pres")

# on assemble les deux tableaux et on supprime les doublons (centroïdes avec lièvre et traces) en conservant  que les 1 
assemblage_timidus <- rbind(coord_timidus, coord_traces)
coord_timidus_pres<-assemblage_timidus[!duplicated(assemblage_timidus[,1:2]),]

assemblage_europaeus <- rbind(coord_europaeus, coord_traces)
coord_europaeus_pres<-assemblage_europaeus[!duplicated(assemblage_europaeus[,1:2]),]

myRespCoordp<-coord_europaeus_pres
myRespCoord<-myRespCoordp[,1:2]
myResp <- as.numeric(myRespCoordp[,3])
myRespName <- "europaeusp_100"

