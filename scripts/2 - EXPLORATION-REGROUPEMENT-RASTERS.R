
#########################################################
############ EXPLORATION ET REGROUPEMENT RASTERS
#########################################################

library(raster)
library(sf)
library(corrplot)

# importation de tous les rasters créés

alt_PNM<-raster::raster("data/rasters/alt_PNM_focal400.asc")
projection(alt_PNM)<-CRS("+init=EPSG:2154")

pente_PNM<-raster::raster("data/rasters/pente_PNM_focal400.asc")
projection(pente_PNM)<-CRS("+init=EPSG:2154")

tpi_PNM<-raster::raster("data/rasters/tpi_PNM_focal400.asc")
projection(tpi_PNM)<-CRS("+init=EPSG:2154")

rugosite_PNM<-raster::raster("data/rasters/rugosite_PNM_focal400.asc")
projection(rugosite_PNM)<-CRS("+init=EPSG:2154")

roches_PNM<-raster::raster("data/rasters/roches_PNM_focal400.asc")
projection(roches_PNM)<-CRS("+init=EPSG:2154")

pelouses_prairies_PNM<-raster::raster("data/rasters/pelouses_prairies_PNM_focal400.asc")
projection(pelouses_PNM_devoluy)<-CRS("+init=EPSG:2154")

landes_PNM<-raster::raster("data/rasters/landes_PNM_focal400.asc")
projection(landes_PNM)<-CRS("+init=EPSG:2154")

forets_PNM<-raster::raster("data/rasters/forets_PNM_focal400.asc")
projection(forets_PNM)<-CRS("+init=EPSG:2154")

dist_chemins_PNM<-raster::raster("data/rasters/distance_chemins_PNM_focal400.asc")
projection(dist_chemins_PNM)<-CRS("+init=EPSG:2154")

dist_troncon_eau_PNM<-raster::raster("data/rasters/distance_eau_PNM_focal400.asc")
projection(dist_troncon_eau_PNM)<-CRS("+init=EPSG:2154")

dist_bati_PNM<-raster::raster("data/rasters/distance_bati_PNM_focal400.asc")
projection(dist_bati_PNM)<-CRS("+init=EPSG:2154")

dist_foret_PNM<-raster::raster("data/rasters/distance_foret_PNM_focal400.asc")
projection(dist_foret_PNM)<-CRS("+init=EPSG:2154")

dist_ski_PNM<-raster::raster("data/rasters/distance_ski_PNM_focal400.asc")
projection(dist_ski_PNM)<-CRS("+init=EPSG:2154")

neige_PNM<-raster::raster("data/rasters/neige_PNM_focal400.asc")
projection(neige_PNM)<-CRS("+init=EPSG:2154")


# ajout d'un masque pour le périmètre du site d'étude

limite_pnm<-sf::st_read(dsn = "data/site", layer = "pnm_total")
maskPNM <- raster::rasterize(limite_pnm, alt_PNM, background=NA)

# découpage des couches RASTER selon l'emprise du site d'étude

alt_PNM <- mask(alt_PNM, maskPNM)
pente_PNM <- mask(pente_PNM, maskPNM)
rugosite_PNM <- mask(rugosite_PNM, maskPNM)
tpi_PNM <- mask(tpi_PNM, maskPNM)
roches_PNM <- mask(roches_PNM, maskPNM)
pelouses_prairies_PNM <- mask(pelouses_prairies_PNM, maskPNM)
landes_PNM <- mask(landes_PNM, maskPNM)
forets_PNM <- mask(forets_PNM, maskPNM)
dist_chemins_PNM <- mask(dist_chemins_PNM, maskPNM)
dist_troncon_eau_PNM <- mask(dist_troncon_eau_PNM, maskPNM)
dist_foret_PNM <- mask(dist_foret_PNM, maskPNM)
dist_ski_PNM <- mask(dist_ski_PNM, maskPNM)
dist_bati_PNM <- mask(dist_bati_PNM, maskPNM)
neige_PNM <- mask(neige_PNM, maskPNM)

# compilation des raster dans un seul fichier avec utilisation de la fonction scale pour centrer réduire les valeurs des variables

cartetot <- stack(list(alt_PNM=scale(alt_PNM), 
                       pente_PNM=scale(pente_PNM), 
                       rugosite_PNM=scale(rugosite_PNM), 
                       tpi_PNM=scale(tpi_PNM), 
                       roches_PNM=scale(roches_PNM), 
                       pelouses_prairies_PNM= scale(pelouses_prairies_PNM),
                       landes_PNM=scale(landes_PNM), 
                       forets_PNM=scale(forets_PNM), 
                       dist_chemins_PNM=scale(dist_chemins_PNM), 
                       dist_troncon_eau_PNM=scale(dist_troncon_eau_PNM),  
                       dist_foret_PNM=scale(dist_foret_PNM),
                       dist_ski_PNM=scale(dist_ski_PNM), 
                       neige_PNM=scale(neige_PNM), 
                       dist_bati_PNM=scale(dist_bati_PNM))) # tout

# ajouter ici une fonction pour rendre l'affichage plus agréable à lire !

plot(cartetot)


# test de corrélation entre les variables pour rechercher celles qui apportent la même information

cor<-raster::layerStats(cartetot,'pearson',na.rm=TRUE)
write.csv2 (cor,'outputs/correlations.csv')


# affichage de la matrice de corrélation (à exporter dans outputs)

corrplot::corrplot(cor$'pearson correlation coefficient', type="upper", order="hclust", tl.col="black", tl.srt=45)


