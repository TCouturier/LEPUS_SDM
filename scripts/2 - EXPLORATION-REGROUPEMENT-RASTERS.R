
#########################################################
############ EXPLORATION ET REGROUPEMENT RASTERS
#########################################################

library(raster)
library(corrplot)

# importation de tous les rasters créés

alt_PNM<-raster::raster("data/rasters/alt_PNM_focal400.asc")
projection(alt_PNM)<-CRS(pcs_l93)

pente_PNM<-raster::raster("data/rasters/pente_PNM_focal400.asc")
projection(pente_PNM)<-CRS(pcs_l93)

tpi_PNM<-raster::raster("data/rasters/tpi_PNM_focal400.asc")
projection(tpi_PNM)<-CRS(pcs_l93)

rugosite_PNM<-raster::raster("rugosite_PNM_focal400.asc")
projection(rugosite_PNM)<-CRS(pcs_l93)

roches_PNM<-raster::raster("data/rasters/data/rasters/roches_PNM_focal400.asc")
projection(roches_PNM)<-CRS(pcs_l93)

pelouses_prairies_PNM<-raster::raster("data/rasters/pelouses_prairies_PNM_focal400.asc")
projection(pelouses_PNM_devoluy)<-CRS(pcs_l93)

landes_PNM<-raster::raster("data/rasters/landes_PNM_focal400.asc")
projection(landes_PNM)<-CRS(pcs_l93)

forets_PNM<-raster::raster("data/rasters/forets_PNM_focal400.asc")
projection(forets_PNM)<-CRS(pcs_l93)

dist_chemins_PNM<-raster::raster("data/rasters/distance_chemins_PNM_focal400.asc")
projection(dist_chemins_PNM)<-CRS(pcs_l93)

dist_troncon_eau_PNM<-raster::raster("data/rasters/distance_eau_PNM_focal400.asc")
projection(dist_troncon_eau_PNM)<-CRS(pcs_l93)

dist_bati_PNM<-raster::raster("data/rasters/distance_bati_PNM_focal400.asc")
projection(dist_bati_PNM)<-CRS(pcs_l93)

dist_foret_PNM<-raster::raster("data/rasters/distance_foret_PNM_focal400.asc")
projection(dist_foret_PNM)<-CRS(pcs_l93)

dist_ski_PNM<-raster:raster("data/rasters/distance_ski_PNM_focal400.asc")
projection(dist_ski_PNM)<-CRS(pcs_l93)

T_hiver_PNM<-raster::raster("data/rasters/T_hiver_PNM_focal400.asc")
projection(T_hiver_PNM)<-CRS(pcs_l93)

neige_PNM<-raster::raster("data/rasters/neige_PNM_focal400.asc")
projection(neige_PNM)<-CRS(pcs_l93)

# ajout d'un masque pour le périmètre du site d'étude

maskzone<-raster::rasterize(pnm, zone, background=NA)
alt_PNM <- mask(alt_PNM, maskzone)


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
                       T_hiver_PNM=scale(T_hiver_PNM), 
                       neige_PNM=scale(neige_PNM), 
                       dist_bati_PNM=scale(dist_bati_PNM))) # tout
plot(cartetot)


# corrélations entre variables
#cor<-layerStats(cartetot,'pearson',na.rm=TRUE)
#write.csv2 (cor,'correlations.csv')

# fonction corrplot
corrplot::corrplot(CorTable, type="upper", order="hclust", tl.col="black", tl.srt=45)