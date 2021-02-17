
#########################################################
############ EXPLORATION ET REGROUPEMENT RASTERS
#########################################################

library(raster)

# importation de tous les rasters créés


alt_PNM<-raster::raster("data/rasters/alt_PNM_focal400.asc")
projection(alt_devoluy)<-CRS(pcs_l93)
pente_devoluy<-raster("pente_devoluy_focal400.asc")
projection(pente_devoluy)<-CRS(pcs_l93)
tpi_devoluy<-raster("tpi_devoluy_focal400.asc")
projection(tpi_devoluy)<-CRS(pcs_l93)
rugosite_devoluy<-raster("rugosite_devoluy_focal400.asc")
projection(rugosite_devoluy)<-CRS(pcs_l93)
roches_devoluy<-raster("roches_devoluy_focal400.asc")
projection(roches_devoluy)<-CRS(pcs_l93)
pelouses_prairies_devoluy<-raster("pelouses_prairies_devoluy_focal400.asc")
projection(pelouses_prairies_devoluy)<-CRS(pcs_l93)
landes_devoluy<-raster("landes_devoluy_focal400.asc")
projection(landes_devoluy)<-CRS(pcs_l93)
forets_devoluy<-raster("forets_devoluy_focal400.asc")
projection(forets_devoluy)<-CRS(pcs_l93)
#dist_carrossables_devoluy<-raster("distance_routes_devoluy_focal400.asc")
#projection(dist_carrossables_devoluy)<-CRS(pcs_l93)
dist_chemins_devoluy<-raster("distance_chemins_devoluy_focal400.asc")
projection(dist_chemins_devoluy)<-CRS(pcs_l93)
dist_troncon_eau_devoluy<-raster("distance_eau_devoluy_focal400.asc")
projection(dist_troncon_eau_devoluy)<-CRS(pcs_l93)
dist_bati_devoluy<-raster("distance_bati_devoluy_focal400.asc")
projection(dist_bati_devoluy)<-CRS(pcs_l93)
dist_foret_devoluy<-raster("distance_foret_devoluy_focal400.asc")
projection(dist_foret_devoluy)<-CRS(pcs_l93)
dist_ski_devoluy<-raster("distance_ski_devoluy_focal400.asc")
projection(dist_ski_devoluy)<-CRS(pcs_l93)
T_hiver_devoluy<-raster("T_hiver_devoluy_focal400.asc")
projection(T_hiver_devoluy)<-CRS(pcs_l93)
neige_devoluy<-raster("neige_devoluy_focal400.asc")
projection(neige_devoluy)<-CRS(pcs_l93)

# ajouter étape masques


cartetot <- stack(list(alt_devoluy=scale(alt_devoluy), 
                       pente_devoluy=scale(pente_devoluy), 
                       rugosite_devoluy=scale(rugosite_devoluy), 
                       tpi_devoluy=scale(tpi_devoluy), 
                       roches_devoluy=scale(roches_devoluy), 
                       pelouses_prairies_devoluy= scale(pelouses_prairies_devoluy),
                       landes_devoluy=scale(landes_devoluy), 
                       forets_devoluy=scale(forets_devoluy), 
                       dist_chemins_devoluy=scale(dist_chemins_devoluy), 
                       dist_troncon_eau_devoluy=scale(dist_troncon_eau_devoluy),  
                       dist_foret_devoluy=scale(dist_foret_devoluy),
                       dist_ski_devoluy=scale(dist_ski_devoluy), 
                       T_hiver_devoluy=scale(T_hiver_devoluy), 
                       neige_devoluy=scale(neige_devoluy), 
                       dist_bati_devoluy=scale(dist_bati_devoluy))) # tout
plot(cartetot)


# corrélations entre variables
cor<-layerStats(cartetot,'pearson',na.rm=TRUE)
write.csv2 (cor,'correlations.csv')