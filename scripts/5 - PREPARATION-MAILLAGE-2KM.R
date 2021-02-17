#################################
# 1/ récupérer le raster avec les projections de distribution produit par biomod
#################################

# hiver 2018-2019
proj_timidus_pnm<- raster("D:/Thibaut/lievre/modele_niche/PNM/final/timidus.final.neige/proj_current/proj_current_timidus.final.neige_ensemble.gri")
proj_europaeus_pnm<- raster("D:/Thibaut/lievre/modele_niche/PNM/final/europaeus.final.neige/proj_current/proj_current_europaeus.final.neige_ensemble.gri")
proj_timidus_pnm_18dec18<- raster("D:/Thibaut/lievre/modele_niche/PNM/final/timidus.cartetot8/proj_current/proj_current_timidus.cartetot8_ensemble.gri")
proj_europaeus_pnm_18dec18<- raster("D:/Thibaut/lievre/modele_niche/PNM/final/europaeus.cartetot8/proj_current/proj_current_europaeus.cartetot8_ensemble.gri")
writeRaster (proj_timidus_pnm_18dec18, filename="proj_timidus_cartetot3.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final
writeRaster (proj_europaeus_pnm_18dec18, filename="proj_europaeus_cartetot3.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final

# hiver 2019-2020
proj_europaeus_pnm_20nov19<-raster("D:/Thibaut/lievre/modele_niche/PNM/data2019/europaeusp.100/proj_current/proj_current_europaeusp.100_ensemble.gri")
proj_timidus_pnm_20nov19<-raster("D:/Thibaut/lievre/modele_niche/PNM/data2019/timidusp.100/proj_current/proj_current_timidusp.100_ensemble.gri")



#################################
# 2/ générer un raster qui où l'on ne retient que les probabilités d'occurrence assez élevées (>300) chez les deux espèces (séparées puis ensemble).
#################################

clamp300_timidus<-clamp(proj_timidus_pnm_20nov19, lower=300, upper=1000, useValues=FALSE) # On récupère les cellules comprises entre 500 et 1000, le reste en NA 
writeRaster (clamp300_timidus, filename="clamp500_timidus_cartetot8.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final
clamp300_timidus<-clamp(proj_timidus_pnm_20nov19, lower=300, upper=1000, useValues=FALSE) # On récupère les cellules comprises entre 500 et 1000, le reste en NA 
writeRaster (clamp300_europaeus, filename="clamp300_europaeus_cartetot8.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final
lievres_clamp300<-clamp300_europaeus+clamp300_timidus # addition des 2 rasters -> occurrence de plus de 500 pour les deux espèces (les cellules avec NA pour l'une ou l'autre sont écartées). Gradient pouvant monter à 1700 (sans savoir laquelle des deux prédomine)
writeRaster (lievres, filename="clamp300_europaeus+timidus_presabs.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final


# D'autres niveaux testés (décembre 2019)
clamp0_timidus<-clamp(proj_timidus_pnm_20nov19, lower=00, upper=1000, useValues=FALSE) # On récupère les cellules comprises entre 0 et 1000, le reste en NA 
clamp1_europaeus<-clamp(proj_europaeus_pnm_20nov19, lower=01, upper=1000, useValues=FALSE) # On récupère les cellules comprises entre 01 et 1000, le reste en NA 

lievres_sansclamp<-proj_timidus_pnm_20nov19+proj_europaeus_pnm_20nov19 # addition des 2 rasters sans clamp selon niveau favorabilité
lievres_clamp1<-clamp1_timidus+clamp1_europaeus # addition des 2 rasters clamp1 selon niveau favorabilité

clamp200_timidus<-clamp(proj_timidus_pnm_20nov19, lower=200, upper=1000, useValues=FALSE) # On récupère les cellules comprises entre 200 et 1000, le reste en NA 
clamp200_europaeus<-clamp(proj_europaeus_pnm_20nov19, lower=200, upper=1000, useValues=FALSE) # On récupère les cellules comprises entre 200 et 1000, le reste en NA 
lievres_clamp200<-clamp200_europaeus+clamp200_timidus # addition des 2 rasters -> occurrence de plus de 400 pour les deux espèces (les cellules avec NA pour l'une ou l'autre sont écartées). Gradient pouvant monter à 1700 (sans savoir laquelle des deux prédomine)

clamp100_timidus<-clamp(proj_timidus_pnm_20nov19, lower=100, upper=1100, useValues=FALSE) # On récupère les cellules comprises entre 100 et 1000, le reste en NA 
clamp100_europaeus<-clamp(proj_europaeus_pnm_20nov19, lower=100, upper=1000, useValues=FALSE) # On récupère les cellules comprises entre 100 et 1000, le reste en NA 
lievres_clamp100<-clamp100_europaeus+clamp100_timidus # addition des 2 rasters -> occurrence de plus de 200 pour les deux espèces (les cellules avec NA pour l'une ou l'autre sont écartées). Gradient pouvant monter à 1700 (sans savoir laquelle des deux prédomine)



#################################
# 3/ convertir les mailles avec niveaux de favorabilité en 0/1 
#################################

# les deux lièvres
lievres01<-lievres_clamp100
cells1<-which(getValues(lievres01)>0)
lievres01[cells1]<-1
writeRaster (lievres01, filename="clamp300_europaeus+timidus_conversion01_cartetot8.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final

# timidus uniquement
timidus01<-clamp300_timidus
cells1<-which(getValues(timidus01)>200)
timidus01[cells1]<-1
writeRaster (timidus01, filename="clamp500_timidus_conversion01_cartetot3.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final

#################################
# 4/ aggregation sur grille 2 km 
#################################

# les 2 lièvres
lievres2000<-aggregate(lievres01, fact=c(20, 20), fun=sum)
writeRaster (lievres2000, filename="lievres_mailles2km_cartetot8.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final

# timidus seulement
timidus2000<-aggregate(timidus01, fact=c(20, 20), fun=sum)
writeRaster (timidus2000, filename="timidus_mailles2km_cartetot3.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final

#################################
# 5/ On contrôle d'autres paramètres environnementaux qui serviront à sélectionner les mailles
#################################

# contrôle pente : estimer sur chaque maille de 2km la proportion de mailles 100m avec pente < 30°(prise en compte risque d'avalanches)
pentes30<-pente_PNM
cells<-which(getValues(pentes30)>30)
pentes30[cells]<-NA
cells<-which(getValues(pentes30)<31)
pentes30[cells]<-1
mailles2km_pentes30<-aggregate(pentes30, fact=c(20, 20), fun=sum)
mailles2km_pentes30<-mailles2km_pentes30/4
writeRaster (mailles2km_pentes30, filename="mailles2km_proportion_pente_inf30.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final


# contrôle nb de jours de neige (modis) : Fixé à >60 (on garde que les mailles où l'on a des chances d'avoir de la neige en hiver)
neige60<-neige_PNM
cells<-which(getValues(neige60)<60)
neige60[cells]<-NA
cells<-which(getValues(neige60)>=60)
neige60[cells]<-1
mailles2km_neige60<-aggregate(neige60, fact=c(20, 20), fun=sum)
cells<-which(getValues(mailles2km_neige60)<200)
mailles2km_neige60[cells] <- NA
cells<-which(getValues(mailles2km_neige60)>=200)
mailles2km_neige60[cells] <- 1 # on obtient uniquement les mailles de 2km où au moins un point a un jour de neige > 60
writeRaster (mailles2km_neige60, filename="mailles2km_neige60_moitiemaille.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final


#################################
# 6/ on sélectionne mailles selon critères précédents (application de masques)
#################################

# test5 (19dec18)
pente30_seuil60 <- clamp(mailles2km_pentes30, lower=60, upper=100, useValues=FALSE) # mailles de 2km avc >60% de pente de moins de 30°
lievres2000_pente30_seuil60<- mask(lievres2000, pente30_seuil60) # parmi les mailles avec forte occurrence, on ne garde que celles avec plus de 60% de pente<30°
lievres2000_pente30_seuil60_neige60moitie<- mask(lievres2000_pente30_seuil60, mailles2km_neige60) # # parmi les mailles pré-sélectionnées, on ne garde que celles avec neige60jours sur plus de la moitié de la maille
writeRaster (lievres2000_pente30_seuil60_neige60moitie, filename="lievres2000_pente30_clamp300_neige60moitie.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final
test<-lievres2000_pente30_seuil60_neige60moitie
cells<-which(getValues(test)>=1)
test[cells]<-1
writeRaster (test, filename="lievres_pente30_clamp100_neige60moitie_01.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final


# sélection mailles clamp100 2 espèces (13dec19)
pente30_seuil60 <- clamp(mailles2km_pentes30, lower=60, upper=100, useValues=FALSE) # mailles de 2km avc >60% de pente de moins de 30°
lievres2000_pente30_seuil60<- mask(lievres2000, pente30_seuil60) # parmi les mailles avec forte occurrence, on ne garde que celles avec plus de 60% de pente<30°
lievres2000_pente30_seuil60_neige60moitie<- mask(lievres2000_pente30_seuil60, mailles2km_neige60) # # parmi les mailles pré-sélectionnées, on ne garde que celles avec neige60jours sur plus de la moitié de la maille
test<-lievres2000_pente30_seuil60_neige60moitie
cells<-which(getValues(test)>=1)
test[cells]<-1
writeRaster (test, filename="lievres_pente30_clamp100_neige60moitie_01.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final


# selection mailles clamp300 timidus (13dec19)
pente30_seuil60 <- clamp(mailles2km_pentes30, lower=60, upper=100, useValues=FALSE) # mailles de 2km avc >60% de pente de moins de 30°
timidus2000_pente30_seuil60<- mask(timidus2000, pente30_seuil60) # parmi les mailles avec forte occurrence, on ne garde que celles avec plus de 60% de pente<30°
timidus2000_pente30_seuil60_neige60moitie<- mask(timidus2000_pente30_seuil60, mailles2km_neige60) # # parmi les mailles pré-sélectionnées, on ne garde que celles avec neige60jours sur plus de la moitié de la maille
test<-timidus2000_pente30_seuil60_neige60moitie
cells<-which(getValues(test)>=1)
test[cells]<-1
writeRaster (test, filename="lievres_pente30_clamp100_neige60moitie_01.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final

