library(sp)
library(maptools)
library(rgeos)
library(raster)
library(rgdal)
# library(spatial.tools) : package supprimé en Avril 2020
library(ncdf4)
library(biomod2)
library(spdep)
library(tiff)
library(gdalUtils)
library(gdalUtilities) # en remplacement de spatial.tools pour rééchantillonnage raster (info Cyril)
library(sf)


pcs_l93 <- "+init=EPSG:2154" # système de coordonnées projetées utilisé pour toute l'analyse

# chercher l'emprise de la zone d'après un shape 
shp_zone <- readOGR(dsn="C:/Thibaut/SIG/Devoluy", layer="emprise_devoluy") # ouverture du fichier avec les limites de la zone d'étude étendue (rectangulaire).
shp_zone<-spTransform(shp_zone, crs(pcs_l93)) # changement du système de projection (dans cet exemple, la projection était déjà ok)
extent(shp_zone) # on récupère les valeurs xmin, xmax, ymin et ymax 

# création raster vide (valeurs à 0) à résolution 100m sur la zone d'étude 
zone<-raster (xmn=906984,xmx=949118,ymn=6372617,ymx=6425812,res=100) # ajustements xmin et ymax pour alignement avec les mailles définies sur le PNM.
projection(zone)<-CRS(pcs_l93) # définition du système de projection du raster
values(zone)<-0



#########################################################
############ CREATION / REECHANTILLONNAGE RASTERS
#########################################################


###################### Topographie

# création du raster "altitude" sur l'emprise "zone"
altitude<-raster("C:/Thibaut/SIG/RASTERS_ALPES/ALT_raster.tif")
alt<-projectRaster(altitude, zone) #rééchantillonnage aux mêmes propriétés (même étendue, même résolution) que le raster de réference
writeRaster (alt, filename="alt_devoluy.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final
fmobile <- focalWeight(alt, 400, type=c('circle')) # fenêtre mobile rayon de 400m.
focal <- focal(alt, w=fmobile, fun=sum)
writeRaster (focal, filename="alt_devoluy_focal400.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final


# création du raster "pente" sur l'emprise "zone"
pente<-raster("C:/Thibaut/SIG/RASTERS_ALPES/PENTE.tif")
pent<-projectRaster(pente, zone) 
writeRaster (pent, filename="pente_devoluy.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) 
fmobile <- focalWeight(pent, 400, type=c('circle')) 
focal <- focal(pent, w=fmobile, fun=sum)
writeRaster (focal, filename="pente_devoluy_focal400.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) 


# création du raster "rugosité" sur l'emprise "zone"
rugosite<-raster("C:/Thibaut/SIG/RASTERS_ALPES/RUGOSITE.tif")
rug<-projectRaster(rugosite, zone)
writeRaster (rug, filename="rugosite_devoluy.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) 
fmobile <- focalWeight(rug, 400, type=c('circle')) 
focal <- focal(rug, w=fmobile, fun=sum)
writeRaster (focal, filename="rugosite_devoluy_focal400.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final


# création du raster "TPI" sur l'emprise "zone"
TPI<-raster("C:/Thibaut/SIG/RASTERS_ALPES/TPI.tif")
tpi<-projectRaster(TPI, zone)
writeRaster (tpi, filename="tpi_devoluy.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) 
fmobile <- focalWeight(tpi, 400, type=c('circle')) 
focal <- focal(tpi, w=fmobile, fun=sum)
writeRaster (focal, filename="tpi_devoluy_focal400.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) 


###################### Occupation du sol

# création du raster "CESBIO" sur l'emprise "zone"
CESBIO<-raster("C:/Thibaut/SIG/CESBIO/2017/OCS_2017_CESBIO.tiff")
cesbio_emprise_devoluy<-crop(CESBIO, zone)
projection(cesbio_emprise_devoluy)<-CRS(pcs_l93)

#raster avec "roches"
roches<-cesbio_emprise_devoluy %in% c("45")
roches_100m<-projectRaster(roches, zone) 
fmobile <- focalWeight(roches_100m, 400, type=c('circle'))
focal <- focal(roches_100m,w=fmobile, fun=sum)
writeRaster (focal, filename="roches_devoluy_focal400.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) 

# pelouses+prairies
pelouses_prairies<-cesbio_emprise_devoluy %in% c("34","211")
pelouses_prairies_100m<-projectRaster(pelouses_prairies, zone) 
fmobile <- focalWeight(pelouses_prairies_100m, 400, type=c('circle'))
focal <- focal(pelouses_prairies_100m, w=fmobile, fun=sum)
writeRaster (focal, filename="pelouses_prairies_devoluy_focal400.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) 

# landes
landes<-cesbio_emprise_devoluy %in% c("36")
landes_100m<-projectRaster(landes, zone) 
fmobile <- focalWeight(landes_100m, 400, type=c('circle'))
focal <- focal(landes_100m, w=fmobile, fun=sum)
writeRaster (focal, filename="landes_devoluy_focal400.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final
writeRaster (landes_100m, filename="landes_devoluy.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final


# forets
forets<-cesbio_emprise_devoluy %in% c("31","32")
forets_100m<-projectRaster(forets, zone)
fmobile <- focalWeight(forets_100m, 400, type=c('circle'))
focal <- focal(forets_100m, w=fmobile, fun=sum)
writeRaster (focal, filename="forets_devoluy_focal400.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final


###################### Linéaires IGN

# création rasters distances aux routes et chemins : ici ajout du départment 26. Puis rasterisation sous cluster

carrossables_dpt0406380573 <- readOGR(dsn="C:/Thibaut/lievre/modele_niche/3PN/cluster/routesPN", layer="routes0406380573") # assemblage routes alpes 
route26 <- readOGR(dsn="C:/Thibaut/SIG/IGN", layer="ROUTE_026")
carrossables_dpt26<-route26[route26@data$NATURE %in% c('Route Ã  1 chaussÃ©e','Route Ã  2 chaussÃ©es', 'Autoroute', 'Quasi-autoroute', 'Bretelle', 'Route empierrÃ©e'),]
carrossables_dpt26ID = spChFIDs(carrossables_dpt26, paste("B", row.names(carrossables_dpt26), sep="")) # ajout d'un "B". 
carrossables_dpt26ID<-spTransform(carrossables_dpt26ID, crs(pcs_l93))
carrossables_dpt040638057326<-spRbind(carrossables_dpt0406380573, carrossables_dpt26ID) # combinaison des deux
writeOGR(carrossables_dpt040638057326, dsn="()", layer="carrossables_dpt040638057326", driver ="ESRI Shapefile")


chemins_dpt0406380573 <- readOGR(dsn="C:/Thibaut/lievre/modele_niche/3PN/cluster/cheminsPN", layer="chemins0406380573") # assemblage routes alpes 
route26 <- readOGR(dsn="C:/Thibaut/SIG/IGN", layer="ROUTE_026")
chemins_dpt26<-route26[route26@data$NATURE %in% c('Chemin','Sentier'),]
chemins_dpt26ID = spChFIDs(chemins_dpt26, paste("B", row.names(chemins_dpt26), sep="")) # ajout d'un "B". 
chemins_dpt26ID<-spTransform(chemins_dpt26ID, crs(pcs_l93))
chemins_dpt040638057326<-spRbind(chemins_dpt0406380573, chemins_dpt26ID) # combinaison des deux
writeOGR(chemins_dpt040638057326, dsn="()", layer="chemins_dpt040638057326", driver ="ESRI Shapefile")


# création raster distances aux stations de ski
ski <- readOGR(dsn="C:/Thibaut/SIG/ski", layer="contourStations")
ski<-spTransform(ski, crs(pcs_l93)) # on s'assure du bon système de projection
raster_ski<-rasterize(ski, field="dse_area", zone) # rasterisation
distance_ski<-distance(raster_ski) # distance aux stations de ski : attention, des stations hors-zone non prises en compte. 
writeRaster (distance_ski, filename="distance_ski_devoluy.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final
fmobile <- focalWeight(distance_ski, 400, type=c('circle')) # fenêtre mobile
focal <- focal(distance_ski, w=fmobile, fun=sum)
writeRaster (focal, filename="distance_ski_devoluy_focal400.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final

# création rasters distance cours d'eau : voir cluster
# création rasters distance forêts : voir cluster
# création rasters distance bâti : voir cluster


###################### climat

# températures moyennes mensuelles (source : Kloog) moyennées sur Décembre-Avril de 2000 à 2010 
T_hiver<-raster("C:/Thibaut/SIG/RASTERS_ALPES/Jules/TEMPERATURES KLOOG ET AL/T_hiver.asc")
sr <- "+proj=lcc +lat_1=46.8 +lat_0=46.8 +lon_0=0 +k_0=0.99987742 +x_0=600000 +y_0=2200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs"
projection(T_hiver)<-CRS(sr)
T_hiver_devoluy<-projectRaster(T_hiver, zone) 
writeRaster (T_hiver_devoluy, filename="T_hiver_devoluy.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final
fmobile <- focalWeight(T_hiver_devoluy, 400, type=c('circle')) # fenêtre mobile
focal <- focal(T_hiver_devoluy, w=fmobile, fun=sum)
writeRaster (focal, filename="T_hiver_devoluy_focal400.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final


# Neige issu de Modis 
neige<-raster("C:/Thibaut/SIG/RASTERS_ALPES/SnowDaysMODIS/moyenne 2000-2016.tif")
neige_devoluy<-projectRaster(neige, zone) 
writeRaster (neige_devoluy, filename="neige_devoluy.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final
fmobile <- focalWeight(neige_devoluy, 400, type=c('circle')) # fenêtre mobile
focal <- focal(neige_devoluy, w=fmobile, fun=sum)
writeRaster (focal, filename="neige_devoluy_focal400.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final


#########################################################
############ EXPLORATION ET REGROUPEMENT RASTERS
#########################################################

# importation de tous les rasters créés

alt_devoluy<-raster("alt_devoluy_focal400.asc")
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


Annexe 2 : Scripts pour l’analyse des modèles de niche (présence simple et présence-absence) 
############################################################################## 
################# BIOMOD2
############################################################################## 

########################
###### présences-simples
########################

myRespCoord<-europaeus_hiver19
myRespCoord<- coordinates(europaeus_hiver)
A<-dim(myRespCoord)
myResp <- as.numeric(rep(1,A[1]))
myRespName <- "europaeus_cartetot8"
nbrep<-10
nbpseudo<-A[1]



# BIOMOD présences seules
# 1. Formating Data
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = cartetot8,
                                     resp.xy = myRespCoord,
                                     resp.name = myRespName,
                                     PA.nb.rep = nbrep,
                                     PA.nb.absences = nbpseudo,
                                     PA.strategy = "random")


plot(myBiomodData)


# tirage des pseudo-absences selon la distance aux chemins 
Znew<-dim(myBiomodData@coord)[1]-A[1]
inv_dist_chemins_PNM<-1/(dist_chemins_PNM+1)
inv_dist_chemins_PNM[is.na(inv_dist_chemins_PNM)]<-0
cells<-which(getValues(inv_dist_chemins_PNM<1))
coord<-xyFromCell(object=inv_dist_chemins_PNM, cell=cells)
ncells<-inv_dist_chemins_PNM@ncols*inv_dist_chemins_PNM@nrows
pa_dist_chemins_PNM<-coord[sample(1:ncells,Znew,prob=inv_dist_chemins_PNM@data@values,replace=TRUE),]
myBiomodData@coord[(A[1]+1):(A[1]+Znew),]<-pa_dist_chemins_PNM


# 2. Defining Models Options using default options.
myBiomodOption <- BIOMOD_ModelingOptions(GAM=list(k=4))

# 3. Doing Modelisation
myBiomodModelOut <- BIOMOD_Modeling(myBiomodData,
                                    models = c("GAM"),
                                    models.options = myBiomodOption,
                                    NbRunEval=10,
                                    DataSplit=70,
                                    Yweights=NULL,
                                    VarImport=3,
                                    models.eval.meth = c("KAPPA","TSS","ROC"),
                                    SaveObj = TRUE )


## print a summary of modeling stuff
myBiomodModelOut
myBiomodModelEval <- get_evaluations(myBiomodModelOut) # obtenir toutes les évaluations des modèles
myBiomodModelOut@variables.importances	 # contributions des variables à la niche
myBiomodModelOut@models.evaluation		 # les critères d'évaluation du modèle

importances_variables<-get_variables_importance(myBiomodModelOut) # obtenir toutes les évaluations de modèles
write.table(importances_variables, 'importances_variables.csv', dec = ",", col.names = TRUE) 
ROC<-myBiomodModelEval["ROC","Testing.data",,,] # imprimer les scores AUC de tous les modèles sélectionnés
write.table(ROC, 'ROC.csv', dec = ",")

# 4. Creating the ensemble models (faire la moyenne de tous les modèles précédents en gardant ceux dont le ROC/AUC est supérieur à 0.75
myBiomodEM <- BIOMOD_EnsembleModeling( 
  modeling.output = myBiomodModelOut,
  chosen.models = grep('_GAM', get_built_models(myBiomodModelOut), 
                       value=TRUE),
  em.by = 'algo',
  eval.metric = c('ROC'),
  eval.metric.quality.threshold = c(0.70),
  prob.mean = TRUE,
  prob.cv = FALSE,
  prob.ci = FALSE,
  prob.ci.alpha = 0.05,
  prob.median = FALSE,
  committee.averaging = FALSE,
  prob.mean.weight = FALSE,
  prob.mean.weight.decay = 'proportional' )


# 5. Individual models projections on current environmental conditions
myBiomodProjection <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = cartetot8,
  proj.name = 'current',
  selected.models = grep('_GAM', get_built_models(
    myBiomodModelOut), value=TRUE),
  compress = FALSE,
  build.clamping.mask = FALSE)
#plot(myBiomodProjection)


# 4. Creating the ensemble projections - creer la carte de distribution moyenne de tous les modèles
myBiomodProjection_ENSEMBLE<-BIOMOD_EnsembleForecasting( projection.output = myBiomodProjection,
                                                         EM.output = myBiomodEM)

# trace la distribution moyenne prédite                            
plot(myBiomodProjection_ENSEMBLE)



# récupérer et tracer les courbes de réponse                            
myGLMs <- BIOMOD_LoadModels(myBiomodModelOut, models='GAM')
windows()

# 4.2 plot 2D response plots
myRespPlot2D <- response.plot2(models = myGLMs,
                               Data = get_formal_data(myBiomodModelOut,'expl.var'),
                               show.variables= get_formal_data(myBiomodModelOut,'expl.var.names'),
                               do.bivariate = FALSE,
                               fixed.var.metric = 'median',
                               col = c("blue", "red"),
                               legend = TRUE,
                               data_species = get_formal_data(myBiomodModelOut,'resp.var'))


########################
###### présences-absences
########################

#  préparation des données en présence-absence

# rastériser les traces -> ss-mailles prospectées (1 lorsque parcourue sur au moins 1m) 
traces <- readOGR(dsn="C:/Thibaut/SIG/PNM/lievres/data_hiver2019/final", layer="loc_traces")
long_traces<-rasterize(traces, maskzone, field='name', fun='length') # calcul de la longeur de transect par sous-maille : très long (et espace stockage insuffisant -> passer par cluster)
long_traces<-raster("C:/Thibaut/lievre/modele_niche/PNM/data2019/long_traces.asc")
projection(long_traces)<-CRS(pcs_l93)
traces <- long_traces
traces[traces == 0] <- NA
traces[traces > 0] <- 1 # mailles avec 1 si parcouru par l'observateur (même sur qqes mètres)
writeRaster (traces, filename="traces.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final

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



# BIOMOD 
# 1. Formating Data
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = cartetot8,
                                     resp.xy = myRespCoord,
                                     resp.name = myRespName)


#plot(myBiomodData)


# 2. Defining Models Options using default options.
myBiomodOption <- BIOMOD_ModelingOptions(GAM=list(k=4))

# 3. Doing Modelisation
myBiomodModelOut <- BIOMOD_Modeling(myBiomodData,
                                    models = c("GAM"),
                                    models.options = myBiomodOption,
                                    NbRunEval=100,
                                    DataSplit=70,
                                    Yweights=NULL,
                                    VarImport=3,
                                    models.eval.meth = c("KAPPA","TSS","ROC"),
                                    SaveObj = TRUE )


## print a summary of modeling stuff
myBiomodModelOut
myBiomodModelEval <- get_evaluations(myBiomodModelOut) # obenir toutes les évaluations des modèles
myBiomodModelOut@variables.importances	 # contributions des variables à la niche
myBiomodModelOut@models.evaluation		 # les critères d'évaluation du modèle

importances_variables<-get_variables_importance(myBiomodModelOut) # obtenir toutes les évaluations de modèles
write.table(importances_variables, 'importances_variables.csv', dec = ",", col.names = TRUE) 
ROC<-myBiomodModelEval["ROC","Testing.data",,,] # imprimer les scores AUC de tous les modèles sélectionnés
write.table(ROC, 'ROC.csv', dec = ",")

# 4. Creating the ensemble models (faire la moyenne de tous les modèles précédents en gardant ceux dont le ROC/AUC est supérieur à 0.75
myBiomodEM <- BIOMOD_EnsembleModeling( 
  modeling.output = myBiomodModelOut,
  chosen.models = grep('_GAM', get_built_models(myBiomodModelOut), 
                       value=TRUE),
  em.by = 'algo',
  eval.metric = c('ROC'),
  eval.metric.quality.threshold = c(0.70),
  prob.mean = TRUE,
  prob.cv = FALSE,
  prob.ci = FALSE,
  prob.ci.alpha = 0.05,
  prob.median = FALSE,
  committee.averaging = FALSE,
  prob.mean.weight = FALSE,
  prob.mean.weight.decay = 'proportional' )


# 5. Individual models projections on current environmental conditions
myBiomodProjection <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = cartetot8,
  proj.name = 'current',
  selected.models = grep('_GAM', get_built_models(
    myBiomodModelOut), value=TRUE),
  compress = FALSE,
  build.clamping.mask = FALSE)
#plot(myBiomodProjection)


# 4. Creating the ensemble projections - creer la carte de distribution moyenne de tous les modèles
myBiomodProjection_ENSEMBLE<-BIOMOD_EnsembleForecasting( projection.output = myBiomodProjection,
                                                         EM.output = myBiomodEM)

# trace la distribution moyenne prédite                            
plot(myBiomodProjection_ENSEMBLE)



# récupérer et tracer les courbes de réponse                            
myGLMs <- BIOMOD_LoadModels(myBiomodModelOut, models='GAM')
windows()

# 4.2 plot 2D response plots
myRespPlot2D <- response.plot2(models = myGLMs,
                               Data = get_formal_data(myBiomodModelOut,'expl.var'),
                               show.variables= get_formal_data(myBiomodModelOut,'expl.var.names'),
                               do.bivariate = FALSE,
                               fixed.var.metric = 'median',
                               col = c("blue", "red"),
                               legend = TRUE,
                               data_species = get_formal_data(myBiomodModelOut,'resp.var'))


# chargement des cartes de favorabilité
proj_timidus_pnm_20nov19_presabs<- raster("C:/Thibaut/lievre/modele_niche/PNM/data2019/timidusp.100/proj_current/proj_current_timidusp.100_ensemble.gri")
proj_europaeus_pnm_20nov19_presabs<- raster("C:/Thibaut/lievre/modele_niche/PNM/data2019/europaeusp.100/proj_current/proj_current_europaeusp.100_ensemble.gri")
























Annexe 3 : Code R pour l’échantillonnage des mailles à prospecter. 



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



#################################
# 7/ sélection aléatoire de mailles 
#################################

# sélection par secteur (= vallée)
vallees_pnm <- readOGR(dsn="C:/Thibaut/SIG/PNM/SIG MERCANTOUR/limite", layer="pnm_vallees")
valles_pnm_raster<-rasterize(vallees_pnm, field="id", lievres2000_pente30_seuil60) 

etape1<-lievres2000_pente30_clamp300_neige60moitie # reprendre le raster issu de l'étape 6
etape1[etape1>0]<-valles_pnm_raster[etape1>0]
plot(etape1)

TRUC <- as.data.frame(etape1,na.rm=TRUE)
TRUC$layer <- factor(TRUC$layer)
TRUC$cell <- rownames(TRUC)
LISTE <- list()
nb_tirages <- 5

for(i in 1:length(levels(TRUC$layer))){
  LVL <- levels(TRUC$layer)[i]
  SEL <- TRUC[which(TRUC$layer==LVL),]
  SAMP <- sample(SEL$cell,nb_tirages,replace = FALSE)
  LISTE <- c(LISTE,SAMP)
}

TEST <- unlist(LISTE)
TEST2 <- as.numeric(TEST)

etape2 <- etape1
etape2[]<- NA
etape2[TEST2]<-1

writeRaster (etape2, filename="mailles2km_2lievres_clamp300_pente30sup60_neige60moitie_5parsecteur_2nd tirage.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # écriture du raster final

alea<-raster("D:/Thibaut/lievre/echantillonnage/maillage2km/mailles2km_2lievres_clamp300_pente30sup60_neige60moitie_5parsecteur_2nd tirage.asc")
