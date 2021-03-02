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

