
# importation et rast?risation des points d'observation li?vres variables
timidus <- readOGR(dsn="C:/Thibaut/SIG/PNM/lievres", layer="lepus_timidus") # ouverture du fichier points li?vres
projection(timidus)<-CRS(pcs_l93)
timidus<-spTransform(timidus, crs(pcs_l93)) 
timidus@data<-cbind(timidus@data, 1)
mois_obs<-substring(timidus$date_obs,6,7) # extrait des caract?res 6 et 7 du champ date, correpondant aux mois
timidus@data<-cbind(timidus@data, mois_obs)
timidus_hiver<-subset(timidus, timidus$mois_obs=="01" | timidus$mois_obs=="02" | timidus$mois_obs=="03" | timidus$mois_obs=="04" | timidus$mois_obs=="05" | timidus$mois_obs=="12") # s?lection mois d'hiver (d?cembre ? mai)
raster_timidus<-rasterize(timidus_hiver, field="1", zone, background=0)
writeRaster (raster_timidus, filename="timidus.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # ?criture du raster final

# importation et rast?risation des points d'observation li?vres d'Europe
europaeus <- readOGR(dsn="C:/Thibaut/SIG/PNM/lievres", layer="lepus_pnm") # ouverture du fichier points li?vres
europaeus<-spTransform(europaeus, crs(pcs_l93)) 
europaeus@data<-cbind(europaeus@data, 1)
mois_obs<-substring(europaeus$date_obs,6,7) # extrait des caract?res 6 et 7 du champ date, correpondant aux mois
europaeus@data<-cbind(europaeus@data, mois_obs)
europaeus_hiver<-subset(europaeus, europaeus$mois_obs=="01" | europaeus$mois_obs=="02" | europaeus$mois_obs=="03" | europaeus$mois_obs=="04" | europaeus$mois_obs=="05" | europaeus$mois_obs=="12") # s?lection mois d'hiver (d?cembre ? mai)
raster_europaeus<-rasterize(europaeus_hiver, field="1", zone, background=0)
writeRaster (raster_europaeus, filename="europaeus.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) # ?criture du raster final
