############################################################################## 
######### divisions en sous-mailles 
############################################################################## 

setwd('D:/Thibaut/lievre/lots_prep/final_hiver2019')
mailles2km<- raster("D:/Thibaut/lievre/echantillonnage/maillage2km/19dec/lievres2000_pente30_clamp300_neige60moitie_01.asc")
mailles2km[]<-0
mailles200m<-disaggregate(mailles2km, fact=c(10, 10), fun=sum)

crottes <- readOGR(dsn="D:/Thibaut/SIG/PNM/lievres/data_hiver2019/final", layer="loc_prelevements")
traces <- readOGR(dsn="D:/Thibaut/SIG/PNM/lievres/data_hiver2019/final", layer="loc_traces")

# mailles 200m
nb_crottes<-rasterize(crottes, mailles200m, field='name', fun='count') # calcul du nombre de crottes par sous-maille
writeRaster (nb_crottes, filename="nb_crottes.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) 
long_traces<-rasterize(traces, mailles200m, field='name', fun='length') # calcul de la longeur de transect par sous-maille
writeRaster (long_traces, filename="long_traces.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) 

mailles_prosp<-mailles200m
cells<-which(getValues(long_traces)>0)
mailles_prosp[cells]<-1 # mailles avec 1 si parcouru par observateur (sinon 0)
cells<-which(getValues(mailles_prosp)==0)
mailles_prosp[cells]<-NA # mailles avec 1 si parcouru par observateur (sinon NA) -> masque pour la suite


nb_crottes0<-nb_crottes
nb_crottes0[is.na(nb_crottes0)]<-0 # nb de crottes par maille (avec 0 dans les cas o? il n'y a pas de crottes, m?me si non prospect?)
nb_crottes_prosp <- mask(nb_crottes0, mailles_prosp) # application du masque : NA lorsque non prospect?
writeRaster (nb_crottes_prosp, filename="nb_crottes_prosp.asc", proj4string = CRS("+init=epsg:32734"), overwrite=TRUE) 


cells<-which(getValues(nb_crottes_prosp)>0)
length(cells)