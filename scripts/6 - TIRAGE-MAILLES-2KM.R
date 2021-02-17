
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
