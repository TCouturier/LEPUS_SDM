library(sf)
library(raster)
library(dismo)
library(mgcv)
library(boot)

pente_PNM<-raster::raster("data/rasters/pente_PNM_focal400.asc")
tpi_PNM<-raster::raster("data/rasters/tpi_PNM_focal400.asc")
pelouses_prairies_PNM<-raster::raster("data/rasters/pelouses_prairies_PNM_focal400.asc")
landes_PNM<-raster::raster("data/rasters/landes_PNM_focal400.asc")
forets_PNM<-raster::raster("data/rasters/forets_PNM_focal400.asc")
dist_troncon_eau_PNM<-raster::raster("data/rasters/distance_eau_PNM_focal400.asc")
dist_foret_PNM<-raster::raster("data/rasters/distance_foret_PNM_focal400.asc")
dist_ski_PNM<-raster::raster("data/rasters/distance_ski_PNM_focal400.asc")
neige_PNM<-raster::raster("data/rasters/neige_PNM_focal400.asc")
dist_bati_PNM<-raster::raster("data/rasters/distance_bati_PNM_focal400.asc")


variables_sdm <- stack(list(pente_PNM=scale(pente_PNM), 
                       tpi_PNM=scale(tpi_PNM), 
                       pelouses_prairies_PNM= scale(pelouses_prairies_PNM),
                       landes_PNM=scale(landes_PNM), 
                       forets_PNM=scale(forets_PNM), 
                       dist_troncon_eau_PNM=scale(dist_troncon_eau_PNM),  
                       dist_foret_PNM=scale(dist_foret_PNM),
                       dist_ski_PNM=scale(dist_ski_PNM), 
                       neige_PNM=scale(neige_PNM), 
                       dist_bati_PNM=scale(dist_bati_PNM))) 


### POURCENTAGE DE DATA POUR LE MODELE (100-SPLIT = POURCENTAGE POUR LES METRIQUES D'EVALUATION)

SPLIT <- 100

###  NB DE REITERATIONS

IT <- 2


####
########################################################## 
# prÈsences seules (on a au prÈalable sÈlectionner une seule observation par maille (pour eviter les prolËmes d'autocorrelation))
#myRespCoord<-read.csv2("./data_aleatoires_pixels_fires.csv")
myRespCoord<-sf::st_read(dsn = "data/obs_opportunistes", layer = "lepus_pnm")
#Rasteriser les point d'observation 
alt_PNM<-raster::raster("data/rasters/alt_PNM_focal400.asc")
projection(alt_PNM)<-CRS("+init=EPSG:2154")

limite_pnm<-sf::st_read(dsn = "data/site", layer = "pnm_total")
mask <- raster::rasterize(limite_pnm, alt_PNM, background=NA)
values(mask) <- 0

#Coord<-myRespCoord[,c(x_l93,y_l93)]
#myRespCoord<-SpatialPointsDataFrame(coords=Coord, data=myRespCoord, proj4string=CRS("+init=epsg:2154"))
#obs<-raster::rasterize(myRespCoord,maskPNM,field=1,background=0)


############################################################################ 
######################################boucle 
##################################################################

TAB_GAM<-data.frame(matrix(NA,nrow=ncell(mask),ncol=IT))

TAB_pente_PNM <- data.frame(matrix(NA,nrow=100,ncol=IT))
TAB_tpi_PNM <- data.frame(matrix(NA,nrow=100,ncol=IT))
TAB_pelouses_prairies_PNM <- data.frame(matrix(NA,nrow=100,ncol=IT))
TAB_landes_PNM <- data.frame(matrix(NA,nrow=100,ncol=IT))
TAB_forets_PNM <- data.frame(matrix(NA,nrow=100,ncol=IT))
TAB_dist_troncon_eau_PNM <- data.frame(matrix(NA,nrow=100,ncol=IT))
TAB_dist_foret_PNM <- data.frame(matrix(NA,nrow=100,ncol=IT))
TAB_dist_ski_PNM <- data.frame(matrix(NA,nrow=100,ncol=IT))
TAB_neige_PNM <- data.frame(matrix(NA,nrow=100,ncol=IT))
TAB_dist_bati_PNM <- data.frame(matrix(NA,nrow=100,ncol=IT))




for (i in 1:IT) {
  
  
  spl<- myRespCoord[sample(1:nrow(myRespCoord),round(nrow(myRespCoord)*SPLIT/100,0),replace=FALSE),]
  
  #####################################################################################
  #Nombre de pseudo-absences ‡ tirer alÈatoirement dans raster de zone
  A<-dim(spl)
  
  
  #Tirage alÈatoire des pseudo absences et contrÙle du nombre de pseudo absences produits
  samp1<-dismo::randomPoints(mask, A)
  cells <- cellFromXY(mask, samp1)
  length(cells)
  cells <- unique(cells)
  length(cells)
  Pseudoabsxy <- xyFromCell(mask, cells)
  
  #Extraire les valeurs des variables par cellule de pseudo-absence
  
  # .rs.unloadPackage("tidyr")
  
  data_export<-extract(variables_sdm,Pseudoabsxy)
  absences<-data_export
  especeAbsence<-rep(0,length(absences[,1]))
  data_absence<-cbind(especeAbsence,absences)
  
  #Extraire les valeurs des variables par cellule de prÈsence
  presences<-extract(variables_sdm,spl)
  espece<-rep(1,length(presences[,1]))
  data_presence<-cbind(espece,presences)
  
  #Confection d'un tableau complet avec prÈsences/absences et valeurs des variables par maille
  dataZ<-rbind(data_presence,data_absence)
  dataZ<-as.data.frame(dataZ)
  
  
  
  
  #########################################################################################################################################################################
  ##### 2- Rasters de prediction ###############
  #######################################################################
  
  
  
  ################# Ecrire le gam en fonction des variables 
  res<-mgcv::gam(espece~
             s(pente_PNM,k=4)
           +s(tpi_PNM,k=4)
           +s(pelouses_prairies_PNM,k=4)
           +s(landes_PNM,k=4)
           +s(forets_PNM,k=4)
           +s(dist_troncon_eau_PNM,k=4)
           +s(dist_foret_PNM,k=4)
           +s(dist_ski_PNM,k=4)
           +s(neige_PNM,k=4)
           +s(dist_bati_PNM,k=4)
           ,data=dataZ,family=binomial)
  
  
  
  ################## Calculer le predict
  
  newdata<-data.frame(
    pente_PNM=values(variables_sdm$pente_PNM),
    tpi_PNM=values(variables_sdm$tpi_PNM),
    pelouses_prairies_PNM=values(variables_sdm$pelouses_prairies_PNM),
    landes_PNM=values(variables_sdm$landes_PNM),
    forets_PNM=values(variables_sdm$forets_PNM),
    dist_troncon_eau_PNM=values(variables_sdm$dist_troncon_eau_PNM),
    dist_foret_PNM=values(variables_sdm$dist_foret_PNM),
    dist_ski_PNM=values(variables_sdm$dist_ski_PNM),
    neige_PNM=values(variables_sdm$neige_PNM),
    dist_bati_PNM=values(variables_sdm$dist_bati_PNM))
  
  
  
  
  pred<-predict(res,newdata=newdata,se=T)
  
  TAB_GAM[i]<-inv.logit(pred$fit)
  

  
  #########################################################################################################################################################################
  ##### 3- Courbes de réponse ###############
  #######################################################################
  
  
  newdata1<-data.frame(
    pente_PNM    =seq(from=cellStats(variables_sdm$pente_PNM,min),to=cellStats(variables_sdm$pente_PNM,max), by=(((cellStats(variables_sdm$pente_PNM,max)-cellStats(variables_sdm$pente_PNM,min))/99))),
    tpi_PNM    =rep(0,100),
    pelouses_prairies_PNM    =rep(0,100),
    landes_PNM			=rep(0,100),
    forets_PNM			=rep(0,100),
    dist_troncon_eau_PNM		=rep(0,100),
    dist_foret_PNM			=rep(0,100),
    dist_ski_PNM			=rep(0,100),
    neige_PNM			=rep(0,100),
    dist_bati_PNM			=rep(0,100))
  
  pred1<-predict(res,newdata=newdata1,se=T)
  
  
  
  #######################################################################Graphiques
  
  newdata2<-data.frame(
    pente_PNM    =rep(0,100),
    tpi_PNM    =seq(from=cellStats(variables_sdm$tpi_PNM,min),to=cellStats(variables_sdm$tpi_PNM,max), by=(((cellStats(variables_sdm$tpi_PNM,max)-cellStats(variables_sdm$tpi_PNM,min))/99))),
    pelouses_prairies_PNM    =rep(0,100),
    landes_PNM			=rep(0,100),
    forets_PNM			=rep(0,100),
    dist_troncon_eau_PNM		=rep(0,100),
    dist_foret_PNM			=rep(0,100),
    dist_ski_PNM			=rep(0,100),
    neige_PNM			=rep(0,100),
    dist_bati_PNM			=rep(0,100))
  
  pred2<-predict(res,newdata=newdata2,se=T)
  
  
  
  #######################################################################Graphiques
  
  newdata3<-data.frame(
    pente_PNM    =rep(0,100),
    tpi_PNM    =rep(0,100),
    pelouses_prairies_PNM    =seq(from=cellStats(variables_sdm$pelouses_prairies_PNM,min),to=cellStats(variables_sdm$pelouses_prairies_PNM,max), by=(((cellStats(variables_sdm$pelouses_prairies_PNM,max)-cellStats(variables_sdm$pelouses_prairies_PNM,min))/99))),
    landes_PNM			=rep(0,100),
    forets_PNM			=rep(0,100),
    dist_troncon_eau_PNM		=rep(0,100),
    dist_foret_PNM			=rep(0,100),
    dist_ski_PNM			=rep(0,100),
    neige_PNM			=rep(0,100),
    dist_bati_PNM			=rep(0,100))
  
  pred3<-predict(res,newdata=newdata3,se=T)
  
  
  #######################################################################Graphiques
  
  newdata4<-data.frame(
    pente_PNM    =rep(0,100),
    tpi_PNM    =rep(0,100),
    pelouses_prairies_PNM    =rep(0,100),
    landes_PNM			=seq(from=cellStats(variables_sdm$landes_PNM,min),to=cellStats(variables_sdm$landes_PNM,max), by=(((cellStats(variables_sdm$landes_PNM,max)-cellStats(variables_sdm$landes_PNM,min))/99))),
    forets_PNM			=rep(0,100),
    dist_troncon_eau_PNM		=rep(0,100),
    dist_foret_PNM			=rep(0,100),
    dist_ski_PNM			=rep(0,100),
    neige_PNM			=rep(0,100),
    dist_bati_PNM			=rep(0,100))
  
  pred4<-predict(res,newdata=newdata4,se=T)
  
  #######################################################################Graphiques
  
  newdata5<-data.frame(
    pente_PNM    =rep(0,100),
    tpi_PNM    =rep(0,100),
    pelouses_prairies_PNM    =rep(0,100),
    landes_PNM			=rep(0,100),
    forets_PNM			=seq(from=cellStats(variables_sdm$forets_PNM,min),to=cellStats(variables_sdm$forets_PNM,max), by=(((cellStats(variables_sdm$forets_PNM,max)-cellStats(variables_sdm$forets_PNM,min))/99))),
    dist_troncon_eau_PNM		=rep(0,100),
    dist_foret_PNM			=rep(0,100),
    dist_ski_PNM			=rep(0,100),
    neige_PNM			=rep(0,100),
    dist_bati_PNM			=rep(0,100))
  
  pred5<-predict(res,newdata=newdata5,se=T)
  
  
  #######################################################################Graphiques
  
  newdata6<-data.frame(
    pente_PNM    =rep(0,100),
    tpi_PNM    =rep(0,100),
    pelouses_prairies_PNM    =rep(0,100),
    landes_PNM			=rep(0,100),
    forets_PNM			=rep(0,100),
    dist_troncon_eau_PNM		=seq(from=cellStats(variables_sdm$dist_troncon_eau_PNM,min),to=cellStats(variables_sdm$dist_troncon_eau_PNM,max), by=(((cellStats(variables_sdm$dist_troncon_eau_PNM,max)-cellStats(variables_sdm$dist_troncon_eau_PNM,min))/99))),
    dist_foret_PNM			=rep(0,100),
    dist_ski_PNM			=rep(0,100),
    neige_PNM			=rep(0,100),
    dist_bati_PNM			=rep(0,100))
  
  pred6<-predict(res,newdata=newdata6,se=T)
  
  
  
  #######################################################################Graphiques
  
  newdata7<-data.frame(
    pente_PNM    =rep(0,100),
    tpi_PNM    =rep(0,100),
    pelouses_prairies_PNM    =rep(0,100),
    landes_PNM			=rep(0,100),
    forets_PNM			=rep(0,100),
    dist_troncon_eau_PNM		=rep(0,100),
    dist_foret_PNM			=seq(from=cellStats(variables_sdm$dist_foret_PNM,min),to=cellStats(variables_sdm$dist_foret_PNM,max), by=(((cellStats(variables_sdm$dist_foret_PNM,max)-cellStats(variables_sdm$dist_foret_PNM,min))/99))),
    dist_ski_PNM			=rep(0,100),
    neige_PNM			=rep(0,100),
    dist_bati_PNM			=rep(0,100))
  
  pred7<-predict(res,newdata=newdata7,se=T)
  #######################################################################Graphiques
  
  
  newdata8<-data.frame(
    pente_PNM    =rep(0,100),
    tpi_PNM    =rep(0,100),
    pelouses_prairies_PNM    =rep(0,100),
    landes_PNM			=rep(0,100),
    forets_PNM			=rep(0,100),
    dist_troncon_eau_PNM		=rep(0,100),
    dist_foret_PNM			=rep(0,100),
    dist_ski_PNM			=seq(from=cellStats(variables_sdm$dist_ski_PNM,min),to=cellStats(variables_sdm$dist_ski_PNM,max), by=(((cellStats(variables_sdm$dist_ski_PNM,max)-cellStats(variables_sdm$dist_ski_PNM,min))/99))),
    neige_PNM			=rep(0,100),
    dist_bati_PNM			=rep(0,100))
  
  pred8<-predict(res,newdata=newdata8,se=T)
  
  #######################################################################Graphiques
  
  
  newdata9<-data.frame(
    pente_PNM    =rep(0,100),
    tpi_PNM    =rep(0,100),
    pelouses_prairies_PNM    =rep(0,100),
    landes_PNM			=rep(0,100),
    forets_PNM			=rep(0,100),
    dist_troncon_eau_PNM		=rep(0,100),
    dist_foret_PNM			=rep(0,100),
    dist_ski_PNM			=rep(0,100),
    neige_PNM			=seq(from=cellStats(variables_sdm$neige_PNM,min),to=cellStats(variables_sdm$neige_PNM,max), by=(((cellStats(variables_sdm$neige_PNM,max)-cellStats(variables_sdm$neige_PNM,min))/99))),
    dist_bati_PNM			=rep(0,100))
  
  pred9<-predict(res,newdata=newdata9,se=T)
  
  
  ####################################################################### Graphique VIGNE
  
  newdata10<-data.frame(
    pente_PNM    =rep(0,100),
    tpi_PNM    =rep(0,100),
    pelouses_prairies_PNM    =rep(0,100),
    landes_PNM			=rep(0,100),
    forets_PNM			=rep(0,100),
    dist_troncon_eau_PNM		=rep(0,100),
    dist_foret_PNM			=rep(0,100),
    dist_ski_PNM			=rep(0,100),
    neige_PNM			=rep(0,100),
    dist_bati_PNM			=seq(from=cellStats(variables_sdm$dist_ski_PNM,min),to=cellStats(variables_sdm$dist_ski_PNM,max), by=(((cellStats(variables_sdm$dist_ski_PNM,max)-cellStats(variables_sdm$dist_ski_PNM,min))/99))))
  
  pred10<-predict(res,newdata=newdata10,se=T)
  
  
  TAB_pente_PNM[i] <- inv.logit(pred1$fit)
  TAB_tpi_PNM[i] <- inv.logit(pred2$fit)
  TAB_pelouses_prairies_PNM[i] <- inv.logit(pred3$fit)
  TAB_landes_PNM[i] <- inv.logit(pred4$fit)
  TAB_forets_PNM[i] <- inv.logit(pred5$fit)
  TAB_dist_troncon_eau_PNM[i] <- inv.logit(pred6$fit)
  TAB_dist_foret_PNM[i] <- inv.logit(pred7$fit)
  TAB_dist_ski_PNM[i] <- inv.logit(pred8$fit)
  TAB_neige_PNM[i] <- inv.logit(pred9$fit)
  TAB_dist_bati_PNM[i] <- inv.logit(pred10$fit)
  
}

dev.off()

####################################################################### 

################################ CARTO MEAN PRESENCE
################################
#######################################################################

################## R»aliser la carto des predictions par maille (pixel)

MEAN.P<-rowMeans(TAB_GAM)
prediction<-mask
values(prediction)<-as.vector(MEAN.P)
plot(prediction)

writeRaster(prediction,"Predict_GAM_MEAN.asc",format="ascii",overwrite=TRUE)




##########################################################################################################











# importer le .shp avec les présences simples. 


############################################################################## 
################# BIOMOD2
############################################################################## 

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

