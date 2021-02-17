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

