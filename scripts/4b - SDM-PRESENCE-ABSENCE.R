library(sf)
library(raster)
library(dismo)
library(mgcv)
library(boot)
library(biomod2)

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

SPLIT <- 70

###  NB DE REITERATIONS

IT <- 2

## Présence et absence pour les deux espèces




##############################################################
##############################################################
# BIOMOD PRESENCE ABSENCE#####################################


# Les données de raster assemblées en stack (étape 2) doivent préalablement être chargées dans l'environnement de travail. Nommé ici 'variables_sdm' 
# Il en est de même pour les données de présence-absence (étape 4b). Nommé ici 'myRespCoord' (les x et y) et 'myResp' (vecteur avec 0 ou 1). 'myRespName' est le nom donné aux fichiers/dossiers d'analyse par la suite.




# 1. Formating Data
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = variables_sdm,
                                     resp.xy = myRespCoord,
                                     resp.name = myRespName)


# sQuotePA.nb.rep = 0 # à tester (voir aide : 
# When both presence and absence data are available, and there is enough absences: set sQuotePA.nb.rep to 0. No pseudo-absence will be extracted.)

setwd("./outputs/biomod") # pour ne pas mettre le dossier biomod à la racine du projet (par défaut, pas trouvé de moyen de le spécifier)


#
plot(myBiomodData)


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

