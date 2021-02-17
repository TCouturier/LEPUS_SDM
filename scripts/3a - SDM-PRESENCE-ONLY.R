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

