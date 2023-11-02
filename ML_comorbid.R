######### comorbidity
library(readxl)
library(stringr)
library(dplyr)
library(grid)
library(gridExtra)
library(combinat)
library(ggpubr)
library(caret)
library(ggplot2)
library(dlookr)
library(MASS)
library(MLeval)

dat<-read.table('LEOSS_Bandyopadhyay_2022-05-27/SUF_Bandyopadhyay_220527.csv', sep = ';')
col<-dat[1,]
colnames<-sapply(1:length(col), function(x) as.character(col[1,x]))

colnames(dat)<-colnames
dat<-dat[-1,]

dat_uc<-dat[,grep('UC_',colnames)]
dat_co<-dat[,grep('CO_',colnames)]
dat_cr<-dat[,grep('CR_',colnames)]
dat_rc<-dat[,grep('RC_',colnames)]

dat$new_age<-dat$BL_Age
#str_replace(dat$new_age, '[0-4]+','0-25')
dat$new_age<-gsub('^[0-5]$','0-25', dat$new_age)
dat$new_age<-gsub('^[6-7]$','26-45', dat$new_age)
dat$new_age<-gsub('^[8-9]$','46-65', dat$new_age)
dat$new_age<-gsub('^10$','65+', dat$new_age)
dat$new_age<-gsub('^11$','65+', dat$new_age)
dat$new_age<-gsub('^12$','65+', dat$new_age)
dat$new_age<-gsub('^13$','0-25', dat$new_age)
dat$new_age<-gsub('^14$','0-25', dat$new_age)


##### Comorbidity analysis ################
comorb<-c('Hemiplegia', 'Dementia', 'CerebrovasDisease','MotoneuronDisease',
          'MovementDisorder', 'MultipleSclerosis', 'MyastheniaGravis', 'NeuroAutoDiseases', 'PriorNeuroDiagnosis',
          'MyocardialInfarction', 'AorticStenosis', 'AVBlock', 'CarotidArtDisease', 'ChronicHeartFailure',
          'PeriphVasDisease', 'Hypertension', 'AtrialFibrillation', 'CoronaryArteryDisease',
          'OtherCVD', 'COPD', 'Asthma', 'ChronPulmDisease', 'Leukemia', 'Lymphoma', 'SolidTumor',
          'SolidTumorMeta', 'StemCellTranspl', 'OtherHemato', 'ConnTissueDisease', 'ChronicLiverDisease',
          'LiverCirrhosis', 'NoDamageDiabetes', 'WithDamageDiabetes', 'AcuteKidneyInjury', 'ChronicKidneyDisease',
          'OnDialysis', 'OrganTransplantation', 'RheumaticDisease', 'HivAids', 'Obesity')

# comorb<-c('MyocardialInfarction', 'AorticStenosis', 'AVBlock', 'CarotidArtDisease', 'ChronicHeartFailure',
#            'Hypertension', 'AtrialFibrillation', 'CoronaryArteryDisease',
#           'OtherCVD', 'COPD', 'Asthma', 'ChronPulmDisease', 'Leukemia', 'Lymphoma', 'OtherHemato', 
#            'ChronicLiverDisease',
#           'LiverCirrhosis', 'NoDamageDiabetes', 'WithDamageDiabetes', 'AcuteKidneyInjury', 'ChronicKidneyDisease',
#           'OnDialysis', 'OrganTransplantation', 'Obesity')

com_fn<-function(x){
  com_pt<-NULL
  cols<-unique(unlist(sapply(1:length(comorb), function(y) grep(comorb[y], colnames(x)))))
  for (i in cols){
    com_pt<-c(com_pt,which(x[,i]==1))
  }
  com_pt<-unique(com_pt)
  return(com_pt)
}

com_pt<-com_fn(dat)
pat_helth<-rep('Healthy',nrow(dat))
pat_helth[com_pt]<-'Comorbid'

check_dat<-data.frame(comorbid=pat_helth)

dat_uc<-cbind(check_dat,dat_uc)
dat_co<-cbind(check_dat,dat_co)
dat_cr<-cbind(check_dat,dat_cr)
dat_rc<-cbind(check_dat,dat_rc)


colnames(dat_rc)[grep('aPTT', colnames(dat_rc))]<-"RC_Lab_aPPT"

######### plotting lab parameters

lab_params<-str_split(colnames(dat_co)[grep('Lab',colnames(dat_co))],'_',simplify = TRUE)[,3]
lab_params[c(1,2)]<-c('GOT_AST','GPT_ALT')

GOT_AST_nan<- c(999,1000,'Missing')
GPT_ALT_nan<-c(999,1000,'Missing')
GammaGT_nan<-c(999,1000,'Missing')
Bilirubine_nan<-c(999,1000,'Missing')
Lipase_nan<-c(999,1000,'Missing')
TroponineT_nan<-c(999,1000,'Missing')
Creatinine_nan<-c(999,1000,'Missing')
Urea_nan<-c(999,1000,'Missing')
LDH_nan<-c(999,1000,'Missing')
Ddimer_nan<-c(999,1000,'Missing')
Fibrinogen_nan<-c(999,1000,'Missing')
Triglyceride_nan<-c(999,1000,'Missing')
LDL_nan<-c(999,1000,'Missing')
HDL_nan<-c(999,1000,'Missing')
SerumAlbumin_nan<-c(999,1000,'Missing')
Hemoglobin_nan<-c(12,13,'Missing')
Leukocytes_nan<-c(13,14,'Missing')
Lymphocytes_nan<-c(13,14,'Missing')
Neutrophils_nan<-c(13,14,'Missing')
Eosinophils_nan<-c(13,14,'Missing')
Platelets_nan<-c(8,9,'Missing')
CRP_nan<-c(8,9,'Missing')
PCT_nan<-c(8,9,'Missing')
IL6_nan<-c(8,9,'Missing')
sIL2R_nan<-c(8,9,'Missing')
Ferrit_nan<-c(8,9,'Missing')
INR_nan<-c(5,6,'Missing')
#aPTT_nan<-c(5,6,'Missing')
aPPT_nan<-c(5,6,'Missing')
CreatineKinase_nan<-c(6,7,'Missing')
NTproBNP_nan<-c(6,7,'Missing')
CKMB_nan<-c(4,5,'Missing')
ATIII_nan<-c(7,8,'Missing')

imp_lab_params<-c(1,2,4,6,10,11,12,15,16,17,18,20,21,22,23,26,27,28,29)



j<-1
count_uc<-count_co<-count_cr<-count_rc<-0
tk_uc<-tk_co<-tk_cr<-tk_rc<-NULL
emp_data_uc<-emp_data_co<-emp_data_cr<-emp_data_rc<-matrix('NA', nrow=12179, ncol = length(imp_lab_params)+2)

for (i in imp_lab_params){
  
  print(i)
  clean_uc<-dat_uc[,c(1,grep(lab_params[i],colnames(dat_uc)))]
  
  clean_uc$pid<-1:dim(clean_uc)[1]
  clean_uc<-clean_uc[-c(which(clean_uc[,2] %in% c('*',get(paste0(lab_params[i],'_nan'))))),]
  colnames(clean_uc)[2]<-c( lab_params[i])
  clean_uc$Cond<-rep('UC', nrow(clean_uc))
  print(paste('length uc', dim(clean_uc)[1]))
  row<-nrow(clean_uc)
  
  if(row>0){
    count_uc<-count_uc+1
    count2<-count_uc+2
    tk_uc[count_uc]<-i
    if (!emp_data_uc[1,1]==clean_uc$Cond[1]){
      emp_data_uc[,1]<-clean_uc$Cond[1]
      # emp_data_cr[1:row,2]<-as.numeric(as.character(clean_cr[,1]))
      emp_data_uc[1:row,2]<-clean_uc[,1]
      emp_data_uc[1:row,3]<-clean_uc[,2]
    }else {
      # emp_data_cr[1:row,count]<-as.numeric(as.character(clean_cr[,1]))
      emp_data_uc[1:row,count2]<-clean_uc[,2]
    }
  }
  
  
  
  clean_co<-dat_co[,c(1,grep(lab_params[i],colnames(dat_co)))]
  clean_co$pid<-1:dim(clean_co)[1]
  clean_co<-clean_co[-c(which(clean_co[,2] %in% c('*',get(paste0(lab_params[i],'_nan'))))),]
  colnames(clean_co)[2]<-c( lab_params[i])
  clean_co$Cond<-rep('CO', nrow(clean_co))
  print(paste('length CO', dim(clean_co)[1]))
  row<-nrow(clean_co)
  
  if(row>0){
    count_co<-count_co+1
    count2<-count_co+2
    tk_co[count_co]<-i
    if (!emp_data_co[1,1]==clean_co$Cond[1]){
      emp_data_co[,1]<-clean_co$Cond[1]
      # emp_data_cr[1:row,2]<-as.numeric(as.character(clean_cr[,1]))
      emp_data_co[1:row,2]<-clean_co[,1]
      emp_data_co[1:row,3]<-clean_co[,2]
    }else {
      # emp_data_cr[1:row,count]<-as.numeric(as.character(clean_cr[,1]))
      emp_data_co[1:row,count2]<-clean_co[,2]
    }
  }
  
  
  
  clean_cr<-dat_cr[,c(1,grep(lab_params[i],colnames(dat_cr)))]
  clean_cr$pid<-1:dim(clean_cr)[1]
  clean_cr<-clean_cr[-c(which(clean_cr[,2] %in% c('*',get(paste0(lab_params[i],'_nan'))))),]
  colnames(clean_cr)[2]<-c(lab_params[i])
  clean_cr$Cond<-rep('CR', nrow(clean_cr))
  print(paste('length CR', dim(clean_cr)[1]))
  row<-nrow(clean_cr)
  
  if(row>0){
    count_cr<-count_cr+1
    count2<-count_cr+2
    tk_cr[count_cr]<-i
    if (!emp_data_cr[1,1]==clean_cr$Cond[1]){
      emp_data_cr[,1]<-clean_cr$Cond[1]
      # emp_data_cr[1:row,2]<-as.numeric(as.character(clean_cr[,1]))
      emp_data_cr[1:row,2]<-clean_cr[,1]
      emp_data_cr[1:row,3]<-clean_cr[,2]
    }else {
      # emp_data_cr[1:row,count]<-as.numeric(as.character(clean_cr[,1]))
      emp_data_cr[1:row,count2]<-clean_cr[,2]
    }
  }
  
  
  clean_rc<-dat_rc[,c(1,grep(lab_params[i],colnames(dat_rc)))]
  clean_rc$pid<-1:dim(clean_rc)[1]
  clean_rc<-clean_rc[-c(which(clean_rc[,2] %in% c('*',get(paste0(lab_params[i],'_nan'))))),]
  colnames(clean_rc)[2]<-c(lab_params[i])
  clean_rc$Cond<-rep('RC', nrow(clean_rc))
  print(paste('length RC', dim(clean_rc)[1]))
  row<-nrow(clean_rc)
  
  if(row>0){
    count_rc<-count_rc+1
    count2<-count_rc+2
    tk_rc[count_rc]<-i
    if (!emp_data_rc[1,1]==clean_rc$Cond[1]){
      emp_data_rc[,1]<-clean_rc$Cond[1]
      # emp_data_cr[1:row,2]<-as.numeric(as.character(clean_cr[,1]))
      emp_data_rc[1:row,2]<-clean_rc[,1]
      emp_data_rc[1:row,3]<-clean_rc[,2]
    }else {
      # emp_data_cr[1:row,count]<-as.numeric(as.character(clean_cr[,1]))
      emp_data_rc[1:row,count2]<-clean_rc[,2]
    }
  }
  
}

emp_data_uc<-data.frame(emp_data_uc)
emp_data_rc<-data.frame(emp_data_rc)
emp_data_co<-data.frame(emp_data_co)
emp_data_cr<-data.frame(emp_data_cr)

colnames(emp_data_uc)<-c('Cond','health',lab_params[tk_uc])
colnames(emp_data_rc)<-c('Cond','health',lab_params[tk_rc])
colnames(emp_data_co)<-c('Cond','health',lab_params[tk_co])
colnames(emp_data_cr)<-c('Cond','health',lab_params[tk_cr])

emp_data_uc<-emp_data_uc[,na.omit(colnames(emp_data_uc))]
emp_data_rc<-emp_data_rc[,na.omit(colnames(emp_data_rc))]
#emp_data_co<-emp_data_co[,na.omit(colnames(emp_data_co))]
#emp_data_cr<-emp_data_cr[,na.omit(colnames(emp_data_cr))]


emp_dat<-rbind(emp_data_co, emp_data_cr)

for (i in 3:21){
  print(i)
  emp_data_co[,i]<-factor(emp_data_co[,i],levels=1:12)
}

emp_data_co[,1]<-factor(emp_data_co[,1],levels=c('CO'))
emp_data_co[,2]<-factor(emp_data_co[,2],levels=c('Healthy','Comorbid'))

emp_data_co<-emp_data_co[1:2000,-1]
emp_data_co<-emp_data_co[,-c(8)]
#emp_data_co<-na.omit(emp_data_co)
plot_na_pareto(emp_data_co, only_na = TRUE)

################ Imputation
for (i in 2:ncol(emp_data_co)){
  print(i)
  if (length(which(is.na(emp_data_co[,i])))>0){
    emp_data_co[,i]<-imputate_na(emp_data_co, colnames(emp_data_co)[i], method = "mice")
  }
  
}

TrainingIndex <- createDataPartition(emp_data_co$health, p=0.8, list = FALSE)
TrainingSet <- emp_data_co[TrainingIndex,] # Training Set
TestingSet <- emp_data_co[-TrainingIndex,] # Test Set

train_control <- trainControl(
  method = "repeatedcv"
  , number = 10
  , repeats = 10,summaryFunction=defaultSummary,#mnLogLoss,#twoClassSummary,
  classProbs=T,
  savePredictions = T
)

Model_svm <- train(health ~ ., data = TrainingSet,
                   method = "svmPoly",
                   #na.action = na.omit,
                   #preProcess=c("scale","center"),
                   trControl= train_control,#trainControl(method="none"),
                   tuneGrid = data.frame(degree=1,scale=1,C=1),
                   metric = "ROC"
)

Model.training <-predict(Model_svm, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model_svm, TestingSet) # Apply model to make prediction on Testing set
#Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$health)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$health)
#Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Cond)

print(Model.training.confusion)
print(Model.testing.confusion)
#print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model_svm)
#plot(Importance)
plot(Importance, col = "red", Main='SVMPoly')



trainctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,summaryFunction=defaultSummary,#mnLogLoss,#twoClassSummary,
                          classProbs=T,
                          savePredictions = T)
mtry <- sqrt(20)
tunegrid <- expand.grid(.mtry=mtry)
Model_rf <- train(health ~ ., data = TrainingSet, method = "rf",tuneGrid=tunegrid, 
                  trControl = trainctrl, metric = "ROC")
Model.training <-predict(Model_rf, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model_rf, TestingSet) # Apply model to make prediction on Testing set

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$health)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$health)

print(Model.training.confusion)
print(Model.testing.confusion)

resamps <- resamples(list(SVMpoly = Model_svm, RF=Model_rf))

summary(resamps) 
pdf('ML_variant_ado_co.pdf', width=10, height=5)
bwplot(resamps, layout = c(3, 1))
dev.off()


############ analysis of CR

for (i in 3:21){
  print(i)
  emp_data_cr[,i]<-factor(emp_data_cr[,i],levels=1:12)
}

emp_data_cr[,1]<-factor(emp_data_cr[,1],levels=c('CR'))
emp_data_cr[,2]<-factor(emp_data_cr[,2],levels=c('Healthy','Comorbid'))

emp_data_cr<-emp_data_cr[1:1000,-1]
#emp_data_co<-emp_data_co[,-c(8)]
#emp_data_co<-na.omit(emp_data_co)
plot_na_pareto(emp_data_cr, only_na = TRUE)

################ Imputation
for (i in 2:ncol(emp_data_cr)){
  print(i)
  if (length(which(is.na(emp_data_cr[,i])))>0){
    emp_data_cr[,i]<-imputate_na(emp_data_cr, colnames(emp_data_cr)[i], method = "mice")
  }
  
}

TrainingIndex <- createDataPartition(emp_data_cr$health, p=0.8, list = FALSE)
TrainingSet <- emp_data_co[TrainingIndex,] # Training Set
TestingSet <- emp_data_co[-TrainingIndex,] # Test Set

train_control <- trainControl(
  method = "repeatedcv"
  , number = 10
  , repeats = 10,summaryFunction=defaultSummary,#mnLogLoss,#twoClassSummary,
  classProbs=T,
  savePredictions = T
)

Model_svm <- train(health ~ ., data = TrainingSet,
                   method = "svmPoly",
                   #na.action = na.omit,
                   #preProcess=c("scale","center"),
                   trControl= train_control,#trainControl(method="none"),
                   tuneGrid = data.frame(degree=1,scale=1,C=1),
                   metric = "ROC"
)

Model.training <-predict(Model_svm, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model_svm, TestingSet) # Apply model to make prediction on Testing set
#Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$health)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$health)
#Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Cond)

print(Model.training.confusion)
print(Model.testing.confusion)
#print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model_svm)
#plot(Importance)
plot(Importance, col = "red", Main='SVMPoly')



trainctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,summaryFunction=defaultSummary,#mnLogLoss,#twoClassSummary,
                          classProbs=T,
                          savePredictions = T)
mtry <- sqrt(20)
tunegrid <- expand.grid(.mtry=mtry)
Model_rf <- train(health ~ ., data = TrainingSet, method = "rf",tuneGrid=tunegrid, 
                  trControl = trainctrl, metric = "ROC")
Model.training <-predict(Model_rf, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model_rf, TestingSet) # Apply model to make prediction on Testing set

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$health)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$health)

print(Model.training.confusion)
print(Model.testing.confusion)

resamps <- resamples(list(SVMpoly = Model_svm, RF=Model_rf))

summary(resamps) 
pdf('ML_variant_ado_co.pdf', width=10, height=5)
bwplot(resamps, layout = c(3, 1))
dev.off()

res <- evalm(list(Model_svm, Model_rf, Model_xgb),gnames=c('SVM','RF','XGB'))
res <- evalm(Model_svm,gnames=c('SVM'))


##################### Critical healthy and uncomplicated healthy

library(data.table)
hr_cr<-emp_data_cr[emp_data_cr$health=='Healthy',]
hr_cr$Cond<-"CR"
#hr_cr<-hr_cr[,-c(1)]
hr_uc<-emp_data_uc[emp_data_uc$health=='Healthy',]
hr_uc<-hr_uc[1:500,]
#plot_na_pareto(hr_cr)
l = list(hr_uc,hr_cr)
emp_data<-rbindlist(l, use.names=TRUE, fill=TRUE)
emp_data<-emp_data[,-c(2,19,20,21)]
#plot_na_pareto(emp_data)
emp_data<-data.frame(emp_data)
for(i in 2:17){
  emp_data[,i]<-factor(emp_data[,i],levels=1:12)
}

emp_data[,1]<-factor(emp_data[,1],levels=c('UC','CR'))

plot_na_pareto(emp_data)

################ Imputation
for (i in 2:ncol(emp_data)){
  print(i)
  if (length(which(is.na(emp_data[,i])))>0){
    emp_data[,i]<-imputate_na(emp_data, colnames(emp_data)[i], method = "mice")
  }
  
}

TrainingIndex <- createDataPartition(emp_data$Cond, p=0.8, list = FALSE)
TrainingSet <- emp_data[TrainingIndex,] # Training Set
TestingSet <- emp_data[-TrainingIndex,] # Test Set

train_control <- trainControl(
  method = "repeatedcv"
  , number = 10
  , repeats = 10,summaryFunction=defaultSummary,#mnLogLoss,#twoClassSummary,
  classProbs=T,
  savePredictions = T
)

Model_svm <- train(Cond ~ ., data = TrainingSet,
                   method = "svmPoly",
                   #na.action = na.omit,
                   #preProcess=c("scale","center"),
                   trControl= train_control,#trainControl(method="none"),
                   tuneGrid = data.frame(degree=1,scale=1,C=1),
                   metric = "ROC"
)

Model.training <-predict(Model_svm, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model_svm, TestingSet) # Apply model to make prediction on Testing set
#Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Cond)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Cond)
#Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Cond)

print(Model.training.confusion)
print(Model.testing.confusion)
#print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model_svm)
#plot(Importance)
plot(Importance, col = "red", Main='SVMPoly')



trainctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,summaryFunction=defaultSummary,#mnLogLoss,#twoClassSummary,
                          classProbs=T,
                          savePredictions = T)
mtry <- sqrt(20)
tunegrid <- expand.grid(.mtry=mtry)
Model_rf <- train(Cond ~ ., data = TrainingSet, method = "rf",tuneGrid=tunegrid, 
                  trControl = trainctrl, metric = "ROC")
Model.training <-predict(Model_rf, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model_rf, TestingSet) # Apply model to make prediction on Testing set

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Cond)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Cond)

print(Model.training.confusion)
print(Model.testing.confusion)

Model_xgb <- train(Cond ~ ., data = TrainingSet,
                   method = "xgbTree",
                   #na.action = na.omit,
                   #preProcess=c("scale","center"),
                   trControl= train_control,#trainControl(method="none"),
                   verbosity = 0, metric = "ROC"
                   
)

Model.training <-predict(Model_xgb, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model_xgb, TestingSet) # Apply model to make prediction on Testing set

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Cond)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Cond)

print(Model.training.confusion)
print(Model.testing.confusion)



pdf('ML_healthy_critical_vs_uncomplicated.pdf', width=10, height=5)

Importance_xgb<-varImp(Model_xgb)
impfeature_xgb<-Importance_xgb$importance
impfeature_xgb$variable<-rownames(impfeature_xgb)
impf_xgb<-na.omit(impfeature_xgb[order(impfeature_xgb, decreasing = TRUE),])
impf_xgb<-impf_xgb[1:20,]
impf_xgb$model<-'XGB'

Importance_rf<-varImp(Model_rf)
impfeature_rf<-Importance_rf$importance
impfeature_rf$variable<-rownames(impfeature_rf)
impf_rf<-impfeature_rf[order(impfeature_rf$Overall, decreasing = TRUE),]
impf_rf<-impf_rf[1:20,]
impf_rf$model<-'RF'

model_dat<-rbind(impf_xgb,impf_rf)

#pdf("classification_cr_variant.pdf", width=10)
ggplot(data=model_dat, aes(x=variable, y=Overall, fill=model)) +
  geom_bar(stat="identity", position=position_dodge())+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Importance of variable in classifying variant in CR')

resamps <- resamples(list(XGB=Model_xgb, RF=Model_rf))
pdf('new.pdf', width=10, height=5)
summary(resamps) 
bwplot(resamps, layout = c(3, 1))


res <- evalm(list(Model_xgb, Model_rf),gnames=c('XGB','RF'))
#res <- evalm(Model_svm,gnames=c('SVM'))


dev.off()

##################### Critical comorbid and uncomplicated comorbid

library(data.table)
co_cr<-emp_data_cr[emp_data_cr$health=='Comorbid',]
co_cr$Cond<-"CR"
co_cr<-co_cr[,-c(1)]
co_uc<-emp_data_uc[emp_data_uc$health=='Comorbid',]
co_uc<-co_uc[1:1300,]
#plot_na_pareto(hr_cr)
l = list(co_uc,co_cr)
emp_data<-rbindlist(l, use.names=TRUE, fill=TRUE)
emp_data<-emp_data[,-c(2,19,20,21)]
#plot_na_pareto(emp_data)
emp_data<-data.frame(emp_data)
for(i in 2:17){
  emp_data[,i]<-factor(emp_data[,i],levels=1:12)
}

emp_data[,1]<-factor(emp_data[,1],levels=c('UC','CR'))

plot_na_pareto(emp_data)

################ Imputation
for (i in 2:ncol(emp_data)){
  print(i)
  if (length(which(is.na(emp_data[,i])))>0){
    emp_data[,i]<-imputate_na(emp_data, colnames(emp_data)[i], method = "mice")
  }
  
}

TrainingIndex <- createDataPartition(emp_data$Cond, p=0.8, list = FALSE)
TrainingSet <- emp_data[TrainingIndex,] # Training Set
TestingSet <- emp_data[-TrainingIndex,] # Test Set

train_control <- trainControl(
  method = "repeatedcv"
  , number = 10
  , repeats = 10,summaryFunction=defaultSummary,#mnLogLoss,#twoClassSummary,
  classProbs=T,
  savePredictions = T
)

Model_svm <- train(Cond ~ ., data = TrainingSet,
                   method = "svmPoly",
                   #na.action = na.omit,
                   #preProcess=c("scale","center"),
                   trControl= train_control,#trainControl(method="none"),
                   tuneGrid = data.frame(degree=1,scale=1,C=1),
                   metric = "ROC"
)

Model.training <-predict(Model_svm, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model_svm, TestingSet) # Apply model to make prediction on Testing set
#Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Cond)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Cond)
#Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Cond)

print(Model.training.confusion)
print(Model.testing.confusion)
#print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model_svm)
#plot(Importance)
plot(Importance, col = "red", Main='SVMPoly')



trainctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,summaryFunction=defaultSummary,#mnLogLoss,#twoClassSummary,
                          classProbs=T,
                          savePredictions = T)
mtry <- sqrt(20)
tunegrid <- expand.grid(.mtry=mtry)
Model_rf <- train(Cond ~ ., data = TrainingSet, method = "rf",tuneGrid=tunegrid, 
                  trControl = trainctrl, metric = "ROC")
Model.training <-predict(Model_rf, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model_rf, TestingSet) # Apply model to make prediction on Testing set

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Cond)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Cond)

print(Model.training.confusion)
print(Model.testing.confusion)

Model_xgb <- train(Cond ~ ., data = TrainingSet,
                   method = "xgbTree",
                   #na.action = na.omit,
                   #preProcess=c("scale","center"),
                   trControl= train_control,#trainControl(method="none"),
                   verbosity = 0, metric = "ROC"
                   
)

Model.training <-predict(Model_xgb, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model_xgb, TestingSet) # Apply model to make prediction on Testing set

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Cond)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Cond)

print(Model.training.confusion)
print(Model.testing.confusion)



pdf('ML_comorbid_critical_vs_uncomplicated.pdf', width=10, height=5)

Importance_xgb<-varImp(Model_xgb)
impfeature_xgb<-Importance_xgb$importance
impfeature_xgb$variable<-rownames(impfeature_xgb)
impf_xgb<-na.omit(impfeature_xgb[order(impfeature_xgb, decreasing = TRUE),])
impf_xgb<-impf_xgb[1:20,]
impf_xgb$model<-'XGB'

Importance_rf<-varImp(Model_rf)
impfeature_rf<-Importance_rf$importance
impfeature_rf$variable<-rownames(impfeature_rf)
impf_rf<-impfeature_rf[order(impfeature_rf$Overall, decreasing = TRUE),]
impf_rf<-impf_rf[1:20,]
impf_rf$model<-'RF'

model_dat<-rbind(impf_xgb,impf_rf)

#pdf("classification_cr_variant.pdf", width=10)
ggplot(data=model_dat, aes(x=variable, y=Overall, fill=model)) +
  geom_bar(stat="identity", position=position_dodge())+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Importance of variable in classifying variant in CR')

resamps <- resamples(list(SVMpoly = Model_svm,XGB=Model_xgb, RF=Model_rf))

summary(resamps) 
bwplot(resamps, layout = c(3, 1))


res <- evalm(list(Model_svm, Model_xgb, Model_rf),gnames=c('SVM','XGB','RF'))
#res <- evalm(Model_svm,gnames=c('SVM'))


dev.off()
