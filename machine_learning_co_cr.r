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


pdf('ML_CO_CR.pdf')

# files<-list.files("/home/abp19/Leoss_data/dlookr/R")
# sapply(files,source,.GlobalEnv)

dat<-read.table('/home/abp19/Leoss_data/LEOSS_Bandyopadhyay_2022-05-27/SUF_Bandyopadhyay_220527.csv', sep = ';')
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

check_dat<-data.frame(BL_age=dat$BL_Age, new_age=dat$new_age)

dat_uc<-cbind(check_dat,dat_uc)
dat_co<-cbind(check_dat,dat_co)
dat_cr<-cbind(check_dat,dat_cr)
dat_rc<-cbind(check_dat,dat_rc)

colnames(dat_rc)[grep('aPTT', colnames(dat_rc))]<-"RC_Lab_aPPT"

######### plotting lab parameters

lab_params<-str_split(colnames(dat_co)[grep('Lab',colnames(dat_co))],'_',simplify = TRUE)[,3]
lab_params[c(1,2)]<-c('GOT_AST','GPT_ALT')
#lab_params<-lab_params[-c(1,2)]

#lab_params<-lab_params[c(4,5,8,9,14,15,16,17,18,19,20,22,24)]

#colnames(dat_co)[grep('Lab',colnames(dat_co))]


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


emp_data_co<-matrix('NA', nrow=12179, ncol = length(imp_lab_params)+1)
j<-1
count<-1
for (i in imp_lab_params){
  count<-count+1
  
  print(i)
  # clean_uc<-dat_uc[,c(2,grep(lab_params[i],colnames(dat_uc)))]
  # #  clean_uc<-clean_uc[-c(which(clean_uc[,2] %in% remove[[j]])),]
  # clean_uc<-clean_uc[-c(which(clean_uc[,2] %in% get(paste0(lab_params[i],'_nan')))),2]
  # colnames(clean_uc)<-c(lab_params[i])
  # clean_uc$Cond<-rep('UC', nrow(clean_uc))
  # print(paste('length uc', dim(clean_uc)[1]))
  # 
  
  clean_co<-dat_co[,c(2,grep(lab_params[i],colnames(dat_co)))]
  clean_co$pid<-1:dim(clean_co)[1]
  # clean_co<-clean_co[-c(which(clean_co[,2] %in% remove[[j]])),]
  clean_co<-clean_co[-c(which(clean_co[,2] %in% get(paste0(lab_params[i],'_nan')))),c(2,3)]
  colnames(clean_co)[1]<-c( lab_params[i])
  clean_co$Cond<-rep('CO', nrow(clean_co))
  print(paste('length co', dim(clean_co)[1]))
 
  
  # clean_cr<-dat_cr[,c(2,grep(lab_params[i],colnames(dat_cr)))]
  # clean_cr$pid<-1:dim(clean_cr)[1]
  # # clean_cr<-clean_cr[-c(which(clean_cr[,2] %in% remove[[j]])),]
  # clean_cr<-clean_cr[-c(which(clean_cr[,2] %in% get(paste0(lab_params[i],'_nan')))),c(2,3)]
  # colnames(clean_cr)[1]<-c( lab_params[i])
  # clean_cr$Cond<-rep('CR', nrow(clean_cr))
  # print(paste('length cr', dim(clean_cr)[1]))
 
  
  # clean_rc<-dat_rc[,c(2,grep(lab_params[i],colnames(dat_rc)))]
  # # clean_rc<-clean_rc[-c(which(clean_rc[,2] %in% remove[[j]])),]
  # clean_rc<-clean_rc[-c(which(clean_rc[,2] %in% get(paste0(lab_params[i],'_nan')))),]
  # colnames(clean_rc)<-c('Age', lab_params[i])
  # clean_rc$Cond<-rep('RC', nrow(clean_rc))
  # 
  # emp_data<-merge(data.frame(clean_uc,row.names=NULL), data.frame(clean_co,row.names=NULL), data.frame(clean_cr,row.names = NULL), 
  #                 data.frame(clean_rc,row.names = NULL), by = 0, all = TRUE)[-1]
  #emp_data<-rbind(emp_data, clean_co)
  row<-nrow(clean_co)
  
  if (i==1){
    emp_data_co[,1]<-clean_co$Cond[1]
    # emp_data_co[1:row,2]<-as.numeric(as.character(clean_co[,1]))
    emp_data_co[1:row,2]<-clean_co[,1]
  }else {
    # emp_data_co[1:row,count]<-as.numeric(as.character(clean_co[,1]))
    emp_data_co[1:row,count]<-clean_co[,1]
  }
  
  
}

emp_data_co<-data.frame(emp_data_co)
colnames(emp_data_co)<-c('Cond',lab_params[imp_lab_params])

emp_data_co<-emp_data_co[1:3000,]

for (i in 2:20){
  emp_data_co[,i]<-factor(emp_data_co[,i],levels=1:10)
  print(c(i,length(which(is.na(emp_data_co[,i])))))
}

plot_na_pareto(emp_data_co, only_na = TRUE)



emp_data_cr<-matrix('NA', nrow=12179, ncol = length(imp_lab_params)+1)
j<-1
count<-1
for (i in imp_lab_params){
  count<-count+1
  
  print(i)
  # clean_uc<-dat_uc[,c(2,grep(lab_params[i],colnames(dat_uc)))]
  # #  clean_uc<-clean_uc[-c(which(clean_uc[,2] %in% remove[[j]])),]
  # clean_uc<-clean_uc[-c(which(clean_uc[,2] %in% get(paste0(lab_params[i],'_nan')))),2]
  # colnames(clean_uc)<-c(lab_params[i])
  # clean_uc$Cond<-rep('UC', nrow(clean_uc))
  # print(paste('length uc', dim(clean_uc)[1]))
  # 
  
  # clean_co<-dat_co[,c(2,grep(lab_params[i],colnames(dat_co)))]
  # clean_co$pid<-1:dim(clean_co)[1]
  # # clean_co<-clean_co[-c(which(clean_co[,2] %in% remove[[j]])),]
  # clean_co<-clean_co[-c(which(clean_co[,2] %in% get(paste0(lab_params[i],'_nan')))),c(2,3)]
  # colnames(clean_co)[1]<-c( lab_params[i])
  # clean_co$Cond<-rep('CO', nrow(clean_co))
  # print(paste('length co', dim(clean_co)[1]))
  
  
  clean_cr<-dat_cr[,c(2,grep(lab_params[i],colnames(dat_cr)))]
  clean_cr$pid<-1:dim(clean_cr)[1]
  # clean_cr<-clean_cr[-c(which(clean_cr[,2] %in% remove[[j]])),]
  clean_cr<-clean_cr[-c(which(clean_cr[,2] %in% get(paste0(lab_params[i],'_nan')))),c(2,3)]
  colnames(clean_cr)[1]<-c( lab_params[i])
  clean_cr$Cond<-rep('CR', nrow(clean_cr))
  print(paste('length cr', dim(clean_cr)[1]))
  
  
  # clean_rc<-dat_rc[,c(2,grep(lab_params[i],colnames(dat_rc)))]
  # # clean_rc<-clean_rc[-c(which(clean_rc[,2] %in% remove[[j]])),]
  # clean_rc<-clean_rc[-c(which(clean_rc[,2] %in% get(paste0(lab_params[i],'_nan')))),]
  # colnames(clean_rc)<-c('Age', lab_params[i])
  # clean_rc$Cond<-rep('RC', nrow(clean_rc))
  # 
  # emp_data<-merge(data.frame(clean_uc,row.names=NULL), data.frame(clean_co,row.names=NULL), data.frame(clean_cr,row.names = NULL), 
  #                 data.frame(clean_rc,row.names = NULL), by = 0, all = TRUE)[-1]
  #emp_data<-rbind(emp_data, clean_co)
  row<-nrow(clean_cr)
  
  if (i==1){
    emp_data_cr[,1]<-clean_cr$Cond[1]
    # emp_data_cr[1:row,2]<-as.numeric(as.character(clean_cr[,1]))
    emp_data_cr[1:row,2]<-clean_cr[,1]
  }else {
    # emp_data_cr[1:row,count]<-as.numeric(as.character(clean_cr[,1]))
    emp_data_cr[1:row,count]<-clean_cr[,1]
  }
  
  
}

emp_data_cr<-data.frame(emp_data_cr)
colnames(emp_data_cr)<-c('Cond',lab_params[imp_lab_params])

emp_data_cr<-emp_data_cr[1:5000,]
# 
# for (i in 2:20){
#   emp_data_cr[,i]<-factor(emp_data_cr[,i],levels=1:10)
#   print(c(i,length(which(is.na(emp_data_cr[,i])))))
# }



emp_data<-rbind(emp_data_co, emp_data_cr)
for (i in 2:20){
  emp_data[,i]<-factor(emp_data[,i],levels=1:10)
}
emp_data[,1]<-factor(emp_data[,1],levels=c('CO','CR'))


emp_data<-na.omit(emp_data)
summary(emp_data)

TrainingIndex <- createDataPartition(emp_data$Cond, p=0.8, list = FALSE)
TrainingSet <- emp_data[TrainingIndex,] # Training Set
TestingSet <- emp_data[-TrainingIndex,] # Test Set


###############################
# SVM model (polynomial kernel)
train_control <- trainControl(
  method = "repeatedcv"
  , number = 10
  , repeats = 10
)

Model_svm <- train(Cond ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= train_control,#trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)


#  # Build CV model
# Model.cv <- train(Cond ~ ., data = TrainingSet,
#                   method = "svmPoly",
#                   na.action = na.omit,
#                   preProcess=c("scale","center"),
#                   trControl= trainControl(method="cv", number=10),
#                   tuneGrid = data.frame(degree=1,scale=1,C=1)
# )


# Apply model for prediction
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

######################## method gbm

library(doParallel)
cl <- makePSOCKcluster(7)
registerDoParallel(cl)

grid <- expand.grid(
  n.trees = seq(10, 500, by = 100)
  , interaction.depth = c(4)
  , shrinkage = c(0.01, 0.05, 0.1, 0.15)
  , n.minobsinnode = c(5, 10, 15, 20)        
)
train_control <- trainControl(
  method = "repeatedcv"
  , number = 10
  , repeats = 10
)

Model_gbm <- train(Cond ~ ., data = TrainingSet,
               method = 'gbm',
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= train_control,
               tuneGrid = grid
)
myGrid <- Model_gbm$bestTune
Model_gbm <- train(Cond ~ ., data = TrainingSet,
               method = 'gbm',
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= train_control,
               tuneGrid = myGrid
)


# Apply model for prediction
Model.training <-predict(Model_gbm, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model_gbm, TestingSet) # Apply model to make prediction on Testing set

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Cond)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Cond)


## When you are done:
#stopCluster(cl)
#stopImplicitCluster()

print(Model.training.confusion)
print(Model.testing.confusion)
#print(Model.cv.confusion)

# Feature importance
kk<-summary(Model_gbm)
kk<-kk[1:20,]
ggplot(kk, aes(x=reorder(var,-rel.inf), y=rel.inf))+geom_point(color='red',size=5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('GBM')



# ####### RF
# # specify that the resampling method is 
# fit_control <- trainControl(## 10 fold CV
#   method = "cv", number =10)
# set.seed(825)
# rf_fit <- train(Cond ~ ., 
#                 data = TrainingSet, 
#                 method = "ranger",
#                 trControl = fit_control)


############## Random forest
## https://www.listendata.com/2014/11/random-forest-with-r.html
# library(randomForest)
# set.seed(71)
# rf <-randomForest(Cond~.,data=TrainingSet, ntree=500) 
# print(rf)
# floor(sqrt(ncol(TrainingSet) - 1))
# mtry <- tuneRF(TrainingSet[-1],TrainingSet$Cond, ntreeTry=500,
#                stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
# 
# best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
# print(mtry)
# print(best.m)
# 
# set.seed(71)
# rf <-randomForest(Cond~.,data=TrainingSet, mtry=best.m, importance=TRUE,ntree=500)
# print(rf)
# #Evaluate variable importance
# importance(rf)
# varImpPlot(rf)
# 
# pred1=predict(rf,type = "prob")
# library(ROCR)
# perf = prediction(pred1[,2], TrainingSet$Cond)
# # 1. Area under curve
# auc = performance(perf, "auc")
# auc
# # 2. True Positive and Negative Rate
# pred3 = performance(perf, "tpr","fpr")
# # 3. Plot the ROC curve
# plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
# abline(a=0,b=1,lwd=2,lty=2,col="gray")
# 
# Model.testing <-predict(rf, TestingSet)
# confusionMatrix(Model.testing, TestingSet$Cond)

trainctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
mtry <- sqrt(20)
tunegrid <- expand.grid(.mtry=mtry)
Model_rf <- train(Cond ~ ., data = TrainingSet, method = "rf",metric='Accuracy',tuneGrid=tunegrid, 
                 trControl = trainctrl)

resamps <- resamples(list(SVMpoly = Model_svm, RF=Model_rf, GBM = Model_gbm))
summary(resamps) 

Importance<-varImp(Model_rf)

impfeature<-Importance$importance
impfeature$variable<-rownames(impfeature)
impf<-impfeature[order(impfeature$Overall, decreasing = TRUE),]
impf<-impf[1:20,]
ggplot(impf, aes(x=reorder(variable, -Overall), y=Overall))+geom_point(color='red',size=5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+xlab('Variable')+ggtitle('Random Forest')

######### to have the dot plot remove ',summaryFunction=twoClassSummary, classProbs=T,savePredictions = T' from train control
dotplot(resamps)



ctrl <- trainControl(method="cv", summaryFunction=mnLogLoss,#twoClassSummary,
                     classProbs=T,
                     savePredictions = T)
Model_svm <- train(Cond ~ .,data=TrainingSet,method="svmPoly",trControl=ctrl)

Model_gbm <- train(Cond ~ .,data=TrainingSet,method="gbm",trControl=ctrl)

Model_rf <- train(Cond ~ .,data=TrainingSet,method="rf",trControl=ctrl)

## run MLeval
res <- evalm(list(Model_svm,Model_gbm,Model_rf),gnames=c('SVM','GBM','RF'))

dev.off()

stopCluster(cl)
