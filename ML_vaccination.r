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


dat<-read.table('/home/abp19/Leoss_data/LEOSS_Bandyopadhyay_2022-05-27/SUF_Bandyopadhyay_220527.csv', sep = ';')
col<-dat[1,]
colnames<-sapply(1:length(col), function(x) as.character(col[1,x]))

colnames(dat)<-colnames
dat<-dat[-1,]

#dat<-dat %>% filter(str_detect(BL_Vaccination, c("1","2","3","4")))
dat<-filter(dat, BL_Vaccination_Type %in% c("1","2","3","4"))

dat_uc<-dat[,grep('UC_',colnames)]
dat_co<-dat[,grep('CO_',colnames)]
dat_cr<-dat[,grep('CR_',colnames)]
dat_rc<-dat[,grep('RC_',colnames)]

dat_uc$vaccination<-dat$BL_Vaccination_Type
dat_co$vaccination<-dat$BL_Vaccination_Type
dat_cr$vaccination<-dat$BL_Vaccination_Type
dat_rc$vaccination<-dat$BL_Vaccination_Type

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


emp_data_co<-matrix('NA', nrow=12000, ncol = length(imp_lab_params)+1)
j<-1
count<-1
for (i in imp_lab_params){
  count<-count+1
  
  print(i)
  clean_co<-dat_co[,c(ncol(dat_co),grep(lab_params[i],colnames(dat_co)))]
  clean_co$pid<-1:dim(clean_co)[1]
  # clean_co<-clean_co[-c(which(clean_co[,2] %in% remove[[j]])),]
  clean_co<-clean_co[-c(which(clean_co[,2] %in% c('*',get(paste0(lab_params[i],'_nan'))))),]
  colnames(clean_co)[2]<-c( lab_params[i])
  clean_co$Cond<-rep('CO', nrow(clean_co))
  print(paste('length co', dim(clean_co)[1]))
  row<-nrow(clean_co)
  
  if (i==1){
    emp_data_co[1:row,1]<-clean_co$vaccination
    # emp_data_co[1:row,2]<-as.numeric(as.character(clean_co[,1]))
    emp_data_co[1:row,2]<-clean_co[,2]
  }else {
    # emp_data_co[1:row,count]<-as.numeric(as.character(clean_co[,1]))
    emp_data_co[1:row,count]<-clean_co[,1]
  }
  
  
}

emp_data_co<-data.frame(emp_data_co)
colnames(emp_data_co)<-c('vaccination',lab_params[imp_lab_params])
emp_data_co<-emp_data_co[1:100,]
emp_data_co<-na.omit(emp_data_co)



for (i in 2:20){
  emp_data_co[,i]<-factor(emp_data_co[,i],levels=1:10)
  print(c(i,length(which(is.na(emp_data_co[,i])))*100/600))
}
emp_data_co[,1]<-factor(emp_data_co[,1],levels=1:4)
plot_na_pareto(emp_data_co, only_na = TRUE)


emp_data_co<-emp_data_co[,-c(7,8)]


############### Imputation
for (i in 2:ncol(emp_data_co)){
  print(i)
  if (length(which(is.na(emp_data_co[,i])))>0){
    emp_data_co[,i]<-imputate_na(emp_data_co, colnames(emp_data_co)[i], method = "mice")
  }

}

######################

TrainingIndex <- createDataPartition(emp_data_co$vaccination, p=0.7, list = FALSE)
TrainingSet <- emp_data_co[TrainingIndex,] # Training Set
TestingSet <- emp_data_co[-TrainingIndex,] # Test Set


###############################
# SVM model (polynomial kernel)
train_control <- trainControl(
  method = "repeatedcv"
  , number = 10
  , repeats = 3
)

Model_svm <- train(vaccination ~ ., data = TrainingSet,
                   method = "svmPoly",
                   na.action = na.omit,
                   #preProcess=c("scale","center"),
                   trControl= train_control,#trainControl(method="none"),
                   tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Apply model for prediction
Model.training <-predict(Model_svm, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model_svm, TestingSet) # Apply model to make prediction on Testing set
# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$vaccination)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$vaccination)
#Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Cond)

print(Model.training.confusion)
print(Model.testing.confusion)
#print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model_svm)
#plot(Importance)
plot(Importance, col = "red", Main='SVMPoly')










############################## analysis of CR

emp_data_cr<-matrix('NA', nrow=12000, ncol = length(imp_lab_params)+1)
j<-1
count<-1
for (i in imp_lab_params){
  count<-count+1
  
  print(i)
  
  
  clean_cr<-dat_cr[,c(ncol(dat_cr),grep(lab_params[i],colnames(dat_cr)))]
  clean_cr$pid<-1:dim(clean_cr)[1]
  # clean_cr<-clean_cr[-c(which(clean_cr[,2] %in% remove[[j]])),]
  clean_cr<-clean_cr[-c(which(clean_cr[,2] %in% c('*',get(paste0(lab_params[i],'_nan'))))),]
  colnames(clean_cr)[2]<-c( lab_params[i])
  clean_cr$Cond<-rep('CR', nrow(clean_cr))
  print(paste('length cr', dim(clean_cr)[1]))
  row<-nrow(clean_cr)
  
  if (i==1){
    emp_data_cr[1:row,1]<-clean_cr$vaccination
    # emp_data_cr[1:row,2]<-as.numeric(as.character(clean_cr[,1]))
    emp_data_cr[1:row,2]<-clean_cr[,2]
  }else {
    # emp_data_cr[1:row,count]<-as.numeric(as.character(clean_cr[,1]))
    emp_data_cr[1:row,count]<-clean_cr[,1]
  }
  
  
}


emp_data_cr<-data.frame(emp_data_cr)
colnames(emp_data_cr)<-c('vaccination',lab_params[imp_lab_params])

emp_data_cr<-emp_data_cr[1:600,]
emp_data_cr<-na.omit(emp_data_cr)

############## very low dataset