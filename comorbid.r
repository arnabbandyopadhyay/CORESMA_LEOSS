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

library(readxl)
library(stringr)
library(dplyr)
library(grid)
library(gridExtra)
library(combinat)

dat<-read.table('SUF_Bandyopadhyay_220527.csv', sep = ';')
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

check_dat<-data.frame(BL_age=dat$BL_Age, new_age=dat$new_age, comorbid=pat_helth)

dat_uc<-cbind(check_dat,dat_uc)
dat_co<-cbind(check_dat,dat_co)
dat_cr<-cbind(check_dat,dat_cr)
dat_rc<-cbind(check_dat,dat_rc)

colnames(dat_rc)[grep('aPTT', colnames(dat_rc))]<-"RC_Lab_aPPT"
lab_params<-str_split(colnames(dat_co)[grep('Lab',colnames(dat_co))],'_',simplify = TRUE)[,3]

lab_params<-lab_params[-c(1,2)]

lab_params<-lab_params[c(4,5,8,9,14,15,16,17,18,19,20,22,24)]

remove<-list(c(999,1000,'Missing'), c(999,1000,'Missing'), c(999,1000,'Missing'),c(999,1000,'Missing') )


plot_list1 <- list() 
plot_list2 <- list() 


j<-1
count<-0
for (i in (1:length(lab_params))){
  emp_data<-data.frame()
  
  print(i)
  clean_uc<-dat_uc[,c(2,3,grep(lab_params[i],colnames(dat_uc)))]
  clean_uc<-clean_uc[-c(which(clean_uc[,3] %in% remove[[j]])),]
  colnames(clean_uc)<-c('Age', 'Comorbid', 'Lab')
  clean_uc$Cond<-rep('UC', nrow(clean_uc))
  clean_uc$Param<-rep(lab_params[i], nrow(clean_uc))
  
  clean_co<-dat_co[,c(2,3,grep(lab_params[i],colnames(dat_co)))]
  clean_co<-clean_co[-c(which(clean_co[,3] %in% remove[[j]])),]
  colnames(clean_co)<-c('Age','Comorbid', 'Lab')
  clean_co$Cond<-rep('CO', nrow(clean_co))
  clean_co$Param<-rep(lab_params[i], nrow(clean_co))
  
  clean_cr<-dat_cr[,c(2,3,grep(lab_params[i],colnames(dat_cr)))]
  clean_cr<-clean_cr[-c(which(clean_cr[,3] %in% remove[[j]])),]
  colnames(clean_cr)<-c('Age','Comorbid', 'Lab')
  clean_cr$Cond<-rep('CR', nrow(clean_cr))
  clean_cr$Param<-rep(lab_params[i], nrow(clean_cr))
  
  clean_rc<-dat_rc[,c(2,3,grep(lab_params[i],colnames(dat_rc)))]
  clean_rc<-clean_rc[-c(which(clean_rc[,3] %in% remove[[j]])),]
  colnames(clean_rc)<-c('Age','Comorbid', 'Lab')
  clean_rc$Cond<-rep('RC', nrow(clean_rc))
  clean_rc$Param<-rep(lab_params[i], nrow(clean_rc))
  
  emp_data<-rbind(emp_data,clean_uc, clean_co, clean_cr, clean_rc)
  
  emp_data$Lab<-as.numeric(emp_data$Lab)
  emp_data<-emp_data[!emp_data$Age=='*',]
  emp_data<-emp_data[emp_data$Cond=='CR',]
  #emp_data<-emp_data[emp_data$Age=='0-25',]
  
  
  
  
  my_comparisons <- list( c("Healthy", 'Comorbid'))
  
  
  if (dim(emp_data)[1]>0){
    count=count+1
    plot_list1[[count]] <- ggboxplot(emp_data, x = 'Comorbid', y = "Lab", add = "jitter",
                                     color = "Cond", palette = "jco")+ rotate_x_text(angle = 45)+
      stat_compare_means(comparisons = my_comparisons, method = "wilcox.test")+theme(strip.text.x = element_text(size = 14,color='red',face = 'bold'),
                                                                                     strip.text.y = element_text(size = 16,color='red',face = 'bold'))+facet_grid(.~Param)
    
    
    plot_list2[[count]] <- ggboxplot(emp_data, x = 'Comorbid', y = "Lab", add = "jitter",
                                     color = "Cond", palette = "jco")+ rotate_x_text(angle = 45)+
      stat_compare_means(comparisons = my_comparisons, method = "wilcox.test")+theme(strip.text.x = element_text(size = 14,color='red',face = 'bold'),
                                                                                     strip.text.y = element_text(size = 16,color='red',face = 'bold'))+facet_grid(Param ~ Age)
    
  }
  
}


pdf('comorbid_wilcox.pdf', width=20, height = 10)

tg <- textGrob("Classification by Param", gp = gpar(fontsize = 20, fontface = 'bold'))
grid.arrange(grobs=plot_list1[1:4],ncol=2, top=tg)
grid.arrange(grobs=plot_list1[5:8],ncol=2)
grid.arrange(grobs=plot_list1[9:11],ncol=2)

tg <- textGrob("Classification by Param and Age", gp = gpar(fontsize = 20, fontface = 'bold'))
grid.arrange(grobs=plot_list2[1:4],ncol=2, top=tg)
grid.arrange(grobs=plot_list2[5:8],ncol=2)
grid.arrange(grobs=plot_list2[9:11],ncol=2)
dev.off()




