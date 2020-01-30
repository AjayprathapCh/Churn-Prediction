#configure working directory

getwd()
setwd("C:\\DS Full stack\\Graded Assignments\\09 - Capstone Project  and Certitication")
getwd()


#Lets load the requied packages

library(dplyr) #for data manipulation purposes
library(ROCR) #to build ROC curve & find out AUC value to validate performance of the model
library(caret) #Confussion matrix
library(irr) #Kappa matrix, for model valiadation
library(ggplot2) #for visualizations
library(gains) #Gains chart


#The data set is in the form of CSV file, lets read the data set

telchurn_dat1 <- read.csv("telecomfinal.csv", header = T, stringsAsFactors = T)



#lets see top level view of the dataset

dim(telchurn_dat1)
colnames(telchurn_dat1) #read the data dictionary to get an idea about each variable
str(telchurn_dat1)
class(telchurn_dat1)
head(telchurn_dat1)
tail(telchurn_dat1)
View(telchurn_dat1)
summary(telchurn_dat1)


options(scipen=999) #to disable scientific notaiton



#----------------lets understand the data and create a Data Quality Report (DQR)----------------

colSums(is.na(telchurn_dat1)) #checking for no.of missed values in each variable

sum(telchurn_dat1$churn)/nrow(telchurn_dat1) #Cheking overall churn percentage (24%)

telchurn_dat1_var<-names(telchurn_dat1)
length(telchurn_dat1_var)


(telchurn_dat1_var_DQR<-as.data.frame(telchurn_dat1_var)) #creating a data frame


(telchurn_dat1_var_DQR$DataType<-sapply(telchurn_dat1,class)) #getting data type for each variable


(telchurn_dat1_var_DQR$Total_Observations<-nrow(telchurn_dat1)) #getting no.of observations


#getting sum of unique variables

i=0
for(i in 1:ncol(telchurn_dat1)) {
  telchurn_dat1_var_DQR$UniqueObservations[i]<-length(unique(telchurn_dat1[,i]))
}

#Data availability for each variable

telchurn_dat1_var_DQR$DataAvailable<-colSums(!is.na(telchurn_dat1))
telchurn_dat1_var_DQR$Perc_DataAvailable<-colSums(!is.na(telchurn_dat1))/66297


#No.of missing vaues in each variable
telchurn_dat1_var_DQR$MissingValues<-colSums(is.na(telchurn_dat1))
telchurn_dat1_var_DQR$Perc_MissingValues<-colSums(is.na(telchurn_dat1))/66297


#Finding numerical measures (min, max,mean and percentiles)
i=0
for (i in 1:ncol(telchurn_dat1)) {
  telchurn_dat1_var_DQR$Minimum[i] <- round(ifelse(class(telchurn_dat1[,i])=="integer" | class(telchurn_dat1[,i])=="numeric",min(telchurn_dat1[,i],na.rm = T),0),4)
  telchurn_dat1_var_DQR$Maximum[i] <- round(ifelse(class(telchurn_dat1[,i])=="integer" | class(telchurn_dat1[,i])=="numeric",max(telchurn_dat1[,i],na.rm = T),0),4)
  telchurn_dat1_var_DQR$Mean[i] <- round(ifelse(class(telchurn_dat1[,i])=="integer" | class(telchurn_dat1[,i])=="numeric",mean(telchurn_dat1[,i],na.rm = T),0),4)
  telchurn_dat1_var_DQR$Fifth_percentile[i] <- round(ifelse(class(telchurn_dat1[,i])=="integer" | class(telchurn_dat1[,i])=="numeric",quantile(telchurn_dat1[,i],p=0.05,na.rm = T),0),4)
  telchurn_dat1_var_DQR$Tenth_percentile[i] <- round(ifelse(class(telchurn_dat1[,i])=="integer" | class(telchurn_dat1[,i])=="numeric",quantile(telchurn_dat1[,i],p=0.10,na.rm = T),0),4)
  telchurn_dat1_var_DQR$TwentyFifth_percentile[i] <- round(ifelse(class(telchurn_dat1[,i])=="integer" | class(telchurn_dat1[,i])=="numeric",quantile(telchurn_dat1[,i],p=0.25,na.rm = T),0),4)
  telchurn_dat1_var_DQR$Fiftyth_percentile[i] <- round(ifelse(class(telchurn_dat1[,i])=="integer" | class(telchurn_dat1[,i])=="numeric",quantile(telchurn_dat1[,i],p=0.50,na.rm = T),0),4)
  telchurn_dat1_var_DQR$SeventyFifth_percentile[i] <- round(ifelse(class(telchurn_dat1[,i])=="integer" | class(telchurn_dat1[,i])=="numeric",quantile(telchurn_dat1[,i],p=0.75,na.rm = T),0),4)
  telchurn_dat1_var_DQR$Ninteyth_percentile[i] <- round(ifelse(class(telchurn_dat1[,i])=="integer" | class(telchurn_dat1[,i])=="numeric",quantile(telchurn_dat1[,i],p=0.90,na.rm = T),0),4)
  telchurn_dat1_var_DQR$NinteyFifth_percentile[i] <- round(ifelse(class(telchurn_dat1[,i])=="integer" | class(telchurn_dat1[,i])=="numeric",quantile(telchurn_dat1[,i],p=0.95,na.rm = T),0),4)
  
  
}

str(telchurn_dat1_var_DQR)

#lets export the DQR into a CSV file

write.csv(telchurn_dat1_var_DQR, "Data Quality Report_Telecom.csv", row.names = T)




#---------------------Finding & Treating Anamolies and Missing values------------------------


#reject the variables which have more than 15% of values missing 



#variable retdays can be an important one for the study, hence, do not reject it

#Anamoly treatment for restdays, adding a dummy variable with missing value fills


str(telchurn_dat1$retdays)
summary(telchurn_dat1$retdays)

telchurn_dat1$retdays_dummy<- ifelse(is.na(telchurn_dat1$retdays)==TRUE,0,1)
str(telchurn_dat1$retdays_dummy)


telchurn_dat2<-telchurn_dat1[,colMeans(is.na(telchurn_dat1))<=0.15]

dim(telchurn_dat2) #rejected 14 variables based on percentage of missing values threshold as 15% 


#the below variables are looking closely associated, lets check the data dictionary to know more
summary(telchurn_dat2$drop_blk_Mean)
summary(telchurn_dat2$blck_dat_Mean)
summary(telchurn_dat2$drop_vce_Mean)
summary(telchurn_dat2$drop_dat_Mean)
summary(telchurn_dat2$drop_vce_mean)

#blck_dat_Mean = Mean number of blocked (failed) data calls
#drop_dat_Mean = Mean number of dropped (failed) data calls

#since both the variables mean same we can drop either of these two variables 

#lets omit blck_dat_Mean from data

names(telchurn_dat2)

telchurn_dat2<-telchurn_dat2[,-50]



#check for missing values
colSums(is.na(telchurn_dat2))


#----------------------------Variable Profiling----------------------------------------#

#Find out each variable type & store them in sperate lists 
telchurn_dat2_var<-names(telchurn_dat2)
var_type_cont<-vector() #For continuous variables
var_type_catg<-vector() #For categorical variables
i=1
j=1

for (k in 1:ncol(telchurn_dat2)) {
  if(class(telchurn_dat2[,k])=="numeric" || class(telchurn_dat2[,k])=="integer" ){
    var_type_cont[i]<-telchurn_dat2_var[k]
    i<-i+1
  }
  else{
    var_type_catg[j]<-telchurn_dat2_var[k]
    j<-j+1
  }
}


#Decile binning should be used to find out event rate (churn rate in thiscase). 
#If there is a trend (increase in event rate or decrease in eventrate), then that variable should be selected for model iterations
#For some variables it won't be possible to do decile binning, one should either divide the data into 8,6,4,3 or 2 equal parts and thenvlook at the event rates. 
#If there is a pattern then, the variable should be chosen for model iterations
#Profiling will also aid in data preparation as deciles with similar event rates can be clubbed together into one group and a categorical variable can be created

names(telchurn_dat2)
str(telchurn_dat2)

#Variable: [1] mou_Mean
#There is a clear downward trend in the event rate (churn) as mou_Mean increases
telchurn_dat2%>%mutate(dec=ntile(mou_Mean,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(mou_Mean,na.rm = TRUE),LessThan=max(mou_Mean,na.rm = TRUE))->dec_mou_Mean


#Variable: [2] totmrc_Mean     
#No clear trend
telchurn_dat2%>%mutate(dec=ntile(totmrc_Mean,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(totmrc_Mean,na.rm = TRUE),LessThan=max(totmrc_Mean,na.rm = TRUE))->dec_totmrc_Mean


#Variable: [3] rev_Range            
#No clear trend
telchurn_dat2%>%mutate(dec=ntile(rev_Range,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(rev_Range,na.rm = TRUE),LessThan=max(rev_Range,na.rm = TRUE))->dec_rev_Range


#Variable: [4] mou_Range                   
#No clear trend
telchurn_dat2%>%mutate(dec=ntile(mou_Range,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(mou_Range,na.rm = TRUE),LessThan=max(mou_Range,na.rm = TRUE))->dec_mou_Range

#Variable: [5] change_mou                        
#No clear trend
telchurn_dat2%>%mutate(dec=ntile(change_mou,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(change_mou,na.rm = TRUE),LessThan=max(change_mou,na.rm = TRUE))->dec_change_mou


#Variable: [6] drop_blk_Mean                          
#No clear trend
telchurn_dat2%>%mutate(dec=ntile(drop_blk_Mean,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(drop_blk_Mean,na.rm = TRUE),LessThan=max(drop_blk_Mean,na.rm = TRUE))->dec_drop_blk_Mean


#Variable: [7] drop_vce_Range                            
#No clear trend
telchurn_dat2%>%mutate(dec=ntile(drop_vce_Range,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(drop_vce_Range,na.rm = TRUE),LessThan=max(drop_vce_Range,na.rm = TRUE))->dec_drop_vce_Range

#Variable: [8] owylis_vce_Range                            
#No clear trend
telchurn_dat2%>%mutate(dec=ntile(owylis_vce_Range,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(owylis_vce_Range,na.rm = TRUE),LessThan=max(owylis_vce_Range,na.rm = TRUE))->dec_owylis_vce_Range

#Variable: [9] mou_opkv_Range                             
#Slight downward trend in the event rate as mou_opkv_Range
telchurn_dat2%>%mutate(dec=ntile(mou_opkv_Range  ,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(mou_opkv_Range,na.rm = TRUE),LessThan=max(mou_opkv_Range,na.rm = TRUE))->dec_mou_opkv_Range


#Variable: [10] months                             
#No clear trend
#One prominent observation is that, many people (42%) tend to leave the company between 10-12 months
telchurn_dat2%>%mutate(dec=ntile(months,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(months,na.rm = TRUE),LessThan=max(months,na.rm = TRUE))->dec_months

#Variable: [11] totcalls                             
#Slight upward trend in the event rate as totcalls increase
telchurn_dat2%>%mutate(dec=ntile(totcalls,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(totcalls,na.rm = TRUE),LessThan=max(totcalls,na.rm = TRUE))->dec_totcalls

#Variable: [12] eqpdays                             
#Slight upward trend in the event rate as eqpdays increase
telchurn_dat2%>%mutate(dec=ntile(eqpdays,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(eqpdays,na.rm = TRUE),LessThan=max(eqpdays,na.rm = TRUE))->dec_eqpdays

#Variable: [13] custcare_Mean                                
#Many values are zeros, many not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(custcare_Mean,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(custcare_Mean,na.rm = TRUE),LessThan=max(custcare_Mean,na.rm = TRUE))->dec_custcare_Mean

#Variable: [14] callwait_Mean                                   
#Many values are zeros, many not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(callwait_Mean,4))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(callwait_Mean,na.rm = TRUE),LessThan=max(callwait_Mean,na.rm = TRUE))->dec_callwait_Mean

#Variable: [15] iwylis_vce_Mean                                    
telchurn_dat2%>%mutate(dec=ntile(iwylis_vce_Mean,6))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(iwylis_vce_Mean,na.rm = TRUE),LessThan=max(iwylis_vce_Mean,na.rm = TRUE))->dec_iwylis_vce_Mean

#Variable: [16] callwait_Range                                     
#Many values are zeros, many not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(callwait_Range,2))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(callwait_Range,na.rm = TRUE),LessThan=max(callwait_Range,na.rm = TRUE))->dec_callwait_Range

#Variable: [16] ccrndmou_Range                                     
#Many values are zeros, many not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(ccrndmou_Range,2))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(ccrndmou_Range,na.rm = TRUE),LessThan=max(ccrndmou_Range,na.rm = TRUE))->dec_ccrndmou_Range

#Variable: [17] adjqty                                    
#Slight upward trend in the event rate as adjqty increases
telchurn_dat2%>%mutate(dec=ntile(adjqty,10))%>%
 group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(adjqty,na.rm = TRUE),LessThan=max(adjqty,na.rm = TRUE))->dec_adjqty

#Variable: [18] ovrrev_Mean                                    
telchurn_dat2%>%mutate(dec=ntile(ovrrev_Mean,4))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(ovrrev_Mean,na.rm = TRUE),LessThan=max(ovrrev_Mean,na.rm = TRUE))->dec_ovrrev_Mean

#Variable: [19] rev_Mean                                    
#No clear trend
telchurn_dat2%>%mutate(dec=ntile(rev_Mean,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(rev_Mean,na.rm = TRUE),LessThan=max(rev_Mean,na.rm = TRUE))->dec_rev_Mean

#Variable: [20] ovrmou_Mean                                        
#Many values are zeros, many not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(ovrmou_Mean,4))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(ovrmou_Mean,na.rm = TRUE),LessThan=max(ovrmou_Mean,na.rm = TRUE))->dec_ovrmou_Mean


#Variable: [21] comp_vce_Mean                                           
#Slight downward trend in the event rate as comp_vce_Mean increases
telchurn_dat2%>%mutate(dec=ntile(comp_vce_Mean,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(comp_vce_Mean,na.rm = TRUE),LessThan=max(comp_vce_Mean,na.rm = TRUE))->dec_comp_vce_Mean


#Variable: [22] plcd_vce_Mean                                             
#Slight downward trend in the event rate as plcd_vce_Mean increases
telchurn_dat2%>%mutate(dec=ntile(plcd_vce_Mean,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(plcd_vce_Mean,na.rm = TRUE),LessThan=max(plcd_vce_Mean,na.rm = TRUE))->dec_plcd_vce_Mean

#Variable: [23] avg3mou                                              
#Slight downward trend in the event rate as avg3mou increases
telchurn_dat2%>%mutate(dec=ntile(avg3mou,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(avg3mou,na.rm = TRUE),LessThan=max(avg3mou,na.rm = TRUE))->dec_avg3mou


#Variable: [24] avgmou                                              
#No clear trend
telchurn_dat2%>%mutate(dec=ntile(avgmou,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(avgmou,na.rm = TRUE),LessThan=max(avgmou,na.rm = TRUE))->dec_avgmou


#Variable: [25] avg3qty                                               
#Clear downward trend in the event rate as avg3qty increases
telchurn_dat2%>%mutate(dec=ntile(avg3qty,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(avg3qty,na.rm = TRUE),LessThan=max(avg3qty,na.rm = TRUE))->dec_avg3qty


#Variable: [26] avgqty                                                
#No clear trend
telchurn_dat2%>%mutate(dec=ntile(avgqty,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(avgqty,na.rm = TRUE),LessThan=max(avgqty,na.rm = TRUE))->dec_avgqty

#Variable: [27] avg6mou                                                 
#Clear downward trend in the event rate as avg6mou increases
telchurn_dat2%>%mutate(dec=ntile(avg6mou,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(avg6mou,na.rm = TRUE),LessThan=max(avg6mou,na.rm = TRUE))->dec_avg6mou


#Variable: [28] avg6qty                                                  
#Slight downward trend in the event rate as avg6mou increases
telchurn_dat2%>%mutate(dec=ntile(avg6qty,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(avg6qty ,na.rm = TRUE),LessThan=max(avg6qty ,na.rm = TRUE))->dec_avg6qty 


#Variable: [38] age1                                                    
#Many values are zeros, many not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(age1,6))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(age1 ,na.rm = TRUE),LessThan=max(age1 ,na.rm = TRUE))->dec_age1

#Variable: [39] age2                                                     
#Many values are zeros, many not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(age2,4))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(age2 ,na.rm = TRUE),LessThan=max(age2 ,na.rm = TRUE))->dec_age2

#Variable: [40] models                                                     
#Can not be deciled, as it is a categorical variable stored as numerical. Will deal with it later
telchurn_dat2%>%mutate(dec=ntile(models,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(models ,na.rm = TRUE),LessThan=max(models ,na.rm = TRUE))->dec_models
unique(telchurn_dat2$models)

#Variable: [41] hnd_price                                                            
#Can not be deciled
#It turns out be categorical variable lets deal with it later
telchurn_dat2%>%mutate(dec=ntile(hnd_price,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(hnd_price ,na.rm = TRUE),LessThan=max(hnd_price ,na.rm = TRUE))->dec_hnd_price
unique(telchurn_dat2$hnd_price)

#Variable: [42] actvsubs                                                            
#Can not be deciled
#It's a categorical variable
telchurn_dat2%>%mutate(dec=ntile(actvsubs,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(actvsubs ,na.rm = TRUE),LessThan=max(actvsubs ,na.rm = TRUE))->dec_actvsubs

#Variable: [43] uniqsubs                                                            
#Can not be deciled
#It's a categorical variable
telchurn_dat2%>%mutate(dec=ntile(uniqsubs,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(uniqsubs ,na.rm = TRUE),LessThan=max(uniqsubs ,na.rm = TRUE))->dec_uniqsubs


#Variable: [44] forgntvl                                                                    
#Can not be deciled
#It's a categorical variable
telchurn_dat2%>%mutate(dec=ntile(forgntvl,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(forgntvl,na.rm = TRUE),LessThan=max(forgntvl,na.rm = TRUE))->dec_forgntvl


#Variable: [45] opk_dat_Mean                                                                        
#Many values are zeros, may not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(opk_dat_Mean,2))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(opk_dat_Mean,na.rm = TRUE),LessThan=max(opk_dat_Mean,na.rm = TRUE))->dec_opk_dat_Mean


#Variable: [46] mtrcycle and #Variable: [47] truck are categorical variables but stored as numerical variables

#Variable: [48] roam_Mean                                                                               
#Many values are zeros, may not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(roam_Mean,2))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(roam_Mean,na.rm = TRUE),LessThan=max(roam_Mean,na.rm = TRUE))->dec_roam_Mean


#Variable: [49] recv_sms_Mean                                                                                  
#Many values are zeros, may not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(recv_sms_Mean,4))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(recv_sms_Mean,na.rm = TRUE),LessThan=max(recv_sms_Mean,na.rm = TRUE))->dec_recv_sms_Mean


#Variable: [50] mou_pead_Mean                                                                                     
#Many values are zeros, may not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(mou_pead_Mean,2))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(mou_pead_Mean,na.rm = TRUE),LessThan=max(mou_pead_Mean,na.rm = TRUE))->dec_mou_pead_Mean


#Variable: [54] da_Mean                                                                                     
#Many values are zeros, may not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(da_Mean,4))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(da_Mean,na.rm = TRUE),LessThan=max(da_Mean,na.rm = TRUE))->dec_da_Mean


#Variable: [55] da_Range                                                                                             
#Many values are zeros, may not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(da_Range,4))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(da_Range,na.rm = TRUE),LessThan=max(da_Range,na.rm = TRUE))->dec_da_Range


#Variable: [56] datovr_Mean                                                                                                  
#Many values are zeros, may not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(datovr_Mean,2))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(datovr_Mean,na.rm = TRUE),LessThan=max(datovr_Mean,na.rm = TRUE))->dec_datovr_Mean


#Variable: [57] datovr_Range                                                                                                      
#Many values are zeros, may not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(datovr_Range,2))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(datovr_Range,na.rm = TRUE),LessThan=max(datovr_Range,na.rm = TRUE))->dec_datovr_Range

#Variable: [58] drop_dat_Mean                                                                                                         
#Many values are zeros, may not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(drop_dat_Mean,2))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(drop_dat_Mean,na.rm = TRUE),LessThan=max(drop_dat_Mean,na.rm = TRUE))->dec_drop_dat_Mean

#Variable: [59] drop_vce_Mean                                                                                                            
#No clear trend
telchurn_dat2%>%mutate(dec=ntile(drop_vce_Mean,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(drop_vce_Mean,na.rm = TRUE),LessThan=max(drop_vce_Mean,na.rm = TRUE))->dec_drop_vce_Mean

#Variable: [60] adjmou                                                                                                            
#No clear trend
telchurn_dat2%>%mutate(dec=ntile(adjmou,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(adjmou,na.rm = TRUE),LessThan=max(adjmou,na.rm = TRUE))->dec_adjmou


#Variable: [61] totrev                                                                                                            
#No clear trend
telchurn_dat2%>%mutate(dec=ntile(totrev,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(totrev,na.rm = TRUE),LessThan=max(totrev,na.rm = TRUE))->dec_totrev

#Variable: [62] adjrev                                                                                                            
#Sligth upward trend in the event rate as adrev increases
telchurn_dat2%>%mutate(dec=ntile(adjrev,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(adjrev,na.rm = TRUE),LessThan=max(adjrev,na.rm = TRUE))->dec_adjrev


#Variable: [63] avgrev                                                                                                            
#No clear trend
telchurn_dat2%>%mutate(dec=ntile(avgrev,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(avgrev,na.rm = TRUE),LessThan=max(avgrev,na.rm = TRUE))->dec_avgrev


#Variable: [65] comp_dat_Mean                                                                                                               
#Many values are zeros, may not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(comp_dat_Mean,2))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(comp_dat_Mean,na.rm = TRUE),LessThan=max(comp_dat_Mean,na.rm = TRUE))->dec_comp_dat_Mean


#Variable: [66] plcd_dat_Mean                                                                                                                  
#Many values are zeros, may not be useful for the model
telchurn_dat2%>%mutate(dec=ntile(plcd_dat_Mean,2))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(plcd_dat_Mean,na.rm = TRUE),LessThan=max(plcd_dat_Mean,na.rm = TRUE))->dec_plcd_dat_Mean

#Variable: [67] retdays_dummy   
#Deciling is not possible, as it is a categorical variable
telchurn_dat2%>%mutate(dec=ntile(retdays_dummy,2))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(retdays_dummy,na.rm = TRUE),LessThan=max(retdays_dummy,na.rm = TRUE))->dec_retdays_dummy



#One of the area to understand is that, the effect of network & service quality influencing churn rate

#To get a top level idea about network & service quality we can drive the following variables

telchurn_dat2$compl_vce_percentage<-(telchurn_dat2$comp_vce_Mean/telchurn_dat2$plcd_vce_Mean)

telchurn_dat2$compl_dat_percentage<-(telchurn_dat2$comp_dat_Mean/telchurn_dat2$plcd_dat_Mean)


#Variable: [68] compl_vce_percentage
#As the completion percentage of voice calls increase churn rate slightly decreases
telchurn_dat2%>%mutate(dec=ntile(compl_vce_percentage,10))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(compl_vce_percentage,na.rm = TRUE),LessThan=max(compl_vce_percentage,na.rm = TRUE))->dec_compl_vce_percentage


#Variable: [69] compl_dat_percentage
#Not useful for the study as there are less customers who placed data calls
telchurn_dat2%>%mutate(dec=ntile(compl_dat_percentage,4))%>%
  group_by(dec)%>%summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2),GreaterThan=min(compl_dat_percentage,na.rm = TRUE),LessThan=max(compl_dat_percentage,na.rm = TRUE))->dec_compl_dat_percentage


telchurn_dat2<-telchurn_dat2[,-69] #Remove compl_dat_percentage from the dataset
View(dec_compl_vce_percentage)



telchurn_deciled_cont<-rbind(dec_mou_Mean,dec_totmrc_Mean,dec_rev_Range,dec_mou_Range,dec_change_mou,dec_drop_blk_Mean,dec_drop_vce_Mean,
                             dec_owylis_vce_Range,dec_mou_opkv_Range,dec_months,dec_totcalls,dec_eqpdays,dec_iwylis_vce_Mean,
                             dec_adjqty,dec_rev_Mean,dec_comp_vce_Mean,dec_plcd_vce_Mean,dec_avg3mou,dec_avgmou,dec_avg3qty, dec_avgqty,
                             dec_avg6mou,dec_avg6qty,dec_hnd_price,dec_drop_vce_Mean,dec_adjmou,dec_totrev,dec_adjrev,dec_avgrev,dec_compl_vce_percentage)

str(telchurn_deciled_cont)

write.csv(telchurn_deciled_cont,"Deciled output_Continuous Variables.csv",row.names = F)


#We can remove variables based on the deciled outputs
#Lets remove those variables which either have many zeros or no variability, these variables do not add any significance to the model

#custcare_Mean,callwait_Mean,callwait_Range,ccrndmou_Range,ovrmou_Mean,opk_dat_Mean,roam_Mean,recv_sms_Mean,mou_pead_Mean   
#da_Mean,da_Range,datovr_Mean,datovr_Range,drop_dat_Mean,comp_dat_Mean, plcd_dat_Mean                                         

names(telchurn_dat2)
telchurn_dat3<-telchurn_dat2[,-c(13,14,16,17,21,45,48:50,54:58,65,66)]
dim(telchurn_dat3)


#Before checking event rates for the categorical variables, lets convert the varibles which are supposed to be categorical but stored as numerical, in to categorical type

telchurn_dat3$models<-as.factor(telchurn_dat3$models)
telchurn_dat3$hnd_price<-as.factor(telchurn_dat3$hnd_price)
telchurn_dat3$actvsubs<-as.factor(telchurn_dat3$actvsubs)
telchurn_dat3$uniqsubs<-as.factor(telchurn_dat3$uniqsubs)
telchurn_dat3$forgntvl<-as.factor(telchurn_dat3$forgntvl)
telchurn_dat3$truck<-as.factor(telchurn_dat3$truck)
telchurn_dat3$mtrcycle<-as.factor(telchurn_dat3$mtrcycle)

str(telchurn_dat3)


#Now Lets do variable profiling for the categorical variables
#Ideally there should be good difference between the event rate across levels


#Variable: crclscod
telchurn_dat3%>%group_by(crclscod)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_crclscod

#Variable: asl_flag   
#There is some difference in the evernt rate across different levels
telchurn_dat3%>%group_by(asl_flag)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_asl_flag

#Variable: prizm_social_one   
#There is some difference in the evernt rate across different levels
#Missing values have the similar event rate as T
#Levels C, S & U have smilar event rates we can club them together 
telchurn_dat3%>%group_by(prizm_social_one)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_prizm_social_one


#Variable: area   
#There are many levels, we will try to club the levels with similar event rate
#Event rate of missing values matches with few other level, we can use this info to treat missing values
telchurn_dat3%>%group_by(area)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_area


#Variable: refurb_new         
#There is some difference in the evernt rate across different levels
telchurn_dat3%>%group_by(refurb_new)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_refurb_new

#Variable: hnd_webcap               
#There is some difference in the evernt rate across different levels
telchurn_dat3%>%group_by(hnd_webcap)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_hnd_webcap


#Variable: marital               
#There is some difference in the evernt rate across different levels
#Event rate of missing values matches with the level S, we can use this info to treat missing values
telchurn_dat3%>%group_by(marital)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_marital


#Variable: ethnic               
#There is some difference in the evernt rate across different levels
#Event rate of missing values matches with the level M, we can use this info to treat missing values
telchurn_dat3%>%group_by(ethnic)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_ethnic


#Variable: models               
#There is some difference in the evernt rate across different levels
#There are many levels, we will try to club the levels with similar event rate
telchurn_dat2%>%group_by(models)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,2))->dec_models


#Variable: hnd_price                      
#There is some difference in the evernt rate across different levels
#Event rate of missing values is very close to the level 299.989990, we can use this info to treat missing values
telchurn_dat3%>%group_by(hnd_price)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_hnd_price

#Variable: actvsubs                     
#There is some difference in the evernt rate across different levels
telchurn_dat3%>%group_by(actvsubs)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_actvsubs


#Variable: uniqsubs                     
#There is clear difference in the evernt rate across different levels
#As the lvels of uniquesubs increase event rate is also increasing
telchurn_dat3%>%group_by(uniqsubs)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_uniqsubs


#Variable: forgntvl                            
#There is not much difference in the evernt rate across different levels
telchurn_dat3%>%group_by(forgntvl)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_forgntvl


#Variable: mtrcycle                            
#There is not much difference in the evernt rate across different levels
telchurn_dat3%>%group_by(mtrcycle)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_mtrcycle


#Variable: truck                             
#There is not much difference in the evernt rate across different levels
telchurn_dat3%>%group_by(truck)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_truck 


#Variable: car_buy                                      
#There is not much difference in the evernt rate across different levels
telchurn_dat3%>%group_by(car_buy)%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_car_buy 

#Variable: csa                                        
#There is some difference in the evernt rate across different levels
telchurn_dat3%>%group_by(csa  )%>%
  summarise(n=sum(churn),N=n(),Churn_perc=round(n/N,4))->dec_csa   


#omit varibles where there is not much change in the event rate at diffrent levles
#car_buy,truck,mtrcycle,forgntvl

telchurn_dat3<-telchurn_dat3[,-c(39:41,43)]
names(telchurn_dat3)
dim(telchurn_dat3)

#----------------------------Treating Missing Values & Anamolies (outliers/unexpected values)------------------#


#We can use outputs from variable profiling to complete this step

#Find out each variable type & store them in sperate lists for future use
dim(telchurn_dat3)
names(telchurn_dat3)

telchurn_dat3_var<-names(telchurn_dat3)
var_type_cont2<-vector() #For continuous variables
var_type_catg2<-vector() #For categorical variables
i=1
j=1

for (k in 1:ncol(telchurn_dat3)) {
  if(class(telchurn_dat3[,k])=="numeric" || class(telchurn_dat3[,k])=="integer" ){
    var_type_cont2[i]<-telchurn_dat3_var[k]
    i<-i+1
  }
  else{
    var_type_catg2[j]<-telchurn_dat3_var[k]
    j<-j+1
  }
}

#First lets treat missing values of categorical variables

colSums(is.na(telchurn_dat3))

var_type_catg2


#Variable: crclscod
#There are no missing values
sum(is.na(telchurn_dat3$crclscod))

#Variable: asl_flag
#There are no missing values
sum(is.na(telchurn_dat3$asl_flag))


#Variable: prizm_social_one
sum(is.na(telchurn_dat3$prizm_social_one)) #4751 missing values

View(dec_prizm_social_one)

missing_prizm<-which(is.na(telchurn_dat3$prizm_social_one))

telchurn_dat3$prizm_social_one[missing_prizm]<-"T" #Replacing them with level T as there is similatiry in the event rate

#As observed above, levels C, S & U have smilar event rates we can club them together 
#Replace levels S & u with C

index_prizm<-which(telchurn_dat3$prizm_social_one=="S" |
                     telchurn_dat3$prizm_social_one=="U")

telchurn_dat3$prizm_social_one[index_prizm]<-"C"
unique(telchurn_dat3$prizm_social_one)
summary(telchurn_dat3$prizm_social_one)


#Variable: area
sum(is.na(telchurn_dat3$area))#18 missing values

View(dec_area) #event rate for missing values is same as level OHIO AREA

missing_area<-which(is.na(telchurn_dat3$area))

telchurn_dat3$area[missing_area]<-"OHIO AREA"



#Variable: refurb_new
sum(is.na(telchurn_dat3$refurb_new)) #There is only one missing observation

missing_refurb_new<-which(is.na(telchurn_dat3$refurb_new))

telchurn_dat3[missing_refurb_new,] #The observation has either zeros or missing values for many variables, hence we can omit this from dataset

dim(telchurn_dat3)
telchurn_dat3[-missing_refurb_new,]->telchurn_dat3


#Variable: hnd_webcap
sum(is.na(telchurn_dat3$hnd_webcap)) #6062 missing values

View(dec_hnd_webcap) #we can replace missing values with level "WC" based on the event rate similarity

missing_hnd_webcap<-which(is.na(telchurn_dat3$hnd_webcap))

telchurn_dat3$hnd_webcap[missing_hnd_webcap]<-"WC"

#Variable: marital
sum(is.na(telchurn_dat3$marital))

View(dec_marital)#we can replace missing values with level "S" based on the event rate similarity

missing_marital<-which(is.na(telchurn_dat3$marital))

telchurn_dat3$marital[missing_marital]<-"S"


#Variable: ethnic
sum(is.na(telchurn_dat3$ethnic))

View(dec_ethnic)

missing_ethnic<-which(is.na(telchurn_dat3$ethnic))
telchurn_dat3$ethnic[missing_ethnic]<-"M"


#Variable: hnd_price
sum(is.na(telchurn_dat3$hnd_price)) #we can replace missing values with level "299.9899902" based on the event rate similarity
View(dec_hnd_price)

missing_hnd_price<-which(is.na(telchurn_dat3$hnd_price))

telchurn_dat3$hnd_price[missing_hnd_price]<- "299.9899902"

#Variable: csa
sum(is.na(telchurn_dat3$csa))
View(dec_csa) #we can replace missing values with level "AIRORA803" based on the event rate similarity

missing_csa<-which(is.na(telchurn_dat3$csa))

telchurn_dat3$csa[missing_csa]<- "AIRORA803"

##Now lets deal with missing values of continuous variables

colSums(is.na(telchurn_dat3))

#Variable: mou_Mean
sum(is.na(telchurn_dat3$mou_Mean))

View(dec_mou_Mean)

missing_mou_Mean<-which(is.na(telchurn_dat3$mou_Mean))

telchurn_dat3[missing_mou_Mean,] #We can omit these observations, as there are missing values for these observations across variables

telchurn_dat4<-telchurn_dat3[-missing_mou_Mean,] #storing the cleaned dataset in a new vector.

dim(telchurn_dat4)

colSums(is.na(telchurn_dat4))

#Variable: change_mou
View(dec_change_mou) #Event for missing values doesn't match with any other decile bin, we can omit these observations as well
missing_change_mou<-which(is.na(telchurn_dat4$change_mou))

dim(telchurn_dat4)
telchurn_dat4<-telchurn_dat4[-missing_change_mou,] 


#Variable: avg6mou
View(dec_avg6mou)

#lets impute the missing values with the average of 10th decile observations

telchurn_dat4%>%mutate(dec=ntile(avg6mou,10))->telchurn_dat4

telchurn_dat4%>%filter(dec==10)%>%summarise(avg=sum(avg6mou)/n())

missing_avg6mou<-which(is.na(telchurn_dat4$avg6mou))

telchurn_dat4$avg6mou[missing_avg6mou]<- 1688.485

#Variable: avg6qty

telchurn_dat4%>%mutate(dec=ntile(avg6qty,10))->telchurn_dat4

telchurn_dat4%>%filter(dec==10)%>%summarise(avg=sum(avg6qty)/n())

missing_avg6qty<-which(is.na(telchurn_dat4$avg6qty))

telchurn_dat4$avg6qty[missing_avg6qty]<-  610.5087

#Variable: age1

View(dec_age1)# 4th & 5th quartiles have similar event rate as missing values

#Lets impute the missing values with the average of 4th & 5th quartile ranges

View(dec_age1)

(36+52)/2

missing_age1<-which(is.na(telchurn_dat4$age1))

telchurn_dat4$age1[missing_age1]<-  44

telchurn_dat4%>%filter(age1==0)%>%summarise(n=n()) #17888 values are zeros

#As age can not be zeros, we can convert this variable into a categorical variable and will name missing values as "None"

telchurn_dat4$age1_dummy<-ifelse(telchurn_dat4$age1==0,"None",
                                 ifelse(telchurn_dat4$age1<=30,"Young",
                                        ifelse(telchurn_dat4$age1>30 & telchurn_dat4<=55,"Mid-Aged","Senior")))


unique(telchurn_dat4$age1_dummy)
str(telchurn_dat4$age1_dummy)

telchurn_dat4$age1_dummy<-as.factor(telchurn_dat4$age1_dummy)



#Variable: age2

View(dec_age2)

telchurn_dat4%>%filter(age2==0)%>%summarise(n=n()) #Half of the "age2" values are stored as zeros. May not be useful for study

#lets omit variable "age2" from the data

names(telchurn_dat4)

telchurn_dat4<-telchurn_dat4[,-c(33,34)] #Removing "age1" as well, as I have created a transformed variable using it

telchurn_dat4<-telchurn_dat4[,-47] #Removing "dec" which has been created for a diffrent purpose, not needed anymore


dim(telchurn_dat4)

colSums(is.na(telchurn_dat4))


#There are missing values in the transformed variables "compl_vce_percentage". Lets inspect it

summary(telchurn_dat4$compl_vce_percentage)

missing_compl_vce_percentage<-which(is.na(telchurn_dat4$compl_vce_percentage))

telchurn_dat4$comp_vce_Mean[missing_compl_vce_percentage]



telchurn_dat4$compl_vce_percentage
#We have missing values in "compl_vce_percentage" due to the fact that the corresponding values of "plcd_vce_Mean" and "plcd_vce_Mean" are zeros
#While createing the transformed variable due to zeros in numerator & denominator, the values have become NaN
#We can omit these observations

telchurn_dat4<-telchurn_dat4[-missing_compl_vce_percentage,]



colSums(is.na(telchurn_dat4))#No missing values found

#There is one more aspect we need to study is the optimal plan
#Lets create a tranformed variable to get high level view on this

summary(telchurn_dat4$ovrrev_Mean)
summary(telchurn_dat4$totrev)

telchurn_dat4$optimum<-telchurn_dat4$ovrrev_Mean/telchurn_dat4$totrev

str(telchurn_dat4)
dim(telchurn_dat4)
names(telchurn_dat4)
telchurn_dat4$retdays_dummy<-as.factor(telchurn_dat4$retdays_dummy)

#check overall churn percentage to see if data cleaning activities impacted the churn rate
sum(telchurn_dat4$churn)/nrow(telchurn_dat4) #churn rate is 23.3%, we haven't lost much information

#The data is good to go


telchurn_dat4_var<-names(telchurn_dat4)
var_type_cont3<-vector() #For continuous variables
var_type_catg3<-vector() #For categorical variables
i=1
j=1

for (k in 1:ncol(telchurn_dat4)) {
  if(class(telchurn_dat4[,k])=="numeric" || class(telchurn_dat4[,k])=="integer" ){
    var_type_cont3[i]<-telchurn_dat4_var[k]
    i<-i+1
  }
  else{
    var_type_catg3[j]<-telchurn_dat4_var[k]
    j<-j+1
  }
}

#Lets deal with outliers

telchurn_cleaned<-telchurn_dat4 #Retaining the cleaned data set as a new vector for future use
dim(telchurn_cleaned)

names(telchurn_cleaned)

#Remove the range variables for which we have Mean as well

telchurn_cleaned<-telchurn_cleaned[,-c(3,4,7)]
dim(telchurn_cleaned)


#---------------------------Model Building----------------------------------#


summary(telchurn_cleaned)
colSums(is.na(telchurn_cleaned))
nrow(telchurn_cleaned)

set.seed(2000)

training_index<-sample(nrow(telchurn_cleaned),nrow(telchurn_cleaned)*0.70,replace = F)
training<-telchurn_cleaned[training_index,]
test<-telchurn_cleaned[-training_index,]

nrow(training)
nrow(test)

#Checking for event rate (churn rate) in training & testing datasets, to make sure the samples are not biased
sum(telchurn_cleaned$churn)/nrow(telchurn_cleaned)

sum(training$churn)/nrow(training)

sum(test$churn)/nrow(test)

#Train & Test datasets are not biased, we can go ahead to build the model.

names(telchurn_cleaned)
str(telchurn_cleaned)

step(mod1,direction = "both") #Not able to perform stepwise regression due to memory issues

#Lets build a base model manually and tyr to improve it in the following steps


mod1<-glm(churn ~ +mou_Mean + totmrc_Mean + change_mou + drop_blk_Mean + owylis_vce_Range + mou_opkv_Range +
            months + totcalls + eqpdays + iwylis_vce_Mean + adjqty + ovrrev_Mean + rev_Mean + comp_vce_Mean + 
            plcd_vce_Mean + avg3mou + avgmou + avg3qty + avgqty + avg6mou + 
            avg6qty + asl_flag + prizm_social_one + area + 
            refurb_new + hnd_webcap + marital + ethnic + models + hnd_price + 
            actvsubs + uniqsubs + drop_vce_Mean + adjmou + totrev + 
            adjrev + avgrev + retdays_dummy + compl_vce_percentage + age1_dummy + optimum,family= binomial(link="logit"), data = training)

summary(mod1)

#Lets create dummy variables for the respective significant levels of categorical variables

dim(telchurn_cleaned)
summary(telchurn_cleaned$asl_flag)
telchurn_cleaned$asl_flag_Y<-ifelse(telchurn_cleaned$asl_flag=="Y",1,0)

summary(telchurn_cleaned$prizm_social_one)

telchurn_cleaned$prizm_social_one_R <-ifelse(telchurn_cleaned$prizm_social_one =="R",1,0)

telchurn_cleaned$prizm_social_one_T <-ifelse(telchurn_cleaned$prizm_social_one =="T",1,0)


summary(telchurn_cleaned$area)
telchurn_cleaned$area_DC_MARYLAND_VIRGINIA <-ifelse(telchurn_cleaned$area=="DC/MARYLAND/VIRGINIA AREA",1,0)

telchurn_cleaned$area_MIDWEST <-ifelse(telchurn_cleaned$area=="MIDWEST AREA",1,0)

telchurn_cleaned$area_NORTHWEST_ROCKYMOUNTAIN <-ifelse(telchurn_cleaned$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)

telchurn_cleaned$area_OHIO <-ifelse(telchurn_cleaned$area=="OHIO AREA",1,0)

telchurn_cleaned$area_SOUTH_FLORIDA<-ifelse(telchurn_cleaned$area=="SOUTH FLORIDA AREA",1,0)

telchurn_cleaned$area_TENNESSEE<-ifelse(telchurn_cleaned$area=="TENNESSEE AREA",1,0)


summary(telchurn_cleaned$refurb_new)

telchurn_cleaned$refurb_new_R <-ifelse(telchurn_cleaned$refurb_new =="R",1,0)

summary(telchurn_cleaned$marital)

telchurn_cleaned$marital_S <-ifelse(telchurn_cleaned$marital =="S",1,0)

summary(telchurn_cleaned$ethnic)


telchurn_cleaned$ethnic_C <-ifelse(telchurn_cleaned$ethnic =="C",1,0)
telchurn_cleaned$ethnic_N <-ifelse(telchurn_cleaned$ethnic =="N",1,0)
telchurn_cleaned$ethnic_P <-ifelse(telchurn_cleaned$ethnic =="P",1,0)
telchurn_cleaned$ethnic_S <-ifelse(telchurn_cleaned$ethnic =="S",1,0)
telchurn_cleaned$ethnic_U <-ifelse(telchurn_cleaned$ethnic =="U",1,0)
telchurn_cleaned$ethnic_Z <-ifelse(telchurn_cleaned$ethnic =="Z",1,0)

summary(telchurn_cleaned$models)

telchurn_cleaned$models_2 <-ifelse(telchurn_cleaned$models =="2",1,0)
telchurn_cleaned$models_3 <-ifelse(telchurn_cleaned$models =="3",1,0)
telchurn_cleaned$models_4 <-ifelse(telchurn_cleaned$models =="4",1,0)
telchurn_cleaned$models_5 <-ifelse(telchurn_cleaned$models =="5",1,0)
telchurn_cleaned$models_8 <-ifelse(telchurn_cleaned$models =="8",1,0)

summary(telchurn_cleaned$hnd_price)

telchurn_cleaned$hnd_price129.9899902 <-ifelse(telchurn_cleaned$hnd_price =="129.9899902",1,0)              
telchurn_cleaned$hnd_price199.9899902 <-ifelse(telchurn_cleaned$hnd_price =="199.9899902",1,0)              
telchurn_cleaned$hnd_price249.9899902 <-ifelse(telchurn_cleaned$hnd_price =="249.9899902",1,0)              
telchurn_cleaned$hnd_price299.9899902 <-ifelse(telchurn_cleaned$hnd_price =="299.9899902",1,0)              

summary(telchurn_cleaned$uniqsubs)

telchurn_cleaned$uniqsubs_2 <-ifelse(telchurn_cleaned$uniqsubs =="2",1,0)
telchurn_cleaned$uniqsubs_3 <-ifelse(telchurn_cleaned$uniqsubs =="3",1,0)
telchurn_cleaned$uniqsubs_4 <-ifelse(telchurn_cleaned$uniqsubs =="4",1,0)
telchurn_cleaned$uniqsubs_5 <-ifelse(telchurn_cleaned$uniqsubs =="5",1,0)
telchurn_cleaned$uniqsubs_6 <-ifelse(telchurn_cleaned$uniqsubs =="6",1,0)
telchurn_cleaned$uniqsubs_9 <-ifelse(telchurn_cleaned$uniqsubs =="9",1,0)

summary(telchurn_cleaned$age1_dummy)

telchurn_cleaned$age1_dummy_Young <-ifelse(telchurn_cleaned$age1_dummy =="Young",1,0)

names(telchurn_cleaned)


set.seed(2000)

training_index<-sample(nrow(telchurn_cleaned),nrow(telchurn_cleaned)*0.70,replace = F)
training<-telchurn_cleaned[training_index,]
test<-telchurn_cleaned[-training_index,]


nrow(training)
nrow(test)

#Checking for event rate (churn rate) in training & testing datasets, to make sure the samples are not biased
sum(telchurn_cleaned$churn)/nrow(telchurn_cleaned)

sum(training$churn)/nrow(training)

sum(test$churn)/nrow(test)

names(training)
mod2<-glm(churn ~ +mou_Mean + totmrc_Mean + change_mou + drop_blk_Mean + owylis_vce_Range + mou_opkv_Range +
            months + totcalls + eqpdays + iwylis_vce_Mean + adjqty + ovrrev_Mean + rev_Mean + comp_vce_Mean + plcd_vce_Mean + avg3mou + avgmou + avg3qty + avgqty + avg6mou + 
            avg6qty + asl_flag_Y + prizm_social_one_R +prizm_social_one_T+area_DC_MARYLAND_VIRGINIA +area_MIDWEST+area_NORTHWEST_ROCKYMOUNTAIN+area_OHIO+ area_SOUTH_FLORIDA+area_TENNESSEE+
            refurb_new_R + hnd_webcap + marital_S + ethnic_C +ethnic_N+ethnic_P+ethnic_S+ethnic_U+ethnic_Z+models_2+models_3+models_4+models_5+models_8+hnd_price129.9899902+hnd_price199.9899902+ 
            hnd_price249.9899902+hnd_price299.9899902+actvsubs +uniqsubs_2+uniqsubs_3+uniqsubs_4+uniqsubs_5+uniqsubs_6+uniqsubs_9+drop_vce_Mean + adjmou + totrev + 
            adjrev + avgrev + retdays_dummy + compl_vce_percentage + age1_dummy_Young + optimum,family= binomial(link="logit"), data = training)



summary(mod2)

#Lets remove the insignificant variables from the above model
#mou_Mean,drop_blk_Mean,totcalls, adjqty, ovrrev_Mean, comp_vce_Mean, plcd_vce_Mean, avg3qty, avgqty, avg6mou,  avg6qty,hnd_webcapWC,hnd_webcapWCMB,actvsubs,
#totrev,adjrev,avgrev  

mod3<-glm(churn ~ + totmrc_Mean + change_mou+owylis_vce_Range+mou_opkv_Range+months + eqpdays + iwylis_vce_Mean + rev_Mean + avg3mou + avgmou + avg6mou + 
            asl_flag_Y + prizm_social_one_R +prizm_social_one_T+area_DC_MARYLAND_VIRGINIA +area_MIDWEST+area_NORTHWEST_ROCKYMOUNTAIN+area_OHIO+ area_SOUTH_FLORIDA+area_TENNESSEE+
            refurb_new_R + marital_S + ethnic_C +ethnic_N+ethnic_P+ethnic_S+ethnic_U+ethnic_Z+models_2+models_3+models_4+models_5+models_8+hnd_price129.9899902+hnd_price199.9899902+ 
            hnd_price249.9899902+hnd_price299.9899902+uniqsubs_2+uniqsubs_3+uniqsubs_4+uniqsubs_5+uniqsubs_6+uniqsubs_9+drop_vce_Mean + adjmou + 
            retdays_dummy + compl_vce_percentage + age1_dummy_Young + optimum,family= binomial(link="logit"), data = training)

summary(mod3)

#Lets remove the insignificant variables from the above model
mod4<-glm(churn ~ + totmrc_Mean + change_mou+owylis_vce_Range+mou_opkv_Range+months + eqpdays + iwylis_vce_Mean + rev_Mean + avg3mou + avgmou + avg6mou + 
            asl_flag_Y + prizm_social_one_R +prizm_social_one_T+area_DC_MARYLAND_VIRGINIA +area_MIDWEST+area_NORTHWEST_ROCKYMOUNTAIN+area_OHIO+ area_SOUTH_FLORIDA+area_TENNESSEE+
            refurb_new_R + marital_S + ethnic_C +ethnic_N+ethnic_P+ethnic_S+ethnic_U+ethnic_Z+models_2+models_3+models_4+models_5+models_8+hnd_price129.9899902+hnd_price199.9899902+ 
            hnd_price249.9899902+uniqsubs_2+uniqsubs_3+uniqsubs_4+uniqsubs_5+drop_vce_Mean + adjmou + 
            retdays_dummy + compl_vce_percentage + age1_dummy_Young + optimum,family= binomial(link="logit"), data = training)
summary(mod4)


library(car)
vif(mod4)

#avgmou,avg3mou &avg6mou are showing multicollinearity
#Let's remove avgmou & avg6mou

mod5<-glm(churn ~ + totmrc_Mean + change_mou+owylis_vce_Range+mou_opkv_Range+months + eqpdays + iwylis_vce_Mean + rev_Mean + avg3mou + 
            asl_flag_Y + prizm_social_one_R +prizm_social_one_T+area_DC_MARYLAND_VIRGINIA +area_MIDWEST+area_NORTHWEST_ROCKYMOUNTAIN+area_OHIO+ area_SOUTH_FLORIDA+area_TENNESSEE+
            refurb_new_R + marital_S + ethnic_C +ethnic_N+ethnic_P+ethnic_S+ethnic_U+ethnic_Z+models_2+models_3+models_4+models_5+models_8+hnd_price129.9899902+hnd_price199.9899902+ 
            hnd_price249.9899902+uniqsubs_2+uniqsubs_3+uniqsubs_4+uniqsubs_5+drop_vce_Mean + adjmou + 
            retdays_dummy + compl_vce_percentage + age1_dummy_Young + optimum,family= binomial(link="logit"), data = training)
summary(mod5)

vif(mod5)

#All the variables are significant & there is no multicollinearity.
#We can finalise mod5 as our model 

#Before finalizing the mod5 as our final model, lets tranform the varible "mou_Mean in to category varibales and build another model again to see if we are able to build a better model

#mou_Mean refers to mean number of monthly minutes of use



training$mou_Mean_catg <-ifelse(training$mou_Mean<=250,"Low",
                                 ifelse(training$mou_Mean>250 & training$mou_Mean<=600,"Medium",
                                        ifelse(training$mou_Mean>600 & training$mou_Mean<=1500,"High","Very High")))



training$mou_Mean_catg<-as.factor(training$mou_Mean_catg)
summary(training$mou_Mean_catg)

test$mou_Mean_catg <- ifelse(test$mou_Mean<=250,"Low",
                              ifelse(test$mou_Mean>250 & test$mou_Mean<=600,"Medium",
                                     ifelse(test$mou_Mean>600 & test$mou_Mean<=1500,"High","Very High")))
test$mou_Mean_catg<-as.factor(test$mou_Mean_catg)
summary(test$mou_Mean_catg)

#Lets re-run the above model including mou_Mean_catg

mod6<-glm(churn ~ + totmrc_Mean + change_mou+owylis_vce_Range+mou_opkv_Range+months + eqpdays + iwylis_vce_Mean + rev_Mean + avg3mou + 
            asl_flag_Y + prizm_social_one_R +prizm_social_one_T+area_DC_MARYLAND_VIRGINIA +area_MIDWEST+area_NORTHWEST_ROCKYMOUNTAIN+area_OHIO+ area_SOUTH_FLORIDA+area_TENNESSEE+
            refurb_new_R + marital_S + ethnic_C +ethnic_N+ethnic_P+ethnic_S+ethnic_U+ethnic_Z+models_2+models_3+models_4+models_5+models_8+hnd_price129.9899902+hnd_price199.9899902+ 
            hnd_price249.9899902+uniqsubs_2+uniqsubs_3+uniqsubs_4+uniqsubs_5+drop_vce_Mean + adjmou + 
            retdays_dummy + compl_vce_percentage + age1_dummy_Young + optimum + mou_Mean_catg,family= binomial(link="logit"), data = training)
summary(mod6)

#No change in the AIC value and the variable is not significant



#----------------------------Model Validation-----------------------------------------------------#

#mod 5 is the best model we have got. Hence finalizing it for the model validation

sum(test$churn) #4183 churned customers are there in "test" dataset
sum(training$churn)

sum(test$churn)/nrow(test) #22%

predictedValues<-predict(mod5, type="response", newdata=test )#type="response" link gives us probabilites, it converts log(Odds ratio) values of dependent variable to probabilities of the event occurence 
head(predictedValues)

confint(mod5)

#We need to choose a cut-off value for predicted probabilities to define the churn

#There is no strict rule to decide on a cutoff value, completley depends on the study we are doing

#If correctly identifying positives is important for us,then we should chose a model with higher Sensitivity.
#However, if correctly identifying negatives is more important, then we should choose Specificity as the measurement metric.

#ROCR Curve

length(predictedValues)
pred<-prediction(predictedValues,test$churn)
class(pred)
perf <- performance(pred,"tpr","fpr")

plot(perf,col="red")

abline(0,1, lty=8, col="grey")

#-------------------------To get the values of TPR & FPR at different cutoff values---------#

cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]],tpr=perf@y.values[[1]])

head(cutoffs)
cutoffs=cutoffs[order(cutoffs$tpr, decreasing = TRUE),]

cutoffs$diff=cutoffs$tpr-cutoffs$fpr

#I will chooe a cut-off value where we can achieve the higher "tpr" while having lower "fpr"
cutoffs<-arrange(cutoffs,-cutoffs$diff)

head(cutoffs)

#Cut-off value is -> 0.2345555
#------------------------------------------------------------------------#


predictedValuesCuttoff=ifelse(predictedValues>0.2345555,1,0)
head(predictedValuesCuttoff)

kappa2(data.frame(test$churn,predictedValuesCuttoff)) #Kappa = 0.139 

test$prob <- predict(mod5,type="response",newdata = test)
test$result<-ifelse(predictedValues>0.2345555,1,0)
names(test)
confusionMatrix(as.factor(test$result),as.factor(test$churn),positive = "1") 

##For the cutoff value 0.2357771 

            #Reference
#Prediction   0    1
         #0 7966 1550
         #1 6172 2633

#Accuracy : 0.5785          
#95% CI : (0.5713, 0.5857)

#Sensitivity : 0.6295          
#Specificity : 0.5634 

#Accuracy = TP+TN/(Total)
#Sensitivity = TP/(TP+FN) -> TPR
#Specificity = FP/(FP+TN)
# FPR = 1-Specificity



Auc<-performance(pred,"auc")
Auc<-unlist(slot(Auc,"y.values"))
Auc 

#------------------AUC is turning out be 0.6296755-------------------#

#Higher the AUC the better the model at predicting 0s as os and 1s as 1s.

#According to the solution guide of this project, AUC of 63% is very good

#In the model validation, mod5 is turning out be a good model. Hence, we can finalize it.

#Let's build the gains chart

gain_chart<-gains(test$churn,predict(mod5,type="response",newdata = test),groups=10)

#The gains chart showsthat by targeting top 30% of cutomers by probabilites, on an average we will target 43% of customers who will churn


class(gain_chart)

# plot.gains(gain_chart)
a=gain_chart[1]
b=gain_chart[6]

gains=as.data.frame(c(a,b))
ggplot(data=gains)+geom_line(aes(x=gains$depth ,y=gains$cume.pct.of.total))


#--------------------------------Answering the business questions using the findings from data analysis--------------------------#




#1) What are the top five factors driving likelihood of churn at Mobicom? 



head(sort(abs(mod5$coefficients),decreasing = T),10) #Gives top 10 variables with absolute coefficients 

#optimum  -> 3.9419350 "optimum" is a transformed variable calculated using ovrrev_Mean/totrev                      
#models_8 -> 1.1623453
#ethnic_C -> 1.0723878
#compl_vce_percentage -> 0.7123650
#retdays_dummy1 -> 0.6594879

options(scipen = 999)
summary(mod5)

#4) What would be your recommendation on how to use this churn model for prioritization of customers for a proactive retention campaigns in the future? 

#a) For Usage Based Promotions

View(dec_adjmou)
#We can define billing adjusted total minutes of use over the life of the customer (adjmou) lessthan 4465 as low usage
#4465 is the average of 5th percentile lower & upper bounds

cust_lowuse <- test[test$prob>0.2345555 & test$adjmou>4465,]
nrow(test)
nrow(cust_lowuse)

head(cust_lowuse)

cust_lowuse<-as.data.frame(cust_lowuse)

write.csv(cust_lowuse,file = "Targter Customers for Usage Campaigns.csv")

#we can target the customers with lessthan 4465 adjmou and probability of churn more than 0.2345555 (cut-off value) for proactive usage increase plans


#Rate Plan Migration:  is a strategy to move customers from non-optimal plans to optimal plans as it has been observed that subscribers on non-optimal rate plans
#have significantly higher odds of churn relative to subscribers on optimal rate

quantile(test$optimum,c(p=(1:100)/100))

#We can choose customers for whom the "optimum" is more than 0.2 (20%), which means their overage revenue contributes more than 20% to the total revenue

cust_planMigration <- test[test$prob>0.2345555 & test$optimum>=0.2,]

nrow(cust_planMigration)

cust_planMigration<-as.data.frame(cust_planMigration)

write.csv(cust_planMigration,"Target Customers for Plan Migration1.csv")

#Family Bundiling offer
#We can offer bundle offers to families to reduce churn. 
#For this the target customers can be those whose family has a minimum of 4 unique subscribers in the household and high churn probability. 

str(test$actvsubs_int)

test$actvsubs_int<-as.numeric(test$actvsubs)

cust_familyBundle <- test[test$prob>0.2345555 & test$actvsubs_int>=4,]

nrow(cust_familyBundle)

cust_familyBundle<-as.data.frame(cust_familyBundle)

write.csv(cust_familyBundle,"Target Customers for Family Bundle.csv")


#Annual Retention Offers: One observation from the analysis is that, on an average 42% of the customers who are with Mobicom between 10-12 months tend to leave. 
#Mobicom can target customers who are having high churn probability and are with the carrier for 10-12 months, to an Annual Offer to reduce the churn.

cust_annualRetention <- test[test$prob>0.2345555 & test$months>=10 & test$months<=12,]

nrow(cust_annualRetention)
summary(cust_annualRetention$months)

cust_annualRetention<-as.data.frame(cust_annualRetention)
write.csv(cust_annualRetention,"Target Customers for Annual Retention Offer.csv")




#5) What would be the target segments for proactive retention campaigns? Falling ARPU forecast is also a concern and therefore, Mobicom would like to save their high revenue customers besides managing churn. 
#Given a budget constraint of a contact list of 20% of the subscriber pool, which subscribers should prioritized if "revenue saves" is also a priority besides controlling churn. 
#In other words, controlling churn is the primary objective and revenue saves is the secondary objective.


#Assuming that the customers in test file is our subscriber pool.
gain_chart
#by looking at the gains chart, if we target top 20% of the subscriber pool (test file) by churn probability, we will be able to reach 3664 customers

#lets find out who are those customers

quantile(test$prob,c(p=(1:10)/10))

#top 20% customers have churn probability between 0.2952577 & 0.8142167

#lets extract their data into a new vector

cust_proRet<-test[test$prob>0.2952577,]

nrow(cust_proRet)

cust_proRet<-as.data.frame(cust_proRet)

write.csv(cust_proRet,"Target Customers for Proactive Retention.csv")

#Lets see whom to target based on revenue as priority
#The idea is to focus more on customers who give high revenue to Mobicom and retain them on priority

quantile(test$prob,c(p=(1:100)/100))
quantile(cust_proRet$prob,c(p=(1:10)/10))

cust_proRet$churnLevel<-ifelse(cust_proRet$prob<=0.32,"Low(0.26-0.32)",ifelse(cust_proRet$prob>0.32 & cust_proRet$prob<=0.362,"Medium(0.321-0.362)","High(0.362-0.814)" ))

cust_proRet$churnLevel<-as.factor(cust_proRet$churnLevel)
summary(cust_proRet$churnLevel)

quantile(cust_proRet$totrev,c(p=(1:10)/10))
cust_proRet$RevenueLevel<-ifelse(cust_proRet$totrev<=690.698,"Low(409-691)",ifelse(cust_proRet$totrev>690.698 & cust_proRet$totrev<=1090.136,"Medium(691-1090)","High(1090-27321)" ))

cust_proRet$RevenueLevel<-as.factor(cust_proRet$RevenueLevel)

summary(cust_proRet$RevenueLevel)

table(cust_proRet$churnLevel,cust_proRet$RevenueLevel)
cust_proRet$Customer_ID

#lets extract the customer IDs of those who give high  & medium revenue and have high & medium probability of churn

cust_proRet%>%filter(RevenueLevel=="High(1090-27321)" & churnLevel=="High(0.362-0.814)")->cust_prioTarget1
nrow(cust_prioTarget1)

cust_proRet%>%filter(RevenueLevel=="High(1090-27321)" & churnLevel=="Medium(0.321-0.362)")->cust_prioTarget2
nrow(cust_prioTarget2)

cust_proRet%>%filter(RevenueLevel=="Medium(691-1090)" & churnLevel=="High(0.362-0.814)")->cust_prioTarget3
nrow(cust_prioTarget3)


cust_prioTarget<-rbind(cust_prioTarget1,cust_prioTarget2,cust_prioTarget3)

nrow(cust_prioTarget)
names(cust_prioTarget)

#lets extract customer IDs of priorty target segments

cust_prioTarget<-cust_prioTarget[,c(41,83,84)]

head(cust_prioTarget)

write.csv(cust_prioTarget,"Target for Priority Proactive Retention.csv")

