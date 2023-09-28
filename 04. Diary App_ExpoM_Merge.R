#PROJECT:	  5GOLIAT 
#PURPOSE: 	Merge Clean Diary App files with ExpoM Data
#FILENAME:  04. Diary App_ExpoM_Merge
#CREATED: 	03.05.2023    
#AUTHOR:    A.F.Veludo

#DATA IN: 



#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#


##### 0. Library ####
library(readxl)  # reads excel files
library(readr)      # reads CSV files
library(dplyr)    #handling data
library(reshape)
library(survival)   # creat cross-tables
library(tidyr)
library(stringr) #join columns names
library(lubridate) #work with dates
library(chron) #work with times
library("xlsx") #export xlsx files
library("anytime") #transform dates into readible dates in R

memory.limit(size=56000)

#*------------------------------------------------------------------------------------tail(UK_Day1)

#-------------------------------------------------------------------------------------*#
### 01. UBristol - UK Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#Diary Clean all days
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Diary App_Clean")
UK_DiaryApp<- read.csv("UK_Diary_Clean.csv", header = T, sep = ",")
UK_DiaryApp<- select(UK_DiaryApp, -"X")
UK_DiaryApp$DateTime_start <- anytime(UK_DiaryApp$DateTime_start ) #put in date/times recognized by R
UK_DiaryApp$DateTime_finish<- anytime(UK_DiaryApp$DateTime_finish )#put in date/times recognized by R

#ExpoM all days
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/ExpoM files")
UK_ExpoMData<- read.csv("UK_ExpoMData.csv", header = T, sep = ",")
UK_ExpoMData<- select(UK_ExpoMData, -"X")
UK_ExpoMData$Date_Time <- anytime(UK_ExpoMData$Date_Time)#put in date/times recognized by R


#A. Merge Data####
UK_ExpoM_Diary <- merge(UK_DiaryApp, UK_ExpoMData, all=TRUE)

#takes up a loooot of time - 10min (time to go smoke a cigarette and hope that this is still running)
UK_ExpoM_Data_Clean =  UK_ExpoM_Diary %>% 
  rowwise() %>% 
  mutate(present = any(Date_Time >= DateTime_start & Date_Time <= DateTime_finish)) %>% 
  filter(present) %>% 
  data.frame() #Here we clean the data. Only include the ExpoM Data that fits the Diary App Time
               #this might take a lot of time as there is a lot of data here


colnames(UK_ExpoM_Data_Clean)

names(UK_ExpoM_Data_Clean)[names(UK_ExpoM_Data_Clean)=="X.3"]<- "GPS Fix Mode"
names(UK_ExpoM_Data_Clean)[names(UK_ExpoM_Data_Clean)=="X.4"]<- "GPS Lat"
names(UK_ExpoM_Data_Clean)[names(UK_ExpoM_Data_Clean)=="X.5"]<- "GPS Lon"
names(UK_ExpoM_Data_Clean)[names(UK_ExpoM_Data_Clean)=="X.6"]<- "GPS Altitude"
names(UK_ExpoM_Data_Clean)[names(UK_ExpoM_Data_Clean)=="X.7"]<- "GPS HDOP"
names(UK_ExpoM_Data_Clean)[names(UK_ExpoM_Data_Clean)=="X.8"]<- "GPS# Satellites"
names(UK_ExpoM_Data_Clean)[names(UK_ExpoM_Data_Clean)=="X.9"]<- "GPS Speed"

UK_Data_RMS<- UK_ExpoM_Data_Clean[, c(1:5,17, 6, 18:53, 9:15, 126:132)]
UK_Data_PEAK<- UK_ExpoM_Data_Clean[, c(1:5,17, 6, 18, 54:88, 9:15, 126:132)]
UK_Data_RMSTotal<- UK_ExpoM_Data_Clean[, c(1:5,17, 6, 18, 124, 9:15, 126:132)]
names(UK_Data_RMSTotal)[names(UK_Data_RMSTotal)=="X.1"]<- "Total (RMS)"


#B. SUM Bands_RMS#### 
Sum_RMS <- UK_Data_RMS 
nm_BroadCast = colnames(UK_Data_RMS[,c(9:16, 30)])
nm_UL = colnames(UK_Data_RMS[,c(17, 21, 22, 25, 28, 31)]) #UL bands
nm_DL=colnames(UK_Data_RMS[,c(19:20, 23:24, 26, 29, 33)]) #DL bands
nm_TDD=colnames(UK_Data_RMS[,c(18, 32, 34:36)]) #TDD bands
nm_WiFi=colnames(UK_Data_RMS[,c(37:43)]) #WiFi bands

Sum_Bands2 <- array(NA,dim=c(nrow(Sum_RMS),5))

for (i in 1:nrow(Sum_RMS)) { 
  
  Sum_Broadcast_1 <-sqrt(rowSums(Sum_RMS[i, nm_BroadCast]^2))
  Sum_UL<- sqrt(rowSums(Sum_RMS[i, nm_UL]^2))
  Sum_DL<- sqrt(rowSums(Sum_RMS[i, nm_DL]^2))
  Sum_TDD<- sqrt(rowSums(Sum_RMS[i, nm_TDD]^2))
  Sum_WiFi<- sqrt(rowSums(Sum_RMS[i, nm_WiFi]^2))
  
  
  Sum_Bands2[i, 1]<- Sum_Broadcast_1
  Sum_Bands2[i, 2]<- Sum_UL
  Sum_Bands2[i, 3]<- Sum_DL
  Sum_Bands2[i, 4]<- Sum_TDD
  Sum_Bands2[i, 5]<- Sum_WiFi
  
}



UK_Data_RMS$Broadcasting_Total<- Sum_Bands[,1]
UK_Data_RMS$UL_Total<- Sum_Bands[,2]
UK_Data_RMS$DL_Total<- Sum_Bands[,3]
UK_Data_RMS$TDD_Total<- Sum_Bands[,4]
UK_Data_RMS$WiFi_Total<- Sum_Bands[,5]



#C. SUM Bands_Peak####
Sum_PEAK <- UK_Data_PEAK 
nm_BroadCast = colnames(UK_Data_PEAK[,c(9:16, 30)])
nm_UL = colnames(UK_Data_PEAK[,c(17, 21, 22, 25, 28, 31)]) #UL bands
nm_DL=colnames(UK_Data_PEAK[,c(19:20, 23:24, 26, 29, 33)]) #DL bands
nm_TDD=colnames(UK_Data_PEAK[,c(18, 32, 34:36)]) #TDD bands
nm_WiFi=colnames(UK_Data_PEAK[,c(37:43)]) #WiFi bands

Sum_Bands <- array(NA,dim=c(nrow(Sum_PEAK),5))

for (i in 1:nrow(Sum_PEAK)) { 
  
  Sum_Broadcast_1 <-sqrt(rowSums(Sum_PEAK[i, nm_BroadCast]^2))
  Sum_UL<- sqrt(rowSums(Sum_PEAK[i, nm_UL]^2))
  Sum_DL<- sqrt(rowSums(Sum_PEAK[i, nm_DL]^2))
  Sum_TDD<- sqrt(rowSums(Sum_PEAK[i, nm_TDD]^2))
  Sum_WiFi<- sqrt(rowSums(Sum_PEAK[i, nm_WiFi]^2))
  
  
  Sum_Bands[i, 1]<- Sum_Broadcast_1
  Sum_Bands[i, 2]<- Sum_UL
  Sum_Bands[i, 3]<- Sum_DL
  Sum_Bands[i, 4]<- Sum_TDD
  Sum_Bands[i, 5]<- Sum_WiFi
  
}



UK_Data_PEAK$Broadcasting_Total<- Sum_Bands[,1]
UK_Data_PEAK$UL_Total<- Sum_Bands[,2]
UK_Data_PEAK$DL_Total<- Sum_Bands[,3]
UK_Data_PEAK$TDD_Total<- Sum_Bands[,4]
UK_Data_PEAK$WiFi_Total<- Sum_Bands[,5]




#D. Export Data####
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/ExpoM_Diary_Merged/UK")
write.csv(UK_Data_RMS, "UK_Data_RMS.csv")
write.csv(UK_Data_PEAK, "UK_Data_PEAK.csv")
write.csv(UK_Data_RMSTotal, "UK_Data_RMSTotal.csv")



#*------------------------------------------------------------------------------------tail(UK_Day1)

#-------------------------------------------------------------------------------------*#
### 02. SwissTPH - Swiss Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#1. Diary Clean all days####
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Modified data/02. cleaned_DiaryApp_files_GitHub/Switzerland")
CH_DiaryApp<- read.csv("Diary_App_CH_Y1_Merge.csv", header = T, sep = ",")
CH_DiaryApp<- select(CH_DiaryApp, -"X", -"...1")

#Change times for R formatting
CH_DiaryApp2<- CH_DiaryApp

#Combine Date and Time Start
CH_DiaryApp2$Time_start<- gsub('.{10}$', '', CH_DiaryApp2$Time_start) #Delete last characters of the time string - only up to seconds
CH_DiaryApp2$DateTime_start<- str_c(CH_DiaryApp2$Date_start," ",CH_DiaryApp2$Time_start) 
CH_DiaryApp2$DateTime_start <- anytime(CH_DiaryApp2$DateTime_start) #Put it in Date Format supported by R


#Combine Date and Time Finish
CH_DiaryApp2$Time_finish<- gsub('.{10}$', '', CH_DiaryApp2$Time_finish) #Delete last characters of the time string - only up to seconds
CH_DiaryApp2$DateTime_finish<- str_c(CH_DiaryApp2$Date_finish," ",CH_DiaryApp2$Time_finish) 
CH_DiaryApp2$DateTime_finish <- anytime(CH_DiaryApp2$DateTime_finish) #Put it in Date Format supported by R






#2. ExpoM Data corrected for CrossTalk####
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Modified data/01. ExpoM_correction cross talk")
CH_ExpoMData<- read.csv("ExpoMRF_crosstalk corrected_CH.csv", header = T, sep = ",")
CH_ExpoMData<- select(CH_ExpoMData, -"X")
CH_ExpoMData$datetime <- anytime(CH_ExpoMData$datetime)#put in date/times recognized by R


#Change entries on the 27th of March that had a wrong time
# (i.e., the hour changed and I forgot to update the ExpoM time)

data2<- CH_ExpoMData %>% filter(datetime >= "2023-03-27 05:00:00"& datetime < "2023-03-27 23:00:00")
data2$datetime<- data2$datetime+3600 #1h has 3600seconds so we need to add these to change time

CH_ExpoMData2<- CH_ExpoMData
CH_ExpoMData2[c(48999:55081), 1]<- data2[,1] #put the changed data times in the original Dataset





#3. Merge ExpoM Data with Diary App####
CH_ExpoM_Diary <- merge(CH_DiaryApp2, CH_ExpoMData2, all=TRUE)


#takes up a loooot of time - >10min (time to go smoke a cigarette and hope that this is still running)
CH_ExpoM_Diary_Clean <-  CH_ExpoM_Diary %>% rowwise() %>%
  mutate(present = any(datetime >= DateTime_start & datetime <= DateTime_finish)) %>% 
  filter(present) %>% 
  data.frame() #Here we clean the data. Only include the ExpoM Data that fits the Diary App Time
#this might take a lot of time as there is a lot of data here



#Separate in RMS Values
CH_Data_RMS<- CH_ExpoM_Diary_Clean[, c(1:7,22:23,44, 30:41,47,49:83,119:122)] #RMS values
#Add Total RMS column that was lost during crosstalk
CH_Data_RMS$Total_RMS<- sqrt(rowSums(CH_Data_RMS[,c(24:58)]^2))


#Separate in Peak Values
CH_Data_PEAK<- CH_ExpoM_Diary_Clean[, c(1:7,22:23,44, 30:41,47,84:122)] #Peak values




#D. Export Data####
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Modified data/03. ExpoM_Diary_Merged/CH")
write.csv(CH_Data_RMS, "CH_Data_RMS.csv")
write.csv(CH_Data_PEAK, "CH_Data_PEAK.csv")


#*------------------------------------------------------------------------------------tail(UK_Day1)






#-------------------------------------------------------------------------------------*#
### 03. UGhent - NL Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#Diary Clean all days
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Diary App_Clean")
NL_DiaryApp<- read.csv("NL_Diary_Clean.csv", header = T, sep = ",")
NL_DiaryApp<- select(NL_DiaryApp, -"X")
NL_DiaryApp$DateTime_start <- anytime(NL_DiaryApp$DateTime_start ) #put in date/times recognized by R
NL_DiaryApp$DateTime_finish<- anytime(NL_DiaryApp$DateTime_finish )#put in date/times recognized by R

#ExpoM all days
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/ExpoM files")
NL_ExpoMData<- read.csv("NL_ExpoMData.csv", header = T, sep = ",")
NL_ExpoMData<- select(NL_ExpoMData, -"X")
NL_ExpoMData$Date_Time <- anytime(NL_ExpoMData$Date_Time)#put in date/times recognized by R


#Merge Data
NL_ExpoM_Diary <- merge(NL_DiaryApp, NL_ExpoMData, all=TRUE)

#takes up a loooot of time - 10min (time to go smoke a cigarette and hope that this is still running)
NL_ExpoM_Data_Clean =  NL_ExpoM_Diary %>% 
  rowwise() %>% 
  mutate(present = any(Date_Time >= DateTime_start & Date_Time <= DateTime_finish)) %>% 
  filter(present) %>% 
  data.frame() #Here we clean the data. Only include the ExpoM Data that fits the Diary App Time
#this might take a lot of time as there is a lot of data here


colnames(NL_ExpoM_Data_Clean)

NL_Data_RMS<- NL_ExpoM_Data_Clean[, c(1:5,17, 6, 19:54, 9:15)]
NL_Data_PEAK<- NL_ExpoM_Data_Clean[, c(1:5,17, 6, 19, 55:89, 9:15)]
NL_Data_RMSTotal<- NL_ExpoM_Data_Clean[, c(1:5,17, 6, 19, 125, 9:15)]
names(NL_Data_RMSTotal)[names(NL_Data_RMSTotal)=="X.1"]<- "Total (RMS)"



#B. SUM Bands_RMS#### 
Sum_RMS <- NL_Data_RMS 
nm_BroadCast = colnames(NL_Data_RMS[,c(9:16, 30)]) #Broadcast Bands
nm_UL = colnames(NL_Data_RMS[,c(17, 21, 22, 25, 28, 31)]) #UL bands
nm_DL=colnames(NL_Data_RMS[,c(19:20, 23:24, 26, 29, 33)]) #DL bands
nm_TDD=colnames(NL_Data_RMS[,c(18, 32, 34:36)]) #TDD bands
nm_WiFi=colnames(NL_Data_RMS[,c(37:43)]) #WiFi bands

Sum_Bands_NL <- array(NA,dim=c(nrow(Sum_RMS),5))

for (i in 1:nrow(Sum_RMS)) { 
  
  Sum_Broadcast_1 <-sqrt(rowSums(Sum_RMS[i, nm_BroadCast]^2))
  Sum_UL<- sqrt(rowSums(Sum_RMS[i, nm_UL]^2))
  Sum_DL<- sqrt(rowSums(Sum_RMS[i, nm_DL]^2))
  Sum_TDD<- sqrt(rowSums(Sum_RMS[i, nm_TDD]^2))
  Sum_WiFi<- sqrt(rowSums(Sum_RMS[i, nm_WiFi]^2))
  
  
  Sum_Bands_NL[i, 1]<- Sum_Broadcast_1
  Sum_Bands_NL[i, 2]<- Sum_UL
  Sum_Bands_NL[i, 3]<- Sum_DL
  Sum_Bands_NL[i, 4]<- Sum_TDD
  Sum_Bands_NL[i, 5]<- Sum_WiFi
  
}



NL_Data_RMS$Broadcasting_Total<- Sum_Bands_NL[,1]
NL_Data_RMS$UL_Total<- Sum_Bands_NL[,2]
NL_Data_RMS$DL_Total<- Sum_Bands_NL[,3]
NL_Data_RMS$TDD_Total<- Sum_Bands_NL[,4]
NL_Data_RMS$WiFi_Total<- Sum_Bands_NL[,5]



#C. SUM Bands_Peak####
Sum_PEAK <- NL_Data_PEAK 
nm_BroadCast = colnames(NL_Data_PEAK[,c(9:16, 30)])
nm_UL = colnames(NL_Data_PEAK[,c(17, 21, 22, 25, 28, 31)]) #UL bands
nm_DL=colnames(NL_Data_PEAK[,c(19:20, 23:24, 26, 29, 33)]) #DL bands
nm_TDD=colnames(NL_Data_PEAK[,c(18, 32, 34:36)]) #TDD bands
nm_WiFi=colnames(NL_Data_PEAK[,c(37:43)]) #WiFi bands

Sum_Bands <- array(NA,dim=c(nrow(Sum_PEAK),5))

for (i in 1:nrow(Sum_PEAK)) { 
  
  Sum_Broadcast_1 <-sqrt(rowSums(Sum_PEAK[i, nm_BroadCast]^2))
  Sum_UL<- sqrt(rowSums(Sum_PEAK[i, nm_UL]^2))
  Sum_DL<- sqrt(rowSums(Sum_PEAK[i, nm_DL]^2))
  Sum_TDD<- sqrt(rowSums(Sum_PEAK[i, nm_TDD]^2))
  Sum_WiFi<- sqrt(rowSums(Sum_PEAK[i, nm_WiFi]^2))
  
  
  Sum_Bands[i, 1]<- Sum_Broadcast_1
  Sum_Bands[i, 2]<- Sum_UL
  Sum_Bands[i, 3]<- Sum_DL
  Sum_Bands[i, 4]<- Sum_TDD
  Sum_Bands[i, 5]<- Sum_WiFi
  
}



NL_Data_PEAK$Broadcasting_Total<- Sum_Bands[,1]
NL_Data_PEAK$UL_Total<- Sum_Bands[,2]
NL_Data_PEAK$DL_Total<- Sum_Bands[,3]
NL_Data_PEAK$TDD_Total<- Sum_Bands[,4]
NL_Data_PEAK$WiFi_Total<- Sum_Bands[,5]




#Export Data
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/ExpoM_Diary_Merged/NL")
write.csv(NL_Data_RMS, "NL_Data_RMS.csv")
write.csv(NL_Data_PEAK, "NL_Data_PEAK.csv")
write.csv(NL_Data_RMSTotal, "NL_Data_RMSTotal.csv")



#*------------------------------------------------------------------------------------tail(UK_Day1)
