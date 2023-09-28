#PROJECT:	  5GOLIAT 
#PURPOSE: 	Merge 2 Diary App files
#FILENAME:  01. Diary App_Merge
#CREATED: 	24.04.2023    
#AUTHOR:    A.F.Veludo

#DATA IN: 
# ODK Diary App_GOLIAT_17.02.2023.csv
# ODK Diary App_GOLIAT_17.02.2023-Micro_environment.csv


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



#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
### 01. SwissTPH - Switzerland Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#

#Set directory to import data
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Diary App/Diary App exports/SwissTPH/V1")


#A. Import latest Diary App data - Data up to 05.04.2023####
Swiss<- read.csv("ODK Diary App_GOLIAT_05.04.2023.csv", header = T, sep = ",")
Swiss_ME<- read.csv("ODK Diary App_GOLIAT_05.04.2023-Micro_environment.csv", header = T, sep = ",")

names(Swiss)[names(Swiss)=="KEY"]<- "PARENT_KEY"

Swiss_ME_V2<-left_join(Swiss,Swiss_ME, by=c("PARENT_KEY"), copy=FALSE, keep=FALSE) #Merge with data above

colnames(Swiss_ME_V2)

#Take out the columns that do not interest us
CH_Diary_V2<- select(Swiss_ME_V2,-"SubmissionDate",-"start",-"end",-"SubmitterID", 
                     -"X__version__" , - "SubmitterName",  -"instanceID", -"AttachmentsPresent",
                     -"AttachmentsExpected",-"Status",
                     -"DeviceID",-"Edits",-"FormVersion", -"KEY", -"PARENT_KEY")

#**************************************************##**************************************************#
#**************************************************##**************************************************#


#B. Merge MEs into one column####
#Need to put Outdoor area V as not NA, otherwise it will create NA's when merged
CH_Diary_V2[is.na(CH_Diary_V2)] <- ""

CH_Diary_V2$ME<- str_c(CH_Diary_V2$Outdoor_area_SC,"", CH_Diary_V2$public_place_BC_SC,"",
                       CH_Diary_V2$public_transport,CH_Diary_V2$Outdoor_area_V, "", CH_Diary_V2$Public_place_V,
                       "", CH_Diary_V2$Outdoor_area_BC) 

colnames(CH_Diary_V2)
#Select Final Columns
CH_Diary_V3<- select(CH_Diary_V2, "Research_centre", "Date", "Measured_country", "Location_of_Measurement",
                     "Usage_scenario", "ME", "Date_start", "Time_start","Date_finish", "Time_finish",
                     "PubTrans_Capacity", "Type_station", "Train_type","Train_class", "Train_floor",
                     "phones_school", "Comments", "Time_addon", "micro_environment_type", "ReviewState")




#**************************************************##**************************************************#
#**************************************************##**************************************************#


#C. Change times####
CH_DiaryApp<- CH_Diary_V3

#Combine Date and Time Start
CH_DiaryApp$Time_start<- gsub('.{10}$', '', CH_DiaryApp$Time_start) #Delete last characters of the time string - only up to seconds
CH_DiaryApp$DateTime_start<- str_c(CH_DiaryApp$Date_start," ",CH_DiaryApp$Time_start) 
CH_DiaryApp$DateTime_start <- anytime(CH_DiaryApp$DateTime_start) #Put it in Date Format supported by R


#Combine Date and Time Finish
CH_DiaryApp$Time_finish<- gsub('.{10}$', '', CH_DiaryApp$Time_finish) #Delete last characters of the time string - only up to seconds
CH_DiaryApp$DateTime_finish<- str_c(CH_DiaryApp$Date_finish," ",CH_DiaryApp$Time_finish) 
CH_DiaryApp$DateTime_finish <- anytime(CH_DiaryApp$DateTime_finish) #Put it in Date Format supported by R


#Select Final Columns
CH_DiaryApp<- select(CH_DiaryApp, "Research_centre", "Date", "Measured_country", "Location_of_Measurement",
                     "Usage_scenario", "ME", "DateTime_start", "DateTime_finish",
                     "PubTrans_Capacity", "Type_station", "Train_type","Train_class", "Train_floor",
                     "phones_school", "Comments", "Time_addon", "micro_environment_type", "ReviewState")


#**************************************************##**************************************************#
#**************************************************##**************************************************#

#D. Export Data ####
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Diary App_Merged")
write.csv(CH_DiaryApp, "CH_Diary_Merged.csv")


#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#




#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
### 02. NPHC - Hungary Data ######
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#

#Set directory to import data
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Diary App/Diary App exports/NPHC") #Import the latest data


#A. Import latest Diary App data - Data up to 05.04.2023####
NPHC<- read.csv("ODK Diary App_GOLIAT_05.04.2023.csv", header = T, sep = ",")
NPHC_ME<- read.csv("ODK Diary App_GOLIAT_05.04.2023-Micro_environment.csv", header = T, sep = ",")

write.csv(NPHC_ME, "NPHC_ME.csv")
write.csv(descriptives, "descriptives_overall.csv")

names(NPHC)[names(NPHC)=="KEY"]<- "PARENT_KEY"

NPHC_ME_V2<-left_join(NPHC,NPHC_ME, by=c("PARENT_KEY"), copy=FALSE, keep=FALSE) #Merge with data above. Two removed because there is no date? line 9 and 18

colnames(NPHC_ME_V2)

#Take out the columns that do not interest us
NPHC_ME_V3<- select(NPHC_ME_V2,-"SubmissionDate",-"start",-"end",-"SubmitterID", 
                     -"X__version__" , - "SubmitterName",  -"instanceID", -"AttachmentsPresent",
                     -"AttachmentsExpected",-"Status",-"ReviewState", 
                     -"DeviceID",-"Edits",-"FormVersion", -"KEY", -"PARENT_KEY")


#**************************************************##**************************************************#
#**************************************************##**************************************************#



#B. Merge MEs into one column####
#Need to put Outdoor area V as not NA, otherwise it will create NA's when merged
NPHC_ME_V3[is.na(NPHC_ME_V3)] <- ""

NPHC_ME_V3$ME<- str_c(NPHC_ME_V3$Outdoor_area_SC,"", NPHC_ME_V3$public_place_BC_SC,"",
                       NPHC_ME_V3$public_transport,NPHC_ME_V3$Outdoor_area_V, "", NPHC_ME_V3$Public_place_V,
                       "", NPHC_ME_V3$Outdoor_area_BC) 


#Select Final Columns
NPHC_ME_V4<- select(NPHC_ME_V3, "Research_centre", "Date", "Measured_country", "Location_of_Measurement",
                     "Usage_scenario", "ME", "Date_start", "Time_start", "Time_finish",
                     "PubTrans_Capacity", "Type_station", "Train_type","Train_class", "Train_floor",
                     "phones_school", "Comments", "Time_addon", "micro_environment_type")



#**************************************************##**************************************************#
#**************************************************##**************************************************#


#C. Change times#### TO BE DONE FOR HUNGARY
UK_DiaryApp<- UK_Diary_V3

#Combine Date and Time Start
UK_DiaryApp$Time_start<- gsub('.{10}$', '', UK_DiaryApp$Time_start) #Delete last characters of the time string - only up to seconds
UK_DiaryApp$DateTime_start<- str_c(UK_DiaryApp$Date_start," ",UK_DiaryApp$Time_start) 
UK_DiaryApp$DateTime_start <- anytime(UK_DiaryApp$DateTime_start) #Put it in Date Format supported by R


#Combine Date and Time Finish
UK_DiaryApp$Time_finish<- gsub('.{10}$', '', UK_DiaryApp$Time_finish) #Delete last characters of the time string - only up to seconds
UK_DiaryApp$DateTime_finish<- str_c(UK_DiaryApp$Date_finish," ",UK_DiaryApp$Time_finish) 
UK_DiaryApp$DateTime_finish <- anytime(UK_DiaryApp$DateTime_finish) #Put it in Date Format supported by R


#Select Final Columns
UK_DiaryApp<- select(UK_DiaryApp, "Research_centre", "Date", "Measured_country", "Location_of_Measurement",
                     "Usage_scenario", "ME", "DateTime_start", "DateTime_finish",
                     "PubTrans_Capacity", "Type_station", "Train_type","Train_class", "Train_floor",
                     "phones_school", "Comments", "Time_addon", "micro_environment_type")


#**************************************************##**************************************************#
#**************************************************##**************************************************#


#D. In case you want to export these files####
write.xlsx(NPHC_ME_V4, "NPHC_Diary_Data_Simplyfied.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx(NPHC_ME_V3, "NPHC_Diary_Data.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)



#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#





#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
### 03. SwissTPH - UK Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#

#Set directory to import data
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Diary App/Diary App exports/SwissTPH/V2")



#A. Import latest Diary App data - Data up to 24.04.2023#### 
UK<- read.csv("ODK Diary App_GOLIAT_V2_17.05.2023.csv", header = T, sep = ",") #Diary app data up to 24.04.2023
UK_ME<- read.csv("ODK Diary App_GOLIAT_V2_17.05.2023-Micro_environment.csv", header = T, sep = ",") #Diary app data up to 24.04.2023



names(UK)[names(UK)=="KEY"]<- "PARENT_KEY" #Change column name so we can merge both Diary App Data

UK_Diary<-left_join(UK,UK_ME, by=c("PARENT_KEY"), copy=FALSE, keep=FALSE) #Merge with data above


#Take out the columns that do not interest us
UK_Diary_V2<- select(UK_Diary,-"SubmissionDate",-"start",-"end",-"SubmitterID", 
                     -"X__version__" , - "SubmitterName",  -"instanceID", -"AttachmentsPresent",
                     -"AttachmentsExpected",-"Status",-"ReviewState", 
                     -"DeviceID",-"Edits",-"FormVersion", -"KEY", -"PARENT_KEY")

#**************************************************##**************************************************#
#**************************************************##**************************************************#


#B. Merge MEs into one column####
#Need to put Outdoor area V as not NA, otherwise it will create NA's when merged
UK_Diary_V2[is.na(UK_Diary_V2)] <- ""

UK_Diary_V2$ME<- str_c(UK_Diary_V2$Outdoor_area_SC,"", UK_Diary_V2$public_place_BC_SC,"",
                       UK_Diary_V2$public_transport,UK_Diary_V2$Outdoor_area_V, "", UK_Diary_V2$Public_place_V,
                       "", UK_Diary_V2$Outdoor_area_BC) 


#Select Final Columns
UK_Diary_V3<- select(UK_Diary_V2, "Research_centre", "Date", "Measured_country", "Location_of_Measurement",
                     "Usage_scenario", "ME", "Date_start", "Time_start","Date_finish", "Time_finish",
                     "PubTrans_Capacity", "Type_station", "Train_type","Train_class", "Train_floor",
                     "phones_school", "Comments", "Time_addon", "micro_environment_type")



#Put scenario in one that is missing (V3 - bus stop)
UK_Diary_V3<- UK_Diary_V3 %>% mutate(Usage_scenario = ifelse(Usage_scenario == "" 
                                                     & ME == "bus_station", "non_user", Usage_scenario))

#**************************************************##**************************************************#
#**************************************************##**************************************************#


#C. Change times####
UK_DiaryApp<- UK_Diary_V3

#Combine Date and Time Start
UK_DiaryApp$Time_start<- gsub('.{10}$', '', UK_DiaryApp$Time_start) #Delete last characters of the time string - only up to seconds
UK_DiaryApp$DateTime_start<- str_c(UK_DiaryApp$Date_start," ",UK_DiaryApp$Time_start) 
UK_DiaryApp$DateTime_start <- anytime(UK_DiaryApp$DateTime_start) #Put it in Date Format supported by R


#Combine Date and Time Finish
UK_DiaryApp$Time_finish<- gsub('.{10}$', '', UK_DiaryApp$Time_finish) #Delete last characters of the time string - only up to seconds
UK_DiaryApp$DateTime_finish<- str_c(UK_DiaryApp$Date_finish," ",UK_DiaryApp$Time_finish) 
UK_DiaryApp$DateTime_finish <- anytime(UK_DiaryApp$DateTime_finish) #Put it in Date Format supported by R

#Set-up the correct time (i.e., UK is one hour behind)
UK_DiaryApp$DateTime_start<- UK_DiaryApp$DateTime_start-3600 #3600 is the number of seconds in an hour
UK_DiaryApp$DateTime_finish<- UK_DiaryApp$DateTime_finish-3600 #3600 is the number of seconds in an hour


#Select Final Columns
UK_DiaryApp<- select(UK_DiaryApp, "Research_centre", "Date", "Measured_country", "Location_of_Measurement",
                     "Usage_scenario", "ME", "DateTime_start", "DateTime_finish",
                     "PubTrans_Capacity", "Type_station", "Train_type","Train_class", "Train_floor",
                     "phones_school", "Comments", "Time_addon", "micro_environment_type")





#**************************************************##**************************************************#
#**************************************************##**************************************************#

#D. Export Data ####
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Diary App_Merged")
write.csv(UK_DiaryApp, "UK_Diary_Merged.csv")
#setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Diary App_Clean/UK") #In this case there is not much cleaning to do with the Diary App so the file is the same
#write.csv(UK_DiaryApp, "UK_Diary_Clean.csv")



#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#


#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
### 04. UGhent - Belgium Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#





#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#


#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
### 05. UGhent - Netherlands Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#

#Set directory to import data
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Diary App/Diary App exports/UGhent/V2")


#A. Import latest Diary App data - Data up to 05.04.2023####
Dutch<- read.csv("ODK Diary App_GOLIAT_V2_08.05.2023.csv", header = T, sep = ",")
Dutch_ME<- read.csv("ODK Diary App_GOLIAT_V2_08.05.2023-Micro_environment.csv", header = T, sep = ",")

names(Dutch)[names(Dutch)=="KEY"]<- "PARENT_KEY"

Dutch_ME_V2<-left_join(Dutch,Dutch_ME, by=c("PARENT_KEY"), copy=FALSE, keep=FALSE) #Merge with data above
Dutch_ME_V2<- Dutch_ME_V2%>%filter(Measured_country=="netherlands")

colnames(Dutch_ME_V2)

#Take out the columns that do not interest us
NL_Diary_V2<- select(Dutch_ME_V2,-"SubmissionDate",-"start",-"end",-"SubmitterID", 
                     -"X__version__" , - "SubmitterName",  -"instanceID", -"AttachmentsPresent",
                     -"AttachmentsPresent",-"Status",
                     -"DeviceID",-"Edits",-"FormVersion", -"KEY", -"PARENT_KEY")

#**************************************************##**************************************************#
#**************************************************##**************************************************#


#B. Merge MEs into one column####
#Need to put Outdoor area V as not NA, otherwise it will create NA's when merged
NL_Diary_V2[is.na(NL_Diary_V2)] <- ""

NL_Diary_V2$ME<- str_c(NL_Diary_V2$Outdoor_area_SC,"", NL_Diary_V2$public_place_BC_SC,"",
                       NL_Diary_V2$public_transport,NL_Diary_V2$Outdoor_area_V, "", NL_Diary_V2$Public_place_V,
                       "", NL_Diary_V2$Outdoor_area_BC) 

colnames(NL_Diary_V2)
#Select Final Columns
NL_Diary_V3<- select(NL_Diary_V2, "Research_centre", "Date", "Measured_country", "Location_of_Measurement",
                     "Usage_scenario", "ME", "Date_start", "Time_start","Date_finish", "Time_finish",
                     "PubTrans_Capacity", "Type_station", "Train_type","Train_class", "Train_floor",
                     "phones_school", "Comments", "Time_addon", "micro_environment_type", "ReviewState")




#**************************************************##**************************************************#
#**************************************************##**************************************************#


#C. NLange times####
NL_DiaryApp<- NL_Diary_V3

#Combine Date and Time Start
NL_DiaryApp$Time_start<- gsub('.{10}$', '', NL_DiaryApp$Time_start) #Delete last NLaracters of the time string - only up to seconds
NL_DiaryApp$DateTime_start<- str_c(NL_DiaryApp$Date_start," ",NL_DiaryApp$Time_start) 
NL_DiaryApp$DateTime_start <- anytime(NL_DiaryApp$DateTime_start) #Put it in Date Format supported by R


#Combine Date and Time Finish
NL_DiaryApp$Time_finish<- gsub('.{10}$', '', NL_DiaryApp$Time_finish) #Delete last NLaracters of the time string - only up to seconds
NL_DiaryApp$DateTime_finish<- str_c(NL_DiaryApp$Date_finish," ",NL_DiaryApp$Time_finish) 
NL_DiaryApp$DateTime_finish <- anytime(NL_DiaryApp$DateTime_finish) #Put it in Date Format supported by R


#Select Final Columns
NL_DiaryApp<- select(NL_DiaryApp, "Research_centre", "Date", "Measured_country", "Location_of_Measurement",
                     "Usage_scenario", "ME", "DateTime_start", "DateTime_finish",
                     "PubTrans_Capacity", "Type_station", "Train_type","Train_class", "Train_floor",
                     "phones_school", "Comments", "Time_addon", "micro_environment_type", "ReviewState")


#**************************************************##**************************************************#
#**************************************************##**************************************************#

#D. Export Data ####
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Diary App_Merged")
write.csv(NL_DiaryApp, "NL_Diary_Merged.csv")





#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#

#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
### 06. TP - France Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#






#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#


#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
### 07. IS GLOBAL - Spain Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#





#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#


#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
### 08. IS GLOBAL - Austria Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#


#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#


#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
### 09. CNR - Italy Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#


#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#



#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
### 10. NIOM - Poland Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#


#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
