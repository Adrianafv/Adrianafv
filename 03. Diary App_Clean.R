#PROJECT:	  5GOLIAT 
#PURPOSE: 	Clean Diary App Files
#FILENAME:  03. Diary App_Clean
#CREATED: 	    
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



#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
### 01. SwissTPH - Switzerland Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Diary App_Merged")
DApp_CH<- read.csv("CH_Diary_Merged.csv", header = T, sep = ",")


#A. Select Dates of final measurements ####
#(some dates will be discarded because some measurements did not work out)

dates<- c("2023-02-20", "2023-02-22", "2023-02-24", "2023-03-02", "2023-03-07",
          "2023-03-08", "2023-03-10", "2023-03-14", "2023-03-27", "2023-03-28",
          "2023-03-29", "2023-03-30", "2023-03-31", "2023-04-03", "2023-04-04")

DApp_CH2<- DApp_CH %>% filter(Date %in% dates) #only dates on final measurements



#B . Clean Wrong Diary Entries ####

#B1. Discard empty Diary App entries##
# Can see in the that some entries do not have any info on user-scenario
# These were labeled as Discard, Discard measurement or Ignore
# Also in the Review State there is one measurement to be rejected - this was not an 
# actual measurement, it was a Diary App entry to test the app


#We start deleting the Rejected from the Review State
DClean_CH<- DApp_CH2%>% filter(ReviewState != "rejected")

#Then the Discard/Ignore measurements
DClean_CH$Comments
DClean_CH2<- DClean_CH%>% filter(Comments != "Discard" & Comments != "Discard measurement" & Comments != "Ignore")



#C . Clean Wrong Diary App Start/Finish Times####
# Here we will put the right start and finish times that were removed 
# from the Diary App due to problems in the App
# The information on the correct Start/Finish times is in a Document called
# Schedule_Measurements Switzerland.xlsx

#C1. Mutate Start times individually##
# 22nd February
DClean_CH3<- DClean_CH2 %>% mutate(DateTime_start = ifelse(DateTime_finish == "2023-02-22 10:46:28" 
                                                     & ME == "bus", "2023-02-22 10:29:16", DateTime_start))

# 7th March
DClean_CH4<- DClean_CH3 %>% mutate(DateTime_start = ifelse(DateTime_finish == "2023-03-07 12:47:06", "2023-03-07 12:35:00", DateTime_start))

DClean_CH4<- DClean_CH4 %>% mutate(DateTime_start = ifelse(DateTime_finish == "2023-03-07 13:00:02" 
                                                           & ME == "tram", "2023-03-07 12:54:15", DateTime_start))

# 29th March
DClean_CH4<- DClean_CH4 %>% mutate(DateTime_start = ifelse(DateTime_finish == "2023-03-29 14:03:26" 
                                                           & ME == "industrial_area", "2023-03-29 13:50:10", DateTime_start))

# 30th March
DClean_CH4<- DClean_CH4 %>% mutate(DateTime_start = ifelse(DateTime_finish == "2023-03-30 09:52:28" 
                                                           & ME == "residential_area", "2023-03-30 09:37:50", DateTime_start))

# 4th April
DClean_CH4<- DClean_CH4 %>% mutate(DateTime_start = ifelse(is.na(DateTime_finish) 
                                                           & is.na(DateTime_start), "2023-04-04 10:37:17", DateTime_start))

DClean_CH4<- DClean_CH4 %>% mutate(DateTime_finish = ifelse(DateTime_start == "2023-04-04 10:37:17" 
                                                           , "2023-04-04 10:51:35", DateTime_finish))



#D . Delete entries that had problems ####
# Eg. On the 29.03 I started measuring on a shopping mall 
# but then realized that this was not a real shopping mall so
# will not include in the final measurement. Also, on the 07.03 we measured a park
# and these measurements were repeated on another date because some of the phone settings
# were not correct.

#Park non-central 7th March
DClean_CH5<- DClean_CH4%>% filter(DateTime_start != "2023-03-07 10:47:55" & DateTime_start != "2023-03-07 11:02:51" & DateTime_start != "2023-03-07 11:18:30") #park measurements repeated another day


#Bus DL and Train DL_UL  27th March
DClean_CH5<- DClean_CH5%>% filter(DateTime_start != "2023-03-27 14:52:16" & DateTime_start != "2023-03-27 16:08:49" & DateTime_start != "2023-03-27 16:31:45") #wrong settings on Qualipoc for these measurements

#Shopping mall and DL train 29th March
DClean_CH5<- DClean_CH5%>% filter(DateTime_start != "2023-03-29 13:37:56") #shopping mall removed from measurements
DClean_CH5<- DClean_CH5%>% filter(DateTime_start != "2023-03-29 08:35:44") #Train DL failed


#E . Export Diary App Cleaned ####
DClean_CH5<- select(DClean_CH5, -"X", -"ReviewState") #remove these columns that we don't want

setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Diary App_Clean")
write.csv(DClean_CH5, "CH_Diary_Clean.csv")



#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
### 02. SwissTPH - UK Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Diary App_Merged")
DApp_UK<- read.csv("UK_Diary_Merged.csv", header = T, sep = ",")


#A. Select Dates of final measurements ####
#(some dates will be discarded because some measurements did not work out)

dates<- c("2023-04-21") #the only day that we need to remove
`%!in%` <- Negate(`%in%`)
DApp_UK2<- DApp_UK %>% filter(Date %!in% dates) #only dates on final measurements




#B . Delete entries that had problems ####
# Eg. Some of the measurements on the 17/04 were repeated on 
# another day, but some we can still keep. 

#14th April
DClean_UK<- DApp_UK2%>% filter(DateTime_start != "2023-04-14 07:32:55") #station measurements repeated another day


#Downtown, Central Residential and Industrial 17th March - repeated another day
DClean_UK<- DClean_UK%>% filter(DateTime_start != "2023-04-17 11:24:24" & DateTime_start != "2023-04-17 11:41:08" 
                                  & DateTime_start != "2023-04-17 11:57:35" & DateTime_start != "2023-04-17 12:17:59") #Industrial area repeated another day

DClean_UK<- DClean_UK%>% filter(DateTime_start != "2023-04-17 07:51:49" & DateTime_start != "2023-04-17 08:08:07" 
                                & DateTime_start != "2023-04-17 08:26:40") #Downtown area repeated another day

DClean_UK<- DClean_UK%>% filter(DateTime_start != "2023-04-17 09:24:43" & DateTime_start != "2023-04-17 09:41:38" 
                                & DateTime_start != "2023-04-17 09:58:38") #Central residential area repeated another day

#Park non central 18th April
DClean_UK<- DClean_UK%>% filter(DateTime_start != "2023-04-18 09:31:47" & DateTime_start != "2023-04-18 09:47:14" 
                                & DateTime_start != "2023-04-18 10:05:00") #Park non-central  repeated another day




#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
### 03. UGhent - NL Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Diary App_Merged")
DApp_NL<- read.csv("NL_Diary_Merged.csv", header = T, sep = ",")


#A. Delete Wrong entries ####
#(entries that are there by mistake)

DApp_NL2<- DApp_NL %>% filter(!is.na(DateTime_start))



#B. Mutate Finish times individually## 
# This will have to be double checked at the end of the 
# measurement campaign!!


# 8th May 
DApp_NL2<- DApp_NL2 %>% mutate(DateTime_finish = ifelse(DateTime_start == "2023-05-08 12:28:35" 
                                                           & ME == "industrial_area", "2023-05-08 12:43:40", DateTime_finish))

# 19th April
DApp_NL2<- DApp_NL2 %>% mutate(DateTime_finish = ifelse(DateTime_start == "2023-04-19 11:14:22" 
                                                        & ME == "playground_park__central", "2023-04-19 11:30:02", DateTime_finish))


#C . Export Diary App Cleaned ####
DClean_NL<- select(DApp_NL2, -"X") #remove these columns that we don't want

setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Diary App_Clean")
write.csv(DClean_NL, "NL_Diary_Clean.csv")