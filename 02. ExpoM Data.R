#PROJECT:	  5GOLIAT 
#PURPOSE: 	Download ExpoM-RF4 Files
#FILENAME:  02. ExpoM Data
#CREATED: 	24.04.2023    
#AUTHOR:    A.F.Veludo

#DATA IN: 
# ODK Diary App_GOLIAT_17.02.2023.csv
# ODK Diary App_GOLIAT_17.02.2023-Micro_environment.csv


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
library("anytime")   

#*------------------------------------------------------------------------------------tail(UK_Day1)
#-------------------------------------------------------------------------------------*#
### SwissTPH - SRM Data #### 
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
setwd("U:/GOLIAT/Task 1.2.2 Spectrum Analyzer/Measurements 2023/Spectrum analyzer/ExpoM Data")


#A. Import ExpoM DataSets and Deleted Last entries
#Day 9, 10 and 11 were excluded because the measurements
#on these days were repeated in another day

CH_Day1<- read.csv("Export_ID22125_2023-07-05_103833_CAL.csv", header = F, skip = 12, sep = "\t")#skip rows that are not important
col_names<- c("Band Names","FM Radio","DAB/DAB+",	"Polycom / TETRAPOL",
              "TETRAPOL, amateur, ISM 433",	"PMR/PAMR",	"Broadcasting (1)",
              "Broadcasting (2)",	"Broadcasting (3)",	"Mobile 700 UL",	"Mobile 700 TDD",
              "Mobile 700 DL",	"Mobile 800 DL",	"Mobile 800 UL",	"Mobile 900 UL",	"Mobile 900 DL",
              "Mobile 1400 SDL","Mobile 1800 UL",	"Mobile 1800 DL",	"DECT",	"Mobile 2100 UL",
              "Mobile 2100 DL",	"ISM 2.4 GHz",	"Mobile 2600 UL",	"Mobile 2600 TDD",
              "Mobile 2600 DL",	"Mobile 3500 (1)",	"Mobile 3500 (2)",	"Mobile 3500 (3)",
              "WiFi 5 GHz (1)",	"WiFi 5 GHz (2)",	"WiFi 5 GHz (3)",	"WiFi 5 GHz (4)",	"WiFi 5 GHz (5)",
              "WiFi / SRD 5.8 GHz (1)",	"WiFi / SRD 5.8 GHz (2)",	FM Radio_PEAK,	DAB/DAB+_PEAK,	Polycom / TETRAPOL_PEAK	TETRAPOL, amateur, ISM 433_PEAK,
              PMR/PAMR_PEAK,	Broadcasting (1)_PEAK,	Broadcasting (2)_PEAK,	Broadcasting (3)_PEAK,
              Mobile 700 UL_PEAK,	Mobile 700 TDD_PEAK,	Mobile 700 DL_PEAK,	Mobile 800 DL_PEAK,
              Mobile 800 UL_PEAK,	Mobile 900 UL_PEAK,	Mobile 900 DL_PEAK,	Mobile 1400 SDL_PEAK,
              Mobile 1800 UL_PEAK,	Mobile 1800 DL_PEAK,	DECT	Mobile 2100 UL_PEAK,
              Mobile 2100 DL_PEAK,	ISM 2.4 GHz_PEAK,	Mobile 2600 UL_PEAK,	Mobile 2600 TDD_PEAK,
              Mobile 2600 DL_PEAK,	Mobile 3500 (1)_PEAK,	Mobile 3500 (2)_PEAK,	Mobile 3500 (3)_PEAK,
              WiFi 5 GHz (1)_PEAK,	WiFi 5 GHz (2)_PEAK,	WiFi 5 GHz (3)_PEAK,	WiFi 5 GHz (4)_PEAK,
              WiFi 5 GHz (5)_PEAK,	WiFi / SRD 5.8 GHz (1)_PEAK,	WiFi / SRD 5.8 GHz (2)_PEAK,
              FM Radio_6MINAVG,	DAB/DAB+_6MINAVG,	Polycom / TETRAPOL_6MINAVG,	TETRAPOL, amateur, ISM 433_6MINAVG,
              PMR/PAMR_6MINAVG,	Broadcasting (1)_6MINAVG,	Broadcasting (2)_6MINAVG,	Broadcasting (3)_6MINAVG,
              Mobile 700 UL_6MINAVG,	Mobile 700 TDD_6MINAVG,	Mobile 700 DL_6MINAVG,
              Mobile 800 DL_6MINAVG,	Mobile 800 UL_6MINAVG,	Mobile 900 UL_6MINAVG,	Mobile 900 DL_6MINAVG,
              Mobile 1400 SDL_6MINAVG,	Mobile 1800 UL_6MINAVG,	Mobile 1800 DL_6MINAVG,
              DECT_6MINAVG,	Mobile 2100 UL_6MINAVG,	Mobile 2100 DL_6MINAVG,	ISM 2.4 GHz_6MINAVG,
              Mobile 2600 UL_6MINAVG,	Mobile 2600 TDD_6MINAVG,	Mobile 2600 DL_6MINAVG,
              Mobile 3500 (1)_6MINAVG,	Mobile 3500 (2)_6MINAVG,	Mobile 3500 (3)_6MINAVG,
              WiFi 5 GHz (1)_6MINAVG,	WiFi 5 GHz (2)_6MINAVG,	WiFi 5 GHz (3)_6MINAVG,	WiFi 5 GHz (4)_6MINAVG,
              WiFi 5 GHz (5)_6MINAVG,	WiFi / SRD 5.8 GHz (1)_6MINAVG,	WiFi / SRD 5.8 GHz (2)_6MINAVG,
              Total (RMS),	Total (6MIN AVG),	GPS Fix Mode,	GPS Lat,	GPS Lon,	GPS Altitude,
              GPS HDOP,	"GPS# Satellites",	GPS Speed	Marker,	Battery charge (%),	Battery voltage (mV))


CH_Day1 <- slice(CH_Day1, 3:(n() - 2))     # Apply slice & n functions

CH_Day2<- read.csv("Export_ID22125_2023-02-22_SC_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
CH_Day2 <- slice(CH_Day2, 3:(n() - 2))     # Apply slice & n functions

CH_Day3<- read.csv("Export_ID22125_2023-02-24_SC_LC_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
CH_Day3 <- slice(CH_Day3, 3:(n() - 2))     # Apply slice & n functions

CH_Day4<- read.csv("Export_ID22125_2023-03-02_LC_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
CH_Day4 <- slice(CH_Day4, 3:(n() - 2))     # Apply slice & n functions

CH_Day5<- read.csv("Export_ID22125_2023-03-07_SC_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
CH_Day5 <- slice(CH_Day5, 3:(n() - 2))     # Apply slice & n functions

CH_Day6<- read.csv("Export_ID22125_2023-03-08_SC_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
CH_Day6 <- slice(CH_Day6, 3:(n() - 2))     # Apply slice & n functions

CH_Day7<- read.csv("Export_ID22125_2023-03-10_SC_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
CH_Day7 <- slice(CH_Day7, 3:(n() - 2))     # Apply slice & n functions

CH_Day8<- read.csv("Export_ID22125_2023-03-14_SC_LC_SC_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
CH_Day8 <- slice(CH_Day8, 3:(n() - 2))     # Apply slice & n functions

#CH_Day9<- read.csv("Export_ID22125_2023-03-20_SC_LC_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
#CH_Day9 <- slice(CH_Day9, 3:(n() - 2))     # Apply slice & n functions

#CH_Day10<- read.csv("Export_ID22125_2023-03-23_LC_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
#CH_Day10 <- slice(CH_Day10, 3:(n() - 2))     # Apply slice & n functions

#CH_Day11<- read.csv("Export_ID22125_2023-03-24_LC_Switzerland_Failed.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
#CH_Day11 <- slice(CH_Day11, 3:(n() - 2))     # Apply slice & n functions

CH_Day12<- read.csv("Export_ID22125_2023-03-27_V3_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
CH_Day12 <- slice(CH_Day12, 3:(n() - 2))     # Apply slice & n functions

CH_Day13<- read.csv("Export_ID22125_2023-03-28_SC_LC_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
CH_Day13 <- slice(CH_Day13, 3:(n() - 2))     # Apply slice & n functions

CH_Day14<- read.csv("Export_ID22125_2023-03-29_SC_V3_V1_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
CH_Day14 <- slice(CH_Day14, 3:(n() - 2))     # Apply slice & n functions

CH_Day15<- read.csv("Export_ID22125_2023-03-30_V1_SC_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
CH_Day15 <- slice(CH_Day15, 3:(n() - 2))     # Apply slice & n functions

CH_Day16<- read.csv("Export_ID22125_2023-03-31_V2_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
CH_Day16 <- slice(CH_Day16, 3:(n() - 2))     # Apply slice & n functions

CH_Day17<- read.csv("Export_ID22125_2023-04-03_LC_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
CH_Day17 <- slice(CH_Day17, 3:(n() - 2))     # Apply slice & n functions

CH_Day18<- read.csv("Export_ID22125_2023-04-04_LC_Switzerland.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
CH_Day18 <- slice(CH_Day18, 3:(n() - 2))     # Apply slice & n functions

#**************************************************##**************************************************#
#**************************************************##**************************************************#


#B. Merge ExpoM Datasets####
CH_ExpoMData<- rbind(CH_Day1, CH_Day2, CH_Day3, CH_Day4, CH_Day5, CH_Day6, CH_Day7, CH_Day8,
                     #CH_Day9, CH_Day10, CH_Day11, 
                     CH_Day12, CH_Day13, CH_Day14, CH_Day15, CH_Day16,
                     CH_Day17, CH_Day18)

#**************************************************##**************************************************#
#**************************************************##**************************************************#


#C. Clean Data####
CH_ExpoMData2<- CH_ExpoMData%>% select(-X)
names(CH_ExpoMData2)[names(CH_ExpoMData2)=="Band.Names"]<- "Date_Time"

#Change time on ExpoM Data
CH_ExpoMData2$Date_Time <- anytime(CH_ExpoMData2$Date_Time)

#Change entries on the 27th of March that had a wrong time
# (i.e., the hour changed and I forgot to update the ExpoM time)

data<- CH_ExpoMData2 %>% filter(Date_Time >= "2023-03-27 05:00:00"&Date_Time < "2023-03-27 23:00:00")
data$Date_Time<- data$Date_Time+3600 #1h has 3600seconds so we need to add these to change time

CH_ExpoMData3<- CH_ExpoMData2
CH_ExpoMData3[c(48999:55081), 1]<- data[,1] #put the changed data times in the original Dataset

#**************************************************##**************************************************#
#**************************************************##**************************************************#


#D. Export Data #### 
# Every column that ends up in .1 are PEAK VALUES - it had to give a new name
# given that the columns names are the same. All the columns that do not finish 
# in .1 are the RMS values for each band

setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/ExpoM files")
write.csv(CH_ExpoMData3, "CH_ExpoMData.csv")




#*------------------------------------------------------------------------------------tail(UK_Day1)

#-------------------------------------------------------------------------------------*#
### 04. UGhent - NL Data ####
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Netherlands/ExpoM-RF4 export")

#A. Import ExpoM DataSets and Deleted Last entries
NL_Day1<- read.csv("Export_ID22122_2023-04-17_133749_UT_day1.csv", header = T, skip = 10, sep = ";")#skip rows that are not important
NL_Day1 <- slice(NL_Day1, 3:(n() - 2))     # Apply slice & n functions

NL_Day2<- read.csv("Export_ID22122_2023-04-18_090752_Utrecht_Day_2.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
NL_Day2 <- slice(NL_Day2, 3:(n() - 2))     # Apply slice & n functions

NL_Day3<- read.csv("Export_ID22122_2023-04-19_093202_UT_day3.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
NL_Day3 <- slice(NL_Day3, 3:(n() - 2))     # Apply slice & n functions

NL_Day4<- read.csv("Export_ID22122_2023-04-20_134833_UT_day4.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
NL_Day4 <- slice(NL_Day4, 3:(n() - 2))     # Apply slice & n functions

NL_Day5<- read.csv("Export_ID22122_2023-04-21_090921_UT_day5.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
NL_Day5 <- slice(NL_Day5, 3:(n() - 2))     # Apply slice & n functions

NL_Day6<- read.csv("Export_ID22122_2023-04-24_114940_UT_day6.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
NL_Day6 <- slice(NL_Day6, 3:(n() - 2))     # Apply slice & n functions

NL_Day7<- read.csv("Export_ID22122_2023-04-25_090841_UT_day7.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
NL_Day7 <- slice(NL_Day7, 3:(n() - 2))     # Apply slice & n functions

NL_Day8<- read.csv("Export_ID22122_2023-04-26_094016_Cothen.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
NL_Day8 <- slice(NL_Day8, 3:(n() - 2))     # Apply slice & n functions

NL_Day9<- read.csv("Export_ID22122_2023-05-01_094502_doorn.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
NL_Day9 <- slice(NL_Day9, 3:(n() - 2))     # Apply slice & n functions

NL_Day10<- read.csv("Export_ID22122_2023-05-02_101323_odijk.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
NL_Day10 <- slice(NL_Day10, 3:(n() - 2))     # Apply slice & n functions

NL_Day11<- read.csv("Export_ID22122_2023-05-03_102326_maarn_bunnik.csv", header = T, sep = ";", skip = 10)#skip rows that are not important
NL_Day11 <- slice(NL_Day11, 3:(n() - 2))     # Apply slice & n functions


#**************************************************##**************************************************#
#**************************************************##**************************************************#


#B. Merge ExpoM Datasets####
NL_ExpoMData<- rbind(NL_Day1, NL_Day2, NL_Day3, NL_Day4, NL_Day5, NL_Day6, NL_Day7, NL_Day8, 
                     NL_Day9, NL_Day10, NL_Day11)

#**************************************************##**************************************************#
#**************************************************##**************************************************#


#C. Clean Data####
NL_ExpoMData2<- NL_ExpoMData%>% select(-X)
names(NL_ExpoMData2)[names(NL_ExpoMData2)=="Band.Names"]<- "Date_Time"

#Change time on ExpoM Data
NL_ExpoMData2$Date_Time <- anytime(NL_ExpoMData2$Date_Time)


#**************************************************##**************************************************#
#**************************************************##**************************************************#


#D. Export Data #### 
# Every column that ends up in .1 are PEAK VALUES - it had to give a new name
# given that the columns names are the same. All the columns that do not finish 
# in .1 are the RMS values for each band

setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/ExpoM files")
write.csv(NL_ExpoMData2, "NL_ExpoMData.csv")




