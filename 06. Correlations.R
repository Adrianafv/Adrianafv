#PROJECT:	  5GOLIAT 
#PURPOSE: 	Correlation between MEs
#FILENAME:  05. Graphs_Preliminary
#CREATED: 	08.05.2023    
#AUTHOR:    A.F.Veludo

#DATA IN: 



#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#


##### 0. Library ####
library(readxl)  # reads excel files
library(ggplot2)
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
library('psych') #geometric mean
library("ggpubr")
library("forcats")
library("scales")
library(viridis)
library(hrbrthemes)
#*-------------------------------------------------------------------------------------------------------------------------------------------------------------------------*#


#1. Import DataSets####

#CH
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Modified data/03. ExpoM_Diary_Merged/CH")
RMS_CH<- read.csv("CH_Data_RMS.csv", header = T, sep = ",")
PEAK_CH<- read.csv("CH_Data_PEAK.csv", header = T, sep = ",")
TOTAL_RMS_CH<- RMS_CH[,c(2:24,60:64)]




#2. Clean names of MEs####

#A. Total RMS
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "bus"]<- "Bus"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "business_area"]<- "Business area"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "playground_park__central"]<- "Park (C)"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "bus_station"]<- "Bus station"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "shopping_cetre"]<- "Shopping Mall"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "residential_area__central"]<- "Residential area (C)"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "train_station"]<- "Train station"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "residential_area__non_central"]<- "Residential area (NC)"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "residential_area__outskirts"]<- "Residential area (OUT)"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "village_school"]<- "School"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "village_centre"]<- "Village centre"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "residential_area"]<- "Residential area"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "playground_park"]<- "Park"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "university"]<- "University"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "highschool"]<- "Highschool"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "basic_school"]<- "Primary school"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "playground_park__on_central"]<- "Park (NC)"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "playground_park__outskirts"]<- "Park (OUT)"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "industrial_area"]<- "Industrial area"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "downtown_area_1"]<- "Downtown area"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "downtown_area_2"]<- "Downtown area"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "tram"]<- "Tram"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "train"]<- "Train"
TOTAL_RMS_CH$ME[TOTAL_RMS_CH$ME== "shopping_centre"]<- "Shopping Mall"


#B. RMS countries
unique(RMS_CH$ME)
RMS_CH$ME[RMS_CH$ME== "bus"]<- "Bus"
RMS_CH$ME[RMS_CH$ME== "business_area"]<- "Business area"
RMS_CH$ME[RMS_CH$ME== "playground_park__central"]<- "Park (C)"
RMS_CH$ME[RMS_CH$ME== "bus_station"]<- "Bus station"
RMS_CH$ME[RMS_CH$ME== "shopping_cetre"]<- "Shopping Mall"
RMS_CH$ME[RMS_CH$ME== "residential_area__central"]<- "Residential area (C)"
RMS_CH$ME[RMS_CH$ME== "train_station"]<- "Train station"
RMS_CH$ME[RMS_CH$ME== "residential_area__non_central"]<- "Residential area (NC)"
RMS_CH$ME[RMS_CH$ME== "residential_area__outskirts"]<- "Residential area (OUT)"
RMS_CH$ME[RMS_CH$ME== "village_school"]<- "School"
RMS_CH$ME[RMS_CH$ME== "village_centre"]<- "Village centre"
RMS_CH$ME[RMS_CH$ME== "residential_area"]<- "Residential area"
RMS_CH$ME[RMS_CH$ME== "playground_park"]<- "Park"
RMS_CH$ME[RMS_CH$ME== "university"]<- "University"
RMS_CH$ME[RMS_CH$ME== "highschool"]<- "Highschool"
RMS_CH$ME[RMS_CH$ME== "basic_school"]<- "Primary school"
RMS_CH$ME[RMS_CH$ME== "playground_park__on_central"]<- "Park (NC)"
RMS_CH$ME[RMS_CH$ME== "playground_park__outskirts"]<- "Park (OUT)"
RMS_CH$ME[RMS_CH$ME== "industrial_area"]<- "Industrial area"
RMS_CH$ME[RMS_CH$ME== "downtown_area_1"]<- "Downtown area"
RMS_CH$ME[RMS_CH$ME== "downtown_area_2"]<- "Downtown area"
RMS_CH$ME[RMS_CH$ME== "tram"]<- "Tram"
RMS_CH$ME[RMS_CH$ME== "train"]<- "Train"
RMS_CH$ME[RMS_CH$ME== "shopping_centre"]<- "Shopping Mall"


#C. Peak countries
PEAK_CH$ME[PEAK_CH$ME== "bus"]<- "Bus"
PEAK_CH$ME[PEAK_CH$ME== "business_area"]<- "Business area"
PEAK_CH$ME[PEAK_CH$ME== "playground_park__central"]<- "Park (C)"
PEAK_CH$ME[PEAK_CH$ME== "bus_station"]<- "Bus station"
PEAK_CH$ME[PEAK_CH$ME== "shopping_cetre"]<- "Shopping Mall"
PEAK_CH$ME[PEAK_CH$ME== "residential_area__central"]<- "Residential area (C)"
PEAK_CH$ME[PEAK_CH$ME== "train_station"]<- "Train station"
PEAK_CH$ME[PEAK_CH$ME== "residential_area__non_central"]<- "Residential area (NC)"
PEAK_CH$ME[PEAK_CH$ME== "residential_area__outskirts"]<- "Residential area (OUT)"
PEAK_CH$ME[PEAK_CH$ME== "village_school"]<- "School"
PEAK_CH$ME[PEAK_CH$ME== "village_centre"]<- "Village centre"
PEAK_CH$ME[PEAK_CH$ME== "residential_area"]<- "Residential area"
PEAK_CH$ME[PEAK_CH$ME== "playground_park"]<- "Park"
PEAK_CH$ME[PEAK_CH$ME== "university"]<- "University"
PEAK_CH$ME[PEAK_CH$ME== "highschool"]<- "Highschool"
PEAK_CH$ME[PEAK_CH$ME== "basic_school"]<- "Primary school"
PEAK_CH$ME[PEAK_CH$ME== "playground_park__on_central"]<- "Park (NC)"
PEAK_CH$ME[PEAK_CH$ME== "playground_park__outskirts"]<- "Park (OUT)"
PEAK_CH$ME[PEAK_CH$ME== "industrial_area"]<- "Industrial area"
PEAK_CH$ME[PEAK_CH$ME== "downtown_area_1"]<- "Downtown area"
PEAK_CH$ME[PEAK_CH$ME== "downtown_area_2"]<- "Downtown area"
PEAK_CH$ME[PEAK_CH$ME== "tram"]<- "Tram"
PEAK_CH$ME[PEAK_CH$ME== "train"]<- "Train"
PEAK_CH$ME[PEAK_CH$ME== "shopping_centre"]<- "Shopping Mall"




#3. Data Visualization####
#Use only Total RMS for now

View(TOTAL_RMS_CH)

#Select variables of interest
TOTAL_RMS_CH2<- TOTAL_RMS_CH[,c(5:10,14:19, 24:28)]

#Transform V/m to W/m2
TOTAL_CH2<- TOTAL_RMS_CH2
TOTAL_CH2$Total_Wm<- ((TOTAL_CH2$Total_RMS^2)/377)*1000 


#Filter per usage scenario
TRMS_NU<- TOTAL_CH2%>%filter(Usage_scenario=="non_user")
TRMS_DL<- TOTAL_CH2%>%filter(Usage_scenario=="user_max_dl")
TRMS_UL<- TOTAL_CH2%>%filter(Usage_scenario=="user_max_ul")



#TRansform Data for Correlations
TRMS_NU_wider- TRMS_NU%>%
  pivot_wider(names_from = ME, values_from = Total_Wm)
