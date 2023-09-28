#PROJECT:	  5GOLIAT 
#PURPOSE: 	Initial Graphs_Barcelona Meeting
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
#UK
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/ExpoM_Diary_Merged/UK")
RMS_UK<- read.csv("UK_Data_RMS.csv", header = T, sep = ",")
PEAK_UK<- read.csv("UK_Data_PEAK.csv", header = T, sep = ",")
TOTAL_RMS_UK<- read.csv("UK_Data_RMSTotal.csv", header = T, sep = ",")

#CH
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/ExpoM_Diary_Merged/CH")
RMS_CH<- read.csv("CH_Data_RMS.csv", header = T, sep = ",")
PEAK_CH<- read.csv("CH_Data_PEAK.csv", header = T, sep = ",")
TOTAL_RMS_CH<- read.csv("CH_Data_RMSTotal.csv", header = T, sep = ",")

#NL
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/ExpoM_Diary_Merged/NL")
RMS_NL<- read.csv("NL_Data_RMS.csv", header = T, sep = ",")
PEAK_NL<- read.csv("NL_Data_PEAK.csv", header = T, sep = ",")
TOTAL_RMS_NL<- read.csv("NL_Data_RMSTotal.csv", header = T, sep = ",")


#Join countries
TotalRMS<- rbind(TOTAL_RMS_UK, TOTAL_RMS_CH, TOTAL_RMS_NL)
#RMS_countries<- rbind(RMS_UK, RMS_CH, RMS_NL)
RMS_countries<- RMS_CH
Peak_countries<- rbind(PEAK_UK, PEAK_CH, PEAK_NL)



#Change names for residential areas UK
unique(TotalRMS$ME)

TotalRMS$ME[TotalRMS$ME=="residential_area__central2" | 
              TotalRMS$ME=="residential_area__central1"] <- "residential_area__central"

TotalRMS$ME[TotalRMS$ME=="residential_area__non_central2" | 
              TotalRMS$ME=="residential_area__non_central1"] <- "residential_area__non_central"

TotalRMS$ME[TotalRMS$ME=="residential_area__outskirts2" | 
              TotalRMS$ME=="residential_area__outskirts1"] <- "residential_area__outskirts"

RMS_countries$ME[RMS_countries$ME=="residential_area__central2" | 
                   RMS_countries$ME=="residential_area__central1"] <- "residential_area__central"

RMS_countries$ME[RMS_countries$ME=="residential_area__non_central2" | 
                   RMS_countries$ME=="residential_area__non_central1"] <- "residential_area__non_central"

RMS_countries$ME[RMS_countries$ME=="residential_area__outskirts2" | 
                   RMS_countries$ME=="residential_area__outskirts1"] <- "residential_area__outskirts"

Peak_countries$ME[Peak_countries$ME=="residential_area__central2" | 
                    Peak_countries$ME=="residential_area__central1"] <- "residential_area__central"

Peak_countries$ME[Peak_countries$ME=="residential_area__non_central2" | 
                    Peak_countries$ME=="residential_area__non_central1"] <- "residential_area__non_central"

Peak_countries$ME[Peak_countries$ME=="residential_area__outskirts2" | 
                    Peak_countries$ME=="residential_area__outskirts1"] <- "residential_area__outskirts"





#Clean names

#A. Total RMS
TotalRMS$ME[TotalRMS$ME== "bus"]<- "Bus"
TotalRMS$ME[TotalRMS$ME== "business_area"]<- "Business area"
TotalRMS$ME[TotalRMS$ME== "playground_park__central"]<- "Park (C)"
TotalRMS$ME[TotalRMS$ME== "bus_station"]<- "Bus station"
TotalRMS$ME[TotalRMS$ME== "shopping_cetre"]<- "Shopping Mall"
TotalRMS$ME[TotalRMS$ME== "residential_area__central"]<- "Residential area (C)"
TotalRMS$ME[TotalRMS$ME== "train_station"]<- "Train station"
TotalRMS$ME[TotalRMS$ME== "residential_area__non_central"]<- "Residential area (NC)"
TotalRMS$ME[TotalRMS$ME== "residential_area__outskirts"]<- "Residential area (OUT)"
TotalRMS$ME[TotalRMS$ME== "village_school"]<- "School"
TotalRMS$ME[TotalRMS$ME== "village_centre"]<- "Village centre"
TotalRMS$ME[TotalRMS$ME== "residential_area"]<- "Residential area"
TotalRMS$ME[TotalRMS$ME== "playground_park"]<- "Park"
TotalRMS$ME[TotalRMS$ME== "university"]<- "University"
TotalRMS$ME[TotalRMS$ME== "highschool"]<- "Highschool"
TotalRMS$ME[TotalRMS$ME== "basic_school"]<- "Primary school"
TotalRMS$ME[TotalRMS$ME== "playground_park__on_central"]<- "Park (NC)"
TotalRMS$ME[TotalRMS$ME== "playground_park__outskirts"]<- "Park (OUT)"
TotalRMS$ME[TotalRMS$ME== "industrial_area"]<- "Industrial area"
TotalRMS$ME[TotalRMS$ME== "downtown_area_1"]<- "Downtown area"
TotalRMS$ME[TotalRMS$ME== "downtown_area_2"]<- "Downtown area"
TotalRMS$ME[TotalRMS$ME== "tram"]<- "Tram"
TotalRMS$ME[TotalRMS$ME== "train"]<- "Train"
TotalRMS$ME[TotalRMS$ME== "shopping_centre"]<- "Shopping Mall"


#B. RMS countries
RMS_countries$ME[RMS_countries$ME== "bus"]<- "Bus"
RMS_countries$ME[RMS_countries$ME== "business_area"]<- "Business area"
RMS_countries$ME[RMS_countries$ME== "playground_park__central"]<- "Park (C)"
RMS_countries$ME[RMS_countries$ME== "bus_station"]<- "Bus station"
RMS_countries$ME[RMS_countries$ME== "shopping_cetre"]<- "Shopping Mall"
RMS_countries$ME[RMS_countries$ME== "residential_area__central"]<- "Residential area (C)"
RMS_countries$ME[RMS_countries$ME== "train_station"]<- "Train station"
RMS_countries$ME[RMS_countries$ME== "residential_area__non_central"]<- "Residential area (NC)"
RMS_countries$ME[RMS_countries$ME== "residential_area__outskirts"]<- "Residential area (OUT)"
RMS_countries$ME[RMS_countries$ME== "village_school"]<- "School"
RMS_countries$ME[RMS_countries$ME== "village_centre"]<- "Village centre"
RMS_countries$ME[RMS_countries$ME== "residential_area"]<- "Residential area"
RMS_countries$ME[RMS_countries$ME== "playground_park"]<- "Park"
RMS_countries$ME[RMS_countries$ME== "university"]<- "University"
RMS_countries$ME[RMS_countries$ME== "highschool"]<- "Highschool"
RMS_countries$ME[RMS_countries$ME== "basic_school"]<- "Primary school"
RMS_countries$ME[RMS_countries$ME== "playground_park__on_central"]<- "Park (NC)"
RMS_countries$ME[RMS_countries$ME== "playground_park__outskirts"]<- "Park (OUT)"
RMS_countries$ME[RMS_countries$ME== "industrial_area"]<- "Industrial area"
RMS_countries$ME[RMS_countries$ME== "downtown_area_1"]<- "Downtown area"
RMS_countries$ME[RMS_countries$ME== "downtown_area_2"]<- "Downtown area"
RMS_countries$ME[RMS_countries$ME== "tram"]<- "Tram"
RMS_countries$ME[RMS_countries$ME== "train"]<- "Train"
RMS_countries$ME[RMS_countries$ME== "shopping_centre"]<- "Shopping Mall"


#C. Peak countries
Peak_countries$ME[Peak_countries$ME== "bus"]<- "Bus"
Peak_countries$ME[Peak_countries$ME== "business_area"]<- "Business area"
Peak_countries$ME[Peak_countries$ME== "playground_park__central"]<- "Park (C)"
Peak_countries$ME[Peak_countries$ME== "bus_station"]<- "Bus station"
Peak_countries$ME[Peak_countries$ME== "shopping_cetre"]<- "Shopping Mall"
Peak_countries$ME[Peak_countries$ME== "residential_area__central"]<- "Residential area (C)"
Peak_countries$ME[Peak_countries$ME== "train_station"]<- "Train station"
Peak_countries$ME[Peak_countries$ME== "residential_area__non_central"]<- "Residential area (NC)"
Peak_countries$ME[Peak_countries$ME== "residential_area__outskirts"]<- "Residential area (OUT)"
Peak_countries$ME[Peak_countries$ME== "village_school"]<- "School"
Peak_countries$ME[Peak_countries$ME== "village_centre"]<- "Village centre"
Peak_countries$ME[Peak_countries$ME== "residential_area"]<- "Residential area"
Peak_countries$ME[Peak_countries$ME== "playground_park"]<- "Park"
Peak_countries$ME[Peak_countries$ME== "university"]<- "University"
Peak_countries$ME[Peak_countries$ME== "highschool"]<- "Highschool"
Peak_countries$ME[Peak_countries$ME== "basic_school"]<- "Primary school"
Peak_countries$ME[Peak_countries$ME== "playground_park__on_central"]<- "Park (NC)"
Peak_countries$ME[Peak_countries$ME== "playground_park__outskirts"]<- "Park (OUT)"
Peak_countries$ME[Peak_countries$ME== "industrial_area"]<- "Industrial area"
Peak_countries$ME[Peak_countries$ME== "downtown_area_1"]<- "Downtown area"
Peak_countries$ME[Peak_countries$ME== "downtown_area_2"]<- "Downtown area"
Peak_countries$ME[Peak_countries$ME== "tram"]<- "Tram"
Peak_countries$ME[Peak_countries$ME== "train"]<- "Train"
Peak_countries$ME[Peak_countries$ME== "shopping_centre"]<- "Shopping Mall"


#Some Values have a comma instead of a point
#this needs to be changed otherwise R does not read it correctly
TotalRMS$Total..RMS.<- as.numeric(sub(',','.',TotalRMS$Total..RMS.))                                                                                                                                                                                                                                                                                                                                  


###
#Morning presentation####
#Only show Total RMS values for the micro-environments in 3 secondary cities
###


#Filter per Location_Measurements

Sec_city<- TotalRMS%>% filter(Location_of_Measurement=="secondary_city")
Sec_city<- Sec_city %>% mutate(Location_of_Measurement = ifelse(Measured_country == "united_kingdom", "Bristol (EN)", Location_of_Measurement))
Sec_city<- Sec_city %>% mutate(Location_of_Measurement = ifelse(Measured_country == "switzerland", "Basel (CH)", Location_of_Measurement))
Sec_city<- Sec_city %>% mutate(Location_of_Measurement = ifelse(Measured_country == "netherlands", "Utrecht (NL)", Location_of_Measurement))

Sec_city$Usage_scenario[Sec_city$Usage_scenario=="non_user"]<- "Non user"
Sec_city$Usage_scenario[Sec_city$Usage_scenario=="user_max_dl"]<- "User Max DL"
Sec_city$Usage_scenario[Sec_city$Usage_scenario=="user_max_ul"]<- "User Max UL"


#Divide per type of microenvironment
Sec_city_OA<- Sec_city%>%filter(micro_environment_type=="outdoor_area")
Sec_city_PS<- Sec_city%>%filter(micro_environment_type=="public_place")
Sec_city_PT<- Sec_city%>%filter(micro_environment_type=="public_transport")

Sec_city_OA_country<- Sec_city_OA %>% group_by(Location_of_Measurement) %>% summarise(Median_RMS = median(Total..RMS.,na.rm = T), Geomean_RMS= geometric.mean(Total..RMS.,na.rm = T), Mean_RMS = mean(Total..RMS.,na.rm = T))
Sec_city_OA_country_US<- Sec_city_OA %>% group_by(Location_of_Measurement, Usage_scenario) %>% summarise(Median_RMS = median(Total..RMS.,na.rm = T), Geomean_RMS= geometric.mean(Total..RMS.,na.rm = T), Mean_RMS = mean(Total..RMS.,na.rm = T))
Sec_city_OA_country_ME<- Sec_city_OA %>% group_by(Location_of_Measurement, ME) %>% summarise(Median_RMS = median(Total..RMS.,na.rm = T), Geomean_RMS= geometric.mean(Total..RMS.,na.rm = T), Mean_RMS = mean(Total..RMS.,na.rm = T))
Sec_city_OA_country_ME_US<- Sec_city_OA %>% group_by(Location_of_Measurement,ME, Usage_scenario) %>% summarise(Median_RMS = median(Total..RMS.,na.rm = T), Geomean_RMS= geometric.mean(Total..RMS.,na.rm = T), Mean_RMS = mean(Total..RMS.,na.rm = T))


  
  
#village<- RMS_UK_CH%>% filter(Location_of_Measurement=="village_1" | Location_of_Measurement=="village_2" |
                                    #Location_of_Measurement=="village_3")
#village_OA<- village%>%filter(Type_ME=="Outdoor area")
#village_PS<- village%>%filter(Type_ME=="Public Space")
#village_PT<- village%>%filter(Type_ME=="Public Transport")



OA_boxplot<- Sec_city_OA %>% ggplot(mapping = aes(x = ME, y = Total..RMS.)) +
  geom_boxplot(aes(color= Usage_scenario),  width= 0.7, na.rm = T, size=0.8)+ facet_wrap(~ factor(Location_of_Measurement, levels = c("Basel (CH)", "Bristol (EN)", "Utrecht (NL)")) , ncol = 1)+
  scale_color_manual(values=c("#66c2a5", "#fc8d62", "#8da0cb"))+scale_y_continuous(breaks = c(0, 2, 4, 6, 8),limits = c(0,8))+
  labs(x="", y="Total RMS Values (V/m)", colour="Usage scenario")+ theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text.x = element_text(size =12,vjust = 0.5, angle = 90),
                                                axis.text.y = element_text(size =12), strip.text.x = element_text(size = 12),
                                                strip.text.y = element_text(size = 12), legend.title = element_text(size = 12, hjust=0.5), 
                                                legend.text = element_text(size =12), plot.title = element_text(hjust=0.05, size = 13), panel.spacing.y = unit(1, "lines")) + ggtitle("A. Outdoor areas")



Ps_boxplot<- Sec_city_PS %>% ggplot(mapping = aes(x = ME, y = Total..RMS.)) +
  geom_boxplot(aes(color= Usage_scenario),  width= 0.7, na.rm = T, size=0.8)+ facet_wrap(~ factor(Location_of_Measurement, levels = c("Basel (CH)", "Bristol (EN)", "Utrecht (NL)")) , ncol = 1)+
  scale_color_manual(values=c("#66c2a5", "#fc8d62", "#8da0cb"))+ scale_y_continuous(breaks = c(0, 2, 4, 6, 8),limits = c(0,8))+
  labs(x="", y="Total RMS Values (V/m)", colour="Usage scenario")+ theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text.x = element_text(size =12,vjust = 0.5, angle = 90),
                                                                         axis.text.y = element_text(size =12), strip.text.x = element_text(size = 12),
                                                                         strip.text.y = element_text(size = 12), legend.title = element_text(size = 12, hjust=0.5), 
                                                                         legend.text = element_text(size =12), plot.title = element_text(hjust=0.05, size = 13), panel.spacing.y = unit(1, "lines")) + ggtitle("B. Public Spaces")


Pt_boxplot<- Sec_city_PT %>% ggplot(mapping = aes(x = ME, y = Total..RMS.)) +
  geom_boxplot(aes(color= Usage_scenario),  width= 0.7, na.rm = T, size=0.8)+ facet_wrap(~ factor(Location_of_Measurement, levels = c("Basel (CH)", "Bristol (EN)", "Utrecht (NL)")) , ncol = 1)+
  scale_color_manual(values=c("#66c2a5", "#fc8d62", "#8da0cb"))+scale_y_continuous(breaks = c(0, 2, 4, 6, 8),limits = c(0,8))+
  labs(x="", y="Total RMS Values (V/m)", colour="Usage scenario")+ theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text.x = element_text(size =12,vjust = 0.5, angle = 90),
                                                                         axis.text.y = element_text(size =12), strip.text.x = element_text(size = 12),
                                                                         strip.text.y = element_text(size = 12), legend.title = element_text(size = 12, hjust=0.5), 
                                                                         legend.text = element_text(size =12), plot.title = element_text(hjust=0.05, size = 13), panel.spacing.y = unit(1, "lines")) + ggtitle("C. Public Transports")




fig_SC<- ggarrange(Sec_boxplot, Ps_boxplot, Pt_boxplot, ncol=1, nrow = 3, common.legend = T)
fig_SC<- annotate_figure(fig_SC, left = text_grob("ExpoM-RF4 Measured Values (V/m)", rot=90, size = 15), right = text_grob("Daily max temperature \n [ºC]", rot=270, size = 15), bottom = text_grob("Date", size=13))




#B. Switzerland only comparisons
switzerland<- TotalRMS%>% filter(Measured_country=="switzerland")
switzerland$Location_of_Measurement[switzerland$Location_of_Measurement=="secondary_city"]<- "Basel"
switzerland$Location_of_Measurement[switzerland$Location_of_Measurement=="capital_big_city"]<- "Zürich"
switzerland$Location_of_Measurement[switzerland$Location_of_Measurement=="village_1"]<- "Dagmersellen"
switzerland$Location_of_Measurement[switzerland$Location_of_Measurement=="village_2"]<- "Hergiswil"
switzerland$Location_of_Measurement[switzerland$Location_of_Measurement=="village_3"]<- "Willisau"

OA<- switzerland%>%filter(micro_environment_type=="outdoor_area")
PS<- switzerland%>%filter(micro_environment_type=="public_place")
PT<- switzerland%>%filter(micro_environment_type=="public_transport")


OA_CH<- OA %>% ggplot(mapping = aes(x = ME, y = Total..RMS.)) +
  geom_boxplot(aes(color= Usage_scenario),  width= 0.7, na.rm = T, size=0.8)+ facet_wrap(~ factor(Location_of_Measurement, levels = c("Zürich", "Basel", "Dagmersellen", "Hergiswil", "Willisau")) , ncol = 1)+
  scale_color_manual(values=c("#66c2a5", "#fc8d62", "#8da0cb"))+scale_y_continuous(breaks = c(0, 2, 4, 6, 8),limits = c(0,8))+
  labs(x="", y="Total RMS Values (V/m)", colour="Usage scenario")+ theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text.x = element_text(size =12,vjust = 0.5, angle = 90),
                                                                         axis.text.y = element_text(size =12), strip.text.x = element_text(size = 12),
                                                                         strip.text.y = element_text(size = 12), legend.title = element_text(size = 12, hjust=0.5), 
                                                                         legend.text = element_text(size =12), plot.title = element_text(hjust=0.05, size = 13), panel.spacing.y = unit(1, "lines")) + ggtitle("A. Outdoor areas")



Ps_CH<- PS %>% ggplot(mapping = aes(x = ME, y = Total..RMS.)) +
  geom_boxplot(aes(color= Usage_scenario),  width= 0.7, na.rm = T, size=0.8)+ facet_wrap(~ factor(Location_of_Measurement, levels = c("Zürich", "Basel", "Dagmersellen", "Hergiswil", "Willisau")) , ncol = 1)+
  scale_color_manual(values=c("#66c2a5", "#fc8d62", "#8da0cb"))+ scale_y_continuous(breaks = c(0, 2, 4, 6, 8),limits = c(0,8))+
  labs(x="", y="Total RMS Values (V/m)", colour="Usage scenario")+ theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text.x = element_text(size =12,vjust = 0.5, angle = 90),
                                                                         axis.text.y = element_text(size =12), strip.text.x = element_text(size = 12),
                                                                         strip.text.y = element_text(size = 12), legend.title = element_text(size = 12, hjust=0.5), 
                                                                         legend.text = element_text(size =12), plot.title = element_text(hjust=0.05, size = 13), panel.spacing.y = unit(1, "lines")) + ggtitle("B. Public Spaces")


Pt_CH<- PT %>% ggplot(mapping = aes(x = ME, y = Total..RMS.)) +
  geom_boxplot(aes(color= Usage_scenario),  width= 0.7, na.rm = T, size=0.8)+ facet_wrap(~ factor(Location_of_Measurement, levels = c("Zürich", "Basel", "Dagmersellen", "Hergiswil", "Willisau")) , ncol = 1)+
  scale_color_manual(values=c("#66c2a5", "#fc8d62", "#8da0cb"))+scale_y_continuous(breaks = c(0, 2, 4, 6, 8),limits = c(0,8))+
  labs(x="", y="Total RMS Values (V/m)", colour="Usage scenario")+ theme(panel.background = element_blank(),axis.line = element_line(colour = "black"), axis.text.x = element_text(size =12,vjust = 0.5, angle = 90),
                                                                         axis.text.y = element_text(size =12), strip.text.x = element_text(size = 12),
                                                                         strip.text.y = element_text(size = 12), legend.title = element_text(size = 12, hjust=0.5), 
                                                                         legend.text = element_text(size =12), plot.title = element_text(hjust=0.05, size = 13), panel.spacing.y = unit(1, "lines")) + ggtitle("C. Public Transports")




#B. Switzerland RMS values - HeatMaps - Basel
#switzerland_RMS<- RMS_countries%>% filter(Measured_country=="switzerland")
switzerland_RMS<- RMS_countries

Basel<- switzerland_RMS%>% filter(Location_of_Measurement=="secondary_city")

#sum bands
#B. SUM Bands_RMS#### 
Sum_RMS <- switzerland_RMS 
nm_BroadCast = colnames(switzerland_RMS[,c(10:17, 31)])
nm_3500TDD=colnames(switzerland_RMS[,c(35:37)]) #3500TDD bands
nm_WiFi=colnames(switzerland_RMS[,c(38:44)]) #WiFi bands

Sum_Bands <- array(NA,dim=c(nrow(Sum_RMS),3))

for (i in 1:nrow(Sum_RMS)) { 
  
  Sum_Broadcast_1 <-sqrt(rowSums(Sum_RMS[i, nm_BroadCast]^2))
  Sum_3500TDD<- sqrt(rowSums(Sum_RMS[i, nm_3500TDD]^2))
  Sum_WiFi<- sqrt(rowSums(Sum_RMS[i, nm_WiFi]^2))
  
  
  Sum_Bands[i, 1]<- Sum_Broadcast_1
  Sum_Bands[i, 2]<- Sum_3500TDD
  Sum_Bands[i, 3]<- Sum_WiFi
  
}



switzerland_RMS$Broadcasting<- Sum_Bands[,1]
switzerland_RMS$TDD_3500<- Sum_Bands[,2]
switzerland_RMS$WiFi_Total<- Sum_Bands[,3]


#Basel
Basel<- switzerland_RMS%>% filter(Location_of_Measurement=="secondary_city")

Basel2<- Basel [, c(2:9, 59, 18:30, 32:34, 60, 61)]

names(Basel2)[names(Basel2) == "Mobile.700.UL"] <- "Mobile 700 UL" #change name of variable
names(Basel2)[names(Basel2) == "Mobile.700.TDD"] <- "Mobile 700 TDD" #change name of variable
names(Basel2)[names(Basel2) == "Mobile.700.DL"] <- "Mobile 700 DL" #change name of variable
names(Basel2)[names(Basel2) == "Mobile.800.DL"] <- "Mobile 800 DL" #change name of variable
names(Basel2)[names(Basel2) == "Mobile.800.UL" ] <- "Mobile 800 UL" #change name of variable
names(Basel2)[names(Basel2) == "Mobile.900.UL"] <- "Mobile 900 UL" #change name of variable
names(Basel2)[names(Basel2) == "Mobile.900.DL" ] <- "Mobile 900 DL" #change name of variable
names(Basel2)[names(Basel2) == "Mobile.1400.SDL" ] <- "Mobile 1400 SDL" #change name of variable
names(Basel2)[names(Basel2) == "Mobile.1800.UL" ] <- "Mobile 1800 UL" #change name of variable
names(Basel2)[names(Basel2) == "Mobile.1800.DL" ] <- "Mobile 1800 DL" #change name of variable
names(Basel2)[names(Basel2) == "Mobile.2100.UL"  ] <- "Mobile 2100 UL" #change name of variable
names(Basel2)[names(Basel2) == "Mobile.2100.DL"  ] <- "Mobile 2100 DL" #change name of variable
names(Basel2)[names(Basel2) == "Mobile.2600.UL"  ] <- "Mobile 2600 UL" #change name of variable
names(Basel2)[names(Basel2) == "Mobile.2600.DL"  ] <- "Mobile 2600 DL" #change name of variable
names(Basel2)[names(Basel2) == "Mobile.2600.TDD"  ] <- "Mobile 2600 TDD" #change name of variable
names(Basel2)[names(Basel2) == "TDD_3500"  ] <- "Mobile 3500 TDD" #change name of variable
names(Basel2)[names(Basel2) == "WiFi_Total"    ] <- "WiFi" #change name of variable




#Change variables names
#names(Basel2)[names(Basel2) == "Broadcasting_Total"] <- "Broadcast" #change name of variable
#names(Basel2)[names(Basel2) == "UL_Total"] <- "Uplink" #change name of variable
#names(Basel2)[names(Basel2) == "DL_Total"] <- "Downlink" #change name of variable
#names(Basel2)[names(Basel2) == "TDD_Total"] <- "TDD" #change name of variable
#names(Basel2)[names(Basel2) == "WiFi_Total" ] <- "WiFi" #change name of variable


Basel_trans <- pivot_longer(data = Basel2, 
                           cols = c(9:27), #columns with my pesticides
                           names_to = "Class", 
                           values_to = "Abundance") #use tidyr to use pivot_longer

View(Basel_trans)


Basel_trans2<- Basel_trans %>% group_by(Usage_scenario, ME, Class) %>% summarise(Median_RMS = median(Abundance,na.rm = T), Geomean_RMS= geometric.mean(Abundance,na.rm = T), Mean_RMS = mean(Abundance,na.rm = T))

Basel_trans2$Usage_scenario[Basel_trans2$Usage_scenario=="non_user"]<- "Non user"
Basel_trans2$Usage_scenario[Basel_trans2$Usage_scenario=="user_max_dl"]<- "Max DL"
Basel_trans2$Usage_scenario[Basel_trans2$Usage_scenario=="user_max_ul"]<- "Max UL"

#Building ggplot - heat map

heatmaps<-Basel_trans2 %>% mutate(ME = fct_relevel(ME, "Downtown area", "Business area",  "Industrial area",
                                               "Residential area (C)","Residential area (NC)","Residential area (OUT)",
                                               "Park (C)", "Park (NC)",  "Park (OUT)" ,"Primary school", "University",
                                               "Shopping Mall", "Train station" ,"Bus station", "Train" ,"Bus" ,"Tram")) %>%
  ggplot(mapping = aes(x = Class,y = ME,fill = Geomean_RMS)) +
  geom_tile(width=0.8) + 
  facet_wrap(~ factor(Usage_scenario, levels = c("Non user", "Max DL", "Max UL")))+scale_fill_stepsn(trans = 'log10',limits= c(0.001,10),na.value = "white", 
                                                colours =c("#3288bd",  "#66c2a5","#e6f598","#fee08b",  "#f46d43"),labels = trans_format("log10", math_format(10^.x)), breaks= c(0, 0.01, 0.1, 1, 10))+
  labs(x = NULL, y = NULL, fill = "Geometric mean (V/m)")+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size =14,vjust = 0.5, angle = 90),axis.text.y = element_text(size =14),
        strip.text.x = element_text(size = 14),strip.text.y = element_text(size = 14),
        legend.title = element_text(size = 14, hjust=0.5), legend.text = element_text(size =14), plot.title = element_text(hjust=0.5))



#C. Stacked plots Switzerland
switzerland_RMS<- RMS_countries%>% filter(Measured_country=="switzerland")

switzerland_RMS<- switzerland_RMS [, c(2:9, 28, 52:56)]


#Change variables names
names(switzerland_RMS)[names(switzerland_RMS) == "Broadcasting_Total"] <- "Broadcast" #change name of variable
names(switzerland_RMS)[names(switzerland_RMS) == "UL_Total"] <- "Uplink" #change name of variable
names(switzerland_RMS)[names(switzerland_RMS) == "DL_Total"] <- "Downlink" #change name of variable
names(switzerland_RMS)[names(switzerland_RMS) == "TDD_Total"] <- "TDD" #change name of variable
names(switzerland_RMS)[names(switzerland_RMS) == "WiFi_Total" ] <- "WiFi" #change name of variable


switzerland_trans <- pivot_longer(data = switzerland_RMS, 
                            cols = c(9:14), #columns with my pesticides
                            names_to = "Class", 
                            values_to = "Abundance") #use tidyr to use pivot_longer


colnames(switzerland_trans)

switzerland_trans$Location_of_Measurement[switzerland_trans$Location_of_Measurement=="village_1"]<- "Villages"
switzerland_trans$Location_of_Measurement[switzerland_trans$Location_of_Measurement=="village_2"]<- "Villages"
switzerland_trans$Location_of_Measurement[switzerland_trans$Location_of_Measurement=="village_3"]<- "Villages"


switzerland_trans2<- switzerland_trans %>% group_by(Location_of_Measurement, Usage_scenario, ME, Class) %>% summarise(Median_RMS = median(Abundance,na.rm = T), Geomean_RMS= geometric.mean(Abundance,na.rm = T), Mean_RMS = mean(Abundance,na.rm = T))
switzerland_trans2<- switzerland_trans %>% group_by(Location_of_Measurement, Usage_scenario, ME, Class) %>% summarise(Median_RMS = median(Abundance,na.rm = T), Geomean_RMS= geometric.mean(Abundance,na.rm = T), Mean_RMS = mean(Abundance,na.rm = T))



switzerland_trans2$Usage_scenario[switzerland_trans2$Usage_scenario=="non_user"]<- "Non user"
switzerland_trans2$Usage_scenario[switzerland_trans2$Usage_scenario=="user_max_dl"]<- "Max DL"
switzerland_trans2$Usage_scenario[switzerland_trans2$Usage_scenario=="user_max_ul"]<- "Max UL"

Basel_stack<- filter(switzerland_trans2, Location_of_Measurement=="secondary_city")
Zurich_stack<- filter(switzerland_trans2, Location_of_Measurement=="capital_big_city")
Villages_stack<- filter(switzerland_trans2, Location_of_Measurement=="Villages")
Basel_stack2<- Basel_trans2

level_order<- c("Downtown area","Business area", "Industrial area", "Residential area (C)","Residential area (NC)",
                "Residential area (OUT)", "Park (C)","Park (NC)","Park (OUT)",
                "Primary school","University", "Shopping Mall", "Train station", "Bus station","Train",
                "Bus", "Tram")
  
  Basel_bar<- ggplot(Basel_stack2, aes(fill=Class, y=Geomean_RMS, x= factor(ME, level = level_order))) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~ factor(Usage_scenario, levels = c("Non user", "Max DL", "Max UL")))+
  #scale_fill_viridis(discrete = T) + 
    scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", 
                                 "#33a02c", "#fb9a99", "#e31a1c",
                                 "#fdbf6f", "#ff7f00", "#cab2d6",
                                 "#6a3d9a", "#04f3f6", "#b15928",
                                 "#1d91c0", "#253494", "#8c96c6",
                                 "#bdbdbd", "#525252", "#af6161",
                                 "#ffff99")) + scale_y_continuous(limits = c(0, 4), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))+
  ggtitle("B. Basel") +labs(x = NULL, y = "Geometric mean (V/m)", fill = "")+
  theme_ipsum() + theme(axis.text.x = element_text(size =11,vjust = 0.5, angle = 90))

level_order2<- c("Downtown area","Business area", "Industrial area", "Residential area (C)","Residential area (NC)",
                  "Residential area (OUT)", "Park (C)","Park (NC)","Park (OUT)",
                  "Primary school","Highschool","University", "Shopping Mall", "Train station", "Bus station","Train",
                  "Bus", "Tram")
  
  
Zurich_bar<- ggplot(Zurich_stack, aes(fill=Class, y=Geomean_RMS, x= factor(ME, level = level_order2))) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~ factor(Usage_scenario, levels = c("Non user", "Max DL", "Max UL")))+
  scale_fill_viridis(discrete = T) + scale_y_continuous(limits = c(0, 4), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))+
  ggtitle("A. Zürich") +labs(x = NULL, y = "Geometric mean (V/m)", fill = "")+
  theme_ipsum() + theme(axis.text.x = element_text(size =11,vjust = 0.5, angle = 90))


level_order3<- c("Village centre" ,"Industrial area", "Residential area", "Park",
                 "Shopping Mall", "Train station", "Bus station","Train",
                 "Bus")


Villages_bar<- ggplot(Villages_stack, aes(fill=Class, y=Geomean_RMS, factor(ME, level = level_order3))) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~ factor(Usage_scenario, levels = c("Non user", "Max DL", "Max UL")))+
  scale_fill_viridis(discrete = T) + scale_y_continuous(limits = c(0, 4), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))+
  ggtitle("C. Villages (Willisau, Hergiswil, Dagmersellen)") +labs(x = NULL, y = "Geometric mean (V/m)", fill = "")+
  theme_ipsum() + theme(axis.text.x = element_text(size =11,vjust = 0.5, angle = 90))



fig_SC<- ggarrange(Zurich_bar, Basel_bar, Villages_bar, ncol=1, nrow = 3, common.legend = T)


fig_SC<- annotate_figure(fig_SC, left = text_grob("ExpoM-RF4 Measured Values (V/m)", rot=90, size = 15), right = text_grob("Daily max temperature \n [ºC]", rot=270, size = 15), bottom = text_grob("Date", size=13))





#D. Stacked plots Switzerland 2
switzerland_RMS2<- RMS_countries%>% filter(Measured_country=="switzerland")

switzerland_RMS2<- switzerland_RMS2 [, c(2:9, 18:30,32:37, 52,56)]


#Change variables names
names(switzerland_RMS2)[names(switzerland_RMS2) == "Mobile.3500..1."] <- "Mobile.3500.1" #change name of variable
names(switzerland_RMS2)[names(switzerland_RMS2) == "Mobile.3500..2."] <- "Mobile.3500.2" #change name of variable
names(switzerland_RMS2)[names(switzerland_RMS2) == "Mobile.3500..3."] <- "Mobile.3500.3" #change name of variable
names(switzerland_RMS2)[names(switzerland_RMS2) == "Broadcasting_Total"] <- "Broadcast" #change name of variable
names(switzerland_RMS2)[names(switzerland_RMS2) == "WiFi_Total" ] <- "WiFi" #change name of variable


switzerland_trans <- pivot_longer(data = switzerland_RMS2, 
                                  cols = c(9:29), #columns with my pesticides
                                  names_to = "Class", 
                                  values_to = "Abundance") #use tidyr to use pivot_longer


colnames(switzerland_trans)

switzerland_trans$Location_of_Measurement[switzerland_trans$Location_of_Measurement=="village_1"]<- "Villages"
switzerland_trans$Location_of_Measurement[switzerland_trans$Location_of_Measurement=="village_2"]<- "Villages"
switzerland_trans$Location_of_Measurement[switzerland_trans$Location_of_Measurement=="village_3"]<- "Villages"


switzerland_trans2<- switzerland_trans %>% group_by(Location_of_Measurement, Usage_scenario, ME, Class) %>% summarise(Median_RMS = median(Abundance,na.rm = T), Geomean_RMS= geometric.mean(Abundance,na.rm = T), Mean_RMS = mean(Abundance,na.rm = T))
switzerland_trans2<- switzerland_trans %>% group_by(Location_of_Measurement, Usage_scenario, ME, Class) %>% summarise(Median_RMS = median(Abundance,na.rm = T), Geomean_RMS= geometric.mean(Abundance,na.rm = T), Mean_RMS = mean(Abundance,na.rm = T))



switzerland_trans2$Usage_scenario[switzerland_trans2$Usage_scenario=="non_user"]<- "Non user"
switzerland_trans2$Usage_scenario[switzerland_trans2$Usage_scenario=="user_max_dl"]<- "Max DL"
switzerland_trans2$Usage_scenario[switzerland_trans2$Usage_scenario=="user_max_ul"]<- "Max UL"

Basel_stack<- filter(switzerland_trans2, Location_of_Measurement=="secondary_city")
Zurich_stack<- filter(switzerland_trans2, Location_of_Measurement=="capital_big_city")
Villages_stack<- filter(switzerland_trans2, Location_of_Measurement=="Villages")

level_order<- c("Downtown area","Business area", "Industrial area", "Residential area (C)","Residential area (NC)",
                "Residential area (OUT)", "Park (C)","Park (NC)","Park (OUT)",
                "Primary school","University", "Shopping Mall", "Train station", "Bus station","Train",
                "Bus", "Tram")

Basel_bar2<- ggplot(Basel_stack, aes(fill=Class, y=Geomean_RMS, x= factor(ME, level = level_order))) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~ factor(Usage_scenario, levels = c("Non user", "Max DL", "Max UL")))+
  scale_fill_viridis(discrete = T) + scale_y_continuous(limits = c(0, 4), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))+
  ggtitle("B. Basel") +labs(x = NULL, y = "Geometric mean (V/m)", fill = "")+
  theme_ipsum() + theme(axis.text.x = element_text(size =11,vjust = 0.5, angle = 90))

level_order2<- c("Downtown area","Business area", "Industrial area", "Residential area (C)","Residential area (NC)",
                 "Residential area (OUT)", "Park (C)","Park (NC)","Park (OUT)",
                 "Primary school","Highschool","University", "Shopping Mall", "Train station", "Bus station","Train",
                 "Bus", "Tram")


Zurich_bar<- ggplot(Zurich_stack, aes(fill=Class, y=Geomean_RMS, x= factor(ME, level = level_order2))) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~ factor(Usage_scenario, levels = c("Non user", "Max DL", "Max UL")))+
  scale_fill_viridis(discrete = T) + scale_y_continuous(limits = c(0, 4), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))+
  ggtitle("A. Zürich") +labs(x = NULL, y = "Geometric mean (V/m)", fill = "")+
  theme_ipsum() + theme(axis.text.x = element_text(size =11,vjust = 0.5, angle = 90))


level_order3<- c("Village centre" ,"Industrial area", "Residential area", "Park",
                 "Shopping Mall", "Train station", "Bus station","Train",
                 "Bus")


Villages_bar<- ggplot(Villages_stack, aes(fill=Class, y=Geomean_RMS, factor(ME, level = level_order3))) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~ factor(Usage_scenario, levels = c("Non user", "Max DL", "Max UL")))+
  scale_fill_viridis(discrete = T) + scale_y_continuous(limits = c(0, 4), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))+
  ggtitle("C. Villages (Willisau, Hergiswil, Dagmersellen)") +labs(x = NULL, y = "Geometric mean (V/m)", fill = "")+
  theme_ipsum() + theme(axis.text.x = element_text(size =11,vjust = 0.5, angle = 90))



fig_SC<- ggarrange(Zurich_bar, Basel_bar, Villages_bar, ncol=1, nrow = 3, common.legend = T)


fig_SC<- annotate_figure(fig_SC, left = text_grob("ExpoM-RF4 Measured Values (V/m)", rot=90, size = 15), right = text_grob("Daily max temperature \n [ºC]", rot=270, size = 15), bottom = text_grob("Date", size=13))











#E. Preliminary Results
CH_RMS<- filter(TotalRMS, Measured_country =="switzerland")
CH_RMS$Location_of_Measurement[CH_RMS$Location_of_Measurement=="village_1"|CH_RMS$Location_of_Measurement=="village_2"|
                                 CH_RMS$Location_of_Measurement=="village_3"]<- "Villages"

CH_RMS_trans<- CH_RMS %>% group_by(Location_of_Measurement, Usage_scenario, ME) %>% summarise(Median_RMS = median(Total..RMS.,na.rm = T), Geomean_RMS= geometric.mean(Total..RMS.,na.rm = T), Mean_RMS = mean(Total..RMS.,na.rm = T))


CH_RMS_Basel_NU<- filter(CH_RMS_trans, Location_of_Measurement=="secondary_city" &
                           Usage_scenario=="non_user")
CH_RMS_Basel_DL<- filter(CH_RMS_trans, Location_of_Measurement=="secondary_city" &
                           Usage_scenario=="user_max_dl")
CH_RMS_Basel_UL<- filter(CH_RMS_trans, Location_of_Measurement=="secondary_city" &
                           Usage_scenario=="user_max_ul")

CH_RMS_Zurich_DL<- filter(CH_RMS_trans, Location_of_Measurement=="capital_big_city" &
                           Usage_scenario=="user_max_dl")
CH_RMS_Villages<- filter(CH_RMS_trans, Location_of_Measurement=="secondary_city")