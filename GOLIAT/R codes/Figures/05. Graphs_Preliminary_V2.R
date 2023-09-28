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

#CH
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/ExpoM_Diary_Merged/CH")
RMS_CH<- read.csv("CH_Data_RMS.csv", header = T, sep = ",")
PEAK_CH<- read.csv("CH_Data_PEAK.csv", header = T, sep = ",")
TOTAL_RMS_CH<- read.csv("CH_Data_RMSTotal.csv", header = T, sep = ",")

RMS_CH$Date_Time


#Clean names

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


#Some Values have a comma instead of a point
#this needs to be changed otherwise R does not read it correctly
TotalRMS$Total..RMS.<- as.numeric(sub(',','.',TotalRMS$Total..RMS.))                                                                                                                                                                                                                                                                                                                                  


###
#Morning presentation####
#Only show Total RMS values for the micro-environments in 3 secondary cities
###


#Get the school values
#school<- RMS_CH%>%filter(ME=="Primary school" | RMS_CH$ME =="Highschool")

#school$Date_Time
#setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis")
#save(school, file = "school.RData")
#write.csv(school, "schools_ch.csv")
#Filter per Location_Measurements

Sec_city<- TOTAL_RMS_CH%>% filter(Location_of_Measurement=="secondary_city")
Sec_city<- Sec_city %>% mutate(Location_of_Measurement = ifelse(Measured_country == "switzerland", "Basel (CH)", Location_of_Measurement))

Sec_city$Usage_scenario[Sec_city$Usage_scenario=="non_user"]<- "Non user"
Sec_city$Usage_scenario[Sec_city$Usage_scenario=="user_max_dl"]<- "User Max DL"
Sec_city$Usage_scenario[Sec_city$Usage_scenario=="user_max_ul"]<- "User Max UL"


#Filter out UL measurements
#sec_city_2<- Sec_city%>% filter(Usage_scenario!="User Max UL")
sec_city_2<- Sec_city%>% filter(Usage_scenario=="Non user")

sec_city_2$ME[sec_city_2$ME=="Bus station"]<- "Bus Station"
sec_city_2$ME[sec_city_2$ME=="Business area"]<- "Geschäftsviertel"
sec_city_2$ME[sec_city_2$ME=="Downtown area"]<- "Zentrum"
sec_city_2$ME[sec_city_2$ME=="Industrial area"]<- "Industriegebiet"
sec_city_2$ME[sec_city_2$ME=="Park (C)"]<- "Parks"
sec_city_2$ME[sec_city_2$ME=="Park (NC)"]<- "Parks"
sec_city_2$ME[sec_city_2$ME=="Park (OUT)"]<- "Parks"
sec_city_2$ME[sec_city_2$ME=="Residential area (C)"]<- "Wohngebieten"
sec_city_2$ME[sec_city_2$ME=="Residential area (NC)"]<- "Wohngebieten"
sec_city_2$ME[sec_city_2$ME=="Residential area (OUT)"]<- "Wohngebieten"
sec_city_2$ME[sec_city_2$ME=="Shopping Mall"]<- "Einkaufszentrum"
sec_city_2$ME[sec_city_2$ME=="Train"]<- "Öffentliche Verkehrsmittel"
sec_city_2$ME[sec_city_2$ME=="Tram"]<- "Öffentliche Verkehrsmittel"
sec_city_2$ME[sec_city_2$ME=="Bus"]<- "Öffentliche Verkehrsmittel"
sec_city_2$ME[sec_city_2$ME=="Train station"]<- "Bahnhof"
sec_city_2$ME[sec_city_2$ME=="University"]<- "Universität"
sec_city_2$ME[sec_city_2$ME=="Primary school"]<- "Primarschule St. Johann"



sec_city_2 %>% group_by(Usage_scenario, ME) %>% summarise(Median_RMS = median(Total..RMS.,na.rm = T), Max_RMS =max(Total..RMS.,na.rm=T), Min_RMS=min(Total..RMS.,na.rm=T))


Boxplots_NU<- sec_city_2 %>% mutate(ME = fct_relevel(ME, "Zentrum", "Geschäftsviertel",  "Industriegebiet",
                                       "Wohngebieten",
                                       "Parks", "Primarschule St. Johann", "Universität",
                                       "Einkaufszentrum", "Bahnhof" ,"Bus Station", "Öffentliche Verkehrsmittel"))%>%
  ggplot(mapping = aes(x = ME, y = Total..RMS.)) +
  geom_boxplot(aes(color= ME),  width= 0.7, na.rm = T, size=0.8)+ 
  scale_color_manual(values=c("#66c2a5","#66c2a5","#66c2a5",
                              "#66c2a5","#66c2a5","#fc8d62","#66c2a5",
                              "#66c2a5","#66c2a5","#66c2a5",
                              "#66c2a5","#66c2a5","#66c2a5","#66c2a5",
                              "#66c2a5","#66c2a5","#66c2a5"))+scale_y_continuous(breaks = c(0,0.5, 1,1.5, 2),limits = c(0,2.01))+
  labs(x="", y="Elektrische Feldstaerke [V/m]", colour="")+ ggtitle("Basel-Stadt, Schweiz") + theme(panel.background = element_blank(),
                                                                                                    axis.line = element_line(colour = "black"), axis.text.x = element_text(size =15,vjust = 0.5, angle = 90, 
                                                                                                                                                                     face = c('plain','plain','plain','plain',
                                                                                                                                                                              'plain','bold','plain','plain','plain',
                                                                                                                                                                              'plain','plain')),
                                                                         axis.text.y = element_text(size =15), strip.text.x = element_text(size = 12),
                                                                         axis.title.y = element_text(size = 15), 
                                                                         strip.text.y = element_text(size = 12), legend.title = element_blank(), legend.position = c("none"),
                                                                         legend.text = element_blank(), plot.title = element_text(hjust=0.05, size = 17), panel.spacing.y = unit(1, "lines")) 






#A. SUM Bands_RMS_GROUPS#### 
Sum_RMS1 <- RMS_CH 
Sum_RMS1<- Sum_RMS1[,-1]
nm_BroadCast = colnames(Sum_RMS1[,c(9:16)])
nm_UL = colnames(Sum_RMS1[,c(17, 21, 22, 25, 28, 31)]) #UL bands
nm_DL=colnames(Sum_RMS1[,c(19:20, 23:24, 26, 29, 33)]) #DL bands
nm_TDD=colnames(Sum_RMS1[,c(18, 32, 34:36)]) #TDD bands
nm_WiFi=colnames(Sum_RMS1[,c(30,37:43)]) #WiFi bands

Sum_Bands1 <- array(NA,dim=c(nrow(Sum_RMS1),5))

for (i in 1:nrow(Sum_RMS1)) { 
  
  Sum_Broadcast_1 <-sqrt(rowSums(Sum_RMS1[i, nm_BroadCast]^2))
  Sum_UL<- sqrt(rowSums(Sum_RMS1[i, nm_UL]^2))
  Sum_DL<- sqrt(rowSums(Sum_RMS1[i, nm_DL]^2))
  Sum_TDD<- sqrt(rowSums(Sum_RMS1[i, nm_TDD]^2))
  Sum_WiFi<- sqrt(rowSums(Sum_RMS1[i, nm_WiFi]^2))
  
  
  Sum_Bands1[i, 1]<- Sum_Broadcast_1
  Sum_Bands1[i, 2]<- Sum_UL
  Sum_Bands1[i, 3]<- Sum_DL
  Sum_Bands1[i, 4]<- Sum_TDD
  Sum_Bands1[i, 5]<- Sum_WiFi
  
}



Sum_RMS1$Broadcasting_Total<- Sum_Bands1[,1]
Sum_RMS1$UL_Total<- Sum_Bands1[,2]
Sum_RMS1$DL_Total<- Sum_Bands1[,3]
Sum_RMS1$TDD_Total<- Sum_Bands1[,4]
Sum_RMS1$WiFi_Total<- Sum_Bands1[,5]





#B. SUM Bands_RMS_BANDS#### 
Sum_RMS <- RMS_CH 
nm_BroadCast = colnames(RMS_CH[,c(10:17)])
nm_3500TDD=colnames(RMS_CH[,c(35:37)]) #3500TDD bands
nm_WiFi=colnames(RMS_CH[,c(38:44, 31)]) #WiFi bands

Sum_Bands <- array(NA,dim=c(nrow(Sum_RMS),3))

for (i in 1:nrow(Sum_RMS)) { 
  
  Sum_Broadcast_1 <-sqrt(rowSums(Sum_RMS[i, nm_BroadCast]^2))
  Sum_3500TDD<- sqrt(rowSums(Sum_RMS[i, nm_3500TDD]^2))
  Sum_WiFi<- sqrt(rowSums(Sum_RMS[i, nm_WiFi]^2))
  
  
  Sum_Bands[i, 1]<- Sum_Broadcast_1
  Sum_Bands[i, 2]<- Sum_3500TDD
  Sum_Bands[i, 3]<- Sum_WiFi
  
}



Sum_RMS1$TDD_3500<- Sum_Bands[,2]



#Add Total RMS for contribution
TOTAL_RMS_CH<- TOTAL_RMS_CH[,-1]
colnames(TOTAL_RMS_CH)
RMS_TOTAL_CH<- left_join(Sum_RMS1, TOTAL_RMS_CH, by=c("Research_centre","Date","Measured_country",
                                                      "Location_of_Measurement","Usage_scenario", "micro_environment_type",
                                                      "ME","Date_Time","PubTrans_Capacity","Type_station","Train_type",
                                                      "Train_class","Train_floor","phones_school","Comments","GPS.Fix.Mode",
                                                      "GPS.Lat", "GPS.Lon","GPS.Altitude","GPS.HDOP","GPS..Satellites","GPS.Speed"))


#Basel
Basel<- RMS_TOTAL_CH%>% filter(Location_of_Measurement=="secondary_city")

#Filter out UL measurements
Basel_2<- Basel%>% filter(Usage_scenario!="user_max_ul")

Basel2<- Basel_2 [, c(1:8, 17:29, 31:33, 44:50, 58:64)]
colnames(Basel2)


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
names(Basel2)[names(Basel2) == "Broadcasting_Total"    ] <- "Broadcast" #change name of variable
names(Basel2)[names(Basel2) == "UL_Total"] <- "Mobile UL" #change name of variable
names(Basel2)[names(Basel2) == "DL_Total"] <- "Mobile DL" #change name of variable
names(Basel2)[names(Basel2) == "TDD_Total"] <- "Mobile TDD" #change name of variable
names(Basel2)[names(Basel2) == "Total..RMS."] <- "Total RMS" #change name of variable



#GROUPS BANDS ####
Basel_groups<- Basel2[, c(1:8, 19, 32:36, 38)]

#UNIQUE BANDS
Basel_unique<- Basel2[, c(1:24, 32, 36:38)]


Basel_trans_group<- pivot_longer(data = Basel_groups, 
                            cols = c(9:15), 
                            names_to = "Class", 
                            values_to = "Abundance") #use tidyr to use pivot_longer


Basel_trans_unique<- pivot_longer(data = Basel_unique, 
                                 cols = c(9:28), 
                                 names_to = "Class", 
                                 values_to = "Abundance") #use tidyr to use pivot_longer



#Calculate RMs (each band or group of bands) for a given ME
Basel_trans_group2<- Basel_trans_group %>% group_by(Usage_scenario, ME, Class) %>% summarise(Median_RMS = median(Abundance,na.rm = T), Geomean_RMS= geometric.mean(Abundance,na.rm = T), Mean_RMS = sqrt(sum(Abundance^2)/length(Abundance)), Max_RMS = max(Abundance, na.rm = T))

Basel_trans_unique2<- Basel_trans_unique %>% group_by(Usage_scenario, ME, Class) %>% summarise(Median_RMS = median(Abundance,na.rm = T), Geomean_RMS= geometric.mean(Abundance,na.rm = T), Mean_RMS = sqrt(sum(Abundance^2)/length(Abundance)), Max_RMS = max(Abundance, na.rm = T))

                                                                                 
#Clean a bit
Basel_trans_group2$Usage_scenario[Basel_trans_group2$Usage_scenario=="non_user"]<- "Non user"
Basel_trans_group2$Usage_scenario[Basel_trans_group2$Usage_scenario=="user_max_dl"]<- "Max DL"

Basel_trans_unique2$Usage_scenario[Basel_trans_unique2$Usage_scenario=="non_user"]<- "Non user"
Basel_trans_unique2$Usage_scenario[Basel_trans_unique2$Usage_scenario=="user_max_dl"]<- "Max DL"


#Back Transform to calculate percentage of contribution each band
#Group of bands
Basel_wider_group<- Basel_trans_group2[, c(1:3, 6)] 
Basel_wider_group<- Basel_wider_group%>%pivot_wider(names_from = Class, values_from = Mean_RMS)
Basel_wider_group$Broadcast_Contribution<- (Basel_wider_group$Broadcast^2)/(Basel_wider_group$`Total RMS`^2)
Basel_wider_group$DECT_Contribution<- (Basel_wider_group$DECT^2)/(Basel_wider_group$`Total RMS`^2)
Basel_wider_group$Mobile_DL_Contribution<- (Basel_wider_group$`Mobile DL`^2)/(Basel_wider_group$`Total RMS`^2)
Basel_wider_group$Mobile_UL_Contribution<- (Basel_wider_group$`Mobile UL`^2)/(Basel_wider_group$`Total RMS`^2)
Basel_wider_group$Mobile_TDD_Contribution<- (Basel_wider_group$`Mobile TDD`^2)/(Basel_wider_group$`Total RMS`^2)
Basel_wider_group$WiFi_Contribution<- (Basel_wider_group$WiFi^2)/(Basel_wider_group$`Total RMS`^2)

#Unique bands
Basel_wider_unique<- Basel_trans_unique2[, c(1:3, 6)] 
Basel_wider_unique<- Basel_wider_unique%>%pivot_wider(names_from = Class, values_from = Mean_RMS)

Basel_wider_unique$Broadcast_Contribution<- (Basel_wider_unique$Broadcast^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$DECT_Contribution<- (Basel_wider_unique$DECT^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$WiFi_Contribution<- (Basel_wider_unique$WiFi^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 700 DL Contribution`<- (Basel_wider_unique$`Mobile 700 DL`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 700 UL Contribution`<- (Basel_wider_unique$`Mobile 700 UL`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 700 TDD Contribution`<- (Basel_wider_unique$`Mobile 700 TDD`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 800 DL Contribution`<- (Basel_wider_unique$`Mobile 800 DL`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 800 UL Contribution`<- (Basel_wider_unique$`Mobile 800 UL`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 900 DL Contribution`<- (Basel_wider_unique$`Mobile 900 DL`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 900 UL Contribution`<- (Basel_wider_unique$`Mobile 900 UL`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 1400 SDL Contribution`<- (Basel_wider_unique$`Mobile 1400 SDL`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 1800 DL Contribution`<- (Basel_wider_unique$`Mobile 1800 DL`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 1800 UL Contribution`<- (Basel_wider_unique$`Mobile 1800 UL`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 2100 DL Contribution`<- (Basel_wider_unique$`Mobile 2100 DL`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 2100 UL Contribution`<- (Basel_wider_unique$`Mobile 2100 UL`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 2600 DL Contribution`<- (Basel_wider_unique$`Mobile 2600 DL`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 2600 UL Contribution`<- (Basel_wider_unique$`Mobile 2600 UL`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 2600 TDD Contribution`<- (Basel_wider_unique$`Mobile 2600 TDD`^2)/(Basel_wider_unique$`Total RMS`^2)
Basel_wider_unique$`Mobile 3500 TDD Contribution`<- (Basel_wider_unique$`Mobile 3500 TDD`^2)/(Basel_wider_unique$`Total RMS`^2)

Basel_wider_unique$TOTAL<- rowSums(Basel_wider_unique[,c(23:41)])

head(Basel_wider_group)  
  
#Building ggplot - heat map
Basel_group_GRAPH<- Basel_trans_group2%>%filter(Class!="Total RMS")

level_order<- c("Downtown area","Business area", "Industrial area", "Residential area (C)","Residential area (NC)",
                "Residential area (OUT)", "Park (C)","Park (NC)","Park (OUT)",
                "Primary school","University", "Shopping Mall", "Train station", "Bus station","Train",
                "Bus", "Tram")

Basel_bar_groups<- ggplot(Basel_group_GRAPH, aes(fill=Class, y=Mean_RMS, x= factor(ME, level = level_order))) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~ factor(Usage_scenario, levels = c("Non user", "Max DL", "Max UL")))+
  scale_fill_viridis(discrete = T) + 
  #scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", 
  #                             "#33a02c", "#fb9a99", "#e31a1c",
  #                             "#fdbf6f", "#ff7f00", "#cab2d6",
  #                             "#6a3d9a", "#04f3f6", "#b15928",
  #                             "#1d91c0", "#253494", "#8c96c6",
  #                             "#bdbdbd", "#525252", "#af6161",
  #                             "#ffff99")) + 
  scale_y_continuous(limits = c(0, 3), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3))+
  ggtitle("Basel, CH") +labs(x = NULL, y = "Average RMS value (V/m)", fill = "")+
  theme_ipsum() + theme(axis.text.x = element_text(size =11,vjust = 0.5, angle = 90))




Basel_unique_GRAPH<- Basel_trans_unique2%>%filter(Class!="Total RMS")

level_order<- c("Downtown area","Business area", "Industrial area", "Residential area (C)","Residential area (NC)",
                "Residential area (OUT)", "Park (C)","Park (NC)","Park (OUT)",
                "Primary school","University", "Shopping Mall", "Train station", "Bus station","Train",
                "Bus", "Tram")


Basel_bar_unique<- ggplot(Basel_unique_GRAPH, aes(fill=Class, y=Mean_RMS, x= factor(ME, level = level_order))) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~ factor(Usage_scenario, levels = c("Non user", "Max DL", "Max UL")))+
  #scale_fill_viridis(discrete = T) + 
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", 
                               "#33a02c", "#fb9a99", "#e31a1c",
                               "#fdbf6f", "#ff7f00", "#cab2d6",
                               "#6a3d9a", "#04f3f6", "#b15928",
                               "#1d91c0", "#253494", "#8c96c6",
                             "#bdbdbd", "#525252", "#af6161",
                               "#ffff99")) + 
  scale_y_continuous(limits = c(0, 4), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))+
  ggtitle("Basel, CH") +labs(x = NULL, y = "Average RMS value (V/m)", fill = "")+
  theme_ipsum() + theme(axis.text.x = element_text(size =11,vjust = 0.5, angle = 90))



#Heatmaps Unique Bands####

Basel_heats_unique<- pivot_longer(data = Basel_wider_unique, 
                                 cols = c(23:41), 
                                 names_to = "Class", 
                                 values_to = "Abundance")

#Max DL Unique
Basel_heats_unique_DL<- Basel_heats_unique%>%filter(Usage_scenario=="Max DL")
unique(Basel_heats_unique_DL$Class)


level_order_Class<- c("Broadcast_Contribution", "DECT_Contribution","WiFi_Contribution",
                      "Mobile 700 UL Contribution",
                      "Mobile 800 UL Contribution", "Mobile 900 UL Contribution", "Mobile 1800 UL Contribution",
                      "Mobile 2100 UL Contribution","Mobile 2600 UL Contribution",
                      "Mobile 700 DL Contribution", "Mobile 800 DL Contribution",
                      "Mobile 900 DL Contribution", "Mobile 1400 SDL Contribution",
                      "Mobile 1800 DL Contribution", "Mobile 2100 DL Contribution",
                      "Mobile 2600 DL Contribution","Mobile 700 TDD Contribution",
                      "Mobile 2600 TDD Contribution", "Mobile 3500 TDD Contribution")

Heatmaps<- ggplot(Basel_heats_unique_DL, aes(y = Abundance, fill=factor(Class, level = level_order_Class), x=factor(ME, level = level_order))) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~ factor(Usage_scenario, levels = c("Non user", "Max DL")))+
  #scale_fill_viridis(discrete = T) + 
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", 
                               "#33a02c", "#fb9a99", "#e31a1c",
                               "#fdbf6f", "#ff7f00", "#cab2d6",
                               "#6a3d9a", "#ffff99", "#b15928",
                               "#1d91c0", "#253494", "#8c96c6",
                               "#bdbdbd", "#525252", "#af6161",
                               "#04f3f6")) + 
  scale_y_continuous(limits = c(0,1.045), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))+
  ggtitle("Basel, CH") +labs(x = NULL, y = "Fraction of Total RMS values \n given by each band", fill = "")+
  theme_ipsum() + theme(axis.text.x = element_text(size =11,vjust = 0.5, angle = 90))


#Non-user Unique
Basel_heats_unique_NU<- Basel_heats_unique%>%filter(Usage_scenario=="Non user")
unique(Basel_heats_unique_NU$Class)


level_order_Class<- c("Broadcast_Contribution", "DECT_Contribution","WiFi_Contribution",
                      "Mobile 700 UL Contribution",
                      "Mobile 800 UL Contribution", "Mobile 900 UL Contribution", "Mobile 1800 UL Contribution",
                      "Mobile 2100 UL Contribution","Mobile 2600 UL Contribution",
                      "Mobile 700 DL Contribution", "Mobile 800 DL Contribution",
                      "Mobile 900 DL Contribution", "Mobile 1400 SDL Contribution",
                      "Mobile 1800 DL Contribution", "Mobile 2100 DL Contribution",
                      "Mobile 2600 DL Contribution","Mobile 700 TDD Contribution",
                      "Mobile 2600 TDD Contribution", "Mobile 3500 TDD Contribution")

Heatmaps_NU<- ggplot(Basel_heats_unique_NU, aes(y = Abundance, fill=factor(Class, level = level_order_Class), x=factor(ME, level = level_order))) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~ factor(Usage_scenario, levels = c("Non user", "Max DL")))+
  #scale_fill_viridis(discrete = T) + 
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", 
                               "#33a02c", "#fb9a99", "#e31a1c",
                               "#fdbf6f", "#ff7f00", "#cab2d6",
                               "#6a3d9a", "#ffff99", "#b15928",
                               "#1d91c0", "#253494", "#8c96c6",
                               "#bdbdbd", "#525252", "#af6161",
                               "#04f3f6")) + 
  scale_y_continuous(limits = c(0,1.045), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))+
  ggtitle("Basel, CH") +labs(x = NULL, y = "Fraction of Total RMS values \n given by each band", fill = "")+
  theme_ipsum() + theme(axis.text.x = element_text(size =11,vjust = 0.5, angle = 90))






#Heatmaps Grouped Bands####

Basel_heats_grouped<- pivot_longer(data = Basel_wider_group, 
                                  cols = c(10:15), 
                                  names_to = "Class", 
                                  values_to = "Abundance")

#Max DL Unique
Basel_heats_grouped_DL<- Basel_heats_grouped%>%filter(Usage_scenario=="Max DL")
unique(Basel_heats_grouped_DL$Class)


level_order_Class<- c("Broadcast_Contribution", "DECT_Contribution","WiFi_Contribution",
                      "Mobile_DL_Contribution","Mobile_UL_Contribution","Mobile_TDD_Contribution")

Heatmaps_DL_grouped<- ggplot(Basel_heats_grouped_DL, aes(x = Abundance, fill=factor(Class, level = level_order_Class), y=factor(ME, level = level_order))) + 
  geom_bar(position="stack", stat="identity") + 
  #scale_fill_viridis(discrete = T) + 
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", 
                               "#33a02c", "#fb9a99", 
                               "#fdbf6f", "#ff7f00", "#cab2d6",
                               "#bdbdbd", "#525252", "#af6161")) + 
  scale_x_continuous(limits = c(0,1.012), breaks = c(0, 0.25, 0.5, 0.75, 1))+
  ggtitle("Basel, Schweiz") +labs(x = NULL, y = "Fraction of Total RMS values \n given by each band", fill = "")+
  theme_ipsum() + theme(axis.text.x = element_text(size =11,vjust = 0.5, angle = 90), 
                        axis.text.y = element_text(face = c('plain','plain','plain','plain',
                                                            'plain','plain','plain','plain',
                                                            'plain','bold','plain','plain',
                                                            'plain','plain')))


#Non-user Unique
Basel_heats_grouped_NU<- Basel_heats_grouped%>%filter(Usage_scenario=="Non user")
unique(Basel_heats_grouped_NU$Class)
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="Bus station"]<- "Bus Station"
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="Business area"]<- "Geschäftsviertel"
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="Downtown area"]<- "Zentrum"
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="Industrial area"]<- "Industriegebiet"
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="Park (C)"]<- "Park (Zentral)"
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="Park (NC)"]<- "Park (Nicht Zentral)"
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="Park (OUT)"]<- "Park (Stadtrand)"
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="Residential area (C)"]<- "Wohngebiet (Zentral)"
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="Residential area (NC)"]<- "Wohngebiet (Nicht Zentral)"
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="Residential area (OUT)"]<- "Wohngebiet (Stadtrand)"
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="Shopping Mall"]<- "Einkaufszentrum"
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="Train"]<- "Zug"
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="Train station"]<- "Bahnhof"
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="University"]<- "Universität"
Basel_heats_grouped_NU$ME[Basel_heats_grouped_NU$ME=="Primary school"]<- "Primarschule St. Johann"


Basel_heats_grouped_NU$Class[Basel_heats_grouped_NU$Class=="Broadcast_Contribution"]<- "Rundfunk"
Basel_heats_grouped_NU$Class[Basel_heats_grouped_NU$Class=="DECT_Contribution"]<- "DECT"
Basel_heats_grouped_NU$Class[Basel_heats_grouped_NU$Class=="Mobile_DL_Contribution"]<- "Downlink"
Basel_heats_grouped_NU$Class[Basel_heats_grouped_NU$Class=="Mobile_UL_Contribution"]<- "Uplink"
Basel_heats_grouped_NU$Class[Basel_heats_grouped_NU$Class=="Mobile_TDD_Contribution"]<- "TDD"
Basel_heats_grouped_NU$Class[Basel_heats_grouped_NU$Class=="WiFi_Contribution"]<- "WLAN"


level_order<- c("Tram","Bus", "Zug","Bus Station", "Bahnhof","Einkaufszentrum", "Universität","Primarschule St. Johann",  "Park (Stadtrand)" , "Park (Nicht Zentral)","Park (Zentral)",
                "Wohngebiet (Stadtrand)","Wohngebiet (Nicht Zentral)","Wohngebiet (Zentral)","Industriegebiet", "Geschäftsviertel","Zentrum")



level_order_Class<- c("Rundfunk", "DECT","WLAN",
                      "Downlink","Uplink","TDD")

Heatmaps_NU_grouped<- ggplot(Basel_heats_grouped_NU, aes(x = Abundance*100, fill=factor(Class, level = level_order_Class), y=factor(ME, level = level_order))) + 
  geom_bar(position="stack", stat="identity") + 
  #scale_fill_viridis(discrete = T) + 
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", 
                               "#33a02c", "#fb9a99", 
                               "#fdbf6f", "#ff7f00", "#cab2d6",
                               "#bdbdbd", "#525252", "#af6161")) + 
  scale_x_continuous(limits = c(0,101), breaks = c(0, 25, 50, 75, 100))+
  ggtitle("") +labs(y = NULL, x = "Durchschnittsanteil [%]", fill = "HF Quelle")+
  theme_ipsum() + theme(axis.text.x = element_text(size =15,vjust = 0.5), legend.text = element_text(size=14),
                        axis.text.y = element_text(face = c('plain','plain','plain','plain',
                                                            'plain','plain','plain','bold','plain',
                                                            'plain','plain','plain',
                                                            'plain','plain'), size = 15),
                        axis.title.x = element_text(size = 15, hjust = 0.5), plot.title = element_text(size=17))

Basel_heats_grouped_NU$ME

St_Johann<- ggarrange(Boxplots_NU, Heatmaps_NU_grouped,nrow=2, align = "v",heights = c(1,1),
                      common.legend = F)





##Zurich####

Zurich<- RMS_TOTAL_CH%>% filter(Location_of_Measurement=="capital_big_city")

#Filter per Location_Measurements

Zurich$Usage_scenario[Zurich$Usage_scenario=="non_user"]<- "Non user"
Zurich$Usage_scenario[Zurich$Usage_scenario=="user_max_dl"]<- "User Max DL"
Zurich$Usage_scenario[Zurich$Usage_scenario=="user_max_ul"]<- "User Max UL"


#Filter out UL measurements
#sec_city_2<- Sec_city%>% filter(Usage_scenario!="User Max UL")
Zurich2<- Zurich%>% filter(Usage_scenario=="Non user")

Zurich2$ME[Zurich2$ME=="Bus station"]<- "Bus Station"
Zurich2$ME[Zurich2$ME=="Business area"]<- "Geschäftsviertel"
Zurich2$ME[Zurich2$ME=="Downtown area"]<- "Zentrum"
Zurich2$ME[Zurich2$ME=="Industrial area"]<- "Industriegebiet"
Zurich2$ME[Zurich2$ME=="Park (C)"]<- "Park (Zentral)"
Zurich2$ME[Zurich2$ME=="Park (NC)"]<- "Park (Nicht Zentral)"
Zurich2$ME[Zurich2$ME=="Park (OUT)"]<- "Park (Stadtrand)"
Zurich2$ME[Zurich2$ME=="Residential area (C)"]<- "Wohngebiet (Zentral)"
Zurich2$ME[Zurich2$ME=="Residential area (NC)"]<- "Wohngebiet (Nicht Zentral)"
Zurich2$ME[Zurich2$ME=="Residential area (OUT)"]<- "Wohngebiet (Stadtrand)"
Zurich2$ME[Zurich2$ME=="Shopping Mall"]<- "Einkaufszentrum"
Zurich2$ME[Zurich2$ME=="Train"]<- "Zug"
Zurich2$ME[Zurich2$ME=="Tram"]<- "Tram"
Zurich2$ME[Zurich2$ME=="Bus"]<- "Bus"
Zurich2$ME[Zurich2$ME=="Train station"]<- "Bahnhof"
Zurich2$ME[Zurich2$ME=="University"]<- "Universität"
Zurich2$ME[Zurich2$ME=="Primary school"]<- "Schulhaus Aegerten"
Zurich2$ME[Zurich2$ME=="Highschool"]<- "Schule Albisriederplatz"



Zurich3<- Zurich2%>%filter(ME!="Schule Albisriederplatz")
Zurich4<- Zurich2%>%filter(ME!="Schulhaus Aegerten")


Boxplots_NU_Zurich3_2<- Zurich3 %>% mutate(ME = fct_relevel(ME, "Zentrum","Geschäftsviertel","Industriegebiet","Wohngebiet (Zentral)","Wohngebiet (Nicht Zentral)",
                                                  "Wohngebiet (Stadtrand)","Park (Zentral)", "Park (Nicht Zentral)",  "Park (Stadtrand)","Schulhaus Aegerten",
                                                  "Universität","Einkaufszentrum", "Bahnhof","Bus Station", "Zug","Bus","Tram"))%>%
  ggplot(mapping = aes(x = ME, y = Total..RMS.)) +
  geom_boxplot(aes(color= ME),  width= 0.7, na.rm = T, size=0.8)+ 
  scale_color_manual(values=c("#66c2a5","#66c2a5","#66c2a5",
                              "#66c2a5","#66c2a5","#66c2a5",
                              "#66c2a5","#66c2a5","#66c2a5","#fc8d62",
                              "#66c2a5","#66c2a5","#66c2a5","#66c2a5",
                              "#66c2a5","#66c2a5","#66c2a5"))+scale_y_continuous(breaks = c(0,0.5, 1,1.5, 2),limits = c(0,2.01))+
  labs(x="", y="Elektrische Feldstaerke [V/m]", colour="")+ ggtitle("Zürich, Schweiz") + theme(panel.background = element_blank(),
                                                                                                    axis.line = element_line(colour = "black"), axis.text.x = element_text(size =15,vjust = 0.5, angle = 90, 
                                                                                                                                                                           face = c('plain','plain','plain','plain',
                                                                                                                                                                                    'plain','plain','plain','plain',
                                                                                                                                                                                    'plain','bold','plain','plain','plain','plain','plain')),
                                                                                                    axis.text.y = element_text(size =15), strip.text.x = element_text(size = 12),
                                                                                                    axis.title.y = element_text(size = 15), 
                                                                                                    strip.text.y = element_text(size = 12), legend.title = element_blank(), legend.position = c("none"),
                                                                                                    legend.text = element_blank(), plot.title = element_text(hjust=0.05, size = 17), panel.spacing.y = unit(1, "lines")) 






Boxplots_NU_Zurich4_2<- Zurich4 %>% mutate(ME = fct_relevel(ME, "Zentrum","Geschäftsviertel","Industriegebiet","Wohngebiet (Zentral)","Wohngebiet (Nicht Zentral)",
                                                            "Wohngebiet (Stadtrand)","Park (Zentral)", "Park (Nicht Zentral)",  "Park (Stadtrand)","Schule Albisriederplatz",
                                                            "Universität","Einkaufszentrum", "Bahnhof","Bus Station", "Zug","Bus","Tram"))%>%
  ggplot(mapping = aes(x = ME, y = Total..RMS.)) +
  geom_boxplot(aes(color= ME),  width= 0.7, na.rm = T, size=0.8)+ 
  scale_color_manual(values=c("#66c2a5","#66c2a5","#66c2a5",
                              "#66c2a5","#66c2a5","#66c2a5",
                              "#66c2a5","#66c2a5","#66c2a5","#fc8d62",
                              "#66c2a5","#66c2a5","#66c2a5","#66c2a5",
                              "#66c2a5","#66c2a5","#66c2a5"))+scale_y_continuous(breaks = c(0,0.5, 1,1.5, 2),limits = c(0,2.01))+
  labs(x="", y="Elektrische Feldstaerke [V/m]", colour="")+ ggtitle("Zürich, Schweiz") + theme(panel.background = element_blank(),
                                                                                               axis.line = element_line(colour = "black"), axis.text.x = element_text(size =15,vjust = 0.5, angle = 90, 
                                                                                                                                                                      face = c('plain','plain','plain','plain',
                                                                                                                                                                               'plain','plain','plain','plain',
                                                                                                                                                                               'plain','bold','plain','plain','plain','plain','plain')),
                                                                                               axis.text.y = element_text(size =15), strip.text.x = element_text(size = 12),
                                                                                               axis.title.y = element_text(size = 15), 
                                                                                               strip.text.y = element_text(size = 12), legend.title = element_blank(), legend.position = c("none"),
                                                                                            legend.text = element_blank(), plot.title = element_text(hjust=0.05, size = 17), panel.spacing.y = unit(1, "lines")) 

#Filter out UL measurements
#Add Total RMS for contribution
Zurich_groups<- RMS_TOTAL_CH%>% filter(Location_of_Measurement=="capital_big_city")
Zurich_groups_NU<- Zurich_groups%>% filter(Usage_scenario=="non_user")

Zurich_groups_NU2<- Zurich_groups_NU [, c(2:9, 17:29, 31:33, 44:50, 58:64)]
colnames(Basel2)


names(Zurich_groups_NU2)[names(Zurich_groups_NU2) == "WiFi_Total"    ] <- "WLAN" #change name of variable
names(Zurich_groups_NU2)[names(Zurich_groups_NU2) == "Broadcasting_Total"    ] <- "Rundfunk" #change name of variable
names(Zurich_groups_NU2)[names(Zurich_groups_NU2) == "UL_Total"] <- "Uplink" #change name of variable
names(Zurich_groups_NU2)[names(Zurich_groups_NU2) == "DL_Total"] <- "Downlink" #change name of variable
names(Zurich_groups_NU2)[names(Zurich_groups_NU2) == "TDD_Total"] <- "TDD" #change name of variable
names(Zurich_groups_NU2)[names(Zurich_groups_NU2) == "Total..RMS."] <- "Total RMS" #change name of variable



#GROUPS BANDS ####
Zurich_groups_NU3<- Zurich_groups_NU2[, c(1:7, 19, 32:36, 38)]


Zurich_trans_group<- pivot_longer(data = Zurich_groups_NU3, 
                                 cols = c(8:14), 
                                 names_to = "Class", 
                                 values_to = "Abundance") #use tidyr to use pivot_longer


#Calculate RMs (each band or group of bands) for a given ME
Zurich_trans_group2<- Zurich_trans_group %>% group_by(Usage_scenario, ME, Class) %>% summarise(Median_RMS = median(Abundance,na.rm = T), Geomean_RMS= geometric.mean(Abundance,na.rm = T), Mean_RMS = sqrt(sum(Abundance^2)/length(Abundance)), Max_RMS = max(Abundance, na.rm = T))



#Clean a bit
Zurich_trans_group2$Usage_scenario[Zurich_trans_group2$Usage_scenario=="non_user"]<- "Non user"



#Back Transform to calculate percentage of contribution each band
#Group of bands
Zurich_wider_group<- Zurich_trans_group2[, c(1:3, 6)] 
Zurich_wider_group<- Zurich_wider_group%>%pivot_wider(names_from = Class, values_from = Mean_RMS)
Zurich_wider_group$Rundfunk<- (Zurich_wider_group$Rundfunk^2)/(Zurich_wider_group$`Total RMS`^2)
Zurich_wider_group$DECT<- (Zurich_wider_group$DECT^2)/(Zurich_wider_group$`Total RMS`^2)
Zurich_wider_group$Downlink<- (Zurich_wider_group$Downlink^2)/(Zurich_wider_group$`Total RMS`^2)
Zurich_wider_group$Uplink<- (Zurich_wider_group$Uplink^2)/(Zurich_wider_group$`Total RMS`^2)
Zurich_wider_group$TDD<- (Zurich_wider_group$TDD^2)/(Zurich_wider_group$`Total RMS`^2)
Zurich_wider_group$WLAN<- (Zurich_wider_group$WLAN^2)/(Zurich_wider_group$`Total RMS`^2)





#Heatmaps Grouped Bands####

Zurich_heats_grouped<- pivot_longer(data = Zurich_wider_group, 
                                   cols = c(3:9), 
                                   names_to = "Class", 
                                   values_to = "Abundance")


#Non-user Unique
Zurich_heats_grouped_NU<- Zurich_heats_grouped%>%filter(Usage_scenario=="Non user")
Zurich_heats_grouped_NU<- Zurich_heats_grouped_NU%>%filter(Class!="Total RMS")

unique(Zurich_heats_grouped_NU$ME)
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Bus station"]<- "Bus Station"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Business area"]<- "Geschäftsviertel"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Downtown area"]<- "Zentrum"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Industrial area"]<- "Industriegebiet"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Park (C)"]<- "Park (Zentral)"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Park (NC)"]<- "Park (Nicht Zentral)"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Park (OUT)"]<- "Park (Stadtrand)"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Residential area (C)"]<- "Wohngebiet (Zentral)"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Residential area (NC)"]<- "Wohngebiet (Nicht Zentral)"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Residential area (OUT)"]<- "Wohngebiet (Stadtrand)"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Shopping Mall"]<- "Einkaufszentrum"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Train"]<- "Zug"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Tram"]<- "Tram"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Bus"]<- "Bus"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Train station"]<- "Bahnhof"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="University"]<- "Universität"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Primary school"]<- "Schulhaus Aegerten"
Zurich_heats_grouped_NU$ME[Zurich_heats_grouped_NU$ME=="Highschool"]<- "Schule Albisriederplatz"

unique(Zurich_heats_grouped_NU$Class)


Zurich_heats_grouped_PS<- Zurich_heats_grouped_NU%>%filter(ME!="Schule Albisriederplatz")
Zurich_heats_grouped_HS<- Zurich_heats_grouped_NU%>%filter(ME!="Schulhaus Aegerten")
unique(Zurich_heats_grouped_HS$ME)


level_order_PS<- c("Tram","Bus", "Zug","Bus Station", "Bahnhof","Einkaufszentrum", "Universität","Schulhaus Aegerten",  "Park (Stadtrand)" , "Park (Nicht Zentral)","Park (Zentral)",
                "Wohngebiet (Stadtrand)","Wohngebiet (Nicht Zentral)","Wohngebiet (Zentral)","Industriegebiet", "Geschäftsviertel","Zentrum")

level_order_HS<- c("Tram","Bus", "Zug","Bus Station", "Bahnhof","Einkaufszentrum", "Universität","Schule Albisriederplatz",  "Park (Stadtrand)" , "Park (Nicht Zentral)","Park (Zentral)",
                   "Wohngebiet (Stadtrand)","Wohngebiet (Nicht Zentral)","Wohngebiet (Zentral)","Industriegebiet", "Geschäftsviertel","Zentrum")


level_order_Class<- c("Rundfunk", "DECT","WLAN",
                      "Downlink","Uplink","TDD")

Heatmaps_NU_Zurich_PS<- ggplot(Zurich_heats_grouped_PS, aes(x = Abundance*100, fill=factor(Class, level = level_order_Class), y=factor(ME, level = level_order_PS))) + 
  geom_bar(position="stack", stat="identity") + 
  #scale_fill_viridis(discrete = T) + 
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", 
                               "#33a02c", "#fb9a99", 
                               "#fdbf6f", "#ff7f00", "#cab2d6",
                               "#bdbdbd", "#525252", "#af6161")) + 
  scale_x_continuous(limits = c(0,101), breaks = c(0, 25, 50, 75, 100))+
  ggtitle("") +labs(y = NULL, x = "Durchschnittsanteil [%]", fill = "HF Quelle")+
  theme_ipsum() + theme(axis.text.x = element_text(size =15,vjust = 0.5), legend.text = element_text(size=14),
                        axis.text.y = element_text(face = c('plain','plain','plain','plain',
                                                            'plain','plain','plain','bold','plain',
                                                            'plain','plain','plain',
                                                            'plain','plain'), size = 15),
                        axis.title.x = element_text(size = 15, hjust = 0.5), plot.title = element_text(size=17))



Heatmaps_NU_Zurich_HS<- ggplot(Zurich_heats_grouped_HS, aes(x = Abundance*100, fill=factor(Class, level = level_order_Class), y=factor(ME, level = level_order_HS))) + 
  geom_bar(position="stack", stat="identity") + 
  #scale_fill_viridis(discrete = T) + 
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", 
                               "#33a02c", "#fb9a99", 
                               "#fdbf6f", "#ff7f00", "#cab2d6",
                               "#bdbdbd", "#525252", "#af6161")) + 
  scale_x_continuous(limits = c(0,101), breaks = c(0, 25, 50, 75, 100))+
  ggtitle("") +labs(y = NULL, x = "Durchschnittsanteil [%]", fill = "HF Quelle")+
  theme_ipsum() + theme(axis.text.x = element_text(size =15,vjust = 0.5), legend.text = element_text(size=14),
                        axis.text.y = element_text(face = c('plain','plain','plain','plain',
                                                            'plain','plain','plain','bold','plain',
                                                            'plain','plain','plain',
                                                            'plain','plain'), size = 15),
                        axis.title.x = element_text(size = 15, hjust = 0.5), plot.title = element_text(size=17))





Aegerten<- ggarrange(Boxplots_NU_Zurich3_2, Heatmaps_NU_Zurich_PS,nrow=2, align = "v",heights = c(1,1),
                      common.legend = F)

Albi<- ggarrange(Boxplots_NU_Zurich4_2, Heatmaps_NU_Zurich_HS,nrow=2, align = "v",heights = c(1,1),
                 common.legend = F)



