###
###
###

#Library
library(stringr)
library(zoo)
library(EMFtools)
library(glue)
library(tidyverse)
library(berryFunctions)


#Source connecting to other R script - With CrossTalk funtions
source("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/R codes/01. Expom cross talk correction/scripts00_generalFunctions.R")


#01. Switzerland####
#list all files in the folder - set wd with the files 
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Raw Data/01. ExpoM/ExpoM corrections/Switzerland")
file_list_CH <- list.files(pattern="*.csv", recursive = TRUE )
file_list_CH

#start loop to read in all files
all_ExpoM_CH=NULL
for (i in seq_along(file_list_CH)){
  expom <- read.csv(file_list_CH[i], na.strings=c("","NA"),as.is=T, quote = "", header = F, sep="\t")
  expom_new<-Process_RF(expom)
  all_ExpoM_CH <- rbind(all_ExpoM_CH, expom_new)
}
#end loop

expom_new<-Process_RF(expom)


setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Modified data/01. ExpoM_correction cross talk")

write.csv(all_ExpoM_CH, "ExpoMRF_crosstalk corrected_CH.csv")




#02. Belgium####
#list all files in the folder - 
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Raw Data/01. ExpoM/ExpoM corrections/Belgium")
file_list_BE <- list.files(pattern="*.csv", recursive = TRUE )
file_list_BE

#start loop to read in all files
all_ExpoM_BE=NULL
for (i in seq_along(file_list_BE)){
  expom <- read.csv(file_list_BE[i], na.strings=c("","NA"),as.is=T, quote = "", header = F, sep="\t")
  expom_new<-Process_RF(expom)
  all_ExpoM_BE <- rbind(all_ExpoM_BE, expom_new)
}
#end loop

expom_new<-Process_RF(expom)

setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Modified data/01. ExpoM_correction cross talk")

write.csv(all_ExpoM_BE, "ExpoMRF_crosstalk corrected_BE.csv")





#03. Netherlands####
#list all files in the folder - 
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Raw Data/01. ExpoM/ExpoM corrections/Netherlands")
file_list_NL <- list.files(pattern="*.csv", recursive = TRUE )
file_list_NL

#start loop to read in all files
all_ExpoM_NL=NULL
for (i in seq_along(file_list_NL)){
  expom <- read.csv(file_list_NL[i], na.strings=c("","NA"),as.is=T, quote = "", header = F, sep="\t")
  expom_new<-Process_RF(expom)
  all_ExpoM_NL <- rbind(all_ExpoM_NL, expom_new)
}
#end loop

expom_new<-Process_RF(expom)


setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Modified data/01. ExpoM_correction cross talk")
write.csv(all_ExpoM_NL, "ExpoMRF_crosstalk corrected_NL.csv")



#04. UK####
#list all files in the folder - 
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Raw Data/01. ExpoM/ExpoM corrections/UK")
file_list_UK <- list.files(pattern="*.csv", recursive = TRUE )
file_list_UK

#start loop to read in all files
all_ExpoM_UK=NULL
for (i in seq_along(file_list_UK)){
  expom <- read.csv(file_list_UK[i], na.strings=c("","NA"),as.is=T, quote = "", header = F, sep="\t")
  expom_new<-Process_RF(expom)
  all_ExpoM_UK <- rbind(all_ExpoM_UK, expom_new)
}
#end loop

expom_new<-Process_RF(expom)


setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Modified data/01. ExpoM_correction cross talk")
write.csv(all_ExpoM_UK, "ExpoMRF_crosstalk corrected_UK.csv")





#05. Hungary####
#list all files in the folder - 
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Raw Data/01. ExpoM/ExpoM corrections/Hungary")
file_list_HU <- list.files(pattern="*.csv", recursive = TRUE )
file_list_HU

#start loop to read in all files
all_ExpoM_HU=NULL
for (i in seq_along(file_list_HU)){
  expom <- read.csv(file_list_HU[i], na.strings=c("","NA"),as.is=T, quote = "", header = F, sep="\t")
  expom_new<-Process_RF(expom)
  all_ExpoM_HU <- rbind(all_ExpoM_HU, expom_new)
}
#end loop

expom_new<-Process_RF(expom)

setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Modified data/01. ExpoM_correction cross talk")
write.csv(all_ExpoM_HU, "ExpoMRF_crosstalk corrected_HU.csv")






#06. Spain####
#list all files in the folder - 
setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Raw Data/01. ExpoM/Spain")
file_list_ES <- list.files(pattern="*.csv", recursive = TRUE )
file_list_ES

#start loop to read in all files
all_ExpoM_ES=NULL
for (i in seq_along(file_list_ES)){
  expom <- read.csv(file_list_ES[i], na.strings=c("","NA"),as.is=T, quote = "", header = F, sep="\t")
  expom_new<-Process_RF(expom)
  all_ExpoM_ES <- rbind(all_ExpoM_ES, expom_new)
}
#end loop

expom_new<-Process_RF(expom)

setwd("U:/GOLIAT/Task 1.2.1 Measurement Protocol GOLIAT/Measurements 2023/Data analysis/Modified data/01. ExpoM_correction cross talk")
write.csv(all_ExpoM_ES, "ExpoMRF_crosstalk corrected_ES.csv")




#07. France#### - Not yet done
#list all files in the folder - 
file_list <- list.files(pattern="*.csv", recursive = TRUE )
file_list

#start loop to read in all files
all_ExpoM=NULL
for (i in seq_along(file_list)){
  expom <- read.csv(file_list[i], na.strings=c("","NA"),as.is=T, quote = "", header = F, sep="\t")
  expom_new<-Process_RF(expom)
  all_ExpoM <- rbind(all_ExpoM, expom_new)
}
#end loop

expom_new<-Process_RF(expom)





#08. Italy#### - Not yet done
#list all files in the folder - 
file_list <- list.files(pattern="*.csv", recursive = TRUE )
file_list

#start loop to read in all files
all_ExpoM=NULL
for (i in seq_along(file_list)){
  expom <- read.csv(file_list[i], na.strings=c("","NA"),as.is=T, quote = "", header = F, sep="\t")
  expom_new<-Process_RF(expom)
  all_ExpoM <- rbind(all_ExpoM, expom_new)
}
#end loop

expom_new<-Process_RF(expom)




#09. Austria#### - Not yet done
#list all files in the folder - 
file_list <- list.files(pattern="*.csv", recursive = TRUE )
file_list

#start loop to read in all files
all_ExpoM=NULL
for (i in seq_along(file_list)){
  expom <- read.csv(file_list[i], na.strings=c("","NA"),as.is=T, quote = "", header = F, sep="\t")
  expom_new<-Process_RF(expom)
  all_ExpoM <- rbind(all_ExpoM, expom_new)
}
#end loop

expom_new<-Process_RF(expom)





#10. Poland #### - Not yet done
#list all files in the folder - 
file_list <- list.files(pattern="*.csv", recursive = TRUE )
file_list

#start loop to read in all files
all_ExpoM=NULL
for (i in seq_along(file_list)){
  expom <- read.csv(file_list[i], na.strings=c("","NA"),as.is=T, quote = "", header = F, sep="\t")
  expom_new<-Process_RF(expom)
  all_ExpoM <- rbind(all_ExpoM, expom_new)
}
#end loop

expom_new<-Process_RF(expom)