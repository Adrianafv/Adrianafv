#### Functions to process ExpoM files

#--------------------------------------------------------------------------------
remove_ExpomRows <- function(data) {
  return(data[-c(1:13, (nrow(data) - 1):nrow(data)), ])
}

#--------------------------------------------------------------------------------
pro_gpsRF <- function(expom) {
  help1 <- c("V111","V112")
  
  for (k in seq_along(help1)) {
    longlat <- eval(parse(text = paste("expom$", help1[k], sep = "")))
    nocoord_index <- grep(x = longlat, pattern = help1[k], fixed = TRUE) # Identify points with no coordinates (0000.0000X or Y)
    grad <- sapply(X = longlat, FUN = function(s) strsplit(s, split = "[.]")[[1]][1])
    grad <- str_sub(grad, end = -3)
    grad[nocoord_index] <- 0
    grad <- as.numeric(grad)
    minutes <- sapply(X = longlat, FUN = function(s) strsplit(s, split = "[.]")[[1]][1])
    minutes <- str_sub(minutes, start = -2)
    minutes[nocoord_index] <- 0
    minutes <- as.numeric(minutes) / 60
    seconds <- sapply(X = longlat, FUN = function(s) strsplit(s, split = "[.]")[[1]][2])
    seconds <- str_sub(seconds, end = -2)
    seconds[nocoord_index] <- 0
    seconds <- as.numeric(paste("0.", seconds, sep = ""))
    seconds <- seconds / 60
    longlat2 <- grad + minutes + seconds
    longlat2[longlat2 %in% c(0)] <- NA
    eval(parse(text = paste("expom$", help1[k], "<-longlat2", sep = "")))
  }
  
  return(expom)
}

#--------------------------------------------------------------------------------
rf_bandnames_colnames <- function(){
  return(data.frame(
    band = c(
      "01-0098M-fm",
      "02-0202M-dab",
      "03-0385M-poly",
      "04-0423M-ism",
      "05-0453M-pmr",
      "06-0508M-tv",
      "07-0584M-tv",
      "08-0660M-tv",
      "09-0718M-ul",
      "10-0748M-tdd",
      "11-0771M-dl",
      "12-0809M-dl",
      "13-0847M-ul",
      "14-0898M-ul",
      "15-0943M-dl",
      "16-1480M-sdl",
      "17-1748M-ul",
      "18-1843M-dl",
      "19-1898M-dect",
      "20-1957M-ul",
      "21-2145M-dl",
      "22-2438M-wifi",
      "23-2535M-ul",
      "24-2593M-tdd",
      "25-2657M-dl",
      "26-3475M-nr",
      "27-3605M-nr",
      "28-3735M-nr",
      "29-5200M-wifi",
      "30-5325M-wifi",
      "31-5450M-wifi",
      "32-5575M-wifi",
      "33-5700M-wifi",
      "34-5825M-wifi",
      "35-5950M-wifi"),
    rms = c(
      "97.75 MHz (RMS)",
      "202 MHz (RMS)",
      "385 MHz (RMS)",
      "422.5 MHz (RMS)",
      "452.5 MHz (RMS)",
      "507.5 MHz (RMS)",
      "583.5 MHz (RMS)",
      "659.5 MHz (RMS)",
      "718 MHz (RMS)",
      "748 MHz (RMS)",
      "770.5 MHz (RMS)",
      "808.5 MHz (RMS)",
      "847 MHz (RMS)",
      "897.5 MHz (RMS)",
      "942.5 MHz (RMS)",
      "1479.5 MHz (RMS)",
      "1747.5 MHz (RMS)",
      "1842.5 MHz (RMS)",
      "1897.5 MHz (RMS)",
      "1957 MHz (RMS)",
      "2145 MHz (RMS)",
      "2438 MHz (RMS)",
      "2535 MHz (RMS)",
      "2592.5 MHz (RMS)",
      "2657 MHz (RMS)",
      "3475 MHz (RMS)",
      "3605 MHz (RMS)",
      "3735 MHz (RMS)",
      "5200 MHz (RMS)",
      "5325 MHz (RMS)",
      "5450 MHz (RMS)",
      "5575 MHz (RMS)",
      "5700 MHz (RMS)",
      "5825 MHz (RMS)",
      "5950 MHz (RMS)"),
    peak = c(
      "97.75 MHz (PEAK)",
      "202 MHz (PEAK)",
      "385 MHz (PEAK)",
      "422.5 MHz (PEAK)",
      "452.5 MHz (PEAK)",
      "507.5 MHz (PEAK)",
      "583.5 MHz (PEAK)",
      "659.5 MHz (PEAK)",
      "718 MHz (PEAK)",
      "748 MHz (PEAK)",
      "770.5 MHz (PEAK)",
      "808.5 MHz (PEAK)",
      "847 MHz (PEAK)",
      "897.5 MHz (PEAK)",
      "942.5 MHz (PEAK)",
      "1479.5 MHz (PEAK)",
      "1747.5 MHz (PEAK)",
      "1842.5 MHz (PEAK)",
      "1897.5 MHz (PEAK)",
      "1957 MHz (PEAK)",
      "2145 MHz (PEAK)",
      "2438 MHz (PEAK)",
      "2535 MHz (PEAK)",
      "2592.5 MHz (PEAK)",
      "2657 MHz (PEAK)",
      "3475 MHz (PEAK)",
      "3605 MHz (PEAK)",
      "3735 MHz (PEAK)",
      "5200 MHz (PEAK)",
      "5325 MHz (PEAK)",
      "5450 MHz (PEAK)",
      "5575 MHz (PEAK)",
      "5700 MHz (PEAK)",
      "5825 MHz (PEAK)",
      "5950 MHz (PEAK)"),
    freq = c(
      97.75,
      202,
      385,
      422.5,
      452.5,
      507.5,
      583.5,
      659.5,
      718,
      748,
      770.5,
      808.5,
      847,
      897.5,
      942.5,
      1479.5,
      1747.5,
      1842.5,
      1897.5,
      1957,
      2145,
      2438,
      2535,
      2592.5,
      2657,
      3550,
      3650,
      3750,
      5200,
      5325,
      5450,
      5575,
      5700,
      5825,
      5950), 
    dbRms = c(
      "data-rms01-0098M-fm", 
      "data-rms02-0202M-dab", 
      "data-rms03-0385M-poly", 
      "data-rms04-0423M-ism", 
      "data-rms05-0453M-pmr", 
      "data-rms06-0508M-tv", 
      "data-rms07-0584M-tv", 
      "data-rms08-0660M-tv", 
      "data-rms09-0718M-ul", 
      "data-rms10-0748M-tdd", 
      "data-rms11-0771M-dl", 
      "data-rms12-0809M-dl", 
      "data-rms13-0847M-ul", 
      "data-rms14-0898M-ul", 
      "data-rms15-0943M-dl", 
      "data-rms16-1480M-sdl", 
      "data-rms17-1748M-ul", 
      "data-rms18-1843M-dl", 
      "data-rms19-1898M-dect", 
      "data-rms20-1957M-ul", 
      "data-rms21-2145M-dl", 
      "data-rms22-2438M-wifi", 
      "data-rms23-2535M-ul", 
      "data-rms24-2593M-tdd", 
      "data-rms25-2657M-dl", 
      "data-rms26-3480M-nr", 
      "data-rms27-3605M-nr", 
      "data-rms28-3735M-nr", 
      "data-rms29-5200M-wifi", 
      "data-rms30-5325M-wifi", 
      "data-rms31-5450M-wifi", 
      "data-rms32-5575M-wifi", 
      "data-rms33-5700M-wifi", 
      "data-rms34-5825M-wifi", 
      "data-rms35-5950M-wifi"), 
    dbPeak = c(
      "data-p01-0098M-fm",
      "data-p02-0202M-dab",
      "data-p03-0385M-poly",
      "data-p04-0423M-ism",
      "data-p05-0453M-pmr",
      "data-p06-0508M-tv",
      "data-p07-0584M-tv",
      "data-p08-0660M-tv",
      "data-p09-0718M-ul",
      "data-p10-0748M-tdd",
      "data-p11-0771M-dl",
      "data-p12-0809M-dl",
      "data-p13-0847M-ul",
      "data-p14-0898M-ul",
      "data-p15-0943M-dl",
      "data-p16-1480M-sdl",
      "data-p17-1748M-ul",
      "data-p18-1843M-dl",
      "data-p19-1898M-dect",
      "data-p20-1957M-ul",
      "data-p21-2145M-dl",
      "data-p22-2438M-wifi",
      "data-p23-2535M-ul",
      "data-p24-2593M-tdd",
      "data-p25-2657M-dl",
      "data-p26-3475M-nr",
      "data-p27-3605M-nr",
      "data-p28-3735M-nr",
      "data-p29-5200M-wifi",
      "data-p30-5325M-wifi",
      "data-p31-5450M-wifi",
      "data-p32-5575M-wifi",
      "data-p33-5700M-wifi",
      "data-p34-5825M-wifi",
      "data-p35-5950M-wifi"
    )))
}

#--------------------------------------------------------------------------------
#function to replace a column by another in original RF file
replaceColumn = function(dataDB_cor, dataDBtruncated_cor, type, frequency){
  
  
  startRow <- which(dataDB_cor == "Band Width")+1
  endRow <- length(dataDB_cor[,1])
  selectColumn <- which(dataDB_cor == frequency, arr.ind=TRUE)
  if (type == "rms"){ col <- selectColumn[1,2]}
  if (type == "p"){ col <- selectColumn[2,2]}
  
  dataDB_cor[startRow:endRow, col] <- as.character(dataDBtruncated_cor[ , paste("data_", type, str_replace_all(frequency, "-", "_"), sep = "")])
  return(dataDB_cor)
  
}

#--------------------------------------------------------------------------------
#Conversion functions
convert_Vm_to_mWm2=function(expom_basic, bands){
  
  #Change the corresponding columns
  expom_mWm2<-expom_basic
  expom_mWm2[, bands]<-lapply(expom_mWm2[, bands],function(xx) xx^2*1000/376.7)
  
  #2) Return converted data:
  return(expom_mWm2)
}


convert_mWm2_to_Vm=function(expom_mWm2, bands){
  
  #Change the corresponding columns
  expom_Vm<-expom_mWm2
  expom_Vm[, bands]<-lapply(expom_Vm[, bands],function(yy) sqrt(376.7/1000*yy))
  
  #2) Return converted data:
  return(expom_Vm)
}

#--------------------------------------------------------------------------------
#Function to run the crosstalk correction one after an other
applyCrosstalk = function(dataDBtruncated, 
                          FB1Name, 
                          FB2Name, 
                          typeName, 
                          threshold){
  
  #processed <- spotRfData
  #processed$datetime = as.POSIXct(processed$Date_Time)
  processed_cor <- dataDBtruncated
  
  for( type in typeName) {
    
    if( type == 'rms') {ratio <- 10}
    if( type == 'p') {ratio <- 50}
    
    
    for( i in 1:length(FB1Name)) {
      
      # i <- 1
      # type = 'rms'
      # type = 'p'
      # ratio <- 50
      
      
      
      FB1 <- paste("data_", type, FB1Name[i], sep = "")
      FB2 <- paste("data_", type, FB2Name[i], sep = "")
      print(c(FB1, FB2))
      
      #extract 2 frequency bands
      proExtract_Vm <- processed_cor[ , c('Date_Time' , FB1, FB2)] 
      
      
      #activityvar1 is negligible as the data are already separated by activity
      #Apply if not measID
      #proExtract_Vm[is.na(expom_mW$mobileClass1), "mobileClass1" ] <- ""
      
      #Transform only the pairs band to mWm2
      proExtract_mW <- convert_Vm_to_mWm2(proExtract_Vm, c(FB1, FB2))
      proExtract_mW$PosixTime <- proExtract_mW$Date_Time
      
      #Apply the crosstalk correction
      correctionCrosstalk<- correct_crosstalk(
        dataset = proExtract_mW, 
        timevar = "PosixTime", 
        signal1 = FB1,
        signal2= FB2, 
        stats= TRUE, 
        window_width = 4,
        change_threshold = ratio, 
        correlation_threshold = threshold[i]) #,plot_folder="C:/Users/sandne/Desktop/Plot_folder"
      
      expom_mWCrosstalk <- correctionCrosstalk$df
      
      #Save the statistics
      expom_mWStats <- correctionCrosstalk$stats
      
      FB1_cor <- paste(FB1, 'cor', sep = '_')
      FB2_cor <- paste(FB2, 'cor', sep = '_')
      
      #Paste the results of the correction into the truncated data
      proExtract_mW_cor <- proExtract_mW
      proExtract_mW_cor[ , c(FB1, FB2)] <- expom_mWCrosstalk[ , c(FB1_cor, FB2_cor)]
      
      #Convert back to Vm
      proExtract_Vm_cor <- convert_mWm2_to_Vm(proExtract_mW_cor, c(FB1, FB2))
      
      
      #Replace the corrected band into the original truncated file
      processed_cor[ , c(FB1, FB2)] <- proExtract_Vm_cor[ , c(FB1, FB2)]
      
    }
  }
  return(processed_cor)
}

#--------------------------------------------------------------------------------
#Function correct_crosstalk:
correct_crosstalk=function(dataset,
                           timevar,
                           signal1,
                           signal2,
                           activityvar,
                           no_ct_sig1,
                           no_ct_sig2,
                           stats=FALSE,
                           plot_folder,
                           plot_nr,
                           window_width=4,
                           change_threshold=10,
                           correlation_threshold=0.20,
                           suppressMessages){
  
  #0) Check settings, generate messages, set defaults:
  if(missing(dataset)){stop("Please specify a dataset...")}
  if(missing(timevar)){stop("Please specify the name of the time variables: timevar")}
  if(missing(signal1)){stop("Please specify the first variable considered for crosstalk: signal1")}
  if(missing(signal2)){stop("Please specify the first variable considered for crosstalk: signal2")}
  if(missing(suppressMessages)){suppressMessages<-FALSE}
  if(suppressMessages==FALSE){
    if(missing(activityvar)&missing(no_ct_sig1)&missing(no_ct_sig2)){message("activityvar not set, default correction to reporting limit (minimum value)")}
    if(!missing(activityvar)){message("Corrections based on median during same activity")}
    if(missing(stats)){message("stats not set, default is: FALSE")}
    if(stats==TRUE){message("Output will be a list with [[1]]: corrected dataset and [[2]]: stats about the corrections")}
    if(missing(window_width)){message("window_width not set, default is: 4")}
    if(missing(change_threshold)){message("change_threshold not set, default is: 10")}
    if(missing(correlation_threshold)){message("correlation_threshold not set, default is: 0.20")}
    if(missing(plot_folder)){message("No plot_folder specified. Time series plots will not be generated.")}}
  # You need the suggested package for this function
  if(missing(plot_folder)==FALSE&!requireNamespace("ggplot2",quietly=TRUE)){
    stop("package ggplot2 is needed for this function to work. Please install it.")
  }
  if(missing(plot_folder)==FALSE&!requireNamespace("gridExtra",quietly=TRUE)){
    stop("package gridExtra is needed for this function to work. Please install it.")
  }
  if(missing(plot_folder)==FALSE&!requireNamespace("reshape2",quietly=TRUE)){
    stop("package reshape2 is needed for this function to work. Please install it.")
  }
  if(missing(plot_folder)==FALSE&!requireNamespace("scales",quietly=TRUE)){
    stop("package scales is needed for this function to work. Please install it.")
  }
  
  #1) Extract the relevant variables from the dataset:
  if(!missing(activityvar)){dat<-subset(dataset,select=c(timevar,signal1,signal2,activityvar))}
  if(missing(activityvar)){dat<-subset(dataset,select=c(timevar,signal1,signal2))}
  dat$sig1_uncor<-dat[[signal1]]
  dat$sig2_uncor<-dat[[signal2]]
  
  #2) Calculate means for both signals during window before and after each observation
  dat$avg_s1_left<-rollapply(lag(dat$sig1_uncor),width=window_width,FUN=mean,na.rm=TRUE,align="left",fill=NA)
  dat$avg_s1_right<-rollapply(lead(dat$sig1_uncor),width=window_width,FUN=mean,na.rm=TRUE,align="right",fill=NA)
  dat$avg_s2_left<-rollapply(lag(dat$sig2_uncor),width=window_width,FUN=mean,na.rm=TRUE,align="left",fill=NA)
  dat$avg_s2_right<-rollapply(lead(dat$sig2_uncor),width=window_width,FUN=mean,na.rm=TRUE,align="right",fill=NA)
  #Calculate the rate of change (ROC) for both signals individually
  dat$ROCslope1<-abs((dat$avg_s1_left-dat$avg_s1_right)/dat$sig1_uncor)
  dat$ROCslope2<-abs((dat$avg_s2_left-dat$avg_s2_right)/dat$sig2_uncor)
  
  #3) Calculate ratios between signal1 and signal2 and the other way around, if they change quickly -> new cluster
  dat$rat_sig1_sig2<-dat$sig1_uncor/dat$sig2_uncor
  dat$rat_abs<-ifelse(dat$rat_sig1_sig2<1,dat$rat_sig1_sig2,1/dat$rat_sig1_sig2)
  dat$avg_rat_abs_left<-rollapply(lag(dat$rat_abs),width=window_width,FUN=mean,na.rm=TRUE,align="left",fill=NA)
  dat$avg_rat_abs_right<-rollapply(lead(dat$rat_abs),width=window_width,FUN=mean,na.rm=TRUE,align="right",fill=NA)
  #Calculate the rate of change (ROC) for the ratio between the signals
  dat$ROCratio<-abs((dat$avg_rat_abs_left-dat$avg_rat_abs_right)/dat$rat_abs)
  
  #4) If one of the ROCs is above the threshold, define indicators a new potential cluster
  dat$change_ind_temp<-0
  dat$change_ind_temp[dat$ROCslope1>change_threshold|dat$ROCslope2>change_threshold|dat$ROCratio>change_threshold]<-1
  dat$change_ind_temp[is.na(dat$sig1_uncor)|is.na(dat$sig2_uncor)]<-0
  
  #5) Define the temporary clusters based on the change indicators:
  dat$cluster_temp<-cumsum(dat$change_ind_temp)+1
  
  #6) Many clusters are defined 4/8/12 seconds apart. We need at least 3 observations in each cluster to calculate a correlation. Therefore, we look for any double / triple / multiple-identified cluster starts. If k+1 k+2 or k+3 is also identified as a cluster, this is undesirable and should be resolved
  dat$change_ind_def<-dat$change_ind_temp
  dat$cluster_def<-dat$cluster_temp
  length_k<-dim(dat)[1]
  #Resolve closely spaced potential clusters
  #With consecutive cluster indicators
  for (k in 1:length_k){
    if(dat$change_ind_def[k]==1&dat$change_ind_def[k+1]==1){
      #If ROCratio is higher for [k+1] than for [k], we run cluster [k-1] 1 instance longer
      if(dat$ROCratio[k]<=dat$ROCratio[k+1]){
        dat$cluster_def[k]<-dat$cluster_def[k-1]
        dat$change_ind_def[k]<-0}
      #If ROCratio is higher for [k] than for [k+1], we start cluster [k+1] 1 instance earlier
      if(dat$ROCratio[k]>dat$ROCratio[k+1]){
        dat$cluster_def[k]<-dat$cluster_def[k+1]
        dat$change_ind_def[k+1]<-0}}}
  #With one observation in between
  for (k in 1:length_k){
    if(dat$change_ind_def[k]==1&dat$change_ind_def[k+2]==1){
      if(dat$ROCratio[k]<=dat$ROCratio[k+2]){
        dat$cluster_def[k]<-dat$cluster_def[k-1]
        dat$cluster_def[k+1]<-dat$cluster_def[k-1]
        dat$change_ind_def[k]<-0}
      if(dat$ROCratio[k]>dat$ROCratio[k+2]){
        dat$cluster_def[k]<-dat$cluster_def[k+2]
        dat$cluster_def[k+1]<-dat$cluster_def[k+2]
        dat$change_ind_def[k+2]<-0}}}
  #With two observations in between
  for (k in 1:length_k){
    if(dat$change_ind_def[k]==1&dat$change_ind_def[k+3]==1){
      if(dat$ROCratio[k]<=dat$ROCratio[k+3]){
        dat$cluster_def[k]<-dat$cluster_def[k-1]
        dat$cluster_def[k+1]<-dat$cluster_def[k-1]
        dat$cluster_def[k+2]<-dat$cluster_def[k-1]
        dat$change_ind_def[k]<-0}
      if(dat$ROCratio[k]>dat$ROCratio[k+3]){
        dat$cluster_def[k]<-dat$cluster_def[k+3]
        dat$cluster_def[k+1]<-dat$cluster_def[k+3]
        dat$cluster_def[k+2]<-dat$cluster_def[k+3]
        dat$change_ind_def[k+3]<-0}}}
  
  #7) Calculate the correlation and ratio between the signals for each cluster
  rep_limit_sig1<-min(dat$sig1_uncor)
  rep_limit_sig2<-min(dat$sig2_uncor)
  cluster_corrs<-data.frame(cluster_def=unique(dat$cluster_def))
  cluster_corrs$correlation<-0
  cluster_corrs$correlation_log<-0
  cluster_corrs$ratio<-1
  for(k in 1:length(cluster_corrs$cluster_def)){
    dat_sel<-subset(dat,cluster_def==cluster_corrs$cluster_def[k]&sig1_uncor>rep_limit_sig1&sig2_uncor>rep_limit_sig2)
    if(dim(dat_sel)[1]>2&var(dat_sel$sig1_uncor)>0&var(dat_sel$sig2_uncor)>0){
      cluster_corrs$correlation[k]<-cor(dat_sel$sig1_uncor,dat_sel$sig2_uncor,use="complete")
      cluster_corrs$correlation_log[k]<-cor(log(dat_sel$sig1_uncor),log(dat_sel$sig2_uncor),use="complete")
      cluster_corrs$ratio[k]<-mean(dat_sel$sig1_uncor,na.rm=TRUE)/mean(dat_sel$sig2_uncor,na.rm=TRUE)
    }
  }
  
  #8) Merge correlation and ratio statistics into time series.
  dat<-merge(dat,cluster_corrs,by="cluster_def")
  #Set default to no corrections: <-0
  dat$sig1_corrected<-0
  dat$sig2_corrected<-0
  #Set default corrected values to the basic corrected ones
  dat$sig1_cor<-dat$sig1_uncor
  dat$sig2_cor<-dat$sig2_uncor
  #If correlation is high and direction is right, DO correct: <-1
  dat$sig1_corrected[dat$correlation_log>correlation_threshold&dat$ratio<1]<-1
  dat$sig2_corrected[dat$correlation_log>correlation_threshold&dat$ratio>1]<-1
  
  #9) Define the correction values and correct the affected values:
  if(missing(activityvar)){
    if(missing(no_ct_sig1)&missing(no_ct_sig2)){
      #Set values to the minimum reported in the dataset:
      no_ct_sig1<-min(dat$sig1_uncor)
      no_ct_sig2<-min(dat$sig2_uncor)
    }
    #If no_ct_sig1 and no_ct_sig2 were not missing, they were set by the user, so use those:
    dat$sig1_cor[dat$sig1_corrected==1&dat$sig1_uncor>no_ct_sig1]<-no_ct_sig1
    dat$sig2_cor[dat$sig2_corrected==1&dat$sig2_uncor>no_ct_sig2]<-no_ct_sig2
  }
  if(!missing(activityvar)){
    activities<-data.frame(activity=unique(dat[[activityvar]]))
    #Set default corrected values to the basic corrected ones
    activities$no_ct_sig1<-min(dat$sig1_uncor)
    activities$no_ct_sig2<-min(dat$sig2_uncor)
    #If there are enough observations available, take the medians of unaffected observations
    for(m in activities$activity){
      dat_no_ct_sig1<-dat[dat[,activityvar]==m&dat$sig1_corrected==0,]
      dat_no_ct_sig2<-dat[dat[,activityvar]==m&dat$sig2_corrected==0,]
      if(dim(dat_no_ct_sig1)[1]>5){
        activities$no_ct_sig1[activities$activity==m]<-median(dat_no_ct_sig1$sig1_uncor)}
      if(dim(dat_no_ct_sig2)[1]>5){
        activities$no_ct_sig2[activities$activity==m]<-median(dat_no_ct_sig2$sig2_uncor)}
      #Then assign those medians to the crosstalk affected values
      dat$sig1_corrected[dat[,activityvar]==m&dat$sig1_uncor<=activities$no_ct_sig1[activities$activity==m]]<-0
      dat$sig2_corrected[dat[,activityvar]==m&dat$sig2_uncor<=activities$no_ct_sig2[activities$activity==m]]<-0
      dat$sig1_cor[dat$sig1_corrected==1&dat[,activityvar]==m]<-activities$no_ct_sig1[activities$activity==m]
      dat$sig2_cor[dat$sig2_corrected==1&dat[,activityvar]==m]<-activities$no_ct_sig2[activities$activity==m]
    }
  }
  dat_cor<-dat[,c("change_ind_temp","change_ind_def","cluster_def","correlation_log","sig1_corrected","sig2_corrected","sig1_cor","sig2_cor")]
  newnames<-c(paste0(signal1,"_cor_01"),paste0(signal2,"_cor_01"),paste0(signal1,"_cor"),paste0(signal2,"_cor"))
  names(dat_cor)<-c("change_ind_temp","change_ind_def","cluster_def","correlation_log",newnames)
  dataset_cor<-cbind(dataset,dat_cor)
  
  #10) Write 1 time series plot per cluster (up to maximum of plot_nr) to the plot_folder specified
  if(!missing(plot_folder)){
    if(suppressMessages==FALSE){message(paste0("Time series plots will be exported to: ",plot_folder))}
    #Plot text settings
    text_s<-element_text(colour="black",size=14)
    text_b<-element_text(colour="black",size=16)
    #Make a plot for all unique new cluster numbers
    if(missing(plot_nr)){plot_nr<-length(unique(dataset_cor$cluster_def))}
    for (k in head(unique(dataset_cor$cluster_def),plot_nr)){
      starttime_cluster<-head(subset(dataset_cor,cluster_def==k,select=timevar),1)[1,1]
      stoptime_cluster<-tail(subset(dataset_cor,cluster_def==k,select=timevar),1)[1,1]
      duration_cluster<-stoptime_cluster-starttime_cluster
      starttime_graph<-starttime_cluster-duration_cluster/3-60
      stoptime_graph<-stoptime_cluster+duration_cluster/3+60
      dat_sel1<-subset(dataset_cor,PosixTime>starttime_graph&PosixTime<stoptime_graph)
      dat_sel2<-subset(dat_sel1,PosixTime>=starttime_cluster&PosixTime<stoptime_cluster)
      correlation_log<-round(dat_sel2$correlation_log[1],digits=2)
      old_times_of_change<-subset(dat_sel1,change_ind_temp==1,select=timevar)
      new_times_of_change<-subset(dat_sel1,change_ind_def==1,select=timevar)
      main_title<-paste0("Cluster ",k,", correlation (log) = ",correlation_log)
      #Reorganize the data:
      dat_sel1a<-subset(dat_sel1,select=c(timevar,signal1,signal2,newnames[3:4]))
      dat_sel1a_melt<-melt(dat_sel1a,id=timevar)
      names(dat_sel1a_melt)<-c("time","band","value")
      p1<-ggplot(data=dat_sel1a_melt,aes(x=time,y=value))+
        geom_line(aes(color=band))+
        geom_vline(data=old_times_of_change,aes(xintercept=as.numeric(PosixTime)),color="yellow")+
        geom_vline(data=new_times_of_change,aes(xintercept=as.numeric(PosixTime)),color="black")+
        scale_size_manual(values=c(1,1,0.5,0.5))+
        scale_color_manual(values=c("#A6CEE3","#FB9A99","#1F78B4","#E31A1C"))+
        theme(title=text_b,axis.title=text_s,axis.text=text_s,legend.text=text_s,legend.title=element_blank(),legend.position="bottom")+
        ggtitle(main_title)+labs(x="",y="Exposure level")+
        scale_x_datetime(labels=date_format("%m/%d %H:%M"))+
        scale_y_log10(limits=c(0.000001,10))
      #Correlation scatterplot
      dat_sel2a<-subset(dat_sel2,select=c(timevar,signal1,signal2))
      names(dat_sel2a)<-c(timevar,"signal1","signal2")
      p2<-ggplot(data=dat_sel2a,aes(signal1,signal2))+geom_point()+
        labs(x=paste0("Exposure level ",signal1),y=paste0("Exposure level ",signal2))+
        geom_smooth(method='lm',formula=y~x,linetype="dashed",color="red",size=1)+
        theme(title=text_b,axis.title=text_s,axis.text=text_s,legend.text=text_s)+
        scale_x_log10(limits=c(0.000001,10))+
        scale_y_log10(limits=c(0.000001,10))
      #Combine the plots and save
      p<-grid.arrange(p1,p2,layout_matrix=cbind(1,1,1,1,1,1,2,2,2))
      ggsave(paste0(plot_folder,"cluster_",k,".png"),plot=p,units="cm",dpi=600,width=30,height=10)
    }
  }
  
  #11) Produce statistics about the correction as 2nd part of output
  if(stats==TRUE){
    n_obs<-dim(dataset_cor)[1]
    duration_hours<-as.numeric(difftime(tail(dataset_cor$PosixTime,1),head(dataset_cor$PosixTime,1),units="hours"))
    duration_days<-as.numeric(difftime(tail(dataset_cor$PosixTime,1),head(dataset_cor$PosixTime,1),units="days"))
    nr_potential_clusters<-sum(dataset_cor$change_ind_temp)
    nr_definitive_clusters<-sum(dataset_cor$change_ind_def)
    eval(parse(text=paste0("nr_corrected_sig1<-sum(dataset_cor$",signal1,"_cor_01)")))
    eval(parse(text=paste0("nr_corrected_sig2<-sum(dataset_cor$",signal2,"_cor_01)")))
    eval(parse(text=paste0("mean_sig1_uncor<-mean(dataset_cor$",signal1,",na.rm=T)")))
    eval(parse(text=paste0("mean_sig2_uncor<-mean(dataset_cor$",signal2,",na.rm=T)")))
    eval(parse(text=paste0("mean_sig1_cor<-mean(dataset_cor$",signal1,"_cor,na.rm=T)")))
    eval(parse(text=paste0("mean_sig2_cor<-mean(dataset_cor$",signal2,"_cor,na.rm=T)")))
    perc_corrected_sig1<-(nr_corrected_sig1/n_obs)*100
    perc_corrected_sig2<-(nr_corrected_sig2/n_obs)*100
    perc_reduction_sig1<-(mean_sig1_uncor-mean_sig1_cor)/mean_sig1_uncor*100
    perc_reduction_sig2<-(mean_sig2_uncor-mean_sig2_cor)/mean_sig2_uncor*100
    statistics<-cbind(n_obs,duration_hours,duration_days,
                      nr_potential_clusters,nr_definitive_clusters,nr_corrected_sig1,nr_corrected_sig2,
                      mean_sig1_uncor,mean_sig2_uncor,mean_sig1_cor,mean_sig2_cor,
                      perc_corrected_sig1,perc_corrected_sig2,perc_reduction_sig1,perc_reduction_sig2)
    if(suppressMessages==FALSE){
      message(paste0(signif(perc_corrected_sig1,3),"% Of observations were corrected for ",signal1,": mean exposure reduced by ",signif(perc_reduction_sig1,digits=3),"%"))
      message(paste0(signif(perc_corrected_sig2,3),"% Of observations were corrected for ",signal2,": mean exposure reduced by ",signif(perc_reduction_sig2,digits=3),"%"))
    }
    dataset_cor<-list(df=dataset_cor,stats=statistics)
  }
  
  #12) Return resulting dataset
  return(dataset_cor)
}

#--------------------------------------------------------------------------------
#Function to apply the crosstalk correction
applyCrosstalk_singleData = function(expom){
  
  dataDB <- expom
  #Band pairs to apply crosstalk 
  FB1Name <- c('08-0660M-tv', 
               '09-0718M-ul', 
               '10-0748M-tdd', 
               '18-1843M-dl')
  
  FB2Name <- c('09-0718M-ul', 
               '10-0748M-tdd', 
               '18-1843M-dl', 
               '19-1898M-dect')
  #Set the correlation threshold
  threshold <- c(0.2, 
                 0.4, 
                 0.4, 
                 0.2)
  typeName <- c('rms',
                'p')
  
  #Remove rows
  dataDBtruncated <- remove_ExpomRows(dataDB)

  #Rename with compatible name
  bandNamesRf <- rf_bandnames_colnames()
  bandNamesRf$dbRms <- str_replace_all(bandNamesRf$dbRms, "-", "_")
  bandNamesRf$dbPeak <- str_replace_all(bandNamesRf$dbPeak, "-", "_")
  FB1Name <- str_replace_all(FB1Name, "-", "_")
  FB2Name <- str_replace_all(FB2Name, "-", "_")
  names(dataDBtruncated)[c(1:72)] <- c("Date_Time", "", bandNamesRf$dbRms, bandNamesRf$dbPeak)
  
  #Transform to integer and Posixct
  dataDBtruncated[ , c(bandNamesRf$dbRms, bandNamesRf$dbPeak)] <- lapply(dataDBtruncated[ , c(bandNamesRf$dbRms, bandNamesRf$dbPeak)] ,as.numeric)
  dataDBtruncated$Date_Time <- as.POSIXct(x=strptime(dataDBtruncated$Date_Time, format="%m/%d/%Y %H:%M:%S"), tz="Europe/Zurich")  
  
  
  frequencyName <- c('08-0660M-tv',
                     '09-0718M-ul', 
                     '10-0748M-tdd', 
                     '18-1843M-dl',
                     '19-1898M-dect')

  #Apply the crosstalk correction on one data set
  dataDBtruncated_cor <- applyCrosstalk(dataDBtruncated, 
                                        FB1Name,
                                        FB2Name, 
                                        typeName, 
                                        threshold)
  
  #Paste the corrected values into old file
  dataDB_cor <- dataDBtruncated_cor
  return(dataDB_cor)
}

#--------------------------------------------------------------------------------
Process_RF = function(expom) {
  
  #apply cross-talk correction
  expom<-applyCrosstalk_singleData(expom)
  
  #convert to numeric
  expom[2:110]<-lapply(expom[2:110],as.numeric)
  expom[113:119]<-lapply(expom[113:119],as.numeric)
  #Converto DateTime to POSIXct
  expom[, 1]<-as.POSIXct(expom[, 1], tz="Europe/Zurich", format="%m/%d/%Y %H:%M:%S")
  
  #convert GPS data
  expom<-pro_gpsRF(expom)
  
  #Change column names
  colnames(expom)[c(1:72, 111:113, 118)]<-c("datetime",
              "seq",
              "FM_rms","DAB_rms","Poly_rms", "ISM_rms","PMR_rms","TV1_rms","TV2_rms",
              "TV3_rms","ul7_rms", "tdd7_rms", "dl7_rms", "dl8_rms", "ul8_rms", "ul9_rms",
              "dl9_rms", "sdl14_rms", "ul18_rms", "dl18_rms",
              "DECT_rms", "ul21_rms", "dl21_rms", "wifi2_rms", "ul26_rms", "tdd26_rms",
              "dl26_rms", "M35a_rms", "M35b_rms", "M35c_rms", "wifi5a_rms","wifi5b_rms",
              "wifi5c_rms","wifi5d_rms","wifi5e_rms","wifi5.8a_rms", "wifi5.8b_rms",
              
              "FM_p","DAB_p","Poly_p", "ISM_p","PMR_p","TV1_p","TV2_p",
              "TV3_p","ul7_p", "tdd7_p", "dl7_p", "dl8_p", "ul8_p", "ul9_p", "dl9_p",
              "sdl14_p", "ul18_p", "dl18_p",
              "DECT_p", "ul21_p", "dl21_p", "wifi2_p", "ul26_p", "tdd26_p",
              "dl26_p", "M35a_p", "M35b_p", "M35c_p", "wifi5a_p","wifi5b_p",
              "wifi5c_p","wifi5d_p","wifi5e_p","wifi5.8a_p", "wifi5.8b_p", 
              
              "GPS_lat", "GPS_lon", "GPS_alt", "Charge"
  )

  return(expom[, c(1:72, 111:113, 118)])
}
