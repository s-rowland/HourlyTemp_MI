####***********************************************************
# Temperature Variability- CVD Analysis 
# 6 Compute Temperature Variability, Mean Temperature, and Combine Years
# Note: this step is broken up by year because it is computationally intensive
# Sebastian T. Rowland 
# Updated 19.7.2019
####***********************************************************
#### Table of Contents #### 
# N: Notes
# 0: Preparation 
# 1: Define Function to Compute Variability of One Variable
# 2: Define Function to Compute Variability and Averages for a Year
# 3: Compute Variability and Averages for Each Year
####***********************************************************
####**************
#### N: Notes #### 
####**************
# Na
# This step is broken up by year because it is computationally intensive

####********************
#### 0: Preparation #### 
####********************
# 0a Load packages
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(lubridate)
library(magrittr)
library(tidyr)
library(ggplot2)

# 0b Declare directories
data.folder <- "H:/Temp_CVD_Analysis/Data/"
raw.data.folder <- paste0(data.folder,"Raw_Data/")
raw.outcome.folder <- paste0(raw.data.folder, "Outcome_Data/")
raw.nldas.folder <- paste0(raw.data.folder, "NLDAS_Zip_Data/")
intermediate.data.folder <- paste0(data.folder, "Intermediate_Data/")
final.data.folder <- paste0(data.folder, "Final_Data/")

# 0c Set CaseType 
# either mi or stroke
CaseType <- "mi"
#
####***************************************************************
#### 1: Define Function to Compute Variability of One Variable ####
####***************************************************************
# 1a Declare function
compute_variability <- function(ActiveYYYY, WeatherVariable, VarInitial, VarInitial2){
#ActiveYYYY <- 2002 ; WeatherVariable <- "temp" ; VarInitial <- "t" ; VarInitial2 <- "T"
  
  # 1b Readin data
  df <- fst::read_fst(paste0(intermediate.data.folder, "assigned_days_", CaseType,"_", ActiveYYYY,"_",WeatherVariable, ".fst" ))
  
  # 1c keep only our vairables of interest and put in long format
  df.e <- df %>% select(DayName0,contains(paste0(VarInitial, "Lag_"))) %>% 
          gather("LagHour", "Exposure", contains(paste0(VarInitial, "Lag_"))) 

  # 1d Remove the hours after 47 
  # hours 47-57 are only relevant for a sensitivity analysis, and not for computing variability 
  df.e <- df.e %>% mutate(LagHourNum = as.numeric(str_sub(LagHour, 6))) %>% 
    filter(LagHourNum <= 47)
  # confirm success 
  # df.e %>% select(LagHour) %>% distinct() 
  # max(df.e$LagHour)

  # 1e Compute the temperature metrics
  sum.e <- df.e %>%  group_by( DayName0) %>% 
      summarise( !!paste0("max", VarInitial) := max(Exposure), 
                 !!paste0("min", VarInitial) := min(Exposure), 
                 !!paste0("mean", VarInitial) := mean(Exposure), 
                 !!paste0("sd", VarInitial) := sd(Exposure), 
                 !!paste0("median", VarInitial) := median(Exposure))
  
  # 1f Compute the first differences  
  df.e1 <- df.e %>% arrange(DayName0) %>% 
    mutate(ExposureFD = Exposure - lag(Exposure)) %>% 
    filter(!str_detect(LagHour, "_00")) 

  # 1g Compute the RH FD metrics
  sum.e.fd <- df.e1 %>%  group_by( DayName0) %>% 
      summarise( !!paste0("maxFD", VarInitial) := max(ExposureFD), 
                 !!paste0("minFD", VarInitial) := min(ExposureFD), 
                 !!paste0("meanFD", VarInitial) := mean(ExposureFD), 
                 !!paste0("sdFD", VarInitial) := sd(ExposureFD), 
                  !!paste0("medianFD", VarInitial) := median(ExposureFD), 
                  !!paste0("maxAbsFD", VarInitial) := max(abs(ExposureFD)), 
                  !!paste0("meanAbsFD", VarInitial) := mean(abs(ExposureFD)))
  # 1h Combine data
  sum.e %>% 
    left_join(sum.e.fd, by = "DayName0")
}

####***********************************************************************
#### 2: Define Function to Compute Variability and Averages for a Year ####
####***********************************************************************
# 2a Declare function
make_var_ave_data <- function(ActiveYYYY){
  # 2b Compute variability of RH
  exp.rh <- compute_variability(ActiveYYYY, "rh", "r", "RH")
  
  # 2c Compute variability of temperature
  exp.temp <- compute_variability(ActiveYYYY, "temp", "t", "T")
  
  # 2d Compute averages
  # 2d.i readin hourly temperature data
  df <- fst::read_fst(paste0(intermediate.data.folder, "assigned_days_", CaseType,"_", ActiveYYYY,"_","temp", ".fst" ))
  # 2d.ii combine data
  df <- df %>% left_join(dplyr::select( fst::read_fst(paste0(intermediate.data.folder, "assigned_days_", CaseType,"_", ActiveYYYY,"_","rh", ".fst" )), DayName0, contains("rLag")), by = "DayName0")
  df <- df %>% left_join(exp.temp, by = "DayName0")
  df <- df %>% left_join(exp.rh, by = "DayName0")
  # 2d.iii compute temperature averages
  df$period1_24hr_avet <- (df$tLag_00+ df$tLag_01+ df$tLag_02+ df$tLag_03+ df$tLag_04+ df$tLag_05+ df$tLag_06+
                           df$tLag_07+ df$tLag_08+ df$tLag_09+ df$tLag_10+ df$tLag_11+ df$tLag_12+ df$tLag_13+ 
                           df$tLag_14+ df$tLag_15+ df$tLag_16+ df$tLag_17+ df$tLag_18+ df$tLag_19+ df$tLag_20+ 
                           df$tLag_21+ df$tLag_22+ df$tLag_23)/24
  df$period2_24hr_avet <- (df$tLag_24+ df$tLag_25+ df$tLag_26+ df$tLag_27+ df$tLag_28+ df$tLag_29+ df$tLag_30+
                           df$tLag_31+ df$tLag_32+ df$tLag_33+ df$tLag_34+ df$tLag_35+ df$tLag_36+ df$tLag_37+ 
                           df$tLag_38+ df$tLag_39+ df$tLag_40+ df$tLag_41+ df$tLag_42+ df$tLag_43+ df$tLag_44+ 
                           df$tLag_45+ df$tLag_46+ df$tLag_47)/24
  # 2d.iv readin hourly RH data
  df.r <- fst::read_fst(paste0(intermediate.data.folder, "assigned_days_", CaseType,"_", ActiveYYYY,"_","rh", ".fst" ))
  # 2d.v compute RH averages
  df$period1_24hr_aver <- (df.r$rLag_00+ df.r$rLag_01+ df.r$rLag_02+ df.r$rLag_03+ df.r$rLag_04+ df.r$rLag_05+ df.r$rLag_06+
                           df.r$rLag_07+ df.r$rLag_08+ df.r$rLag_09+ df.r$rLag_10+ df.r$rLag_11+ df.r$rLag_12+ df.r$rLag_13+ 
                           df.r$rLag_14+ df.r$rLag_15+ df.r$rLag_16+ df.r$rLag_17+ df.r$rLag_18+ df.r$rLag_19+ df.r$rLag_20+ 
                           df.r$rLag_21+ df.r$rLag_22+ df.r$rLag_23)/24
  df$period2_24hr_aver <- (df.r$rLag_24+ df.r$rLag_25+ df.r$rLag_26+ df.r$rLag_27+ df.r$rLag_28+ df.r$rLag_29+ df.r$rLag_30+
                           df.r$rLag_31+ df.r$rLag_32+ df.r$rLag_33+ df.r$rLag_34+ df.r$rLag_35+ df.r$rLag_36+ df.r$rLag_37+ 
                           df.r$rLag_38+ df.r$rLag_39+ df.r$rLag_40+ df.r$rLag_41+ df.r$rLag_42+ df.r$rLag_43+ df.r$rLag_44+ 
                           df.r$rLag_45+ df.r$rLag_46+ df.r$rLag_47)/24
 
   # 2e Save results
  df %>% fst::write_fst(paste0(final.data.folder, "variability_exposed_years_",CaseType, ActiveYYYY, ".fst"))
}

####*******************************************************s
#### 3: Compute Variability and Averages for Each Year ####
####*******************************************************
# 3a Compute variablilty and averages for each year
make_var_data(2000)
make_var_data(2001)
make_var_data(2002)
make_var_data(2003)
make_var_data(2004)
make_var_data(2005)
make_var_data(2006)
make_var_data(2007)
make_var_data(2008)
make_var_data(2009)
make_var_data(2010)
make_var_data(2011)
make_var_data(2012)
make_var_data(2013)
make_var_data(2014)
make_var_data(2015)

