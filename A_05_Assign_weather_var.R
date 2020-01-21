####***********************************************************
# Temperature Variability- CVD Analysis 
# 5 Make Lag Hours and Assign Weather Variables
# Sebastian T. Rowland 
# Updated 15.8.2019
####***********************************************************
#### Table of Contents #### 
# N: Notes
# 0: Preparation 
# 1: Organize
# 2: Process Outcome Timing Data
# 3: Process Weather Data
# 4: Split Data
# 5: Assign Weather 
# 6: Run Join Function Over Years and Variables
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

####***********************************************************
####*****************
#### 1: Organize ####
####*****************
# 1a Define pre-hospitalization delay 
if(CaseType == "mi"){delay <- 3}
if(CaseType == "stroke"){delay <- 2}

# 1b Define function to create lags 
make_lag <- function(days, HR, weather.variable){ 
  VarName <- paste0(substr(weather.variable, 0,1), "Lag", "_", str_pad(HR,2, pad = "0"))
  days %>% mutate(!!VarName := DayHourIndex - delay- HR)
}

# 1c Define make lags function
make_lags_assign_weather <- function(ActiveYYYY, weather.variable){
  #ActiveYYYY <- "2000"; weather.variable <- "rh"
  
  ####************************************
  #### 2: Process Outcome Timing Data ####
  ####************************************
  
  # 2a Readin casecontrol data 
  days <- fst::read_fst(paste0(intermediate.data.folder, 
                              "intermed2_casecontroldays_", CaseType, "_", ActiveYYYY, ".fst"))
  # 2b Keep only variables of interes
  days <- days %>% mutate(DayHourIndex = as.numeric(DayHourIndex))%>% 
                   dplyr::select(contains("_prob"), contains("_prim"),contains("A"), 
                   contains("DX"), contains("pr"), TPADM, UPID, ZCTA5CE10, SEX, 
                   AGE, ageGroup, RACE, SubsequentE, Wait, InOut, CaseDateRaw, 
                   CaseHourIndex, DayName, DayHourIndex)
  
  # 2c Replicate dataset
  days0 <- days
  
  # 2d Create lags for 58 hours
  for(i in 0:57){ 
    days <- make_lag(days, i, weather.variable)
  }

  # 2e Put data in long format via lags 
  days2 <- days %>%
           dplyr::select(-DayHourIndex) %>% 
           gather("LagName", "LagHourIndex", contains("Lag_")) 

  # 2f Make list of active hourindexes for that year 
  ActiveHI <- days2 %>% select(LagHourIndex) %>% 
              distinct() %>% arrange(LagHourIndex)
  ActiveHI.list <- as.list(ActiveHI$LagHourIndex)
  Days <- days2 
  
  # 2g Clean environment
  rm(days2)
  
  ####*****************************
  #### 3: Process Weather Data ####
  ####*****************************
  # 3a Readin current year's weather data 
  wea1 <-  fst::read_fst(paste0(intermediate.data.folder, "wea_L_", ActiveYYYY, ".fst")) %>% 
           as.data.frame() %>%
           rename(wea := weather.variable) %>% 
           filter(HourIndex %in% ActiveHI.list)%>% 
           dplyr::select(ZCTA5CE10, HourIndex, wea)

  # 2b Readin previous year's data 
  # 2b.i Identify previous year
  PrevYYYY <- as.character( as.numeric(ActiveYYYY) - 1 )
  # 2b.ii readin previous years weather
  wea0 <-  fst::read_fst(paste0(intermediate.data.folder, "wea_L_",PrevYYYY, ".fst")) %>% 
           as.data.frame() %>%
           rename(wea := weather.variable) %>% 
           filter(HourIndex %in% ActiveHI.list) %>% 
           dplyr::select(ZCTA5CE10, HourIndex, wea)
  
  # 2c readin subsequent year's data 
  #due to the difference in timezones, NLDAS is in UTC, which ends before EST
  # 2c.i Identify previous year
  NextYYYY <- as.character( as.numeric(ActiveYYYY) + 1 )
  # 2c.ii readin previous years exposure
  wea2 <-  fst::read_fst(paste0(intermediate.data.folder, "wea_L_",NextYYYY, ".fst")) %>% 
    as.data.frame() %>%
    rename(wea := weather.variable) %>% 
    filter(HourIndex %in% ActiveHI.list) %>% 
    dplyr::select(ZCTA5CE10, HourIndex, wea)


  # 2d keep only the active climate variable 
  wea.L <- bind_rows(wea1, wea0, wea2)

  ####**********************
  #### 3: Split Data    ####
  ####**********************
  # 3a clear the environment
  rm(wea0, wea1, wea2)

  # 3b prepare days data 
  d <- Days %>% mutate(UPID_event_lag = paste0(UPID,"_", CaseHourIndex, ".", DayName, ".", LagName)) %>% 
    dplyr::select(ZCTA5CE10, UPID_event_lag, LagHourIndex) %>% arrange(LagHourIndex)

  # 3c prepare weather data
  w <- wea.L %>% mutate(ZCTA5CE10 = as.character(ZCTA5CE10)) %>% arrange(HourIndex) 

  # 3d split data into list 
  Dlist <- split(d, d$LagHourIndex)
  Wlist <- split(w, w$HourIndex)

  # 3e double check 
  if (length(Dlist) != length(Wlist))  stop("check w and d size")

  #### 4: Assign Weather ####
  ####***********************
  # 4a Define our join function 
  join_by_zip <- function(day.df, exp.df) { 
    day.df %>% dplyr::select(-LagHourIndex) %>%
               left_join(exp.df, by = "ZCTA5CE10") %>%
               dplyr::select(-HourIndex) 
  }

  # 4b test
  #day1 <- Dlist[[1]]
  #wea1 <- Wlist[[1]]
  #dv <- join_by_zip(day1, wea1)

  #Dlist2 <- list(Dlist[[1]], Dlist[[2]])
  #Wlist2 <- list(Wlist[[1]], Wlist[[2]])
  #DGK <-purrr:: map2(Dlist2, Wlist2, join_by_zip)
  # end test 

  # 4c join by zip code for the active Year
  DGK <-purrr:: map2(Dlist, Wlist, join_by_zip)

  # 4d combine separate lists into a dataframe
  df <- do.call("rbind", DGK)

  # 4e clean dataframe
  df1 <- df %>% separate(UPID_event_lag, sep = "\\.", c("UPID_event", "DayName", "LagName")) %>%
    mutate(DayName0 = paste0(UPID_event, "_", DayName)) %>% select(-UPID_event)
  DGK.wide <- tidyr::spread(df1, LagName, wea)

  # 4f Combine weather with demographic data 
  days0 <- days0 %>% mutate(DayName0 = paste0(UPID,"_", CaseHourIndex, "_", DayName))
  days0 %>% inner_join(DGK.wide, by = "DayName0") %>% fst::write_fst(paste0(intermediate.data.folder, "assigned_days_", CaseType,"_", ActiveYYYY,"_",weather.variable, ".fst" ))
}


####******************************************************
#### 5: Run Join Function Over Years and Variables    ####
####******************************************************

#make_lags_assign_weather("1995", "rh")
#make_lags_assign_weather("1996", "rh")
#make_lags_assign_weather("1997", "rh")
#make_lags_assign_weather("1998", "rh")
#make_lags_assign_weather("1999", "rh")
make_lags_assign_weather("2000", "rh")
make_lags_assign_weather("2001", "rh")
make_lags_assign_weather("2002", "rh")
make_lags_assign_weather("2003", "rh")
make_lags_assign_weather("2004", "rh")
make_lags_assign_weather("2005", "rh")
make_lags_assign_weather("2006", "rh")
make_lags_assign_weather("2007", "rh")
make_lags_assign_weather("2008", "rh")
make_lags_assign_weather("2009", "rh")
make_lags_assign_weather("2010", "rh")
make_lags_assign_weather("2011", "rh")
make_lags_assign_weather("2012", "rh")
make_lags_assign_weather("2013", "rh")
make_lags_assign_weather("2014", "rh")
make_lags_assign_weather("2015", "rh")
#make_lags_assign_weather("1995", "temp")
#make_lags_assign_weather("1996", "temp")
#make_lags_assign_weather("1997", "temp")
#make_lags_assign_weather("1998", "temp")
#make_lags_assign_weather("1999", "temp")
make_lags_assign_weather("2000", "temp")
make_lags_assign_weather("2001", "temp")
make_lags_assign_weather("2002", "temp")
make_lags_assign_weather("2003", "temp")
make_lags_assign_weather("2004", "temp")
make_lags_assign_weather("2005", "temp")
make_lags_assign_weather("2006", "temp")
make_lags_assign_weather("2007", "temp")
make_lags_assign_weather("2008", "temp")
make_lags_assign_weather("2009", "temp")
make_lags_assign_weather("2010", "temp")
make_lags_assign_weather("2011", "temp")
make_lags_assign_weather("2012", "temp")
make_lags_assign_weather("2013", "temp")
make_lags_assign_weather("2014", "temp")
make_lags_assign_weather("2015", "temp")




