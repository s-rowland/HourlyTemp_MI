####***********************************************************
# Temperature Variability- CVD Analysis 
# 3 Create Control Hours
# Sebastian T. Rowland 
# Updated 01.17.2020
####***********************************************************
#### Table of Contents #### 
# 0: Preparation 
# 1: Readin Data
# 2: Define Function
# 3: Create Control Hours
####***********************************************************
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
####***********************************************************
####***********************
#### 1: Readin Data    ####
####***********************
# 1a Readin data 
cases <- fst::read_fst(paste0(intermediate.data.folder, "intermed1_whole_study_pop_", CaseType, ".fst"))
# 1b Recreate date variable
cases <- cases %>% mutate(CaseDateTime = parse_date_time(CaseDateRaw, "ymd H", tz="America/New_York")) 
# 1c Split data by years
years.list <- cases%>% split(cases$YYYY)

####*******************************
#### 2: Create Control Days    ####
####*******************************
# 2a Define function to create datehours
make_control_dayhr <- function(days1, BeforeAfter, WK){ 
  VarName <- paste0(BeforeAfter, "_", str_trunc(WK,1,"left",""))          #the name of the lag 
  days1 %>% mutate(!!VarName := CaseDateTime + as.period(7*WK, "day"))   #adds WKs number of weeks, preserves hour of day even in daylightsaving
}

# 2b Define function to create control hours
create_control_hours_by_year <- function(df.YYYY){
  #df.YYYY <- years.list[[1]]
  # 2c Use function to create bidirection symmetric datehours 
  days1 <-  df.YYYY
  ActiveYYYY <- days1$YYYY[1]
  days1 <- make_control_dayhr(days1,"Before", -4)
  days1 <- make_control_dayhr(days1,"Before", -3)
  days1 <- make_control_dayhr(days1,"Before", -2)
  days1 <- make_control_dayhr(days1,"Before", -1)
  days1 <- make_control_dayhr(days1,"CaseHour", 0)
  days1 <- make_control_dayhr(days1,"After", 1)
  days1 <- make_control_dayhr(days1,"After", 2)
  days1 <- make_control_dayhr(days1,"After", 3)
  days1 <- make_control_dayhr(days1,"After", 4)

  # 2d Put in long format by DayName
  days2 <- days1 %>% gather("DayName", "DayDateTime", contains("CaseHour_"),contains("Before_"), contains("After_") ) 
  
  # 2e Stratify by month of event 
  days3 <- days2 %>% filter(month(CaseDateTime) == month(DayDateTime))

  # 2f Convert times to HourIndex
  days4 <- days3 %>% 
    mutate(HourIndex0 = as.duration(interval(parse_date_time("1990/01/01 00:00:00", "ymd HMS", tz="America/New_York"), DayDateTime))) %>% 
    mutate(DayHourIndex = as.numeric(HourIndex0, "hours")) %>% 
    select(-HourIndex0) %>% 
    mutate(HourIndex0 = as.duration(interval(parse_date_time("1990/01/01 00:00:00", "ymd HMS", tz="America/New_York"), CaseDateTime))) %>% 
    mutate(CaseHourIndex = as.numeric(HourIndex0, "hours")) %>% 
    select(-HourIndex0) 

  # 2g Check timezone
  #tz(days4$DayDateTime[1])

  # 2h Save results 
  days4 %>% fst::write_fst(paste0(intermediate.data.folder, 
                           "intermed2_casecontroldays_", CaseType, "_", ActiveYYYY, ".fst"))
}
####*****************************
#### 3: Create Control Hours ####
####*****************************
# test with a single year
#create_control_hours_by_year(cases.list[[1]])

# 3a Create control hours for all active years
purrr::map(years.list, create_control_hours_by_year)

# 3b  Clean environment
rm(list = ls(pattern = "^days"))
