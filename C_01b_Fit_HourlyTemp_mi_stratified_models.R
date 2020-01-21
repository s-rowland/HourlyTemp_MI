####***********************************************
# Temperature - CVD Analysis 
# Hourly Temperature - CVD Analysis: Choose Lag Constraints
# Sebastian Rowland 
# Updated 24.7.2019
####***********************************************
#### Table of Contents #### 
# 0: Preparation 
# 1: Prepare Data
# 2: Fit Stratified Models

####***********************************************
#### 0: Preparation #### 
library(readr)
library(dplyr)
library(lubridate)
library(magrittr)
library(tidyr)
library(survival)
library(stringr)
library(splines)
library(mgcv)
library(nlme)
library(fst)
library(dlnm)

#0b Set up environment
project.folder <- "H:/Temp_CVD_Analysis/"
raw.data.folder <- paste0(project.folder,"Data/Raw_Data/")
raw.outcome.folder <- paste0(raw.data.folder, "Outcome_Data/")
raw.nldas.folder <- paste0(raw.data.folder, "NLDAS_Zip_Data/")
intermediate.data.folder <- paste0(raw.data.folder, "Intermediate_Data/")
final.data.folder <- paste0(project.folder, "Data/Final_Data/")
script.folder <- paste0(project.folder, "Scripts/")

####****************************************************
#### 1: Load Analysis Function  ####

source(paste0(script.folder, "analyze_HourlyTemp_function.R"))

####****************************************************
#### 2: Fit Stratified Models ####

# 2a By sex
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "SEX", "F", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "SEX", "M", "primary")

# 2b By AgeGP 
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "ageGroup", "AgeLT40", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "ageGroup", "AgeGTE40LT65", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "ageGroup", "AgeGTE65", "primary")

# 2d By DayPhase 
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "DayPhase", "night", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "DayPhase", "morning", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "DayPhase", "afternoon", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "DayPhase", "evening", "primary")

# 2c By Heating/cooling season 
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "HeatingSeason", "heating", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "HeatingSeason", "cooling", "primary")

# 2d By Mean 48-hr relative humidity 
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "meanrQT", "meanrQT_1", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "meanrQT", "meanrQT_2", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "meanrQT", "meanrQT_3", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "meanrQT", "meanrQT_4", "primary")
