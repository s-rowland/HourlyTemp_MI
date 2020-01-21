####***********************************************
# Temperature - CVD Analysis 
# Hourly Temperature - CVD Analysis: Choose Lag Constraints
# Sebastian Rowland 
# Updated 24.7.2019
####***********************************************
#### Table of Contents #### 
# 0: Preparation 
# 1: Prepare Data
# 2: Model Various Lag Constraints
# 3: Identify Model with Lowest AIC

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
                     
####**************************************************
#### 1: Load Analysis Function  ####

source(paste0(script.folder, "analyze_HourlyTemp_function.R"))

####**************************************************
#### 2: Model Various Lag Constraints  ####

# 2a Fit models
#analyze_HourlyTemp("mi",  48, 1,  "3dfevenknots",  "4dfevenknots", "fullpop", "fullpop", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "4dfevenknots",  "4dfevenknots", "fullpop", "fullpop", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "5dfevenknots",  "4dfevenknots", "fullpop", "fullpop", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "fullpop", "fullpop", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "7dfevenknots",  "4dfevenknots", "fullpop", "fullpop", "primary")
#analyze_HourlyTemp("mi",  48, 1,  "10dfpsp",       "4dfevenknots", "fullpop", "fullpop", "primary")


####**************************************************
#### 3: Identify Model with Lowest AIC   ####

# 3a Create  empty AIC table 
LagConstraint.v <- c(rep(NA, 5))
ModelName.v <- c(rep(NA, 5))
AIC.v <- c(rep(NA, 5))
AIC.df <- data.frame(LagConstraint.v, AIC.v, ModelName.v)
names(AIC.df) <- c("LagConstraint", "AIC", "ModelName")

# 3b Source extract AIC function
source(paste0(script.folder, "extract_aic_function.R"))

# 3c Extract AIC from each model
AIC.df <- extract_aic(1, "mi",  48, 1,  "3dfevenknots",  "4dfevenknots", "fullpop", "fullpop", "primary")
AIC.df <- extract_aic(2, "mi",  48, 1,  "4dfevenknots",  "4dfevenknots", "fullpop", "fullpop", "primary")
AIC.df <- extract_aic(3, "mi",  48, 1,  "5dfevenknots",  "4dfevenknots", "fullpop", "fullpop", "primary")
AIC.df <- extract_aic(4, "mi",  48, 1,  "6dfevenknots",  "4dfevenknots", "fullpop", "fullpop", "primary")
AIC.df <- extract_aic(5, "mi",  48, 1,  "7dfevenknots",  "4dfevenknots", "fullpop", "fullpop", "primary")

# 3d Save results 
AIC.df %>% write.csv(paste0(project.folder, "Outputs/HourlyTemp_mi/", "C_1_MainModel_LagConstraintSelection_AIC_Table.csv"))
