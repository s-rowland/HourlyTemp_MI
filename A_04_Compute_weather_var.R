####***********************************************************
# Temperature Variability- CVD Analysis 
# 4 Compute Weather Variabless
# Sebastian T. Rowland 
# Updated 01.17.2020
####***********************************************************
#### Table of Contents #### 
# 0: Preparation 
# 1: Organize
# 2: Declare Function to Compute Weather Variables
# 3: Variable Conversions
# 4: Computer weather variables
####***********************************************************
####***********************
#### 0: Preparation    #### 
####***********************
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
####***********************************************************
####*****************
#### 1: Organize #### 
####*****************
# 1a Set active years 
ActiveYearList <- c(1999, 2000, 2001, 2002, 2003, 2004, 2005, 
                    2006,2007, 2008, 2009, 2010, 2011, 2012,2013, 2014, 2015, 2016)

# 1b Declare RH calculator function
##' Convert specific humidity to relative humidity
##'
##' converting specific humidity into relative humidity
##' NCEP surface flux data does not have RH
##' from Bolton 1980 The computation of Equivalent Potential Temperature 
##' \url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
##' @title qair2rh
##' @param qair specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
##' @param temp degrees C
##' @param press pressure in mb
##' @return rh relative humidity, ratio of actual water mixing ratio to saturation mixing ratio
##' @export
##' @author David LeBauer
qair2rh <- function(qair, temp, press = 1013.25){
  es <-  6.112 * exp((17.67 * temp)/(temp + 243.5))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  return(rh)
}

####******************************************************
#### 2: Declare Function to Compute Weather Variables #### 
####******************************************************

# 2a Declare function
compute_wea <- function(ActiveYearNumber){

  # 2b Set the ActiveYYY
  ActiveYYYY <- ActiveYearNumber
  
  # 2c Readin weatherdata
  ActiveWeaVar <- "PRES"
  presdf <- fst::read_fst(paste0(raw.nldas.folder, "zip_", ActiveYYYY, "_", ActiveWeaVar, ".fst"))
  ActiveWeaVar <- "SPFH"
  spfhdf <- fst::read_fst(paste0(raw.nldas.folder, "zip_", ActiveYYYY, "_", ActiveWeaVar, ".fst"))
  ActiveWeaVar <- "TMP"
  tempdf <- fst::read_fst(paste0(raw.nldas.folder, "zip_", ActiveYYYY, "_", ActiveWeaVar, ".fst"))

  # 2d Put data in long format 
  presdf.L <- presdf %>% gather("DateHour", "pres", 2:ncol(presdf))
  spfhdf.L <- spfhdf %>% gather("DateHour", "spfh", 2:ncol(spfhdf))
  tempdf.L <- tempdf %>% gather("DateHour", "temp", 2:ncol(tempdf))
  
  # 2e Combine weather variables
  pres.spfh.L <- bind_cols(presdf.L, spfhdf.L, tempdf.L) %>% 
                 select(ZCTA5CE10, DateHour, pres, spfh, temp)

  # 2f Convert Units
  pres.spfh.L$temp_c <- pres.spfh.L$temp -273.15
  pres.spfh.L$pres_mb <- pres.spfh.L$pres *0.01
  
  # 2g Calculate relative humidity 
  wea.L <- pres.spfh.L %>% mutate(rh = qair2rh(spfh, temp_c, pres_mb))
  
  # 2h Keep only variables of interest 
  wea.L <- wea.L %>% select(ZCTA5CE10, DateHour, rh, temp_c) %>% 
           rename(temp = temp_c)

  # 2i Compute HourIndex 
  wea.L <- wea.L %>% mutate(DateHour = str_sub(DateHour, 0, 11)) %>%
           mutate(DateHourTime = parse_date_time(DateHour, "ymd H", tz = "UTC")) %>% 
           mutate(EHourIndex0 = as.duration(interval(parse_date_time("1990/01/01 00:00:00", "ymd HMS", tz="America/New_York"), DateHourTime))) %>%
           mutate(HourIndex = as.numeric(EHourIndex0, "hours")) %>% 
           dplyr::select(ZCTA5CE10, HourIndex, rh, temp)


  # 2j Write final dataset
  wea.L %>% fst::write_fst(paste0(intermediate.data.folder, "wea_L_", ActiveYYYY, ".fst"))
  
  # 2i Clean environment
  rm(list = ls(pattern = "^pres"))
  rm(list = ls(pattern = "^spfh"))
  rm(list = ls(pattern = "^temp"))
}
####*********************************
#### 4 Compute Weather Variables ####
####*********************************
# check code with 1 year
#compute_exp(2016)
 
# 4a Compute weather variables for all active years
purrr::map(ActiveYearList, compute_wea)


