####***********************************************************
# Temperature Variability- CVD Analysis 
# 6 Compute Temperature Variability, Mean Temperature, and Combine Years
# Note: this step is broken up by year because it is computationally intensive
# Sebastian Rowland 
# Updated 19.7.2019
####***********************************************************
#### Table of Contents #### 
# 0: Preparation 
# 1: Readin Data
# 2: Data Manipulation
####***********************************************************
####********************
#### 0: Preparation #### 
####********************
# 0a load packages
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(lubridate)
library(magrittr)
library(tidyr)
library(ggplot2)
library(sf)
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

# 0d Set ExposureType 
ExposureType <- "hourlytemp"

####********************
#### 1: Readin Data ####
####********************
# 1a Declare active year list
YYYY.list <-  c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                "2009", "2010","2011", "2012", "2013", "2014", "2015")

# 1b Readin data 
# 1b.i start with empty df
df <- fst::read_fst(paste0(final.data.folder, "variability_exposed_years_", CaseType,  2000,".fst"))
# 1b.ii fill with data
for(i in 2:length(YYYY.list)){
  df <-df %>% bind_rows(fst::read_fst(paste0(final.data.folder, "variability_exposed_years_",CaseType, YYYY.list[i], ".fst")))
}
####**************************
#### 2: Data Manipulation ####
####**************************
# 2a Remove cases that are missing meanr or temp 
df <- df %>% filter(!is.na(meanr))
  
# 2b Create New AgeGroup Variable
df <- df %>% mutate(AGE = as.numeric(AGE)) %>%
  mutate(ageGroup = if_else(AGE < 40, "AgeLT40", "AgeGTE40LT65")) %>% 
  mutate(ageGroup = if_else(AGE >=65, "AgeGTE65", ageGroup))

# 2c Create Case and TimetoEvent Variables for coxph command 
df <- df %>% mutate(Case = if_else(str_detect(DayName0, "Case"), 1, 0),
                    TimetoEvent = if_else(str_detect(DayName0, "Case"), 1, 2))

# 2d Create deidentified stratum identifier
df <- df %>% 
  mutate(CaseHourIndex2 = as.numeric(as.factor(CaseHourIndex)))%>% 
  mutate(CaseHourIndex2 = str_pad(CaseHourIndex2, 7,"left", "T"))%>% 
  mutate(ID = as.numeric(as.factor(UPID)))%>% 
  mutate(ID = str_pad(ID, 6,"left", "A"))%>% 
  mutate(EventID = paste0(ID, "_", CaseHourIndex2))

#df <- df %>% mutate(Sub = if_else(hem_stroke_prim == "1" & isc_stroke_prim != "1", 1, 0 )) %>% 
#  mutate(SubUNK = if_else(hem_stroke_prim =="1" & isc_stroke_prim == "1", 1, 0), 
#         subHEM = if_else(isc_stroke_prim == "1" & hem_stroke_prim != "1", 1, 0))
#sum(df$SubUNK)
#sum(df$Sub)
#sum(df$subHEM)
#a <- df %>% select(hem_stroke_prim, isc_stroke_prim, isc_strokeA, hem_strokeA)
#head(a)
# 2e Create Stroke Subtype Variable 
if(CaseType == "stroke"){
df <- df %>% mutate(
  StrokeSubType  = case_when(
    hem_stroke_prim == "1" & isc_stroke_prim != "1" ~ "hem",
    isc_stroke_prim == "1" & hem_stroke_prim != "1" ~ "isc", 
    isc_strokeA == "1" ~ "isc", 
    hem_strokeA == "1" ~ "hem",
    PR01 == "9910" | PR02 == "9910" | PR03 =="9910" | PR04 =="9910" | PR05 == "9910" ~ "isc",
    TRUE  ~ "unkn"
  ))
  
}
# 2f Create MI primary variable 

# (a) 410.x1 in the primary diagnosis
# (b) 410.x1 or 410.x0 in the four diagnostic positions,
# (c) 410.xx in the primary diagnosis
# (d) 410.x1 in the primary diagnosis; excluded if reccurent by 6 mo or less

if( CaseType == "mi") {
  df <- df %>% 
    mutate(DXA_410x1 = str_sub(DXA, 1, 3) == "410" & str_sub(DXA,5,5) == "1" ) %>% 
    mutate(DXA_410x1 = if_else(is.na(DXA_410x1), FALSE, DXA_410x1) ) %>% 
    mutate(DX01_410x1 = str_sub(DX01, 1, 3) == "410" & str_sub(DX01,5,5) == "1" ) %>% 
    mutate(DX01_410x1 = if_else(is.na(DX01_410x1), FALSE, DX01_410x1) ) %>% 
    mutate(DX02_410x1 = str_sub(DX02, 1, 3) == "410" & str_sub(DX02,5,5) == "1" ) %>% 
    mutate(DX02_410x1 = if_else(is.na(DX02_410x1), FALSE, DX02_410x1) ) %>% 
    mutate(DX03_410x1 = str_sub(DX03, 1, 3) == "410" & str_sub(DX03,5,5) == "1" ) %>% 
    mutate(DX03_410x1 = if_else(is.na(DX03_410x1), FALSE, DX03_410x1) ) %>% 
    mutate(MI_prim_tf = (as.numeric(DXA_410x1) + as.numeric(DX01_410x1) + as.numeric(DX02_410x1) + as.numeric(DX03_410x1)) ) %>% 
    mutate(MI_prim = as.numeric(MI_prim_tf>0)) %>% 
    select(-MI_prim_tf) 
  # alternative case definitions 
  df <- df %>% 
    mutate(prim_alt_410x1_A = if_else(DXA_410x1 == 1,1,0)) %>% 
    mutate(DXA_410x0 = str_sub(DXA, 1, 3) == "410" & str_sub(DXA,5,5) == "1" ) %>% 
    mutate(DXA_410x0 = if_else(is.na(DXA_410x0), FALSE, DXA_410x0) ) %>% 
    mutate(DX01_410x0 = str_sub(DX01, 1, 3) == "410" & str_sub(DX01,5,5) == "1" ) %>% 
    mutate(DX01_410x0 = if_else(is.na(DX01_410x0), FALSE, DX01_410x0) ) %>% 
    mutate(DX02_410x0 = str_sub(DX02, 1, 3) == "410" & str_sub(DX02,5,5) == "1" ) %>% 
    mutate(DX02_410x0 = if_else(is.na(DX02_410x0), FALSE, DX02_410x0) ) %>% 
    mutate(DX03_410x0 = str_sub(DX03, 1, 3) == "410" & str_sub(DX03,5,5) == "1" ) %>% 
    mutate(DX03_410x0 = if_else(is.na(DX03_410x0), FALSE, DX03_410x0) ) %>% 
    mutate(MI_alt_tf = (as.numeric(DXA_410x1) + as.numeric(DX01_410x1) + as.numeric(DX02_410x1) + as.numeric(DX03_410x1) + 
             as.numeric(DXA_410x1) + as.numeric(DX01_410x1) + as.numeric(DX02_410x1) + as.numeric(DX03_410x1) ) )%>%
    mutate(prim_alt_410x01_A4 = as.numeric(MI_alt_tf>0)) %>% 
    select(-MI_alt_tf)  %>% 
    mutate(prim_alt_410xx_A = as.numeric(str_sub(DXA, 1, 3) == "410"))
  
}
ls(df)
# 2g Filter by admission Type
if( CaseType == "mi") {df <- df %>% filter(TPADM !="4" & TPADM != "5")}

# 2h Save identified dataset
df %>% fst::write_fst(paste0(final.data.folder, "prepared_cohort_48hr_", ExposureType, "_", CaseType, "_identified", ".fst"))

# 2i Deidentify cases: keep only relevant variables 
df.deid <- df %>% select(contains("prim"), contains("prob"),contains("_A_"),EventID, # Case features / strata identifiers
TimetoEvent, Case,                        # coxph variables
contains("tLag"), contains("rLag"),       # hourly weather
sdt, sdFDt,  sdr, sdFDr,               # variability metrics
meant, meanr, period1_24hr_avet, period2_24hr_avet,  period1_24hr_aver, period2_24hr_aver) # averages

# 2j Save deidentified dataset
df.deid %>% fst::write_fst(paste0(final.data.folder, "prepared_cohort_48hr_", ExposureType, "_",CaseType, "_deidentified", ".fst"))

df %>%  select(StrokeSubType) %>% group_by(StrokeSubType) %>% summarize(Count = n())

