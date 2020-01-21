####***********************************************************
# Temperature Variability- CVD Analysis 
# 2 Prepare cases
# Sebastian T. Rowland 
# Updated 01.17.2020
####***********************************************************
#### Table of Contents #### 
# 0: Preparation 
# 1: Readin Data 
# 2: Organize Data
# 3: Apply Exclusion Criteria
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

# 0c Set CaseType 
# either mi or stroke
CaseType <- "mi"
#
####***********************
#### 1: Readin Data    ####
####***********************
#1a Create readin function, which includes some cleaning
ReadOutcomeData <- function(yy, pp, casetype){
  df <- read_csv(paste0(raw.outcome.folder, "raw_", yy, "_", pp, "_", casetype, ".csv"), col_types = cols(.default = "c") ) %>% 
    mutate(InOut = !!pp, EventType = !!casetype, Year = str_sub(ADMDT, 0,4)) 
  if(pp == "op"){
    df <- df %>% mutate(TPADM = "op")
  }
df
  
}

#1b Setup empty dataframe to fill
df <- ReadOutcomeData("00", "op", CaseType) %>% sample_frac(0)

#1c Declare datasets we will readin
# we only use 95-99 in order to accrately identify subsequent cases
PPList <- c("ip", "op")
YYList <- c("95", "96", "97", "98", "99","00", "01", "02", "03","04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15")
if(CaseType == "ct"){
  YYList <- c("00", "01", "02", "03","04", "05", 
                "06", "07", "08", "09", "10", "11", "12", "13", "14", "15")
  PPList <- c("ip")
}

#1d Readin and combine datasets
for(i in 1:length(YYList)){
  for(j in 1:length(PPList)){
    df <- ReadOutcomeData(YYList[i], PPList[j], CaseType) %>%
      bind_rows(df)
  }
}
#
####****************************
#### 2: Process Variables   ####
####****************************
# 2a Clean date variable
cases <- df
cases <- cases %>% filter( !is.na(ADMDT) & !is.na(ADMHR)) %>%
  mutate(CaseDateRaw = paste0(ADMDT, " ", ADMHR)) %>%
  mutate(CaseDateTime = parse_date_time(CaseDateRaw, "ymd H", tz="America/New_York")) 

# 2b Compute datetime variables 
cases <- cases %>% mutate(YYYY = year(CaseDateTime), 
                          HHHH = hour(CaseDateTime), 
                          MMMM = month(CaseDateTime), 
                          WkDy = wday(CaseDateTime), 
                          YDay = yday(CaseDateTime)) %>% 
  mutate(WkDy1 = as.numeric(WkDy)) %>% 
  mutate(WkDy2 = if_else(WkDy1 == "1", 8, WkDy1)) %>%
  mutate(SecularDay = YDay + 365*(as.numeric(YYYY)-1995)) %>%
  mutate(Weekend = if_else(WkDy2 == 7 | WkDy2 == 8, "Weekend", "Week"))

# 2c Assign subsequent status 

if(CaseType == "mi"){
  cases <- cases %>%   group_by(UPID) %>% 
    arrange(SecularDay) %>% mutate(Wait = SecularDay-lag(SecularDay) * as.numeric(lag(MI_01_prim))) %>%
    ungroup()%>%
    arrange(UPID) %>% 
    mutate(SubsequentE = if_else(is.na(Wait), "First", "Subsequent")) %>% 
    mutate(SubsequentE = if_else(Wait <180 | is.na(Wait), SubsequentE,"gt60moSubsequentE")) %>% 
    mutate(SubsequentE = if_else(Wait >2 | is.na(Wait), SubsequentE,"within1 day"))
}

if(CaseType == "stroke"){
  cases <- cases %>%   group_by(UPID) %>% 
    arrange(SecularDay) %>% mutate(Wait = SecularDay-lag(SecularDay)) %>%
    ungroup()%>%
    arrange(UPID) %>% 
    mutate(SubsequentE = if_else(is.na(Wait), "First", "Subsequent")) %>% 
    mutate(SubsequentE = if_else(Wait <180 | is.na(Wait), SubsequentE,"gt60moSubsequentE")) %>% 
    mutate(SubsequentE = if_else(Wait >2 | is.na(Wait), SubsequentE,"within1 day"))
}

# 2d Compute age groups 
cases <- cases %>% mutate(AGE = as.numeric(AGE)) %>%
  mutate(ageGroup = if_else(AGE <45, "AgeLT45", "AgeGTE45LT65")) %>% 
  mutate(ageGroup = if_else(AGE >=65, "AgeGTE65", ageGroup))

# 2e Organize race variable
# 2e.i Read in the simplified race codebook
RaceCodeBook <- read_csv(paste0(raw.data.folder, "RaceCodeBook.csv"), col_types = cols(RACE  = col_character(),
                                                                                       Race1 = col_character(),
                                                                                       Race2 = col_character()))
RaceCodeBook <- RaceCodeBook %>% mutate(RACE = str_pad(RACE, 2, "left", "0"))
# 2e.ii Assign simplified Race categories + hispanic 
cases1 <- cases %>% left_join(RaceCodeBook, by = "RACE") %>%
  mutate(RaceF = if_else(ETHNIC == "1", "Hispanic", Race2)) 
# 2e.iii Keep only the simplified race category
cases2 <- cases1 %>% 
  select(-ETHNIC, -RACE, -Race1, -Race2) %>% 
  rename(RACE = RaceF) 
# 2e.iv Identify the patients who are inconsistently assigned two races 
#We assume that these subjects are multiracial, and it is not an error in coding. 
TwoRace <- cases2 %>%  
  group_by(UPID) %>%
  select(UPID, RACE) %>%
  distinct() %>% 
  summarize(NumRace = n()) %>%
  filter(NumRace !=1) %>% 
  mutate(MR = "Multi-Racial")
# 2e.v Assign multiracial category to the subjects that were assigned more than 1 race
cases3 <- cases2 %>% 
  left_join(TwoRace, by = "UPID")  %>% 
  mutate(RaceZ = if_else(is.na(MR), RACE, "Multi-Racial")) %>% 
  select(-RACE, -MR, -NumRace) %>% 
  rename(RACE = RaceZ)  %>% 
  mutate(RACE = if_else(RACE == "MultiRacial", "Multi-Racial", RACE)) %>% 
  mutate(RACE = if_else(is.na(RACE), "OtherRace", RACE))
# 2e.vi remove any remaining duplicates by race
cases <- cases3 %>% distinct()

####**********************************
#### 3: Apply Exclusion Criteria  ####
####**********************************
# Note that we first apply exclusion criteria based on study population 
# and then based on data availability/ accuracy 
# Note that we also remove observations with wait < 3 days, 
# But that criteria is applied in step A_7

# 3a remove anyone who has multiple entries for the same event 
# First we remove if two entries occured on the same day, and we choose the earlier one 
cases <- cases %>% 
  group_by(UPID, ADMDT) %>% 
  arrange(ADMHR) %>% 
  slice(0:1) %>% 
  ungroup()

# 3b Remove admissions before 2000 or after 2015
cases <- cases %>% filter(YYYY > 1999) %>%
  filter(YYYY < 2016)

# 3c Remove those outside of NY zipcodes 
zipcode <- sf::st_read(paste0(raw.data.folder, "tl_2010_36_zcta510/tl_2010_36_zcta510.shp"))
cases <- cases %>% rename(ZCTA5CE10 = ZIP) 
zip <- zipcode %>% select(ZCTA5CE10)
zip <- zip %>% as.data.frame() %>% select(ZCTA5CE10)
cases <- cases %>% right_join(zip)

# 3d Remove subjects under 18 
cases <- cases %>%  filter(as.numeric(AGE) >17)

# 3e Remove events that occur within 2 days after another event 
cases <- cases %>% filter(SubsequentE != "within1 day")

# 3e Remove those with missing date or zip 
cases <- cases %>% filter(!is.na(CaseDateTime)) %>% 
  mutate(ZCTA5CE10 = as.character(ZCTA5CE10)) %>%
  filter(str_length(ZCTA5CE10) == 5) %>% 
  filter(!str_detect(ZCTA5CE10, "[:alpha:]")) %>% 
  filter(!str_detect(ZCTA5CE10, "[:ALPHA:]"))

# 3f Save Results 
cases %>% fst::write_fst(paste0(intermediate.data.folder, "intermed1_whole_study_pop_", CaseType, ".fst")) 

# 3g Calculate insurance percentages
PubInsKey <- c("C", "D", "E", "J", "K", "H")
a <- cases %>% mutate(PublicIns = if_else(SOURCE1 %in%(PubInsKey), "Public Insurance", "Not Public Insurance")) %>% 
  group_by(PublicIns) %>% 
  summarize(Count2 = n()) %>% 
  mutate(Percent2 = Count2/nrow(cases))

# 3h Clean the environment 
rm(list = ls())
