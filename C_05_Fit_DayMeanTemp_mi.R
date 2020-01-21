####***********************************************
# Temperature Variability- CVD Analysis 
# MI - Hourly Temperature Analysis: Choose Lag Constraints
# Sebastian Rowland 
# Updated 23.7.2019
####***********************************************
#### Table of Contents #### 
# 0: Preparation 
# 1: Prepare Data
# 2: Hourly Temp Models With Range of DF, equidistant knots

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
library(ggplot2)

#0b Set up environment
project.folder <- "H:/Temp_CVD_Analysis/"
raw.data.folder <- paste0(project.folder,"Data/Raw_Data/")
raw.outcome.folder <- paste0(raw.data.folder, "Outcome_Data/")
raw.nldas.folder <- paste0(raw.data.folder, "NLDAS_Zip_Data/")
intermediate.data.folder <- paste0(raw.data.folder, "Intermediate_Data/")
final.data.folder <- paste0(project.folder, "Data/Final_Data/")

############################3
analyze_dayMeanT <- function(CaseType,  NumHr,  AveWindow,  LagConstraint,  ERConstraint, SubPopVar, SubPop, prim){
  # CaseType <- c("mi", "stroke)
  # NumHr = positive nonzero integer, uaually 24, 48, or 58. 
  # Ave Winodow = positive nonzero integer, usually 1, 2, 4, or 6
  # LagConstraint generally Numdfpsp or Numdfevenknots or Numdflogknots
  # ERConstraint= Numdfevenknots 
  # SubPopVar <- Overall category by which we stratify
  # SubPop <- group membership c( )
  # prim <- c("primary", "probprimary") 
  # "probprimary" is the more expansive definition

CaseType <- "mi";  NumHr <- 24;  AveWindow <- "Day"; LagConstraint <- "NA";  
ERConstraint <- "4dfevenknots"; SubPopVar <- "fullpop"; SubPop <- "pop"; prim <- "primary"

  # 1 Set up Environment
  # 1a Set output folder
  output.folder <- paste0(project.folder, "Outputs/HourlyTemp_", CaseType, "/")
  
  # 1b Declare ModelName
  ModelName <- paste0("hourlytemp", "_",CaseType, "_", NumHr, "_", AveWindow, "_Lag", LagConstraint, "_ER", ERConstraint, "_",SubPopVar, "_", SubPop, "_", prim)
  
  # 1c Create CaseType Variables 
  CaseType.vector <- str_split(CaseType, "_")[[1]]
  CaseType.broad <- CaseType.vector[1]
  CaseType.subtype <- CaseType.vector[2]
  
  ####**************************************************
  #### 2: Prepare Data ####
  # 2a Readin Data
  df <- fst::read.fst(paste0(final.data.folder, "prepared_cohort_48hr_", "hourlytemp", "_",CaseType.broad, "_identified", ".fst"))
#if(NumHr == 58){df <- fst::read.fst(paste0(final.data.folder, "prepared_cohort_58hr_", "hourlytemp", "_",CaseType, "_identified", ".fst"))}
# we should no longer need a 58 version. 
# 2b Remove missing exposure 
df <- df %>% filter(!is.na(meanr)) %>%  
  filter(!is.na(meant))

# 2c Keep only primary MI admissions accoridng to outcome definition

if(prim == "primary" & CaseType.broad == "mi") {df <- df %>% filter(MI_prim ==1 )}
if(prim == "prim_alt_410x1_A" & CaseType.broad == "mi") {df <- df %>% filter(prim_alt_410x1_A ==1 )}
if(prim == "prim_alt_410x01_A4" & CaseType.broad == "mi") {df <- df %>% filter(prim_alt_410x01_A4 ==1 )}
if(prim == "prim_alt_410xx_A" & CaseType.broad == "mi") {df <- df %>% filter(prim_alt_410xx_A ==1 )}

if(prim == "primary" & CaseType.broad == "stroke") {df <- df %>% filter(all_stroke_prim ==1 )}

# 2d Compute average temperature 
# we will use the same mean for mi and stroke, 
# the means of the datasets are very similar and will help make everything exactly comprable
meanT.dataset <- 11.3

# 2e Stratify by CaseSubType, if applicable 
if(str_detect(CaseType, "_") & CaseType.broad == "stroke"){
  df <- df %>% filter(StrokeSubType == CaseType.subtype)
}

###########
# 2e Make stratification Variables 
# 2e.i Create DateTime variables
df <- df %>% mutate(CaseDate = parse_date_time(CaseDateRaw, "ymd h")) %>% 
  mutate(MM = month(CaseDate), HH = hour(CaseDate))
# 2e.ii Create 4 month variable 
df <- df %>% mutate(Season = case_when(
  MM == 12 | MM == 1  | MM == 2  ~ "win",
  MM == 3  | MM == 4  | MM == 5  ~ "spr",
  MM == 6  | MM == 7  | MM == 8  ~ "sum",
  MM == 9  | MM == 10 | MM == 11 ~ "fal"))
# 2e.iii Create heating season variable 
df <- df %>% mutate(HeatingSeason = case_when(
  MM >= 10 | MM <=5  ~ "heating",
  MM > 5 & MM <10  ~ "cooling"))

# 2e.iv Create DayPhase variable 
df <- df %>% mutate(DayPhase = case_when(
  HH >= 23 | HH < 6  ~ "night",
  HH >= 6 & HH <= 11  ~ "morning",
  HH >= 12 & HH <=17   ~ "afternoon",
  HH >= 18 &  HH <= 22   ~ "evening"))

# 2e.iv Create DayNight variable 
df <- df %>% mutate(DayNight = case_when(
  HH > 18 | HH <= 6  ~ "night",
  HH > 6 & HH <= 18  ~ "day"))

# 2e.i Create First Variable 
df <- df %>% mutate(First = if_else(SubsequentE == "First", "First", "Recurrent"))

# 2e.vi Make meanrGP variable 
if(SubPopVar == "meanrQT"){
  meanrQT.v <- quantile(df$meanr, c(0, 0.25, 0.5, 0.75, 1))
  # 2e.v.i assign meanrQT to each 
  df$meanrQT <- as.numeric(cut(df$meanr,  meanrQT.v))
  # 2e.v.ii rename variable
  df <- df %>% mutate(meanrQT = paste0("meanrQT_", meanrQT) )
  # 2e.v.iii Assign meanrQT of cases to strata 
  df <- df %>% filter(Case == 1)%>% 
    select(EventID, meanrQT) %>% 
    rename(meanrQTcase = meanrQT) %>% 
    left_join(df, by = "EventID")
  # 2e.v.iv Match Cases and Controls by meanrQT
  # 2e.v.iv.i Isolate cases
  df <- df %>% filter(meanrQT == meanrQTcase) %>%
    mutate(Match = "matched")
  # 2e.v.iv.i Remove cases without any control days 
  # 2e.v.iv.i Identify the cases with matching control days 
  Matched.Cases <- df %>% 
    group_by(EventID) %>% 
    summarize(DayCount = n()) %>% 
    ungroup() %>% 
    filter(DayCount >1 )
  # 2e.v.iv.i Isolate to just the matched cases
  df.matched <- Matched.Cases %>% 
    select(-DayCount) %>%
    left_join(df, by = "EventID")
}


####**************************************************
#### 3: Stratify ####
# 3a Apply any stratification 

if(SubPopVar != "fullpop"){
  df <- df %>% rename(SUBPOPVAR = !!SubPopVar) %>%
    filter(SUBPOPVAR == SubPop)
}

####**************************************************
#### 4: Create DayMean Variable ####
# declare  function
L <- function(Lag, HH){
  ((Lag-HH>0) * (Lag-HH <= 24))
  }
# calculate mean temp
df <- df %>%  mutate(DayMeanT =
  (1/24)*(L(0,HH)*tLag_00 +  L(1,HH)*tLag_00 +  L(2,HH)*tLag_02 +  L(3,HH)*tLag_03 +  
  L(4,HH)*tLag_40 +  L(5,HH)*tLag_05 +  L(6,HH)*tLag_00 +  L(7,HH)*tLag_07 +
  L(8,HH)*tLag_08 +  L(9,HH)*tLag_09 +  L(10,HH)*tLag_10 +  L(11,HH)*tLag_11 +  
  L(12,HH)*tLag_12 +  L(13,HH)*tLag_13 +  L(14,HH)*tLag_14 +  L(15,HH)*tLag_15 +  
  L(16,HH)*tLag_16 +  L(17,HH)*tLag_17 +  L(18,HH)*tLag_18 +  L(19,HH)*tLag_19 +  
  L(20,HH)*tLag_20 +  L(21,HH)*tLag_21 +  L(22,HH)*tLag_22 +  L(23,HH)*tLag_23 +  
  L(24,HH)*tLag_24 +  L(25,HH)*tLag_25 +  L(26,HH)*tLag_26 +  L(27,HH)*tLag_27 +
  L(28,HH)*tLag_28 +  L(29,HH)*tLag_29 +  L(30,HH)*tLag_30 +  L(31,HH)*tLag_31 +  
  L(32,HH)*tLag_32 +  L(33,HH)*tLag_33 +  L(34,HH)*tLag_34 +  L(35,HH)*tLag_35 +  
  L(36,HH)*tLag_36 +  L(37,HH)*tLag_37 +  L(38,HH)*tLag_38 +  L(39,HH)*tLag_39 +  
  L(40,HH)*tLag_40 +  L(41,HH)*tLag_41 +  L(42,HH)*tLag_42 +  L(43,HH)*tLag_43 +
  L(44,HH)*tLag_44 +  L(45,HH)*tLag_45 +  L(46,HH)*tLag_46 +  L(47,HH)*tLag_47 ))


# proof-of-concept
#a <- df %>%  mutate(DayMeanT =
#(L(0,HH) +  L(1,HH) +  L(2,HH) + L(3,HH) +  
#L(4,HH) +  L(5,HH) +   L(6,HH) +  L(7,HH) +
#L(8,HH) +  L(9,HH) +   L(10,HH) +  L(11,HH) +  
#L(12,HH) +  L(13,HH) +  L(14,HH) +  L(15,HH) +  
#L(16,HH) +  L(17,HH) +  L(18,HH) +  L(19,HH) +  
#L(20,HH) +  L(21,HH) +  L(22,HH) +  L(23,HH) +  
#L(24,HH) +  L(25,HH) +  L(26,HH) +  L(27,HH) +
#L(28,HH) +  L(29,HH) +  L(30,HH) +  L(31,HH) +  
#L(32,HH) +  L(33,HH) +  L(34,HH) +  L(35,HH) +  
#L(36,HH) +  L(37,HH) +  L(38,HH) +  L(39,HH)+  
#L(40,HH) +  L(41,HH) +  L(42,HH) +  L(43,HH) +
#L(44,HH) +  L(45,HH) + L(46,HH) +  L(47,HH))) %>% 
#  mutate(test = L(0,HH), test2 = L(1,HH))


#b <- slice(a, 0:100) %>% select(DayMeanT)
#b


####**************************************************
#### 5: Fit Model ####

# 4b Set ER Constraint
ERDF <- as.numeric(str_remove_all(ERConstraint, "[a-z]"))


# 5a Fit the conditional logistic mdoel 
mod  <- coxph(Surv(TimetoEvent, Case) ~ ns(DayMeanT, df=4) +ns(meanr, df = ERDF) + strata(EventID),
                 method = "exact", 
                 na.action = na.omit, 
                 data = df)
  
# 5b Save Model 
mod %>% saveRDS(paste0(output.folder, ModelName, ".RDS"))
mod <- readRDS(paste0(output.folder, ModelName, ".RDS"))
####**************************************************
#### 6: Save Predictions ####
ptemp <- termplot(mod, se=TRUE, plot=FALSE)
expterm <- data.frame(ptemp$DayMeanT) # convert to dataframe 
df.ful <- df %>% rename(ExpVar := DayMeanT ) %>% select(ExpVar)
meanEXP <- mean(df.ful$ExpVar) # compute mean exposure level 
expterm$dif <- abs(expterm$x - meanEXP)#identify effect at mean exposure level 
v <- expterm %>% arrange(dif)
center <- v$y[1] 
#adjust variables so effects centered around mean exposure level (at mean X, HR =1)
expterm <- expterm %>% mutate(y2 = y-center)  ### centers at mimum risk lvl now, not mean
expterm <- expterm %>% mutate(DayMeanT = x, HR = exp(y2), LCI = exp(y2 -1.96*se), UCI = exp(y2 +1.96*se)) %>% 
  select(DayMeanT, HR, LCI, UCI)
# actually, I'm not even certain if substracting like this is correct. 
# I think its bettwe to kppe the oriignal HR for now .
expterm %>% write_csv(paste0(output.folder, "midtomid24hrave_results.csv"))

expterm <- read.csv(paste0(output.folder, "midtomid24hrave_results.csv"))
}
