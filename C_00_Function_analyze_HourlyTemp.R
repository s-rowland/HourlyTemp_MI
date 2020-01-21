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

#0b Set up environment
project.folder <- "H:/Temp_CVD_Analysis/"
raw.data.folder <- paste0(project.folder,"Data/Raw_Data/")
raw.outcome.folder <- paste0(raw.data.folder, "Outcome_Data/")
raw.nldas.folder <- paste0(raw.data.folder, "NLDAS_Zip_Data/")
intermediate.data.folder <- paste0(raw.data.folder, "Intermediate_Data/")
final.data.folder <- paste0(project.folder, "Data/Final_Data/")

############################3
analyze_HourlyTemp <- function(CaseType,  NumHr,  AveWindow,  LagConstraint,  ERConstraint, SubPopVar, SubPop, prim){
  # CaseType <- c("mi", "stroke)
  # NumHr = positive nonzero integer, uaually 24, 48, or 58. 
  # Ave Winodow = positive nonzero integer, usually 1, 2, 4, or 6
  # LagConstraint generally Numdfpsp or Numdfevenknots or Numdflogknots
  # ERConstraint= Numdfevenknots 
  # SubPopVar <- Overall category by which we stratify
  # SubPop <- group membership c( )
  # prim <- c("primary", "probprimary") 
  # "probprimary" is the more expansive definition

#CaseType <- "mi";  NumHr <- 48;  AveWindow <- 1; LagConstraint <- "6dfevenknots";  
#ERConstraint <- "4dfevenknots"; SubPopVar <- "fullpop"; SubPop <- "fullpop"; prim <- "primary_60moRecurr"

  # 1 Set up Environment
  # 1a Set output folder
  output.folder <- paste0(project.folder, "Outputs/HourlyTemp_", CaseType, "/")
  
  # 1b Declare ModelName
  ModelName <- paste0("hourlytemp", "_",CaseType, "_", NumHr, "_", AveWindow, "_Lag", LagConstraint, "_ER", ERConstraint, "_",SubPopVar, "_", SubPop, "_", prim)
  
  # 1c Create CaseType Variables 
  CaseType.vector <- str_split(CaseType, "_")[[1]]
  CaseType.broad <- CaseType.vector[1]
  CaseType.subtype <- CaseType.vector[2]
  if(CaseType.broad == "mi"){CaseType.subtype <- "allmi"}
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
if(prim == "primary_60moRecurr" & CaseType.broad == "mi") {df <- df %>% filter(MI_prim ==1 ) %>% filter(SubsequentE !="Subsequent")}


if(prim == "primary" & CaseType.broad == "stroke") {df <- df %>% filter(all_stroke_prim ==1 )}

if(prim == "primary" & CaseType.subtype == "isc") {df <- df %>% filter(StrokeSubType == "isc" )}
if(prim == "primary" & CaseType.subtype == "hem") {df <- df %>% filter(StrokeSubType == "hem" )}


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

# 2e.v Create 2hr variable 
df <- df %>% mutate(DayPhase2hr = case_when(
  HH == 3 | HH == 4 ~ "HH=3_4", 
  HH == 5 | HH == 6 ~ "HH=5_6", 
  HH == 7 | HH == 8 ~ "HH=7_8", 
  HH == 9 | HH == 10 ~ "HH=9_10", 
  HH == 11 | HH == 12 ~ "HH=11_12", 
  HH == 13 | HH == 14 ~ "HH=13_14",
  HH == 1 | HH == 2 ~ "HH=1_2", 
  HH > 14 ~ "NotMorning"
  
))

# 2e.iv Create DayPhase variable 
df <- df %>% mutate(DayPhase2 = case_when(
  HH >= 22 | HH < 5  ~ "night2",
  HH >= 5 & HH <= 11  ~ "morning2",
  HH >= 12 & HH <=17   ~ "afternoon2",
  HH >= 18 &  HH <= 21   ~ "evening2"))

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
#### 4: Create Crossbasis ####

# 4a Set Lag Constraint
# 4a Set Lag Constraint
LagDF <- as.numeric(str_remove_all(LagConstraint, "[a-z]"))

# 4b Set ER Constraint
ERDF <- as.numeric(str_remove_all(ERConstraint, "[a-z]"))

# 4c Isolate exposure variables
df <- df %>% select(starts_with("tLag_"),meanr, EventID, Case, TimetoEvent)
  
# 4d Keep lags of interest  
ep1 <- df[,1:NumHr]

# 4e Translate into matrix
exposure.profiles <- as.matrix(ep1)

# 4f Create crossbasis
if(str_detect(LagConstraint, "evenknots")){
  cb.hrtemp <- crossbasis(exposure.profiles, lag=c(0,NumHr-1),
                          argvar=list(fun="ns", df = ERDF),
                          arglag=list(fun="ns", df = LagDF))
}
if(str_detect(LagConstraint, "psp")){
  cb.hrtemp <- crossbasis(exposure.profiles, lag=c(0,NumHr-1),
                          argvar=list(fun="ns", df = ERDF),
                          arglag=list(fun="ps", df = LagDF))
}
if(str_detect(LagConstraint, "logknots")){
  LK <- logknots(NumHr, fun = "ns", df = LagDF)
  cb.hrtemp <- crossbasis(exposure.profiles, lag=c(0,NumHr-1),
                          argvar=list(fun="ns", df = ERDF),
                          arglag=list(knots = LK)) 
}
if(NumHr == 58 & str_detect(LagConstraint, "setknots")){
  cb.hrtemp <- crossbasis(exposure.profiles, lag=c(0,NumHr-1),
                          argvar=list(fun="ns", df = ERDF),
                          arglag=list(fun="ns", knots = c(9.4, 18.8, 28.2, 37.6, 48)))
}
if(NumHr == 24 & str_detect(LagConstraint, "setknots")){
  cb.hrtemp <- crossbasis(exposure.profiles, lag=c(0,NumHr-1),
                          argvar=list(fun="ns", df = ERDF),
                          arglag=list(fun="ns", knots = c(9.4, 18.8)))
}

####**************************************************
#### 5: Fit Model ####

# 5a Fit the conditional logistic mdoel 
mod  <- coxph(Surv(TimetoEvent, Case) ~ cb.hrtemp +ns(meanr, df = 4) + strata(EventID),
                 method = "exact", 
                 na.action = na.omit, 
                 data = df)
  
# 5b Save Model 
mod %>% saveRDS(paste0(output.folder, ModelName, ".RDS"))

####**************************************************
#### 6: Save Predictions ####

# 6a Create predictions
pred.temp <- crosspred(cb.hrtemp, mod,cen = meanT.dataset, at=-27:37, bylag=0.2, cumul=TRUE)

# 6b Extract coefficient fit  
fit.table <- as.data.frame(pred.temp$matRRfit)  
colnames(fit.table) <- paste0("fit.", colnames(fit.table))
fit.table <- fit.table %>%   mutate(Temp = as.numeric(row.names(fit.table)))

# 6b Extract 95% CI  
lci.table <- as.data.frame(pred.temp$matRRlow)  
colnames(lci.table) <- paste0("lci.", colnames(lci.table))
  
uci.table <- as.data.frame(pred.temp$matRRhigh)  
colnames(uci.table) <- paste0("uci.", colnames(uci.table))
  
# 6c Combine fit and se 
# note that all OR are relative to the mean temperature. 
pred.table <- bind_cols(fit.table, lci.table, uci.table)
  
# 6d Save prediction table 
pred.table %>% write.csv(paste0(output.folder, "predictions_", ModelName, ".csv"))
 
# 7 : Extract the Cumulative Association
# 6b Extract coefficient fit  
fit.table <- as.data.frame(pred.temp$cumRRfit)  
colnames(fit.table) <- paste0("fit.", colnames(fit.table))
fit.table <- fit.table %>%   mutate(Temp = as.numeric(row.names(fit.table)))

# 6b Extract 95% CI  
lci.table <- as.data.frame(pred.temp$cumRRlow)  
colnames(lci.table) <- paste0("lci.", colnames(lci.table))

uci.table <- as.data.frame(pred.temp$cumRRhigh)  
colnames(uci.table) <- paste0("uci.", colnames(uci.table))

# 6c Combine fit and se 
# note that all OR are relative to the mean temperature. 
pred.table <- bind_cols(fit.table, lci.table, uci.table)

# 6d Save prediction table 
pred.table %>% write.csv(paste0(output.folder, "cumulative_predictions_", ModelName, ".csv"))
#######

 
}
