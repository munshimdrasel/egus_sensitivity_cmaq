
library(fst)
library(data.table)
library(tidyverse)
library(ggplot2)
library(stringi)
library(lubridate)
library(dslabs)
library(openair)
library(gridExtra)
library(ggmap)
library(Rmisc)
library(usmap)
library(ggpubr)
library(janitor)

setwd ("/Users/munshirasel/Library/CloudStorage/GoogleDrive-munshimdrasel@gwmail.gwu.edu/My Drive/R/egus_sensitivity_cmaq")

# list files
daily.aqs <- list.files( "./data/aqs_daily",  pattern = 'AQS_Daily_*', full.names = TRUE)
# hourly.o3 <- list.files( "./data/aqs_o3",  pattern = 'O3_Hourly_*', full.names = TRUE)
epa.region <- read.fst("./data/epa_region.fst")

datalist = list()

for (i in 1:length(daily.aqs)) {
  aqs <- read.csv(daily.aqs[i], skip=c(5))
  aqs$STATE <- state.abb[match(aqs$State, state.name)]
  aqs <- merge(aqs, epa.region, by= "STATE")
  # aqs <- aqs [-c(1:5),]
  # aqs <- aqs %>% row_to_names((row_number = 1))
  names(aqs)[names(aqs) == 'Time.On'] <- 'Time_On'
  names(aqs)[names(aqs) == 'Time.Off'] <- 'Time_Off'
  aqs$Time_On <- as.Date(aqs$Time_On, "%m/%d/%y")
  aqs$Time_Off <- as.Date(aqs$Time_Off, "%m/%d/%y")
  # say spin up period 4 days
  # aqs <- aqs %>% filter(Time_On >="2020-11-01")  ##YYY-dd-mmm
  aqs.var.tot <- aqs %>% dplyr::select(Time_On, EPA.Region, PM25_TOT_ob,PM25_TOT_mod, PM25_SO4_ob, PM25_SO4_mod,
                                       PM25_NO3_ob, PM25_NO3_mod, PM25_NH4_ob, PM25_NH4_mod, 
                                       PM25_OC_ob, PM25_OC_mod, PM25_EC_ob, PM25_EC_mod)
  aqs.var.tot$PM25_TOT_ob <- as.numeric(aqs.var.tot$PM25_TOT_ob)
  aqs.var.tot$PM25_TOT_mod <- as.numeric(aqs.var.tot$PM25_TOT_mod)
  aqs.var.tot$PM25_SO4_ob <- as.numeric(aqs.var.tot$PM25_SO4_ob)
  aqs.var.tot$PM25_SO4_mod <- as.numeric(aqs.var.tot$PM25_SO4_mod)
  aqs.var.tot$PM25_NO3_ob <- as.numeric(aqs.var.tot$PM25_NO3_ob)
  aqs.var.tot$PM25_NO3_mod <- as.numeric(aqs.var.tot$PM25_NO3_mod)
  aqs.var.tot$PM25_NH4_ob <- as.numeric(aqs.var.tot$PM25_NH4_ob)
  aqs.var.tot$PM25_NH4_mod <- as.numeric(aqs.var.tot$PM25_NH4_mod)
  aqs.var.tot$PM25_OC_ob <- as.numeric(aqs.var.tot$PM25_OC_ob)
  aqs.var.tot$PM25_OC_mod <- as.numeric(aqs.var.tot$PM25_OC_mod)
  aqs.var.tot$PM25_EC_ob <- as.numeric(aqs.var.tot$PM25_EC_ob)
  aqs.var.tot$PM25_EC_mod <- as.numeric(aqs.var.tot$PM25_EC_mod)
  
  # summary(aqs.var.tot)
  # aqs.var.tot <- na.omit(aqs.var.tot)
  #replacing -999 values as NA
  
  aqs.var.tot$PM25_TOT_ob <- ifelse(aqs.var.tot$PM25_TOT_ob/-999==1, NA, aqs.var.tot$PM25_TOT_ob)
  aqs.var.tot$PM25_SO4_ob <- ifelse(aqs.var.tot$PM25_SO4_ob/-999==1, NA, aqs.var.tot$PM25_SO4_ob)
  aqs.var.tot$PM25_NH4_ob <- ifelse(aqs.var.tot$PM25_NH4_ob/-999==1, NA, aqs.var.tot$PM25_NH4_ob)
  aqs.var.tot$PM25_NO3_ob <- ifelse(aqs.var.tot$PM25_NO3_ob/-999==1, NA, aqs.var.tot$PM25_NO3_ob)
  aqs.var.tot$PM25_OC_ob <- ifelse(aqs.var.tot$PM25_OC_ob/-999==1, NA, aqs.var.tot$PM25_OC_ob)
  aqs.var.tot$PM25_EC_ob <- ifelse(aqs.var.tot$PM25_EC_ob/-999==1, NA, aqs.var.tot$PM25_EC_ob)
  
  # aqs.var.tot <- aqs.var.tot %>% filter(PM25_TOT_ob >0 & PM25_SO4_ob >0 & PM25_NO3_ob >0 & PM25_NH4_ob >0 & 
  #                                         PM25_OC_ob >0 & PM25_EC_ob >0)
  # aqs.var.tot <- aqs.var.tot %>% filter(PM25_TOT_mod >0 & PM25_SO4_mod >0 & PM25_NO3_mod >0 & PM25_NH4_mod >0 &
  #                                        PM25_OC_mod >0 & PM25_EC_mod >0)
  var_daily <- setDT(aqs.var.tot )[, .(PM25_TOT_ob = mean(PM25_TOT_ob, na.rm=TRUE),
                                       PM25_TOT_mod = mean(PM25_TOT_mod, na.rm=TRUE),
                                       PM25_SO4_ob = mean(PM25_SO4_ob, na.rm=TRUE),
                                       PM25_SO4_mod = mean(PM25_SO4_mod, na.rm=TRUE),
                                       PM25_NO3_ob = mean(PM25_NO3_ob, na.rm=TRUE),
                                       PM25_NO3_mod = mean(PM25_NO3_mod, na.rm=TRUE),
                                       PM25_NH4_ob = mean(PM25_NH4_ob, na.rm=TRUE),
                                       PM25_NH4_mod = mean(PM25_NH4_mod, na.rm=TRUE),
                                       PM25_OC_ob = mean(PM25_OC_ob, na.rm=TRUE),
                                       PM25_OC_mod = mean(PM25_OC_mod, na.rm=TRUE),
                                       PM25_EC_ob = mean(PM25_EC_ob, na.rm=TRUE),
                                       PM25_EC_mod = mean(PM25_EC_mod, na.rm=TRUE)),
                                   by = .(Time_On, EPA.Region)]
  
  
  # o3.var_daily <- o3.var_daily %>%filter(Time_On %in% selected.date)
  
  # combined.var <- merge(var_daily, o3.var_daily, by= c("Time_On", "EPA.Region"))
  
  #selecting only PM2.5 and O3 8hr max
  combined.var <- var_daily %>% dplyr::select(Time_On, EPA.Region, PM25_TOT_ob,PM25_TOT_mod, PM25_SO4_ob, PM25_SO4_mod,
                                              PM25_NO3_ob, PM25_NO3_mod, PM25_NH4_ob, PM25_NH4_mod, 
                                              PM25_OC_ob, PM25_OC_mod, PM25_EC_ob, PM25_EC_mod)
  
  combined.var.pct <- combined.var %>% mutate(so4.pct.mod= ((PM25_SO4_mod*100)/PM25_TOT_mod),
                                          no3.pct.mod= ((PM25_NO3_mod*100)/PM25_TOT_mod),
                                          nh4.pct.mod= ((PM25_NH4_mod*100)/PM25_TOT_mod),
                                          oc.pct.mod= ((PM25_OC_mod*100)/PM25_TOT_mod),
                                          ec.pct.mod= ((PM25_EC_mod*100)/PM25_TOT_mod),
                                          so4.pct.ob= ((PM25_SO4_ob*100)/PM25_TOT_ob),
                                          no3.pct.ob= ((PM25_NO3_ob*100)/PM25_TOT_ob),
                                          nh4.pct.ob= ((PM25_NH4_ob*100)/PM25_TOT_ob),
                                          oc.pct.ob= ((PM25_OC_ob*100)/PM25_TOT_ob),
                                          ec.pct.ob= ((PM25_EC_ob*100)/PM25_TOT_ob))
    
  
  # sumry <- lapply(combined.var, summary)
  
  # species.melt <- melt(combined.var, 
  #                      measure.vars = c("PM25_TOT_mod", "PM25_SO4_mod", "PM25_NO3_mod", "PM25_NH4_mod", "PM25_OC_mod",
  #                                       "PM25_EC_mod"),
  #                      variable.name = "pollutants", value.name = "value")
  
  
  datalist[[i]] <-   combined.var.pct
  
}

spc.data = do.call(rbind, datalist)

spc.data <- na.omit(spc.data)

lapply(spc.data, summary)


