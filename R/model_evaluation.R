
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
hourly.o3 <- list.files( "./data/aqs_o3",  pattern = 'O3_Hourly_*', full.names = TRUE)
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
  
  # selected.date <- as.vector(var_daily$Time_On)
  #o3 data
  o3.aqs <- read.csv(hourly.o3[i], skip=5)
  o3.aqs$STATE <- state.abb[match(o3.aqs$State, state.name)]
  o3.aqs <- merge(o3.aqs, epa.region, by= "STATE")
  # o3.aqs <- o3.aqs [-c(1:2),]
  # o3.aqs <- o3.aqs %>% row_to_names((row_number = 1))
  names(o3.aqs)[names(o3.aqs) == 'Time.On'] <- 'Time_On'
  names(o3.aqs)[names(o3.aqs) == 'Time.Off'] <- 'Time_Off'
  o3.aqs$Time_On <- as.Date(o3.aqs$Time_On, "%m/%d/%y")
  o3.aqs$Time_Off <- as.Date(o3.aqs$Time_Off, "%m/%d/%y")
  # say spin up period 4 days
  # aqs <- aqs %>% filter(Time_On >="2020-11-01")  ##YYY-dd-mmm
  o3.aqs.var.tot <- o3.aqs %>% dplyr::select(Time_On, EPA.Region, O3_8hrmax_ob, O3_8hrmax_mod)
  o3.aqs.var.tot$O3_8hrmax_ob <- as.numeric(o3.aqs.var.tot$O3_8hrmax_ob)
  o3.aqs.var.tot$O3_8hrmax_mod<- as.numeric(o3.aqs.var.tot$O3_8hrmax_mod)
  
  # summary(o3.aqs.var.tot)
  # o3.aqs.var.tot <- na.omit(o3.aqs.var.tot)
  #removing -999 values
  o3.aqs.var.tot$O3_8hrmax_ob <- ifelse(o3.aqs.var.tot$O3_8hrmax_ob/-999==1, NA, o3.aqs.var.tot$O3_8hrmax_ob)
  
  # o3.aqs.var.tot <- o3.aqs.var.tot %>% filter(O3_8hrmax_ob >0)
  # o3.aqs.var.tot <- o3.aqs.var.tot %>% filter(O3_8hrmax_mod >0 )
  o3.var_daily <- setDT(o3.aqs.var.tot )[, .(O3_8hrmax_ob = mean(O3_8hrmax_ob, na.rm=TRUE),
                                             O3_8hrmax_mod = mean(O3_8hrmax_mod, na.rm=TRUE)),
                                         by = .(Time_On, EPA.Region)]
  
  # o3.var_daily <- o3.var_daily %>%filter(Time_On %in% selected.date)
  
  combined.var <- merge(var_daily, o3.var_daily, by= c("Time_On", "EPA.Region"))
  
  #selecting only PM2.5 and O3 8hr max
  combined.var <- combined.var %>% dplyr::select(Time_On, EPA.Region, PM25_TOT_ob, PM25_TOT_mod, O3_8hrmax_ob, O3_8hrmax_mod)
  
  species.melt <- melt(combined.var, 
                       measure.vars = c("PM25_TOT_ob", "PM25_TOT_mod", "O3_8hrmax_ob", "O3_8hrmax_mod"),
                       variable.name = "pollutants", value.name = "value")
  
  
  datalist[[i]] <-species.melt
  
}

spc.data = do.call(rbind, datalist)

# varibles.vec <- as.vector(unique(spc.data$pollutants))

regions.vec <- as.vector(unique(spc.data$EPA.Region))

datalist = list()

for (i in 1: length(regions.vec)) {
  pm.spc.data <- spc.data %>% filter(EPA.Region==i) %>% filter(pollutants %in% c("PM25_TOT_ob", "PM25_TOT_mod"))
  
  # na.omit(pm.spc.data) %>% 
  #   ggplot(aes(x=Time_On)) + geom_point(aes(y=value, color=pollutants), size=3) + 
  #   geom_line(aes(y=value, color=pollutants))+
  #   labs(x="2020", title=expression(paste(PM[2.5], " ", mu, "g/", m^3)))+
  #   theme (axis.title.x = element_blank(),
  #          axis.title.y = element_blank(),
  #          axis.text = element_text(size=15),
  #          title=element_text(size=20),
  #          legend.title = element_blank(),
  #          legend.position = c(0.3,0.8),
  #          legend.text = element_text(size=20)) +
  #   scale_y_continuous(expand = c(0, 0), limits = c(0, 20)) +
  #   scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
  # 
  # ggsave(paste0("pm25_eval_epa_region_", regions.vec[i],".png"), path = "./plots/", width=8, height=6, units="in")
  
  
  pm.spc.data <- reshape(pm.spc.data, idvar = "Time_On", timevar = "pollutants", direction = "wide")
  
  names(pm.spc.data)[names(pm.spc.data) == "value.PM25_TOT_ob"] <- 'PM25_TOT_ob'
  names(pm.spc.data)[names(pm.spc.data) == "value.PM25_TOT_mod"] <- 'PM25_TOT_mod'
  names(pm.spc.data)[names(pm.spc.data) == "Time_On"] <- 'date'
  
  mod.eval.pm25 <- as.data.table(modStats(pm.spc.data , mod = "PM25_TOT_mod", obs = "PM25_TOT_ob", type="season"))
  mod.eval.pm25$variable <- "pm25"
  mod.eval.pm25$epa_region <- regions.vec[i]
  
  
  # ======MDA8hr o3
  pm.spc.data <- spc.data %>% filter(EPA.Region==i)  %>% filter(pollutants %in% c("O3_8hrmax_ob", "O3_8hrmax_mod"))  
  
  # na.omit(pm.spc.data) %>% 
  #   ggplot(aes(x=Time_On)) + geom_point(aes(y=value, color=pollutants), size=3) + 
  #   geom_line(aes(y=value, color=pollutants))+
  #   labs(x="2020", title=expression(paste(O[3], " ppb")))+
  #   theme (axis.title.x = element_blank(),
  #          axis.title.y = element_blank(),
  #          axis.text = element_text(size=15),
  #          title=element_text(size=20),
  #          legend.title = element_blank(),
  #          legend.position = c(0.3,0.3),
  #          legend.text = element_text(size=20)) +
  #   scale_y_continuous(expand = c(0, 0), limits = c(0, 60)) +
  #   scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
  # 
  # ggsave(paste0("o3_8hr_eval_epa_region_", regions.vec[i],".png"), path = "./plots/", width=8, height=6, units="in")
  
  pm.spc.data <- reshape(pm.spc.data, idvar = "Time_On", timevar = "pollutants", direction = "wide")
  
  names(pm.spc.data)[names(pm.spc.data) == "value.O3_8hrmax_ob"] <- 'O3_8hrmax_ob'
  names(pm.spc.data)[names(pm.spc.data) == "value.O3_8hrmax_mod"] <- 'O3_8hrmax_mod'
  names(pm.spc.data)[names(pm.spc.data) == "Time_On"] <- 'date'
  
  mod.eval.pm25.o3 <- as.data.table(modStats(pm.spc.data , mod = "O3_8hrmax_mod", obs = "O3_8hrmax_ob", type="season"))
  mod.eval.pm25.o3$variable <- "o3"
  mod.eval.pm25.o3$epa_region <- regions.vec[i]
  
  # # ======Sulfate
  # pm.spc.data <- spc.data %>% filter(EPA.Region==i)  %>% filter(pollutants %in% c("PM25_SO4_ob", "PM25_SO4_mod"))  
  # 
  # na.omit(pm.spc.data) %>% 
  #   ggplot(aes(x=Time_On)) + geom_point(aes(y=value, color=pollutants), size=3) + 
  #   geom_line(aes(y=value, color=pollutants))+
  #   labs(x="2020", title=expression(paste(SO[4], " ", mu, "g/", m^3)))+
  #   theme (axis.title.x = element_blank(),
  #          axis.title.y = element_blank(),
  #          axis.text = element_text(size=15),
  #          title=element_text(size=20),
  #          legend.title = element_blank(),
  #          legend.position = c(0.3,0.8),
  #          legend.text = element_text(size=20)) +
  #   scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5)) +
  #   scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
  # 
  # ggsave(paste0("pm.so4_eval_epa_region_", regions.vec[i],".png"), path = "./plots/", width=8, height=6, units="in")
  # 
  # pm.spc.data <- reshape(pm.spc.data, idvar = "Time_On", timevar = "pollutants", direction = "wide")
  # 
  # names(pm.spc.data)[names(pm.spc.data) == "value.PM25_SO4_ob"] <- 'PM25_SO4_ob'
  # names(pm.spc.data)[names(pm.spc.data) == "value.PM25_SO4_mod"] <- 'PM25_SO4_mod'
  # names(pm.spc.data)[names(pm.spc.data) == "Time_On"] <- 'date'
  # 
  # mod.eval.pm25.so4 <- as.data.table(modStats(pm.spc.data , mod = "PM25_SO4_mod", obs = "PM25_SO4_ob", type="season"))
  # mod.eval.pm25.so4$variable <- "so4"
  # mod.eval.pm25.so4$epa_region <- regions.vec[i]
  # 
  # 
  # # ======nitrate
  # pm.spc.data <- spc.data %>% filter(EPA.Region==i)  %>% filter(pollutants %in% c("PM25_NO3_ob", "PM25_NO3_mod"))  
  # 
  # na.omit(pm.spc.data) %>% 
  #   ggplot(aes(x=Time_On)) + geom_point(aes(y=value, color=pollutants), size=3) + 
  #   geom_line(aes(y=value, color=pollutants))+
  #   labs(x="2020", title=expression(paste(NO[3], " ", mu, "g/", m^3)))+
  #   theme (axis.title.x = element_blank(),
  #          axis.title.y = element_blank(),
  #          axis.text = element_text(size=15),
  #          title=element_text(size=20),
  #          legend.title = element_blank(),
  #          legend.position = c(0.4,0.8),
  #          legend.text = element_text(size=20)) +
  #   scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  #   scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
  # 
  # ggsave(paste0("pm.no3_eval_epa_region_", regions.vec[i],".png"), path = "./plots/", width=8, height=6, units="in")
  # 
  # pm.spc.data <- reshape(pm.spc.data, idvar = "Time_On", timevar = "pollutants", direction = "wide")
  # 
  # names(pm.spc.data)[names(pm.spc.data) == "value.PM25_NO3_ob"] <- 'PM25_NO3_ob'
  # names(pm.spc.data)[names(pm.spc.data) == "value.PM25_NO3_mod"] <- 'PM25_NO3_mod'
  # names(pm.spc.data)[names(pm.spc.data) == "Time_On"] <- 'date'
  # 
  # mod.eval.pm25.no3 <- as.data.table(modStats(pm.spc.data , mod = "PM25_NO3_mod", obs = "PM25_NO3_ob", type="season"))
  # mod.eval.pm25.no3$variable <- "no3"
  # mod.eval.pm25.no3$epa_region <- regions.vec[i]
  
  
  
  
  # ======NH4
  # pm.spc.data <- spc.data %>% filter(EPA.Region==i)  %>% filter(pollutants %in% c("PM25_NH4_ob", "PM25_NH4_mod"))  
  # 
  # na.omit(pm.spc.data) %>% 
  #   ggplot(aes(x=Time_On)) + geom_point(aes(y=value, color=pollutants), size=3) + 
  #   geom_line(aes(y=value, color=pollutants))+
  #   labs(x="2020", title=expression(paste(NH[4], " ", mu, "g/", m^3)))+
  #   theme (axis.title.x = element_blank(),
  #          axis.title.y = element_blank(),
  #          axis.text = element_text(size=15),
  #          title=element_text(size=20),
  #          legend.title = element_blank(),
  #          legend.position = c(0.3,0.8),
  #          legend.text = element_text(size=20)) +
  #   scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  #   scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
  # 
  # ggsave(paste0("PM.NH4_eval_epa_region_", regions.vec[i],".png"), path = "./plots/", width=8, height=6, units="in")
  # 
  # pm.spc.data <- reshape(pm.spc.data, idvar = "Time_On", timevar = "pollutants", direction = "wide")
  # 
  # names(pm.spc.data)[names(pm.spc.data) == "value.PM25_NH4_ob"] <- 'PM25_NH4_ob'
  # names(pm.spc.data)[names(pm.spc.data) == "value.PM25_NH4_mod"] <- 'PM25_NH4_mod'
  # names(pm.spc.data)[names(pm.spc.data) == "Time_On"] <- 'date'
  # 
  # mod.eval.pm25.nh4 <- as.data.table(modStats(pm.spc.data , mod = "PM25_NH4_mod", obs = "PM25_NH4_ob", type="season"))
  # mod.eval.pm25.nh4$variable <- "nh4"
  # mod.eval.pm25.nh4$epa_region <- regions.vec[i]
  # 
  # # ======OC
  # pm.spc.data <- spc.data %>% filter(EPA.Region==i)  %>% filter(pollutants %in% c("PM25_OC_ob", "PM25_OC_mod"))  
  # 
  # na.omit(pm.spc.data) %>% 
  #   ggplot(aes(x=Time_On)) + geom_point(aes(y=value, color=pollutants), size=3) + 
  #   geom_line(aes(y=value, color=pollutants))+
  #   labs(x="2020", title=expression(paste("OC", " ", mu, "g/", m^3)))+
  #   theme (axis.title.x = element_blank(),
  #          axis.title.y = element_blank(),
  #          axis.text = element_text(size=15),
  #          title=element_text(size=20),
  #          legend.title = element_blank(),
  #          legend.position = c(0.3,0.9),
  #          legend.text = element_text(size=20)) +
  #   scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) +
  #   scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
  # 
  # ggsave(paste0("PM.oc_eval_epa_region_", regions.vec[i],".png"), path = "./plots/", width=8, height=6, units="in")
  # 
  # pm.spc.data <- reshape(pm.spc.data, idvar = "Time_On", timevar = "pollutants", direction = "wide")
  # 
  # names(pm.spc.data)[names(pm.spc.data) == "value.PM25_OC_ob"] <- 'PM25_OC_ob'
  # names(pm.spc.data)[names(pm.spc.data) == "value.PM25_OC_mod"] <- 'PM25_OC_mod'
  # names(pm.spc.data)[names(pm.spc.data) == "Time_On"] <- 'date'
  # 
  # mod.eval.pm25.oc <- as.data.table(modStats(pm.spc.data , mod = "PM25_OC_mod", obs = "PM25_OC_ob", type="season"))
  # mod.eval.pm25.oc$variable <- "oc"
  # mod.eval.pm25.oc$epa_region <- regions.vec[i]
  # 
  # 
  # # ======EC
  # pm.spc.data <- spc.data %>% filter(EPA.Region==i)  %>% filter(pollutants %in% c("PM25_EC_ob", "PM25_EC_mod"))  
  # 
  # na.omit(pm.spc.data) %>% 
  #   ggplot(aes(x=Time_On)) + geom_point(aes(y=value, color=pollutants), size=3) + 
  #   geom_line(aes(y=value, color=pollutants))+
  #   labs(x="2020", title=expression(paste("EC" , " ", mu, "g/", m^3)))+
  #   theme (axis.title.x = element_blank(),
  #          axis.title.y = element_blank(),
  #          axis.text = element_text(size=15),
  #          title=element_text(size=20),
  #          legend.title = element_blank(),
  #          legend.position = c(0.3,0.8),
  #          legend.text = element_text(size=20)) +
  #   scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  #   scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
  # 
  # ggsave(paste0("PM.EC_eval_epa_region_", regions.vec[i],".png"), path = "./plots/", width=8, height=6, units="in")
  # 
  # pm.spc.data <- reshape(pm.spc.data, idvar = "Time_On", timevar = "pollutants", direction = "wide")
  # 
  # names(pm.spc.data)[names(pm.spc.data) == "value.PM25_EC_ob"] <- 'PM25_EC_ob'
  # names(pm.spc.data)[names(pm.spc.data) == "value.PM25_EC_mod"] <- 'PM25_EC_mod'
  # names(pm.spc.data)[names(pm.spc.data) == "Time_On"] <- 'date'
  # 
  # mod.eval.pm25.ec <- as.data.table(modStats(pm.spc.data , mod = "PM25_EC_mod", obs = "PM25_EC_ob", type="season"))
  # mod.eval.pm25.ec$variable <- "ec"
  # mod.eval.pm25.ec$epa_region <- regions.vec[i]
  
  eval.metrics <- rbind(mod.eval.pm25, mod.eval.pm25.o3)
  
  eval.metrics$r2 <- (eval.metrics$r)^2
  
  eval.metrics$NMB <- eval.metrics$NMB*100
  eval.metrics$NMGE <- eval.metrics$NMGE*100
  
  
  #evaluation metrics plots by season
  
  # eval.metrics %>%  ggplot(aes(x=variable, y=r, color=season)) + geom_point(size=5) +
  #   scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "red"))+
  #   labs (title=paste0(("Pearson correlation coefficient"), " in EPA Region ", regions.vec[i])) +
  #   scale_y_continuous(expand=c(0,0), limits = c(-1, 1)) +
  #   theme (axis.title.x = element_blank(),
  #          axis.title.y = element_blank(),
  #          axis.text = element_text(size=15),
  #          title=element_text(size=20),
  #          legend.title = element_blank(),
  #          legend.position = c(0.85,0.2),
  #          legend.text = element_text(size=20)) +
  #   geom_hline(yintercept = 0)
  # 
  # 
  # ggsave(paste0("pearsor_r_epa_region_", regions.vec[i],".png"), path = "./plots/", width=8, height=6, units="in")
  # 
  # eval.metrics  %>%  ggplot(aes(x=variable, y=NMB,  color=season)) + geom_point(size=5) + 
  #   scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "red")) +
  #   labs (title=paste0(("Normalized mean bias (%)"), " in EPA Region ", regions.vec[i]))  +
  #   scale_y_continuous(expand=c(0,0), limits = c(-100, 200)) +
  #   theme (axis.title.x = element_blank(),
  #          axis.title.y = element_blank(),
  #          axis.text = element_text(size=15),
  #          title=element_text(size=20),
  #          legend.title = element_blank(),
  #          legend.position = c(0.25,0.8),
  #          legend.text = element_text(size=20)) +
  #   geom_hline(yintercept = 0)
  # 
  # ggsave(paste0("nmb_epa_region_", regions.vec[i],".png"), path = "./plots/", width=8, height=6, units="in")
  # 
  # eval.metrics %>%  ggplot(aes(x=variable, y=NMGE,  color=season)) + geom_point(size=5) + 
  #   scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "red"))+
  #   labs (title=paste0(("Normalized mean gross error (%)"), " in EPA Region ", regions.vec[i]))  +
  #   scale_y_continuous(expand=c(0,0), limits = c(0, 200)) +
  #   theme (axis.title.x = element_blank(),
  #          axis.title.y = element_blank(),
  #          axis.text = element_text(size=15),
  #          title=element_text(size=20),
  #          legend.title = element_blank(),
  #          legend.position = c(0.85,0.85),
  #          legend.text = element_text(size=20)) 
  # 
  # ggsave(paste0("nmge_epa_region_", regions.vec[i],".png"), path = "./plots/", width=8, height=6, units="in")
  
  
  datalist[[i]] <-  eval.metrics
}

ev.metrics <- do.call(rbind,datalist)


#plots


#NMGE
ev.metrics %>%  ggplot(aes(x=variable, y=NMGE,  color=season)) + geom_point(size=5) +
  scale_x_discrete(breaks=c('o3','pm25'),labels = c(expression(paste( O[3])), expression(paste( PM[2.5])))) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "red"))+
  labs (title=paste0(("Normalized mean gross error (%)")))  +
  scale_y_continuous(expand=c(0,0), limits = c(0, 100)) +
  theme (axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text = element_text(size=15),
         title=element_text(size=20),
         legend.title = element_blank(),
         legend.position = c(0.8,0.15),
         legend.text = element_text(size=20),
         strip.text.x = element_text(size = 20)) + facet_wrap(epa_region~. )

ggsave(paste0("nmge.png"), path = "./plots/", width=8, height=6, units="in")

#NMB
ev.metrics %>%  ggplot(aes(x=variable, y=NMB,  color=season)) + geom_point(size=5) +
  scale_x_discrete(breaks=c('o3','pm25'),labels = c(expression(paste( O[3])), expression(paste( PM[2.5])))) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "red"))+
  labs (x= bquote(O[3]), title=paste0(("Normalized mean bias (%)")))  +
  scale_y_continuous(expand=c(0,0), limits = c(-100, 100)) +
  theme (axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text = element_text(size=15),
         title=element_text(size=20),
         legend.title = element_blank(),
         legend.position = c(0.8,0.15),
         legend.text = element_text(size=20),
         strip.text.x = element_text(size = 30)) + facet_wrap(epa_region~. ) +
  geom_hline(yintercept=0)

ggsave(paste0("pm25_eval_epa_region_", regions.vec[i],".png"), path = "./plots/", width=8, height=6, units="in")

#Pearson R
ev.metrics %>%  ggplot(aes(x=variable, y=r,  color=season)) + geom_point(size=5) +
  scale_x_discrete(breaks=c('o3','pm25'),labels = c(expression(paste( O[3])), expression(paste( PM[2.5])))) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "red"))+
  labs (title=paste0(("Pearson correlation R")))  +
  scale_y_continuous(expand=c(0,0), limits = c(-0.5, 1)) +
  theme (axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text = element_text(size=15),
         title=element_text(size=20),
         legend.title = element_blank(),
         legend.position = c(0.8,0.15),
         legend.text = element_text(size=20),
         strip.text.x = element_text(size = 30)) + facet_wrap(epa_region~. ) 

ggsave(paste0("pm25_eval_epa_region_", regions.vec[i],".png"), path = "./plots/", width=8, height=6, units="in")

