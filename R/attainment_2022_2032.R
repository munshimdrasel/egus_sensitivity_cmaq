rm(list = ls())

library(fst)
library(ncdf4)
library(fields)
library(proj4)
library(downloader)
library(tidyverse)
library(stringi)
library( sf)
library( raster)
library( data.table)
library(tidycensus, quietly = TRUE)
library(tigris, quietly = TRUE)
library( fasterize)
library( USAboundaries)
library( magrittr)
library(reshape2)
library(hrbrthemes)
library(tidyr)


#> Set location where you have downloaded the CMAQ netcdf file.  
setwd("/Users/munshirasel/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/gmu_one_drive_rasel/R/egus_sensitivity_cmaq")


# ==============Reading CMAQ files and combining data===========


#cmaq_1
# loading daily gridded data
load (file=paste0("./data/bruteforce_output/cmaq_1/cmaq_1.RData"))

load (file=paste0("./data/bruteforce_output/cmaq_1/geolist.RData"))

dt_cmaq_1 <- do.call(rbind,final_list)

geo_list <- data.frame(geo_list[[1]])

mon_avg_cmaq_1<- setDT(dt_cmaq_1)[, .(value = mean(z, na.rm=TRUE)),  by = .(ID, variable)] %>% mutate(sens="so2_ptegu_12.45_nox_2.8")

dt_wide_cmaq_1<- spread(mon_avg_cmaq_1, variable, value) 

#cmaq_2
load (file=paste0("./data/bruteforce_output/cmaq_2/cmaq_2.RData"))

dt_cmaq_2 <- do.call(rbind,final_list)

mon_avg_cmaq_2<- setDT(dt_cmaq_2)[, .(value = mean(z, na.rm=TRUE)),  by = .( ID, variable)] %>% mutate(sens="so2_ptegu_1_nox_0")

dt_wide_cmaq_2<- spread(mon_avg_cmaq_2, variable, value) 

#CMAQ_3
load (file=paste0("./data/bruteforce_output/cmaq_3/cmaq_3.RData"))

dt_cmaq_3 <- do.call(rbind,final_list)

mon_avg_cmaq_3<- setDT(dt_cmaq_3)[, .(value = mean(z, na.rm=TRUE)),  by = .( ID, variable)] %>% mutate(sens="so2_ptegu_0_nox_0")

dt_wide_cmaq_3<- spread(mon_avg_cmaq_3, variable, value) 

#CMAQ_4
load (file=paste0("./data/bruteforce_output/cmaq_4/cmaq_4.RData"))

dt_cmaq_4 <- do.call(rbind,final_list)

mon_avg_cmaq_4<- setDT(dt_cmaq_4)[, .(value = mean(z, na.rm=TRUE)),  by = .( ID, variable)]  %>% mutate(sens="so2_ptegu_12.45_nox_0")

dt_wide_cmaq_4<- spread(mon_avg_cmaq_4, variable, value)

#CMAQ_5
load (file=paste0("./data/bruteforce_output/cmaq_5/cmaq_5.RData"))

dt_cmaq_5 <- do.call(rbind,final_list)

mon_avg_cmaq_5<- setDT(dt_cmaq_5)[, .(value = mean(z, na.rm=TRUE)),  by = .(ID, variable)] %>% mutate(sens="so2_ptegu_0_nox_1")

dt_wide_cmaq_5<- spread(mon_avg_cmaq_5, variable, value)


#CMAQ_base
load (file=paste0("./data/bruteforce_output/cmaq_base/cmaq_base.RData"))

dt_cmaq_base <- do.call(rbind,final_list)

mon_avg_cmaq_base<- setDT(dt_cmaq_base)[, .(value = mean(z, na.rm=TRUE)),  
                                        by = .( ID, variable)] %>% mutate(sens="so2_ptegu_1_nox_1")

dt_wide_cmaq_base<- spread(mon_avg_cmaq_base, variable, value)


bf_long <- rbind(mon_avg_cmaq_1, mon_avg_cmaq_2, mon_avg_cmaq_3,
                 mon_avg_cmaq_4, mon_avg_cmaq_5, mon_avg_cmaq_base)

bf_wide <- rbind(dt_wide_cmaq_1, dt_wide_cmaq_2, dt_wide_cmaq_3,
                 dt_wide_cmaq_4, dt_wide_cmaq_5, dt_wide_cmaq_base)

bf_data_sf <- merge(bf_wide, st_as_sf(geo_list), by= c ("ID")) %>% st_as_sf()

bf_data_sf_contributions <- bf_data_sf %>%  mutate(EC_fraction= PM25_EC*100/PM25,
                                                   OC_fraction= PM25_OC*100/PM25,
                                                   NH4_fraction= PM25_NH4*100/PM25,
                                                   NO3_fraction= PM25_NO3*100/PM25,
                                                   SO4_fraction= PM25_SO4*100/PM25) %>% 
  mutate(other_fraction= 100- (EC_fraction+OC_fraction+NH4_fraction+NO3_fraction+SO4_fraction))

#summary of bf PM2.5_SO4 data
bf_data_sf %>% group_by(sens) %>% summarize(min = min(PM25_SO4),
                                            q1 = quantile(PM25_SO4, 0.25),
                                            median = median(PM25_SO4),
                                            mean = mean(PM25_SO4),
                                            q3 = quantile(PM25_SO4, 0.75),
                                            max = max(PM25_SO4))

#coordinate reference system projection string for spatial data
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

us.sf <-  USAboundaries::us_states()  %>%  filter(!stusps %in% c("PR", "HI", "AK") ) %>%st_transform( crs = p4s)


#EPA's non-attainment areas after considering 9 ug/m3 PM2.5 standards

#2032
pm2.5_std_2032 <- c (9)

projected_2032 <- read.csv(paste0("./data/epa_non_attainment/Source 2032_projections for Design Values greater than or equal to ", pm2.5_std_2032, ".csv")) %>% 
  dplyr::select(-Table.2A.8.PM2.5.DVs.for.2032.Projection) %>% mutate(projected_std = pm2.5_std_2032)

names(projected_2032)[names(projected_2032) == 'Annual.2032.DV..ug.m..3.'] <- 'annual.2032_pm2.5'

projected_2032 <- projected_2032 %>% mutate(annual.2032_pm2.5=as.numeric(annual.2032_pm2.5)) %>% na.omit()


#2022
pm2.5_std_2022 <- c (9)

projected_2022 <- read.csv(paste0("./data/epa_non_attainment/Source Table 6a for concentrations greater than or equal to 9 simplified.csv")) 

names(projected_2022)[names(projected_2022) == 'X2020.2022.....Annual......Design.Value..µg.m3...1.2.'] <- 'annual.2022_pm2.5'

projected_2022 <- projected_2022 %>% mutate(annual.2022_pm2.5=as.numeric(annual.2022_pm2.5)) %>% na.omit()

projected_2022 <- setDT(projected_2022)[, .(annual.2022_pm2.5=mean(annual.2022_pm2.5, na.rm=TRUE)),
                                        by = .( State.Name, County.Name)] 

names(projected_2022)[names(projected_2022) == 'State.Name'] <- 'State'
names(projected_2022)[names(projected_2022) == 'County.Name'] <- 'County'

projected_2022 <- projected_2022 #%>% mutate(id= paste0(State, "-", County)) 

#counties geographic info
states.us <- USAboundaries::us_states( ) %>% dplyr::select(name , state_abbr) %>% st_drop_geometry() %>% 
  filter (!state_abbr %in% c( 'AK', 'HI', 'PR'))
names(states.us)[names(states.us) == 'name'] <- 'State'

projected_2022<- merge(projected_2022, states.us, by = c("State")) %>% dplyr::select(-State)

names(projected_2022)[names(projected_2022) == 'state_abbr'] <- 'State'


counties.us <- USAboundaries::us_counties( ) %>% dplyr::select(state_abbr, name, geometry)

names(counties.us)[names(counties.us) == 'state_abbr'] <- 'State'
names(counties.us)[names(counties.us) == 'name'] <- 'County'

counties.us <- counties.us %>% filter (!State %in% c( 'AK', 'HI', 'PR'))

counties_2032_pm2.5 <- merge( projected_2032 , counties.us, by= c("State", "County")) %>%
  st_as_sf( sf_column_name = 'geometry') %>%
  st_transform( crs = st_crs( p4s)) %>% mutate(id= paste0(State, "-", County)) 

counties_2022_pm2.5 <- merge( projected_2022 , counties.us, by= c("State", "County")) %>%
  st_as_sf( sf_column_name = 'geometry') %>%
  st_transform( crs = st_crs( p4s)) %>% mutate(id= paste0(State, "-", County)) %>% as_data_frame() %>% st_as_sf()





#Desid module used brute force method to project summer concentration in 2032
#We have DDM 1 year data. We want to project 2032 concentration using ratio of brutefore to DDMsummer data
# and then multiply with DDM output to get whole year data

# ============DDM data read==========

load(file=paste0("./data/egus_so2_pm2.5/egus_so2_pm25.RData"))

ddm_dt <- do.call(rbind,datalist)

ddm_geolist <- ddm_dt %>% dplyr::select(ID, geometry)

# ddm_geolist_distinct <- ddm_geolist %>% distinct()

# save(ddm_geolist_distinct, file=paste0("./data/ddm_geolist_distinct.RData"))

load("./data/ddm_geolist_distinct.RData")
# setDT(ddm_summer)[, .(pm2.5_so2=mean(z, na.rm=TRUE)),
#                   by = .(ID)]

#ddm yearly average concentration
ddm_yr <- ddm_dt %>% st_drop_geometry() %>% dplyr::select(-year)

ddm_yr_avg <-  setDT(ddm_yr)[, .(pm2.5_so2_yr_avg=mean(z, na.rm=TRUE)),
                             by = .(ID)]

#adding geometry to the yearly average DDM Coal SO2 PM2.5 concentration

ddm_yr_avg_sf <- merge(ddm_yr_avg, geo_list, by="ID") %>% st_as_sf()


#ddm summer average calculation to get ration between DESID and DDM results
ddm_summer_avg <- ddm_dt %>% st_drop_geometry() %>% filter(month %in% c("07", "08")) %>% dplyr::select(-year)

ddm_summer_avg <-  setDT(ddm_summer_avg)[, .(pm2.5_so2_summer_avg=mean(z, na.rm=TRUE)),  by = .(ID)]

#adding geometry

ddm_summer_avg <- merge(ddm_summer_avg, geo_list, by="ID") %>% st_as_sf()


#PM_BF over PM_DDM for 2032

#getting BF data

#PM2.5 [base]- PM2.5[No EGU SO2] =PM2.5[EGU]

pm2.5_no_egu <- bf_data_sf %>% filter (sens %in% c ("so2_ptegu_1_nox_1", 
                                                    "so2_ptegu_0_nox_1",
                                                    "so2_ptegu_12.45_nox_2.8")) %>% 
  dplyr::select(sens, PM25, ID) 

#converting 2005 PM2.5 to 2020 by dividing 12.45
pm2.5_no_linearity_bf_summer <-reshape2::dcast (pm2.5_no_egu, ID ~ sens, value.var = "PM25") %>% 
  dplyr::select(-so2_ptegu_0_nox_1, -so2_ptegu_1_nox_1) %>% 
  mutate(non_linearity_2005 = so2_ptegu_12.45_nox_2.8/12.45)

pm2.5_ddm_summer <- ddm_summer_avg %>% st_drop_geometry()


pm2.5_ddm_yr <- ddm_yr_avg_sf %>% st_drop_geometry()


pm2.5_no_linearity_bf_ddm_summer <- merge(pm2.5_no_linearity_bf_summer, pm2.5_ddm_summer, by = c("ID")  )


pm2.5_no_linearity_bf_ddm_yr <- merge (pm2.5_no_linearity_bf_ddm_summer, pm2.5_ddm_yr, by =c ("ID"))


pm2.5_no_linearity_bf_ddm_yr <- merge(pm2.5_no_linearity_bf_ddm_yr, geo_list, by = ("ID")) %>% st_as_sf()

pm2.5_no_linearity_bf_ddm_yr <- pm2.5_no_linearity_bf_ddm_yr %>% 
  mutate(non_linearity_2005_2020= non_linearity_2005*pm2.5_so2_yr_avg/pm2.5_so2_summer_avg,
         pm2.5_2005_yr=so2_ptegu_12.45_nox_2.8*pm2.5_so2_yr_avg/pm2.5_so2_summer_avg)


#HYADS data

# getting disperseR  pm2.5 data from Henneman et al. 2023


source_impacts_dir <- '/Users/munshirasel/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/gmu_one_drive_rasel/R/causal-study-COVID-beyond/data/actual_disperseR/disperseR/hyads_to_PM'


# get the names of the gridded HyADS output files from the data directory
grid.files.yr <- list.files( source_impacts_dir,
                             pattern = 'grids_pm25_total_\\d{4}\\.fst',
                             full.names = TRUE)

# read select files and combine into single data.table
grid.dat <- lapply( grid.files.yr,
                    function( f){
                      year.f <- gsub( '^.*_|\\.fst', '', f)
                      
                      in.f <- read.fst( f, as.data.table = T)
                      setnames( in.f, 'vals.out', 'coal_pm25')
                      in.f[, year := year.f]
                    }) %>% rbindlist

# summarize the data
summary( grid.dat)

# first, use dcast to get year columns
grid.dat.c <- dcast( grid.dat, x + y ~ year, value.var = 'coal_pm25')

# convert to raster
grid.dat.r <- rasterFromXYZ( grid.dat.c, crs = p4s)

# plot, which will show only ~16 of the years contained in grid.dat.r
# plot( grid.dat.r)


# create sf object
grid.dat.sf <- rasterToPolygons( grid.dat.r) %>%
  st_as_sf()

#interpolating 36km grid into 12km CMAQ grid
grid.dat.sf_12 <- st_interpolate_aw( grid.dat.sf["X2020"], pm2.5_no_linearity_bf_ddm_yr,  
                                     extensive = F)

names(grid.dat.sf_12)[names(grid.dat.sf_12) == 'X2020'] <- 'hyads_coal_pm25'


plot_a <- ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=grid.dat.sf_12, aes( fill = hyads_coal_pm25,  geometry = geometry), color = NA) +
  scale_fill_viridis_c( option="plasma", name = expression(paste(PM[2.5], ", ", mu, "g/", m^3)),
                        limit=c(0,1), oob = scales::squish) +
  scale_x_continuous( expand = c( 0, 0)) +
  scale_y_continuous( expand = c( 0, 0)) +
  theme_bw() +
  theme( axis.text = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.direction = 'horizontal',
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA),
         legend.position = "none",
         legend.text = element_text( size = 15),
         legend.title = element_text( size = 25),
         panel.background = element_rect( fill = 'white'),
         panel.grid = element_blank(),
         panel.border = element_blank()) +
  guides(fill = guide_colorbar( label.position = "bottom", 
                                title.position = "top", title.vjust = -2, title.hjust = 1, 
                                label.vjust= 4,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 10,
                                barheight = 1)) 



# ggarrange( plot_a, plot_b,  labels = c("A", "B"), ncol = 2, nrow = 1, label.x = 0.9, label.y = 0.85,
#            common.legend = T, legend = "bottom") 


#non linearity plot

plot_b <- ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=pm2.5_no_linearity_bf_ddm_yr,
          aes( fill = non_linearity_2005 ,  geometry = geometry), color = NA) +
  geom_sf( data = us.sf, fill="NA", color= "black") +
  scale_fill_viridis_c( option="plasma",
                        name = expression(paste(PM[2.5], ", ",mu, "g/", m^3)),
                        limit=c(0,1), oob = scales::squish) +
  scale_x_continuous( expand = c( 0, 0)) +
  scale_y_continuous( expand = c( 0, 0)) +
  theme_bw() +
  theme( axis.text = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.direction = 'horizontal',
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA),
         legend.position = c(0.15,0.1),
         legend.text = element_text( size = 15),
         legend.title = element_text( size = 25),
         panel.background = element_rect( fill = 'white'),
         panel.grid = element_blank(),
         panel.border = element_blank()) +
  guides(fill = guide_colorbar( label.position = "bottom",
                                title.position = "top", title.vjust = 0, title.hjust = 0,
                                label.vjust= -0.6,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 10,
                                barheight = 1))


plot_c <- ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=pm2.5_no_linearity_bf_ddm_yr,
          aes( fill = pm2.5_so2_summer_avg ,  geometry = geometry), color = NA) +
  geom_sf( data = us.sf, fill="NA", color= "black") +
  scale_fill_viridis_c( option="plasma",
                        name = expression(paste(PM[2.5],", ",mu, "g/", m^3)),
                        limit=c(0,1), oob = scales::squish) +
  scale_x_continuous( expand = c( 0, 0)) +
  scale_y_continuous( expand = c( 0, 0)) +
  theme_bw() +
  theme( axis.text = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.direction = 'horizontal',
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA),
         legend.position = c(0.15,0.1),
         legend.text = element_text( size = 15),
         legend.title = element_text( size = 25),
         panel.background = element_rect( fill = 'white'),
         panel.grid = element_blank(),
         panel.border = element_blank()) +
  guides(fill = guide_colorbar( label.position = "bottom",
                                title.position = "top", title.vjust = 0, title.hjust = 0,
                                label.vjust= -0.6,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 10,
                                barheight = 1))

# cowplot::plot_grid(plot_a, plot_b, plot_c,
#                    # 1 column and two rows - stacked on top of each other
#                    ncol = 3,
#                    nrow = 1,
#                    # top plot is 2/3 as tall as second
#                    rel_heights = c(0.33, 0.33, 0.34),
#                    labels = c("A", "B", "C"),
#                    # Customize label size and position if needed
#                    label_size = 30,
#                    label_x=0.1,
#                    label_y=0.80)


cowplot::plot_grid(plot_a, plot_c,
                   # 1 column and two rows - stacked on top of each other
                   ncol = 2,
                   nrow = 1,
                   # top plot is 2/3 as tall as second
                   rel_heights = c(0.5, 0.5),
                   labels = c("A", "B"),
                   # Customize label size and position if needed
                   label_size = 30,
                   label_x=0.1,
                   label_y=0.90)

ggsave("non_linearity_plot_compare.png", path = "./plots/", width=17, height=8, units="in")



plot_2005_adj_2020 <- ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=pm2.5_no_linearity_bf_ddm_yr, 
          aes( fill = non_linearity_2005_2020 ,  geometry = geometry), color = NA) +
  geom_sf( data = us.sf, fill="NA", color= "black") +
  scale_fill_viridis_c( option="plasma",
                        name = expression(paste(PM[2.5], " ","2005_adj_2020, ",mu, "g/", m^3)),
                        limit=c(0,1), oob = scales::squish) +
  scale_x_continuous( expand = c( 0, 0)) +
  scale_y_continuous( expand = c( 0, 0)) +
  theme_bw() +
  theme( axis.text = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.direction = 'horizontal',
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA),
         legend.position = c(0.15,0.1),
         legend.text = element_text( size = 15),
         legend.title = element_text( size = 25),
         panel.background = element_rect( fill = 'white'),
         panel.grid = element_blank(),
         panel.border = element_blank()) +
  guides(fill = guide_colorbar( label.position = "bottom", 
                                title.position = "top", title.vjust = 0, title.hjust = 0,
                                label.vjust= -0.6,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 10,
                                barheight = 1))


plot_2020 <- ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=pm2.5_no_linearity_bf_ddm_yr, 
          aes( fill = pm2.5_so2_yr_avg ,  geometry = geometry), color = NA) +
  geom_sf( data = us.sf, fill="NA", color= "black") +
  scale_fill_viridis_c( option="plasma", name = expression(paste(PM[2.5], " ","2020, ", mu, "g/", m^3)),
                        limit=c(0,1), oob = scales::squish) +
  scale_x_continuous( expand = c( 0, 0)) +
  scale_y_continuous( expand = c( 0, 0)) +
  theme_bw() +
  theme( axis.text = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.direction = 'horizontal',
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA),
         legend.position = c(0.15,0.1),
         legend.text = element_text( size = 15),
         legend.title = element_text( size = 25),
         panel.background = element_rect( fill = 'white'),
         panel.grid = element_blank(),
         panel.border = element_blank()) +
  guides(fill = guide_colorbar( label.position = "bottom", 
                                title.position = "top", title.vjust = 0, title.hjust = 0,
                                label.vjust= -0.6,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 10,
                                barheight = 1)) 




cowplot::plot_grid(plot_2005_adj_2020, plot_2020,
                   # 1 column and two rows - stacked on top of each other
                   ncol = 2,
                   nrow = 1,
                   # top plot is 2/3 as tall as second
                   rel_heights = c(0.5, 0.5))

ggsave("non_linearity_plot.png", path = "./plots/", width=25, height=8, units="in")




#amount PM2.5 will be reduced due to no Coal EGUs by 2032
pm2.5_no_egu_wide <-reshape2::dcast (pm2.5_no_egu, ID ~ sens, value.var = "PM25") %>% 
  mutate(pm2.5=so2_ptegu_1_nox_1- so2_ptegu_0_nox_1) 

pm2.5_no_egu_wide <- merge(pm2.5_no_egu_wide, geo_list, by= c("ID"))

pm2.5_no_egu_wide<- pm2.5_no_egu_wide

pm2.5_bf_summer <- pm2.5_no_egu_wide %>% st_drop_geometry()
pm2.5_ddm_summer <- ddm_summer_avg %>% st_drop_geometry()
pm2.5_ddm_yr <- ddm_yr_avg_sf %>% st_drop_geometry()

pm2.5_bf_ddm_summer <- merge(pm2.5_bf_summer, pm2.5_ddm_summer, by= c ("ID"))


pm2.5_bf_ddm_summer_yr <- merge(pm2.5_bf_ddm_summer, pm2.5_ddm_yr, by= c ("ID"))

pm2.5_proj_2032 <- pm2.5_bf_ddm_summer_yr %>% mutate(proj_2032= pm2.5*pm2.5_so2_yr_avg/pm2.5_so2_summer_avg) %>% 
  st_drop_geometry() %>% dplyr::select(-geometry)

pm2.5_proj_2032 <- merge(pm2.5_proj_2032, geo_list, by = ("ID")) %>% st_as_sf()

#projected values for 2022 and 2032 same 
#2022: assuming all coal EGUs shutdown by 2022 and vice versa for 2032

pm2.5_proj_2022 <- pm2.5_bf_ddm_summer_yr %>% mutate(proj_2022= 
                                                       pm2.5*pm2.5_so2_yr_avg/pm2.5_so2_summer_avg) %>% 
  st_drop_geometry() %>% dplyr::select(-geometry)

pm2.5_proj_2022 <- merge(pm2.5_proj_2022, geo_list, by = ("ID")) %>% st_as_sf()

#figure of ddm summer sensitivity vs bf adjusted summer sensitivity

#bf adjusted sensitivity
bf_plot <- ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=pm2.5_proj_2022, 
          aes( fill = proj_2022 ,  geometry = geometry), color = NA) +
  geom_sf( data = us.sf, fill="NA", color= "black") +
  scale_fill_viridis_c( option="plasma", name = expression(paste("BF ",PM[2.5], " ", mu, "g/", m^3)),
                        limit=c(0,0.5), oob = scales::squish) +
  scale_x_continuous( expand = c( 0, 0)) +
  scale_y_continuous( expand = c( 0, 0)) +
  theme_bw() +
  theme( axis.text = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.direction = 'horizontal',
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA),
         legend.position = c(0.15,0.1),
         legend.text = element_text( size = 15),
         legend.title = element_text( size = 25),
         panel.background = element_rect( fill = 'white'),
         panel.grid = element_blank(),
         panel.border = element_blank()) +
  guides(fill = guide_colorbar( label.position = "bottom", 
                                title.position = "top", title.vjust = 0, title.hjust = 0,
                                label.vjust= -0.6,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 10,
                                barheight = 1))

ggsave("PM2.5_BF_proj_Ann_DDM_plot.png", path = "./plots/", width=12, height=8, units="in")


#ddm yearly sensitivity
ddm_plot <- ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=pm2.5_proj_2022, 
          aes( fill = pm2.5_so2_yr_avg ,  geometry = geometry), color = NA) +
  geom_sf( data = us.sf, fill="NA", color= "black") +
  scale_fill_viridis_c( option="plasma", name = expression(paste("DDM ",PM[2.5], " ", mu, "g/", m^3)),
                        limit=c(0,0.5), oob = scales::squish) +
  scale_x_continuous( expand = c( 0, 0)) +
  scale_y_continuous( expand = c( 0, 0)) +
  theme_bw() +
  theme( axis.text = element_blank(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         legend.direction = 'horizontal',
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA),
         legend.position = c(0.15,0.1),
         legend.text = element_text( size = 15),
         legend.title = element_text( size = 25),
         panel.background = element_rect( fill = 'white'),
         panel.grid = element_blank(),
         panel.border = element_blank()) +
  guides(fill = guide_colorbar( label.position = "bottom", 
                                title.position = "top", title.vjust = 0, title.hjust = 0,
                                label.vjust= -0.6,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 10,
                                barheight = 1))


ggsave("ddm_yr_plot.png", path = "./plots/", width=12, height=8, units="in")


cowplot::plot_grid(bf_plot, ddm_plot,
                   # 1 column and two rows - stacked on top of each other
                   ncol = 2,
                   nrow = 1,
                   # top plot is 2/3 as tall as second
                   rel_heights = c(0.5, 0.5))

ggsave("bf_ddm_yr_plot.png", path = "./plots/", width=15, height=8, units="in")






# bf counties info

county.vec.2032 <- as.vector(unique(counties_2032_pm2.5$id))
county.vec.2022 <- as.vector(unique(counties_2022_pm2.5$id))

#getting counties gridded concentration by using intersection function for 2032
datalist = list()

for (k in 1:length(county.vec.2032)) {
    county_df <- counties_2032_pm2.5 [k, ]
    county_crop_2032 <-  st_intersection( pm2.5_proj_2032,county_df ) %>% 
      dplyr::select(id, State, County, proj_2032) 
    
    county_crop_2032_mean<- setDT(county_crop_2032)[, .(proj_2032=mean(proj_2032, na.rm=TRUE)),
                                  by = .(id, State, County)] 
    
    
    datalist[[k]] <-county_crop_2032_mean
  }
  
counties_2032_conc = do.call(rbind, datalist)

summary(counties_2032_conc)

# In the 55 counties projected by EPA to be in non-attainment in 2032, 
# the average (range) CMAQ-DDM PM2.5 sensitivity is XX (YY-ZZ). 

#getting counties gridded concentration by using intersection function for 2022
datalist_2022 = list()

for (k in 1:length(county.vec.2022)) {
  county_df <- counties_2022_pm2.5 [k, ]
  county_crop_2022 <-  st_intersection( pm2.5_proj_2022,county_df ) %>% 
    dplyr::select(id, State, County, proj_2022) 
  
  county_crop_2022_mean<- setDT(county_crop_2022)[, .(proj_2022=mean(proj_2022, na.rm=TRUE)),
                                                  by = .(id, State, County)] 
  
  
  datalist_2022[[k]] <-county_crop_2022_mean
}

counties_2022_conc = do.call(rbind, datalist_2022)


coal_egus_2032 <- merge(counties_2032_conc, counties_2032_pm2.5, by = c ("State", "County", "id") ) %>% 
  mutate(attainment= annual.2032_pm2.5- proj_2032)
coal_egus_2022 <- merge(counties_2022_conc, counties_2022_pm2.5, by = c ("State", "County", "id") ) %>% 
  mutate(attainment= annual.2022_pm2.5- proj_2022)

summary(coal_egus_2032)

# Counties that can be brought under attainment by 2032 by shutting down coal EGUs

coal_egus_2032 %>% filter(attainment <9) %>% 
  ggplot() +
  geom_segment( aes(x=reorder(id ,attainment), 
                    xend=id, y=attainment, yend=annual.2032_pm2.5), color="grey") +
  geom_point( aes(x=id, y=attainment), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=id, y=annual.2032_pm2.5), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_ipsum() +
  geom_hline(yintercept = 9,   color = "black", size=1 ) +
  theme( legend.position = 'none',
         axis.text  = element_text( angle = 0, size=16),
         axis.title = element_blank()) +
  xlab("") +
  ylab("pm25_no_coal.egus_same_nox")


highlight_counties <- c("TX-Nueces", "PA-Armstrong", "OH-Jefferson", "PA-Lebanon", "TX-Travis", "PA-Cambria", "IL-Philadelphia")

#all counties in 2032
attain_2032_coal <- coal_egus_2032 %>%
  ggplot() +
  # Grey lines connecting the points
  geom_segment(aes(x = reorder(as.character(coal_egus_2032$id), -attainment), 
                   xend = as.character(coal_egus_2032$id), y = attainment, yend = annual.2032_pm2.5), 
               color = "grey") +
  
  # Left point (attainment) as red
  geom_point(aes(x = as.character(coal_egus_2032$id), y = attainment), 
             color = "darkgreen", size = ifelse(as.character(coal_egus_2032$id) %in% highlight_counties, 4, 3)) +
  
  # Right point (annual.2032_pm2.5) as green
  geom_point(aes(x = as.character(coal_egus_2032$id), y = annual.2032_pm2.5), 
             color = "darkred", size = ifelse(as.character(coal_egus_2032$id) %in% highlight_counties, 4, 3)) +
  
  coord_flip() +
  theme_bw() +
  scale_y_continuous(breaks = seq(9, 16, by = 1)) +
  
  geom_hline(yintercept = 9, color = "black", size = 1) +
  
  theme(
    legend.position = "none",  # Remove legend since color is hardcoded
    axis.text.y = element_text(angle = 0, size = 15),
    axis.text.x = element_text(angle = 0, size = 15),
    axis.title.x = element_text(size = 25),
    panel.background = element_blank()
  ) +
  
  labs(x = "", y = expression(paste(PM[2.5], " ", mu, "g/", m^3)))
  
attain_2032_coal



ggsave("counties_2032.png", path = "./plots/", width=12, height=12, units="in")

# Counties that can be brought under attainment by 2022 by shutting down coal EGUs

coal_egus_2022 %>% filter(attainment <9) %>% 
  ggplot() +
  geom_segment( aes(x=reorder(id ,attainment), 
                    xend=id, y=attainment, yend=annual.2022_pm2.5), color="grey") +
  geom_point( aes(x=id, y=attainment), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=id, y=annual.2022_pm2.5), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_ipsum() +
  theme_bw()+
  scale_y_continuous(breaks = seq(9, 16, by = 1)) +
  geom_hline(yintercept = 9,   color = "black", size=1 ) +
  theme( legend.position = 'none',
         axis.text.y  = element_text( angle = 0, size=15),
         axis.text.x  = element_text( angle = 0, size=15),
         axis.title.x = element_text(size=15)) +
  labs (x="", y=expression(paste( PM[2.5], " ug/", m^3)), title=
          "Counties attainment condition by 2022 after controlling coal EGUs")



#all counties
coal_egus_2022  %>% 
  ggplot() +
  geom_segment( aes(x=reorder(id ,attainment), 
                    xend=id, y=attainment, yend=annual.2022_pm2.5), color="grey") +
  geom_point( aes(x=id, y=attainment), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=id, y=annual.2022_pm2.5), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_ipsum() +
  theme_bw()+
  scale_y_continuous(breaks = seq(9, 25, by = 1)) +
  geom_hline(yintercept = 9,   color = "black", size=1 ) +
  theme( legend.position = 'none',
         axis.text.y  = element_text( angle = 0, size=12),
         axis.text.x  = element_text( angle = 0, size=20),
         axis.title.x = element_text(size=25),
         title = element_text(size=20)) +
  labs (x="", y=expression(paste( PM[2.5], " ",mu, "g/", m^3)), title=
          "Counties attainment condition by 2022 after controlling coal EGUs")


ggsave("counties_2022.png", path = "./plots/", width=12, height=20, units="in")

# How many facilities would need to close to being counties into attainment by 2022 and 2032?

#reading ampd data 
load ("/Users/munshirasel/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/gmu_one_drive_rasel/R/ampd-raw-data-processing/data/PP.units.monthly1997_2023.rda")

# taking data for year of 2022

PP.units.monthly2022 <- PP.units.monthly1997_2023 %>% filter (Year==2022)

#number of active coal EGUs as of 2022
active_coal_egus <- PP.units.monthly2022 %>% filter(Fuel1.IsCoal==1)

PP.units.monthly2022$County <- sub(" County$", "", PP.units.monthly2022$County)

PP.units.monthly2022 <- PP.units.monthly2022 [, facility.id := paste(Facility.ID..ORISPL., Unit.ID, sep = "-")]


#adding county FIPS
counties.us.fips <- USAboundaries::us_counties( ) %>% dplyr::select(state_abbr, name, statefp, countyfp) %>% 
  st_drop_geometry()

county_state_fips <- setDT(counties.us.fips) [, fips := paste(statefp, countyfp, sep = "")]

# merge(PP.units.monthly2022, county_state_fips, by ("State", "County"))

names(county_state_fips)[names(county_state_fips) == 'state_abbr'] <- 'State'
names(county_state_fips)[names(county_state_fips) == 'name'] <- 'County'
names(county_state_fips)[names(county_state_fips) == 'countyfp'] <- 'FIPS'

counties_attainment_fips_2032<- merge(coal_egus_2032, county_state_fips, by = c ("State", "County"))
counties_attainment_fips_2022<- merge(coal_egus_2022, county_state_fips, by = c ("State", "County"))


#yearly emission
PP.units.yearly2022 <- PP.units.monthly2022 %>% dplyr::select(Facility.Name,
                                                              Fuel1.IsCoal,
                                                              State,
                                                              County,
                                                              Facility.ID..ORISPL.,
                                                              Unit.ID,
                                                              FIPS,
                                                              Year,
                                                              Month,
                                                              facility.id,
                                                              Facility.Latitude,
                                                              Facility.Longitude,
                                                              SO2..tons.,
                                                              NOx..tons.,
                                                              CO2..short.tons. )

PP.units.yearly2022 <-  setDT(PP.units.yearly2022)[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
                                                       NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                                                       CO2..short.tons. = sum(CO2..short.tons., na.rm=TRUE)),  
                                                   by = .(Year, 
                                                          Facility.Name, 
                                                          State,
                                                          County,
                                                          Facility.ID..ORISPL.,
                                                          Unit.ID,
                                                          FIPS,
                                                          Year,
                                                          facility.id,
                                                          Facility.Latitude,
                                                          Facility.Longitude)]

PP.units.yearly2022<-  merge(PP.units.yearly2022, county_state_fips, by= c("State","County"))


# egus<-  merge(PP.units.yearly2022, counties_attainment_fips, by= c("fips"))

#coal PM2.5 needs to be reduced percentage
counties_attainment_fips_2022 <- counties_attainment_fips_2022 %>% mutate (coal_reduction_pct = (annual.2022_pm2.5- 9)*100/proj_2022 )
counties_attainment_fips_2032 <- counties_attainment_fips_2032 %>% mutate (coal_reduction_pct = (annual.2032_pm2.5- 9)*100/proj_2032 )

#attainment counties by controlling coal EGUs
attainment_counties_2022 <- counties_attainment_fips_2022 %>% filter (attainment<9)
attainment_counties_2032 <- counties_attainment_fips_2032 %>% filter (attainment<9)

# Coal PM2.5 reduction percentage to be in attainment in 2022

coal_reduction_2022 <- ggplot(attainment_counties_2022, aes(x=reorder(id, -coal_reduction_pct), y=coal_reduction_pct)) + 
  geom_bar(stat = "identity", width=0.2) +
  scale_y_continuous(expand=c(0,0), limits = c(0, 100)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text.x =   element_text(size=15, angle = -45),
         axis.text.y = element_text(size=15, angle = 0),
         title=element_text(size=20)) +
  labs(x= "", y="", title= expression(paste("Coal ", PM[2.5], " Reduction Percentage in 2022")))

# Coal PM2.5 reduction percentage to be in attainment in 2032
coal_reduction_2032 <- ggplot(attainment_counties_2032, aes(x=reorder(id, -coal_reduction_pct), y=coal_reduction_pct)) + 
  geom_bar(stat = "identity", width=0.2) +
  scale_y_continuous(expand=c(0,0), limits = c(0, 100)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text.x =   element_text(size=15, angle = -45),
         axis.text.y = element_text(size=15, angle = 0),
         title=element_text(size=20)) +
  labs(x= "", y="", title= expression(paste("Coal ", PM[2.5], " Reduction Percentage in 2032")))

cowplot::plot_grid(coal_reduction_2022, coal_reduction_2032,
                   # 1 column and two rows - stacked on top of each other
                   ncol = 2,
                   nrow = 1,
                   # top plot is 2/3 as tall as second
                   rel_heights = c(0.5, 0.5))

ggsave("coal_reduction.png", path = "./plots/", width=14, height=6, units="in")

attainments_fips_2022 <- as.vector(unique(attainment_counties_2022$fips))
attainments_fips_2032 <- as.vector(unique(attainment_counties_2032$fips))


egus_facilities_attainment_2022 <- PP.units.yearly2022 %>% filter (fips %in% attainments_fips_2022)
egus_facilities_attainment_2032 <- PP.units.yearly2022 %>% filter (fips %in% attainments_fips_2032)

#number of facilities in those attainment counties that needs to be under control
unique(egus_facilities_attainment_2022$Facility.Name)
unique(egus_facilities_attainment_2032$Facility.Name)

#number of sub-facilities in those attainment counties that needs to be under control
unique(egus_facilities_attainment_2022$facility.id)
unique(egus_facilities_attainment_2032$facility.id)

unique(egus_facilities_attainment_2032$Facility.ID..ORISPL.)

#rank order facilities name
egus_facilities_attainment_2022 %>% arrange(desc(SO2..tons.))

egus_facilities_attainment_2032 %>% arrange(desc(SO2..tons.))



# Locations EPA determined to be out of attainment in 2022 and 2023 that would meet 
# standard by shuttering coal plants

counties.us <- counties.us %>% mutate( id= paste0(State,"-",County))
counties.us$group_2032 <- ifelse (counties.us$id %in% county.vec.2032, 
                                  "non-attainment", 
                                  "attainment")

counties.us$group_2022 <- ifelse (counties.us$id %in% county.vec.2022, 
                                  "non-attainment", 
                                  "attainment")

# Define fill colors for different groups
group_colors <- c("attainment" = "white",
                  "non-attainment" = "black")

#2032 non-attainment counties map
ggplot() + geom_sf( data = counties.us, fill="white", color= "black") +
  geom_sf( data = counties.us, aes(fill=group_2032)) +
  scale_fill_manual(values = group_colors) +theme_bw() +
  theme(legend.position = c( 0.87, .12), 
        legend.direction = "vertical" ,
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        text = element_text(size=10))

#2022 non-attainment counties map
ggplot() + geom_sf( data = counties.us, fill="white", color= "black") +
  geom_sf( data = counties.us, aes(fill=group_2022)) +
  scale_fill_manual(values = group_colors) +theme_bw() +
  theme(legend.position = c( 0.87, .12), 
        legend.direction = "vertical" ,
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        text = element_text(size=10))

# plot of countis that will be in attainment after controlling coal-EGUs

coal_group_2032_vec <- as.vector(unique(attainment_counties_2032$id))
coal_group_2022_vec <- as.vector(unique(attainment_counties_2022$id))


counties.us$coal_group_2032 <- ifelse(counties.us$id %in% coal_group_2032_vec , 
                                      "attainment_coal", 
                                      ifelse(counties.us$id %in% county.vec.2032, 
                                             "non-attainment", 
                                             "attainment"))

counties.us$coal_group_2022 <- ifelse(counties.us$id %in% coal_group_2022_vec , 
                                      "attainment_coal", 
                                      ifelse(counties.us$id %in% county.vec.2022, 
                                             "non-attainment", 
                                             "attainment"))

# Define fill colors for different groups
group_colors_coal <- c("attainment" = "white",
                       "attainment_coal" = "red",
                       "non-attainment" = "black")

# 2032 non-attainment counties map
attain_2032 <- ggplot() + 
  geom_sf(data = counties.us, fill = "white", color = "black") +
  geom_sf(data = counties.us, aes(fill = coal_group_2032)) +
  
  # Use more informative labels for the colors
  scale_fill_manual(values = group_colors_coal,
                    labels = c("attainment" = "Attainment",
                               "attainment_coal" = "Attainment with Coal EGUs",
                               "non-attainment" = "Non-Attainment")) +
   theme(
    legend.position = c(0.15, 0.05),
    legend.direction = "vertical",
    legend.background = element_blank(), 
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),  # Transparent panel background
    plot.background = element_blank(),   # Transparent plot background
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("spatial_attainment_2032.png", path = "./plots/", width=8, height=6, units="in")


library(cowplot)

ggdraw() +
  draw_plot(attain_2032_coal) +
  draw_plot(attain_2032, x = 0.5, y = 0.5, width = 0.5, height = 0.5)

ggsave("att_2032.png", path = "./plots/", width=9, height=10, units="in")




#2022 non-attainment counties map
attain_2022 <- ggplot() + geom_sf( data = counties.us, fill="white", color= "black") +
  geom_sf( data = counties.us, aes(fill=coal_group_2022)) +
  scale_fill_manual(values = group_colors_coal) +
  theme(
    legend.position = c(0.1, 0.0),
    legend.direction = "vertical",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),  # Transparent background
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank()
  )


cowplot::plot_grid(attain_2022, attain_2032,
                   # 1 column and two rows - stacked on top of each other
                   ncol = 2,
                   nrow = 1,
                   # top plot is 2/3 as tall as second
                   rel_heights = c(0.5, 0.5))

ggsave("spatial_attainment.png", path = "./plots/", width=12, height=8, units="in")



cowplot::plot_grid(attain_2032_coal, attain_2032,
                   # 1 column and two rows - stacked on top of each other
                   ncol = 1,
                   nrow = 2,
                   # top plot is 2/3 as tall as second
                   rel_heights = c(0.8, 0.5))

ggsave("att_2032.png", path = "./plots/", width=10, height=10, units="in")




# Assuming you have a list of the 7 counties you're interested in
highlight_counties <- c("TX-Nueces", "PA-Armstrong", "OH-Jefferson", "PA-Lebanon", "TX-Travis", "PA-Cambria", "IL-Philadelphia")

# Flip y-axis and highlight important counties
attain_2032_coal <- coal_egus_2032 %>%
  ggplot() +
  geom_segment(aes(x = reorder(id, attainment), 
                   xend = id, y = attainment, yend = annual.2032_pm2.5), color = "grey") +
  geom_point(aes(x = id, y = attainment, 
                 color = ifelse(id %in% highlight_counties, "highlight", "normal")), 
             size = ifelse(id %in% highlight_counties, 4, 3)) +
  geom_point(aes(x = id, y = annual.2032_pm2.5, 
                 color = ifelse(id %in% highlight_counties, "highlight", "normal")), 
             size = ifelse(id %in% highlight_counties, 4, 3)) +
  scale_color_manual(values = c("highlight" = "blue", "normal" = rgb(0.7, 0.2, 0.1, 0.5))) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(breaks = seq(9, 16, by = 1)) +
  geom_hline(yintercept = 9, color = "black", size = 1) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(angle = 0, size = 15),
    axis.text.x = element_text(angle = 0, size = 15),
    axis.title.x = element_text(size = 25)
  ) +
  labs(x = "", y = expression(paste(PM[2.5], " ", mu, "g/", m^3)))

# Adjust the map with informative labels and better positioning
attain_2032 <- ggplot() + 
  geom_sf(data = counties.us, fill = "white", color = "black") +
  geom_sf(data = counties.us, aes(fill = coal_group_2032)) +
  scale_fill_manual(values = group_colors_coal,
                    labels = c("Attainment", "Attainment with Coal", "Non-Attainment")) +
  theme(
    legend.position = c(0.4, 0.5),
    legend.direction = "vertical",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),  # Transparent background
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank()
  )

# Combine the plots and adjust inset positioning
ggdraw() +
  draw_plot(attain_2032_coal) +
  draw_plot(attain_2032, x = 0.55, y = 0.5, width = 0.4, height = 0.4)  # Adjusted inset position


#rank order facilities

## ================================================================ 
# take over HyADS
## ================================================================ 
# read in hyads
# gridded hyads file location
hyads_file_loc <- '/Users/munshirasel/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/gmu_one_drive_rasel/R/causal-study-COVID-beyond/data/actual_disperseR/disperseR/hyads_to_PM/'

# get the names of the gridded HyADS output files
grid.files.unit.yr <- list.files( hyads_file_loc,
                                  pattern = 'grids_pm25_byunit_\\d{4}\\.fst',
                                  full.names = TRUE)

# read select files
grid.unit.dat <- lapply( grid.files.unit.yr,
                         function( f){
                           print( f)
                           year.f <- gsub( '^.*_|\\.fst', '', f) %>%
                             as( 'integer')
                           
                           in.f <- read.fst( f, as.data.table = T)
                           in.f[, year := year.f]
                           
                           in.f[ is.na( in.f)] <- 0
                           return (in.f)
                         }) %>% rbindlist( fill = T)




#facility contribution
grid.unit.dat.y.sf <- grid.unit.dat[ year == 2020] %>%
  rasterFromXYZ( crs = p4s) %>%
  rasterToPolygons() %>%
  st_as_sf() %>%
  st_interpolate_aw( st_transform( counties.us, p4s), extensive = F)

# convert to data.table
grid.unit.dat.y.dt <- as.data.table( grid.unit.dat.y.sf)
grid.unit.dat.y.dt[, `:=` ( year = NULL,
                            geometry = NULL,
                            id = counties.us$id)]

grid.unit.dat.y.m <- melt( grid.unit.dat.y.dt, 
                           measure.vars = grep( '^X', names( grid.unit.dat.y.dt),
                                                value = T),
                           id.vars = c( 'id'),
                           value.name = 'hyads',
                           variable.name = 'uID')

setDT(grid.unit.dat.y.m)[, `:=` (year = 2020,
                          uID = gsub( '^X', '', uID))]


county.id.vec <- as.vector(unique(grid.unit.dat.y.m$id))


data.table(county.id.vec) %>% filter(county.id.vec %in% coal_group_2032_vec)


#2032 coal EGUs contribution to each attainment counties

datalist= list()

for (i in 1:length(coal_group_2032_vec)) {
  ct.dt <- grid.unit.dat.y.m %>% filter (id %in% coal_group_2032_vec[i])
  
  ct.id.cont <- ct.dt %>% mutate(tot_hyads = sum(ct.dt$hyads, na.rm=T)) %>% 
    mutate(contribution = hyads* 100/tot_hyads)
  
  ct.id.rank <- ct.id.cont %>% arrange(desc(contribution))
  
  ct.rank.cum <- ct.id.rank %>% mutate(cum_cont = cumsum(contribution))
  
  datalist[[i]] <- ct.rank.cum
}

cum.rank.2032 <- do.call(rbind, datalist)

#2022 coal EGUs contribution to each attainment counties

datalist= list()

for (i in 1:length(coal_group_2022_vec)) {
  ct.dt <- grid.unit.dat.y.m %>% filter (id %in% coal_group_2022_vec[i])
  
  ct.id.cont <- ct.dt %>% mutate(tot_hyads = sum(ct.dt$hyads, na.rm=T)) %>% 
    mutate(contribution = hyads* 100/tot_hyads)
  
  ct.id.rank <- ct.id.cont %>% arrange(desc(contribution))
  
  ct.rank.cum <- ct.id.rank %>% mutate(cum_cont = cumsum(contribution))
  
  datalist[[i]] <- ct.rank.cum
}

cum.rank.2022 <- do.call(rbind, datalist)


#facilities info

## read the unit dataset
annual_dat <- fread( '/Users/munshirasel/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/gmu_one_drive_rasel/R/ampd-raw-data-processing/data/AMPD_Unit_2023Q3.csv')

units_and_facs_locs <- unique( annual_dat[, .( Facility.Latitude,
                                               Facility.Longitude,
                                               Facility.Name,
                                               State, County, FIPS,
                                               Facility.ID..ORISPL., 
                                               Unit.ID)]) %>% na.omit()

units_and_facs_locs [, uID := paste(Facility.ID..ORISPL., Unit.ID, sep = ".")]


cum.rank.2022<- merge(cum.rank.2022, units_and_facs_locs, by= ("uID")) 

cum.rank.2032<- merge(cum.rank.2032, units_and_facs_locs, by= ("uID")) 


attainment_counties_2022

attainment_counties_2032

#find out facilities that will need to be shutdown to bring it down to attainment zone

#2032
fac_IL_Madison <- cum.rank.2032 %>% filter(id=="IL-Madison" & cum_cont <10)

fac_OH_Jefferson <- cum.rank.2032 %>% filter(id=="OH-Jefferson" & cum_cont <78) %>% arrange(desc(cum_cont))

fac_PA_Armstrong <-cum.rank.2032 %>% filter(id=="PA-Armstrong" & cum_cont <84) %>% arrange(desc(cum_cont))

fac_PA_Cambria <- cum.rank.2032 %>% filter(id=="PA-Cambria" & cum_cont <24) %>% arrange(desc(cum_cont))

fac_PA_Lebanon <- cum.rank.2032 %>% filter(id=="PA-Lebanon" & cum_cont <50) %>% arrange(desc(cum_cont))

fac_TX_Nueces <- cum.rank.2032 %>% filter(id=="TX-Nueces" & cum_cont <37) %>% arrange(desc(cum_cont))

fac_TX_Travis <- cum.rank.2032 %>% filter(id=="TX-Travis" & cum_cont <39) %>% arrange(desc(cum_cont))


fac_all <- as.vector(c(unique(fac_IL_Madison$uID), 
                       unique(fac_OH_Jefferson$uID), 
                       unique(fac_PA_Armstrong$uID), 
                       unique(fac_PA_Cambria$uID), 
                       unique(fac_PA_Lebanon$uID), 
                       unique(fac_TX_Nueces$uID), 
                       unique(fac_TX_Travis$uID)))

#by shutting down co2 emissions reduction


PP.units.yearly2022 [, uID := paste(Facility.ID..ORISPL., Unit.ID, sep = ".")]

fac_co2_red <- PP.units.yearly2022 %>% filter(uID %in% fac_all)

highlight_counties

# Combine State and County into one column to match the highlight_counties format
fac_co2_red$State_County <- paste(fac_co2_red$State, fac_co2_red$County, sep = "-")

# Filter the dataset for the highlighted counties

sum(fac_co2_red$CO2..short.tons.)

#population info on to be attainment counties


options(timeout=600) #timeout: Set maximum request time.

all.list <- list()
df.list <- list()

#Racial ethnicity variables
race_vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  HIPI = "B03002_007",
  Hispanic = "B03002_012"
)


states_2022_vec <- c ("AL", "GA", "IL", "IN", "KY", "NJ", "OH", "OK", "PA", "TN", "TX")

states_2032_vec <- c ("IL", "OH", "PA", "TX")


#creating loop to get demographic data upto county level

#2022

df.list <- list()


for (j in 1:length(states_2022_vec)) {
    
    #racial/ethnic population data
    race_block_groups <- get_acs(
      geography = "county",
      state = states_2022_vec[j],
      variables = race_vars,
      summary_var = "B03002_001",
      year =  2020,
      geometry=TRUE,
      survey = "acs5",
      output="wide" )  %>% st_transform(crs = p4s)
    
  
    df.list[[j]] <- race_block_groups
  }
  

pop.2022 <- do.call(rbind, df.list)


pop.2022<- pop.2022 %>% separate(NAME, c("county", "state" ), ",") 

names(pop.2022)[names(pop.2022) == 'GEOID'] <- 'fips'

pop.2022<- merge(pop.2022, counties.us.fips, by="fips")

pop.2022 <- pop.2022 %>% mutate(id= paste0(state_abbr,"-",name))

pop.2022<- pop.2022 %>% filter(id %in% coal_group_2022_vec)


#2032

df.list <- list()


for (j in 1:length(states_2032_vec)) {
  
  #racial/ethnic population data
  race_block_groups <- get_acs(
    geography = "county",
    state = states_2032_vec[j],
    variables = race_vars,
    summary_var = "B03002_001",
    year =  2020,
    geometry=TRUE,
    survey = "acs5",
    output="wide" )  %>% st_transform(crs = p4s)
  
  
  df.list[[j]] <- race_block_groups
}


pop.2032 <- do.call(rbind, df.list)


pop.2032<- pop.2032 %>% separate(NAME, c("county", "state" ), ",") 

names(pop.2032)[names(pop.2032) == 'GEOID'] <- 'fips'

pop.2032<- merge(pop.2032, counties.us.fips, by="fips")

pop.2032 <- pop.2032 %>% mutate(id= paste0(state_abbr,"-",name))

pop.2032<- pop.2032 %>% filter(id %in% coal_group_2032_vec)

sum(pop.2032$summary_est)

sum(pop.2022$summary_est)

# population plot
pop.2032

pop.2032.long <- melt(pop.2032, id.vars = c("id") ,
                      measure.vars = c("WhiteE", "BlackE", "NativeE", "AsianE", "HIPIE",
                                       "HispanicE"))

ggplot(pop.2032.long, aes(fill=variable, y=value, x=id)) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(expand=c(0,0), limits = c(0, NA)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         legend.title = element_blank(),
         axis.text.x = element_text(size=15, angle = 90),
         axis.text.y = element_text(size=15, angle = 0),
         title=element_text(size=20)) +
  labs(x= "", y="population %")


#EIA data to get facility shutdown and nameplate capacity

eia_208 <-read.csv("./data/3_1_Generator_Y2022.csv")

eia_208<- janitor::row_to_names(eia_208,1)


names(eia_208)[names(eia_208) == 'Plant Code'] <- 'Facility.ID..ORISPL.'

names(eia_208)[names(eia_208) == 'Plant Name'] <- 'Facility.Name'

names(eia_208)[names(eia_208) == 'Generator ID'] <- 'Unit.ID'

names(eia_208)[names(eia_208) == 'Nameplate Capacity (MW)'] <- 'nameplate_capacity_mw'

names(eia_208)[names(eia_208) == 'Planned Retirement Year'] <- 'Planned_Retirement_Year'


eia_208_id <- eia_208 %>% mutate(uID= paste0(Facility.ID..ORISPL.,".",Unit.ID),
                                 id= paste0(State,"-",County))


#capacity will be reduced if we shut down following facilities by 2032 to meet attainment

madison_id <-eia_208_id  %>% filter (uID %in% fac_IL_Madison$uID | Facility.Name %in% 
                                       fac_IL_Madison$Facility.Name ) %>% 
  mutate(nameplate_capacity_mw=as.numeric(nameplate_capacity_mw))

sum(madison_id$nameplate_capacity_mw)

madison_id %>% filter(Planned_Retirement_Year>2020 & Planned_Retirement_Year<=2032)

madison_retire_id <- unique(madison_id %>% filter(Planned_Retirement_Year>2020 & Planned_Retirement_Year<=2032))
madison_retire_id<- as.vector(madison_retire_id$uID)

jef_id <- eia_208_id  %>% filter (uID %in% fac_OH_Jefferson$uID | Facility.Name %in% 
                                        fac_OH_Jefferson$Facility.Name ) %>% 
  mutate(nameplate_capacity_mw=as.numeric(nameplate_capacity_mw), na.rm=T,
         Planned_Retirement_Year=as.numeric(Planned_Retirement_Year), na.rm=T)

sum(jef_id$nameplate_capacity_mw, na.rm=T)
jef_id %>% filter(Planned_Retirement_Year>2020 & Planned_Retirement_Year<=2032)

jef_retire_id <- unique(jef_id %>% filter(Planned_Retirement_Year>2020 & Planned_Retirement_Year<=2032))
jef_retire_id<- as.vector(jef_retire_id$uID)



arm_id <- eia_208_id  %>% filter (uID %in% fac_PA_Armstrong$uID | Facility.Name %in% 
                                    fac_PA_Armstrong$Facility.Name ) %>% 
  mutate(nameplate_capacity_mw=as.numeric(nameplate_capacity_mw), na.rm=T,
         Planned_Retirement_Year=as.numeric(Planned_Retirement_Year), na.rm=T)

sum(arm_id$nameplate_capacity_mw , na.rm=T)
arm_id %>% filter(Planned_Retirement_Year>2020 & Planned_Retirement_Year<=2032)

arm_retire_id <- unique(arm_id %>% filter(Planned_Retirement_Year>2020 & Planned_Retirement_Year<=2032))
arm_retire_id<- as.vector(arm_retire_id$uID)


cambria_id <- eia_208_id  %>% filter (uID %in% fac_PA_Cambria$uID | Facility.Name %in% 
                                        fac_PA_Cambria$Facility.Name ) %>% 
  mutate(nameplate_capacity_mw=as.numeric(nameplate_capacity_mw), na.rm=T,
         Planned_Retirement_Year=as.numeric(Planned_Retirement_Year), na.rm=T)

sum(cambria_id$nameplate_capacity_mw , na.rm=T)
cambria_id %>% filter(Planned_Retirement_Year>2020 & Planned_Retirement_Year<=2032)

cambria_retire_id <- unique(cambria_id %>% filter(Planned_Retirement_Year>2020 & Planned_Retirement_Year<=2032))
cambria_retire_id<- as.vector(cambria_retire_id$uID)



lebanon_id <- eia_208_id  %>% filter (uID %in% fac_PA_Lebanon$uID | Facility.Name %in% 
                                        fac_PA_Lebanon$Facility.Name ) %>% 
  mutate(nameplate_capacity_mw=as.numeric(nameplate_capacity_mw), na.rm=T,
         Planned_Retirement_Year=as.numeric(Planned_Retirement_Year), na.rm=T)

sum(lebanon_id$nameplate_capacity_mw , na.rm=T)
lebanon_id %>% filter(Planned_Retirement_Year>2020 & Planned_Retirement_Year<=2032)

lebanon_retire_id <- unique(lebanon_id %>% filter(Planned_Retirement_Year>2020 & Planned_Retirement_Year<=2032))
lebanon_retire_id<- as.vector(lebanon_retire_id$uID)



nueces_id <- eia_208_id  %>% filter (uID %in% fac_TX_Nueces$uID | Facility.Name %in% 
                                       fac_TX_Nueces$Facility.Name ) %>% 
  mutate(nameplate_capacity_mw=as.numeric(nameplate_capacity_mw), na.rm=T,
         Planned_Retirement_Year=as.numeric(Planned_Retirement_Year), na.rm=T)

sum(nueces_id$nameplate_capacity_mw , na.rm=T)
nueces_id %>% filter(Planned_Retirement_Year>2020 & Planned_Retirement_Year<=2032)

nueces_retire_id <- unique(nueces_id %>% filter(Planned_Retirement_Year>2020 & Planned_Retirement_Year<=2032))
nueces_retire_id<- as.vector(nueces_retire_id$uID)



travis_id <- eia_208_id  %>% filter (uID %in% fac_TX_Travis$uID | Facility.Name %in% 
                                       fac_TX_Travis$Facility.Name ) %>% 
  mutate(nameplate_capacity_mw=as.numeric(nameplate_capacity_mw), na.rm=T,
         Planned_Retirement_Year=as.numeric(Planned_Retirement_Year), na.rm=T)

sum(travis_id$nameplate_capacity_mw , na.rm=T)
travis_id %>% filter(Planned_Retirement_Year>2020 & Planned_Retirement_Year<=2032)


travis_retire_id <- unique(travis_id %>% filter(Planned_Retirement_Year>2020 & Planned_Retirement_Year<=2032))
travis_retire_id<- as.vector(travis_retire_id$uID)


all_retired_id <- c(travis_retire_id, nueces_retire_id, cambria_retire_id,
                    lebanon_retire_id, arm_retire_id, jef_retire_id, madison_retire_id)

all_attainment_counties_id <- as.vector(c(unique(travis_id$uID),
                                        unique(nueces_id$uID),
                                        unique(lebanon_id$uID),
                                        unique(cambria_id$uID),
                                        unique(arm_id$uID),
                                        unique(jef_id$uID),
                                        unique(madison_id$uID)))

#getting co2 emissions of these utilites

# =======Deaths attributable to each coal EGUs=================

death_file <- read.csv("./data/pm25_facility_state.csv")


madison_death<- death_file  %>% filter (FacID%in% fac_IL_Madison$Facility.ID..ORISPL. &
                                       state_facility %in% 
                                       fac_IL_Madison$State & year==2020 ) 

sum(madison_death$deaths_coef_1)
sum(madison_death$deaths_coef_2)
sum(madison_death$deaths_coef_3)


jef_death<- death_file  %>% filter (FacID %in% fac_OH_Jefferson$Facility.ID..ORISPL. &
                                        state_facility %in% 
                                    fac_OH_Jefferson$State & year==2020 ) 

sum(jef_death$deaths_coef_1)
sum(jef_death$deaths_coef_2)
sum(jef_death$deaths_coef_3)


arm_death<- death_file  %>% filter (FacID %in% fac_PA_Armstrong$Facility.ID..ORISPL. &
                                        state_facility %in% 
                                      fac_PA_Armstrong$State & year==2020 ) 

sum(arm_death$deaths_coef_1)
sum(arm_death$deaths_coef_2)
sum(arm_death$deaths_coef_3)


cambria_death<- death_file  %>% filter (FacID %in% fac_PA_Cambria$Facility.ID..ORISPL. &
                                        state_facility %in% 
                                          fac_PA_Cambria$State & year==2020 ) 
sum(cambria_death$deaths_coef_1)
sum(cambria_death$deaths_coef_2)
sum(cambria_death$deaths_coef_3)


lebanon_death <- death_file  %>% filter (FacID %in% fac_PA_Lebanon$Facility.ID..ORISPL. &
                                        state_facility %in% 
                                          fac_PA_Lebanon$State & year==2020 ) 
sum(lebanon_death$deaths_coef_1)
sum(lebanon_death$deaths_coef_2)
sum(lebanon_death$deaths_coef_3)


nueces_death <- death_file  %>% filter (FacID %in% fac_TX_Nueces$Facility.ID..ORISPL. &
                                        state_facility %in% 
                                          fac_TX_Nueces$State & year==2020 ) 
sum(nueces_death$deaths_coef_1)
sum(nueces_death$deaths_coef_2)
sum(nueces_death$deaths_coef_3)


travis_death <- death_file  %>% filter (FacID %in% fac_TX_Travis$Facility.ID..ORISPL. &
                                        state_facility %in% 
                                          fac_TX_Travis$State & year==2020 ) 
sum(travis_death$deaths_coef_1)
sum(travis_death$deaths_coef_2)
sum(travis_death$deaths_coef_3)



# Create the data frame
data <- data.frame(
  State = c("IL", "OH", "PA", "PA", "PA", "TX", "TX"),
  County = c("Madison", "Jefferson", "Armstrong", "Cambria", "Lebanon", "Nueces", "Travis"),
  EGUs_to_control = c(1, 49, 101, 7, 33, 10, 5),
  Population = c(264400, 66000, 65360, 131610, 140410, 362150, 1250884 ),
  Nameplate_Capacity = c(620, 22940, 49330, 2440, 18240, 4630, 7062),
  Deaths_avoided = c(11, 654, 995, 178, 550, 68, 170),
  Retirement_EGUs = c(1, 8, 23, 1, 6, 5, 5),
  coal_emission_reduction = c (9.91, 77.58, 83.00, 23.42, 49.32, 36.17, 38.32)
)

# Normalize data for visualization
data_long <- data %>%
  pivot_longer(cols = c(EGUs_to_control, Population, Nameplate_Capacity, Deaths_avoided, Retirement_EGUs,
                        coal_emission_reduction),
               names_to = "Metric", values_to = "Value")




data_long$County_State <- paste(data_long$County, data_long$State, sep = ", ")

ggplot(data_long, aes(x = County_State, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~ Metric, scales = "free_y", labeller = as_labeller(
    c(coal_emission_reduction = "coal SO2 emissions reduced (%)",
      Nameplate_Capacity = "nameplate capacity (MW)",
      Deaths_avoided = "# of deaths avoided",
      EGUs_to_control = "# of EGUs to control",
      Retirement_EGUs = "# of EGUs will retire",
      Population = "Population"))) +  # Update the labels for each panel
  
  # Use the viridis color palette (colorblind-friendly)
  scale_fill_viridis_d(option = "D") +
  
  # General theme and adjustments for readability
  theme_minimal() + theme_bw() +
  labs(title = "", x = "County", y = "Value") +
  
  # Adjust the x-axis text for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Remove the legend
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # White background
        plot.background = element_rect(fill = "white", color = NA)) +  # White background for plot
  
  # Remove the titles for better layout
  labs(title = NULL, x = NULL, y = NULL)

ggsave(paste0("benefits.png"), path = "./plots/", width=8, height=5, units="in")


plt<-data_long %>%
  mutate(Metric = factor(Metric,
                           labels = c(bquote("coal"~SO[2]~"emissions"~"reduced"~"(%)"),
                                      bquote("#"~"of"~"deaths"~"avoided"),
                                      bquote("#"~"of"~"EGUs"~"to"~"control"),
                                      bquote("nameplate~capacity~(MW)"),
                                      bquote("#"~"of"~"population"~"exposure"),
                                       bquote("#"~"of"~"EGUs"~"will"~"retire")  )))  %>%
  ggplot( aes(x = County_State, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "D") +
  
  # General theme and adjustments for readability
  theme_minimal() + theme_bw() +
  labs(title = "", x = "County", y = "Value") +
  
  # Adjust the x-axis text for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",  # Remove the legend
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # White background
        plot.background = element_rect(fill = "white", color = NA)) +  # White background for plot
  
  # Remove the titles for better layout
  labs(title = NULL, x = NULL, y = NULL) +
  facet_wrap(~ Metric, nrow = 2, scales = "free_y", labeller = label_parsed) 

ggsave(paste0("benefits.png"), path = "./plots/", width=8, height=5, units="in")


# Correlation analysis
cor_matrix <- data %>%
  dplyr::select(EGUs_to_control, Population, Nameplate_Capacity, Deaths_avoided, Retirement_EGUs) %>%
  cor()

# Print correlation matrix
print(cor_matrix)


# Melt the correlation matrix into long format
cor_data <- melt(cor_matrix)
# Create the heatmap plot
ggplot(data = cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  labs(title = "Correlation Matrix of Key Metrics",
       x = "Metrics", y = "Metrics")



# For SI: comparison between SO2 sensitivities and observed sulfate; and SO2 sensitivities and total PM

# ============DDM data read==========

load(file=paste0("./data/egus_so2_pm2.5/egus_so2_pm25.RData"))

ddm_dt <- do.call(rbind,datalist)

ddm_geolist <- ddm_dt %>% dplyr::select(ID, geometry)

# ddm_geolist_distinct <- ddm_geolist %>% distinct()

# save(ddm_geolist_distinct, file=paste0("./data/ddm_geolist_distinct.RData"))

load("./data/ddm_geolist_distinct.RData")



dt_long <- dt_long %>%
  mutate(group_so4 = case_when(
    grepl("ptegu_12.45", sens) ~ "12.45",
    grepl("ptegu_1", sens) ~ "1",
    grepl("ptegu_0", sens) ~ "0",
    TRUE ~ NA_character_
  ))

# Map numeric codes to years and ensure that all required years are factors
dt_long <- dt_long %>%
  mutate(
    year = case_when(
      group_so4 == "0" ~ "2032",
      group_so4 == "1" ~ "2020",
      group_so4 == "12.45" ~ "2005",
      TRUE ~ as.character(group_so4)
    )
  )

# Ensure the 'year' column is a factor with all required levels
dt_long$year <- factor(dt_long$year, levels = c("2005", "2010", "2015", "2020", 
                                                "2025", "2030", "2032"))


# Create dummy data for the years 2010 and 2015
dummy_data<- data.frame(
  variable = rep(unique(dt_long$variable), each = 4),
  group_so4 = NA,
  value = NA,
  sens = NA,
  year = factor(rep(c("2010", "2015", "2025", "2030"), length(unique(dt_long$variable))), 
                levels = c("2005", "2010", "2015", "2020", "2025", "2030", "2032"))
)


# Combine the original data with the dummy data
dt_long_with_gaps <- bind_rows(dt_long, dummy_data)


#adding observation data

# aqs.data <- read.fst("/Users/munshirasel/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/gmu_one_drive_rasel/R/spatial-temporal-PM2.5-chemicals/data/aqs.1999.2020.monthly.fst")
# 
# write.csv2(aqs.data, "./data/aqs_data.csv")

aqs.data <- read.csv2("./data/aqs_data.csv")


aqs.data_2005_2020 <- aqs.data %>% filter (year %in% c (2005, 2010, 2015, 2020))

# write.csv2(aqs.data, "./data/aqs_data_modified.csv")

aqs.data_2005_2020_adj1 <- aqs.data_2005_2020 %>% filter(chemicals %in% c("Sulfate",
                                                                          "Nitrate", "Ammonium")  )

aqs.data_2005_2020_pm2.5_adj2 <- aqs.data_2005_2020 %>% filter(chemicals=="PM2.5") %>% 
  filter(mean <20)

aqs.data_2005_2020<- rbind(aqs.data_2005_2020_adj1, aqs.data_2005_2020_pm2.5_adj2 )

aqs.data_2005_2020 <- aqs.data_2005_2020 %>% dplyr::select(chemicals,
                                                           mean, year, 
                                                           State.Name, County.Name)

# Rename the chemicals
chemical_rename_dict <- c(
  "PM2.5" = "PM25",
  "Sulfate" = "PM25_SO4",
  "Nitrate" = "PM25_NO3",
  "Ammonium" = "PM25_NH4"
)

aqs.data_2005_2020<- aqs.data_2005_2020 %>%
  mutate(chemicals = recode(chemicals, !!!chemical_rename_dict))


aqs_dt <- as.data.frame(aqs.data_2005_2020) %>% dplyr::select(chemicals, mean, year) %>%
  mutate(
    ID = row_number(),
    variable = chemicals,
    value = mean,
    sens = "EPA_AQS_Obs",
    group_so4 = NA
  ) %>%
  dplyr::select(ID, variable, value, sens, group_so4, year) %>% mutate(year=as.factor(year))

combined_data <- bind_rows(dt_long_with_gaps, aqs_dt) 

combined_data_attain <- combined_data %>% filter (sens %in% c("so2_ptegu_1_nox_1",
                                                              "EPA_AQS_Obs"))

# Function to calculate slopes separately for EPA_AQS_Obs and other data
calculate_slope_separately <- function(data, var) {
  # Filter data for the variable
  data_filtered <- data %>% filter(variable == var, year %in% c("2005", "2020", "2032"))
  
  # Calculate slope for other data
  slope1 <- lm(value ~ as.numeric(as.character(year)), data = data_filtered %>% filter(year %in% c("2005", "2020"), sens != "EPA_AQS_Obs"))
  slope1_value <- coef(slope1)[2]
  
  slope2 <- lm(value ~ as.numeric(as.character(year)), data = data_filtered %>% filter(year %in% c("2020", "2032"), sens != "EPA_AQS_Obs"))
  slope2_value <- coef(slope2)[2]
  
  # Calculate slope for EPA_AQS_Obs data
  slope1_epa <- lm(value ~ as.numeric(as.character(year)), data = data_filtered %>% filter(year %in% c("2005", "2020"), sens == "EPA_AQS_Obs"))
  slope1_value_epa <- coef(slope1_epa)[2]
  
  slope2_epa <- lm(value ~ as.numeric(as.character(year)), data = data_filtered %>% filter(year %in% c("2020", "2032"), sens == "EPA_AQS_Obs"))
  slope2_value_epa <- coef(slope2_epa)[2]
  
  list(
    slope1 = slope1_value, 
    slope2 = slope2_value, 
    slope1_epa = slope1_value_epa, 
    slope2_epa = slope2_value_epa
  )
}


# List of variables to plot
variables_to_plot <-  c("PM25",     "PM25_SO4", "PM25_NO3", "PM25_NH4")

var <-"PM25_SO4"

slopes <- calculate_slope_separately(combined_data_attain, var)

y_label <- bquote(.(var) ~ mu*g/m^3)


ggplot(combined_data_attain %>% 
         filter(variable == var), aes(x = year, y = value, color = sens)) +
  geom_boxplot() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8, 0.7),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 15, angle = 0),
    axis.text.x = element_text(size = 15, angle = 0),
    title = element_text(size = 20)
  ) +
  labs(
    x = "",
    y = "",
    title = var)


combined_data_attain <- combined_data_attain #%>%  filter(year %in% c(2020))

combined_data_attain<- combined_data_attain %>% filter(variable %in% c("PM25",
                                                                       "PM25_NH4",
                                                                       "PM25_NO3",
                                                                       "PM25_SO4"))

ggplot(combined_data_attain, aes(x = variable, y = value, fill = sens)) +
  geom_boxplot() + labs (title="", x= "", y= "") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8, 0.7),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 15, angle = 0),
    axis.text.x = element_text(size = 15, angle = 0),
    title = element_text(size = 20)) 
  
combined_data_attain %>% filter(sens=="EPA_AQS_Obs") %>% 
  ggplot( aes(x = year, y = value, color = variable)) +
  geom_boxplot() + labs (title="", x= "", y= "") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9, 0.8),
    axis.title.y = element_text(size = 25),
    axis.text.y = element_text(size = 15, angle = 0),
    axis.text.x = element_text(size = 15, angle = 0),
    title = element_text(size = 20)) 


setDT(combined_data_attain)[ , list(mean_gr = mean(value)) , by = .(sens, variable)]


summary(ddm_yr_avg_sf)
