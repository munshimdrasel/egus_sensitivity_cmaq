rm(list = ls())

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

#> Set location where you have downloaded the CMAQ netcdf file.  
setwd("/Users/munshirasel/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/gmu_one_drive_rasel/R/egus_sensitivity_cmaq")

# =====following is sample code======ran on hopper cluster=========
#on hopper
# setwd ("/projects/HAQ_LAB/mrasel/R/egus_sensitivity_cmaq")

month.vec <- c("07")
dates.vec <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
               "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
               "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31")

# dates.vec <- c("01", "02")
# months <- c("01", "02")

# cmaq netcdf file reading
# variable_list <- list()
# datalist <- list()
# final_list <- list()
# geo_list = list()
# 
# for (j in 1:length(month.vec)) {
#   for (i in 1: length(dates.vec)) {
#     ## define file name and open ncdf file
#     # (anthropogenic so2 emissions from CEDS data)
#     cctm.file <- paste0("./data/bruteforce/so2_12.45_nox_2.8_cmaq_1/CCTM_AELMO_v54_DDM3D_gcc_AQF5X_2020",month.vec[j],"", dates.vec[i],".nc")
#     
#     # cctm.file <- './data/combine_egus_so2/HR2DAY_ES2_ASENS_v54_DDM3D_gcc_AQF5X_2020.nc'
#     
#     #coordinate reference system projection string for spatial data
#     p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"
#     
#     #> Open the CCTM file.
#     cctm.in <- nc_open(cctm.file)
#     
#     #> Print information about a netCDF file, including the variables and dimensions it contains.
#     # print(cctm.in)
#     
#     # names of the variables
#     # names( cctm.in$var)
#     # names( cctm.in$dim)
#     
#     
#     
#     #> Create a list of all model variables in cctm.file.
#     #> CMAQ netcdf files are formatted following I/O API conventions (https://www.cmascenter.org/ioapi/).  I/O API is a data storage library designed specifically for CMAQ data.
#     #> A variable called “TFLAG” will always be the first variable listed in a CMAQ I/O API file, so remove the first element of the list.
#     all.mod.names <- unlist(lapply(cctm.in$var, function(var)var$name))[-1]
#     
#     #> Create a list units for all the model variables in the cctm.file.
#     #> A variable called “TFLAG” will always be the first variable listed in an I/O API file, so remove the first element of the list.
#     #> Use gsub to strip out extra spaces.
#     all.mod.units <- gsub(" ","",unlist(lapply(cctm.in$var, function(var)var$units))[-1])
#     
#     #> Pull out the time steps and the grid coordinates associated with the data.
#     #> These functions from the M3 library are wrappers for functions in the ncdf4 package.
#     
#     # M3 package is no longer availalble
#     
#     ##read files with helper M3 functions
#     source("./R/m3_package_functions.R")
#     
#     date.seq <- get.datetime.seq(cctm.file)
#     format.date.seq <- format.Date(date.seq,"%m/%d/%Y")
#     
#     #> Lambert projected coordinates of the grid cell CENTERS (unit=km).
#     #> These are the unique x, y coordinates of the grid cell centers -- NOT the coordinates for every grid cell, since the data are on a regular grid.
#     #> You can also use the get.coord.for.dimension() function to extract the grid cell edges by changing the “position” argument.
#     x.proj.coord <- get.coord.for.dimension(cctm.file,"col")$coords
#     length(x.proj.coord)
#     #[1] 442
#     y.proj.coord <- get.coord.for.dimension(cctm.file,"row")$coords
#     length(y.proj.coord)
#     #[1] 265
#     
#     #> Also get the grid cell centers of all of the grid cell with units=meters.  We will use this later when we convert the data to an object raster.
#     xy.proj.coord.meters <- get.matrix.all.grid.cell.ctrs(cctm.file,units="m")$coord
#     dim(xy.proj.coord.meters)
#     #[1] 117130      2
#     
#     #on hopper cluster run following two lines to load proj4 library; or ask hhopper people to install this library on  r/4.1.2-dx version
#     # dyn.load("/opt/sw/spack/apps/linux-centos8-cascadelake/gcc-9.3.0/proj-7.1.0-7x/lib/libproj.so.19")
#     # library(proj4, lib.loc = '/home/mrasel/R/x86_64-pc-linux-gnu-library/4.0/')
#     
#     #> Convert lambert coordinates for grid cell centers to long/lat
#     long.lat.coord <- as.data.frame(project.M3.to.lonlat(xy.proj.coord.meters[,1],xy.proj.coord.meters[,2],cctm.file,units="m")$coord)
#     
#     #> Projection information character string that can be used by the R package rgdal.
#     proj.string <- get.proj.info.M3(cctm.file)
#     # proj.string
#     #[1] "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"
#     
#     #> US, Canada, Mexico map lines in Lambert projected coordinates.
#     # map.lines <- get.map.lines.M3.proj(cctm.file,database="canusamex")$coords
#     
#     #> Extract PM25_AVG (daily 24hr LST averaged PM2.5 concentrations for 1/1/2017 - 12/31/2017).
#     
#     var.vec <- c( "PM25",
#                   "PM25_SO4",
#                   "PM25_NO3",
#                   "PM25_NH4",
#                   "PM25_OC" ,
#                   "PM25_EC")
#     
#     for (k in 1: length (var.vec)) {
#       mod.name <- var.vec[k]
#       mod.unit <- all.mod.units[all.mod.names==mod.name]
#       mod.array <- ncvar_get(cctm.in,var=mod.name)
#       dim (mod.array)
#       #[1] 442 265 31
#       
#       #> Create annual average.
#       mod.annual.avg <- apply(mod.array,c(1,2),mean)
#       # dim(mod.annual.avg)
#       #[1] 442 265
# 
#     
#       #> Create raster object using projection information extracted from the I/O API file.
#       xyz <- data.frame(x=xy.proj.coord.meters[,1],y=xy.proj.coord.meters[,2],z=matrix(mod.annual.avg))
#       mod.raster <- rasterFromXYZ(xyz,crs=proj.string)
#       
#       # plot(mod.raster)
#       
#       #coordinate reference system projection string for spatial data
#       # p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"
#       
#       mod.sf <- rasterToPolygons(mod.raster)
#       
#       r_poly_transformed <- spTransform(mod.sf, CRS(p4s))
#       
#       # Convert the transformed polygons to an sf object
#       sf_object <- st_as_sf(r_poly_transformed)
#       
#       # ggplot( sf_object) + geom_sf( aes( fill = z,  geometry = geometry),
#       #                        color = NA) +scale_fill_viridis_c( option="plasma")
#       
#       
#       us.sf <-  USAboundaries::us_states()  %>%  filter(!stusps %in% c("PR", "HI", "AK") ) %>%
#         st_transform( crs = p4s)
#       
#       # combine into single geom for easy plotting
#       # tx_allblocks <- st_union( us.sf)
#       
#       
#       # sf_object_crop <-
#       #   st_crop( sf_object,
#       #            tx_allblocks)
#       sf_object_intersect <-
#         st_intersection( sf_object,us.sf)
#       
#       sf_object_intersect$year <- 2020
#       sf_object_intersect$month <- month.vec[j]
#       sf_object_intersect$day <- dates.vec[i]
#       
#       
#       sf_object_intersect <- sf_object_intersect %>% dplyr::select(year, month, day, z, geometry) %>%
#         mutate( ID= 1:nrow(sf_object_intersect),
#                 variable= var.vec[k])
#       
#       variable_list[[k]] <- sf_object_intersect
#       # ggplot( ) +
#       #   geom_sf( data = us.sf, size = 1) +
#       #   geom_sf(data=sf_object_intersect, aes( fill = z,  geometry = geometry), color = NA) +
#       #   scale_fill_viridis_c( option="plasma")
#       message("***** I am done for variable ***** ", var.vec[k])
#     }
#     
#     df <- do.call(rbind,variable_list)
#     
#     df_geo <- df %>% dplyr::select(ID, geometry) %>% distinct()
#     
#     df_drop_geo <- df %>% st_drop_geometry()
#     
#     datalist[[i]] <-  df_drop_geo
#     
#     geo_list [[i]] <- df_geo
#     
#     message("***** I am done for day ***** ", dates.vec[i])
#     
#     # save(datalist, file=paste0("./data/bruteforce_output/cmaq_1_",dates.vec[i],".RData"))
#   }
#   
#   final_list[[j]] <- do.call(rbind,datalist)
#   save(final_list, file=paste0("./data/bruteforce_output/cmaq_1/cmaq_1.RData"))
#   save(geo_list, file=paste0("./data/bruteforce_output/cmaq_1/geolist.RData"))
#   
# }


#loading daily gridded data
# load (file=paste0("./data/bruteforce_output/cmaq_1.RData"))
# 
# load (file=paste0("./data/bruteforce_output/geolist.RData"))
# 
# dt <- do.call(rbind,datalist)
# 
# sf_geo <- do.call(rbind,geo_list)



#taking mean of month

#plotting US Map for all facility 
# dt_mon_avg<- setDT(dt)[, .(value = mean(z, na.rm=TRUE)),
#                                                   by = .(year, month, ID, variable)]
# 
# dt_wide <- spread(dt_mon_avg, variable, value)
# 
# data_merge <- merge(dt_wide, sf_geo, by= c ("ID")) %>% st_as_sf()

# ggplot( ) +
#   geom_sf( data = us.sf, size = 1) +
#   geom_sf(data=data_merge, aes( fill = PM25 ,  geometry = geometry), color = NA) +
#   scale_fill_viridis_c( option="plasma", name = (paste( "PM2.5")),
#                         limit=c(0,12), oob = scales::squish) +
#   scale_x_continuous( expand = c( 0, 0)) +
#   scale_y_continuous( expand = c( 0, 0)) +
#   theme_bw() +
#   theme( axis.text = element_blank(),
#          axis.title = element_blank(),
#          axis.ticks = element_blank(),
#          legend.direction = 'horizontal',
#          legend.background = element_rect(fill = NA),
#          legend.key = element_rect(fill = NA, color = NA),
#          legend.position = c(0.15,0.1),
#          legend.text = element_text( size = 15),
#          legend.title = element_text( size = 25),
#          panel.background = element_rect( fill = 'white'),
#          panel.grid = element_blank(),
#          panel.border = element_blank()) +
#   guides(fill = guide_colorbar( label.position = "bottom", 
#                                 title.position = "top", title.vjust = 0, title.hjust = 0,
#                                 label.vjust= -0.6,
#                                 # draw border around the legend
#                                 frame.colour = "black",
#                                 barwidth = 10,
#                                 barheight = 1))
# 
# 
# dt_mon_avg %>% filter(variable=="PM25") %>% 
#   ggplot(aes( variable, value, group= year)) + geom_boxplot() + 
#   scale_y_continuous(expand=c(0,0), limits = c(0, NA))  + theme_bw() +
#   theme( axis.title.y = element_text(size=25),
#          axis.text = element_text(size=20),
#          title=element_text(size=20)) 


# ==============Reading CMAQ files and combining data===========
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"
us.sf <-  USAboundaries::us_states()  %>%  filter(!stusps %in% c("PR", "HI", "AK") ) %>%
          st_transform( crs = p4s)

#cmaq_1 so2_ptegu_12.45_nox_2.8
# loading daily gridded data
load (file=paste0("./data/bruteforce_output/cmaq_1/cmaq_1.RData"))

load (file=paste0("./data/bruteforce_output/cmaq_1/geolist.RData"))

dt_cmaq_1 <- do.call(rbind,final_list)

geo_list <- data.frame(geo_list[[1]])

mon_avg_cmaq_1<- setDT(dt_cmaq_1)[, .(value = mean(z, na.rm=TRUE)),  by = .(ID, variable)] %>% mutate(sens="so2_ptegu_12.45_nox_2.8")

dt_wide_cmaq_1<- spread(mon_avg_cmaq_1, variable, value) 

summary(dt_wide_cmaq_1)

#cmaq_2 so2_ptegu_1_nox_0
load (file=paste0("./data/bruteforce_output/cmaq_2/cmaq_2.RData"))

dt_cmaq_2 <- do.call(rbind,final_list)

mon_avg_cmaq_2<- setDT(dt_cmaq_2)[, .(value = mean(z, na.rm=TRUE)),  by = .( ID, variable)] %>% mutate(sens="so2_ptegu_1_nox_0")

dt_wide_cmaq_2<- spread(mon_avg_cmaq_2, variable, value) 

#CMAQ_3 so2_ptegu_0_nox_0
load (file=paste0("./data/bruteforce_output/cmaq_3/cmaq_3.RData"))

dt_cmaq_3 <- do.call(rbind,final_list)

mon_avg_cmaq_3<- setDT(dt_cmaq_3)[, .(value = mean(z, na.rm=TRUE)),  by = .( ID, variable)] %>% mutate(sens="so2_ptegu_0_nox_0")

dt_wide_cmaq_3<- spread(mon_avg_cmaq_3, variable, value) 

#CMAQ_4 so2_ptegu_12.45_nox_0
load (file=paste0("./data/bruteforce_output/cmaq_4/cmaq_4.RData"))

dt_cmaq_4 <- do.call(rbind,final_list)

mon_avg_cmaq_4<- setDT(dt_cmaq_4)[, .(value = mean(z, na.rm=TRUE)),  by = .( ID, variable)]  %>% mutate(sens="so2_ptegu_12.45_nox_0")

dt_wide_cmaq_4<- spread(mon_avg_cmaq_4, variable, value)

#CMAQ_5 so2_ptegu_0_nox_1
load (file=paste0("./data/bruteforce_output/cmaq_5/cmaq_5.RData"))

dt_cmaq_5 <- do.call(rbind,final_list)

mon_avg_cmaq_5<- setDT(dt_cmaq_5)[, .(value = mean(z, na.rm=TRUE)),  by = .(ID, variable)] %>% mutate(sens="so2_ptegu_0_nox_1")

dt_wide_cmaq_5<- spread(mon_avg_cmaq_5, variable, value)


#CMAQ_base so2_ptegu_1_nox_1
load (file=paste0("./data/bruteforce_output/cmaq_base/cmaq_base.RData"))

dt_cmaq_base <- do.call(rbind,final_list)

mon_avg_cmaq_base<- setDT(dt_cmaq_base)[, .(value = mean(z, na.rm=TRUE)),  
                                        by = .( ID, variable)] %>% mutate(sens="so2_ptegu_1_nox_1")

dt_wide_cmaq_base<- spread(mon_avg_cmaq_base, variable, value)



#plotting US Map for all facility 

dt_long <- rbind(mon_avg_cmaq_1, mon_avg_cmaq_2, mon_avg_cmaq_3,
                 mon_avg_cmaq_4, mon_avg_cmaq_5, mon_avg_cmaq_base)

dt_wide <- rbind(dt_wide_cmaq_1, dt_wide_cmaq_2, dt_wide_cmaq_3,
                 dt_wide_cmaq_4, dt_wide_cmaq_5, dt_wide_cmaq_base)

data_merge <- merge(dt_wide, st_as_sf(geo_list), by= c ("ID")) %>% st_as_sf()

data_merge %>% group_by(sens) %>% summarize(min = min(PM25_SO4),
                                              q1 = quantile(PM25_SO4, 0.25),
                                              median = median(PM25_SO4),
                                              mean = mean(PM25_SO4),
                                              q3 = quantile(PM25_SO4, 0.75),
                                              max = max(PM25_SO4))

#domain of model
ggplot() +
  geom_sf(data = data_merge %>% filter(sens == "so2_ptegu_12.45_nox_2.8"),
          aes(geometry = geometry), fill = "lightblue", alpha = 0.7, color = "black", show.legend = FALSE) +
  
  # Set proper limits for x and y axis with slight expansion
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),  # Remove axis labels
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank(), # Remove axis ticks
    panel.grid.major = element_line(color = "gray90", size = 0.2),  # Fine, subtle grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.5), # Black border for the plot
    panel.background = element_rect(fill = 'white'), # White background
    legend.position = "none"  # Remove legend
  ) 

ggsave(paste0("us_domain_cmaq.png"), path = "./plots/", width=9, height=5, units="in")

dt_long %>% filter (variable=="PM25") %>% ggplot (aes(x = reorder(sens, -value), y = value))+
   geom_boxplot() + 
  scale_y_continuous(expand=c(0,0), limits = c(0, NA)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=15, angle = -45),
         title=element_text(size=20)) +
  labs(x= "", y="PM2.5")

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

model_2005 <- dt_long %>% filter (sens %in% c("so2_ptegu_12.45_nox_0" ) & 
                                    variable %in% c("PM25") )

summary(model_2005)
model_2020 <- dt_long %>% filter (sens %in% c("so2_ptegu_1_nox_0" ) & variable %in% c("PM25") )

summary(model_2020)

observed_2005 <- aqs_dt %>% filter ( variable %in% c("PM25") & year==2005)

summary(observed_2005)

observed_2020 <- aqs_dt %>% filter ( variable %in% c("PM25") & year==2020)

summary(observed_2020)


combined_data_EPA <- combined_data %>% filter (sens %in% c("EPA_AQS_Obs" ))


#observation contributions

combined_data_EPA <- combined_data_EPA %>% filter(!variable %in% c("PM25_EC", "PM25_OC") )


# Filter PM25 values
pm25_values <- combined_data_EPA %>% filter(variable == "PM25") %>% dplyr::select(year, value) 


# Calculate mean values for each variable and year
data_mean <- setDT(combined_data_EPA)[, .(mean_value = mean(value)), by = c("variable", "year")]


# Filter PM25 mean values
pm25_means <- data_mean %>% filter(variable == "PM25") %>% dplyr::select(year, mean_value)

# Join PM25 mean values to get total PM25 for each year
data_joined <- data_mean %>%
  inner_join(pm25_means, by = "year", suffix = c("", "_PM25")) %>%
  mutate(percentage = (mean_value / mean_value_PM25) * 100)

# Calculate "Others" category
data_others <- data_joined %>%
  filter(variable != "PM25") %>%
  group_by(year) %>%
  summarize(percentage = 100 - sum(percentage), .groups = 'drop') %>%
  mutate(variable = "Others")

# Combine with the original data
data_final <- bind_rows(data_joined, data_others)


# Arrange data to ensure SO4 is on top
data_final <- data_final %>%
  arrange(variable == "PM25_SO4", variable)

# Define custom x-axis breaks
x_breaks <- c("2020", "2015", "2010", "2005")

# Plotting with reversed years and custom x-axis breaks
data_final %>% 
  filter(!variable %in% "PM25") %>% 
  ggplot( aes(x = year, y = percentage, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_discrete(limits = x_breaks) +
  labs(title = "Obervational percentage contribution species to total PM25",
       x = "", y = "") +
  theme_minimal() + theme_bw() +
  scale_fill_discrete(name = "Species") +
  theme(legend.position = "bottom") 
  


sens <- combined_data
#sensitivity contributions
combined_data <- sens %>% filter (sens %in% c("so2_ptegu_1_nox_0" , 
                                                       "so2_ptegu_12.45_nox_0",
                                                       "so2_ptegu_0_nox_0"))

combined_data <- combined_data %>% filter(!variable %in% c("PM25_EC", "PM25_OC") )


# Filter PM25 values
# pm25_values <- combined_data %>% filter(variable == "PM25") %>% dplyr::select(year, value) 


# Calculate mean values for each variable and year
data_mean <- setDT(combined_data)[, .(mean_value = mean(value)), by = c("variable", "year", "group_so4")]



data_wide <- dcast(data_mean, group_so4 ~ variable )

data_wide<- data_wide %>% mutate(PM25_Others= PM25- sum(PM25_NH4 +   PM25_NO3 + PM25_SO4) )

# Reshape the data to long format excluding PM25
data_long <- data_wide %>%
  pivot_longer(cols = c( PM25_NH4, PM25_NO3, PM25_SO4, PM25_Others), 
               names_to = "variable", values_to = "value")

# Load the libraries
library(sf)
library(ggplot2)
library(dplyr)
library(maps)
library(tidyr)
library(data.table)
library(readr)
library(viridis)

# Retrieve US state boundaries
us_states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

# Add state names to the dataset
us_states <- us_states %>%
  mutate(State.Name = tolower(ID))

# Create a mapping of states to EPA regions
epa_regions <- data.frame(
  State.Name = c("connecticut", "maine", "massachusetts", "new hampshire", "rhode island", "vermont",
                 "new jersey", "new york", "puerto rico", "virgin islands",
                 "delaware", "district of columbia", "maryland", "pennsylvania", "virginia", "west virginia",
                 "alabama", "florida", "georgia", "kentucky", "mississippi", "north carolina", "south carolina", "tennessee",
                 "illinois", "indiana", "michigan", "minnesota", "ohio", "wisconsin",
                 "arkansas", "louisiana", "new mexico", "oklahoma", "texas",
                 "iowa", "kansas", "missouri", "nebraska",
                 "colorado", "montana", "north dakota", "south dakota", "utah", "wyoming",
                 "arizona", "california", "hawaii", "nevada", "pacific islands",
                 "alaska", "idaho", "oregon", "washington"),
  EPA_REGION = c(rep(1, 6), rep(2, 4), rep(3, 6), rep(4, 8), rep(5, 6), rep(6, 5), rep(7, 4),
                 rep(8, 6), rep(9, 5), rep(10, 4))
)

# Merge EPA region information with US states
us_states <- us_states %>%
  left_join(epa_regions, by = "State.Name")

# Coordinate reference system projection string for spatial data
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

# Plot the map with EPA regions and region numbers
ggplot(data = us_states) +
  geom_sf(aes(fill = factor(EPA_REGION))) +
  scale_fill_viridis_d(name = "EPA Region") +
  theme_void() +
  labs(title = "EPA Regions of the United States") +
  theme(legend.position = "bottom")

us_states <- us_states %>% st_transform(crs = p4s)

new_geo <- geo_list %>% st_as_sf() %>% st_centroid()

# Read your spatial data
sf_epa_region <- merge(combined_data, new_geo, by = "ID") %>% st_as_sf()

# Ensure both datasets have the same CRS
region_data <- st_join(us_states, sf_epa_region, join = st_intersects) %>% as.data.frame()

region_data$emission_scale <- as.numeric(region_data$group_so4)
# Calculate mean values for each variable and emission_scale
data_mean_region <- region_data %>% setDT() %>%
  .[, .(mean_value = mean(value, na.rm = TRUE)), by = .(variable, emission_scale, EPA_REGION)]

write_csv(data_mean_region, "./data/reg_data.csv")

# Load the data from the provided CSV file
data <- data_mean_region# read_csv('./data/reg_data.csv')

# Add missing emission scales if necessary
scales_to_add <- c(12.45, 1, 0)
variables <- unique(data$variable)
regions <- unique(data$EPA_REGION)

# Create rows with NA values for the new scales and combine with original data
new_rows <- expand.grid(variable = variables, emission_scale = scales_to_add, EPA_REGION = regions, mean_value = NA)
data <- rbind(data, new_rows)

# Convert emission_scale to numeric and order levels as specified
data$emission_scale <- as.numeric(data$emission_scale)

# Calculate slope between 12.45 and 0 for each variable and region
data_slope_0 <- data %>%
  filter(emission_scale %in% c(12.45, 0))

data_slope_0 <-  setDT(data_slope_0)[, .(
  mean_value_12.45 = mean(mean_value[emission_scale == 12.45], na.rm = TRUE),
  mean_value_0 = mean(mean_value[emission_scale == 0], na.rm = TRUE)), 
  by = c("variable", "EPA_REGION")] %>%
  mutate( slope_0 = (mean_value_0 - mean_value_12.45) / (0 - 12.45) ) %>%
  mutate( mean_value_1_from_0 = mean_value_12.45 + slope_0 * (1 - 12.45) )

# Calculate slope between 12.45 and 1 for each variable and region
data_slope_1 <- data %>%
  filter(emission_scale %in% c(12.45, 1))

data_slope_1 <-  setDT(data_slope_1)[, .(
  mean_value_12.45 = mean(mean_value[emission_scale == 12.45], na.rm = TRUE),
  mean_value_1 = mean(mean_value[emission_scale == 1], na.rm = TRUE)), 
  by = c("variable", "EPA_REGION")] %>%
  mutate( slope_1 = (mean_value_1 - mean_value_12.45) / (1 - 12.45) )

# Merge data_slope_0 and data_slope_1 by variable and EPA_REGION
data_merged <- merge(data_slope_0, data_slope_1, by = c("variable", "EPA_REGION"), 
                     suffixes = c("_from_0", "_from_1"))

# Calculate delta of mean_value_1 from data_slope_0 and data_slope_1
data_merged <- data_merged %>%
  mutate(delta_1 = mean_value_1 - mean_value_1_from_0,
         delta_1_pct= (mean_value_1 - mean_value_1_from_0)*100/mean_value_1)

data_merged %>%  filter(variable == "PM25_SO4")

data_merged %>%  filter(variable == "PM25_NO3")

data_merged %>%  filter(variable == "PM25_NH4")



summary_by_variable <- data_merged %>%
  group_by(variable) %>%
  summarise(
    mean_mean_value_0 = mean(mean_value_0, na.rm = TRUE),
    sd_mean_value_0 = sd(mean_value_0, na.rm = TRUE),
    min_mean_value_0 = min(mean_value_0, na.rm = TRUE),
    max_mean_value_0 = max(mean_value_0, na.rm = TRUE),
    mean_slope_0 = mean(slope_0, na.rm = TRUE),
    sd_slope_0 = sd(slope_0, na.rm = TRUE),
    mean_delta_1 = mean(delta_1, na.rm = TRUE),
    sd_delta_1 = sd(delta_1, na.rm = TRUE),
    mean_delta_1_pct = mean(delta_1_pct, na.rm = TRUE),
    sd_delta_1_pct = sd(delta_1_pct, na.rm = TRUE)
  )

# Plot using ggplot2 with facet_wrap for different EPA regions
p <- ggplot(data, aes(x = emission_scale, y = mean_value, group = variable, color = variable)) +
  geom_point(size = 2) +
  geom_line(size = 1, na.rm = TRUE) +
  geom_segment(data = data_slope_0, aes(x = 12.45, xend = 0, y = mean_value_12.45, yend = mean_value_0), linetype = "dotdash", size = 0.5) +
  geom_segment(data = data_slope_1, aes(x = 12.45, xend = 1, y = mean_value_12.45, yend = mean_value_1), size = 0.5) +
  geom_segment(data = data_slope_0, aes(x = 12.45, xend = 1, y = mean_value_12.45, yend = mean_value_1_from_0), linetype = "longdash", size = 0.5) +
  geom_point(data = data_merged, aes(x = 1, y = mean_value_1_from_0), color = "red", size = 2)+
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +
  labs(title = expression(paste("")),
       x = expression(paste("Scale ", SO[2]," Emissions with 0 NOx emissions")) ,
       y = expression(paste( "Concentration, "," ", mu, "g/", m^3))) +
  theme_bw() +
  facet_wrap(~ EPA_REGION, nrow=2) +
  theme(
    legend.background = element_rect("transparent"),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0),
    axis.title  = element_text(size=20),
    plot.title = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    strip.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "white", color = NA)
  ) 
# Print the plot
print(p)

ggsave(paste0("linearity.png"), path = "./plots/", width=9, height=5, units="in")

# Print the merged data with deltas
print(data_merged)

# Summarize the differences for each EPA region and variable



# Visualize the differences using ggplot2
p_diff <- ggplot(data_merged, aes(x = variable, y = delta_1, fill = NA)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ EPA_REGION, nrow=2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  labs(title = "",
       x = "",
       y = expression(paste( "Concentration, ", mu, "g/", m^3)) ) +
  theme_bw() +
  theme(
    legend.background = element_rect("transparent"),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(size=20),
    plot.title = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    strip.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "white", color = NA)
  )



data_merged$variable<- as.factor(data_merged$variable)

p_diff <- ggplot(data_merged %>% filter(!variable %in% c("PM25", "PM25_OC", "PM25_EC")), aes(x = EPA_REGION, y = delta_1)) +
  geom_bar(aes(fill = variable),stat="identity") +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_continuous( expand = c( 0, 0), breaks = seq(from = 0, to = 10, by = 1)) +
  scale_y_continuous( expand = c( 0, 0), breaks = seq(from = 0, to = 0.25, by = 0.05)) +
  theme_minimal() +
  labs(title = "",
       x = "EPA Regions",
       y = expression(paste( PM[2.5], ", ", mu, "g/", m^3)) ) +
  theme_bw() +
  theme(
    legend.background = element_rect("transparent"),
    legend.title = element_blank(),
    legend.position = c(0.8,0.8),
    axis.text.x = element_text(angle = 0),
    axis.title = element_text(size=20),
    plot.title = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    strip.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "white", color = NA)
  )


# Load the Okabe-Ito color palette for color-blind friendliness
okabe_ito_colors <- c("#E69F00", "#56B4E9", "#009E73")

ggplot(data_merged %>% filter(!variable %in% c("PM25", "PM25_OC", "PM25_EC")), aes(x = EPA_REGION, y = delta_1)) +
  geom_bar(aes(fill = variable), stat = "identity") +
  
  # Use a color-blind friendly palette (Okabe-Ito colors)
  scale_fill_manual(values = okabe_ito_colors, 
                    labels = c(expression(SO[4]^"2-"), 
                               expression(NO[3]^"-"), 
                               expression(NH[4]^"+"))) +
  
  scale_x_continuous(expand = c(0, 0), breaks = seq(from = 0, to = 10, by = 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.25), breaks = seq(0, 0.25, by = 0.05)) +
  
  # Add axis and theme adjustments
  theme_minimal() +
  labs(title = "",
       x = "EPA Regions",
       y = expression(paste(PM[2.5], ", ", mu, "g/", m^3))) +
  
  # Adjust the theme settings
  theme_bw() +
  theme(
    legend.background = element_rect("transparent"),
    legend.title = element_blank(),
    legend.position = c(0.8, 0.8),
    axis.text.x = element_text(angle = 0),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    strip.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
# Print the plot of differences
print(p_diff)

ggsave(paste0("slope_difference_at_1_region.png"), path = "./plots/", width=6, height=4, units="in")

# Filter data for PM25_SO4 and PM25_NH4
data_so4 <- data_merged %>% 
  filter(variable == "PM25_SO4") %>% 
  dplyr::select(EPA_REGION, mean_value_0_so4 = delta_1)

data_nh4 <- data_merged %>% 
  filter(variable == "PM25_NH4") %>% 
  dplyr::select(EPA_REGION, mean_value_0_nh4 = delta_1)

# Join the datasets
data_ratios <- left_join(data_so4, data_nh4, by = "EPA_REGION")

# Calculate the ratios of NH4 to SO4
data_ratios <- data_ratios %>%
  mutate(ratio_nh4_so4 = mean_value_0_nh4 / mean_value_0_so4)

# View the results
print(data_ratios)




p_diff_pct <- ggplot(data_merged %>% filter(!variable %in% "PM25"), aes(x = EPA_REGION, y = delta_1_pct )) +
  geom_bar(aes(fill = variable),stat="identity") +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_continuous( expand = c( 0, 0), breaks = seq(from = 0, to = 10, by = 1)) +
  scale_y_continuous( expand = c( 0, 0), breaks = seq(from = 0, to = 100, by = 20)) +
  theme_minimal() +
  labs(title = "",
       x = "EPA Regions",
       y = expression(paste( PM[2.5], " fraction %") )) +
  theme_bw() +
  theme(
    legend.background = element_rect("transparent"),
    legend.title = element_blank(),
    legend.position = c(0.8,0.8),
    axis.text.x = element_text(angle = 0),
    axis.title = element_text(size=20),
    plot.title = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    strip.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "white", color = NA)
  )
# Print the plot of differences
print(p_diff_pct)

ggsave(paste0("slope_difference_at_1_region_pct.png"), path = "./plots/", width=6, height=4, units="in")

# ====================================
#NOx variable with 0 SO2 emissions
# ====================================

#sensitivity contributions
combined_data <- sens %>% filter (sens %in% c("so2_ptegu_0_nox_1" , 
                                              "so2_ptegu_0_nox_0",
                                              "so2_ptegu_1_nox_1"))

combined_data <- combined_data %>% filter(!variable %in% c("PM25_EC", "PM25_OC") )


# Filter PM25 values
# pm25_values <- combined_data %>% filter(variable == "PM25") %>% dplyr::select(year, value) 


# Calculate mean values for each variable and year
data_mean <- setDT(combined_data)[, .(mean_value = mean(value)), 
                                  by = c("variable", "sens")]

data_mean
# ==========++=============
#hyads vs pm25_so4 at 1x emission interpolated from 0x emissions

library(fst)
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
  st_as_sf() %>% st_transform(crs=p4s)



names(grid.dat.sf)[names(grid.dat.sf) == 'X2020'] <- 'hyads_coal_pm25'



# Retrieve US state boundaries
us_states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

# Add state names to the dataset
us_states <- us_states %>%
  mutate(State.Name = tolower(ID))

# Create a mapping of states to EPA regions
epa_regions <- data.frame(
  State.Name = c("connecticut", "maine", "massachusetts", "new hampshire", "rhode island", "vermont",
                 "new jersey", "new york", "puerto rico", "virgin islands",
                 "delaware", "district of columbia", "maryland", "pennsylvania", "virginia", "west virginia",
                 "alabama", "florida", "georgia", "kentucky", "mississippi", "north carolina", "south carolina", "tennessee",
                 "illinois", "indiana", "michigan", "minnesota", "ohio", "wisconsin",
                 "arkansas", "louisiana", "new mexico", "oklahoma", "texas",
                 "iowa", "kansas", "missouri", "nebraska",
                 "colorado", "montana", "north dakota", "south dakota", "utah", "wyoming",
                 "arizona", "california", "hawaii", "nevada", "pacific islands",
                 "alaska", "idaho", "oregon", "washington"),
  EPA_REGION = c(rep(1, 6), rep(2, 4), rep(3, 6), rep(4, 8), rep(5, 6), rep(6, 5), rep(7, 4),
                 rep(8, 6), rep(9, 5), rep(10, 4))
)

# Merge EPA region information with US states
us_states <- us_states %>%
  left_join(epa_regions, by = "State.Name")

# Coordinate reference system projection string for spatial data
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

# Ensure the CRS of both datasets are the same
# Assuming you have the EPA regions dataset (us_states), transform CRS if needed
epa_regions_sf <- us_states %>% dplyr::select(EPA_REGION, geom) %>% st_transform(crs=p4s)

grid.dat.sf <- st_transform(grid.dat.sf, st_crs(epa_regions_sf))

# Perform spatial join between grid data and EPA regions
grid_with_regions <- st_join(grid.dat.sf, epa_regions_sf, join = st_intersects) %>% na.omit()

# Calculate the mean hyads_coal_pm25 for each EPA region
mean_values_by_region_hyads <- grid_with_regions %>%
  group_by(EPA_REGION) %>%
  summarise(mean_hyads_coal_pm25 = mean(hyads_coal_pm25, na.rm = TRUE)) %>% st_drop_geometry()


data_slope_0_hyads <- data_slope_0 %>% filter(variable=="PM25_SO4") %>% 
  dplyr::select(EPA_REGION, mean_value_1_from_0)


bf_vs_hyads <-merge(data_slope_0_hyads, mean_values_by_region_hyads, by = c("EPA_REGION"))

print(bf_vs_hyads)

stats<- as.data.table(openair::modStats(bf_vs_hyads , mod = "mean_value_1_from_0", obs = "mean_hyads_coal_pm25"))

# =========individual grid cell wise slope and difference at base case plot========


# Load the libraries
library(sf)
library(ggplot2)
library(dplyr)
library(maps)
library(tidyr)
library(data.table)
library(readr)
library(viridis)

# Retrieve US state boundaries
us_states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

# Add state names to the dataset
us_states <- us_states %>%
  mutate(State.Name = tolower(ID))

# Create a mapping of states to EPA regions
epa_regions <- data.frame(
  State.Name = c("connecticut", "maine", "massachusetts", "new hampshire", "rhode island", "vermont",
                 "new jersey", "new york", "puerto rico", "virgin islands",
                 "delaware", "district of columbia", "maryland", "pennsylvania", "virginia", "west virginia",
                 "alabama", "florida", "georgia", "kentucky", "mississippi", "north carolina", "south carolina", "tennessee",
                 "illinois", "indiana", "michigan", "minnesota", "ohio", "wisconsin",
                 "arkansas", "louisiana", "new mexico", "oklahoma", "texas",
                 "iowa", "kansas", "missouri", "nebraska",
                 "colorado", "montana", "north dakota", "south dakota", "utah", "wyoming",
                 "arizona", "california", "hawaii", "nevada", "pacific islands",
                 "alaska", "idaho", "oregon", "washington"),
  EPA_REGION = c(rep(1, 6), rep(2, 4), rep(3, 6), rep(4, 8), rep(5, 6), rep(6, 5), rep(7, 4),
                 rep(8, 6), rep(9, 5), rep(10, 4))
)

# Merge EPA region information with US states
us_states <- us_states %>%
  left_join(epa_regions, by = "State.Name")

# Coordinate reference system projection string for spatial data
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

# Plot the map with EPA regions and region numbers
ggplot(data = us_states) +
  geom_sf(aes(fill = factor(EPA_REGION))) +
  scale_fill_viridis_d(name = "EPA Region") +
  theme_void() +
  labs(title = "") +
  theme(legend.position = "bottom")

ggsave(paste0("epa_region_map.png"), path = "./plots/", width=8, height=4, units="in")


us_states <- us_states %>% st_transform(crs = p4s)

new_geo <- geo_list %>% st_as_sf() %>% st_centroid()

# Read your spatial data
sf_epa_region <- merge(combined_data, new_geo, by = "ID") %>% st_as_sf()

# Ensure both datasets have the same CRS
region_data <- st_join(us_states, sf_epa_region, join = st_intersects) %>% as.data.frame()

region_data$emission_scale <- as.numeric(region_data$group_so4)
# Calculate mean values for each variable and emission_scale
data_mean_region <- region_data %>% setDT() %>%
  .[, .(mean_value = mean(value, na.rm = TRUE)), by = .(variable, emission_scale, ID.y)]

# write_csv(data_mean_region, "./data/reg_data.csv")

# Load the data from the provided CSV file
data <- data_mean_region# read_csv('./data/reg_data.csv')

# Add missing emission scales if necessary
scales_to_add <- c(12.45, 1, 0)
variables <- unique(data$variable)
regions <- unique(data$ID.y)

# Create rows with NA values for the new scales and combine with original data
new_rows <- expand.grid(variable = variables, emission_scale = scales_to_add, ID.y = regions, mean_value = NA)
data <- rbind(data, new_rows)

# Convert emission_scale to numeric and order levels as specified
data$emission_scale <- as.numeric(data$emission_scale)

# Calculate slope between 12.45 and 0 for each variable and region
data_slope_0 <- data %>%
  filter(emission_scale %in% c(12.45, 0))

data_slope_0 <-  setDT(data_slope_0)[, .(
  mean_value_12.45 = mean(mean_value[emission_scale == 12.45], na.rm = TRUE),
  mean_value_0 = mean(mean_value[emission_scale == 0], na.rm = TRUE)), 
  by = c("variable", "ID.y")] %>%
  mutate( slope_0 = (mean_value_0 - mean_value_12.45) / (0 - 12.45) ) %>%
  mutate( mean_value_1_from_0 = mean_value_12.45 + slope_0 * (1 - 12.45) )

# Calculate slope between 12.45 and 1 for each variable and region
data_slope_1 <- data %>%
  filter(emission_scale %in% c(12.45, 1))

data_slope_1 <-  setDT(data_slope_1)[, .(
  mean_value_12.45 = mean(mean_value[emission_scale == 12.45], na.rm = TRUE),
  mean_value_1 = mean(mean_value[emission_scale == 1], na.rm = TRUE)), 
  by = c("variable", "ID.y")] %>%
  mutate( slope_1 = (mean_value_1 - mean_value_12.45) / (1 - 12.45) )

# Merge data_slope_0 and data_slope_1 by variable and EPA_REGION
data_merged <- merge(data_slope_0, data_slope_1, by = c("variable", "ID.y"), suffixes = c("_from_0", "_from_1"))

# Calculate delta of mean_value_1 from data_slope_0 and data_slope_1
data_merged <- data_merged %>%
  mutate(delta_1 = mean_value_1 - mean_value_1_from_0,
         delta_1_pct= (mean_value_1 - mean_value_1_from_0)*100/mean_value_1)

names(data_merged)[names(data_merged) == 'ID.y'] <- 'ID'


data_merged_new<- merge(data_merged, geo_list, by = c("ID")) %>% st_as_sf()

ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=data_merged_new %>% filter(variable== "PM25_SO4"), 
          aes( fill = delta_1 ,  geometry = geometry)) +
  scale_fill_viridis_c( option="plasma", name = (paste( "PM2.5_SO4_delta")),
                        limit=c(0,1), oob = scales::squish) +
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



ggsave(paste0("delta_pm25_spatiaL.png"), path = "./plots/", width=8, height=4, units="in")



# ======================

data_merge_contributions <- data_merge %>%  mutate(EC_fraction= PM25_EC*100/PM25,
                                                   OC_fraction= PM25_OC*100/PM25,
                                                   NH4_fraction= PM25_NH4*100/PM25,
                                                   NO3_fraction= PM25_NO3*100/PM25,
                                                   SO4_fraction= PM25*100/PM25) %>% 
  mutate(other_fraction= 100- (EC_fraction+OC_fraction+NH4_fraction+NO3_fraction+SO4_fraction))

#coordinate reference system projection string for spatial data
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

us.sf <-  USAboundaries::us_states()  %>%  filter(!stusps %in% c("PR", "HI", "AK") ) %>%st_transform( crs = p4s)

#PM2.5 [base]- PM2.5[No EGU SO2] =PM2.5[EGU]

pm2.5_no_egu <- data_merge_contributions %>% filter (sens %in% c ("so2_ptegu_1_nox_1", 
                                                                  "so2_ptegu_0_nox_1")) %>% 
  dplyr::select(sens, PM25_SO4, ID) 

#amount PM2.5 will be reduced due to no Coal EGUs by 2032
pm2.5_no_egu_wide <-reshape2::dcast (pm2.5_no_egu, ID ~ sens, value.var = "PM25_SO4") %>% 
  mutate(pm2.5=so2_ptegu_1_nox_1- so2_ptegu_0_nox_1) 

pm2.5_no_egu_wide <- merge(pm2.5_no_egu_wide, geo_list, by= c("ID"))


ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=pm2.5_no_egu_wide, 
          aes( fill = pm2.5 ,  geometry = geometry), color = NA) +
  scale_fill_viridis_c( option="plasma", name = (paste( "PM2.5_No_EGU")),
                        limit=c(0,NA), oob = scales::squish) +
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

ggsave("PM2.5_reduced_no_coal_EGU.png", path = "./plots/", width=12, height=8, units="in")


#PM2.5 SO4 [2005]- PM2.5 SO4[2020] =PM2.5 SO4[reduction over 15 years]
#amount of PM2.5 reduced from 2005 to 2020 due to coal EGUs SO2 emission reduction

pm2.5_no_egu <- data_merge_contributions %>% filter (sens %in% c ("so2_ptegu_1_nox_1", "so2_ptegu_0_nox_1")) %>% 
  dplyr::select(sens, PM25_SO4, ID) 


pm2.5_no_egu_wide <- reshape2::dcast (pm2.5_no_egu, ID ~ sens, value.var = "PM25_SO4") %>% 
  mutate(pm2.5=so2_ptegu_1_nox_1- so2_ptegu_0_nox_1) 

# pm2.5_no_egu_wide <-reshape2::dcast (pm2.5_no_egu, ID ~ sens, value.var = "PM25") %>% 
#   mutate(pm2.5=so2_ptegu_1_nox_1- 0) 
# hist(pm2.5_no_egu_wide$pm2.5)

#adding geometry for plotting
pm2.5_no_egu_wide <- merge(pm2.5_no_egu_wide, geo_list, by= c("ID"))

hist(pm2.5_no_egu_wide$pm2.5)

ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=pm2.5_no_egu_wide, 
          aes( fill = pm2.5 ,  geometry = geometry), color = NA) +
  scale_fill_viridis_c( option="plasma", name = (paste( "PM2.5 difference")),
                         oob = scales::squish) +
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

ggsave("PM2.5_SO4_diff_2005_2020.png", path = "./plots/", width=12, height=8, units="in")



ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=data_merge_contributions %>% filter(sens=="so2_ptegu_0_nox_1"), 
          aes( fill = SO4_fraction ,  geometry = geometry), color = NA) +
  scale_fill_viridis_c( option="plasma", name = (paste( "SO4 fraction")),
                        limit=c(0,NA), oob = scales::squish) +
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


ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=data_merge_contributions %>% filter(sens=="so2_ptegu_0_nox_0"), 
          aes( fill = SO4_fraction ,  geometry = geometry), color = NA) +
  scale_fill_viridis_c( option="plasma", name = (paste( "SO4 fraction")),
                        limit=c(0,NA), oob = scales::squish) +
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


ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=data_merge_contributions %>% filter(sens=="so2_ptegu_12.45_nox_2.8"), 
          aes( fill = PM25_SO4 ,  geometry = geometry), color = NA) +
  scale_fill_viridis_c( option="plasma", name = (paste( "SO4 PM2.5")),
                        limit=c(0,2), oob = scales::squish) +
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

ggsave("SO4_pm2.5_2005.png", path = "./plots/", width=12, height=8, units="in")


ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=data_merge_contributions %>% filter(sens=="so2_ptegu_1_nox_1"), 
          aes( fill = PM25_SO4 ,  geometry = geometry), color = NA) +
  scale_fill_viridis_c( option="plasma", name = (paste( "SO4 PM2.5")),
                        limit=c(0,2), oob = scales::squish) +
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
  

ggsave("SO4_pm2.5_2020.png", path = "./plots/", width=12, height=8, units="in")



ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=data_merge_contributions %>% filter(sens=="so2_ptegu_0_nox_1"), 
          aes( fill = PM25_SO4 ,  geometry = geometry), color = NA) +
  scale_fill_viridis_c( option="plasma", name = (paste( "SO4 PM2.5")),
                        limit=c(0,2), oob = scales::squish) +
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

ggsave("SO4_pm2.5_2032.png", path = "./plots/", width=12, height=8, units="in")



ggplot( ) +
  geom_sf( data = us.sf, size = 1) +
  geom_sf(data=data_merge_contributions %>% filter(sens=="so2_ptegu_0_nox_1"), 
          aes( fill = SO4_fraction ,  geometry = geometry), color = NA) +
  scale_fill_viridis_c( option="plasma", name = (paste( "SO4 fraction")),
                        limit=c(0,NA), oob = scales::squish) +
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


#PM2.5 contributions

contributions_long <- data_merge_contributions %>% dplyr::select(ID, sens, EC_fraction, OC_fraction,
                                                                 NO3_fraction, SO4_fraction, NH4_fraction,
                                                                 other_fraction) %>% st_drop_geometry() 

contributions_long <- melt(contributions_long, id.vars = c("ID", "sens") ,
           measure.vars = c("EC_fraction", "OC_fraction", "NO3_fraction", "SO4_fraction", "NH4_fraction",
                            "other_fraction"))

# ============================================================================================
#how much so4 PM2.5 assuming coal EGUs emissions contributing to total PM2.5?
contributions_long %>% filter (sens %in% c("so2_ptegu_12.45_nox_2.8",
                                           "so2_ptegu_1_nox_1",
                                           "so2_ptegu_0_nox_1") & variable=="SO4_fraction") %>% 
  ggplot (aes(y = reorder(sens, value), x = value))+
  geom_boxplot()  + 
  scale_x_continuous(expand=c(0,0), limits = c(0, NA)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=15, angle = 0),
         title=element_text(size=20)) +
  labs(x= "Contributions of coal EGUs to total PM2.5", y="")

#so4 PM2.5 assuming coal EGUs emissions contributing to total PM2.5?

dt_long %>% filter (sens %in% c("so2_ptegu_12.45_nox_2.8",
                                "so2_ptegu_1_nox_1",
                                "so2_ptegu_0_nox_1") &
                      variable=="PM25_SO4") %>% 
  ggplot (aes(y = reorder(sens, value), x = value))+
  geom_boxplot()  + 
  scale_x_continuous(expand=c(0,0), limits = c(0, NA)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=15, angle = 0),
         title=element_text(size=20))+
  labs(x= "PM25_SO4", y="")


# ============================================================================================

#How has the sensitivity of PM2.5 changed with change in emissions and how will it change in future?
dt_long %>% filter (sens %in% c("so2_ptegu_12.45_nox_2.8",
                                "so2_ptegu_1_nox_1",
                                "so2_ptegu_0_nox_1") &
                      variable=="PM25_SO4") %>% 
  ggplot (aes(y = reorder(sens, value), x = value))+
  geom_boxplot()  + 
  scale_x_continuous(expand=c(0,0), limits = c(0, NA)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=15, angle = 0),
         title=element_text(size=20))+
  labs(x= "PM25_SO4", y="")



dt_long %>% filter (sens %in% c("so2_ptegu_12.45_nox_2.8",
                                "so2_ptegu_1_nox_1",
                                "so2_ptegu_1_nox_0",
                                "so2_ptegu_0_nox_1",
                                "so2_ptegu_0_nox_0") &
                      variable=="PM25_SO4") %>% 
  ggplot (aes(y = reorder(sens, value), x = value))+
  geom_boxplot()  + 
  scale_x_continuous(expand=c(0,0), limits = c(0, NA)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=15, angle = 0),
         title=element_text(size=20))+
  labs(x= "PM25_SO4", y="")

dt_long %>% filter (sens %in% c("so2_ptegu_12.45_nox_2.8",
                                "so2_ptegu_1_nox_1",
                                "so2_ptegu_1_nox_0",
                                "so2_ptegu_0_nox_1",
                                "so2_ptegu_0_nox_0") &
                      variable=="PM25_NH4") %>% 
  ggplot (aes(y = reorder(sens, value), x = value))+
  geom_boxplot()  + 
  scale_x_continuous(expand=c(0,0), limits = c(0, NA)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=15, angle = 0),
         title=element_text(size=20))+
  labs(x= "PM25_NH4", y="")

dt_long %>% filter (sens %in% c("so2_ptegu_12.45_nox_2.8",
                                "so2_ptegu_1_nox_1",
                                "so2_ptegu_1_nox_0",
                                "so2_ptegu_0_nox_1",
                                "so2_ptegu_0_nox_0") &
                      variable=="PM25_NO3") %>% 
  ggplot (aes(y = reorder(sens, value), x = value))+
  geom_boxplot()  + 
  scale_x_continuous(expand=c(0,0), limits = c(0, NA)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=15, angle = 0),
         title=element_text(size=20))+
  labs(x= "PM25_NO3", y="")

#How has it changed with Coal emissions?
contributions_long %>% filter (variable=="SO4_fraction" & 
                                 sens %in% c("so2_ptegu_12.45_nox_2.8",
                                             "so2_ptegu_1_nox_1",
                                             "so2_ptegu_1_nox_0",
                                             "so2_ptegu_0_nox_1",
                                             "so2_ptegu_0_nox_0") ) %>% 
  ggplot (aes(y = reorder(sens, value), x = value))+
  geom_boxplot()  + 
  scale_x_continuous(expand=c(0,0), limits = c(0, NA)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=15, angle = 0),
         title=element_text(size=20)) +
  labs(x= "% Contributions of coal EGUs PM25_SO4", y="")


# How NH4 fraction to toal PM2.5 has changed?
  
contributions_long %>% filter (variable=="NH4_fraction") %>% 
  ggplot (aes(y = reorder(sens, value), x = value))+
  geom_boxplot()  + 
  scale_x_continuous(expand=c(0,0), limits = c(0, NA)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=15, angle = 0),
         title=element_text(size=20)) +
  labs(x= "Contributions NH4_fraction to total PM2.5", y="")



# ====================contributions to total PM2.5 under different scenario
contributions_long %>% filter ( !variable %in% c("OC_fraction",
                                                 "other_fraction",
                                                 "EC_fraction") & 
  
  sens %in% c("so2_ptegu_12.45_nox_2.8",
                                           "so2_ptegu_1_nox_1",
                                           "so2_ptegu_0_nox_1",
                                           "so2_ptegu_1_nox_0",
                                           "so2_ptegu_0_nox_0")) %>% 
  ggplot (aes(x = reorder(variable, -value), y = value, fill=sens))+
  geom_boxplot()  + 
  scale_y_continuous(expand=c(0,0), limits = c(0, NA)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=15, angle = -45),
         title=element_text(size=20)) +
  labs(x= "", y="")


contributions_long %>%
  filter(!variable %in% c("EC_fraction", "OC_fraction", "other_fraction") & 
           sens %in% c("so2_ptegu_12.45_nox_2.8",
                       "so2_ptegu_12.45_nox_0",
                       "so2_ptegu_1_nox_1",
                       "so2_ptegu_0_nox_1",
                       "so2_ptegu_1_nox_0",
                       "so2_ptegu_0_nox_0")) %>%
  ggplot(aes(x = reorder(variable, -value), y = value, fill = sens)) + 
  geom_boxplot() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  theme_bw() +
  theme(axis.title.y = element_text(size=20),
        legend.position = c(0.7, 0.7),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),  # Transparent legend panel
        axis.text = element_text(size = 15, angle = 0)) + 
  # Apply colorblind-friendly palette
  scale_fill_brewer(palette = "Set2", 
                    labels = c(expression(SO[2]~E12.45~NO[x]~A2.8), 
                               expression(SO[2]~E12.45~NO[x]~A0), 
                               expression(SO[2]~E1~NO[x]~A1), 
                               expression(SO[2]~E0~NO[x]~A1), 
                               expression(SO[2]~E1~NO[x]~A0), 
                               expression(SO[2]~E0~NO[x]~A0))) +
  labs(x = "", y = expression(paste("Fraction of species to base " , PM[2.5])), title ="") +
  scale_x_discrete(labels = c(expression(SO[4]), expression(NH[4]), expression(NO[3])))


ggsave("sensitivities_different_emissions_scenario.png", path = "./plots/", width=8, height=5, units="in")


dt_long %>% filter ( !variable %in% c("PM25_OC",
                                                 "PM25_EC",
                                                 "PM25") & 
                                  
                                  sens %in% c("so2_ptegu_12.45_nox_2.8",
                                              "so2_ptegu_1_nox_1",
                                              "so2_ptegu_0_nox_1",
                                              "so2_ptegu_1_nox_0",
                                              "so2_ptegu_0_nox_0")) %>% 
  ggplot (aes(x = reorder(variable, -value), y = value, fill=sens))+
  geom_boxplot()  + 
  scale_y_continuous(expand=c(0,0), limits = c(0, NA)) + theme_bw() +
  theme( axis.title.y = element_text(size=25),
         legend.position = c(0.7,0.7),
         legend.title = element_blank(),
         axis.text = element_text(size=15, angle = 0),
         title=element_text(size=20)) +
  labs(x= "", y="", title= expression(paste( "Concentration, ", mu, "g/", m^3)))


# =============
# fraction with respect to base case's PM2.5_SO4'

# Filter the data for 'so2_ptegu_1_nox_1'
so2_ptegu_1_nox_1_data <- data_merge[data_merge$sens == "so2_ptegu_1_nox_1", ]

# Extract the SO4 value for 'so2_ptegu_1_nox_1'
so4_reference_value <- so2_ptegu_1_nox_1_data %>% dplyr::select(ID, PM25_SO4, PM25_NO3, PM25_NH4) %>% 
  rename(PM25_SO4_base = PM25_SO4,
         PM25_NH4_base = PM25_NH4,
         PM25_NO3_base = PM25_NO3) %>% st_drop_geometry()

# Create new columns for the fractions of SO4, NH4, and NO3 with respect to 'so2_ptegu_1_nox_1' SO4

data_merge_ratio <- merge(data_merge, so4_reference_value, by = c("ID")) %>% st_drop_geometry()

data_merge_ratio$SO4_fraction <- data_merge_ratio$PM25_SO4 / data_merge_ratio$PM25_SO4_base
data_merge_ratio$NH4_fraction <- data_merge_ratio$PM25_NH4 / data_merge_ratio$PM25_NH4_base
data_merge_ratio$NO3_fraction <- data_merge_ratio$PM25_NO3 / data_merge_ratio$PM25_NO3_base



# View the result
head(data_merge_ratio[, c("sens", "PM25_SO4", "PM25_NH4", "PM25_NO3", "SO4_fraction",
                    "NH4_fraction", "NO3_fraction")])


contributions_long <- data_merge_ratio %>% dplyr::select(ID, sens, 
                                                                 NO3_fraction, 
                                                                 SO4_fraction, 
                                                                 NH4_fraction) %>% st_drop_geometry() 

contributions_long <- melt(contributions_long, id.vars = c("ID", "sens") ,
                           measure.vars = c("NO3_fraction", "SO4_fraction", "NH4_fraction"))



contributions_long %>%
  filter(sens %in% c("so2_ptegu_12.45_nox_2.8",
                     "so2_ptegu_12.45_nox_0",
                     "so2_ptegu_1_nox_1",
                     "so2_ptegu_0_nox_1",
                     "so2_ptegu_1_nox_0",
                     "so2_ptegu_0_nox_0")) %>%
  mutate(sens = factor(sens, levels = c("so2_ptegu_12.45_nox_2.8",
                                        "so2_ptegu_12.45_nox_0",
                                        "so2_ptegu_1_nox_1",
                                        "so2_ptegu_0_nox_1",
                                        "so2_ptegu_1_nox_0",
                                        "so2_ptegu_0_nox_0"))) %>%
  ggplot(aes(x = reorder(variable, -value), y = value, fill = sens)) + 
  geom_boxplot() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6)) + 
  theme_bw() +
  theme(axis.title.y = element_text(size=20),
        legend.position = c(0.7, 0.7),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        axis.text = element_text(size = 11, angle = 0)) + 
  scale_fill_brewer(palette = "Set2", 
                    labels = c(expression(SO[2]~E12.45~NO[x]~A2.8), 
                               expression(SO[2]~E12.45~NO[x]~A0), 
                               expression(SO[2]~E1~NO[x]~A1), 
                               expression(SO[2]~E0~NO[x]~A1), 
                               expression(SO[2]~E1~NO[x]~A0), 
                               expression(SO[2]~E0~NO[x]~A0))) +
  labs(x = "", y = expression(paste("Ratio of species to base ", PM[2.5], " species"))) + 
  scale_x_discrete(labels = c(expression(SO[4]), expression(NH[4]), expression(NO[3])))


ggsave("sensitivities_different_emissions_scenario.png", path = "./plots/", width=8, height=5, units="in")




# Assuming dt_long is your data frame

summary(dt_long %>% 
          filter(variable== "PM25_NO3"& 
                   sens %in% c("so2_ptegu_12.45_nox_2.8")))
summary(dt_long %>% 
          filter(variable== "PM25_NO3"& 
                   sens %in% c("so2_ptegu_1_nox_1")))

summary(dt_long %>% 
          filter(variable== "PM25_NO3"& 
                   sens %in% c("so2_ptegu_0_nox_0")))


dt_long %>% 
  filter(!variable %in% c("PM25_OC", "PM25_EC", "PM25") & 
           sens %in% c("so2_ptegu_12.45_nox_2.8", 
                       "so2_ptegu_12.45_nox_0", 
                       "so2_ptegu_1_nox_1", 
                       "so2_ptegu_0_nox_1", 
                       "so2_ptegu_1_nox_0", 
                       "so2_ptegu_0_nox_0")) %>% 
  ggplot(aes(x = reorder(variable, -value), y = value, fill = sens)) +
  geom_boxplot() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  theme_bw() +
  theme(axis.title.y = element_text(size = 25),
        legend.position = c(0.7, 0.7),
        legend.title = element_blank(),
        axis.text = element_text(size = 15, angle = 0),
        title = element_text(size = 20)) +
  labs(x = "", 
       y = "", 
       title = expression(paste(PM[2.5], ", ", mu, "g/", m^3))) +
  scale_fill_discrete(labels = c("SO2_C0_NOx_A0",
                                 "SO2_C1_NOx_A0",
                                 "SO2_C0_NOx_A1",
                                 "SO2_C1_NOx_A1",
                                 "SO2_C12.45_NOx_A0",
                                 "SO2_C12.45_NOx_A2.8")) +
  scale_x_discrete(labels = c("PM25_SO4" = expression(SO[4]), 
                              "PM25_NH4" = expression(NH[4]), 
                              "PM25_NO3" = expression(NO[3])))

# ggsave("sensitivities_different_emissions_scenario.png", path = "./plots/", width=8, height=5, units="in")


# ============================================================================================
# How contributions to total PM2.5 has changed or will change for different emissions scenario?

cont_avg <- setDT(contributions_long)[, .(value = mean(value, na.rm=TRUE)), by = .(sens, variable)]

# Stacked + percent
cont_avg %>% filter (sens %in% c("so2_ptegu_12.45_nox_2.8",
                                 "so2_ptegu_1_nox_1",
                                 "so2_ptegu_0_nox_1") ) %>% 
  ggplot(aes(x = reorder(sens, -value), y = value, fill=variable)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=15, angle = -45),
         title=element_text(size=20)) +
  labs(x= "", y="")


cont_avg %>% filter (sens %in% c("so2_ptegu_12.45_nox_2.8",
                                 "so2_ptegu_1_nox_1",
                                 "so2_ptegu_1_nox_0") ) %>% 
  ggplot(aes(x = reorder(sens, -value), y = value, fill=variable)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=15, angle = -45),
         title=element_text(size=20)) +
  labs(x= "", y="")



cont_avg %>% filter (sens %in% c("so2_ptegu_12.45_nox_2.8",
                                 "so2_ptegu_1_nox_1",
                                 "so2_ptegu_1_nox_0",
                                 "so2_ptegu_0_nox_1",
                                 "so2_ptegu_0_nox_0") ) %>% 
  ggplot(aes(x = reorder(sens, -value), y = value, fill=variable)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_bw() +
  theme( axis.title.y = element_text(size=25),
         axis.text = element_text(size=15, angle = -45),
         title=element_text(size=20)) +
  labs(x= "", y="")

# =================== coal EGUs contribution to bring down counties into attainment zone

# •	How many facilities would need to close to being counties into attainment both in 2022 and 2032?

# which of the counties can be brought into attainment by tackling coal EGUs SO2 emissions?

# which of the counties can be brought into attainment by tackling coal EGUs SO2 emissions and mobile NOx?

# which of the counties can be brought into attainment by tackling mobile NOx?

#projected 2032 counties more than PM2.5 of 9, 10, 12 ug/m3

pm2.5_std_2032 <- c (9)

dt_list= list()

for (i in 1:length (pm2.5_std_2032)) {
  projected_2032 <- read.csv(paste0("./data/epa_non_attainment/Source 2032_projections for Design Values greater than or equal to ", pm2.5_std_2032[i], ".csv")) %>% 
    dplyr::select(-Table.2A.8.PM2.5.DVs.for.2032.Projection) %>% mutate(projected_std = pm2.5_std_2032[i])
  
  names(projected_2032)[names(projected_2032) == 'Annual.2032.DV..ug.m..3.'] <- 'annual.2032_pm2.5'
  
  projected_2032 <- projected_2032 %>% mutate(annual.2032_pm2.5=as.numeric(annual.2032_pm2.5)) %>% na.omit()
  
  
  ## ====================================================== 
  #  retrieve spatial data about the counties
  ## ====================================================== 
  
  counties.us <- USAboundaries::us_counties( ) %>% dplyr::select(state_abbr, name, geometry)
  
  names(counties.us)[names(counties.us) == 'state_abbr'] <- 'State'
  names(counties.us)[names(counties.us) == 'name'] <- 'County'
  
  counties_2032_pm2.5 <- merge( projected_2032 , counties.us, by= c("State", "County")) %>%
    st_as_sf( sf_column_name = 'geometry') %>%
    st_transform( crs = st_crs( p4s))
  
  county.vec <- as.vector(unique(counties_2032_pm2.5$County))
  sens.vec <- c ("so2_ptegu_12.45_nox_2.8" ,
                 "so2_ptegu_1_nox_0" ,
                 "so2_ptegu_0_nox_0",       
                 "so2_ptegu_12.45_nox_0",
                 "so2_ptegu_0_nox_1" ,     
                 "so2_ptegu_1_nox_1")
  
  
  datalist = list()
  datalist_sens = list()
  
  for (j in 1:length (sens.vec)) {
    
    df_sens<- data_merge_contributions %>% filter (sens %in% sens.vec[j])
    for (k in 1:length(county.vec)) {
      county_df <- counties_2032_pm2.5 [k, ]
      df_crop <-  st_intersection( df_sens,county_df ) 
      
      datalist[[k]] <-df_crop
    }
    
    sens_cont_combined = do.call(rbind, datalist)
    
    datalist_sens[[j]] <- sens_cont_combined
  }
  
  sens_cont_combined_all = do.call(rbind, datalist_sens)

  #highest contribution
  projected_annual_contribution_2032<- setDT(sens_cont_combined_all)[, .(annual.2032_pm2.5=mean(annual.2032_pm2.5, na.rm=TRUE),
                                                                           PM25 =mean(PM25, na.rm=TRUE),
                                                                           PM25_EC = mean(PM25_EC, na.rm=TRUE),
                                                                           PM25_NH4= mean(PM25_NH4, na.rm=T),
                                                                           PM25_NO3= mean(PM25_NO3, na.rm=T),
                                                                           PM25_OC= mean(PM25_OC, na.rm=T),
                                                                           PM25_SO4= mean(PM25_SO4, na.rm=T),
                                                                           EC_fraction= mean(EC_fraction, na.rm=T),
                                                                           OC_fraction= mean(OC_fraction, na.rm=T),
                                                                           NH4_fraction= mean(NH4_fraction, na.rm=T),
                                                                           NO3_fraction= mean(NO3_fraction, na.rm=T),
                                                                           SO4_fraction= mean(SO4_fraction, na.rm=T),
                                                                           other_fraction= mean(other_fraction, na.rm=T)),
                                                                       by = .(sens, State, County)] 
  
  
  dt_list[[i]] <- projected_annual_contribution_2032
  
  message ("done for PM2.5 standard ", pm2.5_std_2032[i], " in 2032" )
}


projected_2032 <- do.call(rbind, dt_list) %>% mutate(id= paste0(State, "-", County)) #%>% dplyr::select(-egus_contribution_to_tot_pm2.5)

# Which counties will be in attainment by controlling coal EGUs emissions?
proj_32 <- projected_2032 %>% dplyr::select(State, County, sens, annual.2032_pm2.5, PM25)

proj_32_wide <- reshape2::dcast (proj_32, State + County + annual.2032_pm2.5 ~ sens, value.var = c("PM25"))


#right???? 
proj_32_wide <- proj_32_wide %>% mutate (no_coal.egus_no_nox= so2_ptegu_0_nox_0,
                                           same_coal.egus_no_nox=so2_ptegu_1_nox_0,
                                           no_coal.egus_same_nox=so2_ptegu_0_nox_1,
                                           same_coal.egus_same_nox= so2_ptegu_1_nox_1) %>% 
  mutate (pm25_no_coal.egus_no_nox= annual.2032_pm2.5- no_coal.egus_no_nox,
            pm25_same_coal.egus_no_nox= annual.2032_pm2.5-same_coal.egus_no_nox,
            pm25_no_coal.egus_same_nox= annual.2032_pm2.5-no_coal.egus_same_nox,
            pm25_same_coal.egus_same_nox= annual.2032_pm2.5-same_coal.egus_same_nox,
          id= paste0(State, "-", County))

proj_32_wide <- proj_32_wide %>% mutate (no_coal.egus_no_nox= so2_ptegu_0_nox_0,
                                         same_coal.egus_no_nox=so2_ptegu_1_nox_0,
                                         no_coal.egus_same_nox=so2_ptegu_0_nox_1,
                                         same_coal.egus_same_nox= so2_ptegu_1_nox_1) %>% 
  mutate (pm25_no_coal.egus_no_nox= annual.2032_pm2.5- no_coal.egus_no_nox,
          pm25_same_coal.egus_no_nox= annual.2032_pm2.5-same_coal.egus_no_nox,
          pm25_no_coal.egus_same_nox= annual.2032_pm2.5-no_coal.egus_same_nox,
          delta= (same_coal.egus_same_nox-no_coal.egus_same_nox),
          pm25_same_coal.egus_same_nox= annual.2032_pm2.5- (same_coal.egus_same_nox-no_coal.egus_same_nox),
          id= paste0(State, "-", County))

#No coal EGUs but NOx emissions same in 2032


#No coal EGUs and no NOx emissions in 2032 (all electric cars?) 


#keep coal emissions as is but no NOx emissions in 2032


# reshape2::melt(proj_32, id.vars = c ("State", "County"), measure.vars = c("sens"))


# ===============counties that will be in attainment by comtrolling coal EGUs emissions by 2032======



#Brute force data of PM2.5 due to Coal EGUs emissions

pm2.5_std_2032 <- c (9)

projected_2032 <- read.csv(paste0("./data/epa_non_attainment/Source 2032_projections for Design Values greater than or equal to ", pm2.5_std_2032, ".csv")) %>% 
    dplyr::select(-Table.2A.8.PM2.5.DVs.for.2032.Projection) %>% mutate(projected_std = pm2.5_std_2032)
  
names(projected_2032)[names(projected_2032) == 'Annual.2032.DV..ug.m..3.'] <- 'annual.2032_pm2.5'
  
projected_2032 <- projected_2032 %>% mutate(annual.2032_pm2.5=as.numeric(annual.2032_pm2.5)) %>% na.omit()
  
  
## ====================================================== 
#  retrieve spatial data about the counties
## ====================================================== 

#counties info  
counties.us <- USAboundaries::us_counties( ) %>% dplyr::select(state_abbr, name, geometry)
counties.us <- counties.us %>% filter (!State %in% c( 'AK', 'HI', 'PR'))

names(counties.us)[names(counties.us) == 'state_abbr'] <- 'State'
names(counties.us)[names(counties.us) == 'name'] <- 'County'
  
counties_2032_pm2.5 <- merge( projected_2032 , counties.us, by= c("State", "County")) %>%
    st_as_sf( sf_column_name = 'geometry') %>%
    st_transform( crs = st_crs( p4s))
  
county.vec <- as.vector(unique(counties_2032_pm2.5$County))
sens.vec <- c ("so2_ptegu_12.45_nox_2.8" ,
                 "so2_ptegu_1_nox_0" ,
                 "so2_ptegu_0_nox_0",       
                 "so2_ptegu_12.45_nox_0",
                 "so2_ptegu_0_nox_1" ,     
                 "so2_ptegu_1_nox_1")
  

#getting counties gridded concentration by using intersection function
datalist = list()
datalist_sens = list()
  
for (j in 1:length (sens.vec)) {
    
  df_sens<- data_merge_contributions %>% filter (sens %in% sens.vec[j])
    for (k in 1:length(county.vec)) {
      county_df <- counties_2032_pm2.5 [k, ]
      df_crop <-  st_intersection( df_sens,county_df ) 
      
      datalist[[k]] <-df_crop
    }
    
    sens_cont_combined = do.call(rbind, datalist)
    
    datalist_sens[[j]] <- sens_cont_combined
  }
  
sens_cont_combined_all = do.call(rbind, datalist_sens)
  
#highest contribution
projected_annual_contribution_2032<- setDT(sens_cont_combined_all)[, .(annual.2032_pm2.5=mean(annual.2032_pm2.5, na.rm=TRUE),
                                                                         PM25 =mean(PM25, na.rm=TRUE),
                                                                         PM25_EC = mean(PM25_EC, na.rm=TRUE),
                                                                         PM25_NH4= mean(PM25_NH4, na.rm=T),
                                                                         PM25_NO3= mean(PM25_NO3, na.rm=T),
                                                                         PM25_OC= mean(PM25_OC, na.rm=T),
                                                                         PM25_SO4= mean(PM25_SO4, na.rm=T),
                                                                         EC_fraction= mean(EC_fraction, na.rm=T),
                                                                         OC_fraction= mean(OC_fraction, na.rm=T),
                                                                         NH4_fraction= mean(NH4_fraction, na.rm=T),
                                                                         NO3_fraction= mean(NO3_fraction, na.rm=T),
                                                                         SO4_fraction= mean(SO4_fraction, na.rm=T),
                                                                         other_fraction= mean(other_fraction, na.rm=T)),
                                                                     by = .(sens, State, County)] 
  
  

  

projected_2032 <- projected_annual_contribution_2032 %>% mutate(id= paste0(State, "-", County)) 

# Which counties will be in attainment by controlling coal EGUs emissions?
proj_32 <- projected_2032 %>% dplyr::select(State, County, sens, annual.2032_pm2.5, PM25)

proj_32_wide <- reshape2::dcast (proj_32, State + County + annual.2032_pm2.5 ~ sens, value.var = c("PM25"))



proj_32_wide <- proj_32_wide %>% mutate (no_coal.egus_no_nox= so2_ptegu_0_nox_0,
                                         same_coal.egus_no_nox=so2_ptegu_1_nox_0,
                                         no_coal.egus_same_nox=so2_ptegu_0_nox_1,
                                         same_coal.egus_same_nox= so2_ptegu_1_nox_1) %>% 
  mutate (pm25_no_coal.egus_no_nox= annual.2032_pm2.5- no_coal.egus_no_nox,
          pm25_same_coal.egus_no_nox= annual.2032_pm2.5-same_coal.egus_no_nox,
          pm25_no_coal.egus_same_nox= annual.2032_pm2.5-no_coal.egus_same_nox,
          delta= (same_coal.egus_same_nox-no_coal.egus_same_nox),
          pm25_same_coal.egus_same_nox= annual.2032_pm2.5- (same_coal.egus_same_nox-no_coal.egus_same_nox),
          id= paste0(State, "-", County))

bf_coal_egus <- proj_32_wide %>% dplyr:: select(State, County, id, delta)


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
  

#DDM sensitivity plot during summer
ggplot( ) +
  geom_sf(data=ddm_summer_avg, aes( fill = pm2.5_so2_summer_avg ,  geometry = geometry), color = NA) +
  scale_fill_viridis_c( option="plasma", name = expression(paste("Coal " , PM[2.5], " ug/", m^3)),
                        limit=c(-NA,NA), oob = scales::squish) +
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
         legend.title = element_text( size = 19),
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


ggsave("ddm_coal_pm25.png", path = "./plots/", width=12, height=8, units="in")


#DDM sensitivity plot year average of coal pm2.5 concentration
summary(ddm_yr_avg_sf)
ddm_yr_plot <- ggplot( ) +
  geom_sf(data=ddm_yr_avg_sf, aes( fill = pm2.5_so2_yr_avg ,  geometry = geometry), color = NA) +
  scale_fill_viridis_c( option="plasma", name = expression(paste("Coal " ,PM[2.5], " " ,mu, "g/", m^3)),
                        limit=c(-NA,1.0), oob = scales::squish) +
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
         legend.title = element_text( size = 20),
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

ggsave("ddm_coal_pm25_yr_avg.png", path = "./plots/", width=12, height=8, units="in")



ggsave("proj_2032_plot.png", path = "./plots/", width=12, height=8, units="in")
# PMBF_Summer = Base (SO2_1_NOx_1) - Future (SO2_0_NOx_1)
pm2.5_no_egu_wide

# PM2.5 in 2032 = PMBF_Summer x 〖𝑃𝑀〗_(2.5_𝑆𝑂2_𝐸𝐺𝑈_𝑊ℎ𝑜𝑙𝑒 𝑦𝑒𝑎𝑟)/〖𝑃𝑀〗_(2.5_𝑆𝑂4_𝑆𝑢𝑚𝑚𝑒𝑟) 

#PM_BF over PM_DDM
pm2.5_bf_summer <- pm2.5_no_egu_wide %>% st_drop_geometry()
pm2.5_ddm_summer <- ddm_summer_avg %>% st_drop_geometry()
pm2.5_ddm_yr <- ddm_yr_avg_sf %>% st_drop_geometry()

pm2.5_bf_ddm_summer<- merge(pm2.5_bf_summer, pm2.5_ddm_summer, by= c ("ID"))

pm2.5_bf_ddm_summer_yr <- merge(pm2.5_bf_ddm_summer, pm2.5_ddm_yr, by= c ("ID"))

pm2.5_proj_2032 <- pm2.5_bf_ddm_summer_yr %>% mutate(proj_2032= pm2.5*pm2.5_so2_yr_avg/pm2.5_so2_summer_avg) %>% 
  st_drop_geometry() %>% dplyr::select(-geometry)

pm2.5_proj_2032 <- merge(pm2.5_proj_2032, geo_list, by = ("ID")) %>% st_as_sf()

# •	Figure with 2 maps showing annual average SO2 EGU PM2.5 sensitivities;
# one showing DDM sensitivities, one showing brute force projected to annual with DDM sensitivities. 
# [include outlines of counties marked for non-attainment in 2022 and 2023]

#projected pm2.5 in 2032 plot

proj_2032_plot <- ggplot( ) +
  geom_sf(data=pm2.5_proj_2032, aes( fill = proj_2032 ,  geometry = geometry), color = NA) +
  geom_sf( data = counties.us, fill="white", color= "black")+
  scale_fill_viridis_c( option="plasma", name = expression(paste("projected " ,PM[2.5], " ug/", m^3)),
                        limit=c(-NA,1.2), oob = scales::squish) +
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



#taking summer months data
ddm_dt_summer <- ddm_dt %>% filter (month %in% c("07", "08")) 

summary(ddm_dt_summer)

#county wise average concentration during summer and for whole year

pm2.5_new_std <- c (9)

pm2.5_new_std_non_attain <- read.csv(paste0("./data/epa_non_attainment/Source 2032_projections for Design Values greater than or equal to ", pm2.5_std_2032[i], ".csv")) %>% 
  dplyr::select(-Table.2A.8.PM2.5.DVs.for.2032.Projection) %>% mutate(projected_std = pm2.5_new_std)

names(pm2.5_new_std_non_attain)[names(pm2.5_new_std_non_attain) == 'Annual.2032.DV..ug.m..3.'] <- 'annual.2032_pm2.5'

pm2.5_new_std_non_attain <- pm2.5_new_std_non_attain %>% mutate(annual.2032_pm2.5=as.numeric(annual.2032_pm2.5)) %>% na.omit()


## ====================================================== 
#  retrieve spatial data about the counties
## ====================================================== 

#counties info  
counties.us <- USAboundaries::us_counties( ) %>% dplyr::select(state_abbr, name, geometry)

names(counties.us)[names(counties.us) == 'state_abbr'] <- 'State'
names(counties.us)[names(counties.us) == 'name'] <- 'County'

counties_non_attain_pm2.5 <- merge( pm2.5_new_std_non_attain , counties.us, by= c("State", "County")) %>%
  st_as_sf( sf_column_name = 'geometry') %>%
  st_transform( crs = st_crs( p4s))

county.vec <- as.vector(unique(counties_non_attain_pm2.5$County))


datalist = list()
datalist_sens = list()

for (k in 1:length(county.vec)) {
    county_df <- counties_non_attain_pm2.5 [k, ]
    
    #intersection during summer
    ddm_summer_county <-  st_intersection( ddm_dt_summer,county_df ) 
    
    #intersection for whole year
    ddm_year_county <-  st_intersection( ddm_dt,county_df ) 
    
    datalist[[k]] <-ddm_summer_county
    
    datalist_sens [[k]] <- ddm_year_county
}
  

ddm_summer = do.call(rbind, datalist)
ddm_year = do.call(rbind, datalist_sens)

#highest contribution
ddm_summer_mean<- setDT(ddm_summer)[, .(ddm_summer_2.5_so4=mean(z, na.rm=TRUE)),
                                                                   by = .(State, County)] %>% 
  mutate(id= paste0(State, "-", County)) 

ddm_year_mean<- setDT(ddm_year)[, .(ddm_year_2.5_so4=mean(z, na.rm=TRUE)),
                                    by = .(State, County)] %>% mutate(id= paste0(State, "-", County)) 


df <- merge(bf_coal_egus, ddm_summer_mean,  by= c("State", "County", "id"))

df <- merge(df, ddm_year_mean,  by= c("State", "County", "id"))

df_bf_ddm_adjusted <- df %>%mutate (ratio=delta * (ddm_year_2.5_so4/ddm_summer_2.5_so4))

pm2.5_new_std_non_attain_ctm <- merge(pm2.5_new_std_non_attain, df_bf_ddm_adjusted, by = c ("State", "County"))

counties_attainment <- pm2.5_new_std_non_attain_ctm %>% mutate(attainment= annual.2032_pm2.5- ratio)

standard <- 9

# Figure showing assuming 12µg/m3 standard 

counties_attainment %>% filter(attainment <9.5) %>% 
  ggplot() +
  geom_segment( aes(x=reorder(id ,attainment), 
                    xend=id, y=attainment, yend=annual.2032_pm2.5), color="grey") +
  geom_point( aes(x=id, y=attainment), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=id, y=annual.2032_pm2.5), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_ipsum() +
  geom_hline(yintercept = standard,   color = "black", size=1 ) +
  theme( legend.position = 'none',
         axis.text  = element_text( angle = 0, size=16),
         axis.title = element_blank()) +
  xlab("") +
  ylab("pm25_no_coal.egus_same_nox")



# How many facilities would need to close to being counties into attainment?

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

counties_attainment_fips<- merge(counties_attainment, county_state_fips, by = c ("State", "County"))


#yearly emission
PP.units.yearly2022 <- PP.units.monthly2022 %>% dplyr::select(Facility.Name,
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

#attainment counties by controlling coal EGUs
attainment_counties <- counties_attainment_fips %>% filter (attainment<9)
attainments_fips <- as.vector(attainment_counties$fips)

#number of facilities in those attainment counties that needs to be under control
egus_facilities_attainment <- PP.units.yearly2022 %>% filter (fips %in% attainments_fips)

unique(egus_facilities_attainment$Facility.Name)
                              
# Locations EPA determined to be out of attainment in 2022 and 2023 that would meet 
# standard by shuttering coal plants
counties_non_attainment_vec_2032 <- unique(counties_attainment_fips$id) 

counties.us <- counties.us %>% mutate(id = paste(State, County, sep = "-")) 

counties.us <- counties.us %>% filter (!State %in% c( 'AK', 'HI', 'PR'))

counties.us_2032 <- counties.us

counties.us_2032$group <- ifelse (counties.us_2032$id %in% counties_non_attainment_vec_2032, 
                                  "non-attainment", 
                                  "attainment")

# Define fill colors for different groups
group_colors <- c("attainment" = "white",
                  "non-attainment" = "red")


ggplot() + geom_sf( data = counties.us, fill="white", color= "black") +
  geom_sf( data = counties.us_2032, aes(fill=group)) +
  scale_fill_manual(values = group_colors) +theme_bw() +
  theme(legend.position = c( 0.87, .12), 
        legend.direction = "vertical" ,
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        text = element_text(size=10))


# answered above for 2032

# Figure with 2 maps showing annual average SO2 EGU PM2.5 sensitivities; 
# one showing DDM sensitivities, one showing brute force projected to annual with DDM sensitivities.
# [include outlines of counties marked for non-attainment in 2022 and 2023]




# Figure showing before/after assuming 9µg/m3 standard in out of attainment counties in 2022 and 2023. 

#2032
counties_attainment %>% filter(attainment <9) %>% 
  ggplot() +
  geom_segment( aes(x=reorder(id ,attainment), 
                    xend=id, y=attainment, yend=annual.2032_pm2.5), color="grey") +
  geom_point( aes(x=id, y=attainment), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=id, y=annual.2032_pm2.5), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_ipsum() +
  geom_hline(yintercept = standard,   color = "black", size=1 ) +
  theme( legend.position = 'none',
         axis.text  = element_text( angle = 0, size=16),
         axis.title = element_blank()) +
  xlab("") +
  ylab("pm25_no_coal.egus_same_nox")



# Additional analysis: for each county,
# what fraction of PM2.5 from coal SO2 would need to be cut to meet the 9µg m 3 standard?
#ratio is projected 2032 concentration
counties_attainment_fraction <- counties_attainment %>% filter(attainment<9) %>% 
  mutate(fraction=(annual.2032_pm2.5-attainment)*100/annual.2032_pm2.5 )

counties.us_p4s <- counties.us %>% st_transform(p4s)

counties.us_2032_p4s <- counties.us_2032 %>% st_transform(p4s)

proj_2032_plot + geom_sf( data = counties.us_p4s, fill="NA", color= "black") 

ggsave("proj_2032_plot.png", path = "./plots/", width=12, height=8, units="in")

ddm_yr_plot + geom_sf( data = counties.us_p4s, fill="NA", color= "black") 

ggsave("ddm_yr_plot.png", path = "./plots/", width=12, height=8, units="in")


# ===================================================================
# ========same analysis for year of 2022==========
# ===================================================================

# ===============counties that will be in attainment by comtrolling coal EGUs emissions by 2022======



#Brute force data of PM2.5 due to Coal EGUs emissions

pm2.5_std_2022 <- c (9)

projected_2022 <- read.csv(paste0("./data/epa_non_attainment/Source Table 6a for concentrations greater than or equal to 9 simplified.csv")) 

names(projected_2022)[names(projected_2022) == 'X2020.2022.....Annual......Design.Value..µg.m3...1.2.'] <- 'annual.2022_pm2.5'

projected_2022 <- projected_2022 %>% mutate(annual.2022_pm2.5=as.numeric(annual.2022_pm2.5)) %>% na.omit()

# taking mean by state and county
projected_2022 <- setDT(projected_2022)[, .(annual.2022_pm2.5=mean(annual.2022_pm2.5, na.rm=TRUE)),
                              by = .( State.Name, County.Name)] 

names(projected_2022)[names(projected_2022) == 'State.Name'] <- 'State'
names(projected_2022)[names(projected_2022) == 'County.Name'] <- 'County'


## ====================================================== 
#  retrieve spatial data about the counties
## ====================================================== 

#getting states info
states.us <- USAboundaries::us_states( ) %>% dplyr::select(name , state_abbr) %>% st_drop_geometry() %>% 
  filter (!state_abbr %in% c( 'AK', 'HI', 'PR'))

names(states.us)[names(states.us) == 'name'] <- 'State'

projected_2022<- merge(projected_2022, states.us, by = c("State")) %>% dplyr::select(-State)

names(projected_2022)[names(projected_2022) == 'state_abbr'] <- 'State'
#counties info  
counties.us <- USAboundaries::us_counties( ) %>% dplyr::select(state_abbr, name, geometry)

counties.us <- counties.us %>% filter (!state_abbr %in% c( 'AK', 'HI', 'PR'))


names(counties.us)[names(counties.us) == 'state_abbr'] <- 'State'
names(counties.us)[names(counties.us) == 'name'] <- 'County'

counties_2022_pm2.5 <- merge( projected_2022 , counties.us, by= c("State", "County")) %>%
  st_as_sf( sf_column_name = 'geometry') %>%
  st_transform( crs = st_crs( p4s))

county.vec <- as.vector(unique(counties_2022_pm2.5$County))
sens.vec <- c ("so2_ptegu_12.45_nox_2.8" ,
               "so2_ptegu_1_nox_0" ,
               "so2_ptegu_0_nox_0",       
               "so2_ptegu_12.45_nox_0",
               "so2_ptegu_0_nox_1" ,     
               "so2_ptegu_1_nox_1")


datalist = list()
datalist_sens = list()

for (j in 1:length (sens.vec)) {
  
  df_sens<- data_merge_contributions %>% filter (sens %in% sens.vec[j])
  for (k in 1:length(county.vec)) {
    county_df <- counties_2022_pm2.5 [k, ]
    df_crop <-  st_intersection( df_sens,county_df ) 
    
    datalist[[k]] <-df_crop
  }
  
  sens_cont_combined = do.call(rbind, datalist)
  
  datalist_sens[[j]] <- sens_cont_combined
}

sens_cont_combined_all = do.call(rbind, datalist_sens) %>% mutate(id= paste0(State, "-", County)) 

#highest contribution
projected_annual_contribution_2022<- setDT(sens_cont_combined_all)[, .(annual.2022_pm2.5=mean(annual.2022_pm2.5, na.rm=TRUE),
                                                                       PM25 =mean(PM25, na.rm=TRUE),
                                                                       PM25_EC = mean(PM25_EC, na.rm=TRUE),
                                                                       PM25_NH4= mean(PM25_NH4, na.rm=T),
                                                                       PM25_NO3= mean(PM25_NO3, na.rm=T),
                                                                       PM25_OC= mean(PM25_OC, na.rm=T),
                                                                       PM25_SO4= mean(PM25_SO4, na.rm=T),
                                                                       EC_fraction= mean(EC_fraction, na.rm=T),
                                                                       OC_fraction= mean(OC_fraction, na.rm=T),
                                                                       NH4_fraction= mean(NH4_fraction, na.rm=T),
                                                                       NO3_fraction= mean(NO3_fraction, na.rm=T),
                                                                       SO4_fraction= mean(SO4_fraction, na.rm=T),
                                                                       other_fraction= mean(other_fraction, na.rm=T)),
                                                                   by = .(sens, State, County, id)] 





projected_2022 <- projected_annual_contribution_2022 

# Which counties will be in attainment by controlling coal EGUs emissions?
proj_22 <- projected_2022 %>% dplyr::select(State, County, id, sens, annual.2022_pm2.5, PM25)

proj_22_wide <- reshape2::dcast (proj_22, State + County + annual.2022_pm2.5 ~ sens, value.var = c("PM25"))

proj_22_wide <- proj_22_wide %>%  mutate(id= paste0(State, "-", County))

proj_22_wide <- proj_22_wide %>% mutate (no_coal.egus_no_nox= so2_ptegu_0_nox_0,
                                         same_coal.egus_no_nox=so2_ptegu_1_nox_0,
                                         no_coal.egus_same_nox=so2_ptegu_0_nox_1,
                                         same_coal.egus_same_nox= so2_ptegu_1_nox_1) %>% 
  mutate (pm25_no_coal.egus_no_nox= annual.2022_pm2.5- no_coal.egus_no_nox,
          pm25_same_coal.egus_no_nox= annual.2022_pm2.5-same_coal.egus_no_nox,
          pm25_no_coal.egus_same_nox= annual.2022_pm2.5-no_coal.egus_same_nox,
          delta= (same_coal.egus_same_nox-no_coal.egus_same_nox),
          pm25_same_coal.egus_same_nox= annual.2022_pm2.5- (same_coal.egus_same_nox-no_coal.egus_same_nox))

bf_coal_egus <- proj_22_wide %>% dplyr:: select(State, County, id, delta)


# ============DDM data read==========

# load(file=paste0("./data/egus_so2_pm2.5/egus_so2_pm25.RData"))
# 
# ddm_dt <- do.call(rbind,datalist)
# 
# ddm_geolist <- ddm_dt %>% dplyr::select(ID, geometry)
# 
# ddm_geolist_distinct <- ddm_geolist %>% distinct()
# 
# save(ddm_geolist_distinct, file=paste0("./data/ddm_geolist_distinct.RData"))


# setDT(ddm_summer)[, .(pm2.5_so2=mean(z, na.rm=TRUE)),
#                   by = .(ID)]

#ddm yearly average
ddm_yr <- ddm_dt %>% st_drop_geometry() %>% dplyr::select(-year)

ddm_yr_avg <-  setDT(ddm_yr)[, .(pm2.5_so2_yr_avg=mean(z, na.rm=TRUE)),
                             by = .(ID)]

#adding geometry

ddm_yr_avg_sf <- merge(ddm_yr_avg, geo_list, by="ID") %>% st_as_sf()


#ddm summer average
ddm_summer_avg <- ddm_dt %>% st_drop_geometry() %>% filter(month %in% c("07", "08")) %>% dplyr::select(-year)

ddm_summer_avg <-  setDT(ddm_summer_avg)[, .(pm2.5_so2_summer_avg=mean(z, na.rm=TRUE)),  by = .(ID)]

#adding geometry

ddm_summer_avg <- merge(ddm_summer_avg, geo_list, by="ID") %>% st_as_sf()




# PM2.5 in 2022 = PMBF_Summer x 〖𝑃𝑀〗_(2.5_𝑆𝑂2_𝐸𝐺𝑈_𝑊ℎ𝑜𝑙𝑒 𝑦𝑒𝑎𝑟)/〖𝑃𝑀〗_(2.5_𝑆𝑂4_𝑆𝑢𝑚𝑚𝑒𝑟) 

#PM_BF over PM_DDM
pm2.5_bf_summer <- pm2.5_no_egu_wide %>% st_drop_geometry()
pm2.5_ddm_summer <- ddm_summer_avg %>% st_drop_geometry()
pm2.5_ddm_yr <- ddm_yr_avg_sf %>% st_drop_geometry()

pm2.5_bf_ddm_summer<- merge(pm2.5_bf_summer, pm2.5_ddm_summer, by= c ("ID"))

pm2.5_bf_ddm_summer_yr <- merge(pm2.5_bf_ddm_summer, pm2.5_ddm_yr, by= c ("ID"))

pm2.5_proj_2022 <- pm2.5_bf_ddm_summer_yr %>% mutate(proj_2022= pm2.5*pm2.5_so2_yr_avg/pm2.5_so2_summer_avg) %>% 
  st_drop_geometry() %>% dplyr::select(-geometry)

pm2.5_proj_2022 <- merge(pm2.5_proj_2022, geo_list, by = ("ID")) %>% st_as_sf()

#projected pm2.5 in 2022 plot

proj_2022_plot <- ggplot( ) +
  geom_sf(data=pm2.5_proj_2022, aes( fill = proj_2022 ,  geometry = geometry), color = NA) +
  scale_fill_viridis_c( option="plasma", name = expression(paste("projected " ,PM[2.5], " ug/", m^3)),
                        limit=c(-NA,1), oob = scales::squish) +
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



#taking summer months data
ddm_dt_summer <- ddm_dt %>% filter (month %in% c("07", "08")) 

summary(ddm_dt_summer)

#county wise average concentration during summer and for whole year

pm2.5_std_2022 <- c (9)

projected_2022 <- read.csv(paste0("./data/epa_non_attainment/Source Table 6a for concentrations greater than or equal to 9 simplified.csv")) 

names(projected_2022)[names(projected_2022) == 'X2020.2022.....Annual......Design.Value..µg.m3...1.2.'] <- 'annual.2022_pm2.5'

projected_2022 <- projected_2022 %>% mutate(annual.2022_pm2.5=as.numeric(annual.2022_pm2.5)) %>% na.omit()

# taking mean by state and county
projected_2022 <- setDT(projected_2022)[, .(annual.2022_pm2.5=mean(annual.2022_pm2.5, na.rm=TRUE)),
                                        by = .( State.Name, County.Name)] 

names(projected_2022)[names(projected_2022) == 'State.Name'] <- 'State'
names(projected_2022)[names(projected_2022) == 'County.Name'] <- 'County'


## ====================================================== 
#  retrieve spatial data about the counties
## ====================================================== 

#getting states abbreviation info
states.us <- USAboundaries::us_states( ) %>% dplyr::select(name , state_abbr) %>% st_drop_geometry() %>% 
  filter (!state_abbr %in% c( 'AK', 'HI', 'PR'))

names(states.us)[names(states.us) == 'name'] <- 'State'

projected_2022<- merge(projected_2022, states.us, by = c("State")) %>% dplyr::select(-State)

names(projected_2022)[names(projected_2022) == 'state_abbr'] <- 'State'
#counties info  
counties.us <- USAboundaries::us_counties( ) %>% dplyr::select(state_abbr, name, geometry)

counties.us <- counties.us %>% filter (!state_abbr %in% c( 'AK', 'HI', 'PR'))


names(counties.us)[names(counties.us) == 'state_abbr'] <- 'State'
names(counties.us)[names(counties.us) == 'name'] <- 'County'

counties_2022_pm2.5 <- merge( projected_2022 , counties.us, by= c("State", "County")) %>%
  st_as_sf( sf_column_name = 'geometry') %>%
  st_transform( crs = st_crs( p4s))

counties_2022_pm2.5 <- counties_2022_pm2.5 %>% mutate(id= paste0(State, "-", County)) 
county.vec <- as.vector(unique(counties_2022_pm2.5$id))



datalist = list()
datalist_sens = list()

for (k in 1:length(county.vec)) {
  county_df <- counties_2022_pm2.5 [k, ]
  
  #intersection during summer
  ddm_summer_county <-  st_intersection( ddm_dt_summer,county_df ) 
  
  #intersection for whole year
  ddm_year_county <-  st_intersection( ddm_dt,county_df ) 
  
  datalist[[k]] <-ddm_summer_county
  
  datalist_sens[[k]] <- ddm_year_county
}


ddm_summer_2022 = do.call(rbind, datalist)
ddm_year_2022 = do.call(rbind, datalist_sens)

#highest contribution
ddm_summer_mean<- setDT(ddm_summer_2022)[, .(ddm_summer_2.5_so4=mean(z, na.rm=TRUE)),
                                    by = .(State, County, id)]

ddm_year_mean <- setDT(ddm_year_2022)[, .(ddm_year_2.5_so4=mean(z, na.rm=TRUE)),
                                by = .(State, County, id)] 


df <- merge(bf_coal_egus, ddm_summer_mean,  by= c("State", "County", "id"))

df <- merge(df, ddm_year_mean,  by= c("State", "County", "id"))

df_bf_ddm_adjusted <- df %>%mutate (ratio=delta * (ddm_year_2.5_so4/ddm_summer_2.5_so4))

pm2.5_new_std_non_attain_ctm <- merge(pm2.5_new_std_non_attain, df_bf_ddm_adjusted, by = c ("State", "County"))

counties_attainment <- pm2.5_new_std_non_attain_ctm %>% mutate(attainment= annual.2022_pm2.5- ratio)

standard <- 9


counties_attainment %>% filter(attainment <9) %>% 
  ggplot() +
  geom_segment( aes(x=reorder(id ,attainment), 
                    xend=id, y=attainment, yend=annual.2022_pm2.5), color="grey") +
  geom_point( aes(x=id, y=attainment), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=id, y=annual.2022_pm2.5), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_ipsum() +
  geom_hline(yintercept = standard,   color = "black", size=1 ) +
  theme( legend.position = 'none',
         axis.text  = element_text( angle = 0, size=16),
         axis.title = element_blank()) +
  xlab("") +
  ylab("pm25_no_coal.egus_same_nox")



# How many facilities would need to close to being counties into attainment?

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

counties_attainment_fips<- merge(counties_attainment, county_state_fips, by = c ("State", "County"))


#yearly emission
PP.units.yearly2022 <- PP.units.monthly2022 %>% dplyr::select(Facility.Name,
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

#attainment counties by controlling coal EGUs
attainment_counties <- counties_attainment_fips %>% filter (attainment<9)
attainments_fips <- as.vector(attainment_counties$fips)

#number of facilities in those attainment counties that needs to be under control
egus_facilities_attainment <- PP.units.yearly2022 %>% filter (fips %in% attainments_fips)

# Locations EPA determined to be out of attainment in 2022 and 2023 that would meet 
# standard by shuttering coal plants
counties_non_attainment_vec_2022 <- unique(counties_attainment_fips$id) 

counties.us <- counties.us %>% mutate(id = paste(State, County, sep = "-")) 

counties.us <- counties.us %>% filter (!State %in% c( 'AK', 'HI', 'PR'))

counties.us_2022 <- counties.us

counties.us_2022$group <- ifelse (counties.us_2022$id %in% counties_non_attainment_vec_2022, 
                                  "non-attainment", 
                                  "attainment")

# Define fill colors for different groups
group_colors <- c("attainment" = "white",
                  "non-attainment" = "red")


ggplot() + geom_sf( data = counties.us, fill="white", color= "black") +
  geom_sf( data = counties.us_2022, aes(fill=group)) +
  scale_fill_manual(values = group_colors) +theme_bw() +
  theme(legend.position = c( 0.87, .12), 
        legend.direction = "vertical" ,
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        text = element_text(size=10))


# answered above for 2022

# Figure with 2 maps showing annual average SO2 EGU PM2.5 sensitivities; 
# one showing DDM sensitivities, one showing brute force projected to annual with DDM sensitivities.
# [include outlines of counties marked for non-attainment in 2022 and 2023]




# Figure showing before/after assuming 9µg/m3 standard in out of attainment counties in 2022 and 2023. 

#2022
counties_attainment %>% filter(attainment <9) %>% 
  ggplot() +
  geom_segment( aes(x=reorder(id ,attainment), 
                    xend=id, y=attainment, yend=annual.2022_pm2.5), color="grey") +
  geom_point( aes(x=id, y=attainment), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=id, y=annual.2022_pm2.5), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_ipsum() +
  geom_hline(yintercept = standard,   color = "black", size=1 ) +
  theme( legend.position = 'none',
         axis.text  = element_text( angle = 0, size=16),
         axis.title = element_blank()) +
  xlab("") +
  ylab("pm25_no_coal.egus_same_nox")



# Additional analysis: for each county,
# what fraction of PM2.5 from coal SO2 would need to be cut to meet the 9µg m 3 standard?
#ratio is projected 2022 concentration
counties_attainment_fraction <- counties_attainment %>% filter(attainment<9) %>% mutate(fraction=ratio*100/annual.2022_pm2.5 )

counties.us_p4s <- counties.us %>% st_transform(p4s)

counties.us_2022_p4s <- counties.us_2022 %>% st_transform(p4s)

proj_2022_plot + geom_sf( data = counties.us_p4s, fill="NA", color= "black") 

ggsave("proj_2022_plot.png", path = "./plots/", width=12, height=8, units="in")

ddm_yr_plot + geom_sf( data = counties.us_p4s, fill="NA", color= "black") 

ggsave("ddm_yr_plot.png", path = "./plots/", width=12, height=8, units="in")


# ============linearity

dt_wide_pm_so4<- dt_wide %>%  dplyr::select(ID, sens, PM25_SO4) 

dt_wide_pm_so4<- reshape2::dcast (dt_wide_pm_so4, ID ~ sens, value.var = "PM25_SO4")

summary(dt_wide_pm_so4)
#take mean values
df<- data.frame(so2=c(0,0,1,1,12.45,12.45), nox=c(0,1,0,1,0,2.8), value=c(0.4272,
                                                                     0.4725,
                                                                     0.5192,
                                                                     0.5946,
                                                                     1.0034,
                                                                     1.0034))

# Reorder dataframe based on the SO2 column in decreasing order
df <- df[order(-df$so2), ]

# Create scatter plot
ggplot(df, aes(x = reorder(so2, -so2), y = nox, z= value, size = value)) +
  geom_point() +
  scale_size_continuous(range = c(10, 3)) +  # Adjust the range of point sizes
  labs(x = "SO2", y = "NOx", size = "Value") +  # Labels for axes and legend
  theme_minimal() + # Use a minimal theme+ 
theme_void() +
  axes_3D() +
  stat_3D()

library(plotly)
so2=c(0,0,1,1,12.45,12.45)
nox=c(0,1,0,1,0,2.8)
value=c(0.4272,
                                                          0.4725,
                                                          0.5192,
                                                          0.5946,
                                                          1.0034,
                                                          1.0034)
plot_ly(x=nox, y=so2, z=value, type="scatter3d", mode="markers", color=value)



# Load required library
library(scatterplot3d)

# Create 3D scatter plot
scatterplot3d(df$so2,  df$value, df$nox,
              xlab = "SO2", ylab = "NOx", zlab = "Value",
              color = "blue", pch = 16,
              main = "3D Scatter Plot of SO2, NOx, and Value",  angle = 120)



# Sample data
data <- c("so2_ptegu_12.45_nox_2.8", "so2_ptegu_1_nox_0", "so2_ptegu_0_nox_0",
          "so2_ptegu_12.45_nox_0", "so2_ptegu_0_nox_1", "so2_ptegu_1_nox_1")

# Extract SO2 and NOx values using regular expressions
so2_values <- as.numeric(gsub(".*so2_ptegu_(\\d+\\.?\\d*)_nox_.*", "\\1", data))
nox_values <- as.numeric(gsub(".*nox_(\\d+\\.?\\d*)$", "\\1", data))

# Print the extracted values
print(so2_values)
print(nox_values)





