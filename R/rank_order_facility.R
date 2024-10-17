library( data.table)
library( ggplot2)
library( viridis)
library( readxl)
library( sf)
library( ncdf4)
library( raster)
library( fst)
library( tidyverse)
library( cowplot)

 
setwd("/Users/munshirasel/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/gmu_one_drive_rasel/R/egus_sensitivity_cmaq")


#coordinate reference system projection string for spatial data
p4s <-   '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m'


# load major metropolitan areas
# from https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk
# met.by.pop <- fread( '~/Dropbox/Harvard/Manuscripts/MobileBiasMS/metro_populations/PEP_2018_PEPANNCHG.US24PR/PEP_2018_PEPANNCHG.US24PR_with_ann.csv',
#                      check.names = T)
# met.by.pop.descript <- met.by.pop[1]
# met.by.pop <- met.by.pop[-1]
# met.by.pop[ ,GC.display.label := gsub( "[^[:alnum:]]", ' ', GC.display.label)]
# met.by.pop <- met.by.pop[ -grep( ' PR Metro', GC.display.label)]
# 
# met.by.pop.X <- met.by.pop[ as( rank72018, 'numeric') <= 20]
# MSA.codes <- unique( met.by.pop.X$GC.target.geo.id2)

# load counties spatial object
# counties.all <- USAboundaries::us_counties( ) %>%
#   st_transform( p4s)
# counties.all$state_name <- NULL
# 
# # read in table of msa codes
# county.msa <- data.table( read_excel( '~/Dropbox/Harvard/Manuscripts/MobileBiasMS/metro_populations/list1_Sep_2018.xls',
#                                       skip = 2))
# county.use <- county.msa[ `CBSA Code` %in% MSA.codes & !is.na( `CBSA Code`)]
# 
# # merge to get county-MSA spatial object
# county.msa.sf <- sf::st_as_sf( merge( county.use, counties.all,
#                                       by.y = c( 'statefp', 'countyfp'), 
#                                       by.x = c( 'FIPS State Code', 'FIPS County Code')))
# county.msa.sf <- sf::st_transform( county.msa.sf, crs.use)
# 
# # combine counties into single county's
# msa.sf <- county.msa.sf %>%
#   group_by( `CBSA Code`, `CBSA Title`) %>%
#   summarize %>%
#   st_as_sf()

## ================================================================ 
# take over HyADS
## ================================================================ 
# read in hyads
# gridded hyads file location
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



#county info

counties.us <- USAboundaries::us_counties( ) %>% dplyr::select(state_abbr, name, geometry)

names(counties.us)[names(counties.us) == 'state_abbr'] <- 'State'
names(counties.us)[names(counties.us) == 'name'] <- 'County'

counties.us <- counties.us %>% filter (!State %in% c( 'AK', 'HI', 'PR')) %>%
  mutate(id= paste0(State, "-", County)) 


## ================================================================ 
# EGU data
## ================================================================ 
## read the unit dataset
annual_dat <- fread( '/Users/munshirasel/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/gmu_one_drive_rasel/R/ampd-raw-data-processing/data/AMPD_Unit_2023Q3.csv')

units_and_facs_locs <- unique( annual_dat[, .( Facility.Latitude,
                                               Facility.Longitude,
                                               Facility.Name,
                                               State,Facility.ID..ORISPL.,
                                               Unit.ID)]) %>% na.omit()

units_and_facs_locs [, uID := paste(Facility.ID..ORISPL., Unit.ID, sep = ".")]



#facility contr


## ================================================================ 
# area weight over county
## ================================================================ 
hyads_unit_county <- 
  lapply( unique( grid.unit.dat$year),
          function( yy){
            print( yy)
            grid.unit.dat.y.sf <- grid.unit.dat[ year == yy] %>%
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
            
            grid.unit.dat.y.m %>% filter (id== "AL-Geneva")
            
            
            # add year and uID
            grid.unit.dat.y.m[, `:=` (year = yy,
                                      uID = gsub( '^X', '', uID))]
            
            
            #reading ampd data 
            load ("/Users/munshirasel/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/gmu_one_drive_rasel/R/ampd-raw-data-processing/data/PP.units.monthly1997_2023.rda")
            
            # taking data for year of 2022
            
            PP.units.monthly2022 <- PP.units.monthly1997_2023 %>% filter (Year==2020) %>% distinct()
            
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
            
            #yearly emission
            PP.units.yearly2022 <- PP.units.monthly2022 %>% dplyr::select(Facility.Name,
                                                                          Fuel1.IsCoal,
                                                                          State,
                                                                          County,
                                                                          Facility.ID..ORISPL.,
                                                                          Unit.ID,
                                                                          facility.id,
                                                                          Facility.Latitude,
                                                                          Facility.Longitude,
                                                                          uID) %>% distinct() %>% unique()
            
            
            
            PP.units.yearly2022<-  merge(PP.units.yearly2022, county_state_fips, by= c("State","County"))
            
            
            PP.units.yearly2022<-  merge(PP.units.yearly2022, county_state_fips, by= c("State","County", "FIPS", "fips", "statefp")) %>% distinct()
            
            # merge with & sum by facility info
            # unit_fac_xwalk <- fread( '~/Dropbox/GeorgeMason/Grants/2020/EPA_Empower/data/unit_fac_crosswalk.csv')
            # unit_fac_xwalk_edit <- unit_fac_xwalk[ !duplicated( uID), 
            #                                        .( FacID, uID, Facility.Name, State)] %>%
            #   unique()
            
       
            grid.unit.dat.y.m.f <- merge( grid.unit.dat.y.m, PP.units.yearly2022,
                                          by = 'uID')
            
            # sum by facility data
            fac.dat <- grid.unit.dat.y.m.f[, .( hyads = sum( hyads)),
                                           by = .(Facility.Name, State, County, fips)]
            
            # do the ranking
            fac.dat[, `:=` ( rank = frankv( hyads, order = -1, ties.method = 'first')), 
                    by = fips]
            
            return( fac.dat)
          }) %>% rbindlist()

hyads_unit_county[is.na( hyads), hyads := 0]
hyads_unit_county[rank == 1 ] %>%
  summary()

hyads_unit_county[County == "Vermillion" ] %>%
  summary()


hyads_unit_county <- hyads_unit_county %>%  mutate(id= paste0(State, "-", County)) 

pm2.5_std_2032 <- c (9)

projected_2032 <- read.csv(paste0("./data/epa_non_attainment/Source 2032_projections for Design Values greater than or equal to ", pm2.5_std_2032, ".csv")) %>% 
  dplyr::select(-Table.2A.8.PM2.5.DVs.for.2032.Projection) %>% mutate(projected_std = pm2.5_std_2032)

names(projected_2032)[names(projected_2032) == 'Annual.2032.DV..ug.m..3.'] <- 'annual.2032_pm2.5'

projected_2032 <- projected_2032 %>% mutate(annual.2032_pm2.5=as.numeric(annual.2032_pm2.5)) %>% na.omit() %>% 
   mutate(id= paste0(State, "-", County)) 


county.vec.2032 <- as.vector(unique(projected_2032$id))

hyads_unit_county %>% filter (id %in% county.vec.2032)

hyads_unit_county %>% filter (id %in% county.vec.2022)
