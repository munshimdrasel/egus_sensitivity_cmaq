# A sample plot of cmaq output file on R

library(fields)
library(maps)
library(htmlwidgets)
library(proj4)
library(downloader)
library(tidyverse)
library(fst)
library(stringi)
library(plyr)
library( sf)
library( raster)
library( data.table)
library(tidycensus, quietly = TRUE)
library(tigris, quietly = TRUE)
library( fasterize)
library( USAboundaries)
library( magrittr)
library( ncdf4)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

#on my laptop
setwd ("/Users/munshirasel/Library/CloudStorage/GoogleDrive-munshimdrasel@gwmail.gwu.edu/My Drive/R/egus_sensitivity_cmaq")

#on hopper
setwd ("/Users/munshirasel/Library/CloudStorage/GoogleDrive-munshimdrasel@gwmail.gwu.edu/My Drive/R/egus_sensitivity_cmaq")

months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

for (i in 1: length(months)) {
  ## define file name and open ncdf file
  # (anthropogenic so2 emissions from CEDS data)
  cctm.file <- paste0("./data/combine_egus_so2/HR2DAY_ES2_ASENS_v54_DDM3D_gcc_AQF5X_2020",months[i],".nc")
  
  # cctm.file <- './data/combine_egus_so2/HR2DAY_ES2_ASENS_v54_DDM3D_gcc_AQF5X_2020.nc'
  
  #coordinate reference system projection string for spatial data
  p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"
  
  #> Open the CCTM file.
  cctm.in <- nc_open(cctm.file)
  
  #> Print information about a netCDF file, including the variables and dimensions it contains.
  print(cctm.in)
  
  # names of the variables
  names( cctm.in$var)
  names( cctm.in$dim)
  
  
  
  #> Create a list of all model variables in cctm.file. 
  #> CMAQ netcdf files are formatted following I/O API conventions (https://www.cmascenter.org/ioapi/).  I/O API is a data storage library designed specifically for CMAQ data. 
  #> A variable called “TFLAG” will always be the first variable listed in a CMAQ I/O API file, so remove the first element of the list.
  all.mod.names <- unlist(lapply(cctm.in$var, function(var)var$name))[-1]
  
  #> Create a list units for all the model variables in the cctm.file. 
  #> A variable called “TFLAG” will always be the first variable listed in an I/O API file, so remove the first element of the list.
  #> Use gsub to strip out extra spaces.
  all.mod.units <- gsub(" ","",unlist(lapply(cctm.in$var, function(var)var$units))[-1])
  
  #> Pull out the time steps and the grid coordinates associated with the data.
  #> These functions from the M3 library are wrappers for functions in the ncdf4 package.
  
  # M3 package is no longer availalble
  
  ##read files with helper M3 functions
  source("./R/m3_package_functions.R")
  
  date.seq <- get.datetime.seq(cctm.file)
  format.date.seq <- format.Date(date.seq,"%m/%d/%Y")
  
  #> Lambert projected coordinates of the grid cell CENTERS (unit=km).
  #> These are the unique x, y coordinates of the grid cell centers -- NOT the coordinates for every grid cell, since the data are on a regular grid.
  #> You can also use the get.coord.for.dimension() function to extract the grid cell edges by changing the “position” argument.
  x.proj.coord <- get.coord.for.dimension(cctm.file,"col")$coords
  length(x.proj.coord)
  #[1] 442
  y.proj.coord <- get.coord.for.dimension(cctm.file,"row")$coords
  length(y.proj.coord)
  #[1] 265
  
  #> Also get the grid cell centers of all of the grid cell with units=meters.  We will use this later when we convert the data to an object raster.
  xy.proj.coord.meters <- get.matrix.all.grid.cell.ctrs(cctm.file,units="m")$coord
  dim(xy.proj.coord.meters)
  #[1] 117130      2
  
  #> Convert lambert coordinates for grid cell centers to long/lat
  long.lat.coord <- as.data.frame(project.M3.to.lonlat(xy.proj.coord.meters[,1],xy.proj.coord.meters[,2],cctm.file,units="m")$coord)
  
  #> Projection information character string that can be used by the R package rgdal.
  proj.string <- get.proj.info.M3(cctm.file)
  proj.string
  #[1] "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"
  
  #> US, Canada, Mexico map lines in Lambert projected coordinates.
  # map.lines <- get.map.lines.M3.proj(cctm.file,database="canusamex")$coords
  
  #> Extract PM25_AVG (daily 24hr LST averaged PM2.5 concentrations for 1/1/2017 - 12/31/2017).
  mod.name <- "PM25_TOT_AVG"
  mod.unit <- all.mod.units[all.mod.names==mod.name]
  mod.array <- ncvar_get(cctm.in,var=mod.name) 
  dim (mod.array)
  #[1] 442 265 31
  
  #> Create annual average.
  mod.annual.avg <- apply(mod.array,c(1,2),mean)
  dim(mod.annual.avg)
  #[1] 442 265
  
  
  #> A 'nice' color palette to try for mapping. 
  # my.color.palette <- colorRampPalette(c("#E6E6E6","#999999","#56B4E9","#0072B2","#009E73","#F0E442","#E69F00","#D55E00","#A52A2A"))
  # my.range <- c(0,25)
  # my.color.bins <- seq(0,25,by=1)
  # n.colors <- length(my.color.bins)-1
  # my.colors <- my.color.palette(n.colors)
  #> Spatial map of annual average PM25 for 2017.
  # image.plot(x.proj.coord,y.proj.coord,mod.annual.avg,col=my.colors,breaks=my.color.bins,legend.args=list(text=paste(mod.unit)),xlab="",ylab="",axes=F,main=paste("Annual Average",mod.name)) 
  #> Add US, Canada, Mexico map lines
  # lines(map.lines)
  
  
  
  #> Create raster object using projection information extracted from the I/O API file. 
  xyz <- data.frame(x=xy.proj.coord.meters[,1],y=xy.proj.coord.meters[,2],z=matrix(mod.annual.avg))
  mod.raster <- rasterFromXYZ(xyz,crs=proj.string)
  
  # plot(mod.raster)
  
  #coordinate reference system projection string for spatial data
  p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"
  
  mod.sf <- rasterToPolygons(mod.raster) 
  
  r_poly_transformed <- spTransform(mod.sf, CRS(p4s))
  
  # Convert the transformed polygons to an sf object
  sf_object <- st_as_sf(r_poly_transformed)
  
  # Print the resulting sf object
  print(sf_object)
  
  
  us_states <- USAboundaries::us_states()  %>%  filter(!stusps %in% c("PR", "HI", "AK") )
  states.vec <- as.vector(us_states$stusps)
  states.name <- as.vector(us_states$name)
  
  df.list <- list()
  
  for (j in 1:length(states.vec)) {
    block.groups <- block_groups(states.vec[j], year=2020)
    
    block.groups.transform <- block.groups %>% st_transform(crs=p4s)
    bg.selected <- block.groups.transform %>% dplyr::select( GEOID, geometry)
    
    conc.bg <- st_interpolate_aw( sf_object["z"], block.groups.transform,   extensive = F)
    vec <- as.vector(intersect(bg.selected$geometry, conc.bg$geometry))
    bg.new <- bg.selected %>%  filter(geometry %in% vec)
    conc.bg$species <- "pm2.5" #FOR urban change 3 to 2
    conc.bg$month <- months[i]
    conc.bg$state <- states.vec[j]
    conc<- cbind (bg.new,conc.bg, by="geometry")%>% dplyr::select(-geometry.1)
    # ggplot( conc) + geom_sf( aes( fill = z,  geometry = geometry),
    #                        color = NA) +scale_fill_viridis_c( option="plasma")
    # 
    # cbind(bg.selected, conc.bg)
    df.list[[j]] <- conc
    
  }  
  
  df <-  do.call (rbind, df.list) 
  
  save(df, file=paste0("./data/egus_so2_pm2.5/egus_so2_pm25_", months[i],".RData"))
}



