
# setwd('~/R 2021/Climate_Scenarios/SSP126/CMCC-CM2-SR5/netcdf')
setwd('~/R 2021/Climate_Scenarios/SSP126/CESM2-WACCM/netcdf')
setwd('~/R 2021/Climate_Scenarios/SSP585/CESM2-WACCM/netcdf') 
setwd('~/R 2021/Climate_Scenarios/Historical/CESM2-WACCM/netcdf') #This one
setwd('~/R 2021/Climate_Scenarios/SSP126/NorESM2-MM/netcdf')

library(tidync)
library(dplyr)
library(ncdf4)
library(RNetCDF)
library(raster)
library(plyr)
library(rgdal)
library(ggplot2)
library(ncdfgeom)
library(RColorBrewer)  # For colors
library(maps)          # For World map
library(rasterVis)
library(viridis)
library(broom)
library(tidyverse)

#Read in Data
ncname <- "fNdep_Emon_CMCC-CM2-SR5_ssp126_r1i1p1f1_gn_201501-210012_CONUS.nc"  #SSP125, 2015-2100
ncname1 = 'fNdep_Emon_CESM2-WACCM_ssp585_r1i1p1f1_gn_201501-210012_CONUS.nc'   #SSP125, 2015-2100
ncname2 = 'nLeaf_Emon_CESM2-WACCM_ssp585_r1i1p1f1_gn_201501-210012_CONUS.nc'   #SSP125, 2015-2100
ncname3 = 'prveg_Lmon_CESM2-WACCM_historical_r1i1p1f1_gn_198401-201412_CONUS.nc'   #SSP125, 2015-2100

# ncin = open.nc(ncname1)
ncin = nc_open(ncname3)
# nc_get = ncvar_get(ncin)
# print.nc(ncin)
# dat <- read.nc(ncin)

# lon <- var.get.nc(ncfile = ncin, variable = 'lon')
# lat <- rev(var.get.nc(ncfile = ncin, variable = 'lat')) # Plotting requires incremented values
# flux <- var.get.nc(ncin, variable = 'fNdep') #fNdep
# title <- att.get.nc(ncin, variable = 'fNdep', attribute = 'long_name')
# title <- gsub('_', ' ', title)   

# Visualize netcdf
#TO DO: ANIMATE BY YEAR (CHANGE FLUX SLICE - https://stackoverflow.com/questions/58531225/r-convert-raster-stack-or-brick-to-an-animation)
# flux_slice <- flux[, , 1] 
# r <- raster(t(flux_slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# r <- flip(r, direction='y')
# states <- map_data("state")
# levelplot(r, 
#           margin=FALSE,                       # suppress marginal graphics
#           colorkey=list(
#             space='bottom',                   # plot legend at bottom
#             labels=list(at=-5:5, font=4),
#             axis.line=list(col='black')    
#           ),    
#           par.settings=list(
#             axis.line=list(col='transparent') # suppress axes and legend outline
#           ),
#           scales=list(draw=FALSE),            # suppress axis labels
#           col.regions=viridis)                 # colour ramp


# Extract data from netCDF
# nc_dim_list <- ncin$dim
obsdates = as.Date(ncin$dim$time$vals,origin = '1850-01-01')
#Get the whole data first
obsoutput = ncdf4::ncatt_get(ncin, varid = 'prveg') #fNdep or nLeaf or prveg

#function to get many lat/lon combos
for (i in 1:nrow(ntn_dvi)){
  obsoutput <- ncvar_get(ncin, varid = 'prveg',   #fNdep or nLeaf or prVeg
               start= c(which.min(abs(ncin$dim$lon$vals - ntn_dvi$lon[i])), # look for closest long
               which.min(abs(ncin$dim$lat$vals - ntn_dvi$lat[i])), 1), # look for closest lat
               count = c(1,1,-1)) #count '-1' means 'all values along that dimension'that dimension'
  assign(paste0("DF", i), data.frame(dates= obsdates, obs = obsoutput, lat = ntn_dvi$lat[i], lon = ntn_dvi$lon[i]))
  }

#rbind all dataframes
objs <- mget(ls(envir = globalenv()))
dfs <- objs[map_lgl(objs, is.data.frame)]
a <- ldply(dfs, data.frame)
filt = a[a$.id != 'ntn_dvi',]
filta = filt[,1:5]
write.csv(filta, 'prveg_Emon_CESM2-WACCM_ssp585_obs.csv')
        