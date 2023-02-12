

#####################
# not being used



####################################################
### library(hydroGOF)		# for nse calculations
library(data.table)
library(lubridate)
library(ncdf4)
library(ncdf4)
library(easyNCDF)	# for ArrayToNc()
#library(robslopes)	# for TheilSen()
#library(trend)		# for sens.slope()
library(mblm)		# for sens slope mlbm()
library(raster)


#########################################
# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\SeaLevelRise\\globalDEM\\"
ncname = 'srtm30plus_v11_land.nc'
ncin = nc_open(paste0(ncpath, ncname))
nc_lon = ncvar_get(ncin, 'lon')
nc_lat = ncvar_get(ncin, 'lat')
nc_close(ncin)
nc_elev = brick(paste0(ncpath, ncname), 'elev')
nc_floodDepth50_10 = nc_elev
nc_floodDepth50_20 = nc_elev
nc_floodDepth50_30 = nc_elev
nc_floodDepth50_40 = nc_elev
nc_floodDepth50_50 = nc_elev
nc_floodDepth50_60 = nc_elev
nc_floodDepth50_70 = nc_elev
nc_floodDepth50_80 = nc_elev
nc_floodDepth50_90 = nc_elev

nc_floodDepth25 = nc_elev
nc_floodDepth75 = nc_elev
nc_floodDepthPct = nc_elev
nc_floodTrend = nc_elev
nc_flooddignif = nc_elev

whichDecades = 1:9
valueType = 1:6

dataOutArray = array(rep(NA, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(seaL_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))


# saving ncdf
ncpath = "J:\\Cai_data\\TCFD\\SeaLevelRise\\"
ncVarFileName = 'sea_level_change'
#ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncOutputPath, ncVarFileName, '_glblGrdv2_processed.nc'))
seaL_nc = nc_open(paste0(ncpath, ncVarFileName, '_glblGrdv2_processed.nc'))
seaL_lat = ncvar_get(seaL_nc, 'lat')	# lat is given from high to low
seaL_lon = ncvar_get(seaL_nc, 'lon')
seaL_scen = ncvar_get(seaL_nc, 'rcpScen')
seaL_elev = ncvar_get(seaL_nc, 'tcfdVariable')

for(i in 1:length(seaL_scen))	{
	thisScen = seaL_scen[i]
	
	for(elevLat in 1:length(nc_lat))	{
			# pulling in elevation data for this latitude
		theseElev = nc_elev[elevLat, ]
		
			# ensuring we are not drawing from only water tiles
		if(any(!is.na(theseElev)))	{
				# subset of sea level latitudes for a later grid search
			closestSeaLats = order(abs(seaL_lat - nc_lat[elevLat]))[1:3]

			maxFlood = max(seaL_elev[ , closestSeaLats, , i, 6], na.rm=TRUE)
			nonaElevs = which(!is.na(theseElev))
			
			for(elevLon in 1:length(nonaElevs))	{
				thisLon_loc = nonaElevs[elevLon]
				thisLon = nc_lon[thisLon_loc]
				thisElev = theseElev[thisLon_loc]

					# subset of sea level locations 
				closestSeaLons = order(abs(seaL_lat - thisLon))[1:3]
				thisSeaL = mean(seaL_elev[closestSeaLons, closestSeaLats, thisDecade, i, 6], na.rm=TRUE) / 1000
				thisFloodDepth = thisElev - thisSeaL

				nc_floodDepth50_10[elevLat, thisLon_loc] = ifelse(thisFloodDepth < 0, thisFloodDepth, 0)

	
	nonaElevs

			maxFlood = max(seaL_elev[ , closestSeaLats, , i, 6], na.rm=TRUE)





ncpath = "J:\\Cai_data\\TCFD\\SeaLevelRise\\highResIndicators\\"
ncname = 'future_surge_actual-value_2021-2050_ensemble-median_50-percentile_v1.nc'
ncin = nc_open(paste0(ncpath, ncname))
nc_lon = ncvar_get(ncin, 'station_x_coordinate')
nc_lat = ncvar_get(ncin, 'station_y_coordinate')
nc_seaLevel = ncvar_get(ncin, 'ensemble_median_surge_50')
	#lol, each dimension is the same length, so one lon and one lat per variable of interest

library(ggplot2)
library(sf)
geomData = st_as_sf(data.frame(lat = nc_lat, lon = nc_lon, seaLevel = nc_seaLevel), coords = c('lon', 'lat'))
ggplot()	+
	geom_sf(data = geomData, aes(fill = seaLevel, color = seaLevel))















