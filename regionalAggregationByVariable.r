########################################################################################################################
#reading in functions and libraries
library(data.table)
library(ncdf4)
library(sf)
library(rnaturalearth)
library(raster)
#library(rnaturalearthdata)
library(spdep) # for queens / poly2nb
sf_use_s2(FALSE)

# paths and values that are unlikely to change between runs
thisDate = Sys.Date()
thisWD = 'J:\\Cai_data\\TCFD\\'	# '~//'
hazardFolder = paste0(thisWD, 'ProcessedNCs\\')
ncpathRiverFloods = paste0(thisWD, 'Flash Floods\\')
floodMapTiffLoc = paste0(thisWD, 'CurrentFloodHazard')
waterMaskLoc = paste0(thisWD, 'CurrentFloodHazard\\LandMaskArray.rds')
ncpathDEM = paste0(thisWD, 'SeaLevelRise\\globalDEM\\srtm30plus_v11_land.nc')
ncpathSeaLevelRise = paste0(thisWD, 'SeaLevelRise\\sea_level_change')


#################################################################
#### section does not need to be rerun
	# read in regional boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")
india_all = subset(world, sovereignt == "India")

watersheds = st_read("J:\\Cai_data\\BasinATLAS_Data_v10\\BasinATLAS_v10\\BasinATLAS_v10_lev05.shp")
indiaIntersects = st_intersects(watersheds, india_all)
india = watersheds[lengths(indiaIntersects) > 0 , 1]
#india = sf::st_simplify(india, dTolerance = 5000)
india = sf::st_transform(india, crs="+proj=longlat +datum=WGS84")
st_write(india, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\India_Watersheds_lev05_.shp", overwrite = TRUE, append=FALSE)

#india = st_read("J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\India_State_Boundary.shp")
#india = st_read("J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianDistricts\\geoBoundaries-IND-ADM2.geojson")
#### end of section does not need to be rerun
#################################################################





##########################################################
### Section 0: reading in shapefile

	# read in region [note to self: rename from india to myRegion]
india = st_read("J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\India_Watersheds_lev05_.shp")
st_crs(india) = "WGS84"




############################################################################
### Section 1: processing standard hazards
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'dis_WRIbasedDroughtSeverityv2_processed'
thisDecade = 1
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

finalShapefile = geomAvgMrg_sf[,1]

	################################ name your variable
	################################ name your variable
finalShapefile$DroughtSeverity_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)




########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'dis_WRIbasedDroughtSeverityv2_processed'
thisDecade = 4
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

	# read in hazard data
hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# convert .nc to sf object for aggregation
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# aggregate by poly
geomJoin = st_join(india, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
	# filling polys with NaN data (due to some small polys not encompassing a point in the gridded dataset)
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
	# converting to sf object for stitching
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

	
queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$DroughtSeverity_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)






########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'dis_WRIbasedDroughtSeverityv2_processed'
thisDecade = 1
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$DroughtSeverity_percentile_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)







########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'dis_WRIbasedDroughtSeverityv2_processed'
thisDecade = 4
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}
	################################ name your variable
	################################ name your variable
finalShapefile$DroughtSeverity_percentile_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)








###########################################################################################################################
###########################################################################################################################
###########################################################################################################################



	################################ choose your variable
	################################ choose your variable
hazardName = 'WRI_basedStreamflowInterannualVariability_v2_processed'
thisDecade = 1
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$InterannualDroughtSeverity_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)




########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'WRI_basedStreamflowInterannualVariability_v2_processed'
thisDecade = 4
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}
	################################ name your variable
	################################ name your variable
finalShapefile$InterannualDroughtSeverity_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)






########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'WRI_basedStreamflowInterannualVariability_v2_processed'
thisDecade = 1
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$InterannualDroughtSeverity_percentile_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)







########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'WRI_basedStreamflowInterannualVariability_v2_processed'
thisDecade = 4
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$InterannualDroughtSeverity_percentile_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)








###########################################################################################################################
###########################################################################################################################
###########################################################################################################################



	################################ choose your variable
	################################ choose your variable
hazardName = 'burntareav2_processed'
thisDecade = 1
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$WildfireBurndedArea_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)




########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'burntareav2_processed'
thisDecade = 4
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$WildfireBurndedArea_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)






########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'burntareav2_processed'
thisDecade = 1
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$WildfireBurndedArea_percentile_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)







########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'burntareav2_processed'
thisDecade = 4
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$WildfireBurndedArea_percentile_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)









###########################################################################################################################
###########################################################################################################################
###########################################################################################################################



	################################ choose your variable
	################################ choose your variable
hazardName = 'letv2_processed'
thisDecade = 1
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$TropicalStormCat1_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)




########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'letv2_processed'
thisDecade = 4
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$TropicalStormCat1_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)






########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'letv2_processed'
thisDecade = 1
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$TropicalStormCat1_percentile_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)







########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'letv2_processed'
thisDecade = 4
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$TropicalStormCat1_percentile_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)











###########################################################################################################################
###########################################################################################################################
###########################################################################################################################



	################################ choose your variable
	################################ choose your variable
hazardName = 'lehv2_processed'
thisDecade = 1
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$Heatwave_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)




########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'lehv2_processed'
thisDecade = 4
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$Heatwave_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)






########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'lehv2_processed'
thisDecade = 1
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$Heatwave_percentile_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)







########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'lehv2_processed'
thisDecade = 4
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$Heatwave_percentile_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)






###########################################################################################################################
###########################################################################################################################
###########################################################################################################################



	################################ choose your variable
	################################ choose your variable
hazardName = 'lehv2_processed'
thisDecade = 1
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$Heatwave_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)




########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'lehv2_processed'
thisDecade = 4
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$Heatwave_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)






########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'lehv2_processed'
thisDecade = 1
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$Heatwave_percentile_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)







########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'lehv2_processed'
thisDecade = 4
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$Heatwave_percentile_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)











###########################################################################################################################
###########################################################################################################################
###########################################################################################################################



	################################ choose your variable
	################################ choose your variable
hazardName = 'Precipitation_decadalRawVals'
thisDecade = 1
rawOrPercentile = 13
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')


	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$Precipitation_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)




########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'Precipitation_decadalRawVals'
thisDecade = 4
rawOrPercentile = 13
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$Precipitation_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)






########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'Precipitation_decadalRawVals'
thisDecade = 1
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$Precipitation_percentile_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)







########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'Precipitation_decadalRawVals'
thisDecade = 4
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$Precipitation_percentile_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)





########################################################################################################################################################
########################################################################################################################################################

	################################ choose your variable
	################################ choose your variable
hazardName = 'disv2_processed'
thisDecade = 1
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}


	################################ name your variable
	################################ name your variable
finalShapefile$RiverDischarge_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)




########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'disv2_processed'
thisDecade = 4
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

	# read in hazard data
hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# convert .nc to sf object for aggregation
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# aggregate by poly
geomJoin = st_join(india, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
	# filling polys with NaN data (due to some small polys not encompassing a point in the gridded dataset)
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
	# converting to sf object for stitching
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

	
queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$RiverDischarge_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)





########################################################################################################################################################
########################################################################################################################################################

	################################ choose your variable
	################################ choose your variable
hazardName = 'disv2_processed'
thisDecade = 1
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$RiverDischarge_percentile_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)




########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'disv2_processed'
thisDecade = 4
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

	# read in hazard data
hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# convert .nc to sf object for aggregation
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# aggregate by poly
geomJoin = st_join(india, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
	# filling polys with NaN data (due to some small polys not encompassing a point in the gridded dataset)
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
	# converting to sf object for stitching
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

	
queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$RiverDischarge_percentile_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.gpkg", overwrite = TRUE, append=FALSE)






########################################################################################################################################################
########################################################################################################################################################

	################################ choose your variable
	################################ choose your variable
hazardName = 'WRI_based_WaterStress_v2_processed'
thisDecade = 1
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}


	################################ name your variable
	################################ name your variable
finalShapefile$WaterStress_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)




########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'WRI_based_WaterStress_v2_processed'
thisDecade = 4
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

	# read in hazard data
hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# convert .nc to sf object for aggregation
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# aggregate by poly
geomJoin = st_join(india, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
	# filling polys with NaN data (due to some small polys not encompassing a point in the gridded dataset)
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
	# converting to sf object for stitching
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

	
queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$WaterStress_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)





########################################################################################################################################################
########################################################################################################################################################

	################################ choose your variable
	################################ choose your variable
hazardName = 'WRI_based_WaterStress_v2_processed'
thisDecade = 1
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$WaterStress_percentile_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)




########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'WRI_based_WaterStress_v2_processed'
thisDecade = 4
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

	# read in hazard data
hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# convert .nc to sf object for aggregation
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# aggregate by poly
geomJoin = st_join(india, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
	# filling polys with NaN data (due to some small polys not encompassing a point in the gridded dataset)
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
	# converting to sf object for stitching
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

	
queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$WaterStress_percentile_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.gpkg", overwrite = TRUE, append=FALSE)








###########################################################################################################################
###########################################################################################################################
###########################################################################################################################



	################################ choose your variable
	################################ choose your variable
hazardName = 'Streamflow_decadalRawVals'
thisDecade = 1
rawOrPercentile = 13
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')


	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$Streamflow_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)




########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'Streamflow_decadalRawVals'
thisDecade = 4
rawOrPercentile = 13
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$Streamflow_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.gpkg", overwrite = TRUE, append=FALSE)










########################################################################################################################################################
########################################################################################################################################################

	################################ choose your variable
	################################ choose your variable
hazardName = 'WRI_based_WaterDepletion_v2_processed'
thisDecade = 1
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}


	################################ name your variable
	################################ name your variable
finalShapefile$WaterDepletion_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)




########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'WRI_based_WaterDepletion_v2_processed'
thisDecade = 4
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

	# read in hazard data
hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# convert .nc to sf object for aggregation
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# aggregate by poly
geomJoin = st_join(india, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
	# filling polys with NaN data (due to some small polys not encompassing a point in the gridded dataset)
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
	# converting to sf object for stitching
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

	
queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$WaterDepletion_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)





########################################################################################################################################################
########################################################################################################################################################

	################################ choose your variable
	################################ choose your variable
hazardName = 'WRI_based_WaterDepletion_v2_processed'
thisDecade = 1
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$WaterDepletion_percentile_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)




########################################################################################################################################################
########################################################################################################################################################
	
	################################ choose your variable
	################################ choose your variable
hazardName = 'WRI_based_WaterDepletion_v2_processed'
thisDecade = 4
rawOrPercentile = 2
	################################ choose your variable
	################################ choose your variable

	# read in hazard data
hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# convert .nc to sf object for aggregation
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# aggregate by poly
geomJoin = st_join(india, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
	# filling polys with NaN data (due to some small polys not encompassing a point in the gridded dataset)
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
	# converting to sf object for stitching
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

	
queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	################################ name your variable
	################################ name your variable
finalShapefile$WaterDepletion_percentile_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.gpkg", overwrite = TRUE, append=FALSE)



#########





########################################################################################################################################################
########################################################################################################################################################

	################################ choose your variable
	################################ choose your variable
hazardName = 'WRI_based_WaterStress_v2_processed'
thisDecade = 4
rawOrPercentile = 3
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile])) * 100
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}


	################################ name your variable
	################################ name your variable
finalShapefile$WaterStress_trend_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.gpkg", overwrite = TRUE, append=FALSE)





########################################################################################################################################################
########################################################################################################################################################

	################################ choose your variable
	################################ choose your variable
hazardName = 'WRI_based_WaterStress_v2_processed'
thisDecade = 9
rawOrPercentile = 3
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]) * 10000)
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}


	################################ name your variable
	################################ name your variable
finalShapefile$WaterStress_trend_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.gpkg", overwrite = TRUE, append=FALSE)





########################################################################################################################################################
########################################################################################################################################################

	################################ choose your variable
	################################ choose your variable
hazardName = 'dis_WRIbasedDroughtSeverityv2_processed'
thisDecade = 9
rawOrPercentile = 3
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile])* 100)
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}


	################################ name your variable
	################################ name your variable
finalShapefile$DroughtSeverity_trend_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.gpkg", overwrite = TRUE, append=FALSE)





########################################################################################################################################################
########################################################################################################################################################

	################################ choose your variable
	################################ choose your variable
hazardName = 'disv2_processed'
thisDecade = 9
rawOrPercentile = 3
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(india, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg$HYBAS_ID))
geomNN = st_join(india[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}


	################################ name your variable
	################################ name your variable
finalShapefile$Streamflow_trend_2040 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.gpkg", overwrite = TRUE, append=FALSE)










#########################################################################################################################################
#########################################################################################################################################
#########################################################################################################################################

###################################################################
###### Section 2: coastal flood

# aggregate elevation data
ncin = nc_open(paste0(ncpathDEM))
nc_lon = ncvar_get(ncin, 'lon')
nc_lat = rev(ncvar_get(ncin, 'lat'))
indiaBoundBox = st_bbox(geomAvgMrg_sf)
whichIndLons = which(nc_lon <= indiaBoundBox$xmax & nc_lon >= indiaBoundBox$xmin)
whichIndLats = which(nc_lat <= indiaBoundBox$ymax & nc_lat >= indiaBoundBox$ymin)
nc_valsIndia = ncvar_get(ncin)[whichIndLons,whichIndLats]
nc_close(ncin)
nc_elev = brick(paste0(ncpathDEM), 'elev')
elevRaster = raster(brick(paste0(ncpathDEM), 'elev'), layer=1)


	# aggregate sea level data
seaL_nc = nc_open(paste0(ncpathSeaLevelRise, '_glblGrdv2_processed.nc'))
seaL_lat = ncvar_get(seaL_nc, 'lat')	# lat is given from high to low
seaL_lon = ncvar_get(seaL_nc, 'lon')
seaL_elev = ncvar_get(seaL_nc, 'tcfdVariable')




#############################################################
###### Section 2b: individual var runs
###### if you ever see this code JG don't judge me it is super late and I really just need to get this done.... omg why did they wait so late to tell me that they needed this... ugh

	################################ choose your variable
	################################ choose your variable
thisDecade = 1
levelOfFLood = 10 # 2.1336 # 0	
	################################ choose your variable
	################################ choose your variable

	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
		
seaLevData = data.frame(lat = rep(seaL_lat, each=length(seaL_lon)), lon = rep(seaL_lon, length(seaL_lat)),
	plotData = as.vector(seaL_elev[ , , thisDecade, 2, 1]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
seaLevData_sf = st_as_sf(seaLevData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin_seaLev = st_join(india, seaLevData_sf, join = st_nearest_feature)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
geomAvg_seaLev = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin_seaLev, na.action = na.pass)
geomAvgMrg_seaLev = merge(geomAvg_seaLev, india, by = 'HYBAS_ID')
missingLocs = which(!(india$name %in% geomAvgMrg_seaLev$name))
geomNN = st_join(india[missingLocs,], seaLevData_sf, join = st_nearest_feature)
geomAvgMrg_seaLev = dplyr::bind_rows(geomAvgMrg_seaLev, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg_seaLev)

	# nearest neighbor interp
library(spdep)
queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	
pctFlood = data.frame(
	HYBAS_ID = geomAvgMrg_sf$HYBAS_ID, 
	################################ name your variable
	################################ name your variable
	CoastalFlood_Nuisance_2010s = NA)
	################################ name your variable
	################################ name your variable
	
for(i in 1:nrow(geomAvgMrg_sf))	{
	geomJoin_elevationExtnt = crop(elevRaster, extent(geomAvgMrg_sf[i,]))
	geomJoin_elevationMask = mask(geomJoin_elevationExtnt, geomAvgMrg_sf[i,])

	pctFlood[i,2] = 
		length(which(as.vector(geomJoin_elevationMask) < (geomAvgMrg_sf$plotData[i]/1000 + levelOfFLood))) / 
			length(which(!is.na(as.vector(geomJoin_elevationMask))))
	# in Key West, buildings must be 7 ft above sea level (or on piers of that height): https://www.cityofkeywest-fl.gov/671/Construction-in-Flood-Zones
	# 7 ft ~ 2.1336
	print(i)
}

finalShapefile = merge(finalShapefile, pctFlood, by = 'HYBAS_ID')
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)







	################################ choose your variable
	################################ choose your variable
thisDecade = 4
levelOfFLood = 10 # 2.1336 # 0	
	################################ choose your variable
	################################ choose your variable

	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
		
seaLevData = data.frame(lat = rep(seaL_lat, each=length(seaL_lon)), lon = rep(seaL_lon, length(seaL_lat)),
	plotData = as.vector(seaL_elev[ , , thisDecade, 2, 1]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
seaLevData_sf = st_as_sf(seaLevData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin_seaLev = st_join(india, seaLevData_sf, join = st_nearest_feature)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
geomAvg_seaLev = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin_seaLev, na.action = na.pass)
geomAvgMrg_seaLev = merge(geomAvg_seaLev, india, by = 'HYBAS_ID')
missingLocs = which(!(india$name %in% geomAvgMrg_seaLev$name))
geomNN = st_join(india[missingLocs,], seaLevData_sf, join = st_nearest_feature)
geomAvgMrg_seaLev = dplyr::bind_rows(geomAvgMrg_seaLev, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg_seaLev)

	# nearest neighbor interp
library(spdep)
queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	
pctFlood = data.frame(
	HYBAS_ID = geomAvgMrg_sf$HYBAS_ID, 
	################################ name your variable
	################################ name your variable
	CoastalFlood_Nuisance_2040s = NA)
	################################ name your variable
	################################ name your variable
	
for(i in 1:nrow(geomAvgMrg_sf))	{
	geomJoin_elevationExtnt = crop(elevRaster, extent(geomAvgMrg_sf[i,]))
	geomJoin_elevationMask = mask(geomJoin_elevationExtnt, geomAvgMrg_sf[i,])

	pctFlood[i,2] = 
		length(which(as.vector(geomJoin_elevationMask) < (geomAvgMrg_sf$plotData[i]/1000 + levelOfFLood))) / 
			length(which(!is.na(as.vector(geomJoin_elevationMask))))
	# in Key West, buildings must be 7 ft above sea level (or on piers of that height): https://www.cityofkeywest-fl.gov/671/Construction-in-Flood-Zones
	# 7 ft ~ 2.1336
	print(i)
}

finalShapefile = merge(finalShapefile, pctFlood, by = 'HYBAS_ID')
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)








	################################ choose your variable
	################################ choose your variable
thisDecade = 1
levelOfFLood = 3#3.1336 # 0	#10 # 
	################################ choose your variable
	################################ choose your variable

	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
		
seaLevData = data.frame(lat = rep(seaL_lat, each=length(seaL_lon)), lon = rep(seaL_lon, length(seaL_lat)),
	plotData = as.vector(seaL_elev[ , , thisDecade, 2, 1]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
seaLevData_sf = st_as_sf(seaLevData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin_seaLev = st_join(india, seaLevData_sf, join = st_nearest_feature)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
geomAvg_seaLev = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin_seaLev, na.action = na.pass)
geomAvgMrg_seaLev = merge(geomAvg_seaLev, india, by = 'HYBAS_ID')
missingLocs = which(!(india$name %in% geomAvgMrg_seaLev$name))
geomNN = st_join(india[missingLocs,], seaLevData_sf, join = st_nearest_feature)
geomAvgMrg_seaLev = dplyr::bind_rows(geomAvgMrg_seaLev, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg_seaLev)

	# nearest neighbor interp
library(spdep)
queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	
pctFlood = data.frame(
	HYBAS_ID = geomAvgMrg_sf$HYBAS_ID, 
	################################ name your variable
	################################ name your variable
	CoastalFlood_Major_2010s = NA)
	################################ name your variable
	################################ name your variable
	
for(i in 1:nrow(geomAvgMrg_sf))	{
	geomJoin_elevationExtnt = crop(elevRaster, extent(geomAvgMrg_sf[i,]))
	geomJoin_elevationMask = mask(geomJoin_elevationExtnt, geomAvgMrg_sf[i,])

	pctFlood[i,2] = 
		length(which(as.vector(geomJoin_elevationMask) < (geomAvgMrg_sf$plotData[i]/1000 + levelOfFLood))) / 
			length(which(!is.na(as.vector(geomJoin_elevationMask))))
	# in Key West, buildings must be 7 ft above sea level (or on piers of that height): https://www.cityofkeywest-fl.gov/671/Construction-in-Flood-Zones
	# 7 ft ~ 2.1336
	print(c(i, geomJoin_elevationMask, geomAvgMrg_sf$plotData[i]/1000 + levelOfFLood))
}

finalShapefile = merge(finalShapefile, pctFlood, by = 'HYBAS_ID')
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)







	################################ choose your variable
	################################ choose your variable
thisDecade = 4
levelOfFLood = 3.1#3.1336 # 0	#10 # 
	################################ choose your variable
	################################ choose your variable

	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
		
seaLevData = data.frame(lat = rep(seaL_lat, each=length(seaL_lon)), lon = rep(seaL_lon, length(seaL_lat)),
	plotData = as.vector(seaL_elev[ , , thisDecade, 2, 1]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
seaLevData_sf = st_as_sf(seaLevData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin_seaLev = st_join(india, seaLevData_sf, join = st_nearest_feature)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
geomAvg_seaLev = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin_seaLev, na.action = na.pass)
geomAvgMrg_seaLev = merge(geomAvg_seaLev, india, by = 'HYBAS_ID')
missingLocs = which(!(india$name %in% geomAvgMrg_seaLev$name))
geomNN = st_join(india[missingLocs,], seaLevData_sf, join = st_nearest_feature)
geomAvgMrg_seaLev = dplyr::bind_rows(geomAvgMrg_seaLev, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg_seaLev)

	# nearest neighbor interp
library(spdep)
queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	
pctFlood = data.frame(
	HYBAS_ID = geomAvgMrg_sf$HYBAS_ID, 
	################################ name your variable
	################################ name your variable
	CoastalFlood_Major_2040s = NA)
	################################ name your variable
	################################ name your variable
	
for(i in 1:nrow(geomAvgMrg_sf))	{
	geomJoin_elevationExtnt = crop(elevRaster, extent(geomAvgMrg_sf[i,]))
	geomJoin_elevationMask = mask(geomJoin_elevationExtnt, geomAvgMrg_sf[i,])

	pctFlood[i,2] = 
		length(which(as.vector(geomJoin_elevationMask) < (geomAvgMrg_sf$plotData[i]/1000 + levelOfFLood))) / 
			length(which(!is.na(as.vector(geomJoin_elevationMask))))
	# in Key West, buildings must be 7 ft above sea level (or on piers of that height): https://www.cityofkeywest-fl.gov/671/Construction-in-Flood-Zones
	# 7 ft ~ 2.1336
	print(i)
}

finalShapefile = merge(finalShapefile, pctFlood, by = 'HYBAS_ID')
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)










	################################ choose your variable
	################################ choose your variable
thisDecade = 1
levelOfFLood =  1	#10 # 2.1336 #
	################################ choose your variable
	################################ choose your variable

	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
		
seaLevData = data.frame(lat = rep(seaL_lat, each=length(seaL_lon)), lon = rep(seaL_lon, length(seaL_lat)),
	plotData = as.vector(seaL_elev[ , , thisDecade, 2, 1]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
seaLevData_sf = st_as_sf(seaLevData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin_seaLev = st_join(india, seaLevData_sf, join = st_nearest_feature)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
geomAvg_seaLev = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin_seaLev, na.action = na.pass)
geomAvgMrg_seaLev = merge(geomAvg_seaLev, india, by = 'HYBAS_ID')
missingLocs = which(!(india$name %in% geomAvgMrg_seaLev$name))
geomNN = st_join(india[missingLocs,], seaLevData_sf, join = st_nearest_feature)
geomAvgMrg_seaLev = dplyr::bind_rows(geomAvgMrg_seaLev, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg_seaLev)

	# nearest neighbor interp
library(spdep)
queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	
pctFlood = data.frame(
	HYBAS_ID = geomAvgMrg_sf$HYBAS_ID, 
	################################ name your variable
	################################ name your variable
	CoastalFlood_Severe_2010s = NA)
	################################ name your variable
	################################ name your variable
	
for(i in 1:nrow(geomAvgMrg_sf))	{
	geomJoin_elevationExtnt = crop(elevRaster, extent(geomAvgMrg_sf[i,]))
	geomJoin_elevationMask = mask(geomJoin_elevationExtnt, geomAvgMrg_sf[i,])

	pctFlood[i,2] = 
		length(which(as.vector(geomJoin_elevationMask) < (geomAvgMrg_sf$plotData[i]/1000 + levelOfFLood))) / 
			length(which(!is.na(as.vector(geomJoin_elevationMask))))
	# in Key West, buildings must be 7 ft above sea level (or on piers of that height): https://www.cityofkeywest-fl.gov/671/Construction-in-Flood-Zones
	# 7 ft ~ 2.1336
	print(i)
}

finalShapefile = merge(finalShapefile, pctFlood, by = 'HYBAS_ID')
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)







	################################ choose your variable
	################################ choose your variable
thisDecade = 4
levelOfFLood =  1.1	#10 # 2.1336 #
	################################ choose your variable
	################################ choose your variable

	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
		
seaLevData = data.frame(lat = rep(seaL_lat, each=length(seaL_lon)), lon = rep(seaL_lon, length(seaL_lat)),
	plotData = as.vector(seaL_elev[ , , thisDecade, 2, 1]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
seaLevData_sf = st_as_sf(seaLevData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin_seaLev = st_join(india, seaLevData_sf, join = st_nearest_feature)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
geomAvg_seaLev = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin_seaLev, na.action = na.pass)
geomAvgMrg_seaLev = merge(geomAvg_seaLev, india, by = 'HYBAS_ID')
missingLocs = which(!(india$name %in% geomAvgMrg_seaLev$name))
geomNN = st_join(india[missingLocs,], seaLevData_sf, join = st_nearest_feature)
geomAvgMrg_seaLev = dplyr::bind_rows(geomAvgMrg_seaLev, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg_seaLev)

	# nearest neighbor interp
library(spdep)
queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

	
pctFlood = data.frame(
	HYBAS_ID = geomAvgMrg_sf$HYBAS_ID, 
	################################ name your variable
	################################ name your variable
	CoastalFlood_Severe_2040s = NA)
	################################ name your variable
	################################ name your variable
	
for(i in 1:nrow(geomAvgMrg_sf))	{
	geomJoin_elevationExtnt = crop(elevRaster, extent(geomAvgMrg_sf[i,]))
	geomJoin_elevationMask = mask(geomJoin_elevationExtnt, geomAvgMrg_sf[i,])

	pctFlood[i,2] = 
		length(which(as.vector(geomJoin_elevationMask) < (geomAvgMrg_sf$plotData[i]/1000 + levelOfFLood))) / 
			length(which(!is.na(as.vector(geomJoin_elevationMask))))
	# in Key West, buildings must be 7 ft above sea level (or on piers of that height): https://www.cityofkeywest-fl.gov/671/Construction-in-Flood-Zones
	# 7 ft ~ 2.1336
	print(i)
}

finalShapefile = merge(finalShapefile, pctFlood, by = 'HYBAS_ID')
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)










#########################################################################################################################################
#########################################################################################################################################
#########################################################################################################################################

###################################################################
###### Section 3: fluvial flood

	# read in historic floods data
fldDepthList = list()
fldDepthList[[1]] = raster(paste0(floodMapTiffLoc, '\\floodMapGL_rp10y.tif'))
fldDepthList[[2]] = raster(paste0(floodMapTiffLoc, '\\floodMapGL_rp20y.tif'))
fldDepthList[[3]] = raster(paste0(floodMapTiffLoc, '\\floodMapGL_rp50y.tif'))
fldDepthList[[4]] = raster(paste0(floodMapTiffLoc, '\\floodMapGL_rp100y.tif'))
fldDepthList[[5]] = raster(paste0(floodMapTiffLoc, '\\floodMapGL_rp200y.tif'))
fldDepthList[[6]] = raster(paste0(floodMapTiffLoc, '\\floodMapGL_rp500y.tif'))

		
		# read in flood recurrence data
fldRcrIntNC = nc_open(paste0(ncpathRiverFloods, 'floodRecurIntervals_v5.nc'))
fld_lat = ncvar_get(fldRcrIntNC, 'lat')
fld_lon = ncvar_get(fldRcrIntNC, 'lon')
fldRcrVals = ncvar_get(fldRcrIntNC, 'fldRecurIntrvl')
fldRecurs = c(10,20,50,100,200,500)



#######################################################################
###### Section 3b: individual vars
	################################ choose your variable
	################################ choose your variable
thisDecade = 1
thisFloodDepth = 0 # 0.1 # 1
	################################ choose your variable
	################################ choose your variable



	# [lon,lat,decade,recurInterval,valueTypes,rcpScen]
floodLevData = data.frame(lat = rep(fld_lat, each=length(fld_lon)), lon = rep(fld_lon, length(fld_lat)),
	plotData = as.vector(fldRcrVals[ , , thisDecade, 4, 1, 2]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
floodLevData_sf = st_as_sf(floodLevData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin_floodLev = st_join(india, floodLevData_sf, join = st_nearest_feature)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
geomAvg_floodLev = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin_floodLev, na.action = na.omit)
geomAvgMrg_floodLev = merge(geomAvg_floodLev, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg_floodLev$HYBAS_ID))
geomNN = st_join(india[missingLocs,], floodLevData_sf, join = st_nearest_feature)
geomAvgMrg_floodLev = dplyr::bind_rows(geomAvgMrg_floodLev, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg_floodLev)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}


pctFlood = data.frame(
	HYBAS_ID = geomAvgMrg_sf$HYBAS_ID, 
	################################ name your variable
	################################ name your variable
	fluvialFloodRisk_Nuisance_2010_raw = NA)
	################################ name your variable
	################################ name your variable
	
for(i in 1:nrow(geomAvgMrg_sf))	{
	whichFloodLevel = which.min(abs(fldRecurs - geomAvgMrg_sf$plotData[i]))
	geomJoin_elevationExtnt = crop(fldDepthList[[whichFloodLevel]], extent(geomAvgMrg_sf[i,]))
	geomJoin_elevationExtnt[is.na(geomJoin_elevationExtnt[])] <- 0 
	geomJoin_elevationMask = mask(geomJoin_elevationExtnt, geomAvgMrg_sf[i,])

	pctFlood[i,2] = 
		length(which(as.vector(geomJoin_elevationMask) > thisFloodDepth)) / 
			length(which(!is.na(as.vector(geomJoin_elevationMask))))
	print(i)
}

finalShapefile = merge(finalShapefile, pctFlood, by = 'HYBAS_ID')
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)








	################################ choose your variable
	################################ choose your variable
thisDecade = 4
thisFloodDepth = 0 # 0.1 # 1
	################################ choose your variable
	################################ choose your variable



	# [lon,lat,decade,recurInterval,valueTypes,rcpScen]
floodLevData = data.frame(lat = rep(fld_lat, each=length(fld_lon)), lon = rep(fld_lon, length(fld_lat)),
	plotData = as.vector(fldRcrVals[ , , thisDecade, 4, 1, 2]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
floodLevData_sf = st_as_sf(floodLevData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin_floodLev = st_join(india, floodLevData_sf, join = st_nearest_feature)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
geomAvg_floodLev = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin_floodLev, na.action = na.omit)
geomAvgMrg_floodLev = merge(geomAvg_floodLev, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg_floodLev$HYBAS_ID))
geomNN = st_join(india[missingLocs,], floodLevData_sf, join = st_nearest_feature)
geomAvgMrg_floodLev = dplyr::bind_rows(geomAvgMrg_floodLev, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg_floodLev)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}


pctFlood = data.frame(
	HYBAS_ID = geomAvgMrg_sf$HYBAS_ID, 
	################################ name your variable
	################################ name your variable
	fluvialFloodRisk_Nuisance_2040_raw = NA)
	################################ name your variable
	################################ name your variable
	
for(i in 1:nrow(geomAvgMrg_sf))	{
	whichFloodLevel = which.min(abs(fldRecurs - geomAvgMrg_sf$plotData[i]))
	geomJoin_elevationExtnt = crop(fldDepthList[[whichFloodLevel]], extent(geomAvgMrg_sf[i,]))
	geomJoin_elevationExtnt[is.na(geomJoin_elevationExtnt[])] <- 0 
	geomJoin_elevationMask = mask(geomJoin_elevationExtnt, geomAvgMrg_sf[i,])

	pctFlood[i,2] = 
		length(which(as.vector(geomJoin_elevationMask) > thisFloodDepth)) / 
			length(which(!is.na(as.vector(geomJoin_elevationMask))))
	print(i)
}

finalShapefile = merge(finalShapefile, pctFlood, by = 'HYBAS_ID')
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)









	################################ choose your variable
	################################ choose your variable
thisDecade = 1
thisFloodDepth = 0.1 # 1 #0 # 
	################################ choose your variable
	################################ choose your variable



	# [lon,lat,decade,recurInterval,valueTypes,rcpScen]
floodLevData = data.frame(lat = rep(fld_lat, each=length(fld_lon)), lon = rep(fld_lon, length(fld_lat)),
	plotData = as.vector(fldRcrVals[ , , thisDecade, 4, 1, 2]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
floodLevData_sf = st_as_sf(floodLevData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin_floodLev = st_join(india, floodLevData_sf, join = st_nearest_feature)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
geomAvg_floodLev = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin_floodLev, na.action = na.omit)
geomAvgMrg_floodLev = merge(geomAvg_floodLev, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg_floodLev$HYBAS_ID))
geomNN = st_join(india[missingLocs,], floodLevData_sf, join = st_nearest_feature)
geomAvgMrg_floodLev = dplyr::bind_rows(geomAvgMrg_floodLev, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg_floodLev)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}


pctFlood = data.frame(
	HYBAS_ID = geomAvgMrg_sf$HYBAS_ID, 
	################################ name your variable
	################################ name your variable
	fluvialFloodRisk_Major_2010_raw = NA)
	################################ name your variable
	################################ name your variable
	
for(i in 1:nrow(geomAvgMrg_sf))	{
	whichFloodLevel = which.min(abs(fldRecurs - geomAvgMrg_sf$plotData[i]))
	geomJoin_elevationExtnt = crop(fldDepthList[[whichFloodLevel]], extent(geomAvgMrg_sf[i,]))
	geomJoin_elevationExtnt[is.na(geomJoin_elevationExtnt[])] <- 0 
	geomJoin_elevationMask = mask(geomJoin_elevationExtnt, geomAvgMrg_sf[i,])

	pctFlood[i,2] = 
		length(which(as.vector(geomJoin_elevationMask) > thisFloodDepth)) / 
			length(which(!is.na(as.vector(geomJoin_elevationMask))))
	print(i)
}

finalShapefile = merge(finalShapefile, pctFlood, by = 'HYBAS_ID')
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)






	################################ choose your variable
	################################ choose your variable
thisDecade = 4
thisFloodDepth = 0.1 # 1 #0 # 
	################################ choose your variable
	################################ choose your variable



	# [lon,lat,decade,recurInterval,valueTypes,rcpScen]
floodLevData = data.frame(lat = rep(fld_lat, each=length(fld_lon)), lon = rep(fld_lon, length(fld_lat)),
	plotData = as.vector(fldRcrVals[ , , thisDecade, 4, 1, 2]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
floodLevData_sf = st_as_sf(floodLevData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin_floodLev = st_join(india, floodLevData_sf, join = st_nearest_feature)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
geomAvg_floodLev = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin_floodLev, na.action = na.omit)
geomAvgMrg_floodLev = merge(geomAvg_floodLev, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg_floodLev$HYBAS_ID))
geomNN = st_join(india[missingLocs,], floodLevData_sf, join = st_nearest_feature)
geomAvgMrg_floodLev = dplyr::bind_rows(geomAvgMrg_floodLev, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg_floodLev)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}


pctFlood = data.frame(
	HYBAS_ID = geomAvgMrg_sf$HYBAS_ID, 
	################################ name your variable
	################################ name your variable
	fluvialFloodRisk_Major_2040_raw = NA)
	################################ name your variable
	################################ name your variable
	
for(i in 1:nrow(geomAvgMrg_sf))	{
	whichFloodLevel = which.min(abs(fldRecurs - geomAvgMrg_sf$plotData[i]))
	geomJoin_elevationExtnt = crop(fldDepthList[[whichFloodLevel]], extent(geomAvgMrg_sf[i,]))
	geomJoin_elevationExtnt[is.na(geomJoin_elevationExtnt[])] <- 0 
	geomJoin_elevationMask = mask(geomJoin_elevationExtnt, geomAvgMrg_sf[i,])

	pctFlood[i,2] = 
		length(which(as.vector(geomJoin_elevationMask) > thisFloodDepth)) / 
			length(which(!is.na(as.vector(geomJoin_elevationMask))))
	print(i)
}

finalShapefile = merge(finalShapefile, pctFlood, by = 'HYBAS_ID')
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)








	################################ choose your variable
	################################ choose your variable
thisDecade = 1
thisFloodDepth = 1 #0 # 0.1 # 
	################################ choose your variable
	################################ choose your variable



	# [lon,lat,decade,recurInterval,valueTypes,rcpScen]
floodLevData = data.frame(lat = rep(fld_lat, each=length(fld_lon)), lon = rep(fld_lon, length(fld_lat)),
	plotData = as.vector(fldRcrVals[ , , thisDecade, 4, 1, 2]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
floodLevData_sf = st_as_sf(floodLevData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin_floodLev = st_join(india, floodLevData_sf, join = st_nearest_feature)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
geomAvg_floodLev = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin_floodLev, na.action = na.omit)
geomAvgMrg_floodLev = merge(geomAvg_floodLev, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg_floodLev$HYBAS_ID))
geomNN = st_join(india[missingLocs,], floodLevData_sf, join = st_nearest_feature)
geomAvgMrg_floodLev = dplyr::bind_rows(geomAvgMrg_floodLev, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg_floodLev)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}


pctFlood = data.frame(
	HYBAS_ID = geomAvgMrg_sf$HYBAS_ID, 
	################################ name your variable
	################################ name your variable
	fluvialFloodRisk_Severe_2010_raw = NA)
	################################ name your variable
	################################ name your variable
	
for(i in 1:nrow(geomAvgMrg_sf))	{
	whichFloodLevel = which.min(abs(fldRecurs - geomAvgMrg_sf$plotData[i]))
	geomJoin_elevationExtnt = crop(fldDepthList[[whichFloodLevel]], extent(geomAvgMrg_sf[i,]))
	geomJoin_elevationExtnt[is.na(geomJoin_elevationExtnt[])] <- 0 
	geomJoin_elevationMask = mask(geomJoin_elevationExtnt, geomAvgMrg_sf[i,])

	pctFlood[i,2] = 
		length(which(as.vector(geomJoin_elevationMask) > thisFloodDepth)) / 
			length(which(!is.na(as.vector(geomJoin_elevationMask))))
	print(i)
}

finalShapefile = merge(finalShapefile, pctFlood, by = 'HYBAS_ID')
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)






	################################ choose your variable
	################################ choose your variable
thisDecade = 4
thisFloodDepth = 1.0 #0 # 0.1 # 
	################################ choose your variable
	################################ choose your variable



	# [lon,lat,decade,recurInterval,valueTypes,rcpScen]
floodLevData = data.frame(lat = rep(fld_lat, each=length(fld_lon)), lon = rep(fld_lon, length(fld_lat)),
	plotData = as.vector(fldRcrVals[ , , thisDecade, 4, 1, 2]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
floodLevData_sf = st_as_sf(floodLevData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin_floodLev = st_join(india, floodLevData_sf, join = st_nearest_feature)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
geomAvg_floodLev = aggregate(plotData ~ HYBAS_ID, mean, data = geomJoin_floodLev, na.action = na.omit)
geomAvgMrg_floodLev = merge(geomAvg_floodLev, india, by = 'HYBAS_ID')
missingLocs = which(!(india$HYBAS_ID %in% geomAvgMrg_floodLev$HYBAS_ID))
geomNN = st_join(india[missingLocs,], floodLevData_sf, join = st_nearest_feature)
geomAvgMrg_floodLev = dplyr::bind_rows(geomAvgMrg_floodLev, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg_floodLev)

queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}


pctFlood = data.frame(
	HYBAS_ID = geomAvgMrg_sf$HYBAS_ID, 
	################################ name your variable
	################################ name your variable
	fluvialFloodRisk_Severe_2040_raw = NA)
	################################ name your variable
	################################ name your variable
	
for(i in 1:nrow(geomAvgMrg_sf))	{
	whichFloodLevel = which.min(abs(fldRecurs - geomAvgMrg_sf$plotData[i]))
	geomJoin_elevationExtnt = crop(fldDepthList[[whichFloodLevel]], extent(geomAvgMrg_sf[i,]))
	geomJoin_elevationExtnt[is.na(geomJoin_elevationExtnt[])] <- 0 
	geomJoin_elevationMask = mask(geomJoin_elevationExtnt, geomAvgMrg_sf[i,])

	pctFlood[i,2] = 
		length(which(as.vector(geomJoin_elevationMask) > thisFloodDepth)) / 
			length(which(!is.na(as.vector(geomJoin_elevationMask))))
	print(i)
}

finalShapefile = merge(finalShapefile, pctFlood, by = 'HYBAS_ID')
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)


finalShapefile
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.gpkg", overwrite = TRUE, append=FALSE)





















############# global provinces / counties
provinces10 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_admin_1_states_provinces\\ne_10m_admin_1_states_provinces.shp'))



################################ choose your variable
	################################ choose your variable
hazardName = 'cwood-dcddrybdltrv2_processed'
thisDecade = 1
rawOrPercentile = 1
	################################ choose your variable
	################################ choose your variable

hazardMeasureNC = nc_open(paste0(hazardFolder, hazardName, '.nc'))
hazardData = ncvar_get(hazardMeasureNC, "tcfdVariable")
nc_lat = ncvar_get(hazardMeasureNC, 'lat')
nc_lon = ncvar_get(hazardMeasureNC, 'lon')

	# abs values
	# nc in dimensions of [lon/lat, lat/lon, decade, scen, variable]
		#variable med = 1, Q25=5, Q75=6, slope=3, pval=4, percentile=2
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)),
	plotData = as.vector(hazardData[ , , thisDecade, 2, rawOrPercentile]))
#df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ State_Name, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'State_Name')
#geomAvg = aggregate(plotData ~ shapeName, mean, data = geomJoin, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, india, by = 'shapeName')
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin, na.action = na.omit)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
geomAvgMrg_sf = st_as_sf(geomAvgMrg)






ggplot(data = geomAvgMrg_sf) +
	geom_sf(colour = "grey10", aes(fill = plotData, color=NA))	+
	scale_fill_viridis(option = 'viridis', 
		trans = scales::pseudo_log_trans(sigma = 3), 
		direction = 1, name = "kg/m2")	+

#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light(base_size = 22)	+
	theme(panel.ontop=FALSE, panel.background=element_blank(), legend.position = c(.1,.2)) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
#	geom_sf(data=ocean50, fill="white", colour='white')+
#	borders('world', xlim=range(latlonBox_local$lon), ylim=range(latlonBox_local$lat), 
 #           colour='gray90', size=.2)	+
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+

#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
#	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
#	geom_sf(data=locOfInt_sf, col='red1',size=5, shape='+', stroke=3) +
	coord_sf(xlim=c(-170, -30), ylim=c(-60,75), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Timber'),
		subtitle='kg / m2', 
         x="Longitude", y="Latitude", 
         fill='')








queens <- poly2nb(geomAvgMrg_sf, queen = TRUE, snap = 1)
if(any(is.na(geomAvgMrg_sf$plotData)))	{
	for(i in which(is.na(geomAvgMrg_sf$plotData)))	{
		geomAvgMrg_sf$plotData[i] <- geomAvgMrg_sf$plotData[queens[[i]][1]]
	}
}

finalShapefile = geomAvgMrg_sf[,1]

	################################ name your variable
	################################ name your variable
finalShapefile$DroughtSeverity_2010 = geomAvgMrg_sf$plotData
	################################ name your variable
	################################ name your variable
st_write(finalShapefile, "J:\\Cai_data\\TCFD\\PoliticalBoundaries\\IndianStates\\indiaBasicAssessData_MAR2024.shp", overwrite = TRUE, append=FALSE)

























myWaterMask = readRDS(waterMaskLoc)

		# identifying lat lon coordinates for tiffs
	tif_lat = rev(seq(-54.00824,83.2251,length.out=16468))
	tif_lon = seq(-166.8, 180, length.out=41616)

	hazardDepthName = paste0("Avg Flood Depth (m)")
	hazardLikliName = paste0('Flood Areal Extent (%)')



for(thisIntrvl in 1:length(recurIntrvls))	{
		for(thisLoc in 1:nrow(customerTable))	{
			closeTiffLons = which.min(abs(customerTable$Lon[thisLoc] - tif_lon))
			closeTiffLats = which.min(abs(customerTable$Lat[thisLoc] - tif_lat))
			
			theseLats = seq(closeTiffLats - locationFootprint, closeTiffLats + locationFootprint, 1)
			theseLons = seq(closeTiffLons - locationFootprint, closeTiffLons + locationFootprint, 1)
				#ensuring lats / lons don't go outside bounding box
			if(any(theseLats < 1))	{theseLats[theseLats < 1] = 1}
			if(any(theseLats > length(tif_lat)))	{theseLats[theseLats > length(tif_lat)] = length(tif_lat)}
			if(any(theseLons < 1))	{theseLons[theseLons < 1] = 1}
			if(any(theseLons > length(tif_lon)))	{theseLons[theseLons > length(tif_lon)] = length(tif_lon)}

				# defining the water mask
			thisWaterMask = myWaterMask[theseLons, theseLats]

			histFloodDepth = mean(fldDepthList[[6]][theseLats, theseLons], na.rm=TRUE)
			histFloodLikli = 100 * length(which(fldDepthList[[6]][theseLats, theseLons] > 0)) / length(theseLats)^2
			
				# check to see if even 500yr floods trigger historically; if not, the skip next analysis
			if(!is.na(histFloodDepth))	{
					# ensuring we are not drawing from a water tile, then searching box around point if so
			#### !!!! todo: implement new smoothing scheme !!!!!

				closeNCLon = which.min(abs(customerTable$Lon[thisLoc] - nc_lon))
				closeNCLat = which.min(abs(customerTable$Lat[thisLoc] - nc_lat))
				if(is.na(fldRcrVals[closeNCLon, closeNCLat, 1, thisIntrvl, 1, 1]))	{
						closeNCLon = closeNCLon + 1
						if(is.na(fldRcrVals[closeNCLon, closeNCLat, 1, thisIntrvl, 1, 1]))	{
							closeNCLon = closeNCLon - 2
							if(is.na(fldRcrVals[closeNCLon, closeNCLat, 1, thisIntrvl, 1, 1]))	{
								closeNCLon = closeNCLon + 1
								closeNCLat = closeNCLat + 1
								if(is.na(fldRcrVals[closeNCLon, closeNCLat, 1, thisIntrvl, 1, 1]))	{
									closeNCLat = closeNCLat - 2
				}}}}
					
				for(thisScenario in 1:length(rcpScenarios))	{
					fldRcrSignif_all = fldRcrVals[closeNCLon, closeNCLat, 9,  thisIntrvl, 2, thisScenario]
	#				fldRcrDrctn_all = fldRcrVals[closeNCLon, closeNCLat, 9,  thisIntrvl, 3, thisScenario]
					
					for(thisDecade in 1:length(whichDecades))	{
						thisFldRcrVal = 		fldRcrVals[closeNCLon, closeNCLat, thisDecade, thisIntrvl, 1, thisScenario]
						thisFldRcrSignif = 		fldRcrVals[closeNCLon, closeNCLat, thisDecade, thisIntrvl, 2, thisScenario]
	#					thisFldRcrDrctn = 		fldRcrVals[closeNCLon, closeNCLat, thisDecade, thisIntrvl, 3, thisScenario]
						
						if(thisFldRcrVal >= recurIntrvls[1])	{
							closestFldRcrIntrvl = 	last(which(thisFldRcrVal >= recurIntrvls))
							
							theseFloodImpacts = fldDepthList[[closestFldRcrIntrvl]][theseLats, theseLons]
							theseFloodImpacts[is.na(theseFloodImpacts)] = 0
							theseFloodImpacts = theseFloodImpacts * thisWaterMask
		
							prevFloodImpacts = fldDepthList[[thisIntrvl]][theseLats, theseLons]
							prevFloodImpacts[is.na(prevFloodImpacts)] = 0
							prevFloodImpacts = prevFloodImpacts * thisWaterMask
		
							
							if(any(!is.na(theseFloodImpacts)))	{
								avgFloodDepth = mean(theseFloodImpacts, na.rm=TRUE)
								avgFloodDepthChng = avgFloodDepth - mean(prevFloodImpacts, na.rm=TRUE) / max(c(1, thisDecade - 1))

								avgFloodLikli = 100 * (length(which(theseFloodImpacts > 0)) / length(!is.na(theseFloodImpacts)))
								avgFloodLikliChng = avgFloodLikli - 100 * (length(which(prevFloodImpacts > 0)) / length(!is.na(prevFloodImpacts))) / max(c(1, thisDecade - 1))
							

								dataOutput = rbind(dataOutput,
			#### !!!! todo: consolidate and simplify !!!!!
									data.table(
										User = userName,
										Location = customerTable$Location[thisLoc],
										Region = customerTable$Region[thisLoc],
										Subregion = customerTable$Subregion[thisLoc],
										Lat = customerTable$Lat[thisLoc],
										Lon = customerTable$Lon[thisLoc],
										Hazard = "River Flood (Local)",
										Hazard_Measure = c(hazardDepthName, hazardLikliName),
										Decade = 2000 + whichDecades[thisDecade],
										Scenario = rcpScenarios[thisScenario],
										Raw_Hazard_Value = c(avgFloodDepth, avgFloodLikli),					# Raw_Hazard_Value
										Percentile_Score = NA,												# Percentile_Score
										Relative_Hazard_Score = NA,											# Relative_Hazard_Score
										Decadal_Trend_Strength = c(avgFloodDepthChng, avgFloodLikliChng),	# Decadal_Trend_Strength
										Decadal_Trend_Significance = thisFldRcrSignif,						# Decadal_Trend_Significance
										Long_Term_Trend_Strength = NA,										# Long_Term_Trend_Strength
										Long_Term_Trend_Significance = fldRcrSignif_all,					# Long_Term_Trend_Significance					
										Relative_Hazard_Score_Number = NA,
										Trend_Aggregated_For_Looker = NA,
										Advanced_Data_Measures =  recurIntrvls[thisIntrvl],
										Advanced_Data_Measures_Units = "Yr Flood",
										Raw_Hazard_Value_25th = NA,
										Raw_Hazard_Value_75th = NA,
										Asset_Type = customerTable$AssetType[thisLoc],
										Business_Unit = customerTable$BusinessUnit[thisLoc],
										Country = customerTable$Country[thisLoc],
										State = customerTable$State[thisLoc],
										City = customerTable$City[thisLoc]))


							} 	else	{ # if all theseFloodImpacts are NAs
								dataOutput = rbind(dataOutput,
									data.table(
										User = userName,
										Location = customerTable$Location[thisLoc],
										Region = customerTable$Region[thisLoc],
										Subregion = customerTable$Subregion[thisLoc],
										Lat = customerTable$Lat[thisLoc],
										Lon = customerTable$Lon[thisLoc],
										Hazard = "River Flood (Local)",
										Hazard_Measure = c(hazardDepthName, hazardLikliName),
										Decade = 2000 + whichDecades[thisDecade],
										Scenario = rcpScenarios[thisScenario],
										Raw_Hazard_Value = 0,								# Raw_Hazard_Value
										Percentile_Score = NA,								# Percentile_Score
										Relative_Hazard_Score = NA,							# Relative_Hazard_Score
										Decadal_Trend_Strength = 0,							# Decadal_Trend_Strength
										Decadal_Trend_Significance = thisFldRcrSignif,		# Decadal_Trend_Significance
										Long_Term_Trend_Strength = NA,						# Long_Term_Trend_Strength
										Long_Term_Trend_Significance = fldRcrSignif_all,	# Long_Term_Trend_Significance					
										Relative_Hazard_Score_Number = NA,
										Trend_Aggregated_For_Looker = NA,
										Advanced_Data_Measures =  recurIntrvls[thisIntrvl],
										Advanced_Data_Measures_Units = "Yr Flood",
										Raw_Hazard_Value_25th = NA,
										Raw_Hazard_Value_75th = NA,
										Asset_Type = customerTable$AssetType[thisLoc],
										Business_Unit = customerTable$BusinessUnit[thisLoc],
										Country = customerTable$Country[thisLoc],
										State = customerTable$State[thisLoc],
										City = customerTable$City[thisLoc]))

							}
						}	else	{ # if thisFldRcrVal < 10 yr recurrence
							dataOutput = rbind(dataOutput,
								data.table(
									User = userName,
									Location = customerTable$Location[thisLoc],
									Region = customerTable$Region[thisLoc],
									Subregion = customerTable$Subregion[thisLoc],
									Lat = customerTable$Lat[thisLoc],
									Lon = customerTable$Lon[thisLoc],
									Hazard = "River Flood (Local)",
									Hazard_Measure = c(hazardDepthName, hazardLikliName),
									Decade = 2000 + whichDecades[thisDecade],
									Scenario = rcpScenarios[thisScenario],
									Raw_Hazard_Value = 0,								# Raw_Hazard_Value
									Percentile_Score = NA,								# Percentile_Score
									Relative_Hazard_Score = NA,							# Relative_Hazard_Score
									Decadal_Trend_Strength = 0,							# Decadal_Trend_Strength
									Decadal_Trend_Significance = thisFldRcrSignif,		# Decadal_Trend_Significance
									Long_Term_Trend_Strength = NA,						# Long_Term_Trend_Strength
									Long_Term_Trend_Significance = fldRcrSignif_all,	# Long_Term_Trend_Significance
									Relative_Hazard_Score_Number = NA,
									Trend_Aggregated_For_Looker = NA,
									Advanced_Data_Measures =  recurIntrvls[thisIntrvl],
									Advanced_Data_Measures_Units = "Yr Flood",
									Raw_Hazard_Value_25th = NA,
									Raw_Hazard_Value_75th = NA,
									Asset_Type = customerTable$AssetType[thisLoc],
									Business_Unit = customerTable$BusinessUnit[thisLoc],
									Country = customerTable$Country[thisLoc],
									State = customerTable$State[thisLoc],
									City = customerTable$City[thisLoc]))

						}
					} # for each decade

				dataOutput[which(dataOutput$Location == customerTable$Location[thisLoc] & dataOutput$Scenario == rcpScenarios[thisScenario]), ]$Long_Term_Trend_Strength = last(dataOutput)$Decadal_Trend_Strength

				} # for each rcp scenario
			}	else	{	# if there are not historical floods out to 500 yrs
			#### !!!! todo: consolidate and simplify !!!!!
				dataOutput = rbind(rbind(rbind(dataOutput,
					data.table(
						User = userName,
						Location = customerTable$Location[thisLoc],
						Region = customerTable$Region[thisLoc],
						Subregion = customerTable$Subregion[thisLoc],
						Lat = customerTable$Lat[thisLoc],
						Lon = customerTable$Lon[thisLoc],
						Hazard = "River Flood (Local)",
						Hazard_Measure = c(hazardDepthName, hazardLikliName),
						Decade = 2000 + rep(whichDecades, each = 2),
						Scenario = rcpScenarios[1],
						Raw_Hazard_Value = 0,				# Raw_Hazard_Value
						Percentile_Score = NA,				# Percentile_Score
						Relative_Hazard_Score = NA,			# Relative_Hazard_Score
						Decadal_Trend_Strength = NA,			# Decadal_Trend_Strength
						Decadal_Trend_Significance = NA,		# Decadal_Trend_Significance
						Long_Term_Trend_Strength = NA,		# Long_Term_Trend_Strength
						Long_Term_Trend_Significance = NA,	# Long_Term_Trend_Significance
						Relative_Hazard_Score_Number = NA,
						Trend_Aggregated_For_Looker = NA,
						Advanced_Data_Measures =  recurIntrvls[thisIntrvl],
						Advanced_Data_Measures_Units = "Yr Flood",
						Raw_Hazard_Value_25th = NA,
						Raw_Hazard_Value_75th = NA,
						Asset_Type = customerTable$AssetType[thisLoc],
						Business_Unit = customerTable$BusinessUnit[thisLoc],
						Country = customerTable$Country[thisLoc],
						State = customerTable$State[thisLoc],
						City = customerTable$City[thisLoc])), 
					data.table(
						User = userName,
						Location = customerTable$Location[thisLoc],
						Region = customerTable$Region[thisLoc],
						Subregion = customerTable$Subregion[thisLoc],
						Lat = customerTable$Lat[thisLoc],
						Lon = customerTable$Lon[thisLoc],
						Hazard = "River Flood (Local)",
						Hazard_Measure = c(hazardDepthName, hazardLikliName),
						Decade = 2000 + rep(whichDecades, each = 2),
						Scenario = rcpScenarios[2],
						Raw_Hazard_Value = 0,				# Raw_Hazard_Value
						Percentile_Score = NA,				# Percentile_Score
						Relative_Hazard_Score = NA,			# Relative_Hazard_Score
						Decadal_Trend_Strength = NA,			# Decadal_Trend_Strength
						Decadal_Trend_Significance = NA,		# Decadal_Trend_Significance
						Long_Term_Trend_Strength = NA,		# Long_Term_Trend_Strength
						Long_Term_Trend_Significance = NA,	# Long_Term_Trend_Significance
						Relative_Hazard_Score_Number = NA,
						Trend_Aggregated_For_Looker = NA,
						Advanced_Data_Measures =  recurIntrvls[thisIntrvl],
						Advanced_Data_Measures_Units = "Yr Flood",
						Raw_Hazard_Value_25th = NA,
						Raw_Hazard_Value_75th = NA,
						Asset_Type = customerTable$AssetType[thisLoc],
						Business_Unit = customerTable$BusinessUnit[thisLoc],
						Country = customerTable$Country[thisLoc],
						State = customerTable$State[thisLoc],
						City = customerTable$City[thisLoc])), 
					data.table(
						User = userName,
						Location = customerTable$Location[thisLoc],
						Region = customerTable$Region[thisLoc],
						Subregion = customerTable$Subregion[thisLoc],
						Lat = customerTable$Lat[thisLoc],
						Lon = customerTable$Lon[thisLoc],
						Hazard = "River Flood (Local)",
						Hazard_Measure = c(hazardDepthName, hazardLikliName),
						Decade = 2000 + rep(whichDecades, each = 2),
						Scenario = rcpScenarios[3],
						Raw_Hazard_Value = 0,				# Raw_Hazard_Value
						Percentile_Score = NA,				# Percentile_Score
						Relative_Hazard_Score = NA,			# Relative_Hazard_Score
						Decadal_Trend_Strength = NA,			# Decadal_Trend_Strength
						Decadal_Trend_Significance = NA,		# Decadal_Trend_Significance
						Long_Term_Trend_Strength = NA,		# Long_Term_Trend_Strength
						Long_Term_Trend_Significance = NA,	# Long_Term_Trend_Significance
						Relative_Hazard_Score_Number = NA,
						Trend_Aggregated_For_Looker = NA,
						Advanced_Data_Measures =  recurIntrvls[thisIntrvl],
						Advanced_Data_Measures_Units = "Yr Flood",
						Raw_Hazard_Value_25th = NA,
						Raw_Hazard_Value_75th = NA,
						Asset_Type = customerTable$AssetType[thisLoc],
						Business_Unit = customerTable$BusinessUnit[thisLoc],
						Country = customerTable$Country[thisLoc],
						State = customerTable$State[thisLoc],
						City = customerTable$City[thisLoc]))

			}
			print(c(thisLoc, thisIntrvl))
		}
	}	

	dataOutput$Raw_Hazard_Value = as.numeric(dataOutput$Raw_Hazard_Value)
	dataOutput = dataOutput[-1,]

	hazardDepthNames =  c("Avg Flood Depth (m)")
	hazardExtentNames = c("Flood Areal Extent (%)")
		# defining relative flood hazard
	basSeqDepth = c(0,seq(0.01, 10, length.out=99))
	basSeqLikli = c(0,seq(1, 100, length.out=99))
	relFloodHazardDepth = (basSeqDepth^2 / max(basSeqDepth^2)) * 10 + min(basSeqDepth)
	relFloodHazardLikli = basSeqLikli
	#relFloodHazard = c(rep(0, 33), seq(0.01,10,length.out=(67)))
	dataOutput$Percentile_Score = 1
	depthRows = which(dataOutput$Hazard_Measure %in% hazardDepthNames)
	likliRows = which(dataOutput$Hazard_Measure %in% hazardExtentNames)
	for(ll in 1:length(relFloodHazardDepth))	{
		dataOutput$Percentile_Score[depthRows][which(dataOutput$Raw_Hazard_Value[depthRows] > relFloodHazardDepth[ll])] = ll + 0
		dataOutput$Percentile_Score[likliRows][which(dataOutput$Raw_Hazard_Value[likliRows] > relFloodHazardLikli[ll])] = ll + 0
	}

	fileName_localFlood = paste0(userName, '_', thisDate, '_highRestFlood_', (locationFootprint*2+1), 'km')
	fwrite(dataOutput, paste0(customerFolder, fileName_localFlood, thisDate, '.csv'))
	


















	
	# rasterize hazard data
hazardRaster = raster::rasterFromXYZ(data.table::data.table(lon = nc_lons, lat = nc_lats, rasVal = unlist(hazardData[,,1,2,1])))
raster::crs(hazardRaster) <- 'EPSG:4326'

summaryOutput_df = data.table::fread(paste0(dataPath, "SoilMoisture_projectionOutput_", saveDate, ".csv"))
summaryOutput_df$diffVals = summaryOutput_df$projectedQ50 - summaryOutput_df$climatologyQ50
if(any(summaryOutput_df$diffVals > 20))	{summaryOutput_df$diffVals[summaryOutput_df$diffVals > 20] = 20}
if(any(summaryOutput_df$diffVals < -20))	{summaryOutput_df$diffVals[summaryOutput_df$diffVals < -20] = -20}

rasterList_abs = list()
rasterList_dif = list()
for(thisMonth in 1:6)	{
	summaryOutput_df_thisMonth = subset(summaryOutput_df, monthsOut == thisMonth - 1)
#	print(summary(summaryOutput_df_thisMonth))
	summaryOutput_rs_abs = raster::rasterFromXYZ(data.table::data.table(lon = summaryOutput_df_thisMonth$Lon, lat = summaryOutput_df_thisMonth$Lat, rasVal = summaryOutput_df_thisMonth$projectedQ50))
	summaryOutput_rs_diff = raster::rasterFromXYZ(data.table::data.table(lon = summaryOutput_df_thisMonth$Lon, lat = summaryOutput_df_thisMonth$Lat, rasVal = summaryOutput_df_thisMonth$diffVals))
#	raster::writeRaster(summaryOutput_rs, filename=paste0(dataPath, "SoilMoisture_projectionOutput_month-", thisMonth, ".tif"), datatype="FLT4S", overwrite=TRUE)
	rasterList_abs[[thisMonth]] = summaryOutput_rs_abs
	rasterList_dif[[thisMonth]] = summaryOutput_rs_diff
}
summaryOutput_brick_abs = raster::brick(rasterList_abs)
summaryOutput_brick_dif = raster::brick(rasterList_dif)
raster::writeRaster(summaryOutput_brick_abs, filename=paste0("C:/Users/arik/Documents/GitHub/seasonalForecasting_otherClimate_/regionalSoilMoisture_shinyApp/regionalSoilMoistureForecast/SoilMoisture_projectionOutput_month-all.tif"), format = "GTiff", datatype="FLT4S", overwrite=TRUE)
raster::writeRaster(summaryOutput_brick_dif, filename=paste0("C:/Users/arik/Documents/GitHub/seasonalForecasting_otherClimate_/regionalSoilMoisture_shinyApp/regionalSoilMoistureForecast/SoilMoisture_projectionOutput_month-all_dif.tif"), format = "GTiff", datatype="FLT4S", overwrite=TRUE)





for(thisMonth in 1:length(names(summaryOutput_brick_dif)))	{
	extractedValues_dif = raster::extract(summaryOutput_brick_dif[[thisMonth]], thisCountry)
	extractedValues_abs = raster::extract(summaryOutput_brick_abs[[thisMonth]], thisCountry)

	avgValues_dif <- sapply(extractedValues_dif, function(x) mean(x, na.rm = TRUE))
	avgValues_abs <- sapply(extractedValues_abs, function(x) mean(x, na.rm = TRUE))

	thisCountry[, paste0("dif_", thisMonth)] = avgValues_dif
	thisCountry[, paste0("abs_", thisMonth)] = avgValues_abs
}
sf::st_write(thisCountry, "C:/Users/arik/Documents/GitHub/seasonalForecasting_otherClimate_/regionalSoilMoisture_shinyApp/regionalSoilMoistureForecast/provinceLevelForecast.shp", append=FALSE)


aggregated_data <- raster::extract(hazardData[ , , 1, 2, 1], india, fun = mean, na.rm = TRUE)					





hazardMeasureNC








states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
california = subset(states, ID == 'california')
california_proj = st_transform(california, crs = thisCRS)


counties <- sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("california", counties$ID))
counties_cent = st_centroid(counties)	;	rm(counties)
counties_cent = cbind(counties_cent, st_coordinates(counties_cent))







#downscaledGrace
library(ncdf4)
library(viridis)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


waterIndex = readRDS("J:\\Cai_data\\Rabo\\Locations\\BeefNW\\BeefNW_waterIndex.rds")

customerTable = data.table::fread("J:\\Cai_data\\Rabo\\Locations\\BeefNW\\Customer Onboarding Information_BNW.csv", skip = 1)
nameOfRows = c("2010s", "2020s", "2030s", "2040s", "2050s", "2060s", "2070s", "2080s", "2090s")
nameOfCols = c("Percentile_05th", "Percentile_15th", "Percentile_25th", "Percentile_50th", "Percentile_75th", "Percentile_85th", "Percentile_95th")
thisFolder =  "J:\\Cai_data\\Rabo\\dataForNorAmTeam\\"
indexValues = c('Aridity Index - Avg', 'Soil Moisture Stress - Avg', 'Plant Water Demand - Avg', 'Aridity Index - Drought', 'Soil Moisture Stress - Drought', 'Plant Water Demand - Drought', 'Aridity Index w/ Irrigation - Avg', 'Plant Water Demand w/ Irrigation - Avg', 'Aridity Index w/ Irrigation - Drought', 'Plant Water Demand w/ Irrigation - Drought')
indexValues = c('Aridity Index - Avg', 'Soil Moisture Stress - Avg', 'AnnualAvg_noIrrigation', 'Aridity Index - Drought', 'Soil Moisture Stress - Drought', 'Drought_noIrrigation', 'Aridity Index w/ Irrigation - Avg', 'AnnualAvg_withIrrigation', 'Aridity Index w/ Irrigation - Drought', 'Drought_withIrrigation')
climScenarios = c("LowEmissions", "MiddleOfRoad", "HighEmissions")
iter = 0
for(thisLoc in 1:nrow(customerTable))	{
	thisFile = paste0("BeefNW_", customerTable[thisLoc,2], "_waterIndex_summaryData.xlsx")
	for(thisScen in 1:length(climScenarios))	{
		for(thisIndexVal in c(3,6,8,10))	{ 
			iter = iter + 1
			thisTable = as.data.frame(waterIndex[thisLoc, , , thisScen, thisIndexVal, 1])
			rownames(thisTable) = nameOfRows
			names(thisTable) = nameOfCols 

			if(iter == 1) {
				xlsx::write.xlsx(thisTable, file=paste0(thisFolder, thisFile),
					sheetName=paste0(climScenarios[thisScen], " - ", indexValues[thisIndexVal]), col.names=TRUE, row.names = TRUE)
			} else	{
				xlsx::write.xlsx(thisTable, file=paste0(thisFolder, thisFile),
						sheetName=paste0(climScenarios[thisScen], " - ", indexValues[thisIndexVal]), col.names=TRUE, row.names = TRUE, append=TRUE)
			}
		}
	}
}




#########################################################################################################
# county level gw insights

thisCRS = 4326
	# read in high res basinAtlas
basinAt12 = st_read(dsn="J:\\Cai_data\\BasinATLAS_Data_v10\\BasinATLAS_v10.gdb", layer="BasinATLAS_v10_lev10")
sf_use_s2(FALSE)
basinAt12_cent = st_centroid(basinAt12)		; rm(basinAt12)
xyCoords = st_coordinates(basinAt12_cent)
basinAt12_cent = cbind(basinAt12_cent, xyCoords)
basinAt_proj = st_transform(basinAt12_cent, thisCRS)	; rm(basinAt12_cent)
basinAt_dt = st_drop_geometry(basinAt_proj)

	# read in california county boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")
america = ne_countries(country = 'United States of America', scale='medium', returnclass = 'sf')
class(world)

states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
california = subset(states, ID == 'california')
california_proj = st_transform(california, crs = thisCRS)


counties <- sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("california", counties$ID))
counties_cent = st_centroid(counties)	;	rm(counties)
counties_cent = cbind(counties_cent, st_coordinates(counties_cent))

#counties_proj = st_transform(counties_cent, crs = thisCRS)


#basinAtlasToCounty = st_intersects(basinAt_proj, counties_proj) # keeps points
#sf_use_s2(TRUE)

	# read in grace tellus data (3 deg data but represented at 0.5)
grace_nc = ncdf4::nc_open('J:\\Cai_data\\Rabo\\GRACE\\GRCTellus.JPL.200204_202211.GLO.RL06.1M.MSCNv03CRI.nc')
graceLons = ncdf4::ncvar_get(grace_nc, 'lon')
graceLons[graceLons > 180] = -(360 - graceLons[graceLons > 180])
graceLats = ncdf4::ncvar_get(grace_nc, 'lat')
graceDates = as.vector(ncdf4::ncvar_get(grace_nc, 'time')) #+ as.Date('2002-01-01')
graceLandMask = ncdf4::ncvar_get(grace_nc, 'land_mask')
graceWaterTile = which(graceLandMask == 0)
graceLWEthick = ncdf4::ncvar_get(grace_nc, 'lwe_thickness') * 10 # convert cm to mm
graceLWEthickLand = graceLWEthick ; graceLWEthickLand[graceWaterTile] = NA
graceUncertainty = ncdf4::ncvar_get(grace_nc, 'uncertainty') * 10 # convert cm to mm

graceTable = data.frame(lon = rep(graceLons, length(graceLats)), lat = rep(graceLats, each=length(graceLons)), index = seq(1,by=1,length.out=length(graceLats)*length(graceLons))) 




	
nearestHybas = NULL
nearestGrace = NULL
	# merging datasets: grace and counties
for(thisRow in 1:nrow(counties_cent))	{
	thisLat = counties_cent$Y[thisRow]
	thisLon = counties_cent$X[thisRow]
	
	nearestHybas = c(nearestHybas, which.min((basinAt_dt$X - thisLon)^2 + (basinAt_dt$Y - thisLat)^2))
	nearestGrace = c(nearestGrace, which.min((graceTable$lon - thisLon)^2 + (graceTable$lat - thisLat)^2))
}	

graceTrend = NULL
graceSignif = NULL
graceYears = graceDates / 365
for(thisGrace in 1:length(nearestGrace))	{
	thisLat = which(graceLats == graceTable$lat[nearestGrace[thisGrace]])
	thisLon = which(graceLons == graceTable$lon[nearestGrace[thisGrace]])
	theseGraceVals = graceLWEthickLand[thisLon, thisLat, ]
	
	graceTrend = c(graceTrend, mblm::mblm(theseGraceVals ~ graceYears)$coef[2])
	graceSignif = c(graceSignif, cor.test(graceDates, graceLWEthickLand[thisLon, thisLat, ], method='spearman')$p.value)
}
counties_cent$gracewaterTrend_MMprYr = graceTrend
counties_cent$gracewaterTrend_pVal = graceSignif
depthToGroundwater = basinAt_dt$gwt_cm_sav[nearestHybas] * 10
counties_cent$groundwaterLossesPct = 100 * (((0.05 * (58000 - depthToGroundwater)) +  (10 * counties_cent$gracewaterTrend_MMprYr)) / 
	(0.05 * (58000 - depthToGroundwater)) - 1)#state avg of 190ft --> 58 m; est avg drain
counties_cent$avgPrecipitation = basinAt_dt$pre_mm_syr[nearestHybas]
counties_cent$avgPotentialEvapotranspiration = basinAt_dt$pet_mm_syr[nearestHybas]
counties_cent$avgAridityIndex = basinAt_dt$ari_ix_sav[nearestHybas] / 100


	# converting points back to polys
counties <- sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("california", counties$ID))
st_geometry(counties_cent) = st_geometry(counties)



world <- ne_countries(scale = "medium", returnclass = "sf")
america = ne_countries(country = 'United States of America', scale='medium', returnclass = 'sf')
class(world)



myTitle = "Groundwater Loss"
mySubtitle = "% change in previous decade (est.)"

ggplot(data = counties_cent) +
	geom_sf(colour = "grey10", aes(fill = groundwaterLossesPct))	+
	scale_fill_viridis(option = 'viridis', trans = scales::pseudo_log_trans(sigma = 1.00), direction = -1, name = "GW Loss (%)" )	+

#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light(base_size = 22)	+
	theme(panel.ontop=FALSE, panel.background=element_blank(), legend.position = c(0.81, 0.85)) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
#	geom_sf(data=ocean50, fill="white", colour='white')+
#	borders('world', xlim=range(latlonBox_local$lon), ylim=range(latlonBox_local$lat), 
 #           colour='gray90', size=.2)	+
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+

#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
#	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
#	geom_sf(data=locOfInt_sf, col='red1',size=5, shape='+', stroke=3) +
#	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0(myTitle, ' 2010s-2020s'),
		subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')

names(counties_cent)
st_write(counties_cent[ ,-c(5,6)], "J:\\Cai_data\\Rabo\\dataForNorAmTeam\\preliminaryCountyLevelData.gpkg")

gg = st_read("J:\\Cai_data\\Rabo\\dataForNorAmTeam\\preliminaryCountyLevelData.gpkg")

















































# old not likley useful



dsGrace = nc_open("J:\\Downloads\\GRACE_ITSG_downscaled.nc")
dsGrace = nc_open("J:\\Downloads\\GRACE_JPL_downscaled.nc")
dsGrace
theLats = ncvar_get(dsGrace, 'Lat')
theLons = ncvar_get(dsGrace, 'Long')
theVals = ncvar_get(dsGrace, 'EWH_mm')
theTime = ncvar_get(dsGrace, 'time')
image(theLats, theLons, ncvar_get(dsGrace, EWH_mm[ , 1]))











ocean50 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_ocean\\ne_10m_ocean.shp'))
world <- ne_countries(scale = "medium", returnclass = "sf")
#countries10 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_admin_0_countries\\ne_10m_admin_0_countries.shp'))
provinces10 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_admin_1_states_provinces\\ne_10m_admin_1_states_provinces.shp'))

thisCRS = 4087#4326











#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = theLats, lon = theLons, plotData = as.vector(theVals[,10]))
oddVals = which(geomData$lat > 9.96921e+35)
geomData_real = subset(geomData, lat < 9.96921e+35  & lon < 9.96921e+35 ) 


geomData_real = data.frame(lon = rep(unique(geomData_real$lat), each=length(unique(geomData_real$lon))), 
	lat = rep(unique(geomData_real$lon), length(unique(geomData_real$lat))), plotData = geomData_real$plotData)

allLats = seq(0.25, 179.25, 0.5)
#allLons = seq(-179.75, 179.25, length.out=719)
allLons = seq(0.25, 359.75, length.out=719)
geomData_real = data.frame(lat = rep(allLats, each=length(allLons)), lon = rep(allLons, length(allLats)), plotData = geomData_real$plotData)

latlonBox_local = c(0, 180,0,360)#c(35,53,-12,46)
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(geomData_real, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)


myTitle = "test"
mySubtitle = "test"

ggplot(data = grace_sf) +
#	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 5.5) +

	geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE) +
	scale_fill_viridis(option = 'viridis', trans = scales::pseudo_log_trans(sigma = .5))	+

	geom_sf(aes(colour = plotData))	+
	scale_colour_viridis(option = 'viridis', trans = scales::pseudo_log_trans(sigma = .5))	+

#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light(base_size = 22)	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
#	geom_sf(data=ocean50, fill="white", colour='white')+
#	borders('world', xlim=range(latlonBox_local$lon), ylim=range(latlonBox_local$lat), 
 #           colour='gray90', size=.2)	+
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+

#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
#	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
#	geom_sf(data=locOfInt_sf, col='red1',size=5, shape='+', stroke=3) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0(myTitle, ' - 2020s'),
		subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')















	
					

# calculating province level avgs
thisCountry = sf::st_read("C:/Users/arik/Documents/GitHub/seasonalForecasting_otherClimate_/regionalSoilMoisture_shinyApp/regionalSoilMoistureForecast/STE_2021_AUST_GDA2020.shp")
thisCountry = thisCountry[!sf::st_is_empty(thisCountry),]
#thisCountry = sf::st_cast(thisCountry, "POLYGON")
thisCountry = sf::st_simplify(thisCountry, dTolerance = 5000)
thisCountry = sf::st_transform(thisCountry, crs="+proj=longlat +datum=WGS84")


for(thisMonth in 1:length(names(summaryOutput_brick_dif)))	{
	extractedValues_dif = raster::extract(summaryOutput_brick_dif[[thisMonth]], thisCountry)
	extractedValues_abs = raster::extract(summaryOutput_brick_abs[[thisMonth]], thisCountry)

	avgValues_dif <- sapply(extractedValues_dif, function(x) mean(x, na.rm = TRUE))
	avgValues_abs <- sapply(extractedValues_abs, function(x) mean(x, na.rm = TRUE))

	thisCountry[, paste0("dif_", thisMonth)] = avgValues_dif
	thisCountry[, paste0("abs_", thisMonth)] = avgValues_abs
}
sf::st_write(thisCountry, "C:/Users/arik/Documents/GitHub/seasonalForecasting_otherClimate_/regionalSoilMoisture_shinyApp/regionalSoilMoistureForecast/provinceLevelForecast.shp", append=FALSE)
