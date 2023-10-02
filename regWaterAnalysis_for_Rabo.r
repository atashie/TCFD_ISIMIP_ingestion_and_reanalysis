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


