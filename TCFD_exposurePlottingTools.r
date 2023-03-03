###################################################
library(data.table)
library(ncdf4)
library(sf)
library(ggplot2)
library(viridis)
library(rnaturalearth)
#library(maps)
library(ggfx)



#########################################
# reading in climai netcdf data
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'

# reading in customer data
userName = 'ITC'	
hazardFolder = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
customerFolder = 'J:\\Cai_data\\TCFD\\locations\\ITC_Dec2022\\'
thisDate = Sys.Date()

customerTable = fread(paste0(customerFolder, 'HMClause_Locations_Nov2022 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
#hazardTable = fread(paste0(customerFolder, 'Hazard_Tables_Corbion_Nov2022 - Hazard Definitions.csv'))							# 
#relHazScores = fread(paste0(customerFolder, 'Hazard_Tables_Corbion_Nov2022 - Hazard Scores.csv'))				
#appendedHazardNames = c("River Flood (Local)", 'Sea Level Rise', 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\Corbion_Nov2022\\Corbion_temp_precip_hazards_dec_6 - Corbion_temp_precip_hazards_dec_6.csv'

ocean50 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_ocean\\ne_10m_ocean.shp'))
world <- ne_countries(scale = "medium", returnclass = "sf")
#countries10 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_admin_0_countries\\ne_10m_admin_0_countries.shp'))
provinces10 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_admin_1_states_provinces\\ne_10m_admin_1_states_provinces.shp'))

# region to maplatlonBox = c(25, 65, -10, 60) #(minlat, maxlat, minlon, maxlon)
latlonBox = c(-55, 75, -180, 180) #(minlat, maxlat, minlon, maxlon)
latlonBox_local = c(-60,-15,-80,-50)#c(35,53,-12,46)
locOfInt = st_point(c(customerTable$Lat[1], customerTable$Lon[1]))





##########################################################################################################################################################################################
##########################################################################################################################################################################################
# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'yield-soy-noirrv2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')



###############
# P1: maps
# 1a: gridded
myTitle = 'Soy Yield - 2020s'
mySubtitle = 'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = geomData) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.5))	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(geomData$lon), ylim=range(geomData$lat), 
            colour='gray90', size=.2)	+
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
myTitle = 'Soy Yield - Trends to 2050s - High Emissions'
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 5, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = geomData) +
	with_blur(geom_raster(data = geomData, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(low = 'darkred', high = 'darkgreen', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .00001), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(geomData$lon), ylim=range(geomData$lat), 
            colour='gray10', size=.2)	+
#	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
#df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
geomJoin = st_join(world, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2

myTitle = 'Soy Yield - 2020s'
mySubtitle = 'nonirrigated'
ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.5))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox[c(3,4)], ylim = latlonBox[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit_c.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 5, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
#df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
geomJoin = st_join(world, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(low = 'darkred', high = 'darkgreen', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .01), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox[c(3,4)], ylim = latlonBox[c(1,2)] + c(-1, -1), expand = FALSE) +
#	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit_c.png"), width = 20, height = 8)













# part 2: time series
#thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
#thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
#thisTestDat = nc_testDat[thisLon, thisLat, , , ]

# abs values
whichGeoUnit = 'Argentina'
geoUnitVals = matrix(nrow=9, ncol=4)
thisScen = 1
for(i in 1:9)	{
	for(ii in 1:4)	{
		j = c(1,3,5,6)[ii]
		geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , i, thisScen, j]))
		df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
		geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

			# provinces
	#	geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
	#	geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
	#	geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
	#	missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
	#	geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
	#	geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

			# countries
		geomJoin = st_join(world, geomData_sf, join = st_intersects)
		geomSub = subset(geomJoin, sovereignt == whichGeoUnit)
		geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomSub)#, na.action = na.pass)
		geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
		missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
		geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
		geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
		geoUnitVals[i,ii] = geomAvgMrg$plotData[1]
	}
}



theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = 'kg/ha/yr'

png(paste0(customerFolder, varName, theScenarios[thisScen], "_avgYield.png"), width=800, height=400)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, geoUnitVals[ , 1], ylim = c(0, max(geoUnitVals[ , 4])*1.025),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Yield (kg/ha/yr)', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(geoUnitVals[ , 3], rev(geoUnitVals[ , 4])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, geoUnitVals[ , 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(geoUnitVals[ , 4])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
#	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
#		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
#			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
#			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
#	} 	else {
#		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
#			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
#				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
#				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
#		}	else	{
#			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
#				paste0('Trend: No Significant Trend'),
#				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
#			}
#	}
	dev.off()





















































##########################################################################################################################################################################################
##########################################################################################################################################################################################
# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'WRI_based_WaterStress_v2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')



###############
# P1: maps
# 1a: gridded
myTitle = 'Water Stress'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.0005))	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .00001), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.0005))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .00001), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()

































##########################################################################################################################################################################################
##########################################################################################################################################################################################
# variable options:
	# WRI_based_WaterStress_v2_processed; WRI_based_WaterDepletion_v2_processed; qrv2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'WRI_based_WaterDepletion_v2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Water Depletion'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.0001))	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .000001), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.0001))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .000001), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()








































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed; dis_WRIbasedDroughtSeverityv2_processed

varName = 'dis_WRIbasedDroughtSeverityv2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Drought Intensity'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = .5))	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .01), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = .5))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.01) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .00001), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()













































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'WRI_basedStreamflowInterannualVariability_v2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Interannual Variability'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.2))	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .001), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.2) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.2))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.005) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .001), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()















































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'WRI_basedStreamflowSeasonalVariability_v2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Seasonal Variability'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.2))	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .001), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.2))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .001), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()








































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'disv2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Streamflow'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = .00001), direction = -1)	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .0001), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.00001), direction = -1)	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .0001), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()
















































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'twsv2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Total Water Storage'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = .0001), direction = -1)	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 10), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.0001), direction = -1)	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 10), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()












































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'qrv2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Groundwater Rechage'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 1), direction = -1)	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 3.0), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 1), direction = -1)	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 3.0), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()










































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'rootmoistv2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Root Zone Soil Moisture'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 2), direction = -1)	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
#	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 10), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 2), direction = -1)	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 10), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()























































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed; rootmoistv2_processed

varName = 'cwood-evgndltr_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Carbon Mass in Wood (evgndltr)'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])


df_proj = st_as_sf(geomData, coords = c('lon', 'lat'), crs = 4326)
#df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = geomData) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = .01) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 2), direction = -1)	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(geomData$lon), ylim=range(geomData$lat), 
            colour='gray90', size=.2)	+
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
#	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = geomData) +
	with_blur(geom_raster(data = geomData, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = .01) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 10), limits = c(-lmt, lmt))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 10))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(geomData$lon), ylim=range(geomData$lat), 
            colour='gray10', size=.2)	+
#	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
#	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
geomJoin = st_join(world, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 2), direction = -1)	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
#	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
geomJoin = st_join(world, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 10), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
#	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()



