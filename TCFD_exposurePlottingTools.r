###################################################
library(data.table)
library(ncdf4)
library(sf)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(maps)



#########################################
# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\SurfaceWater_Streamflow\\"
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'

# reading in customer data
userName = 'Rabobank'	
hazardFolder = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
customerFolder = 'J:\\Cai_data\\TCFD\\locations\\Rabobank_Jan2023\\'
thisDate = Sys.Date()

customerTable = fread(paste0(customerFolder, 'Customer_Hazards_and_Locations-Rabobank - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
#hazardTable = fread(paste0(customerFolder, 'Hazard_Tables_Corbion_Nov2022 - Hazard Definitions.csv'))							# 
#relHazScores = fread(paste0(customerFolder, 'Hazard_Tables_Corbion_Nov2022 - Hazard Scores.csv'))				
#appendedHazardNames = c("River Flood (Local)", 'Sea Level Rise', 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\Corbion_Nov2022\\Corbion_temp_precip_hazards_dec_6 - Corbion_temp_precip_hazards_dec_6.csv'



# region to maplatlonBox = c(25, 65, -10, 60) #(minlat, maxlat, minlon, maxlon)
latlonBox = c(-55, 75, -180, 180) #(minlat, maxlat, minlon, maxlon)
latlonBox_local = c(-55,11,-82, -35)
locOfInt = st_point(c(customerTable$Lat[1], customerTable$Lon[1]))

# variable options:
	# dis; 
ncVarFileName = 'dis'

# testing output, squinty eye test
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, ncVarFileName, 'v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Annual Streamflow'
#mySubtitle = 'nonirrigated'

geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 1, 1]))
df_sub = subset(geomData, lat > -5000)
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = .8, height = .8)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_viridis(trans = scales::pseudo_log_trans(sigma = 0.001))	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='red3', col='red3', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill=expression(km^2 / year))

myTitle = 'Annual Streamflow Trends'
#mySubtitle = 'nonirrigated'
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 5, 3, 3]))
df_sub = subset(geomData, lat > -5000)
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/40
lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = 1, height = 1)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill=expression(km^2 / year))







# 1b: country avg
sf_use_s2(FALSE)
world <- ne_countries(scale = "medium", returnclass = "sf")
#usa = ne_states(country='usa')

st_as_sf(world)
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')
geomJoin = st_join(world, geomData_sf, join = st_intersects)

geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/40
lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
#	scale_colour_viridis_c(alpha = 1) +
#	scale_colour_gradientn(colours=rev(c('#F2F3F3', '#F2F3F3', '#FFF2F2', '#FFE5E5', 'orangered1', 'red1','red3','firebrick')),name='drought') +
#	scale_fill_gradient2(low='firebrick', mid='#DBDDDF', high='#196CE1', midpoint = .5, na.value = NA, limits = c(0,1),
#		breaks = c(.25, .5, .75), labels = c('severe drought', 'drought', 'wet conditions'), name='SPEI 3 month') +
#	geom_sf(data = california, fill = '#DBDDDF', color = '#1A232F', size=1.4) +
#	geom_sf(data = states, fill = '#DBDDDF')	+
#	geom_sf(data = counties, fill = NA, color = gray(.5))
#	geom_sf(data = world, fill = NA, color = gray(0.05), size=0.8) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData)) +
	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	ylim(-2,2) geomAvgMrg_sf
#	geom_sf(data = mask, fill='#F2F3F3', color='#F2F3F3') +
#	geom_sf(data = counties, fill = NA, color = '#666D74') +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	theme(panel.grid = element_line(colour='#F2F3F3')) +
	theme(panel.background = element_rect(fill = '#F2F3F3', colour='#F2F3F3')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey10')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)] - c(2, -3), ylim = latlonBox_local[c(1,2)] + c(-3, 3), expand = FALSE) +
	labs(title=myTitle, #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill=expression(km^2 / year))

#	




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
	# dis; WRI_based_WaterStress_
ncVarFileName = 'WRI_based_WaterStress_'


# testing output, squinty eye test
myNC = nc_open(paste0(ncOutputPath, ncVarFileName, 'v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Water Stress (Demand / Supply)'
#mySubtitle = 'nonirrigated'

geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 1, 1]))
df_sub = subset(geomData, lat > -5000)
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = .8, height = .8)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_viridis(trans = scales::pseudo_log_trans(sigma = 0.000001))	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='red3', col='red3', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
 #        fill=expression(km^2 / year))
        fill='Stress [-]')


myTitle = 'Trends in Water Stress (Demand / Supply)'
#mySubtitle = 'nonirrigated'
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 5, 3, 3]))
df_sub = subset(geomData, lat > -5000)
lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000000
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = 1, height = 1)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_stepsn(colors=rev(c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac')),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
 #        fill=expression(km^2 / year))
        fill='Trend in Stress [-]')







# 1b: country avg
sf_use_s2(FALSE)
world <- ne_countries(scale = "medium", returnclass = "sf")
#usa = ne_states(country='usa')

st_as_sf(world)
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')
geomJoin = st_join(world, geomData_sf, join = st_intersects)

geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/500000


ggplot(data = geomAvgMrg_sf) +
#	scale_colour_viridis_c(alpha = 1) +
#	scale_colour_gradientn(colours=rev(c('#F2F3F3', '#F2F3F3', '#FFF2F2', '#FFE5E5', 'orangered1', 'red1','red3','firebrick')),name='drought') +
#	scale_fill_gradient2(low='firebrick', mid='#DBDDDF', high='#196CE1', midpoint = .5, na.value = NA, limits = c(0,1),
#		breaks = c(.25, .5, .75), labels = c('severe drought', 'drought', 'wet conditions'), name='SPEI 3 month') +
#	geom_sf(data = california, fill = '#DBDDDF', color = '#1A232F', size=1.4) +
#	geom_sf(data = states, fill = '#DBDDDF')	+
#	geom_sf(data = counties, fill = NA, color = gray(.5))
#	geom_sf(data = world, fill = NA, color = gray(0.05), size=0.8) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData)) +
	scale_fill_stepsn(colors=rev(c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac')),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	ylim(-2,2) geomAvgMrg_sf
#	geom_sf(data = mask, fill='#F2F3F3', color='#F2F3F3') +
#	geom_sf(data = counties, fill = NA, color = '#666D74') +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	theme(panel.grid = element_line(colour='#F2F3F3')) +
	theme(panel.background = element_rect(fill = '#F2F3F3', colour='#F2F3F3')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey10')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)] - c(2, -3), ylim = latlonBox_local[c(1,2)] + c(-3, 3), expand = FALSE) +
	labs(title=myTitle, #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
#        fill=expression(km^2 / year))
        fill='Trend in Stress [-]')

#	




# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' [-] per year'
thisScen = 3

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Water Scarcity [-]', xlab='',
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
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.96,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.96,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.96,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()




















# variable options:
	# dis; WRI_based_WaterStress_
ncVarFileName = 'dis_WRIbasedDroughtSeverity'


# testing output, squinty eye test
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, ncVarFileName, 'v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Drought Severity'
#mySubtitle = 'nonirrigated'

geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 1, 1]))
df_sub = subset(geomData, lat > -5000)
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = .8, height = .8)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_viridis()+#trans = scales::pseudo_log_trans(sigma = 0.000001))	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='red3', col='red3', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
 #        fill=expression(km^2 / year))
        fill='Severity [-]')


myTitle = 'Trends in Drought Severity'
#mySubtitle = 'nonirrigated'
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 5, 3, 3]))
df_sub = subset(geomData, lat > -5000)
lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/50
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = 1, height = 1)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_stepsn(colors=rev(c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac')),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
 #        fill=expression(km^2 / year))
        fill='Trend in Severity [-]')







# 1b: country avg
sf_use_s2(FALSE)
world <- ne_countries(scale = "medium", returnclass = "sf")
#usa = ne_states(country='usa')

st_as_sf(world)
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')
geomJoin = st_join(world, geomData_sf, join = st_intersects)

geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/200


ggplot(data = geomAvgMrg_sf) +
#	scale_colour_viridis_c(alpha = 1) +
#	scale_colour_gradientn(colours=rev(c('#F2F3F3', '#F2F3F3', '#FFF2F2', '#FFE5E5', 'orangered1', 'red1','red3','firebrick')),name='drought') +
#	scale_fill_gradient2(low='firebrick', mid='#DBDDDF', high='#196CE1', midpoint = .5, na.value = NA, limits = c(0,1),
#		breaks = c(.25, .5, .75), labels = c('severe drought', 'drought', 'wet conditions'), name='SPEI 3 month') +
#	geom_sf(data = california, fill = '#DBDDDF', color = '#1A232F', size=1.4) +
#	geom_sf(data = states, fill = '#DBDDDF')	+
#	geom_sf(data = counties, fill = NA, color = gray(.5))
#	geom_sf(data = world, fill = NA, color = gray(0.05), size=0.8) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData)) +
	scale_fill_stepsn(colors=rev(c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac')),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	ylim(-2,2) geomAvgMrg_sf
#	geom_sf(data = mask, fill='#F2F3F3', color='#F2F3F3') +
#	geom_sf(data = counties, fill = NA, color = '#666D74') +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	theme(panel.grid = element_line(colour='#F2F3F3')) +
	theme(panel.background = element_rect(fill = '#F2F3F3', colour='#F2F3F3')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey10')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)] - c(2, -3), ylim = latlonBox_local[c(1,2)] + c(-3, 3), expand = FALSE) +
	labs(title=myTitle, #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
#        fill=expression(km^2 / year))
        fill='Trend in Severity [-]')

#	




# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' [-] per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Drought Severity [-]', xlab='',
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
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.96,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.94,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.94,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()
















# variable options:
	# dis; WRI_based_WaterStress_; dis_WRIbasedDroughtSeverity
ncVarFileName = 'qr'


# testing output, squinty eye test
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, ncVarFileName, 'v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Groundwater Recharge'
#mySubtitle = 'nonirrigated'

geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 1, 1]))
df_sub = subset(geomData, lat > -5000)
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = .8, height = .8)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_viridis(limits=c(0,1000), oob = scales::squish)+#trans = scales::pseudo_log_trans(sigma = 0.000001))	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='red3', col='red3', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
 #        fill=expression(km^2 / year))
        fill='Recharge [mm]')


myTitle = 'Trends in Groundwater Recharge'
#mySubtitle = 'nonirrigated'
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 5, 3, 3]))
df_sub = subset(geomData, lat > -5000)
lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/200
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = 1, height = 1)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_stepsn(colors=(c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac')),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
 #        fill=expression(km^2 / year))
        fill='Trend in Recharge [mm/yr]')







# 1b: country avg
sf_use_s2(FALSE)
world <- ne_countries(scale = "medium", returnclass = "sf")
#usa = ne_states(country='usa')

st_as_sf(world)
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')
geomJoin = st_join(world, geomData_sf, join = st_intersects)

geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/400


ggplot(data = geomAvgMrg_sf) +
#	scale_colour_viridis_c(alpha = 1) +
#	scale_colour_gradientn(colours=rev(c('#F2F3F3', '#F2F3F3', '#FFF2F2', '#FFE5E5', 'orangered1', 'red1','red3','firebrick')),name='drought') +
#	scale_fill_gradient2(low='firebrick', mid='#DBDDDF', high='#196CE1', midpoint = .5, na.value = NA, limits = c(0,1),
#		breaks = c(.25, .5, .75), labels = c('severe drought', 'drought', 'wet conditions'), name='SPEI 3 month') +
#	geom_sf(data = california, fill = '#DBDDDF', color = '#1A232F', size=1.4) +
#	geom_sf(data = states, fill = '#DBDDDF')	+
#	geom_sf(data = counties, fill = NA, color = gray(.5))
#	geom_sf(data = world, fill = NA, color = gray(0.05), size=0.8) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData)) +
	scale_fill_stepsn(colors=(c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac')),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	ylim(-2,2) geomAvgMrg_sf
#	geom_sf(data = mask, fill='#F2F3F3', color='#F2F3F3') +
#	geom_sf(data = counties, fill = NA, color = '#666D74') +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	theme(panel.grid = element_line(colour='#F2F3F3')) +
	theme(panel.background = element_rect(fill = '#F2F3F3', colour='#F2F3F3')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey10')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)] - c(2, -3), ylim = latlonBox_local[c(1,2)] + c(-3, 3), expand = FALSE) +
	labs(title=myTitle, #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
#        fill=expression(km^2 / year))
        fill='Trend in Recharge [mm/yr]')

#	




# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ] 
thisTestDat[ , , c(1,5,6)] = thisTestDat[ , , c(1,5,6)] + 15

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' [mm] per year'
thisScen = 3

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Recharge [mm]', xlab='',
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
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.84,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.84,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.84,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()





























# variable options:
	# dis; q;, WRI_based_WaterStress_; dis_WRIbasedDroughtSeverity
ncVarFileName = 'rootmoist'


# testing output, squinty eye test
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, ncVarFileName, 'v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'RZ Soil Moisture'
#mySubtitle = 'nonirrigated'

geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 1, 1]))
df_sub = subset(geomData, lat > -5000)
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = .8, height = .8)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_viridis(limits=c(0,50), oob = scales::squish)+#trans = scales::pseudo_log_trans(sigma = 0.000001))	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='red3', col='red3', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
 #        fill=expression(km^2 / year))
        fill='Soil Moisture [cm]')


myTitle = 'Trends in Soil Moisture'
#mySubtitle = 'nonirrigated'
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 5, 3, 3]))
df_sub = subset(geomData, lat > -5000)
lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/100
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = 1, height = 1)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_stepsn(colors=(c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac')),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
 #        fill=expression(km^2 / year))
        fill='Trend in Soil Moisture [cm/yr]')







# 1b: country avg
sf_use_s2(FALSE)
world <- ne_countries(scale = "medium", returnclass = "sf")
#usa = ne_states(country='usa')

st_as_sf(world)
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')
geomJoin = st_join(world, geomData_sf, join = st_intersects)

geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/200


ggplot(data = geomAvgMrg_sf) +
#	scale_colour_viridis_c(alpha = 1) +
#	scale_colour_gradientn(colours=rev(c('#F2F3F3', '#F2F3F3', '#FFF2F2', '#FFE5E5', 'orangered1', 'red1','red3','firebrick')),name='drought') +
#	scale_fill_gradient2(low='firebrick', mid='#DBDDDF', high='#196CE1', midpoint = .5, na.value = NA, limits = c(0,1),
#		breaks = c(.25, .5, .75), labels = c('severe drought', 'drought', 'wet conditions'), name='SPEI 3 month') +
#	geom_sf(data = california, fill = '#DBDDDF', color = '#1A232F', size=1.4) +
#	geom_sf(data = states, fill = '#DBDDDF')	+
#	geom_sf(data = counties, fill = NA, color = gray(.5))
#	geom_sf(data = world, fill = NA, color = gray(0.05), size=0.8) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData)) +
	scale_fill_stepsn(colors=(c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac')),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	ylim(-2,2) geomAvgMrg_sf
#	geom_sf(data = mask, fill='#F2F3F3', color='#F2F3F3') +
#	geom_sf(data = counties, fill = NA, color = '#666D74') +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	theme(panel.grid = element_line(colour='#F2F3F3')) +
	theme(panel.background = element_rect(fill = '#F2F3F3', colour='#F2F3F3')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey10')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)] - c(2, -3), ylim = latlonBox_local[c(1,2)] + c(-3, 3), expand = FALSE) +
	labs(title=myTitle, #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
#        fill=expression(km^2 / year))
        fill='Trend in Soil Moisture [cm/yr]')

#	




# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ] 
#thisTestDat[ , , c(1,5,6)] = thisTestDat[ , , c(1,5,6)] + 15

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' [cm] per year'
thisScen = 3

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Soil Moisture [cm]', xlab='',
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
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.96,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.96,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.96,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()






























# variable options:
	# dis; q;, WRI_based_WaterStress_; dis_WRIbasedDroughtSeverity
ncVarFileName = 'tws'


# testing output, squinty eye test
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, ncVarFileName, 'v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Total Water Storage'
#mySubtitle = 'nonirrigated'

geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 1, 1]))
df_sub = subset(geomData, lat > -5000)
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = .8, height = .8)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_viridis(trans = scales::pseudo_log_trans(sigma = 1000))+#limits=c(0,50), oob = scales::squish)+#trans = scales::pseudo_log_trans(sigma = 0.000001))	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='red3', col='red3', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
 #        fill=expression(km^2 / year))
        fill='Total Water Storage [mm]')


myTitle = 'Trends in Total Water Storage'
#mySubtitle = 'nonirrigated'
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 5, 3, 3]))
df_sub = subset(geomData, lat > -5000)
lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/200
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = 1, height = 1)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_stepsn(colors=(c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac')),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
 #        fill=expression(km^2 / year))
        fill='Trend in Total Water Storage [mm/yr]')







# 1b: country avg
sf_use_s2(FALSE)
world <- ne_countries(scale = "medium", returnclass = "sf")
#usa = ne_states(country='usa')

st_as_sf(world)
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')
geomJoin = st_join(world, geomData_sf, join = st_intersects)

geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/500


ggplot(data = geomAvgMrg_sf) +
#	scale_colour_viridis_c(alpha = 1) +
#	scale_colour_gradientn(colours=rev(c('#F2F3F3', '#F2F3F3', '#FFF2F2', '#FFE5E5', 'orangered1', 'red1','red3','firebrick')),name='drought') +
#	scale_fill_gradient2(low='firebrick', mid='#DBDDDF', high='#196CE1', midpoint = .5, na.value = NA, limits = c(0,1),
#		breaks = c(.25, .5, .75), labels = c('severe drought', 'drought', 'wet conditions'), name='SPEI 3 month') +
#	geom_sf(data = california, fill = '#DBDDDF', color = '#1A232F', size=1.4) +
#	geom_sf(data = states, fill = '#DBDDDF')	+
#	geom_sf(data = counties, fill = NA, color = gray(.5))
#	geom_sf(data = world, fill = NA, color = gray(0.05), size=0.8) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData)) +
	scale_fill_stepsn(colors=(c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac')),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	ylim(-2,2) geomAvgMrg_sf
#	geom_sf(data = mask, fill='#F2F3F3', color='#F2F3F3') +
#	geom_sf(data = counties, fill = NA, color = '#666D74') +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	theme(panel.grid = element_line(colour='#F2F3F3')) +
	theme(panel.background = element_rect(fill = '#F2F3F3', colour='#F2F3F3')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey10')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)] - c(2, -3), ylim = latlonBox_local[c(1,2)] + c(-3, 3), expand = FALSE) +
	labs(title=myTitle, #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
#        fill=expression(km^2 / year))
        fill='Trend in Total Water Storage [mm/yr]')

#	




# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ] 
#thisTestDat[ , , c(1,5,6)] = thisTestDat[ , , c(1,5,6)] + 15

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' [mm] per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Total Water Storage [mm]', xlab='',
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
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.96,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.96,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.96,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()











# variable options:
	# dis; q; rootmoist; tws; WRI_based_WaterStress_; dis_WRIbasedDroughtSeverity
ncVarFileName = 'WRI_basedStreamflowInterannualVariability_'


# testing output, squinty eye test
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, ncVarFileName, 'v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Interannual Variability'
#mySubtitle = 'nonirrigated'

geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 1, 1]))
df_sub = subset(geomData, lat > -5000)
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = .8, height = .8)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_viridis(limits=c(0,2))+#trans = scales::pseudo_log_trans(sigma = 1000))+#limits=c(0,50), oob = scales::squish)+#trans = scales::pseudo_log_trans(sigma = 0.000001))	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='red3', col='red3', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
 #        fill=expression(km^2 / year))
        fill='Interannual Variability [cv]')


myTitle = 'Trends in Interannual Variability'
#mySubtitle = 'nonirrigated'
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 5, 3, 3]))
df_sub = subset(geomData, lat > -5000)
lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/100
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = 1, height = 1)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_stepsn(colors=(rev(c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'))),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
 #        fill=expression(km^2 / year))
        fill='Trend in Interannual Variability [cv]')







# 1b: country avg
sf_use_s2(FALSE)
world <- ne_countries(scale = "medium", returnclass = "sf")
#usa = ne_states(country='usa')

st_as_sf(world)
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')
geomJoin = st_join(world, geomData_sf, join = st_intersects)

geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/500


ggplot(data = geomAvgMrg_sf) +
#	scale_colour_viridis_c(alpha = 1) +
#	scale_colour_gradientn(colours=rev(c('#F2F3F3', '#F2F3F3', '#FFF2F2', '#FFE5E5', 'orangered1', 'red1','red3','firebrick')),name='drought') +
#	scale_fill_gradient2(low='firebrick', mid='#DBDDDF', high='#196CE1', midpoint = .5, na.value = NA, limits = c(0,1),
#		breaks = c(.25, .5, .75), labels = c('severe drought', 'drought', 'wet conditions'), name='SPEI 3 month') +
#	geom_sf(data = california, fill = '#DBDDDF', color = '#1A232F', size=1.4) +
#	geom_sf(data = states, fill = '#DBDDDF')	+
#	geom_sf(data = counties, fill = NA, color = gray(.5))
#	geom_sf(data = world, fill = NA, color = gray(0.05), size=0.8) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData)) +
	scale_fill_stepsn(colors=rev(c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac')),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	ylim(-2,2) geomAvgMrg_sf
#	geom_sf(data = mask, fill='#F2F3F3', color='#F2F3F3') +
#	geom_sf(data = counties, fill = NA, color = '#666D74') +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	theme(panel.grid = element_line(colour='#F2F3F3')) +
	theme(panel.background = element_rect(fill = '#F2F3F3', colour='#F2F3F3')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey10')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)] - c(2, -3), ylim = latlonBox_local[c(1,2)] + c(-3, 3), expand = FALSE) +
	labs(title=myTitle, #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
#        fill=expression(km^2 / year))
        fill='Trend in Interannual Variability [cv]')

#	




# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ] 
#thisTestDat[ , , c(1,5,6)] = thisTestDat[ , , c(1,5,6)] + 15

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' [cv] per year'
thisScen = 3

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Interannual Variability [c v]', xlab='',
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
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.88,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.88,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.88,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()




















# variable options:
	# dis; q; rootmoist; tws; WRI_based_WaterStress_; dis_WRIbasedDroughtSeverity
ncVarFileName = 'WRI_basedStreamflowSeasonalVariability_'


# testing output, squinty eye test
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, ncVarFileName, 'v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Seasonal Variability'
#mySubtitle = 'nonirrigated'

geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 1, 1]))
df_sub = subset(geomData, lat > -5000)
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = .8, height = .8)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_viridis(limits=c(0,2))+#trans = scales::pseudo_log_trans(sigma = 1000))+#limits=c(0,50), oob = scales::squish)+#trans = scales::pseudo_log_trans(sigma = 0.000001))	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='red3', col='red3', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
 #        fill=expression(km^2 / year))
        fill='Seasonal Variability [cv]')


myTitle = 'Trends in Seasonal Variability'
#mySubtitle = 'nonirrigated'
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 5, 3, 3]))
df_sub = subset(geomData, lat > -5000)
lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/200
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = 1, height = 1)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_stepsn(colors=(rev(c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'))),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
    labs(title=myTitle, #subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
 #        fill=expression(km^2 / year))
        fill='Trend in Seasonal Variability [cv]')







# 1b: country avg
sf_use_s2(FALSE)
world <- ne_countries(scale = "medium", returnclass = "sf")
#usa = ne_states(country='usa')

st_as_sf(world)
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')
geomJoin = st_join(world, geomData_sf, join = st_intersects)

geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
geomAvgMrg_sf = st_as_sf(geomAvgMrg)

lmt <- 0.1#ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/1000
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/500


ggplot(data = geomAvgMrg_sf) +
#	scale_colour_viridis_c(alpha = 1) +
#	scale_colour_gradientn(colours=rev(c('#F2F3F3', '#F2F3F3', '#FFF2F2', '#FFE5E5', 'orangered1', 'red1','red3','firebrick')),name='drought') +
#	scale_fill_gradient2(low='firebrick', mid='#DBDDDF', high='#196CE1', midpoint = .5, na.value = NA, limits = c(0,1),
#		breaks = c(.25, .5, .75), labels = c('severe drought', 'drought', 'wet conditions'), name='SPEI 3 month') +
#	geom_sf(data = california, fill = '#DBDDDF', color = '#1A232F', size=1.4) +
#	geom_sf(data = states, fill = '#DBDDDF')	+
#	geom_sf(data = counties, fill = NA, color = gray(.5))
#	geom_sf(data = world, fill = NA, color = gray(0.05), size=0.8) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData)) +
	scale_fill_stepsn(colors=rev(c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac')),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape=2, stroke=2)	+
#	ylim(-2,2) geomAvgMrg_sf
#	geom_sf(data = mask, fill='#F2F3F3', color='#F2F3F3') +
#	geom_sf(data = counties, fill = NA, color = '#666D74') +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	theme(panel.grid = element_line(colour='#F2F3F3')) +
	theme(panel.background = element_rect(fill = '#F2F3F3', colour='#F2F3F3')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey10')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)] - c(2, -3), ylim = latlonBox_local[c(1,2)] + c(-3, 3), expand = FALSE) +
	labs(title=myTitle, #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
#        fill=expression(km^2 / year))
        fill='Trend in Seasonal Variability [cv]')

#	




# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ] 
#thisTestDat[ , , c(1,5,6)] = thisTestDat[ , , c(1,5,6)] + 15

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' [cv] per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Seasonal Variability [c v]', xlab='',
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
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.92,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.92,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.92,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()
