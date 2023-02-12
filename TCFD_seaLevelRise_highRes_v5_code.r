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


#########################################
# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\SeaLevelRise\\highResIndicators\\"
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'

#ncin_hstAvg = nc_open(paste0(ncpath, 'historical_tide_actual-value_1985-2014_MHHW_v1.nc'))
#ncin_ftrAvg = nc_open(paste0(ncpath, 'future_tide_actual-value_2021-2050_MHHW_v1.nc'))
#nc_seaLevel_hst = ncvar_get(ncin_hstAvg, 'MHHW')
#nc_seaLevel_ftr = ncvar_get(ncin_ftrAvg, 'MHHW')

ncin_hstAvg = nc_open(paste0(ncpath, 'historical_tide_actual-value_1985-2014_MSL_v1.nc'))
ncin_ftrAvg = nc_open(paste0(ncpath, 'future_tide_actual-value_2021-2050_MSL_v1.nc'))
nc_seaLevel_hst = ncvar_get(ncin_hstAvg, 'MSL')
nc_seaLevel_ftr = ncvar_get(ncin_ftrAvg, 'MSL')
nc_lon = ncvar_get(ncin_hstAvg, 'station_x_coordinate')
nc_lat = ncvar_get(ncin_hstAvg, 'station_y_coordinate')

whichDecades = c('1985-2014', '2021-2050')
returnPeriods = c(1,5,10,50,100)

# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_seaLevel_ftr) * length(whichDecades) * (length(returnPeriods) + 2)), 
	dim = c(length(nc_seaLevel_ftr), length(whichDecades), (length(returnPeriods) + 2)))

dataOutArray[ , 1, 1] = nc_lon 
dataOutArray[ , 2, 1] = nc_lon 
dataOutArray[ , 1, 2] = nc_lat 
dataOutArray[ , 2, 2] = nc_lat 



for(j in 1:length(returnPeriods))	{
	thisRp = returnPeriods[j]

	ncname_ftr = paste0('future_surge_actual-value_2021-2050_rp', thisRp, '_ensemble-median_best-fit_v1.nc')
	ncname_hst = paste0('historical_surge_actual-value_1985-2014_rp', thisRp, '_ensemble-median_best-fit_v1.nc')

	ncname_hst = paste0('historical_surge_actual-value_1985-2014_rp', thisRp, '_ensemble-median_best-fit_v1.nc')
	
	ncin_ftr = nc_open(paste0(ncpath, ncname_ftr))
	ncin_hst = nc_open(paste0(ncpath, ncname_hst))
		
	nc_fldElev_ftr = ncvar_get(ncin_ftr, 'ensemble_median_surge') + nc_seaLevel_ftr
	nc_fldElev_hst = ncvar_get(ncin_hst, 'ensemble_median_surge') + nc_seaLevel_hst

	dataOutArray[ , 1, j+2] = nc_fldElev_hst
	dataOutArray[ , 2, j+2] = nc_fldElev_ftr
	
	nc_close(ncin_ftr)
	nc_close(ncin_hst)
}	


tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Storm Surge - meters above MSL in 1986-2005'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('value', 'decade', 'returnPeriod')

decade = c(10, 40)
dim(decade) = length(decade)
metadata = list(decade = list(units = 'decades_of_21st_C'))
attr(decade, 'variables') = metadata
names(dim(decade)) = 'decade'

returnPeriod = c(180, 90, returnPeriods)
dim(returnPeriod) = length(returnPeriod)
metadata = list(returnPeriod = list(units = 'class'))
attr(returnPeriod, 'variables') = metadata
names(dim(returnPeriod)) = 'returnPeriod'

	# saving ncdf
ArrayToNc(list(tcfdVariable, decade, returnPeriod), file_path = paste0(ncOutputPath, 'coastalFlood_elev', 'v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncOutputPath, 'coastalFlood_elev', 'v2_processed.nc'))
nc_testDat = ncvar_get(myNC, 'tcfdVariable')
nc_lat = nc_testDat[ , 1, 2]	# lat is given from high to low
nc_lon = nc_testDat[ , 1, 1]


# fancy plots
library(sf)
library(ggplot2)
library(viridis)
#geomData = st_as_sf(data.frame(lat = nc_lat, lon = nc_lon, plotData = as.vector(nc_testDat[ 3, 1, 1])), coords = c('lon', 'lat'))
thisPeriod = 5
geomData = data.frame(lat = nc_lat, lon = nc_lon, plotData = as.vector(nc_testDat[ , 1, thisPeriod + 2]))

df_sub = subset(geomData, lat > -50)
latlonBox = c(15, 35, -110, -70) #(minlat, maxlat, minlon, maxlon)
myTitle = 'Sea Level Rise + Storm Surge (2020-2050)'
mySubtitle = paste0('Return Period of ', returnPeriods[thisPeriod], ' Years')
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = .77, height = .77)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_viridis(limits = c(0.01, 10), trans = 'log10')	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2])) +
    labs(title=myTitle, subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill=paste0('meters above MSL'))
	
	
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 2, 1] - nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat > -50)
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/40
latlonBox = c(25, 65, -10, 60) #(minlat, maxlat, minlon, maxlon)
myTitle = 'Change in Sugar Beet Yield'
mySubtitle = 'nonirrigated'
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = .5, height = .5)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis()	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	coord_quickmap(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2])) +
    labs(title=myTitle, subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill=expression(tons / hectare / year))
	
	
