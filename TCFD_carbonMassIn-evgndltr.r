#######################################################
library(data.table)
library(lubridate)
library(ncdf4)
library(easyNCDF)	# for ArrayToNc()
#library(robslopes)	# for TheilSen()
#library(trend)		# for sens.slope()
library(mblm)		# for sens slope mlbm()
library(zoo)		# for na.fill()


#########################################
# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\Trees\\carbonMassInWood\\"
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
ncVarFileName = 'cwood-evgndltr'
saveDate = '03FEB2023'
rcpScenarios = c(126, 370, 585)
whichDecades = seq(10,90,10)
valueType = 1:6



	# initializing start decade
initDates = 1:6
ncname_ukesml = paste0('classic_ukesm1-0-ll_w5e5_ssp370_2015soc_default_', ncVarFileName, '_global_annual_2015_2100.nc')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_ukesml = nc_open(paste0(ncpath, ncname_ukesml))
nc_ukesml_init = ncvar_get(ncin_ukesml,ncVarFileName)[ , , initDates]	# lon, lat, time
	# identifying lat lons before closing data
nc_lat = ncvar_get(ncin_ukesml, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_ukesml, 'lon')
nc_close(ncin_ukesml)

ncname_gfdl = paste0('classic_gfdl-esm4_w5e5_ssp370_2015soc_default_', ncVarFileName, '_global_annual_2015_2100.nc')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
nc_gfdl_init = ncvar_get(ncin_gfdl,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_gfdl)

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))


for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]
	rcpScen = paste0('ssp', rcpScenNum)

	ncname_ukesml = paste0('classic_ukesm1-0-ll_w5e5_', rcpScen, '_2015soc_default_', ncVarFileName, '_global_annual_2015_2100.nc')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_ukesml = nc_open(paste0(ncpath, ncname_ukesml))
	nc_ukesml = ncvar_get(ncin_ukesml,ncVarFileName)	# lon, lat, time

	ncname_gfdl = paste0('classic_gfdl-esm4_w5e5_', rcpScen, '_2015soc_default_', ncVarFileName, '_global_annual_2015_2100.nc')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName)	# lon, lat, time

	nc_date = as.Date("1601-01-01") + ncvar_get(ncin_gfdl, 'time') # time is days after 1601-1-1
	nc_years = unique(year(nc_date))
	numYears = length(nc_years)
	missing_data = 1.00000002004088e+20

	for(i in 1:length(nc_lat))	{
		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j,i, ] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				print(c(i, j))
				gfdl_yrly = na.fill(c(nc_gfdl_init[j,i, initDates], nc_gfdl[j,i, -initDates]), 'extend')
				ukesml_yrly = na.fill(c(nc_ukesml_init[j,i, initDates], nc_ukesml[j,i, -initDates]), 'extend')
				
				gfdl_smth = ksmooth(nc_years, gfdl_yrly, kernel = 'normal', bandwidth = 10, n.points = numYears)$y
				ukesml_smth = ksmooth(nc_years, ukesml_yrly, kernel = 'normal', bandwidth = 10, n.points = numYears)$y
				
#				dataQuantDiffs = diff(quantile(c(gfdl_yrly, hadgem_yrly, ipsl_yrly, miroc_yrly), c(0.25, 0.5, 0.75)))
				dataSdDiffs = sd(c(gfdl_yrly, ukesml_yrly))

	
				theseYears = 1:5
					# defining absolute values
				dataSmoothMed = median(c(gfdl_smth[theseYears], ukesml_smth[theseYears]))
				dataOutArray[j, i, 1, thisScen, 1] = dataSmoothMed
				dataOutArray[j, i, 1, thisScen, 5] = max(dataSmoothMed - dataSdDiffs, 0)
				dataOutArray[j, i, 1, thisScen, 6] =  dataSmoothMed + dataSdDiffs
					# calculating decadal trends (sens slope) and 	decadal significance (spearmans)	
				theseGfdl = gfdl_smth[theseYears]
				theseUkesml = ukesml_smth[theseYears]
				dataOutArray[j, i, 1, thisScen, 3] = median(mblm(theseGfdl ~ theseYears)$coefficients[2],
					mblm(theseUkesml ~ theseYears)$coefficients[2], na.rm=TRUE)
				theseGfdl = gfdl_yrly[theseYears]
				theseUkesml = ukesml_yrly[theseYears]
				dataOutArray[j, i, 1, thisScen, 4] = median(cor.test(theseYears, theseGfdl, method='spearman')$p.value,
					cor.test(theseYears, theseUkesml, method='spearman')$p.value, na.rm=TRUE)

				for(thisDecade in 2:9)	{
					theseYears = (thisDecade - 2) * 10 + 7:16
						# defining absolute values
					dataSmoothMed = median(c(gfdl_smth[theseYears], ukesml_smth[theseYears]))
					dataOutArray[j, i, thisDecade, thisScen, 1] = dataSmoothMed
					dataOutArray[j, i, thisDecade, thisScen, 5] = max(dataSmoothMed - dataSdDiffs, 0)
					dataOutArray[j, i, thisDecade, thisScen, 6] =  dataSmoothMed + dataSdDiffs
						# calculating decadal trends (sens slope) and 	decadal significance (spearmans)	
					theseYears = 1:((thisDecade - 2) * 10 + 16)
					theseGfdl = gfdl_smth[theseYears]
					theseUkesml = ukesml_smth[theseYears]
						dataOutArray[j, i, thisDecade, thisScen, 3] = median(mblm(theseGfdl ~ theseYears)$coefficients[2],
						mblm(theseUkesml ~ theseYears)$coefficients[2], na.rm=TRUE)
					theseGfdl = gfdl_yrly[theseYears]
					theseUkesml = ukesml_yrly[theseYears]
					dataOutArray[j, i, thisDecade, thisScen, 4] = median(cor.test(theseYears, theseGfdl, method='spearman')$p.value,
						cor.test(theseYears, theseUkesml, method='spearman')$p.value, na.rm=TRUE)
				}	
					
					# calculating long-term trends (sens slope)
#				dataOutArray[j, i, , thisScen, 5] = dataOutArray[j, i, 9, thisScen, 3]

					# calculating long-term significance (spearmans)
#				dataOutArray[j, i, , thisScen, 6] = dataOutArray[j, i, 9, thisScen, 4]				

			}
		}
	saveRDS(dataOutArray, file=paste0(ncpath, 'data_out.rds'))
	}
	nc_close(ncin_gfdl)
	nc_close(ncin_ukesml)
}

dataOutArray = readRDS(file=paste0(ncpath, 'data_out.rds'))

	# defining quantiles 
maskedLocs126 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset126 =  dataOutArray[ , , 1, 1, 1][-maskedLocs126]
maskedLocs370 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset370 =  dataOutArray[ , , 1, 2, 1][-maskedLocs370]
maskedLocs585 = which(is.na(dataOutArray[ , , 1, 3, 1]))
histDatSubset585 =  dataOutArray[ , , 1, 3, 1][-maskedLocs585]
histQuants = rev(quantile(c(histDatSubset126, histDatSubset370, histDatSubset585), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(whichDecades))	{
	dataOutArray[ , , i, , 2] = 1
	for(j in 1:(length(histQuants)))	{
		dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 2, 2][dataOutArray[ , , i, 2, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 3, 2][dataOutArray[ , , i, 3, 1] <= histQuants[j]] = j
	}
	dataOutArray[ , , i, 1, 2][maskedLocs126] = NA
	dataOutArray[ , , i, 2, 2][maskedLocs370] = NA
	dataOutArray[ , , i, 3, 2][maskedLocs585] = NA
}



tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Carbon Mass in Wood (evgndltr) - kg / m^2'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

lon = nc_lon
dim(lon) = length(lon)
metadata = list(lon = list(units = 'degrees'))
attr(lon, 'variables') = metadata
names(dim(lon)) = 'lon'

lat = nc_lat
dim(lat) = length(lat)
metadata = list(lat = list(units = 'degrees'))
attr(lat, 'variables') = metadata
names(dim(lat)) = 'lat'

decade = whichDecades
dim(decade) = length(decade)
metadata = list(decade = list(units = 'decades_of_21st_C'))
attr(decade, 'variables') = metadata
names(dim(decade)) = 'decade'

rcpScen = rcpScenarios
dim(rcpScen) = length(rcpScen)
metadata = list(rcpScen = list(units = 'RCP_scenario'))
attr(rcpScen, 'variables') = metadata
names(dim(rcpScen)) = 'rcpScen'

valueClass = 1:6#valueType
dim(valueClass) = length(valueClass)
metadata = list(valueClass = list(units = 'class'))
attr(valueClass, 'variables') = metadata
names(dim(valueClass)) = 'valueClass'

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncOutputPath, ncVarFileName, 'v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncOutputPath, ncVarFileName, 'v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,2,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,3,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,3])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,4])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,5])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,6])

image(nc_lon, rev(nc_lat), nc_testDat[,,9,2,2] - nc_testDat[,,1,2,2])

image(nc_lon, rev(nc_lat), nc_testDat[,,9,1,1] - nc_testDat[,,1,1,1])

image(nc_lon, rev(nc_lat), nc_testDat[,,1,2,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,2,3])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,2,4])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,2,5])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,2,6])



# fancy plots
library(sf)
library(ggplot2)
library(viridis)
library(ggfx)
library(rnaturalearth)

ocean50 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_ocean\\ne_10m_ocean.shp'))

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat > -50)
latlonBox = c(25, 65, -10, 60) #(minlat, maxlat, minlon, maxlon)
myTitle = 'Change in Annual Avg Wood Mass'
mySubtitle = 'for temperate needleleaf evergreen trees [kg / m^2]'
ggplot(data = df_sub) +
	with_blur(geom_raster(aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
	scale_fill_viridis()	+
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
#	coord_quickmap(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2])) +
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2])) + 
	labs(x = NULL, y = NULL, title=myTitle, subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill=expression(kg / m^2))
	
	
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 2, 1] - nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat > -50)
lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/20
latlonBox = c(-55, 90, -180, 180)#c(25, 65, -10, 60) #(minlat, maxlat, minlon, maxlon)
myTitle = 'Change in Annual Avg Wood Mass'
mySubtitle = 'for temperate needleleaf evergreen trees [kg / m^2]'
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = .75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis()	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	scale_fill_gradient2(low = 'darkred', high = 'darkgreen', mid = 'white', trans = scales::pseudo_log_trans(sigma = 10))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                           '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #          n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
#	coord_quickmap(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2])) +
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2])) + 
#	coord_fixed(expand = FALSE)	+
	labs(x = NULL, y = NULL, title=myTitle, subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill=expression(kg / m^2))
	
	




library(rnaturalearth)
library(maps)
sf_use_s2(FALSE)
world <- ne_countries(scale = "medium", returnclass = "sf")
#usa = ne_states(country='usa')

st_as_sf(world)
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 5, 2, 3]))
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

geomJoin = st_join(world, geomData_sf, join = st_intersects)

geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
geomAvgMrg_sf = st_as_sf(geomAvgMrg)


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
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#				             n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis_c(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.001), direction = -1)+#, trans = 'log10') +
#	ylim(-2,2) geomAvgMrg_sf
#	geom_sf(data = mask, fill='#F2F3F3', color='#F2F3F3') +
#	geom_sf(data = counties, fill = NA, color = '#666D74') +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	theme(panel.grid = element_line(colour='#F2F3F3')) +
	theme(panel.background = element_rect(fill = '#F2F3F3', colour='#F2F3F3')) +
	theme(legend.position = 'bottom',
		legend.background = element_rect(fill='white', colour='grey10')) +
	guides(fill=guide_legend(title="Change in Annual Avg Wood Mass (temperate needleleaf evergreen) [kg / m^2]")) +
	coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE)
#	


	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_viridis()	+
