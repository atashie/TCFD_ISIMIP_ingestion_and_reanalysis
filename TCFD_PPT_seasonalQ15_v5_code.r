##############################################
library(data.table)
library(lubridate)
library(ncdf4)
library(easyNCDF)	# for ArrayToNc()
#library(robslopes)	# for TheilSen()
#library(trend)		# for sens.slope()
library(mblm)		# for sens slope mlbm()



#########################################
# reading in climai netcdf data
###########################################
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
ncpath = "J:\\Cai_data\\TCFD\\Precipitation\\"
saveDate = '10MAR2023'
ncVarFileName1 = 'rainf'
rcpScenarios = c(26, 60, 85)
whichDecades = seq(10,90,10)
valueType = 1:6

	# initializing start decade
initDates = 1:168
ncin_gfdl = nc_open(paste0(ncpath, 'totPrecip_gfdl_26.nc'))
nc_gfdl_init = ncvar_get(ncin_gfdl, ncVarFileName1)[ , , initDates]	# lon, lat, time
	# identifying lat lons before closing data
nc_lat = ncvar_get(ncin_gfdl, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_gfdl, 'lon')
nc_close(ncin_gfdl)

ncin_hadgem = nc_open(paste0(ncpath, 'totPrecip_hadgem_26.nc'))
nc_hadgem_init = ncvar_get(ncin_hadgem, ncVarFileName1)[ , , initDates]	# lon, lat, time
nc_close(ncin_hadgem)

ncin_ipsl = nc_open(paste0(ncpath, 'totPrecip_ipsl_26.nc'))
nc_ipsl_init = ncvar_get(ncin_ipsl, ncVarFileName1)[ , , initDates]	# lon, lat, time
nc_close(ncin_ipsl)

ncin_miroc = nc_open(paste0(ncpath, 'totPrecip_miroc_26.nc'))
nc_miroc_init = ncvar_get(ncin_miroc, ncVarFileName1)[ , , initDates]	# lon, lat, time
nc_close(ncin_miroc)

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))

	# rescaling for signif digits
scalar = 60*60*24*30.4375 # secs per month


for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]

	ncname_gfdl = paste0('totPrecip_gfdl_',rcpScenNum,'.nc')
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName1)	# lon, lat, time

	ncname_hadgem = paste0('totPrecip_hadgem_',rcpScenNum,'.nc')
	ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
	nc_hadgem = ncvar_get(ncin_hadgem,ncVarFileName1)	# lon, lat, time
	
	ncname_ipsl = paste0('totPrecip_ipsl_',rcpScenNum,'.nc')
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
	nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName1)	# lon, lat, time
	
	ncname_miroc = paste0('totPrecip_miroc_',rcpScenNum,'.nc')
	ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
	nc_miroc = ncvar_get(ncin_miroc,ncVarFileName1)	# lon, lat, time

	nc_date = as.Date("1661-01-01") + ncvar_get(ncin_miroc, 'time') * 30.4375# time is months after 1661-1-1
	nc_years = unique(year(nc_date))
	numYears = length(nc_years)
	missing_data = 1.00000002004088e+20
	nonZeroGenerator = sample(seq(0.0001,0.001,length.out=1000), length(nc_years) * 12, replace = TRUE)

	for(i in 1:length(nc_lat))	{
		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j,i, ] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				print(c(i, j))
				gfdl_all = c(nc_gfdl_init[j,i, initDates], nc_gfdl[j,i, -initDates]) * scalar + nonZeroGenerator 
				hadgem_all = c(nc_hadgem_init[j,i, initDates], nc_hadgem[j,i, -initDates]) * scalar + nonZeroGenerator
				ipsl_all = c(nc_ipsl_init[j,i, initDates], nc_ipsl[j,i, -initDates]) * scalar + nonZeroGenerator
				miroc_all = c(nc_miroc_init[j,i, initDates], nc_miroc[j,i, -initDates]) * scalar + nonZeroGenerator

				gfdl_yrly = NULL
				hadgem_yrly = NULL
				ipsl_yrly = NULL
				miroc_yrly = NULL
				for(thisYear in nc_years)	{
					theseDates = which(year(nc_date) == thisYear)
					gfdl_yrly =   c(gfdl_yrly,   quantile(gfdl_all[theseDates], 0.15) * 12)
					hadgem_yrly = c(hadgem_yrly, quantile(hadgem_all[theseDates], 0.15) * 12)
					ipsl_yrly =   c(ipsl_yrly,   quantile(ipsl_all[theseDates], 0.15) * 12)
					miroc_yrly =  c(miroc_yrly,  quantile(miroc_all[theseDates], 0.15) * 12)
				}
				
				gfdl_smth = ksmooth(nc_years, gfdl_yrly, kernel = 'normal', bandwidth = 10, n.points = numYears)$y
				hadgem_smth = ksmooth(nc_years, hadgem_yrly, kernel = 'normal', bandwidth = 10, n.points = numYears)$y
				ipsl_smth = ksmooth(nc_years, ipsl_yrly, kernel = 'normal', bandwidth = 10, n.points = numYears)$y
				miroc_smth = ksmooth(nc_years, miroc_yrly, kernel = 'normal', bandwidth = 10, n.points = numYears)$y


				for(thisDecade in 1:9)	{
					theseYears = (thisDecade - 1) * 10 + 5:14
						# defining absolute values
					dataSmoothMed = median(c(gfdl_smth[theseYears], hadgem_smth[theseYears], ipsl_smth[theseYears], miroc_smth[theseYears]))
					dataOutArray[j, i, thisDecade, thisScen, 1] = dataSmoothMed
					dataQuantDiffs = diff(quantile(c(gfdl_yrly[theseYears], hadgem_yrly[theseYears], ipsl_yrly[theseYears], miroc_yrly[theseYears]), c(0.25, 0.5, 0.75)))
					dataOutArray[j, i, thisDecade, thisScen, 5] = dataSmoothMed - abs(dataQuantDiffs[1])
					dataOutArray[j, i, thisDecade, thisScen, 6] =  dataSmoothMed + abs(dataQuantDiffs[2])
						# calculating decadal trends (sens slope) and 	decadal significance (spearmans)	
					theseYears = 1:((thisDecade - 1) * 10 + 14)
					theseGfdl = gfdl_yrly[theseYears]
					theseHadgem = hadgem_yrly[theseYears]
					theseIpsl = ipsl_yrly[theseYears]
					theseMiroc =  miroc_yrly[theseYears]
					dataOutArray[j, i, thisDecade, thisScen, 3] = median(mblm(theseGfdl ~ theseYears)$coefficients[2],
						mblm(theseHadgem ~ theseYears)$coefficients[2],
						mblm(theseIpsl ~ theseYears)$coefficients[2],
						mblm(theseMiroc ~ theseYears)$coefficients[2])
					dataOutArray[j, i, thisDecade, thisScen, 4] = median(cor.test(theseYears, theseGfdl, method='spearman')$p.value,
						cor.test(theseYears, theseHadgem, method='spearman')$p.value,
						cor.test(theseYears, theseIpsl, method='spearman')$p.value,
						cor.test(theseYears, theseMiroc, method='spearman')$p.value, na.rm=TRUE)
				}	
						
						# calculating long-term trends (sens slope)
		#			dataOutArray[j, i, , thisScen, 5] = dataOutArray[j, i, 9, thisScen, 3]

						# calculating long-term significance (spearmans)
		#			dataOutArray[j, i, , thisScen, 6] = dataOutArray[j, i, 9, thisScen, 4]				
			}
		}
	}
	saveRDS(dataOutArray, file=paste0(ncpath, 'data_out.rds'))
	nc_close(ncin_gfdl)
	nc_close(ncin_hadgem)
	nc_close(ncin_ipsl)
	nc_close(ncin_miroc)
}


dataOutArray = readRDS(file=paste0(ncpath, 'data_out.rds'))

	# defining quantiles
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 3, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 3, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(whichDecades))	{
	dataOutArray[ , , i, , 2] = 1
	for(j in 1:(length(histQuants)))	{
		dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 2, 2][dataOutArray[ , , i, 2, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 3, 2][dataOutArray[ , , i, 3, 1] <= histQuants[j]] = j
	}
	dataOutArray[ , , i, 1, 2][maskedLocs26] = NA
	dataOutArray[ , , i, 2, 2][maskedLocs60] = NA
	dataOutArray[ , , i, 3, 2][maskedLocs85] = NA
}



tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Precip Seasonal 15th Quantile'))
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
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncOutputPath,'Precipitation_seasonalQ15_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncOutputPath, 'Precipitation_seasonalQ15_v2_processed.nc'))
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

image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,1] - nc_testDat[,,1,3,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,1] - nc_testDat[,,1,2,1])

image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,3,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,2,2])

image(nc_lon, rev(nc_lat), nc_testDat[,,1,2,6] - nc_testDat[,,1,2,5])






# fancy plots
library(sf)
library(ggplot2)
library(viridis)
geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 1, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 1, 1]))
df_sub = subset(geomData, lat > -5000)
latlonBox = c(25, 65, -10, 60) #(minlat, maxlat, minlon, maxlon)
latlonBox = c(-55, 75, -180, 180) #(minlat, maxlat, minlon, maxlon)
myTitle = 'Sugar Beet Yield'
mySubtitle = 'nonirrigated'
ggplot(data = df_sub, aes(fill = plotData, y = lat, x = lon)) +
	geom_tile(width = 1, height = 1)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_viridis()	+
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2])) +
    labs(title=myTitle, subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill=expression(tons / hectare / year))
	








library(rnaturalearth)
library(maps)
sf_use_s2(FALSE)
world <- ne_countries(scale = "medium", returnclass = "sf")

st_as_sf(world)
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
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
	scale_fill_viridis_c(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.001))+#, trans = 'log10') +
#	ylim(-2,2) geomAvgMrg_sf
#	geom_sf(data = mask, fill='#F2F3F3', color='#F2F3F3') +
#	geom_sf(data = counties, fill = NA, color = '#666D74') +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	theme(panel.grid = element_line(colour='#F2F3F3')) +
	theme(panel.background = element_rect(fill = '#F2F3F3', colour='#F2F3F3')) +
	theme(legend.position = 'bottom',
		legend.background = element_rect(fill='white', colour='grey10')) +
	guides(fill=guide_legend(title="Water Depletion")) +
	coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE)
#	


	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_viridis()	+
