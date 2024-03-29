#######################################################
library(data.table)
library(lubridate)
library(ncdf4)
library(easyNCDF)	# for ArrayToNc()
#library(robslopes)	# for TheilSen()
#library(trend)		# for sens.slope()
library(mblm)		# for sens slope mlbm()



#########################################
# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\SugarbeetRainfed\\"
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
ncVarFileName = 'yield-sgb-noirr'
saveDate = '16JAN2023'
rcpScenarios = c(26, 60)
whichDecades = seq(10,90,10)
valueType = 1:6



	# initializing start decade
initDates = 1:14
ncname_gfdl = paste0('lpjml_gfdl-esm2m_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
nc_gfdl_init = ncvar_get(ncin_gfdl,ncVarFileName)[ , , initDates]	# lon, lat, time
	# identifying lat lons before closing data
nc_lat = ncvar_get(ncin_gfdl, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_gfdl, 'lon')
nc_close(ncin_gfdl)

ncname_hadgem = paste0('lpjml_hadgem2-es_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_hadgem-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
nc_hadgem_init = ncvar_get(ncin_hadgem,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_hadgem)

ncname_ipsl = paste0('lpjml_ipsl-cm5a-lr_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_ipsl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
nc_ipsl_init = ncvar_get(ncin_ipsl,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_ipsl)

ncname_miroc = paste0('lpjml_miroc5_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_miroc-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
nc_miroc_init = ncvar_get(ncin_miroc,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_miroc)

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))


for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]
	rcpScen = paste0('rcp', rcpScenNum)

	ncname_gfdl = paste0('lpjml_gfdl-esm2m_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName)	# lon, lat, time

	ncname_hadgem = paste0('lpjml_hadgem2-es_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
	nc_hadgem = ncvar_get(ncin_hadgem,ncVarFileName)	# lon, lat, time
	
	ncname_ipsl = paste0('lpjml_ipsl-cm5a-lr_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
	nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName)	# lon, lat, time
	
	ncname_miroc = paste0('lpjml_miroc5_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
	nc_miroc = ncvar_get(ncin_miroc,ncVarFileName)	# lon, lat, time

	nc_date = as.Date("1661-01-01") + ncvar_get(ncin_miroc, 'time') * 365.25# time is months after 1661-1-1
	nc_years = unique(year(nc_date))
	numYears = length(nc_years)
	missing_data = 1.00000002004088e+20

	for(i in 1:length(nc_lat))	{
		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j,i, ] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				print(c(i, j))
				gfdl_yrly = c(nc_gfdl_init[j,i, initDates], nc_gfdl[j,i, -initDates])
				hadgem_yrly = c(nc_hadgem_init[j,i, initDates], nc_hadgem[j,i, -initDates])
				ipsl_yrly = c(nc_ipsl_init[j,i, initDates], nc_ipsl[j,i, -initDates])
				miroc_yrly = c(nc_miroc_init[j,i, initDates], nc_miroc[j,i, -initDates])
			
				gfdl_smth = ksmooth(nc_years, gfdl_yrly, kernel = 'normal', bandwidth = 10, n.points = numYears)$y
				hadgem_smth = ksmooth(nc_years, hadgem_yrly, kernel = 'normal', bandwidth = 10, n.points = numYears)$y
				ipsl_smth = ksmooth(nc_years, ipsl_yrly, kernel = 'normal', bandwidth = 10, n.points = numYears)$y
				miroc_smth = ksmooth(nc_years, miroc_yrly, kernel = 'normal', bandwidth = 10, n.points = numYears)$y

#				dataQuantDiffs = diff(quantile(c(gfdl_yrly, hadgem_yrly, ipsl_yrly, miroc_yrly), c(0.25, 0.5, 0.75)))
				dataSdDiffs = sd(c(gfdl_yrly, hadgem_yrly, ipsl_yrly, miroc_yrly))

				for(thisDecade in 1:9)	{
					theseYears = (thisDecade - 1) * 10 + 5:14
						# defining absolute values
					dataSmoothMed = median(c(gfdl_smth[theseYears], hadgem_smth[theseYears], ipsl_smth[theseYears], miroc_smth[theseYears]))
					dataOutArray[j, i, thisDecade, thisScen, 1] = dataSmoothMed
					dataOutArray[j, i, thisDecade, thisScen, 5] = max(dataSmoothMed - dataSdDiffs, 0)
					dataOutArray[j, i, thisDecade, thisScen, 6] =  dataSmoothMed + dataSdDiffs
						# calculating decadal trends (sens slope) and 	decadal significance (spearmans)	
					theseYears = 1:((thisDecade - 1) * 10 + 14)
					theseGfdl = gfdl_smth[theseYears]
					theseHadgem = hadgem_smth[theseYears]
					theseIpsl = ipsl_smth[theseYears]
					theseMiroc =  miroc_smth[theseYears]
						dataOutArray[j, i, thisDecade, thisScen, 3] = median(mblm(theseGfdl ~ theseYears)$coefficients[2],
						mblm(theseHadgem ~ theseYears)$coefficients[2],
						mblm(theseIpsl ~ theseYears)$coefficients[2],
						mblm(theseMiroc ~ theseYears)$coefficients[2])
					theseGfdl = gfdl_yrly[theseYears]
					theseHadgem = hadgem_yrly[theseYears]
					theseIpsl = ipsl_yrly[theseYears]
					theseMiroc =  miroc_yrly[theseYears]
					dataOutArray[j, i, thisDecade, thisScen, 4] = median(cor.test(theseYears, theseGfdl, method='spearman')$p.value,
						cor.test(theseYears, theseHadgem, method='spearman')$p.value,
						cor.test(theseYears, theseIpsl, method='spearman')$p.value,
						cor.test(theseYears, theseMiroc, method='spearman')$p.value, na.rm=TRUE)
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
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(whichDecades))	{
	dataOutArray[ , , i, , 2] = 1
	for(j in 1:(length(histQuants)))	{
		dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 2, 2][dataOutArray[ , , i, 2, 1] <= histQuants[j]] = j
	}
	dataOutArray[ , , i, 1, 2][maskedLocs26] = NA
	dataOutArray[ , , i, 2, 2][maskedLocs60] = NA
#	dataOutArray[ , , i, 3, 2][maskedLocs85] = NA
}



tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Sugar Beet Yield (nonirrigated) - tons / ha / yr'))
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
geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 1, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 1, 1]))
df_sub = subset(geomData, lat > -50)
latlonBox = c(25, 65, -10, 60) #(minlat, maxlat, minlon, maxlon)
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
	
	




library(rnaturalearth)
library(maps)
sf_use_s2(FALSE)
world <- ne_countries(scale = "medium", returnclass = "sf")
usa = ne_states(country='usa')

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
	guides(fill=guide_legend(title="Sugar Beet Yield trend (tons/ha/yr/yr)")) +
	coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE)
#	


	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
	scale_fill_viridis()	+
