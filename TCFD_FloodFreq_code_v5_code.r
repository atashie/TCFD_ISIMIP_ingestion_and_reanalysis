########################################################
### library(hydroGOF)		# for nse calculations
library(dataRetrieval)	# for streamflow data (I think)
library(data.table)
library(sf)
	sf::sf_use_s2(FALSE) # for problem with intersecting spherical w flat
library(lubridate)
library(dplyr) # for left_join() merging by date
library(ncdf4)
library(magrittr)
library(maptools)
#library(FAdist) 	# for the log-pearson type III distribution
#library(moments)	# for skewness
library(lmom)		# for samlmu()
library(easyNCDF)	# for ArrayToNc()
library(mblm)		# for sens slope mlbm()



	# function for translating future flood freq according to historic flood freq
newFldFrqFunc = function(oldQ, newQ, probFld)	{
	# note : reversing old 
		# generating distribution projected time series data
	volQ = newQ
	sortedVolQ = sort(volQ, decreasing=TRUE)
	probQ = c(1:length(volQ)) / (length(volQ) + 1)
	trQ = 1 / probQ

	fit = samlmu(volQ)
	para3 = pelpe3(fit)

	lp3Accum = cdfpe3(sortedVolQ, para3)
	fittedTr3 = 1 / (1 - lp3Accum)

	thisProbFld = probFld
	theseFldVols = seq(max(volQ)*5, min(volQ), length.out = 10000)
	thisProbFldVol = theseFldVols[
		which.min(abs(thisProbFld - (1 / (1 -  cdfpe3(theseFldVols, para3)))))
		]

		# creating a orig distribution with the future time series data 
	origVolQ = oldQ
	origSortedvolQ = sort(origVolQ, decreasing = TRUE)
	origFit = samlmu(origVolQ)
	origPara3 = pelpe3(origFit)

		# identifying the return period of a 'probFld' from the historic data according to the distribution from the future data
	origLp3Accum = cdfpe3(thisProbFldVol, origPara3)
	origFittedTr3 = 1 / (1 - origLp3Accum)
	return(origFittedTr3)
}









#########################################
# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\Flash Floods\\"
ncVarFileName = 'maxdis'
saveDate = '04JAN2023'
rcpScenarios = c(26, 60, 85)
recurIntrvls = c(10, 20, 50, 100, 200, 500)
valueType = 1:3		# recurrence interval, significance
whichDecades = seq(10,90,10)

startHist = as.Date('1920-01-01')

		# initializing start decade
initDates = 1:168
ncname_gfdl = paste0('clm45_gfdl-esm2m_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
nc_gfdl_init = ncvar_get(ncin_gfdl,ncVarFileName)[ , , initDates]	# lon, lat, time
	# identifying lat lons before closing data
nc_lat = ncvar_get(ncin_gfdl, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_gfdl, 'lon')
nc_close(ncin_gfdl)

ncname_hadgem = paste0('clm45_hadgem2-es_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_hadgem-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
nc_hadgem_init = ncvar_get(ncin_hadgem,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_hadgem)

ncname_ipsl = paste0('clm45_ipsl-cm5a-lr_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_ipsl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
nc_ipsl_init = ncvar_get(ncin_ipsl,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_ipsl)

ncname_miroc = paste0('clm45_miroc5_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_miroc-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
nc_miroc_init = ncvar_get(ncin_miroc,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_miroc)

	# array for holding outputs
myMissingData = NA
fldRecurArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(recurIntrvls) * length(valueType) * length(rcpScenarios)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(recurIntrvls), length(valueType), length(rcpScenarios)))


for(thisScen in c(3))	{
	rcpScenNum = rcpScenarios[thisScen]
	rcpScen = paste0('rcp', rcpScenNum)
	
	ncname_gfdl = paste0('clm45_gfdl-esm2m_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName)		# lon, lat, time
	
	ncname_hadgem = paste0('clm45_hadgem2-es_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
	nc_hadgem = ncvar_get(ncin_hadgem,ncVarFileName)	# lon, lat, time
	
	ncname_ipsl = paste0('clm45_ipsl-cm5a-lr_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
	nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName)		# lon, lat, time
	
	ncname_miroc = paste0('clm45_miroc5_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
	nc_miroc = ncvar_get(ncin_miroc,ncVarFileName)		# lon, lat, time
	
	nc_date = as.Date("1661-01-01") + ncvar_get(ncin_miroc, 'time') * 30.4375# time is months after 1661-1-1
	nc_years = unique(year(nc_date))
	numYears = length(nc_years)
	missing_data = 1.00000002004088e+20

		# for samlmu()
	nonZeroGenerator = sample(seq(0.0001,0.001,length.out=100), length(nc_years))
				
	for(i in 1:length(which(nc_lat > -55)))	{
		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j,i, ] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				print(c(i,j))
				gfdl_all = c(nc_gfdl_init[j,i, initDates], nc_gfdl[j,i, -initDates]) + nonZeroGenerator
				hadgem_all = c(nc_hadgem_init[j,i, initDates], nc_hadgem[j,i, -initDates]) + nonZeroGenerator
				ipsl_all = c(nc_ipsl_init[j,i, initDates], nc_ipsl[j,i, -initDates]) + nonZeroGenerator
				miroc_all = c(nc_miroc_init[j,i, initDates], nc_miroc[j,i, -initDates]) + nonZeroGenerator

				gfdl_yrly = NULL
				hadgem_yrly = NULL
				ipsl_yrly = NULL
				miroc_yrly = NULL
				for(thisYear in nc_years)	{
					theseDates = which(year(nc_date) == thisYear)
					gfdl_yrly = c(gfdl_yrly, max(gfdl_all[theseDates]))
					hadgem_yrly = c(hadgem_yrly, max(hadgem_all[theseDates]))
					ipsl_yrly = c(ipsl_yrly, max(ipsl_all[theseDates]))
					miroc_yrly = c(miroc_yrly, max(miroc_all[theseDates]))
				}
	
				for(intrvl in 1:length(recurIntrvls))	{
					gfdl_floodProbs = NULL
					hadgem_floodProbs = NULL
					ipsl_floodProbs = NULL
					miroc_floodProbs = NULL
					for(thisDecade in 1:9)	{
						theseYears = (thisDecade - 1) * 10 + 5:14
						gfdl_floodProbs =   c(gfdl_floodProbs,   newFldFrqFunc(oldQ = gfdl_yrly[1:24],   newQ = gfdl_yrly[theseYears],   probFld = recurIntrvls[intrvl]))
						hadgem_floodProbs = c(hadgem_floodProbs, newFldFrqFunc(oldQ = hadgem_yrly[1:24], newQ = hadgem_yrly[theseYears], probFld = recurIntrvls[intrvl]))
						ipsl_floodProbs =   c(ipsl_floodProbs,   newFldFrqFunc(oldQ = ipsl_yrly[1:24],   newQ = ipsl_yrly[theseYears],   probFld = recurIntrvls[intrvl]))
						miroc_floodProbs =  c(miroc_floodProbs,  newFldFrqFunc(oldQ = miroc_yrly[1:24],  newQ = miroc_yrly[theseYears],  probFld = recurIntrvls[intrvl]))
					}

					gfdl_smth = ksmooth(1:9, gfdl_floodProbs, kernel = 'normal', bandwidth = 4, n.points = length(gfdl_floodProbs))$y
					hadgem_smth = ksmooth(1:9, hadgem_floodProbs, kernel = 'normal', bandwidth = 4, n.points = length(gfdl_floodProbs))$y
					ipsl_smth = ksmooth(1:9, ipsl_floodProbs, kernel = 'normal', bandwidth = 4, n.points = length(gfdl_floodProbs))$y
					miroc_smth = ksmooth(1:9, miroc_floodProbs, kernel = 'normal', bandwidth = 4, n.points = length(gfdl_floodProbs))$y

					for(thisDecade in 1:9)	{
						# defining absolute values
						dataSmoothMed = median(c(gfdl_smth[thisDecade], hadgem_smth[thisDecade], ipsl_smth[thisDecade], miroc_smth[thisDecade]))
						fldRecurArray[j, i, thisDecade, intrvl, 1, thisScen] = dataSmoothMed
#						dataQuantDiffs = diff(quantile(c(gfdl_yrly[theseYears], hadgem_yrly[theseYears], ipsl_yrly[theseYears], miroc_yrly[theseYears]), c(0.25, 0.5, 0.75)))
#						dataOutArray[j, i, thisDecade, thisScen, 5] = dataSmoothMed - abs(dataQuantDiffs[1])
#						dataOutArray[j, i, thisDecade, thisScen, 6] =  dataSmoothMed + abs(dataQuantDiffs[2])
							# calculating decadal trends (sens slope) and 	decadal significance (spearmans)	
						theseYears = 1:((thisDecade - 1) * 10 + 14)
						theseGfdl = gfdl_yrly[theseYears]
						theseHadgem = hadgem_yrly[theseYears]
						theseIpsl = ipsl_yrly[theseYears]
						theseMiroc =  miroc_yrly[theseYears]
						fldRecurArray[j, i, thisDecade, intrvl, 2, thisScen] = median(mblm(theseGfdl ~ theseYears)$coefficients[2],
							mblm(theseHadgem ~ theseYears)$coefficients[2],
							mblm(theseIpsl ~ theseYears)$coefficients[2],
							mblm(theseMiroc ~ theseYears)$coefficients[2])
						fldRecurArray[j, i, thisDecade, intrvl, 3, thisScen] = median(cor.test(theseYears, theseGfdl, method='spearman')$p.value,
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
	}
	nc_close(ncin_gfdl)
	nc_close(ncin_hadgem)
	nc_close(ncin_ipsl)
	nc_close(ncin_miroc)
	saveRDS(fldRecurArray, file=paste0(ncpath, 'data_out6.rds')) # rcp 8.5
#	saveRDS(fldRecurArray, file=paste0(ncpath, 'data_out5.rds')) # rcp 2.6
#	saveRDS(fldRecurArray, file=paste0(ncpath, 'data_out4.rds')) # rcp 6.0
}
#saveRDS(fldRecurArray, file=paste0(ncpath, 'data_out6.rds'))

fldRccurArray = readRDS(file=paste0(ncpath, 'data_outCombined.rds'))

#names(dim(fldRecurArray)) = c('lon', 'lat', 'decade', 'recurInterval', 'rcpScen')
#ArrayToNc(list(fldRecurArray = fldRecurArray, lon = nc_lon, lat = nc_lat, decade = seq(10,90,10), recurInterval = recurIntrvls), file_path = 'ex.nc')


fldRecurIntrvl =fldRccurArray
metadata = list(fldRecurIntrvl = list(units = 'recurrence interval ( / yr)'))
attr(fldRecurIntrvl, 'variables') = metadata
names(dim(fldRecurIntrvl)) = c('lon', 'lat', 'decade', 'recurInterval', 'valueTypes', 'rcpScen')

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

recurInterval = recurIntrvls
dim(recurInterval) = length(recurInterval)
metadata = list(recurInterval = list(units = 'recurrence_intervals'))
attr(recurInterval, 'variables') = metadata
names(dim(recurInterval)) = 'recurInterval'

valueTypes = valueType
dim(valueTypes) = length(valueTypes)
metadata = list(valueTypes = list(units = 'interval_or_significance'))
attr(valueTypes, 'variables') = metadata
names(dim(valueTypes)) = 'valueTypes'

rcpScen = rcpScenarios
dim(rcpScen) = length(rcpScen)
metadata = list(rcpScen = list(units = 'RCP_scenario'))
attr(rcpScen, 'variables') = metadata
names(dim(rcpScen)) = 'rcpScen'


ArrayToNc(list(fldRecurIntrvl, lon, lat, decade, recurInterval, valueTypes, rcpScen), file_path = paste0(ncpath, 'floodRecurIntervals_v5.nc'))
#ArrayToNc(list(fldRecurIntrvl, lon, lat, decade, recurInterval, valueTypes, rcpScen), file_path = paste0(ncpath, 'floodRecurIntervals_v5_scen2.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'floodRecurIntervals_v5.nc'))
#myNC = nc_open(paste0(ncpath, 'floodRecurIntervals_v5_scen2.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'fldRecurIntrvl')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,2,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,3,1])

image(nc_lon, rev(nc_lat), nc_testDat[,,9,1,1,2] - nc_testDat[,,1,1,1,2])

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,2,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,2,3])

image(nc_lon, rev(nc_lat), nc_testDat[,,9,1,1,3] - nc_testDat[,,1,1,1,3])



########################################################################################################################
#	step 2: masking water bodies from flood depth database
# https://www.rdocumentation.org/packages/rnaturalearth/versions/0.1.0
library(rnaturalearth)
library(raster)
library(data.table)
library(sf)
sf_use_s2(FALSE) # for error suppression of Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  :
land10 <- ne_download(scale = 10, type = 'land', category = 'physical') #scal50 scale110
sp::plot(land10)
land10Sf = st_as_sf(land10)
	# lons and lats to match the flood rasters

#floodRast = raster('J:\\Cai_data\\TCFD\\CurrentFloodHazard\\floodMapGL_rp100y.tif')
lats = rev(seq(-54.00824,83.2251,length.out=16468))
lons = seq(-166.8, 180, length.out=41616)
#spPoints = st_multipoint(rbind(c(lats[1], lons[1]), c(lats[100],lons[50])))
pointDf = data.frame('lon' = rep(lons, length(lats)),
	'lat' = rep(lats, each=length(lons)))
	# pointDf is too long to allocate to one SF object, so splitting to 4
theseRows = 1:5000000 #685332288
pointSf = st_as_sf(pointDf[theseRows,], coords = c('lon', 'lat'))
st_crs(pointSf) = st_crs(land10)
goodPoints = data.frame(row1 = rep(NA, nrow(pointSf)))
goodPoints[,1]  = st_intersects(pointSf, land10Sf[1,], sparse=FALSE)[,1]
goodPoints[,2] = st_intersects(pointSf, land10Sf[2,], sparse=FALSE)[,1]
goodPoints[,3] = st_intersects(pointSf, land10Sf[3,], sparse=FALSE)[,1]
goodPoints[,4] = st_intersects(pointSf, land10Sf[4,], sparse=FALSE)[,1]
goodPoints[,5] = st_intersects(pointSf, land10Sf[5,], sparse=FALSE)[,1]
goodPoints[,6] = st_intersects(pointSf, land10Sf[6,], sparse=FALSE)[,1]
goodPoints[,7] = st_intersects(pointSf, land10Sf[7,], sparse=FALSE)[,1]
goodPoints[,8] = st_intersects(pointSf, land10Sf[9,], sparse=FALSE)[,1]
goodPoints[,9] = st_intersects(pointSf, land10Sf[10,], sparse=FALSE)[,1]
goodPoints[,10] = st_intersects(pointSf, land10Sf[11,], sparse=FALSE)[,1]
goodPoints[,11] = apply(goodPoints, 1, any)
goodRows = theseRows[goodPoints[,11]]

iter = 1
write.csv(goodRows, paste0('goodRows', iter, '.csv'))
#sp::plot(goodPointsSf)
	# repeating for all news rows
theseRows = theseRows + 5000000 #685332288
maxRow = 685332288
while(theseRows[1] <= 685332288)	{
	if(last(theseRows) > maxRow)	{
		theseRows = seq(theseRows[1], maxRow, 1)
	}
	print(theseRows[1] / maxRow)
	pointSf = st_as_sf(pointDf[theseRows,], coords = c('lon', 'lat'))
	st_crs(pointSf) = st_crs(land10)
	goodPoints = data.frame(row1 = rep(NA, nrow(pointSf)))
	goodPoints[,1]  = st_intersects(pointSf, land10Sf[1,], sparse=FALSE)[,1]
	goodPoints[,2] = st_intersects(pointSf, land10Sf[2,], sparse=FALSE)[,1]
	goodPoints[,3] = st_intersects(pointSf, land10Sf[3,], sparse=FALSE)[,1]
	goodPoints[,4] = st_intersects(pointSf, land10Sf[4,], sparse=FALSE)[,1]
	goodPoints[,5] = st_intersects(pointSf, land10Sf[5,], sparse=FALSE)[,1]
	goodPoints[,6] = st_intersects(pointSf, land10Sf[6,], sparse=FALSE)[,1]
	goodPoints[,7] = st_intersects(pointSf, land10Sf[7,], sparse=FALSE)[,1]
	goodPoints[,8] = st_intersects(pointSf, land10Sf[9,], sparse=FALSE)[,1]
	goodPoints[,9] = st_intersects(pointSf, land10Sf[10,], sparse=FALSE)[,1]
	goodPoints[,10] = st_intersects(pointSf, land10Sf[11,], sparse=FALSE)[,1]
	goodPoints[,11] = apply(goodPoints, 1, any)
	goodRows = theseRows[goodPoints[,11]]
	#goodRows = c(goodRows, theseRows[goodPoints[,11]])
	#write.table(goodRows, 'goodRows2.txt')
	#saveRDS(goodRows, 'goodRows2.rds')
	#gg=readRDS('goodRows2.rds')
	iter = iter + 1
	write.csv(goodRows, paste0('goodRows', iter, '.csv'))

	theseRows = theseRows + 5000000 #685332288
	#sp::plot(goodPointsDf)
}


dataOutArray = array(rep(0, length(lons) * length(lats)), 
	dim = c(length(lons), length(lats)))

for(i in 1:138)	{
	theseGoodRows = read.csv(paste0('goodRows', i, '.csv'))$x
	print(theseGoodRows[1])
	dataOutArray[theseGoodRows] = 1
}
	
saveRDS(dataOutArray, 'J:\\Cai_data\\TCFD\\CurrentFloodHazard\\LandMaskArray.rds')








########################################################################################################################
# step 3 intersecting recurrence intervals with flood depth
library(raster)
library(ncdf4)
library(data.table)
customerName = 'Corbion' #'Richs Foods'
locationFootprint = 2		# how big is the footprint of the location of interest? in number of 'boxes' to search to the left and right (so 0 is equal to 1 km^2, 1 is 9 km^2, 2 is 25 km^2, etc.
dataOutputLoc = 'J:\\Cai_data\\TCFD\\CustomerOutputs\\'
waterMaskLoc = 'J:\\Cai_data\\TCFD\\CurrentFloodHazard\\LandMaskArray.rds'



	# reading in table of customer locations
customerPath = 'J:\\Cai_data\\TCFD\\locations\\'
customerCsv = 'Corbion_Locations_Nov2022 - Sheet1.csv' #'Richs-sample-locations.csv'
customerTable = fread(paste0(customerPath, customerCsv))

	# defining rcp scenarios, recurrence intervals, and decades of interest; this should not change for the foreseeable future
rcpScenarios = c('RCP 2.6', 'RCP 6.0', 'RCP 8.5')
recurIntrvls = c(10, 20, 50, 100, 200, 500)
whichDecades = seq(10,90,10)

	
	# read in flood recurrence data
ncpath = "J:\\Cai_data\\TCFD\\Flash Floods\\"
fldRcrIntNC = nc_open(paste0(ncpath, 'floodRecurIntervals_v4.nc'))
nc_lat = ncvar_get(fldRcrIntNC, 'lat')
nc_lon = ncvar_get(fldRcrIntNC, 'lon')
fldRcrVals = ncvar_get(fldRcrIntNC, 'fldRecurIntrvl')


	# initializing data output
dataOutput = data.frame(User = NA, Location = NA, Region = NA, Subregion = NA, Lat = NA, Lon = NA,
	Hazard = NA, Hazard_Measure = NA, Decade = NA, Scenario = NA,
	Raw_Hazard_Value = NA, Percentile_Score = NA, Relative_Hazard_Score = NA, Decadal_Trend_Strength = NA, Decadal_Trend_Significance = NA, Long_Term_Trend_Strength = NA, Long_Term_Trend_Significance = NA)

	# read in historic floods data
fileLoc = 'J:\\Cai_data\\TCFD\\CurrentFloodHazard'
fldDepthList = list()
fldDepthList[[1]] = raster(paste0(fileLoc, '\\floodMapGL_rp10y.tif'))
fldDepthList[[2]] = raster(paste0(fileLoc, '\\floodMapGL_rp20y.tif'))
fldDepthList[[3]] = raster(paste0(fileLoc, '\\floodMapGL_rp50y.tif'))
fldDepthList[[4]] = raster(paste0(fileLoc, '\\floodMapGL_rp100y.tif'))
fldDepthList[[5]] = raster(paste0(fileLoc, '\\floodMapGL_rp200y.tif'))
fldDepthList[[6]] = raster(paste0(fileLoc, '\\floodMapGL_rp500y.tif'))
myWaterMask = readRDS(waterMaskLoc)

	# identifying lat lon coordinates for tiffs
tif_lat = rev(seq(-54.00824,83.2251,length.out=16468))
tif_lon = seq(-166.8, 180, length.out=41616)


for(thisIntrvl in 1:length(recurIntrvls))	{
	hazardDepthName = paste0("Avg Flood Depth (m) ", recurIntrvls[thisIntrvl], 'yr Flood')
	hazardLikliName = paste0('Flood Areal Extent (%) ', recurIntrvls[thisIntrvl], 'yr Flood')
	for(j in 1:nrow(customerTable))	{
		closeTiffLons = which.min(abs(customerTable$Lon[j] - tif_lon))
		closeTiffLats = which.min(abs(customerTable$Lat[j] - tif_lat))
		
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
			closeNCLon = which.min(abs(customerTable$Lon[j] - nc_lon))
			closeNCLat = which.min(abs(customerTable$Lat[j] - nc_lat))
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
						closestFldRcrIntrvl = 	last(which(thisFldRcrVal > recurIntrvls))
						
						theseFloodImpacts = fldDepthList[[closestFldRcrIntrvl]][theseLats, theseLons]
						theseFloodImpacts[is.na(theseFloodImpacts)] = 0
						theseFloodImpacts = theseFloodImpacts * thisWaterMask
	
						prevFloodImpacts = fldDepthList[[thisIntrvl]][theseLats, theseLons]
						prevFloodImpacts[is.na(prevFloodImpacts)] = 0
						prevFloodImpacts = prevFloodImpacts * thisWaterMask
	
						
						if(any(!is.na(theseFloodImpacts)))	{
							avgFloodDepth = mean(theseFloodImpacts, na.rm=TRUE)
							avgFloodDepthChng = avgFloodDepth - mean(prevFloodImpacts, na.rm=TRUE) / thisDecade

							avgFloodLikli = 100 * (length(which(theseFloodImpacts > 0)) / length(!is.na(theseFloodImpacts)))
							avgFloodLikliChng = avgFloodLikli - 100 * (length(which(prevFloodImpacts > 0)) / length(!is.na(prevFloodImpacts)) / (thisDecade - 1))
						

							dataOutput = rbind(dataOutput,
								data.table(
								User = customerName,
								Location = customerTable$Location[j],
								Region = customerTable$Region[j],
								Subregion = customerTable$Subregion[j],
								Lat = customerTable$Lat[j],
								Lon = customerTable$Lon[j],
								Hazard = "River Flood",
								Hazard_Measure = c(hazardDepthName, hazardLikliName),
								Decade = paste0('20', whichDecades[thisDecade], 's'),
								Scenario = rcpScenarios[thisScenario],
								Raw_Hazard_Value = c(avgFloodDepth, avgFloodLikli),					# Raw_Hazard_Value
								Percentile_Score = NA,												# Percentile_Score
								Relative_Hazard_Score = NA,											# Relative_Hazard_Score
								Decadal_Trend_Strength = c(avgFloodDepthChng, avgFloodLikliChng),	# Decadal_Trend_Strength
								Decadal_Trend_Significance = thisFldRcrSignif,						# Decadal_Trend_Significance
								Long_Term_Trend_Strength = NA,										# Long_Term_Trend_Strength
								Long_Term_Trend_Significance = fldRcrSignif_all))					# Long_Term_Trend_Significance					

						} 	else	{ 
							dataOutput = rbind(dataOutput,
								data.table(
								User = customerName,
								Location = customerTable$Location[j],
								Region = customerTable$Region[j],
								Subregion = customerTable$Subregion[j],
								Lat = customerTable$Lat[j],
								Lon = customerTable$Lon[j],
								Hazard = "River Flood",
								Hazard_Measure = c(hazardDepthName, hazardLikliName),
								Decade = paste0('20', whichDecades[thisDecade], 's'),
								Scenario = rcpScenarios[thisScenario],
								Raw_Hazard_Value = 0,								# Raw_Hazard_Value
								Percentile_Score = NA,								# Percentile_Score
								Relative_Hazard_Score = NA,							# Relative_Hazard_Score
								Decadal_Trend_Strength = 0,							# Decadal_Trend_Strength
								Decadal_Trend_Significance = thisFldRcrSignif,		# Decadal_Trend_Significance
								Long_Term_Trend_Strength = NA,						# Long_Term_Trend_Strength
								Long_Term_Trend_Significance = fldRcrSignif_all))	# Long_Term_Trend_Significance						}
						}
					}	else	{
						dataOutput = rbind(dataOutput,
							data.table(
								User = customerName,
								Location = customerTable$Location[j],
								Region = customerTable$Region[j],
								Subregion = customerTable$Subregion[j],
								Lat = customerTable$Lat[j],
								Lon = customerTable$Lon[j],
								Hazard = "River Flood",
								Hazard_Measure = c(hazardDepthName, hazardLikliName),
								Decade = paste0('20', rep(whichDecades, each = 2), 's'),
								Scenario = rcpScenarios[thisScenario],
								Raw_Hazard_Value = 0,								# Raw_Hazard_Value
								Percentile_Score = NA,								# Percentile_Score
								Relative_Hazard_Score = NA,							# Relative_Hazard_Score
								Decadal_Trend_Strength = 0,						# Decadal_Trend_Strength
								Decadal_Trend_Significance = 0,		# Decadal_Trend_Significance
								Long_Term_Trend_Strength = NA,						# Long_Term_Trend_Strength
								Long_Term_Trend_Significance = 0))	# Long_Term_Trend_Significance
								
					}
				}
				dataOutput[which(dataOutput$Location == customerTable$Location[j] & dataOutput$Scenario == rcpScenarios[thisScenario]), ]$Long_Term_Trend_Strength = last(dataOutput)$Decadal_Trend_Strength
			}
		} 	else {
			dataOutput = rbind(dataOutput,
				data.table(
					User = customerName,
					Location = customerTable$Location[j],
					Region = customerTable$Region[j],
					Subregion = customerTable$Subregion[j],
					Lat = customerTable$Lat[j],
					Lon = customerTable$Lon[j],
					Hazard = "River Flood",
					Hazard_Measure = c(hazardDepthName, hazardLikliName),
					Decade = paste0('20', rep(whichDecades, each = 2), 's'),
					Scenario = rcpScenarios[1],
					Raw_Hazard_Value = 0,				# Raw_Hazard_Value
					Percentile_Score = NA,				# Percentile_Score
					Relative_Hazard_Score = NA,			# Relative_Hazard_Score
					Decadal_Trend_Strength = 0,			# Decadal_Trend_Strength
					Decadal_Trend_Significance = 0,		# Decadal_Trend_Significance
					Long_Term_Trend_Strength = 0,		# Long_Term_Trend_Strength
					Long_Term_Trend_Significance = 0),	# Long_Term_Trend_Significance
				data.table(
					User = customerName,
					Location = customerTable$Location[j],
					Region = customerTable$Region[j],
					Subregion = customerTable$Subregion[j],
					Lat = customerTable$Lat[j],
					Lon = customerTable$Lon[j],
					Hazard = "River Flood",
					Hazard_Measure = c(hazardDepthName, hazardLikliName),
					Decade = paste0('20', rep(whichDecades, each = 2), 's'),
					Scenario = rcpScenarios[2],
					Raw_Hazard_Value = 0,				# Raw_Hazard_Value
					Percentile_Score = NA,				# Percentile_Score
					Relative_Hazard_Score = NA,			# Relative_Hazard_Score
					Decadal_Trend_Strength = 0,			# Decadal_Trend_Strength
					Decadal_Trend_Significance = 0,		# Decadal_Trend_Significance
					Long_Term_Trend_Strength = 0,		# Long_Term_Trend_Strength
					Long_Term_Trend_Significance = 0))	# Long_Term_Trend_Significance
		}
		print(c(j, thisIntrvl))
	}
}	

dataOutput$Raw_Hazard_Value = as.numeric(dataOutput$Raw_Hazard_Value)
dataOutput = dataOutput[-1,]


	# defining relativ flood hazard
basSeqDepth = seq(0.01, 10, length.out=80)
basSeqLikli = seq(1, 100, length.out=80)
relFloodHazardDepth = (basSeqDepth^2 / max(basSeqDepth^2)) * 10 + min(basSeqDepth)
relFloodHazardLikli = basSeqLikli
#relFloodHazard = c(rep(0, 33), seq(0.01,10,length.out=(67)))
dataOutput$Percentile = 1
depthRows = which(dataOutput$Hazard_Measure == hazardDepthName)
likliRows = which(dataOutput$Hazard_Measure == hazardLikliName)
for(ll in 1:length(relFloodHazardDepth))	{
	dataOutput$Percentile[depthRows][which(dataOutput$Raw_Hazard_Value[depthRows] > relFloodHazardDepth[ll])] = ll + 20
	dataOutput$Percentile[likliRows][which(dataOutput$Raw_Hazard_Value[likliRows] > relFloodHazardLikli[ll])] = ll + 20
}

fileName = ifelse(chooseDepth, paste0(customerName, '_', Sys.Date(), '_avgDepth_Footprint_', (locationFootprint*2+1), 'km'), paste0(customerName, '_', Sys.Date(), '_relLikelihood_Footprint_', (locationFootprint*2+1), 'km'))
fwrite(dataOutput, paste0(dataOutputLoc, fileName, '.csv'))





















rejiggerData = all_data[c(1:200,1090000:1090200), c('User', 'Region', 'Subregion', 'Lat', 'Lon', 'Hazard', 'Decade', 'Scenario', 'Raw_Hazard_Value', 'Percentile', 'Relative_Hazard_Score')]
fwrite(rejiggerData, paste0(ncpath, ncVarFileName, "_", "sampleData_", saveDate,  ".csv"))

	

	closeLonLats = NULL
	theseLabels = NULL
	theseRegions = NULL
	theseSubregions = NULL
	for(k in 1:length(closeLons))	{
		newCloseLonLats = which(hazardMaps$Lat == closeLats[k] & hazardMaps$Lon == closeLons[k])
		closeLonLats = c(closeLonLats, newCloseLonLats)
		theseLabels = c(theseLabels, rep(customerTable$Name[k], length(newCloseLonLats)))
		theseRegions = c(theseRegions, rep(customerTable$Region[k], length(newCloseLonLats)))
		theseSubregions = c(theseSubregions, rep(customerTable$Subregion[k], length(newCloseLonLats)))
	}












dataOutArray[ , , , , 2][dataOutArray[ , , , , 2] != missing_data

all_data$Percentile = 1
for(i in 2:length(histQuants))	{
	all_data$Percentile[which(all_data$Raw_Hazard_Value > histQuants[i])] = i
}
	# converting fraction to percent, and rounding for Looker
all_data$Raw_Hazard_Value = round(all_data$Raw_Hazard_Value * 100, 6)

	# identifying relative hazard values with numbers for plotting
all_data$Relative_Hazard_Score = 1
all_data$Relative_Hazard_Score[all_data$Percentile > 33] = 2
all_data$Relative_Hazard_Score[all_data$Percentile > 67] = 3










		# array for holding outputs
#	fldRecurArray = array(rep(-(10^10), length(tif_lat) * length(tif_lon) * length(whichDecades) * length(recurIntrvls) * length(rcpScenarios)), 
#		dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(recurIntrvls), length(rcpScenarios)))







ncpath = "J:\\Cai_data\\TCFD\\Flash Floods\\"
ncVarFileName = 'fldRecurIntrvl'
saveDate = '10OCT2022'
rcpScenarios = c(26, 60)
recurIntrvls = c(10, 20, 50, 100, 200, 500)
whichDecades = seq(10,90,10)
startHist = as.Date('1920-01-01')

	# reading in dummy data for lat lons
ncname_dummy = paste0('clm45_gfdl-esm2m_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_dummy = nc_open(paste0(ncpath, ncname_dummy))
nc_lat = ncvar_get(ncin_dummy, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_dummy, 'lon')


	# array for holding outputs
fldRecurArray = array(rep(-(10^10), length(nc_lon) * length(nc_lat) * length(whichDecades) * length(recurIntrvls) * length(rcpScenarios)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(recurIntrvls), length(rcpScenarios)))









########################################################################################################################
# step 2: interfacing recurrence intervals with flood depth
library(rnaturalearth)
library(sf)
library(raster)

ncpath = "J:\\Cai_data\\TCFD\\Flash Floods\\"
fldRcrInt = nc_open(paste0(ncpath, 'floodRecurIntervals.nc'))


sf::st_crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
+proj=longlat +datum=WGS84 +no_defs


str_name = 'floodMapGL_rp100y.tif' 

for(thisIntrvl in recurIntrvls)	{
	str_name = paste0('floodMapGL_rp', thisIntrvl, 'y.tif')
	fileLoc = 'J:\\Cai_data\\TCFD\\CurrentFloodHazard'
	imported_raster=raster(paste0(fileLoc, '\\', str_name))
	plot(imported_raster, col='red')














#########################################
# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\Flash Floods\\"
	# reading ssp126 climate data 
ncname = "clm45_gfdl-esm2m_ewembi_historical_2005soc_co2_maxdis_global_monthly_1861_2005.nc4"  
ncfname = paste0(ncpath, ncname)
ncin = nc_open(ncfname)
nc_lat = rev(ncvar_get(ncin, 'lat'))	# lat is given high to low
nc_lon = ncvar_get(ncin, 'lon')
nc_date = as.Date("1661-01-01") + ncvar_get(ncin, 'time') * 30.4375# time is days after jan 1 2015
nc_years = unique(year(nc_date))
nc_maxdis_all = ncvar_get(ncin,"maxdis")

ncname_f = "clm45_gfdl-esm2m_ewembi_rcp26_2005soc_co2_maxdis_global_monthly_2006_2099.nc4"#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_maxdis_global_monthly_2006_2099.nc4"  
ncfname_f = paste0(ncpath, ncname_f)
ncin_f = nc_open(ncfname_f)
nc_lat_f = ncvar_get(ncin_f, 'lat')
nc_lon_f = ncvar_get(ncin_f, 'lon')
nc_date_f = as.Date("1661-01-01") + ncvar_get(ncin_f, 'time') * 30.4375# time is days after jan 1 2015
nc_years_f = unique(year(nc_date_f))
nc_maxdis_all_f = ncvar_get(ncin_f,"maxdis")	# in lon,lat,time


ftrFldIntrv_df_5_2029 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_10_2029 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_20_2029 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_30_2029 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_50_2029 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_100_2029 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))

ftrFldIntrv_df_5_3039 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_10_3039 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_20_3039 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_30_3039 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_50_3039 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_100_3039 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))

ftrFldIntrv_df_5_4049 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_10_4049 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_20_4049 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_30_4049 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_50_4049 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_100_4049 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))

ftrFldIntrv_df_5_5059 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_10_5059 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_20_5059 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_30_5059 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_50_5059 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_100_5059 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))

ftrFldIntrv_df_5_6069 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_10_6069 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_20_6069 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_30_6069 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_50_6069 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_100_6069 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))

old_dates_past = which(year(nc_date) == 1920)[1]:length(nc_date)
old_dates_future = 1:which(year(nc_date_f) == 2020)[12]
future_dates2029 = which(year(nc_date_f) == 2010)[1]:which(year(nc_date_f) == 2039)[12]
future_dates3039 = which(year(nc_date_f) == 2020)[1]:which(year(nc_date_f) == 2049)[12]
future_dates4049 = which(year(nc_date_f) == 2030)[1]:which(year(nc_date_f) == 2059)[12]
future_dates5059 = which(year(nc_date_f) == 2040)[1]:which(year(nc_date_f) == 2069)[12]
future_dates6069 = which(year(nc_date_f) == 2050)[1]:length(nc_date_f)

returnFreqs = c(5,10,20,30,50,100)

for(i in 1:length(nc_lat))	{
	for(j in 1:length(nc_lon))	{
		nc_maxdis = c(nc_maxdis_all[j,i,old_dates_past], nc_maxdis_all_f[j,i,old_dates_future])	# in lon,lat,time
		if(any(!is.na(nc_maxdis)))	{
			nc_maxdis_f_2029 = nc_maxdis_all_f[j,i,future_dates2029] #ncvar_get(ncin_f,"maxdis")[j,i,]	# in lon,lat,time
			nc_maxdis_f_3039 = nc_maxdis_all_f[j,i,future_dates3039] #ncvar_get(ncin_f,"maxdis")[j,i,]	# in lon,lat,time
			nc_maxdis_f_4049 = nc_maxdis_all_f[j,i,future_dates4049] #ncvar_get(ncin_f,"maxdis")[j,i,]	# in lon,lat,time
			nc_maxdis_f_5059 = nc_maxdis_all_f[j,i,future_dates5059] #ncvar_get(ncin_f,"maxdis")[j,i,]	# in lon,lat,time
			nc_maxdis_f_6069 = nc_maxdis_all_f[j,i,future_dates6069] #ncvar_get(ncin_f,"maxdis")[j,i,]	# in lon,lat,time			
			if(any(nc_maxdis > 0) & any(nc_maxdis_f_2029 > 0) & any(nc_maxdis_f_3039) & any(nc_maxdis_f_4049) & any(nc_maxdis_f_5059) & any(nc_maxdis_f_6069))	{
					
				print(c(i,j))

				randNoise = seq(0.1^3,0.1^2,length.out = 100)
				nc_maxdis_y = NULL
				for(kh in 1:(length(nc_maxdis)/12))	{
					nc_maxdis_y = c(nc_maxdis_y, max(nc_maxdis[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_maxdis_y_f_2029 = NULL
				for(kh in 1:(length(nc_maxdis_f_2029)/12))	{
					nc_maxdis_y_f_2029 = c(nc_maxdis_y_f_2029, max(nc_maxdis_f_2029[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_maxdis_y_f_3039 = NULL
				for(kh in 1:(length(nc_maxdis_f_3039)/12))	{
					nc_maxdis_y_f_3039 = c(nc_maxdis_y_f_3039, max(nc_maxdis_f_3039[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_maxdis_y_f_4049 = NULL
				for(kh in 1:(length(nc_maxdis_f_4049)/12))	{
					nc_maxdis_y_f_4049 = c(nc_maxdis_y_f_4049, max(nc_maxdis_f_4049[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_maxdis_y_f_5059 = NULL
				for(kh in 1:(length(nc_maxdis_f_5059)/12))	{
					nc_maxdis_y_f_5059 = c(nc_maxdis_y_f_5059, max(nc_maxdis_f_5059[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_maxdis_y_f_6069 = NULL
				for(kh in 1:(length(nc_maxdis_f_6069)/12))	{
					nc_maxdis_y_f_6069 = c(nc_maxdis_y_f_6069, max(nc_maxdis_f_6069[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				ftrFldIntrv_df_5_2029[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_2029, probFld = 5)
				ftrFldIntrv_df_5_3039[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_3039, probFld = 5)
				ftrFldIntrv_df_5_4049[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_4049, probFld = 5)
				ftrFldIntrv_df_5_5059[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_5059, probFld = 5)
				ftrFldIntrv_df_5_6069[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_6069, probFld = 5)
				ftrFldIntrv_df_10_2029[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_2029, probFld = 10)
				ftrFldIntrv_df_10_3039[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_3039, probFld = 10)
				ftrFldIntrv_df_10_4049[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_4049, probFld = 10)
				ftrFldIntrv_df_10_5059[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_5059, probFld = 10)
				ftrFldIntrv_df_10_6069[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_6069, probFld = 10)
				ftrFldIntrv_df_20_2029[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_2029, probFld = 20)
				ftrFldIntrv_df_20_3039[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_3039, probFld = 20)
				ftrFldIntrv_df_20_4049[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_4049, probFld = 20)
				ftrFldIntrv_df_20_5059[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_5059, probFld = 20)
				ftrFldIntrv_df_20_6069[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_6069, probFld = 20)
				ftrFldIntrv_df_30_2029[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_2029, probFld = 30)
				ftrFldIntrv_df_30_3039[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_3039, probFld = 30)
				ftrFldIntrv_df_30_4049[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_4049, probFld = 30)
				ftrFldIntrv_df_30_5059[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_5059, probFld = 30)
				ftrFldIntrv_df_30_6069[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_6069, probFld = 30)
				ftrFldIntrv_df_50_2029[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_2029, probFld = 50)
				ftrFldIntrv_df_50_3039[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_3039, probFld = 50)
				ftrFldIntrv_df_50_4049[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_4049, probFld = 50)
				ftrFldIntrv_df_50_5059[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_5059, probFld = 50)
				ftrFldIntrv_df_50_6069[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_6069, probFld = 50)
				ftrFldIntrv_df_100_2029[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_2029, probFld = 100)
				ftrFldIntrv_df_100_3039[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_3039, probFld = 100)
				ftrFldIntrv_df_100_4049[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_4049, probFld = 100)
				ftrFldIntrv_df_100_5059[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_5059, probFld = 100)
				ftrFldIntrv_df_100_6069[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_6069, probFld = 100)
			}
		} 
	}
#	print(ftrFldIntrv_df)
}
write.csv(ftrFldIntrv_df_5_2029, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_5yrflod_CLM_2029.csv")
write.csv(ftrFldIntrv_df_5_3039, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_5yrflod_CLM_3039.csv")
write.csv(ftrFldIntrv_df_5_4049, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_5yrflod_CLM_4049.csv")
write.csv(ftrFldIntrv_df_5_5059, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_5yrflod_CLM_5059.csv")
write.csv(ftrFldIntrv_df_5_6069, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_5yrflod_CLM_6069.csv")

write.csv(ftrFldIntrv_df_10_2029, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_10yrflod_CLM_2029.csv")
write.csv(ftrFldIntrv_df_10_3039, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_10yrflod_CLM_3039.csv")
write.csv(ftrFldIntrv_df_10_4049, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_10yrflod_CLM_4049.csv")
write.csv(ftrFldIntrv_df_10_5059, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_10yrflod_CLM_5059.csv")
write.csv(ftrFldIntrv_df_10_6069, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_10yrflod_CLM_6069.csv")

write.csv(ftrFldIntrv_df_20_2029, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_20yrflod_CLM_2029.csv")
write.csv(ftrFldIntrv_df_20_3039, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_20yrflod_CLM_3039.csv")
write.csv(ftrFldIntrv_df_20_4049, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_20yrflod_CLM_4049.csv")
write.csv(ftrFldIntrv_df_20_5059, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_20yrflod_CLM_5059.csv")
write.csv(ftrFldIntrv_df_20_6069, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_20yrflod_CLM_6069.csv")

write.csv(ftrFldIntrv_df_30_2029, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_30yrflod_CLM_2029.csv")
write.csv(ftrFldIntrv_df_30_3039, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_30yrflod_CLM_3039.csv")
write.csv(ftrFldIntrv_df_30_4049, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_30yrflod_CLM_4049.csv")
write.csv(ftrFldIntrv_df_30_5059, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_30yrflod_CLM_5059.csv")
write.csv(ftrFldIntrv_df_30_6069, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_30yrflod_CLM_6069.csv")

write.csv(ftrFldIntrv_df_50_2029, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_50yrflod_CLM_2029.csv")
write.csv(ftrFldIntrv_df_50_3039, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_50yrflod_CLM_3039.csv")
write.csv(ftrFldIntrv_df_50_4049, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_50yrflod_CLM_4049.csv")
write.csv(ftrFldIntrv_df_50_5059, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_50yrflod_CLM_5059.csv")
write.csv(ftrFldIntrv_df_50_6069, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_50yrflod_CLM_6069.csv")

write.csv(ftrFldIntrv_df_100_2029, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_100yrflod_CLM_2029.csv")
write.csv(ftrFldIntrv_df_100_3039, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_100yrflod_CLM_3039.csv")
write.csv(ftrFldIntrv_df_100_4049, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_100yrflod_CLM_4049.csv")
write.csv(ftrFldIntrv_df_100_5059, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_100yrflod_CLM_5059.csv")
write.csv(ftrFldIntrv_df_100_6069, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_100yrflod_CLM_6069.csv")



#ftrFldIntrv_df = as.data.frame(read.csv("J:\\Cai_data\\TCFD\\Flash Floods\\test_out.csv"))[,-1]


library(colorRamps)
col5 <- colorRampPalette(c('blue4', 'gray96', 'yellow4'))  #create color ramp starting from blue to red

par(mar=c(2.1,2.1,2.1,1))
image(nc_lon,rev(nc_lat),as.matrix(ftrFldIntrv_df[,ncol(ftrFldIntrv_df):1]), ylim = c(-57,85),
	col=col5(n=11), breaks = seq(0,40,length.out=12),
	ylab='Lat', xlab = 'lon', main = "Changes in flood frequency in 21st C")
data(wrld_simpl)
plot(wrld_simpl,add=TRUE)

if (any(is.na(as.matrix(ftrFldIntrv_df[,ncol(ftrFldIntrv_df):1])))) {
    mmat <- ifelse(is.na(as.matrix(ftrFldIntrv_df[,ncol(ftrFldIntrv_df):1])), 1, NA)
    image(mmat, axes = FALSE, xlab = "", ylab = "", col = 'white', useRaster=TRUE, add = TRUE)
  }

ftrFldIntrv_mt = as.matrix(unlist(ftrFldIntrv_df))

pt1
ftrFldIntrv_sf = st_as_sf(as.vector(ftrFldIntrv_df), coords = 

















ftrFldIntrv_df[is.na(ftrFldIntrv_df)] = 0

# converting df into array 
lon2 = nc_lon	; lat2 = nc_lat
nlon2 = length(nc_lon)	; nlat2 = length(nc_lat)
#time2 = time; tunits2 = tunits	; nt2 = nt

# convert to 2d array
ftrFldIntrv_ar <- array(ftrFldIntrv_df, dim=c(nlon2,nlat2))
dim(ftrFldIntrv_ar)

# plot to check creation of arrays
library(lattice)
library(RColorBrewer)

grid <- expand.grid(lon=nc_lon, lat=nc_lat)

levelplot(ftrFldIntrv_ar ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
  col.regions=(rev(brewer.pal(10,"RdBu"))), main="MTWA (C)")



	#names of path, file, and dname
ncpath = "J:\\Cai_data\\TCFD\\Flash Floods\\test_out.csv"
ncname = "GLM_ftrFlood"
ncfname = paste(ncpath, ncname, '.nc', sep='')
dname = 'fldInt'


	#define dimensions
londim = ncdim_def('lon','degrees',as.double(nc_lon))
latdim = ncdim_def('lat','degrees',as.double(nc_lat))
#timedim <- ncdim_def("time",tunits3,as.double(time3))

# define variables
fillvalue = NA
dlname = "Flood Interval in Years"
fldInt_def = ncvar_def("fldInt","yrs",list(londim,latdim),fillvalue,dlname,prec="single")
#dlname = "mean_temperture_warmest_month"
#mtwa.def <- ncvar_def("mtwa","deg_C",list(londim,latdim,timedim),fillvalue,dlname,prec="single")


# create netCDF file and put arrays
#ncout <- nc_create(ncfname,list(tmp_def,mtco.def,mtwa.def,mat.def),force_v4=TRUE)
ncout <- nc_create(ncfname,list(fldInt_def),force_v4=FALSE)

# put variables
#ncvar_put(ncout,tmp_def,tmp_array3)
#ncvar_put(ncout,mtwa.def,mtwa_array3)
ncvar_put(ncout,fldInt_def,ftrFldIntrv_df)
ncvar_put(ncout,mat.def,mat_array3)

# put additional attributes into dimension and data variables
ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncout,"lat","axis","Y")
ncatt_put(ncout,"time","axis","T")

# add global attributes
ncatt_put(ncout,0,"title",title$value)
ncatt_put(ncout,0,"institution",institution$value)
ncatt_put(ncout,0,"source",datasource$value)
ncatt_put(ncout,0,"references",references$value)
history <- paste("P.J. Bartlein", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"Conventions",Conventions$value)

# Get a summary of the created file:
ncout





dim1 <- dim.def.ncdf( name = "lat", units = "degrees", vals = as.double(nc_lat) )
dim2 <- dim.def.ncdf( name = "lon", units = "degrees", vals = as.double(nc_lon) )
ftrFldIntrv_mt = as.matrix(ftrFldIntrv_df)

# define the EMPTY (elevation) netcdf variable

varz <- var.def.ncdf(name = "floodFreq", units =  "years", 
                    dim = list(dim1, dim2), missval = NA, 
                    longname ="Flood Frequency of what was a 20 year flood")		

nc.ex <- create.ncdf( filename = "J:\\Cai_data\\TCFD\\Flash Floods\\test_out.nc", vars = varz )
put.var.ncdf(nc = nc.ex, varid = varz, vals = ftrFldIntrv_mt)




ggplot(aes(x=X1, y=X2, fill=value), data=mpr) + geom_raster() 
		
myClrs =  colorRamp(c("red", "blue"))
plot(x=nc_lon, y=nc_lat, 



lon1 <- ncdim_def( "Lon", "degrees_east", unique(DB$lon))
lat1 <- ncdim_def( "Lat", "degrees_north", unique(DB$lat))
record <- ncdim_def( "record", "files", list_obs$record_uid,unlim=T)
mv <- -999 # missing value to use
tmpmean_ <- ncvar_def( "tmpmean_", "degrees", list(lon1,lat1,record),mv)

time<-list_obs$record_uid

ncnew <- nc_create( "tmpmean_.nc",tmpmean_)

for( i in 1:length(record))
  ncvar_put( ncnew, tmpmean_, DB$tmpmean_, start=c(1,1,i), 
             count=c(-1,-1,1))
		
		
		
		
		

cal_out = data.frame(sfcf=rep(NA,length(HBV_in_ls)))

#"https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=ORO&SensorNums=8&dur_code=D&Start=1900-01-01&End=2022-03-23"
for(thisClmMod in c(4,6:15))	{
	HBVoutCol = HBVoutCol + 1

#	ncvar_thismod_ppt =  ncvar_get(ncin_ppt_1, "tp")[ ,thisClmMod , , ]	# is [time,model,lon,lat] ugh annoying
	ncvar_thismod_ppt_1 = ncvar_get(ncin_ppt_1, "tp")[ ,thisClmMod , , ]	# is [time,model,lon,lat] and now merging two separate netcdfs
	ncvar_thismod_ppt_2 = ncvar_get(ncin_ppt_2, "tp")[date_range_ppt2,thisClmMod , , ]
	ncvar_thismod_tmin = ncvar_get(ncin, "t2m_min")[, , , thisClmMod]
	ncvar_thismod_tmax = ncvar_get(ncin, "t2m_max")[, , , thisClmMod]




































##############
# edited

########################################################
### library(hydroGOF)		# for nse calculations
library(dataRetrieval)	# for streamflow data (I think)
library(data.table)
library(sf)
	sf::sf_use_s2(FALSE) # for problem with intersecting spherical w flat
library(lubridate)
library(ddplyr) # for left_join() merging by date
library(ncdf4)
library(magrittr)
library(maptools)
#library(FAdist) 	# for the log-pearson type III distribution
#library(moments)	# for skewness
library(lmom)		# for samlmu()
library(easyNCDF)	# for ArrayToNc()



	# function for translating future flood freq according to historic flood freq
newFldFrqFunc = function(oldQ, newQ, probFld)	{
	# note : reversing old 
		# generating distribution projected time series data
	volQ = newQ
	sortedVolQ = sort(volQ, decreasing=TRUE)
	probQ = c(1:length(volQ)) / (length(volQ) + 1)
	trQ = 1 / probQ

	fit = samlmu(volQ)
	para3 = pelpe3(fit)

	lp3Accum = cdfpe3(sortedVolQ, para3)
	fittedTr3 = 1 / (1 - lp3Accum)

	thisProbFld = probFld
	theseFldVols = seq(max(volQ)*5, min(volQ), length.out = 10000)
	thisProbFldVol = theseFldVols[
		which.min(abs(thisProbFld - (1 / (1 -  cdfpe3(theseFldVols, para3)))))
		]

		# creating a orig distribution with the future time series data data
	origVolQ = oldQ
	origSortedvolQ = sort(origVolQ, decreasing = TRUE)
	origFit = samlmu(origVolQ)
	origPara3 = pelpe3(origFit)

		# identifying the return period of a 'probFld' from the historic data according to the distribution from the future data
	origLp3Accum = cdfpe3(thisProbFldVol, origPara3)
	origFittedTr3 = 1 / (1 - origLp3Accum)
	return(origFittedTr3)
}









#########################################
# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\Flash Floods\\"
ncVarFileName = 'maxdis'
saveDate = '15NOV2022'
rcpScenarios = c(26, 60)
recurIntrvls = c(10, 20, 50, 100, 200, 500)
valueType = 1:2		# recurrence interval, significance
whichDecades = seq(10,90,10)

startHist = as.Date('1920-01-01')

	# reading in dummy data for lat lons
ncname_dummy = paste0('clm45_gfdl-esm2m_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_dummy = nc_open(paste0(ncpath, ncname_dummy))
nc_lat = ncvar_get(ncin_dummy, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_dummy, 'lon')


	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(recurIntrvls) * length(valueType) * length(rcpScenarios)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(recurIntrvls), length(valueType), length(rcpScenarios)))




for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]
	rcpScen = paste0('rcp', rcpScenNum)
	
	ncname_gfdl = paste0('clm45_gfdl-esm2m_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName)		# lon, lat, time
	
	ncname_hadgem = paste0('clm45_hadgem2-es_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
	nc_hadgem = ncvar_get(ncin_hadgem,ncVarFileName)	# lon, lat, time
	
	ncname_ipsl = paste0('clm45_ipsl-cm5a-lr_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
	nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName)		# lon, lat, time
	
	ncname_miroc = paste0('clm45_miroc5_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
	nc_miroc = ncvar_get(ncin_miroc,ncVarFileName)		# lon, lat, time
	
	nc_date = as.Date("1661-01-01") + ncvar_get(ncin_miroc, 'time') * 30.4375# time is months after 1661-1-1
	nc_years = unique(year(nc_date))
	missing_data = 1.00000002004088e+20


	dates10s = which(year(nc_date) == 2006)[1]:which(year(nc_date) == 2025)[12]
	dates20s = which(year(nc_date) == 2016)[1]:which(year(nc_date) == 2035)[12]
	dates30s = which(year(nc_date) == 2026)[1]:which(year(nc_date) == 2045)[12]
	dates40s = which(year(nc_date) == 2036)[1]:which(year(nc_date) == 2055)[12]
	dates50s = which(year(nc_date) == 2046)[1]:which(year(nc_date) == 2065)[12]
	dates60s = which(year(nc_date) == 2056)[1]:which(year(nc_date) == 2075)[12]
	dates70s = which(year(nc_date) == 2066)[1]:which(year(nc_date) == 2085)[12]
	dates80s = which(year(nc_date) == 2076)[1]:which(year(nc_date) == 2095)[12]
	dates90s = which(year(nc_date) == 2086)[1]:which(year(nc_date) == 2099)[12]


	for(i in 1:length(nc_lat))	{
		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j, i, dates10s] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				print(c(i,j))
				nc_10s = c(nc_gfdl[j, i, dates10s], nc_hadgem[j, i, dates10s], nc_ipsl[j, i, dates10s], nc_miroc[j, i, dates10s]) 
				nc_20s = c(nc_gfdl[j, i, dates20s], nc_hadgem[j, i, dates20s], nc_ipsl[j, i, dates20s], nc_miroc[j, i, dates20s]) 
				nc_30s = c(nc_gfdl[j, i, dates30s], nc_hadgem[j, i, dates30s], nc_ipsl[j, i, dates30s], nc_miroc[j, i, dates30s]) 
				nc_40s = c(nc_gfdl[j, i, dates40s], nc_hadgem[j, i, dates40s], nc_ipsl[j, i, dates40s], nc_miroc[j, i, dates40s]) 
				nc_50s = c(nc_gfdl[j, i, dates50s], nc_hadgem[j, i, dates50s], nc_ipsl[j, i, dates50s], nc_miroc[j, i, dates50s]) 
				nc_60s = c(nc_gfdl[j, i, dates60s], nc_hadgem[j, i, dates60s], nc_ipsl[j, i, dates60s], nc_miroc[j, i, dates60s]) 
				nc_70s = c(nc_gfdl[j, i, dates70s], nc_hadgem[j, i, dates70s], nc_ipsl[j, i, dates70s], nc_miroc[j, i, dates70s]) 
				nc_80s = c(nc_gfdl[j, i, dates80s], nc_hadgem[j, i, dates80s], nc_ipsl[j, i, dates80s], nc_miroc[j, i, dates80s]) 
				nc_90s = c(nc_gfdl[j, i, dates90s], nc_hadgem[j, i, dates90s], nc_ipsl[j, i, dates90s], nc_miroc[j, i, dates90s]) 
				
				datesSeq1019 = rep(seq(first(dates1019), last(dates1019), 12), 4)
				datesSeq2029 = rep(seq(first(dates2029), last(dates2029), 12), 4)
				datesSeq3039 = rep(seq(first(dates3039), last(dates3039), 12), 4)
				datesSeq4049 = rep(seq(first(dates4049), last(dates4049), 12), 4)
				datesSeq5059 = rep(seq(first(dates5059), last(dates5059), 12), 4)
				datesSeq6069 = rep(seq(first(dates6069), last(dates6069), 12), 4)
				datesSeq7079 = rep(seq(first(dates7079), last(dates7079), 12), 4)
				datesSeq8089 = rep(seq(first(dates8089), last(dates8089), 12), 4)
				datesSeq9099 = rep(seq(first(dates9099), last(dates9099), 12), 4)

				
				randNoise = seq(0.1^3,0.1^2,length.out = 100)
				nc_yr10s = NULL
				for(kh in 1:(length(nc_10s)/12))	{
					nc_yr10s = c(nc_yr10s, max(nc_10s[(1:12)+(12*(kh-1))]) + sample(randNoise,1))
				}
				
				nc_yr20s = NULL
				for(kh in 1:(length(nc_20s)/12))	{
					nc_yr20s = c(nc_yr20s, max(nc_20s[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_yr30s = NULL
				for(kh in 1:(length(nc_30s)/12))	{
					nc_yr30s = c(nc_yr30s, max(nc_30s[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_yr40s = NULL
				for(kh in 1:(length(nc_40s)/12))	{
					nc_yr40s = c(nc_yr40s, max(nc_40s[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_yr50s = NULL
				for(kh in 1:(length(nc_50s)/12))	{
					nc_yr50s = c(nc_yr50s, max(nc_50s[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_yr60s = NULL
				for(kh in 1:(length(nc_60s)/12))	{
					nc_yr60s = c(nc_yr60s, max(nc_60s[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_yr70s = NULL
				for(kh in 1:(length(nc_70s)/12))	{
					nc_yr70s = c(nc_yr70s, max(nc_70s[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_yr80s = NULL
				for(kh in 1:(length(nc_80s)/12))	{
					nc_yr80s = c(nc_yr80s, max(nc_80s[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_yr90s = NULL
				for(kh in 1:(length(nc_90s)/12))	{
					nc_yr90s = c(nc_yr90s, max(nc_90s[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				for(intrvl in 1:length(recurIntrvls))	{
					fldRecurArray[j, i, 1, intrvl, 1, thisScen] = newFldFrqFunc(oldQ = nc_yr10s, newQ = nc_yr10s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 2, intrvl, 1, thisScen] = newFldFrqFunc(oldQ = nc_yr10s, newQ = nc_yr20s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 3, intrvl, 1, thisScen] = newFldFrqFunc(oldQ = nc_yr10s, newQ = nc_yr30s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 4, intrvl, 1, thisScen] = newFldFrqFunc(oldQ = nc_yr10s, newQ = nc_yr40s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 5, intrvl, 1, thisScen] = newFldFrqFunc(oldQ = nc_yr10s, newQ = nc_yr50s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 6, intrvl, 1, thisScen] = newFldFrqFunc(oldQ = nc_yr10s, newQ = nc_yr60s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 7, intrvl, 1, thisScen] = newFldFrqFunc(oldQ = nc_yr10s, newQ = nc_yr70s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 8, intrvl, 1, thisScen] = newFldFrqFunc(oldQ = nc_yr10s, newQ = nc_yr80s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 9, intrvl, 1, thisScen] = newFldFrqFunc(oldQ = nc_yr10s, newQ = nc_yr90s, probFld = recurIntrvls[intrvl])
				}
				
				fldRecurArray[j, i, 1, , 2, thisScen] = cor.test(rep(datesSeq1019, 4), 
														 nc_yr10s, method='spearman')$p.value 
				fldRecurArray[j, i, 2, , 2, thisScen] = cor.test(c(datesSeq1019, datesSeq2029),
														 c(nc_yr10s, nc_yr20s), method='spearman')$p.value
				fldRecurArray[j, i, 3, , 2, thisScen] = cor.test(c(datesSeq1019, datesSeq2029, datesSeq3039),
														 c(nc_yr10s, nc_yr20s, nc_yr30s), method='spearman')$p.value 
				fldRecurArray[j, i, 4, , 2, thisScen] = cor.test(c(datesSeq1019, datesSeq2029, datesSeq3039, datesSeq4049),
														 c(nc_yr10s, nc_yr20s, nc_yr30s, nc_yr40s), method='spearman')$p.value 
				fldRecurArray[j, i, 5, , 2, thisScen] = cor.test(c(datesSeq1019, datesSeq2029, datesSeq3039, datesSeq4049, datesSeq5059),
														 c(nc_yr10s, nc_yr20s, nc_yr30s, nc_yr40s, nc_yr40s, nc_yr50s), method='spearman')$p.value 
				fldRecurArray[j, i, 6, , 2, thisScen] = cor.test(c(datesSeq1019, datesSeq2029, datesSeq3039, datesSeq4049, datesSeq5059, datesSeq6069),
														 c(nc_yr10s, nc_yr20s, nc_yr30s, nc_yr40s, nc_yr40s, nc_yr50s, nc_yr60s), method='spearman')$p.value  
				fldRecurArray[j, i, 7, , 2, thisScen] = cor.test(c(datesSeq1019, datesSeq2029, datesSeq3039, datesSeq4049, datesSeq5059, datesSeq6069, datesSeq7079),
														 c(nc_yr10s, nc_yr20s, nc_yr30s, nc_yr40s, nc_yr40s, nc_yr50s, nc_yr60s, nc_yr70s), method='spearman')$p.value   
				fldRecurArray[j, i, 8, , 2, thisScen] = cor.test(c(datesSeq1019, datesSeq2029, datesSeq3039, datesSeq4049, datesSeq5059, datesSeq6069, datesSeq7079, datesSeq8089),
														 c(nc_yr10s, nc_yr20s, nc_yr30s, nc_yr40s, nc_yr40s, nc_yr50s, nc_yr60s, nc_yr70s, nc_yr80s), method='spearman')$p.value  
				fldRecurArray[j, i, 9, , 2, thisScen] = cor.test(c(datesSeq1019, datesSeq2029, datesSeq3039, datesSeq4049, datesSeq5059, datesSeq6069, datesSeq7079, datesSeq8089, datesSeq9099),
														 c(nc_yr10s, nc_yr20s, nc_yr30s, nc_yr40s, nc_yr40s, nc_yr50s, nc_yr60s, nc_yr70s, nc_yr80s, nc_yr90s), method='spearman')$p.value  
				
			}
		}
	saveRDS(fldRecurArray, file=paste0(ncpath, 'data_out2.rds'))
	}
	nc_close(ncin_gfdl)
	nc_close(ncin_hadgem)
	nc_close(ncin_ipsl)
	nc_close(ncin_miroc)
}


#names(dim(fldRecurArray)) = c('lon', 'lat', 'decade', 'recurInterval', 'rcpScen')
#ArrayToNc(list(fldRecurArray = fldRecurArray, lon = nc_lon, lat = nc_lat, decade = seq(10,90,10), recurInterval = recurIntrvls), file_path = 'ex.nc')


fldRecurIntrvl = fldRecurArray
metadata = list(fldRecurIntrvl = list(units = 'recurrence interval ( / yr)'))
attr(fldRecurIntrvl, 'variables') = metadata
names(dim(fldRecurIntrvl)) = c('lon', 'lat', 'decade', 'recurInterval', 'valueTypes', 'rcpScen')

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

recurInterval = recurIntrvls
dim(recurInterval) = length(recurInterval)
metadata = list(recurInterval = list(units = 'recurrence_intervals'))
attr(recurInterval, 'variables') = metadata
names(dim(recurInterval)) = 'recurInterval'

valueTypes = valueType
dim(valueTypes) = length(valueTypes)
metadata = list(valueTypes = list(units = 'interval_or_significance'))
attr(valueTypes, 'variables') = metadata
names(dim(valueTypes)) = 'valueTypes'

rcpScen = rcpScenarios
dim(rcpScen) = length(rcpScen)
metadata = list(rcpScen = list(units = 'RCP_scenario'))
attr(rcpScen, 'variables') = metadata
names(dim(rcpScen)) = 'rcpScen'


ArrayToNc(list(fldRecurIntrvl, lon, lat, decade, recurInterval, valueTypes, rcpScen), file_path = paste0(ncpath, 'floodRecurIntervals_v4.nc'))





########################################################################################################################
#	step 2: masking water bodies from flood depth database
# https://www.rdocumentation.org/packages/rnaturalearth/versions/0.1.0
library(rnaturalearth)
library(raster)
library(data.table)
library(sf)
sf_use_s2(FALSE) # for error suppression of Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  :
land10 <- ne_download(scale = 10, type = 'land', category = 'physical') #scal50 scale110
sp::plot(land10)
land10Sf = st_as_sf(land10)
	# lons and lats to match the flood rasters

#floodRast = raster('J:\\Cai_data\\TCFD\\CurrentFloodHazard\\floodMapGL_rp100y.tif')
lats = rev(seq(-54.00824,83.2251,length.out=16468))
lons = seq(-166.8, 180, length.out=41616)
#spPoints = st_multipoint(rbind(c(lats[1], lons[1]), c(lats[100],lons[50])))
pointDf = data.frame('lon' = rep(lons, length(lats)),
	'lat' = rep(lats, each=length(lons)))
	# pointDf is too long to allocate to one SF object, so splitting to 4
theseRows = 1:5000000 #685332288
pointSf = st_as_sf(pointDf[theseRows,], coords = c('lon', 'lat'))
st_crs(pointSf) = st_crs(land10)
goodPoints = data.frame(row1 = rep(NA, nrow(pointSf)))
goodPoints[,1]  = st_intersects(pointSf, land10Sf[1,], sparse=FALSE)[,1]
goodPoints[,2] = st_intersects(pointSf, land10Sf[2,], sparse=FALSE)[,1]
goodPoints[,3] = st_intersects(pointSf, land10Sf[3,], sparse=FALSE)[,1]
goodPoints[,4] = st_intersects(pointSf, land10Sf[4,], sparse=FALSE)[,1]
goodPoints[,5] = st_intersects(pointSf, land10Sf[5,], sparse=FALSE)[,1]
goodPoints[,6] = st_intersects(pointSf, land10Sf[6,], sparse=FALSE)[,1]
goodPoints[,7] = st_intersects(pointSf, land10Sf[7,], sparse=FALSE)[,1]
goodPoints[,8] = st_intersects(pointSf, land10Sf[9,], sparse=FALSE)[,1]
goodPoints[,9] = st_intersects(pointSf, land10Sf[10,], sparse=FALSE)[,1]
goodPoints[,10] = st_intersects(pointSf, land10Sf[11,], sparse=FALSE)[,1]
goodPoints[,11] = apply(goodPoints, 1, any)
goodRows = theseRows[goodPoints[,11]]

iter = 1
write.csv(goodRows, paste0('goodRows', iter, '.csv'))
#sp::plot(goodPointsSf)
	# repeating for all news rows
theseRows = theseRows + 5000000 #685332288
maxRow = 685332288
while(theseRows[1] <= 685332288)	{
	if(last(theseRows) > maxRow)	{
		theseRows = seq(theseRows[1], maxRow, 1)
	}
	print(theseRows[1] / maxRow)
	pointSf = st_as_sf(pointDf[theseRows,], coords = c('lon', 'lat'))
	st_crs(pointSf) = st_crs(land10)
	goodPoints = data.frame(row1 = rep(NA, nrow(pointSf)))
	goodPoints[,1]  = st_intersects(pointSf, land10Sf[1,], sparse=FALSE)[,1]
	goodPoints[,2] = st_intersects(pointSf, land10Sf[2,], sparse=FALSE)[,1]
	goodPoints[,3] = st_intersects(pointSf, land10Sf[3,], sparse=FALSE)[,1]
	goodPoints[,4] = st_intersects(pointSf, land10Sf[4,], sparse=FALSE)[,1]
	goodPoints[,5] = st_intersects(pointSf, land10Sf[5,], sparse=FALSE)[,1]
	goodPoints[,6] = st_intersects(pointSf, land10Sf[6,], sparse=FALSE)[,1]
	goodPoints[,7] = st_intersects(pointSf, land10Sf[7,], sparse=FALSE)[,1]
	goodPoints[,8] = st_intersects(pointSf, land10Sf[9,], sparse=FALSE)[,1]
	goodPoints[,9] = st_intersects(pointSf, land10Sf[10,], sparse=FALSE)[,1]
	goodPoints[,10] = st_intersects(pointSf, land10Sf[11,], sparse=FALSE)[,1]
	goodPoints[,11] = apply(goodPoints, 1, any)
	goodRows = theseRows[goodPoints[,11]]
	#goodRows = c(goodRows, theseRows[goodPoints[,11]])
	#write.table(goodRows, 'goodRows2.txt')
	#saveRDS(goodRows, 'goodRows2.rds')
	#gg=readRDS('goodRows2.rds')
	iter = iter + 1
	write.csv(goodRows, paste0('goodRows', iter, '.csv'))

	theseRows = theseRows + 5000000 #685332288
	#sp::plot(goodPointsDf)
}


dataOutArray = array(rep(0, length(lons) * length(lats)), 
	dim = c(length(lons), length(lats)))

for(i in 1:138)	{
	theseGoodRows = read.csv(paste0('goodRows', i, '.csv'))$x
	print(theseGoodRows[1])
	dataOutArray[theseGoodRows] = 1
}
	
saveRDS(dataOutArray, 'J:\\Cai_data\\TCFD\\CurrentFloodHazard\\LandMaskArray.rds')








########################################################################################################################
# step 3 intersecting recurrence intervals with flood depth
library(raster)
library(ncdf4)
library(data.table)
customerName = 'Corbion' #'Richs Foods'
locationFootprint = 2		# how big is the footprint of the location of interest? in number of 'boxes' to search to the left and right (so 0 is equal to 1 km^2, 1 is 9 km^2, 2 is 25 km^2, etc.
dataOutputLoc = 'J:\\Cai_data\\TCFD\\CustomerOutputs\\'
waterMaskLoc = 'J:\\Cai_data\\TCFD\\CurrentFloodHazard\\LandMaskArray.rds'



	# reading in table of customer locations
customerPath = 'J:\\Cai_data\\TCFD\\locations\\'
customerCsv = 'Corbion_Locations_Nov2022 - Sheet1.csv' #'Richs-sample-locations.csv'
customerTable = fread(paste0(customerPath, customerCsv))

	# defining rcp scenarios, recurrence intervals, and decades of interest; this should not change for the foreseeable future
rcpScenarios = c(26, 60)
recurIntrvls = c(10, 20, 50, 100, 200, 500)
whichDecades = seq(10,90,10)

	
	# read in flood recurrence data
ncpath = "J:\\Cai_data\\TCFD\\Flash Floods\\"
fldRcrIntNC = nc_open(paste0(ncpath, 'floodRecurIntervals_v2.nc'))
nc_lat = ncvar_get(fldRcrIntNC, 'lat')
nc_lon = ncvar_get(fldRcrIntNC, 'lon')
fldRcrVals = ncvar_get(fldRcrIntNC, 'fldRecurIntrvl')


	# initializing data output
dataOutput = data.frame(User = NA, Location = NA, Region = NA, Subregion = NA, Lat = NA, Lon = NA,
	Hazard = NA, Hazard_Measure = NA, Decade = NA, Scenario = NA,
	Raw_Hazard_Value = NA, Percentile_Score = NA, Relative_Hazard_Score = NA, Decadal_Trend_Strength = NA, Decadal_Trend_Significance = NA, Long_Term_Trend_Strength = NA, Long_Term_Trend_Significance = NA)

	# read in historic floods data
fileLoc = 'J:\\Cai_data\\TCFD\\CurrentFloodHazard'
fldDepthList = list()
fldDepthList[[1]] = raster(paste0(fileLoc, '\\floodMapGL_rp10y.tif'))
fldDepthList[[2]] = raster(paste0(fileLoc, '\\floodMapGL_rp20y.tif'))
fldDepthList[[3]] = raster(paste0(fileLoc, '\\floodMapGL_rp50y.tif'))
fldDepthList[[4]] = raster(paste0(fileLoc, '\\floodMapGL_rp100y.tif'))
fldDepthList[[5]] = raster(paste0(fileLoc, '\\floodMapGL_rp200y.tif'))
fldDepthList[[6]] = raster(paste0(fileLoc, '\\floodMapGL_rp500y.tif'))


	# identifying lat lon coordinates for tiffs
tif_lat = rev(seq(-54.00824,83.2251,length.out=16468))
tif_lon = seq(-166.8, 180, length.out=41616)


for(thisIntrvl in 1:length(recurIntrvls))	{
	hazardDepthName = paste0("Flood Hazard (m) ", recurIntrvls[thisIntrvl], 'yr Flood')
	hazardLikliName = paste0('Flood Hazard (-) ', recurIntrvls[thisIntrvl], 'yr Flood')
	for(j in 1:nrow(customerTable))	{
		closeTiffLons = which.min(abs(customerTable$Lon[j] - tif_lon))
		closeTiffLats = which.min(abs(customerTable$Lat[j] - tif_lat))
		
		theseLats = seq(closeTiffLats - locationFootprint, closeTiffLats + locationFootprint, 1)
		theseLons = seq(closeTiffLons - locationFootprint, closeTiffLons + locationFootprint, 1)
			#ensuring lats / lons don't go outside bounding box
		if(any(theseLats < 1))	{theseLats[theseLats < 1] = 1}
		if(any(theseLats > length(tif_lat)))	{theseLats[theseLats > length(tif_lat)] = length(tif_lat)}
		if(any(theseLons < 1))	{theseLons[theseLons < 1] = 1}
		if(any(theseLons > length(tif_lon)))	{theseLons[theseLons > length(tif_lon)] = length(tif_lon)}

			# defining the water mask
		thisWaterMask = readRDS(waterMaskLoc)[theseLons, theseLats]

		histFloodDepth = mean(fldDepthList[[6]][theseLats, theseLons], na.rm=TRUE)
		histFloodLikli = length(which(fldDepthList[[6]][theseLats, theseLons] > 0)) / length(theseLats)^2
		
			# check to see if even 500yr floods trigger historically; if not, the skip next analysis
		if(!is.na(histFloodDepth))	{
						# ensuring we are not drawing from a water tile, then searching box around point if so
			closeNCLon = which.min(abs(customerTable$Lon[j] - nc_lon))
			closeNCLat = which.min(abs(customerTable$Lat[j] - nc_lat))
			if(is.na(fldRcrVals[closeNCLon, closeNCLat, 1, thisIntrvl, 1]))	{
					closeNCLon = closeNCLon + 1
					if(is.na(fldRcrVals[closeNCLon, closeNCLat, 1, thisIntrvl, 1]))	{
						closeNCLon = closeNCLon - 2
						if(is.na(fldRcrVals[closeNCLon, closeNCLat, 1, thisIntrvl, 1]))	{
							closeNCLon = closeNCLon + 1
							closeNCLat = closeNCLat + 1
							if(is.na(fldRcrVals[closeNCLon, closeNCLat, 1, thisIntrvl, 1]))	{
								closeNCLat = closeNCLat - 2
			}}}}
				
			for(thisScenario in 1:length(rcpScenarios))	{
				FldRcrSignif_all = fldRcrVals[closeNCLon, closeNCLat, 9,  thisIntrvl, thisScenario]
				
				for(thisDecade in 1:length(whichDecades))	{
					thisFldRcrVal = 		fldRcrVals[closeNCLon, closeNCLat, thisDecade, 1, thisIntrvl, thisScenario]
					thisFldRcrSignif = 		fldRcrVals[closeNCLon, closeNCLat, thisDecade, 2, thisIntrvl, thisScenario]
					
					if(thisFldRcrVal >= recurIntrvls[1])	{
						closestFldRcrIntrvl = 	last(which(thisFldRcrVal > recurIntrvls))
						closestFldRcrIntrvlAll =last(which(FldRcrSignif_all > recurIntrvls))
						
						theseFloodImpacts = fldDepthList[[closestFldRcrIntrvl]][theseLats, theseLons]
						theseFloodImpacts[is.na(theseFloodImpacts)] = 0
						theseFloodImpacts = theseFloodImpacts * thisWaterMask
	
						theseFloodImpacts = fldDepthList[[closestFldRcrIntrvl]][theseLats, theseLons]
						theseFloodImpacts[is.na(theseFloodImpacts)] = 0
						theseFloodImpacts = theseFloodImpacts * thisWaterMask
	
						
						if(any(!is.na(theseFloodImpacts)))	{
							histFloodDepth = mean(theseFloodImpacts, na.rm=TRUE),
							histFloodDepthChng = histFloodDepth - 

							histFloodLikli = length(which(theseFloodImpacts > 0)) / length(!is.na(theseFloodImpacts)))
							histFloodLikliChng = 
						

							dataOutput = rbind(dataOutput,
								c(customerName,
								customerTable$Name[j],
								customerTable$Region[j],
								customerTable$Subregion[j],
								customerTable$Lat[j],
								customerTable$Lon[j],
								hazardDepthName,
								paste0('20', whichDecades[thisDecade], 's'),
								paste0('RCP', rcpScenarios[thisScenario]),
								histFloodDepth,
								,
								thisFldRcrSignif,
								,
								FldRcrSignif_all))

							dataOutput = rbind(dataOutput,
								c(customerName,
								customerTable$Name[j],
								customerTable$Region[j],
								customerTable$Subregion[j],
								customerTable$Lat[j],
								customerTable$Lon[j],
								hazardLikliName,
								paste0('20', whichDecades[thisDecade], 's'),
								paste0('RCP', rcpScenarios[thisScenario]),
								histFloodLikli,
								,
								thisFldRcrSignif,
								,
								FldRcrSignif_all))

					
						} else	{ 
							dataOutput = rbind(dataOutput,
								c(customerName,
								customerTable$Name[j],
								customerTable$Region[j],
								customerTable$Subregion[j],
								customerTable$Lat[j],
								customerTable$Lon[j],
								hazardName,
								paste0('20', whichDecades[thisDecade], 's'),
								paste0('RCP', rcpScenarios[thisScenario]),
								0,
								,
								thisFldRcrSignif,
								,
								FldRcrSignif_all))
						}
					}	else	{
						dataOutput = rbind(dataOutput,
							c(customerName,
							customerTable$Name[j],
							customerTable$Region[j],
							customerTable$Subregion[j],
							customerTable$Lat[j],
							customerTable$Lon[j],
							hazardName,
							paste0('20', whichDecades[thisDecade], 's'),
							paste0('RCP', rcpScenarios[thisScenario]),
							0))
					}
				}
			}
			
			
		} else {
			dataOutput = rbind(dataOutput,
				data.frame(User = customerName,
				Location = customerTable$Name[j],
				Region = customerTable$Region[j],
				Subregion = customerTable$Subregion[j],
				Lat = customerTable$Lat[j],
				Lon = customerTable$Lon[j],
				Hazard = hazardName,
				Decade = rep(paste0('20', whichDecades, 's'), length(rcpScenarios)),
				Scenario = rep(paste0('RCP', rcpScenarios), each=length(whichDecades)),
				Raw_Hazard_Value = 0))
			
		}
	}
}	

dataOutput$Raw_Hazard_Value = as.numeric(dataOutput$Raw_Hazard_Value)
dataOutput = dataOutput[-1,]


	# defining relativ flood hazard
if(chooseDepth)	{
	basSeq = seq(0.1, 1, length.out=67)^4
	relFloodHazard = c(rep(0, 33), basSeq*30)
	#relFloodHazard = c(rep(0, 33), seq(0.01,10,length.out=(67)))
	dataOutput$Percentile = 1
	for(ll in 2:length(relFloodHazard))	{
		dataOutput$Percentile[which(dataOutput$Raw_Hazard_Value > relFloodHazard[ll])] = ll
	}
} else {
	dataOutput$Percentile = round(dataOutput$Raw_Hazard_Value * 100, 0)
}
dataOutput$Relative_Hazard_Score = 'Low'
dataOutput$Relative_Hazard_Score[dataOutput$Percentile > 33] = 'Medium'
dataOutput$Relative_Hazard_Score[dataOutput$Percentile > 76] = 'High'

fileName = ifelse(chooseDepth, paste0(customerName, '_', Sys.Date(), '_avgDepth_Footprint_', (locationFootprint*2+1), 'km'), paste0(customerName, '_', Sys.Date(), '_relLikelihood_Footprint_', (locationFootprint*2+1), 'km'))
fwrite(dataOutput, paste0(dataOutputLoc, fileName, '.csv'))





















rejiggerData = all_data[c(1:200,1090000:1090200), c('User', 'Region', 'Subregion', 'Lat', 'Lon', 'Hazard', 'Decade', 'Scenario', 'Raw_Hazard_Value', 'Percentile', 'Relative_Hazard_Score')]
fwrite(rejiggerData, paste0(ncpath, ncVarFileName, "_", "sampleData_", saveDate,  ".csv"))

	

	closeLonLats = NULL
	theseLabels = NULL
	theseRegions = NULL
	theseSubregions = NULL
	for(k in 1:length(closeLons))	{
		newCloseLonLats = which(hazardMaps$Lat == closeLats[k] & hazardMaps$Lon == closeLons[k])
		closeLonLats = c(closeLonLats, newCloseLonLats)
		theseLabels = c(theseLabels, rep(customerTable$Name[k], length(newCloseLonLats)))
		theseRegions = c(theseRegions, rep(customerTable$Region[k], length(newCloseLonLats)))
		theseSubregions = c(theseSubregions, rep(customerTable$Subregion[k], length(newCloseLonLats)))
	}












dataOutArray[ , , , , 2][dataOutArray[ , , , , 2] != missing_data

all_data$Percentile = 1
for(i in 2:length(histQuants))	{
	all_data$Percentile[which(all_data$Raw_Hazard_Value > histQuants[i])] = i
}
	# converting fraction to percent, and rounding for Looker
all_data$Raw_Hazard_Value = round(all_data$Raw_Hazard_Value * 100, 6)

	# identifying relative hazard values with numbers for plotting
all_data$Relative_Hazard_Score = 1
all_data$Relative_Hazard_Score[all_data$Percentile > 33] = 2
all_data$Relative_Hazard_Score[all_data$Percentile > 67] = 3










		# array for holding outputs
#	fldRecurArray = array(rep(-(10^10), length(tif_lat) * length(tif_lon) * length(whichDecades) * length(recurIntrvls) * length(rcpScenarios)), 
#		dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(recurIntrvls), length(rcpScenarios)))







ncpath = "J:\\Cai_data\\TCFD\\Flash Floods\\"
ncVarFileName = 'fldRecurIntrvl'
saveDate = '10OCT2022'
rcpScenarios = c(26, 60)
recurIntrvls = c(10, 20, 50, 100, 200, 500)
whichDecades = seq(10,90,10)
startHist = as.Date('1920-01-01')

	# reading in dummy data for lat lons
ncname_dummy = paste0('clm45_gfdl-esm2m_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_dummy = nc_open(paste0(ncpath, ncname_dummy))
nc_lat = ncvar_get(ncin_dummy, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_dummy, 'lon')


	# array for holding outputs
fldRecurArray = array(rep(-(10^10), length(nc_lon) * length(nc_lat) * length(whichDecades) * length(recurIntrvls) * length(rcpScenarios)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(recurIntrvls), length(rcpScenarios)))









########################################################################################################################
# step 2: interfacing recurrence intervals with flood depth
library(rnaturalearth)
library(sf)
library(raster)

ncpath = "J:\\Cai_data\\TCFD\\Flash Floods\\"
fldRcrInt = nc_open(paste0(ncpath, 'floodRecurIntervals.nc'))


sf::st_crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
+proj=longlat +datum=WGS84 +no_defs


str_name = 'floodMapGL_rp100y.tif' 

for(thisIntrvl in recurIntrvls)	{
	str_name = paste0('floodMapGL_rp', thisIntrvl, 'y.tif')
	fileLoc = 'J:\\Cai_data\\TCFD\\CurrentFloodHazard'
	imported_raster=raster(paste0(fileLoc, '\\', str_name))
	plot(imported_raster, col='red')














#########################################
# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\Flash Floods\\"
	# reading ssp126 climate data 
ncname = "clm45_gfdl-esm2m_ewembi_historical_2005soc_co2_maxdis_global_monthly_1861_2005.nc4"  
ncfname = paste0(ncpath, ncname)
ncin = nc_open(ncfname)
nc_lat = rev(ncvar_get(ncin, 'lat'))	# lat is given high to low
nc_lon = ncvar_get(ncin, 'lon')
nc_date = as.Date("1661-01-01") + ncvar_get(ncin, 'time') * 30.4375# time is days after jan 1 2015
nc_years = unique(year(nc_date))
nc_maxdis_all = ncvar_get(ncin,"maxdis")

ncname_f = "clm45_gfdl-esm2m_ewembi_rcp26_2005soc_co2_maxdis_global_monthly_2006_2099.nc4"#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_maxdis_global_monthly_2006_2099.nc4"  
ncfname_f = paste0(ncpath, ncname_f)
ncin_f = nc_open(ncfname_f)
nc_lat_f = ncvar_get(ncin_f, 'lat')
nc_lon_f = ncvar_get(ncin_f, 'lon')
nc_date_f = as.Date("1661-01-01") + ncvar_get(ncin_f, 'time') * 30.4375# time is days after jan 1 2015
nc_years_f = unique(year(nc_date_f))
nc_maxdis_all_f = ncvar_get(ncin_f,"maxdis")	# in lon,lat,time


ftrFldIntrv_df_5_2029 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_10_2029 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_20_2029 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_30_2029 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_50_2029 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_100_2029 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))

ftrFldIntrv_df_5_3039 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_10_3039 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_20_3039 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_30_3039 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_50_3039 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_100_3039 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))

ftrFldIntrv_df_5_4049 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_10_4049 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_20_4049 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_30_4049 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_50_4049 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_100_4049 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))

ftrFldIntrv_df_5_5059 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_10_5059 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_20_5059 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_30_5059 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_50_5059 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_100_5059 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))

ftrFldIntrv_df_5_6069 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_10_6069 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_20_6069 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_30_6069 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_50_6069 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))
ftrFldIntrv_df_100_6069 = data.frame(matrix(NA, nrow=length(nc_lat), ncol=length(nc_lon)))

old_dates_past = which(year(nc_date) == 1920)[1]:length(nc_date)
old_dates_future = 1:which(year(nc_date_f) == 2020)[12]
future_dates2029 = which(year(nc_date_f) == 2010)[1]:which(year(nc_date_f) == 2039)[12]
future_dates3039 = which(year(nc_date_f) == 2020)[1]:which(year(nc_date_f) == 2049)[12]
future_dates4049 = which(year(nc_date_f) == 2030)[1]:which(year(nc_date_f) == 2059)[12]
future_dates5059 = which(year(nc_date_f) == 2040)[1]:which(year(nc_date_f) == 2069)[12]
future_dates6069 = which(year(nc_date_f) == 2050)[1]:length(nc_date_f)

returnFreqs = c(5,10,20,30,50,100)

for(i in 1:length(nc_lat))	{
	for(j in 1:length(nc_lon))	{
		nc_maxdis = c(nc_maxdis_all[j,i,old_dates_past], nc_maxdis_all_f[j,i,old_dates_future])	# in lon,lat,time
		if(any(!is.na(nc_maxdis)))	{
			nc_maxdis_f_2029 = nc_maxdis_all_f[j,i,future_dates2029] #ncvar_get(ncin_f,"maxdis")[j,i,]	# in lon,lat,time
			nc_maxdis_f_3039 = nc_maxdis_all_f[j,i,future_dates3039] #ncvar_get(ncin_f,"maxdis")[j,i,]	# in lon,lat,time
			nc_maxdis_f_4049 = nc_maxdis_all_f[j,i,future_dates4049] #ncvar_get(ncin_f,"maxdis")[j,i,]	# in lon,lat,time
			nc_maxdis_f_5059 = nc_maxdis_all_f[j,i,future_dates5059] #ncvar_get(ncin_f,"maxdis")[j,i,]	# in lon,lat,time
			nc_maxdis_f_6069 = nc_maxdis_all_f[j,i,future_dates6069] #ncvar_get(ncin_f,"maxdis")[j,i,]	# in lon,lat,time			
			if(any(nc_maxdis > 0) & any(nc_maxdis_f_2029 > 0) & any(nc_maxdis_f_3039) & any(nc_maxdis_f_4049) & any(nc_maxdis_f_5059) & any(nc_maxdis_f_6069))	{
					
				print(c(i,j))

				randNoise = seq(0.1^3,0.1^2,length.out = 100)
				nc_maxdis_y = NULL
				for(kh in 1:(length(nc_maxdis)/12))	{
					nc_maxdis_y = c(nc_maxdis_y, max(nc_maxdis[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_maxdis_y_f_2029 = NULL
				for(kh in 1:(length(nc_maxdis_f_2029)/12))	{
					nc_maxdis_y_f_2029 = c(nc_maxdis_y_f_2029, max(nc_maxdis_f_2029[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_maxdis_y_f_3039 = NULL
				for(kh in 1:(length(nc_maxdis_f_3039)/12))	{
					nc_maxdis_y_f_3039 = c(nc_maxdis_y_f_3039, max(nc_maxdis_f_3039[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_maxdis_y_f_4049 = NULL
				for(kh in 1:(length(nc_maxdis_f_4049)/12))	{
					nc_maxdis_y_f_4049 = c(nc_maxdis_y_f_4049, max(nc_maxdis_f_4049[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_maxdis_y_f_5059 = NULL
				for(kh in 1:(length(nc_maxdis_f_5059)/12))	{
					nc_maxdis_y_f_5059 = c(nc_maxdis_y_f_5059, max(nc_maxdis_f_5059[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				nc_maxdis_y_f_6069 = NULL
				for(kh in 1:(length(nc_maxdis_f_6069)/12))	{
					nc_maxdis_y_f_6069 = c(nc_maxdis_y_f_6069, max(nc_maxdis_f_6069[(1:12)+(12*(kh-1))])+sample(randNoise,1))
				}

				ftrFldIntrv_df_5_2029[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_2029, probFld = 5)
				ftrFldIntrv_df_5_3039[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_3039, probFld = 5)
				ftrFldIntrv_df_5_4049[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_4049, probFld = 5)
				ftrFldIntrv_df_5_5059[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_5059, probFld = 5)
				ftrFldIntrv_df_5_6069[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_6069, probFld = 5)
				ftrFldIntrv_df_10_2029[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_2029, probFld = 10)
				ftrFldIntrv_df_10_3039[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_3039, probFld = 10)
				ftrFldIntrv_df_10_4049[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_4049, probFld = 10)
				ftrFldIntrv_df_10_5059[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_5059, probFld = 10)
				ftrFldIntrv_df_10_6069[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_6069, probFld = 10)
				ftrFldIntrv_df_20_2029[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_2029, probFld = 20)
				ftrFldIntrv_df_20_3039[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_3039, probFld = 20)
				ftrFldIntrv_df_20_4049[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_4049, probFld = 20)
				ftrFldIntrv_df_20_5059[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_5059, probFld = 20)
				ftrFldIntrv_df_20_6069[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_6069, probFld = 20)
				ftrFldIntrv_df_30_2029[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_2029, probFld = 30)
				ftrFldIntrv_df_30_3039[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_3039, probFld = 30)
				ftrFldIntrv_df_30_4049[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_4049, probFld = 30)
				ftrFldIntrv_df_30_5059[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_5059, probFld = 30)
				ftrFldIntrv_df_30_6069[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_6069, probFld = 30)
				ftrFldIntrv_df_50_2029[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_2029, probFld = 50)
				ftrFldIntrv_df_50_3039[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_3039, probFld = 50)
				ftrFldIntrv_df_50_4049[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_4049, probFld = 50)
				ftrFldIntrv_df_50_5059[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_5059, probFld = 50)
				ftrFldIntrv_df_50_6069[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_6069, probFld = 50)
				ftrFldIntrv_df_100_2029[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_2029, probFld = 100)
				ftrFldIntrv_df_100_3039[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_3039, probFld = 100)
				ftrFldIntrv_df_100_4049[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_4049, probFld = 100)
				ftrFldIntrv_df_100_5059[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_5059, probFld = 100)
				ftrFldIntrv_df_100_6069[i,j] = newFldFrqFunc(oldQ = nc_maxdis_y, newQ = nc_maxdis_y_f_6069, probFld = 100)
			}
		} 
	}
#	print(ftrFldIntrv_df)
}
write.csv(ftrFldIntrv_df_5_2029, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_5yrflod_CLM_2029.csv")
write.csv(ftrFldIntrv_df_5_3039, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_5yrflod_CLM_3039.csv")
write.csv(ftrFldIntrv_df_5_4049, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_5yrflod_CLM_4049.csv")
write.csv(ftrFldIntrv_df_5_5059, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_5yrflod_CLM_5059.csv")
write.csv(ftrFldIntrv_df_5_6069, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_5yrflod_CLM_6069.csv")

write.csv(ftrFldIntrv_df_10_2029, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_10yrflod_CLM_2029.csv")
write.csv(ftrFldIntrv_df_10_3039, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_10yrflod_CLM_3039.csv")
write.csv(ftrFldIntrv_df_10_4049, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_10yrflod_CLM_4049.csv")
write.csv(ftrFldIntrv_df_10_5059, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_10yrflod_CLM_5059.csv")
write.csv(ftrFldIntrv_df_10_6069, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_10yrflod_CLM_6069.csv")

write.csv(ftrFldIntrv_df_20_2029, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_20yrflod_CLM_2029.csv")
write.csv(ftrFldIntrv_df_20_3039, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_20yrflod_CLM_3039.csv")
write.csv(ftrFldIntrv_df_20_4049, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_20yrflod_CLM_4049.csv")
write.csv(ftrFldIntrv_df_20_5059, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_20yrflod_CLM_5059.csv")
write.csv(ftrFldIntrv_df_20_6069, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_20yrflod_CLM_6069.csv")

write.csv(ftrFldIntrv_df_30_2029, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_30yrflod_CLM_2029.csv")
write.csv(ftrFldIntrv_df_30_3039, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_30yrflod_CLM_3039.csv")
write.csv(ftrFldIntrv_df_30_4049, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_30yrflod_CLM_4049.csv")
write.csv(ftrFldIntrv_df_30_5059, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_30yrflod_CLM_5059.csv")
write.csv(ftrFldIntrv_df_30_6069, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_30yrflod_CLM_6069.csv")

write.csv(ftrFldIntrv_df_50_2029, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_50yrflod_CLM_2029.csv")
write.csv(ftrFldIntrv_df_50_3039, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_50yrflod_CLM_3039.csv")
write.csv(ftrFldIntrv_df_50_4049, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_50yrflod_CLM_4049.csv")
write.csv(ftrFldIntrv_df_50_5059, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_50yrflod_CLM_5059.csv")
write.csv(ftrFldIntrv_df_50_6069, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_50yrflod_CLM_6069.csv")

write.csv(ftrFldIntrv_df_100_2029, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_100yrflod_CLM_2029.csv")
write.csv(ftrFldIntrv_df_100_3039, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_100yrflod_CLM_3039.csv")
write.csv(ftrFldIntrv_df_100_4049, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_100yrflod_CLM_4049.csv")
write.csv(ftrFldIntrv_df_100_5059, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_100yrflod_CLM_5059.csv")
write.csv(ftrFldIntrv_df_100_6069, "J:\\Cai_data\\TCFD\\Flash Floods\\rcp26_100yrflod_CLM_6069.csv")



#ftrFldIntrv_df = as.data.frame(read.csv("J:\\Cai_data\\TCFD\\Flash Floods\\test_out.csv"))[,-1]


library(colorRamps)
col5 <- colorRampPalette(c('blue4', 'gray96', 'yellow4'))  #create color ramp starting from blue to red

par(mar=c(2.1,2.1,2.1,1))
image(nc_lon,rev(nc_lat),as.matrix(ftrFldIntrv_df[,ncol(ftrFldIntrv_df):1]), ylim = c(-57,85),
	col=col5(n=11), breaks = seq(0,40,length.out=12),
	ylab='Lat', xlab = 'lon', main = "Changes in flood frequency in 21st C")
data(wrld_simpl)
plot(wrld_simpl,add=TRUE)

if (any(is.na(as.matrix(ftrFldIntrv_df[,ncol(ftrFldIntrv_df):1])))) {
    mmat <- ifelse(is.na(as.matrix(ftrFldIntrv_df[,ncol(ftrFldIntrv_df):1])), 1, NA)
    image(mmat, axes = FALSE, xlab = "", ylab = "", col = 'white', useRaster=TRUE, add = TRUE)
  }

ftrFldIntrv_mt = as.matrix(unlist(ftrFldIntrv_df))

pt1
ftrFldIntrv_sf = st_as_sf(as.vector(ftrFldIntrv_df), coords = 

















ftrFldIntrv_df[is.na(ftrFldIntrv_df)] = 0

# converting df into array 
lon2 = nc_lon	; lat2 = nc_lat
nlon2 = length(nc_lon)	; nlat2 = length(nc_lat)
#time2 = time; tunits2 = tunits	; nt2 = nt

# convert to 2d array
ftrFldIntrv_ar <- array(ftrFldIntrv_df, dim=c(nlon2,nlat2))
dim(ftrFldIntrv_ar)

# plot to check creation of arrays
library(lattice)
library(RColorBrewer)

grid <- expand.grid(lon=nc_lon, lat=nc_lat)

levelplot(ftrFldIntrv_ar ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
  col.regions=(rev(brewer.pal(10,"RdBu"))), main="MTWA (C)")



	#names of path, file, and dname
ncpath = "J:\\Cai_data\\TCFD\\Flash Floods\\test_out.csv"
ncname = "GLM_ftrFlood"
ncfname = paste(ncpath, ncname, '.nc', sep='')
dname = 'fldInt'


	#define dimensions
londim = ncdim_def('lon','degrees',as.double(nc_lon))
latdim = ncdim_def('lat','degrees',as.double(nc_lat))
#timedim <- ncdim_def("time",tunits3,as.double(time3))

# define variables
fillvalue = NA
dlname = "Flood Interval in Years"
fldInt_def = ncvar_def("fldInt","yrs",list(londim,latdim),fillvalue,dlname,prec="single")
#dlname = "mean_temperture_warmest_month"
#mtwa.def <- ncvar_def("mtwa","deg_C",list(londim,latdim,timedim),fillvalue,dlname,prec="single")


# create netCDF file and put arrays
#ncout <- nc_create(ncfname,list(tmp_def,mtco.def,mtwa.def,mat.def),force_v4=TRUE)
ncout <- nc_create(ncfname,list(fldInt_def),force_v4=FALSE)

# put variables
#ncvar_put(ncout,tmp_def,tmp_array3)
#ncvar_put(ncout,mtwa.def,mtwa_array3)
ncvar_put(ncout,fldInt_def,ftrFldIntrv_df)
ncvar_put(ncout,mat.def,mat_array3)

# put additional attributes into dimension and data variables
ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncout,"lat","axis","Y")
ncatt_put(ncout,"time","axis","T")

# add global attributes
ncatt_put(ncout,0,"title",title$value)
ncatt_put(ncout,0,"institution",institution$value)
ncatt_put(ncout,0,"source",datasource$value)
ncatt_put(ncout,0,"references",references$value)
history <- paste("P.J. Bartlein", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"Conventions",Conventions$value)

# Get a summary of the created file:
ncout





dim1 <- dim.def.ncdf( name = "lat", units = "degrees", vals = as.double(nc_lat) )
dim2 <- dim.def.ncdf( name = "lon", units = "degrees", vals = as.double(nc_lon) )
ftrFldIntrv_mt = as.matrix(ftrFldIntrv_df)

# define the EMPTY (elevation) netcdf variable

varz <- var.def.ncdf(name = "floodFreq", units =  "years", 
                    dim = list(dim1, dim2), missval = NA, 
                    longname ="Flood Frequency of what was a 20 year flood")		

nc.ex <- create.ncdf( filename = "J:\\Cai_data\\TCFD\\Flash Floods\\test_out.nc", vars = varz )
put.var.ncdf(nc = nc.ex, varid = varz, vals = ftrFldIntrv_mt)




ggplot(aes(x=X1, y=X2, fill=value), data=mpr) + geom_raster() 
		
myClrs =  colorRamp(c("red", "blue"))
plot(x=nc_lon, y=nc_lat, 



lon1 <- ncdim_def( "Lon", "degrees_east", unique(DB$lon))
lat1 <- ncdim_def( "Lat", "degrees_north", unique(DB$lat))
record <- ncdim_def( "record", "files", list_obs$record_uid,unlim=T)
mv <- -999 # missing value to use
tmpmean_ <- ncvar_def( "tmpmean_", "degrees", list(lon1,lat1,record),mv)

time<-list_obs$record_uid

ncnew <- nc_create( "tmpmean_.nc",tmpmean_)

for( i in 1:length(record))
  ncvar_put( ncnew, tmpmean_, DB$tmpmean_, start=c(1,1,i), 
             count=c(-1,-1,1))
		
		
		
		
		

cal_out = data.frame(sfcf=rep(NA,length(HBV_in_ls)))

#"https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=ORO&SensorNums=8&dur_code=D&Start=1900-01-01&End=2022-03-23"
for(thisClmMod in c(4,6:15))	{
	HBVoutCol = HBVoutCol + 1

#	ncvar_thismod_ppt =  ncvar_get(ncin_ppt_1, "tp")[ ,thisClmMod , , ]	# is [time,model,lon,lat] ugh annoying
	ncvar_thismod_ppt_1 = ncvar_get(ncin_ppt_1, "tp")[ ,thisClmMod , , ]	# is [time,model,lon,lat] and now merging two separate netcdfs
	ncvar_thismod_ppt_2 = ncvar_get(ncin_ppt_2, "tp")[date_range_ppt2,thisClmMod , , ]
	ncvar_thismod_tmin = ncvar_get(ncin, "t2m_min")[, , , thisClmMod]
	ncvar_thismod_tmax = ncvar_get(ncin, "t2m_max")[, , , thisClmMod]



