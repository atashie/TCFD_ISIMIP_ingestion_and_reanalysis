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
		# generating distribution using old / historic time series data
	volQ = oldQ
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

		# creating a new distribution with the future time series data data
	newVolQ = newQ
	newSortedvolQ = sort(newVolQ, decreasing = TRUE)
	newFit = samlmu(newVolQ)
	newPara3 = pelpe3(newFit)

		# identifying the return period of a 'probFld' from the historic data according to the distribution from the future data
	newLp3Accum = cdfpe3(thisProbFldVol, newPara3)
	newFittedTr3 = 1 / (1 - newLp3Accum)
	return(newFittedTr3)
}









#########################################
# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\Flash Floods\\"
ncVarFileName = 'maxdis'
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




for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]
	rcpScen = paste0('rcp', rcpScenNum)
	ncname_gfdl = paste0('clm45_gfdl-esm2m_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
	ncname_gfdl_hist = paste0('clm45_gfdl-esm2m_ewembi_historical_2005soc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_gfdl_hist = nc_open(paste0(ncpath, ncname_gfdl_hist))
	nc_date_hist = as.Date("1661-01-01") + ncvar_get(ncin_gfdl_hist, 'time') * 30.4375# time is months after 1661-1-1
	nc_keepHistDates = which(nc_date_hist > startHist)

	ncname_hadgem = paste0('clm45_hadgem2-es_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
	ncname_hadgem_hist = paste0('clm45_hadgem2-es_ewembi_historical_2005soc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_hadgem_hist = nc_open(paste0(ncpath, ncname_hadgem_hist))

	ncname_ipsl = paste0('clm45_ipsl-cm5a-lr_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
	ncname_ipsl_hist = paste0('clm45_ipsl-cm5a-lr_ewembi_historical_2005soc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_ipsl_hist = nc_open(paste0(ncpath, ncname_ipsl_hist))

	ncname_miroc = paste0('clm45_miroc5_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
	ncname_miroc_hist = paste0('clm45_miroc5_ewembi_historical_2005soc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_miroc_hist = nc_open(paste0(ncpath, ncname_miroc_hist))

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
		nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName)[, i, ]	# lon, lat, time
		nc_gfdl_hist = ncvar_get(ncin_gfdl_hist,ncVarFileName)[, i,nc_keepHistDates]	# lon, lat, time

		nc_hadgem = ncvar_get(ncin_hadgem,ncVarFileName)[, i, ]	# lon, lat, time
		nc_hadgem_hist = ncvar_get(ncin_hadgem_hist,ncVarFileName)[, i,nc_keepHistDates]	# lon, lat, time

		nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName)[, i, ]	# lon, lat, time
		nc_ipsl_hist = ncvar_get(ncin_ipsl_hist,ncVarFileName)[, i,nc_keepHistDates]	# lon, lat, time

		nc_miroc = ncvar_get(ncin_miroc,ncVarFileName)[, i, ]	# lon, lat, time
		nc_miroc_hist = ncvar_get(ncin_miroc_hist,ncVarFileName)[, i,nc_keepHistDates]	# lon, lat, time

		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j, dates10s] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				nc_hist = c(nc_gfdl_hist[j, ], nc_hadgem_hist[j, ], nc_ipsl_hist[j, ], nc_miroc_hist[j, ])
				nc_10s = c(nc_gfdl[j, dates10s], nc_hadgem[j, dates10s], nc_ipsl[j, dates10s], nc_miroc[j, dates10s]) 
				nc_20s = c(nc_gfdl[j, dates20s], nc_hadgem[j, dates20s], nc_ipsl[j, dates20s], nc_miroc[j, dates20s]) 
				nc_30s = c(nc_gfdl[j, dates30s], nc_hadgem[j, dates30s], nc_ipsl[j, dates30s], nc_miroc[j, dates30s]) 
				nc_40s = c(nc_gfdl[j, dates40s], nc_hadgem[j, dates40s], nc_ipsl[j, dates40s], nc_miroc[j, dates40s]) 
				nc_50s = c(nc_gfdl[j, dates50s], nc_hadgem[j, dates50s], nc_ipsl[j, dates50s], nc_miroc[j, dates50s]) 
				nc_60s = c(nc_gfdl[j, dates60s], nc_hadgem[j, dates60s], nc_ipsl[j, dates60s], nc_miroc[j, dates60s]) 
				nc_70s = c(nc_gfdl[j, dates70s], nc_hadgem[j, dates70s], nc_ipsl[j, dates70s], nc_miroc[j, dates70s]) 
				nc_80s = c(nc_gfdl[j, dates80s], nc_hadgem[j, dates80s], nc_ipsl[j, dates80s], nc_miroc[j, dates80s]) 
				nc_90s = c(nc_gfdl[j, dates90s], nc_hadgem[j, dates90s], nc_ipsl[j, dates90s], nc_miroc[j, dates90s]) 
				
		
				print(c(i,j))

				randNoise = seq(0.1^3,0.1^2,length.out = 100)
				nc_yrHist = NULL
				for(kh in 1:(length(nc_hist)/12))	{
					nc_yrHist = c(nc_yrHist, max(nc_hist[(1:12)+(12*(kh-1))]) + sample(randNoise,1))
				}

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
					fldRecurArray[j, i, 1, intrvl, thisScen] = newFldFrqFunc(oldQ = nc_yrHist, newQ = nc_yr10s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 2, intrvl, thisScen] = newFldFrqFunc(oldQ = nc_yrHist, newQ = nc_yr20s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 3, intrvl, thisScen] = newFldFrqFunc(oldQ = nc_yrHist, newQ = nc_yr30s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 4, intrvl, thisScen] = newFldFrqFunc(oldQ = nc_yrHist, newQ = nc_yr40s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 5, intrvl, thisScen] = newFldFrqFunc(oldQ = nc_yrHist, newQ = nc_yr50s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 6, intrvl, thisScen] = newFldFrqFunc(oldQ = nc_yrHist, newQ = nc_yr60s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 7, intrvl, thisScen] = newFldFrqFunc(oldQ = nc_yrHist, newQ = nc_yr70s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 8, intrvl, thisScen] = newFldFrqFunc(oldQ = nc_yrHist, newQ = nc_yr80s, probFld = recurIntrvls[intrvl])
					fldRecurArray[j, i, 9, intrvl, thisScen] = newFldFrqFunc(oldQ = nc_yrHist, newQ = nc_yr90s, probFld = recurIntrvls[intrvl])
				}
			}
		}
	saveRDS(fldRecurArray, file=paste0(ncpath, 'data_out.rds'))
	}
	nc_close(ncin_gfdl)
	nc_close(ncin_hadgem)
	nc_close(ncin_ipsl)
	nc_close(ncin_miroc)
	nc_close(ncin_gfdl_hist)
	nc_close(ncin_hadgem_hist)
	nc_close(ncin_ipsl_hist)
	nc_close(ncin_miroc_hist)
}

#fldRecurArray = readRDS(file=paste0(ncpath, 'data_out.rds'))
#names(dim(fldRecurArray)) = c('lon', 'lat', 'decade', 'recurInterval', 'rcpScen')
#ArrayToNc(list(fldRecurArray = fldRecurArray, lon = nc_lon, lat = nc_lat, decade = seq(10,90,10), recurInterval = recurIntrvls), file_path = 'ex.nc')


fldRecurIntrvl = fldRecurArray
metadata = list(fldRecurIntrvl = list(units = 'recurrence interval ( / yr)'))
attr(fldRecurIntrvl, 'variables') = metadata
names(dim(fldRecurIntrvl)) = c('lon', 'lat', 'decade', 'recurInterval','rcpScen')

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

rcpScen = rcpScenarios
dim(rcpScen) = length(rcpScen)
metadata = list(rcpScen = list(units = 'RCP_scenario'))
attr(rcpScen, 'variables') = metadata
names(dim(rcpScen)) = 'rcpScen'


ArrayToNc(list(fldRecurIntrvl, lon, lat, decade, recurInterval, rcpScen), file_path = paste0(ncpath, 'floodRecurIntervals.nc'))
	# testing output, squinty eye test
myNC = nc_open( paste0(ncpath, 'floodRecurIntervals.nc'))
myNC =  nc_open(paste0(ncpath, 'floodRecurIntervals_v5.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'fldRecurIntrvl')


image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])





########################################################################################################################
#	step 2: masking water bodies from flood depth database
# https://www.rdocumentation.org/packages/rnaturalearth/versions/0.1.0
library(rnaturalearth)
library(raster)
library(sf)
sf_use_s2(FALSE) # for error suppression of Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  :
land10 <- ne_download(scale = 10, type = 'land', category = 'physical') #scal50 scale110
sp::plot(land10)
land10Sf = st_as_sf(land10)

	# lons and lats to match the flood rasters
#floodRast = raster('floodMapGL_rp100y.tif')
lats = seq(-54.00824,83.2251,length.out=16468)
lons = seq(-166.8, 180, length.out=41616)

#spPoints = st_multipoint(rbind(c(lats[1], lons[1]), c(lats[100],lons[50])))
pointDf = data.frame('lon' = rep(lons, length(lats)),
	'lat' = rep(lats, each=length(lons)))

	# pointDf is too long to allocate to one SF object, so splitting up runs
theseRows = 1:10000000 #685332288
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
write.csv(goodRows, 'goodRows2.csv')
#sp::plot(goodPointsSf)

	# repeating for all new rows
theseRows = theseRows + 10000000 #685332288
maxRow = 685332288
while(theseRows[1] <= 685332288)
	if(last(theseRows) > maxRow)	{
		theseRows = seq(theseRows[1], maxRow, 1)
	}
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
	goodRows = c(goodRows, theseRows[goodPoints[,11]])
	theseRows = theseRows + 10000000 #685332288
	#sp::plot(goodPointsDf)
}
write.csv(goodRows, 'goodRows2.csv')











# tbd
	# converting tiffs to array
	# read in historic floods data
str_name = paste0('floodMapGL_rp500y.tif')
fileLoc = 'J:\\Cai_data\\TCFD\\CurrentFloodHazard'
fldDepthTiff10 = raster(paste0(fileLoc, '\\floodMapGL_rp10y.tif'))
fldDepthTiff20 = raster(paste0(fileLoc, '\\floodMapGL_rp20y.tif'))
fldDepthTiff50 = raster(paste0(fileLoc, '\\floodMapGL_rp50y.tif'))
fldDepthTiff100 = raster(paste0(fileLoc, '\\floodMapGL_rp100y.tif'))
fldDepthTiff200 = raster(paste0(fileLoc, '\\floodMapGL_rp200y.tif'))
fldDepthTiff500 = raster(paste0(fileLoc, '\\floodMapGL_rp500y.tif'))

	# read in historic floods data
str_name = paste0('floodMapGL_rp500y.tif')
fileLoc = 'J:\\Cai_data\\TCFD\\CurrentFloodHazard'
fldDepthTiff10 = as.matrix(raster(paste0(fileLoc, '\\floodMapGL_rp10y.tif')))
fldDepthTiff20 = as.matrix(raster(paste0(fileLoc, '\\floodMapGL_rp20y.tif')))
fldDepthTiff50 = as.matrix(raster(paste0(fileLoc, '\\floodMapGL_rp50y.tif')))
fldDepthTiff100 = as.matrix(raster(paste0(fileLoc, '\\floodMapGL_rp100y.tif')))
fldDepthTiff200 = as.matrix(raster(paste0(fileLoc, '\\floodMapGL_rp200y.tif')))
fldDepthTiff500 = as.matrix(raster(paste0(fileLoc, '\\floodMapGL_rp500y.tif')))


# array for holding outputs
fldRecurArray = array(c(fldDepthTiff10, fldDepthTiff20, fldDepthTiff50, fldDepthTiff100, fldDepthTiff200 fldDepthTiff500), 
	dim = c(length(tif_lon), length(tif_lat), 6))




fldDepthList = list()
fldDepthList[[1]] = fldDepthTiff10
fldDepthList[[2]] = fldDepthTiff20
fldDepthList[[3]] = fldDepthTiff50
fldDepthList[[4]] = fldDepthTiff100
fldDepthList[[5]] = fldDepthTiff200
fldDepthList[[6]] = fldDepthTiff500

tif_lat = seq(-54.00824,83.2251,length.out=16468)
tif_lon = seq(-166.8, 180, length.out=41616)






########################################################################################################################
# step 3 intersecting recurrence intervals with flood depth
library(raster)

whichRecurIntrvls = c(10)	# customer request: which recurrence intervals
chooseDepth = TRUE			# does the customer want flood depth or just occurrence
customerName = 'Richs Foods'
locationFootprint = 1		# how big is the footprint of the location of interest? in number of 'boxes' to search to the left and right (so 0 is equal to 1 km^2, 1 is 9 km^2, 2 is 25 km^2, etc.
rcpScenarios = c(26, 60)
recurIntrvls = c(10, 20, 50, 100, 200, 500)
whichDecades = seq(10,90,10)

	# reading in table of customer locations
customerPath = 'J:\\Cai_data\\TCFD\\'
customerCsv = 'Richs-sample-locations.csv'
customerTable = fread(paste0(customerPath, customerCsv))
	
	# read in flood recurrence data
fldRcrIntNC = nc_open(paste0(ncpath, 'floodRecurIntervals.nc'))
nc_lat = ncvar_get(fldRcrIntNC, 'lat')
nc_lon = ncvar_get(fldRcrIntNC, 'lon')
fldRcrVals = ncvar_get(fldRcrIntNC, 'fldRecurIntrvl')
fldRcrIntrvlOpts = ncvar_get(fldRcrIntNC, 'recurInterval')


	# array for holding outputs
fldRecurArray = array(rep(-(10^10), length(nc_lon) * length(nc_lat) * length(whichDecades) * length(recurIntrvls) * length(rcpScenarios)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(recurIntrvls), length(rcpScenarios)))



tif_lat = seq(-54.00824,83.2251,length.out=16468)
tif_lon = seq(-166.8, 180, length.out=41616)



	# initializing data output
hazardName = ifelse(chooseDepth, paste0("Flood Hazard (m) ", whichRecurIntrvls[1], 'yr Flood'), paste0('Flood Hazard (-) ', whichRecurIntrvls[1], 'yr Flood'))
dataOutput = data.frame(User = NA, Location = NA, Region = NA, Subregion = NA, Lat = NA, Lon = NA, Hazard = NA, Decade = NA, Scenario = NA, Raw_Hazard_Value = NA, Percentile = NA, Relative_Hazard_Score = NA)

for(thisIntrvl in whichRecurIntrvls)	{
	thisRcrIntDim = which(recurIntrvls == thisIntrvl)

	histFloodImpact = NULL
	for(j in 1:nrow(customerTable))	{
		closeTiffLons = which.min(abs(customerTable$Lon[j] - tif_lon))
		closeTiffLats = which.min(abs(customerTable$Lat[j] - tif_lat))
		
		for(k in 1:locationFootprint)	{
			theseLats = seq(closeTiffLats - locationFootprint, closeTiffLats + locationFootprint, 1)
			theseLons = seq(closeTiffLons - locationFootprint, closeTiffLons + locationFootprint, 1)
			histFloodImpact = ifelse(chooseDepth, mean(fldDepthTiff[theseLats, theseLons], na.rm=TRUE), length(which(fldDepthTiff[theseLats, theseLons] > 0)) / length(theseLats)^2)
			
		}
		
			# check to see if even 500yr floods trigger historically; if not, the skip next analysis
		if(histFloodImpact > 0)	{
			
			
			
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
				Raw_Hazard_Value = 0,
				Percentile = 1,
				Relative_Hazard_Score = 'Low')

				
				data.frame(
					User = NA,
					Region = NA,
					Subregion = NA,	Lat = NA, Lon = NA, Hazard = NA, Decade = NA, Scenario = NA, Raw_Hazard_Value = NA, Percentile = NA, Relative_Hazard_Score = NA)


		closeNCLon = which.min(abs(customerTable$Lon[j] - nc_lon))
			closeNCLat = which.min(abs(customerTable$Lat[j] - nc_lat))
			
				# ensuring we are not drawing from a water tile, then searching box around point if so
			if(fldRcrVals[closeNCLon, closeNCLat, 1, 1, 1] == -1e+10)	{
				closeNCLon = closeNCLon + 1
				if(fldRcrVals[closeNCLon, closeNCLat, 1, 1, 1] == -1e+10)	{
					closeNCLon = closeNCLon - 2
					if(fldRcrVals[closeNCLon, closeNCLat, 1, 1, 1] == -1e+10)	{
						closeNCLon = closeNCLon + 1
						closeNCLat = closeNCLat + 1
						if(fldRcrVals[closeNCLon, closeNCLat, 1, 1, 1] == -1e+10)	{
							closeNCLat = closeNCLat - 2
			}}}}

			theseFldRcrVals = fldRcrVals[closeNCLon, closeNCLat, , which(fldRcrIntrvlOpts == thisIntrvl) , ]












			
	histFloodImpact = NULL
	for(k in 1:nrow(customerTable))	{
		
		histFloodImpact = c(histFloodImpact, 
	








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



