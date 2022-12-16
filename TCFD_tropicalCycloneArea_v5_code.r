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
ncpath = "J:\\Cai_data\\TCFD\\TropicalCycloneArea\\"
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
ncVarFileName = 'let'
saveDate = '09DEC2022'
rcpScenarios = c(26, 60)
whichDecades = seq(10,90,10)
valueType = 1:6


	# reading in dummy data for lat lons
ncname_dummy = paste0('lange2020_ke-tg-meanfield_gfdl-esm2m_ewembi_rcp60_nosoc_co2_let_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_dummy = nc_open(paste0(ncpath, ncname_dummy))
nc_lat = ncvar_get(ncin_dummy, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_dummy, 'lon')


	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))


for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]
	rcpScen = paste0('rcp', rcpScenNum)

	ncname_gfdl = paste0('lange2020_ke-tg-meanfield_gfdl-esm2m_ewembi_',rcpScen,'_nosoc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName)	# lon, lat, time

	ncname_hadgem = paste0('lange2020_ke-tg-meanfield_hadgem2-es_ewembi_',rcpScen,'_nosoc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
	nc_hadgem = ncvar_get(ncin_hadgem,ncVarFileName)	# lon, lat, time
	
	ncname_ipsl = paste0('lange2020_ke-tg-meanfield_ipsl-cm5a-lr_ewembi_',rcpScen,'_nosoc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
	nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName)	# lon, lat, time
	
	ncname_miroc = paste0('lange2020_ke-tg-meanfield_miroc5_ewembi_',rcpScen,'_nosoc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
	nc_miroc = ncvar_get(ncin_miroc,ncVarFileName)	# lon, lat, time

	nc_date = as.Date("1661-01-01") + ncvar_get(ncin_miroc, 'time') * 365.25# time is months after 1661-1-1
	nc_years = unique(year(nc_date))
	missing_data = 1.00000002004088e+20

	dates1019 = which(year(nc_date) == 2010):which(year(nc_date) == 2019)
	dates2029 = which(year(nc_date) == 2020):which(year(nc_date) == 2029)
	dates3039 = which(year(nc_date) == 2030):which(year(nc_date) == 2039)
	dates4049 = which(year(nc_date) == 2040):which(year(nc_date) == 2049)
	dates5059 = which(year(nc_date) == 2050):which(year(nc_date) == 2059)
	dates6069 = which(year(nc_date) == 2060):which(year(nc_date) == 2069)
	dates7079 = which(year(nc_date) == 2070):which(year(nc_date) == 2079)
	dates8089 = which(year(nc_date) == 2080):which(year(nc_date) == 2089)
	dates9099 = which(year(nc_date) == 2090):which(year(nc_date) == 2099)

	wt1019 = c(seq(0.1,1,length.out = (2015 - 2006))^2, 1, rev(seq(0.1,1,length.out=(2099 - 2015)))^2)
	wt2029 = c(seq(0.1,1,length.out = (2025 - 2006))^2, 1, rev(seq(0.1,1,length.out=(2099 - 2025)))^2)
	wt3039 = c(seq(0.1,1,length.out = (2035 - 2006))^2, 1, rev(seq(0.1,1,length.out=(2099 - 2035)))^2)
	wt4049 = c(seq(0.1,1,length.out = (2045 - 2006))^2, 1, rev(seq(0.1,1,length.out=(2099 - 2045)))^2)
	wt5059 = c(seq(0.1,1,length.out = (2055 - 2006))^2, 1, rev(seq(0.1,1,length.out=(2099 - 2055)))^2)
	wt6069 = c(seq(0.1,1,length.out = (2065 - 2006))^2, 1, rev(seq(0.1,1,length.out=(2099 - 2065)))^2)
	wt7079 = c(seq(0.1,1,length.out = (2075 - 2006))^2, 1, rev(seq(0.1,1,length.out=(2099 - 2075)))^2)
	wt8089 = c(seq(0.1,1,length.out = (2085 - 2006))^2, 1, rev(seq(0.1,1,length.out=(2099 - 2085)))^2)
	wt9099 = c(seq(0.1,1,length.out = (2095 - 2006))^2, 1, rev(seq(0.1,1,length.out=(2099 - 2095)))^2)

	for(i in 1:length(nc_lat))	{
		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j,i,dates1019] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				print(c(i, j))
				dat_gfdl = nc_gfdl[j,i,]
				dat_hadgem = nc_hadgem[j,i,]
				dat_ipsl = nc_ipsl[j,i,]
				dat_miroc = nc_miroc[j,i,]

				nc_1019 = c(nc_gfdl[j,i,dates1019],nc_hadgem[j,i,dates1019],nc_ipsl[j,i,dates1019],nc_miroc[j,i,dates1019]) 
				nc_2029 = c(nc_gfdl[j,i,dates2029],nc_hadgem[j,i,dates2029],nc_ipsl[j,i,dates2029],nc_miroc[j,i,dates2029]) 
				nc_3039 = c(nc_gfdl[j,i,dates3039],nc_hadgem[j,i,dates3039],nc_ipsl[j,i,dates3039],nc_miroc[j,i,dates3039]) 
				nc_4049 = c(nc_gfdl[j,i,dates4049],nc_hadgem[j,i,dates4049],nc_ipsl[j,i,dates4049],nc_miroc[j,i,dates4049]) 
				nc_5059 = c(nc_gfdl[j,i,dates5059],nc_hadgem[j,i,dates5059],nc_ipsl[j,i,dates5059],nc_miroc[j,i,dates5059]) 
				nc_6069 = c(nc_gfdl[j,i,dates6069],nc_hadgem[j,i,dates6069],nc_ipsl[j,i,dates6069],nc_miroc[j,i,dates6069]) 			
				nc_7079 = c(nc_gfdl[j,i,dates7079],nc_hadgem[j,i,dates7079],nc_ipsl[j,i,dates7079],nc_miroc[j,i,dates7079]) 			
				nc_8089 = c(nc_gfdl[j,i,dates8089],nc_hadgem[j,i,dates8089],nc_ipsl[j,i,dates8089],nc_miroc[j,i,dates8089]) 		
				nc_9099 = c(nc_gfdl[j,i,dates9099],nc_hadgem[j,i,dates9099],nc_ipsl[j,i,dates9099],nc_miroc[j,i,dates9099]) 			


					# defining absolute values
				dataOutArray[j, i, 1, thisScen, 1] = mean(weighted.mean(c(dat_gfdl, dat_hadgem, dat_ipsl, dat_miroc), rep(wt1019,4))) * 100
				dataOutArray[j, i, 2, thisScen, 1] = mean(weighted.mean(c(dat_gfdl, dat_hadgem, dat_ipsl, dat_miroc), rep(wt2029,4))) * 100
				dataOutArray[j, i, 3, thisScen, 1] = mean(weighted.mean(c(dat_gfdl, dat_hadgem, dat_ipsl, dat_miroc), rep(wt3039,4))) * 100
				dataOutArray[j, i, 4, thisScen, 1] = mean(weighted.mean(c(dat_gfdl, dat_hadgem, dat_ipsl, dat_miroc), rep(wt4049,4))) * 100
				dataOutArray[j, i, 5, thisScen, 1] = mean(weighted.mean(c(dat_gfdl, dat_hadgem, dat_ipsl, dat_miroc), rep(wt5059,4))) * 100
				dataOutArray[j, i, 6, thisScen, 1] = mean(weighted.mean(c(dat_gfdl, dat_hadgem, dat_ipsl, dat_miroc), rep(wt6069,4))) * 100
				dataOutArray[j, i, 7, thisScen, 1] = mean(weighted.mean(c(dat_gfdl, dat_hadgem, dat_ipsl, dat_miroc), rep(wt7079,4))) * 100
				dataOutArray[j, i, 8, thisScen, 1] = mean(weighted.mean(c(dat_gfdl, dat_hadgem, dat_ipsl, dat_miroc), rep(wt8089,4))) * 100
				dataOutArray[j, i, 9, thisScen, 1] = mean(weighted.mean(c(dat_gfdl, dat_hadgem, dat_ipsl, dat_miroc), rep(wt9099,4))) * 100


					# calculating decadal trends (sens slope) and 	decadal significance (spearmans)	
				theDates = rep(dates1019, 4)
				dataOutArray[j, i, 1, thisScen, 3] = lm(nc_1019 ~ theDates)$coefficients[2] * 10 * 100
				dataOutArray[j, i, 1, thisScen, 4] = cor.test(theDates, nc_1019, method='spearman')$p.value
				
				theDates = c(theDates, rep(dates2029, 4))
				theValues =  c(nc_1019, nc_2029)
				dataOutArray[j, i, 2, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * 100
				dataOutArray[j, i, 2, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value
				
				theDates = c(theDates, rep(dates3039, 4))
				theValues =  c(nc_1019, nc_2029, nc_3039)
				dataOutArray[j, i, 3, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * 100
				dataOutArray[j, i, 3, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value
				
				theDates = c(theDates, rep(dates4049, 4))
				theValues =  c(nc_1019, nc_2029, nc_3039, nc_4049)
				dataOutArray[j, i, 4, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * 100
				dataOutArray[j, i, 4, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value
				
				theDates = c(theDates, rep(dates5059, 4))
				theValues =  c(nc_1019, nc_2029, nc_3039, nc_4049, nc_5059)
				dataOutArray[j, i, 5, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * 100
				dataOutArray[j, i, 5, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value
				
				theDates = c(theDates, rep(dates6069, 4))
				theValues =  c(nc_1019, nc_2029, nc_3039, nc_4049, nc_5059, nc_6069)
				dataOutArray[j, i, 6, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * 100
				dataOutArray[j, i, 6, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value
				
				theDates = c(theDates, rep(dates7079, 4))
				theValues =  c(nc_1019, nc_2029, nc_3039, nc_4049, nc_5059, nc_6069, nc_7079)
				dataOutArray[j, i, 7, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * 100
				dataOutArray[j, i, 7, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value
				
				theDates = c(theDates, rep(dates8089, 4))
				theValues =  c(nc_1019, nc_2029, nc_3039, nc_4049, nc_5059, nc_6069, nc_7079, nc_8089)
				dataOutArray[j, i, 8, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * 100
				dataOutArray[j, i, 8, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value
				
				theDates = c(theDates, rep(dates9099, 4))
				theValues =  c(nc_1019, nc_2029, nc_3039, nc_4049, nc_5059, nc_6069, nc_7079, nc_8089, nc_9099)
				dataOutArray[j, i, 9, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * 100
				dataOutArray[j, i, 9, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value

					
					# calculating long-term trends (sens slope)
				dataOutArray[j, i, , thisScen, 5] = dataOutArray[j, i, 9, thisScen, 3]

					# calculating long-term significance (spearmans)
				dataOutArray[j, i, , thisScen, 6] = dataOutArray[j, i, 9, thisScen, 4]				

			}
		}
	saveRDS(dataOutArray, file=paste0(ncpath, 'data_out.rds'))
	}
	nc_close(ncin_gfdl)
	nc_close(ncin_hadgem)
	nc_close(ncin_ipsl)
	nc_close(ncin_miroc)
}

#dataOutArray = readRDS(file=paste0(ncpath, 'data_out.rds'))
##### temp fix for not having rcp 8.5
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), 3, length(valueType)))
old_dataOutArray = readRDS(file=paste0(ncpath, 'data_out.rds'))
dataOutArray[ , , , 1:2, ] = old_dataOutArray
##### end temp fix


	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
#histQuants = quantile(c(histDatSubset26, histDatSubset60), seq(0.01, 1, 0.01))

	# removing zeroes from non-impacted regions
maskedLocs26_zeroes = which(is.na(dataOutArray[ , , 1, 1, 1]) | dataOutArray[ , , 1, 1, 1] == 0)
histDatSubset26_zeroes =  dataOutArray[ , , 1, 1, 1][-maskedLocs26_zeroes]
maskedLocs60_zeroes = which(is.na(dataOutArray[ , , 1, 2, 1]) | dataOutArray[ , , 1, 2, 1] == 0)
histDatSubset60_zeroes =  dataOutArray[ , , 1, 2, 1][-maskedLocs60_zeroes]
histQuants = quantile(c(histDatSubset26_zeroes, histDatSubset60_zeroes), seq(0.01, 1, length.out=80))
#oldHistQuants

for(i in 1:length(whichDecades))	{
	dataOutArray[ , , i, 1, 2] = 1
	dataOutArray[ , , i, 2, 2] = 1
	for(j in 1:(length(histQuants)))	{
		dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] > histQuants[j]] = j + 20
		dataOutArray[ , , i, 2, 2][dataOutArray[ , , i, 2, 1] > histQuants[j]] = j + 20
	}
	dataOutArray[ , , i, 1, 2][maskedLocs26] = NA
	dataOutArray[ , , i, 2, 2][maskedLocs60] = NA
}



tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Tropical Cyclone - annual % area impacted'))
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

#### temp fix for not having rcp 8.5
rcpScenarios = c(26, 60, 85)
#### end temp fix for not having rcp 8.5
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
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncOutputPath, ncVarFileName, '_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncOutputPath, ncVarFileName, '_processed.nc'))
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

image(nc_lon, rev(nc_lat), nc_testDat[,,1,3,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,3,3])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,3,4])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,3,5])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,3,6])

