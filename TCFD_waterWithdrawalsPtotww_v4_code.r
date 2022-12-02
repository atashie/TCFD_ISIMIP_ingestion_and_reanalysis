########################################################
library(data.table)
library(lubridate)
library(ncdf4)
library(easyNCDF)	# for ArrayToNc()
#library(robslopes)	# for TheilSen()
#library(trend)		# for sens.slope()
library(mblm)		# for sens slope mlbm()




#########################################
# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\PotentialTotalWaterWithdrawal\\"
ncVarFileName = 'ptotww'
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
saveDate = '30NOV2022'
rcpScenarios = c(26, 60, 85)
whichDecades = seq(10,90,10)
valueType = 1:6

	# reading in dummy data for lat lons
ncname_dummy = paste0('watergap2-2c_gfdl-esm2m_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_dummy = nc_open(paste0(ncpath, ncname_dummy))
nc_lat = ncvar_get(ncin_dummy, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_dummy, 'lon')
nc_close(ncin_dummy)

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(NA, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))


scalar = 60 * 60 * 24 * 30.4375 #kg m-2 s-1 to mm per month



for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]
	rcpScen = paste0('rcp', rcpScenNum)

	ncname_gfdl = paste0('watergap2-2c_gfdl-esm2m_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName)	# lon, lat, time

	ncname_hadgem = paste0('watergap2-2c_hadgem2-es_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
	nc_hadgem = ncvar_get(ncin_hadgem,ncVarFileName)	# lon, lat, time
	
	ncname_ipsl = paste0('watergap2-2c_ipsl-cm5a-lr_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
	nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName)	# lon, lat, time
	
	ncname_miroc = paste0('watergap2-2c_miroc5_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
	nc_miroc = ncvar_get(ncin_miroc,ncVarFileName)	# lon, lat, time

	nc_date = as.Date("1661-01-01") + ncvar_get(ncin_miroc, 'time') * 30.4375# time is months after 1661-1-1
	nc_years = unique(year(nc_date))
	missing_data = 1.00000002004088e+20

	dates1019 = which(year(nc_date) == 2010)[1]:which(year(nc_date) == 2019)[12]
	dates2029 = which(year(nc_date) == 2020)[1]:which(year(nc_date) == 2029)[12]
	dates3039 = which(year(nc_date) == 2030)[1]:which(year(nc_date) == 2039)[12]
	dates4049 = which(year(nc_date) == 2040)[1]:which(year(nc_date) == 2049)[12]
	dates5059 = which(year(nc_date) == 2050)[1]:which(year(nc_date) == 2059)[12]
	dates6069 = which(year(nc_date) == 2060)[1]:which(year(nc_date) == 2069)[12]
	dates7079 = which(year(nc_date) == 2070)[1]:which(year(nc_date) == 2079)[12]
	dates8089 = which(year(nc_date) == 2080)[1]:which(year(nc_date) == 2089)[12]
	dates9099 = which(year(nc_date) == 2090)[1]:which(year(nc_date) == 2099)[12]


	for(i in 1:length(nc_lat))	{
		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j,i,dates1019] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				print(c(i, j))
				nc_1019 = c(nc_gfdl[j,i,dates1019],nc_hadgem[j,i,dates1019],nc_ipsl[j,i,dates1019],nc_miroc[j,i,dates1019]) 
				nc_2029 = c(nc_gfdl[j,i,dates2029],nc_hadgem[j,i,dates2029],nc_ipsl[j,i,dates2029],nc_miroc[j,i,dates2029]) 
				nc_3039 = c(nc_gfdl[j,i,dates3039],nc_hadgem[j,i,dates3039],nc_ipsl[j,i,dates3039],nc_miroc[j,i,dates3039]) 
				nc_4049 = c(nc_gfdl[j,i,dates4049],nc_hadgem[j,i,dates4049],nc_ipsl[j,i,dates4049],nc_miroc[j,i,dates4049]) 
				nc_5059 = c(nc_gfdl[j,i,dates5059],nc_hadgem[j,i,dates5059],nc_ipsl[j,i,dates5059],nc_miroc[j,i,dates5059]) 
				nc_6069 = c(nc_gfdl[j,i,dates6069],nc_hadgem[j,i,dates6069],nc_ipsl[j,i,dates6069],nc_miroc[j,i,dates6069]) 			
				nc_7079 = c(nc_gfdl[j,i,dates7079],nc_hadgem[j,i,dates7079],nc_ipsl[j,i,dates7079],nc_miroc[j,i,dates7079]) 			
				nc_8089 = c(nc_gfdl[j,i,dates8089],nc_hadgem[j,i,dates8089],nc_ipsl[j,i,dates8089],nc_miroc[j,i,dates8089]) 		
				nc_9099 = c(nc_gfdl[j,i,dates9099],nc_hadgem[j,i,dates9099],nc_ipsl[j,i,dates9099],nc_miroc[j,i,dates9099]) 			

				datesSeq1019 = rep(seq(first(dates1019), last(dates1019), 12), 4)
				datesSeq2029 = rep(seq(first(dates2029), last(dates2029), 12), 4)
				datesSeq3039 = rep(seq(first(dates3039), last(dates3039), 12), 4)
				datesSeq4049 = rep(seq(first(dates4049), last(dates4049), 12), 4)
				datesSeq5059 = rep(seq(first(dates5059), last(dates5059), 12), 4)
				datesSeq6069 = rep(seq(first(dates6069), last(dates6069), 12), 4)
				datesSeq7079 = rep(seq(first(dates7079), last(dates7079), 12), 4)
				datesSeq8089 = rep(seq(first(dates8089), last(dates8089), 12), 4)
				datesSeq9099 = rep(seq(first(dates9099), last(dates9099), 12), 4)


				data1019 = NULL
				for(kh in 1:(length(nc_1019)/12))	{
					data1019 = c(data1019,
						sum(nc_1019[(1:12)+(12*(kh-1))]))
				}
				data2029 = NULL
				for(kh in 1:(length(nc_2029)/12))	{
					data2029 = c(data2029,
						sum(nc_2029[(1:12)+(12*(kh-1))]))
				}
				data3039 = NULL
				for(kh in 1:(length(nc_3039)/12))	{
					data3039 = c(data3039,
						sum(nc_3039[(1:12)+(12*(kh-1))]))
				}
				data4049 = NULL
				for(kh in 1:(length(nc_4049)/12))	{
					data4049 = c(data4049,
						sum(nc_4049[(1:12)+(12*(kh-1))]))
				}
				data5059 = NULL
				for(kh in 1:(length(nc_5059)/12))	{
					data5059 = c(data5059,
						sum(nc_5059[(1:12)+(12*(kh-1))]))
				}
				data6069 = NULL
				for(kh in 1:(length(nc_6069)/12))	{
					data6069 = c(data6069,
						sum(nc_6069[(1:12)+(12*(kh-1))]))
				}
				data7079 = NULL
				for(kh in 1:(length(nc_7079)/12))	{
					data7079 = c(data7079,
						sum(nc_7079[(1:12)+(12*(kh-1))]))
				}
				data8089 = NULL
				for(kh in 1:(length(nc_8089)/12))	{
					data8089 = c(data8089,
						sum(nc_8089[(1:12)+(12*(kh-1))]))
				}
				data9099 = NULL
				for(kh in 1:(length(nc_9099)/12))	{
					data9099 = c(data9099,
						sum(nc_9099[(1:12)+(12*(kh-1))]))
				}

					# defining absolute values
				dataOutArray[j, i, 1, thisScen, 1] = sum(data1019) * scalar
				dataOutArray[j, i, 2, thisScen, 1] = sum(data2029) * scalar
				dataOutArray[j, i, 3, thisScen, 1] = sum(data3039) * scalar
				dataOutArray[j, i, 4, thisScen, 1] = sum(data4049) * scalar
				dataOutArray[j, i, 5, thisScen, 1] = sum(data5059) * scalar
				dataOutArray[j, i, 6, thisScen, 1] = sum(data6069) * scalar
				dataOutArray[j, i, 7, thisScen, 1] = sum(data7079) * scalar
				dataOutArray[j, i, 8, thisScen, 1] = sum(data8089) * scalar
				dataOutArray[j, i, 9, thisScen, 1] = sum(data9099) * scalar

					# calculating decadal trends (sens slope) and 	decadal significance (spearmans)	
				theDates = datesSeq1019
				dataOutArray[j, i, 1, thisScen, 3] = lm(data1019 ~ theDates)$coefficients[2] * 10 * scalar
				dataOutArray[j, i, 1, thisScen, 4] = cor.test(theDates, data1019, method='spearman')$p.value
				
				theDates = c(theDates, datesSeq2029)
				theValues =  c(data1019, data2029)
				dataOutArray[j, i, 2, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * scalar
				dataOutArray[j, i, 2, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value
				
				theDates = c(theDates, datesSeq3039)
				theValues =  c(data1019, data2029, data3039)
				dataOutArray[j, i, 3, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * scalar
				dataOutArray[j, i, 3, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value
				
				theDates = c(theDates, datesSeq4049)
				theValues =  c(data1019, data2029, data3039, data4049)
				dataOutArray[j, i, 4, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * scalar
				dataOutArray[j, i, 4, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value
				
				theDates = c(theDates, datesSeq5059)
				theValues =  c(data1019, data2029, data3039, data4049, data5059)
				dataOutArray[j, i, 5, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * scalar
				dataOutArray[j, i, 5, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value
				
				theDates = c(theDates, datesSeq6069)
				theValues =  c(data1019, data2029, data3039, data4049, data5059, data6069)
				dataOutArray[j, i, 6, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * scalar
				dataOutArray[j, i, 6, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value
				
				theDates = c(theDates, datesSeq7079)
				theValues =  c(data1019, data2029, data3039, data4049, data5059, data6069, data7079)
				dataOutArray[j, i, 7, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * scalar
				dataOutArray[j, i, 7, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value
				
				theDates = c(theDates, datesSeq8089)
				theValues =  c(data1019, data2029, data3039, data4049, data5059, data6069, data7079, data8089)
				dataOutArray[j, i, 8, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * scalar
				dataOutArray[j, i, 8, thisScen, 4] = cor.test(theDates, theValues, method='spearman')$p.value
				
				theDates = c(theDates, datesSeq9099)
				theValues =  c(data1019, data2029, data3039, data4049, data5059, data6069, data7079, data8089, data9099)
				dataOutArray[j, i, 9, thisScen, 3] = lm(theValues ~ theDates)$coefficients[2] * 10 * scalar
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

dataOutArray = readRDS(file=paste0(ncpath, 'data_out.rds'))

	# defining quantiles
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 3, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 3, 1][-maskedLocs85]
histQuants = quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01))


for(i in 1:length(whichDecades))	{
	dataOutArray[ , , i, 1, 2] = 1
	dataOutArray[ , , i, 2, 2] = 1
	dataOutArray[ , , i, 3, 2] = 1
	for(j in 1:(length(histQuants)))	{
		dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] > histQuants[j]] = j
		dataOutArray[ , , i, 2, 2][dataOutArray[ , , i, 2, 1] > histQuants[j]] = j
		dataOutArray[ , , i, 3, 2][dataOutArray[ , , i, 3, 1] > histQuants[j]] = j
	}
	dataOutArray[ , , i, 1, 2][maskedLocs26] = NA
	dataOutArray[ , , i, 2, 2][maskedLocs60] = NA
	dataOutArray[ , , i, 3, 2][maskedLocs85] = NA
}


tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Fire - % Area Burned / yr'))
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

image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,1] - nc_testDat[,,1,3,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,1] - nc_testDat[,,1,2,1])

image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,3,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,2,2])

