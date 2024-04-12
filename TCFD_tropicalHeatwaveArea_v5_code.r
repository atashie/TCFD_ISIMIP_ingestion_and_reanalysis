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
ncpath = "J:\\Cai_data\\TCFD\\Heatwave\\"
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
ncVarFileName = 'leh'
saveDate = '27MAR024'
rcpScenarios = c(26, 60)
whichDecades = seq(10,90,10)
valueType = 1:6


	# initializing start decade
initDates = 1:14
ncname_gfdl = paste0('lange2020_hwmid-humidex_gfdl-esm2m_ewembi_rcp26_nosoc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
nc_gfdl_init = ncvar_get(ncin_gfdl,ncVarFileName)[ , , initDates]	# lon, lat, time
	# identifying lat lons before closing data
nc_lat = ncvar_get(ncin_gfdl, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_gfdl, 'lon')
nc_close(ncin_gfdl)

ncname_hadgem = paste0('lange2020_hwmid-humidex_hadgem2-es_ewembi_rcp26_nosoc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_hadgem-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
nc_hadgem_init = ncvar_get(ncin_hadgem,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_hadgem)

ncname_ipsl = paste0('lange2020_hwmid-humidex_ipsl-cm5a-lr_ewembi_rcp26_nosoc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_ipsl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
nc_ipsl_init = ncvar_get(ncin_ipsl,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_ipsl)

ncname_miroc = paste0('lange2020_hwmid-humidex_miroc5_ewembi_rcp26_nosoc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_miroc-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
nc_miroc_init = ncvar_get(ncin_miroc,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_miroc)

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))

scalar = 100

for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]
	rcpScen = paste0('rcp', rcpScenNum)

	ncname_gfdl = paste0('lange2020_hwmid-humidex_gfdl-esm2m_ewembi_',rcpScen,'_nosoc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName)	# lon, lat, time

	ncname_hadgem = paste0('lange2020_hwmid-humidex_hadgem2-es_ewembi_',rcpScen,'_nosoc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
	nc_hadgem = ncvar_get(ncin_hadgem,ncVarFileName)	# lon, lat, time
	
	ncname_ipsl = paste0('lange2020_hwmid-humidex_ipsl-cm5a-lr_ewembi_',rcpScen,'_nosoc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
	nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName)	# lon, lat, time
	
	ncname_miroc = paste0('lange2020_hwmid-humidex_miroc5_ewembi_',rcpScen,'_nosoc_co2_', ncVarFileName, '_global_annual_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
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

				if(j <= 2 | j >= length(nc_lon) - 1 | i <= 2 | i >= length(nc_lat) -1)	{
					gfdl_yrly = c(nc_gfdl_init[j,i, initDates], nc_gfdl[j,i, -initDates]) * scalar
					hadgem_yrly = c(nc_hadgem_init[j,i, initDates], nc_hadgem[j,i, -initDates]) * scalar
					ipsl_yrly = c(nc_ipsl_init[j,i, initDates], nc_ipsl[j,i, -initDates])  * scalar
					miroc_yrly = c(nc_miroc_init[j,i, initDates], nc_miroc[j,i, -initDates]) * scalar
				}	else	{
					gfdl_yrly = apply(cbind(c(nc_gfdl_init[j,i, initDates], nc_gfdl[j,i, -initDates]),
						c(nc_gfdl_init[j-1,i, initDates], nc_gfdl[j-1,i, -initDates]),
						c(nc_gfdl_init[j+1,i, initDates], nc_gfdl[j+1,i, -initDates]),
						c(nc_gfdl_init[j,i-1, initDates], nc_gfdl[j,i-1, -initDates]), 
						c(nc_gfdl_init[j,i+1, initDates], nc_gfdl[j,i+1, -initDates]),
						c(nc_gfdl_init[j-1,i-1, initDates], nc_gfdl[j-1,i-1, -initDates]),
						c(nc_gfdl_init[j+1,i+1, initDates], nc_gfdl[j+1,i+1, -initDates]),
						c(nc_gfdl_init[j+1,i-1, initDates], nc_gfdl[j+1,i-1, -initDates]), 
						c(nc_gfdl_init[j-1,i+1, initDates], nc_gfdl[j-1,i+1, -initDates]),
						c(nc_gfdl_init[j-2,i, initDates], nc_gfdl[j-2,i, -initDates]),
						c(nc_gfdl_init[j+2,i, initDates], nc_gfdl[j+2,i, -initDates]),
						c(nc_gfdl_init[j,i-2, initDates], nc_gfdl[j,i-2, -initDates]), 
						c(nc_gfdl_init[j,i+2, initDates], nc_gfdl[j,i+2, -initDates]),
						c(nc_gfdl_init[j-2,i-1, initDates], nc_gfdl[j-2,i-1, -initDates]),
						c(nc_gfdl_init[j+2,i-1, initDates], nc_gfdl[j+2,i-1, -initDates]),
						c(nc_gfdl_init[j-1,i-2, initDates], nc_gfdl[j-1,i-2, -initDates]), 
						c(nc_gfdl_init[j-1,i+2, initDates], nc_gfdl[j-1,i+2, -initDates]),
						c(nc_gfdl_init[j-2,i+1, initDates], nc_gfdl[j-2,i+1, -initDates]),
						c(nc_gfdl_init[j+2,i+1, initDates], nc_gfdl[j+2,i+1, -initDates]),
						c(nc_gfdl_init[j+1,i-2, initDates], nc_gfdl[j+1,i-2, -initDates]), 
						c(nc_gfdl_init[j+1,i+2, initDates], nc_gfdl[j+1,i+2, -initDates])) * scalar,
						1, mean, na.rm = TRUE)
					hadgem_yrly = apply(cbind(c(nc_hadgem_init[j,i, initDates], nc_hadgem[j,i, -initDates]),
						c(nc_hadgem_init[j-1,i, initDates], nc_hadgem[j-1,i, -initDates]),
						c(nc_hadgem_init[j+1,i, initDates], nc_hadgem[j+1,i, -initDates]),
						c(nc_hadgem_init[j,i-1, initDates], nc_hadgem[j,i-1, -initDates]), 
						c(nc_hadgem_init[j,i+1, initDates], nc_hadgem[j,i+1, -initDates]),
						c(nc_hadgem_init[j-1,i-1, initDates], nc_hadgem[j-1,i-1, -initDates]),
						c(nc_hadgem_init[j+1,i+1, initDates], nc_hadgem[j+1,i+1, -initDates]),
						c(nc_hadgem_init[j+1,i-1, initDates], nc_hadgem[j+1,i-1, -initDates]), 
						c(nc_hadgem_init[j-1,i+1, initDates], nc_hadgem[j-1,i+1, -initDates]),
						c(nc_hadgem_init[j-2,i, initDates], nc_hadgem[j-2,i, -initDates]),
						c(nc_hadgem_init[j+2,i, initDates], nc_hadgem[j+2,i, -initDates]),
						c(nc_hadgem_init[j,i-2, initDates], nc_hadgem[j,i-2, -initDates]), 
						c(nc_hadgem_init[j,i+2, initDates], nc_hadgem[j,i+2, -initDates]),
						c(nc_hadgem_init[j-2,i-1, initDates], nc_hadgem[j-2,i-1, -initDates]),
						c(nc_hadgem_init[j+2,i-1, initDates], nc_hadgem[j+2,i-1, -initDates]),
						c(nc_hadgem_init[j-1,i-2, initDates], nc_hadgem[j-1,i-2, -initDates]), 
						c(nc_hadgem_init[j-1,i+2, initDates], nc_hadgem[j-1,i+2, -initDates]),
						c(nc_hadgem_init[j-2,i+1, initDates], nc_hadgem[j-2,i+1, -initDates]),
						c(nc_hadgem_init[j+2,i+1, initDates], nc_hadgem[j+2,i+1, -initDates]),
						c(nc_hadgem_init[j+1,i-2, initDates], nc_hadgem[j+1,i-2, -initDates]), 
						c(nc_hadgem_init[j+1,i+2, initDates], nc_hadgem[j+1,i+2, -initDates])) * scalar,
						1, mean, na.rm = TRUE)
					ipsl_yrly = apply(cbind(c(nc_ipsl_init[j,i, initDates], nc_ipsl[j,i, -initDates]),
						c(nc_ipsl_init[j-1,i, initDates], nc_ipsl[j-1,i, -initDates]),
						c(nc_ipsl_init[j+1,i, initDates], nc_ipsl[j+1,i, -initDates]),
						c(nc_ipsl_init[j,i-1, initDates], nc_ipsl[j,i-1, -initDates]), 
						c(nc_ipsl_init[j,i+1, initDates], nc_ipsl[j,i+1, -initDates]),
						c(nc_ipsl_init[j-1,i-1, initDates], nc_ipsl[j-1,i-1, -initDates]),
						c(nc_ipsl_init[j+1,i+1, initDates], nc_ipsl[j+1,i+1, -initDates]),
						c(nc_ipsl_init[j+1,i-1, initDates], nc_ipsl[j+1,i-1, -initDates]), 
						c(nc_ipsl_init[j-1,i+1, initDates], nc_ipsl[j-1,i+1, -initDates]),
						c(nc_ipsl_init[j-2,i, initDates], nc_ipsl[j-2,i, -initDates]),
						c(nc_ipsl_init[j+2,i, initDates], nc_ipsl[j+2,i, -initDates]),
						c(nc_ipsl_init[j,i-2, initDates], nc_ipsl[j,i-2, -initDates]), 
						c(nc_ipsl_init[j,i+2, initDates], nc_ipsl[j,i+2, -initDates]),
						c(nc_ipsl_init[j-2,i-1, initDates], nc_ipsl[j-2,i-1, -initDates]),
						c(nc_ipsl_init[j+2,i-1, initDates], nc_ipsl[j+2,i-1, -initDates]),
						c(nc_ipsl_init[j-1,i-2, initDates], nc_ipsl[j-1,i-2, -initDates]), 
						c(nc_ipsl_init[j-1,i+2, initDates], nc_ipsl[j-1,i+2, -initDates]),
						c(nc_ipsl_init[j-2,i+1, initDates], nc_ipsl[j-2,i+1, -initDates]),
						c(nc_ipsl_init[j+2,i+1, initDates], nc_ipsl[j+2,i+1, -initDates]),
						c(nc_ipsl_init[j+1,i-2, initDates], nc_ipsl[j+1,i-2, -initDates]), 
						c(nc_ipsl_init[j+1,i+2, initDates], nc_ipsl[j+1,i+2, -initDates])) * scalar,
						1, mean, na.rm = TRUE)
					miroc_yrly = apply(cbind(c(nc_miroc_init[j,i, initDates], nc_miroc[j,i, -initDates]),
						c(nc_miroc_init[j-1,i, initDates], nc_miroc[j-1,i, -initDates]),
						c(nc_miroc_init[j+1,i, initDates], nc_miroc[j+1,i, -initDates]),
						c(nc_miroc_init[j,i-1, initDates], nc_miroc[j,i-1, -initDates]), 
						c(nc_miroc_init[j,i+1, initDates], nc_miroc[j,i+1, -initDates]),
						c(nc_miroc_init[j-1,i-1, initDates], nc_miroc[j-1,i-1, -initDates]),
						c(nc_miroc_init[j+1,i+1, initDates], nc_miroc[j+1,i+1, -initDates]),
						c(nc_miroc_init[j+1,i-1, initDates], nc_miroc[j+1,i-1, -initDates]), 
						c(nc_miroc_init[j-1,i+1, initDates], nc_miroc[j-1,i+1, -initDates]),
						c(nc_miroc_init[j-2,i, initDates], nc_miroc[j-2,i, -initDates]),
						c(nc_miroc_init[j+2,i, initDates], nc_miroc[j+2,i, -initDates]),
						c(nc_miroc_init[j,i-2, initDates], nc_miroc[j,i-2, -initDates]), 
						c(nc_miroc_init[j,i+2, initDates], nc_miroc[j,i+2, -initDates]),
						c(nc_miroc_init[j-2,i-1, initDates], nc_miroc[j-2,i-1, -initDates]),
						c(nc_miroc_init[j+2,i-1, initDates], nc_miroc[j+2,i-1, -initDates]),
						c(nc_miroc_init[j-1,i-2, initDates], nc_miroc[j-1,i-2, -initDates]), 
						c(nc_miroc_init[j-1,i+2, initDates], nc_miroc[j-1,i+2, -initDates]),
						c(nc_miroc_init[j-2,i+1, initDates], nc_miroc[j-2,i+1, -initDates]),
						c(nc_miroc_init[j+2,i+1, initDates], nc_miroc[j+2,i+1, -initDates]),
						c(nc_miroc_init[j+1,i-2, initDates], nc_miroc[j+1,i-2, -initDates]), 
						c(nc_miroc_init[j+1,i+2, initDates], nc_miroc[j+1,i+2, -initDates])) * scalar,
						1, mean, na.rm = TRUE)
				}

				gfdl_smth = ksmooth(nc_years, gfdl_yrly, kernel = 'normal', bandwidth = 100, n.points = numYears)$y
				hadgem_smth = ksmooth(nc_years, hadgem_yrly, kernel = 'normal', bandwidth = 100, n.points = numYears)$y
				ipsl_smth = ksmooth(nc_years, ipsl_yrly, kernel = 'normal', bandwidth = 100, n.points = numYears)$y
				miroc_smth = ksmooth(nc_years, miroc_yrly, kernel = 'normal', bandwidth = 100, n.points = numYears)$y

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
#	saveRDS(dataOutArray, file=paste0(ncpath, 'data_out.rds'))
#	saveRDS(dataOutArray, file=paste0(ncpath, 'data_out_bigSmoothAndSpatial.rds'))
#	saveRDS(dataOutArray, file=paste0(ncpath, 'data_out_bigSmoothAndSpatialMore.rds'))
	saveRDS(dataOutArray, file=paste0(ncpath, 'data_out_bigSmoothAndSpatialMorer.rds'))
	}
	nc_close(ncin_gfdl)
	nc_close(ncin_hadgem)
	nc_close(ncin_ipsl)
	nc_close(ncin_miroc)
}

#dataOutArray = readRDS(file=paste0(ncpath, 'data_out.rds'))
#dataOutArray = readRDS(file=paste0(ncpath, 'data_out_bigSmoothAndSpatial.rds'))
#dataOutArray = readRDS(file=paste0(ncpath, 'data_out_bigSmoothAndSpatialMore.rds'))
dataOutArray = readRDS(file=paste0(ncpath, 'data_out_bigSmoothAndSpatialMorer.rds'))
##### temp fix for not having rcp 8.5
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), 3, length(valueType)))
old_dataOutArray = readRDS(file=paste0(ncpath, 'data_out_bigSmoothAndSpatialMorer.rds'))
dataOutArray[ , , , 1:2, ] = old_dataOutArray[ , , , 1:2, ]
##### end temp fix



	# removing zeroes from non-impacted regions
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
histQuants = quantile(c(histDatSubset26, histDatSubset60), seq(0.01, 1, length.out=100))
histQuants

minSignif = 0.5
	# removing low values from non-impacted regions
maskedLocs26_zeroes = which(is.na(dataOutArray[ , , 1, 1, 1]) | dataOutArray[ , , 1, 1, 1] < minSignif)
histDatSubset26_zeroes =  dataOutArray[ , , 1, 1, 1][-maskedLocs26_zeroes]
maskedLocs60_zeroes = which(is.na(dataOutArray[ , , 1, 2, 1]) | dataOutArray[ , , 1, 2, 1] < minSignif)
histDatSubset60_zeroes =  dataOutArray[ , , 1, 2, 1][-maskedLocs60_zeroes]
histQuants = quantile(c(histDatSubset26_zeroes, histDatSubset60_zeroes), seq(0.01, 1, length.out=100))
histQuants

for(i in 1:length(whichDecades))	{
	dataOutArray[ , , i, 1, 2] = 1
	dataOutArray[ , , i, 2, 2] = 1
	for(j in 1:(length(histQuants)))	{
		dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] > histQuants[j]] = j + 0
		dataOutArray[ , , i, 2, 2][dataOutArray[ , , i, 2, 1] > histQuants[j]] = j + 0
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
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncOutputPath, ncVarFileName, 'v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncOutputPath, ncVarFileName, 'v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,2,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,3,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,3])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,4])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,5])
image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,6])

image(nc_lon, rev(nc_lat), nc_testDat[,,9,2,2] - nc_testDat[,,1,2,2])

image(nc_lon, rev(nc_lat), nc_testDat[,,9,1,1] - nc_testDat[,,1,1,1])

image(nc_lon, rev(nc_lat), nc_testDat[,,9,2,1] - nc_testDat[,,1,2,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,2,1] - nc_testDat[,,1,2,1])

image(nc_lon, rev(nc_lat), nc_testDat[,,9,2,2] - nc_testDat[,,1,2,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,2,2] - nc_testDat[,,1,2,2])

image(nc_lon, rev(nc_lat), nc_testDat[,,1,2,6] - nc_testDat[,,1,2,5])