####################################################
library(data.table)
library(lubridate)
library(ncdf4)
library(easyNCDF)	# for ArrayToNc()
#library(robslopes)	# for TheilSen()
#library(trend)		# for sens.slope()
#library(mblm)		# for sens slope mlbm()
#library(cmsafops)	# for cmsaf.div() and cmsaf.sub()



#########################################
# naming in climate netcdf data
ncpath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
ncVarName = 'tcfdVariable'

ncFileName_PPT_annual = 'Total_Precipitation_v2_processed.nc'
ncFileName_PET_annual = 'potevap_ssp_v2_processed.nc'
ncFileName_Q_annual = 'disv2_processed.nc'
ncFileName_Rech_annual = 'qrv2_processed.nc'
ncFileName_RZ_annual = 'rootmoistv2_processed.nc'
ncFileName_C3sumNPP_annual = 'npp-c3sumv2_processed.nc'
ncFileName_C3winNPP_annual = 'npp-c3winv2_processed.nc'
ncFileName_C4othNPP_annual = 'npp-c4othv2_processed.nc'

ncFileName_PPT_interannualSD = 'Precipitation_interannualSD_v2_processed.nc'
ncFileName_PPT_interannualQ15 = 'Precipitation_interannualQ15_v2_processed.nc'
ncFileName_Q_interannualSD = 'streamflow_interannualSD_v2_processed.nc'
ncFileName_RZ_interannualSD = 'rootmoist_interannualSD_v2_processed.nc'
ncFileName_Rech_interannualSD = 'qr_interannualSD_v2_processed.nc'

ncFileName_PPT_seasonalSD = 'Precipitation_seasonalSD_v2_processed.nc'
ncFileName_PPT_seasonalQ15 = 'Precipitation_seasonalQ15_v2_processed.nc'
ncFileName_Q_seasonalSD = 'streamflow_seasonalSD_v2_processed.nc'
ncFileName_RZ_seasonalSD = 'rootmoist_seasonalSD_v2_processed.nc'

##############################################
# defining constants
pwp = 25
AI_humid = 0.65
diverg = 0.1
halfDegInKM = 111 / 2
kmToMm = 10^6
runoffRatio = 0.2
wPlant = 650
valueType = 1:6


################################################
# naming dimensions for creation of ncs - only needs to be read once

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

	decade = nc_decade
	dim(decade) = length(decade)
	metadata = list(decade = list(units = 'decades_of_21st_C'))
	attr(decade, 'variables') = metadata
	names(dim(decade)) = 'decade'

	rcpScen = nc_scen
	dim(rcpScen) = length(rcpScen)
	metadata = list(rcpScen = list(units = 'SSP_scenario'))
	attr(rcpScen, 'variables') = metadata
	names(dim(rcpScen)) = 'rcpScen'

	valueClass = 1:6#valueType
	dim(valueClass) = length(valueClass)
	metadata = list(valueClass = list(units = 'class'))
	attr(valueClass, 'variables') = metadata
	names(dim(valueClass)) = 'valueClass'
	#####################################################




#######################################################################################################################################
# 	ON-SITE SUPPLY
#######################################################################################################################################

##########################################################
	# Aridity Index
nc_ppt = nc_open(paste0(ncpath, ncFileName_PPT_annual))
nc_pet = nc_open(paste0(ncpath, ncFileName_PET_annual))
nc_lat = ncvar_get(nc_ppt, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(nc_ppt, 'lon')
nc_decade = ncvar_get(nc_ppt, 'decade')
nc_scen = ncvar_get(nc_ppt, 'rcpScen')
ppt = ncvar_get(nc_ppt, 'tcfdVariable')
pet = ncvar_get(nc_pet, 'tcfdVariable')

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(nc_decade) * length(nc_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(nc_decade), length(nc_scen), length(valueType)))


for(i in 1:length(nc_scen))	{
	for(j in 1:length(nc_decade))	{
		dataOutArray[ , , j, i, 1] = (ppt[ , , j, i, 1] / pet[ , , j, i, 1]) / AI_humid
		dataOutArray[ , , j, i, 5] = (ppt[ , , j, i, 5] / pet[ , , j, i, 5]) / AI_humid
		dataOutArray[ , , j, i, 6] = (ppt[ , , j, i, 6] / pet[ , , j, i, 6]) / AI_humid
	}
}

	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 2, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(nc_decade))	{
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

	# converting array to nc
tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Aridity Index - annual avg [-]'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncpath,'aridityIndex_ssp_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'aridityIndex_ssp_v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
nc_onesDat = nc_testDat	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] < 1] = 0	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] >= 1] = 1
image(nc_lon, rev(nc_lat), nc_onesDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,6] - nc_testDat[,,9,3,5])

nc_close(nc_ppt)
nc_close(nc_pet)
nc_close(myNC)







##########################################################
	# PWP exceedance
nc_rz = nc_open(paste0(ncpath, ncFileName_RZ_annual))
nc_lat = ncvar_get(nc_rz, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(nc_rz, 'lon')
nc_decade = ncvar_get(nc_rz, 'decade')
nc_scen = ncvar_get(nc_rz, 'rcpScen')
rzsm = ncvar_get(nc_rz, 'tcfdVariable')

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(nc_decade) * length(nc_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(nc_decade), length(nc_scen), length(valueType)))

for(i in 1:length(nc_scen))	{
	for(j in 1:length(nc_decade))	{
		dataOutArray[ , , j, i, 1] = rzsm[ , , j, i, 1] / pwp
		dataOutArray[ , , j, i, 5] = rzsm[ , , j, i, 5] / pwp
		dataOutArray[ , , j, i, 6] = rzsm[ , , j, i, 6] / pwp
	}
}

	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 3, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 3, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(nc_decade))	{
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

	# converting array to nc
tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'PWP Exceedance - annual avg [-]'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncpath,'pwpExceedance_ssp_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'pwpExceedance_ssp_v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
nc_onesDat = nc_testDat	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] < 1] = 0	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] >= 1] = 1
image(nc_lon, rev(nc_lat), nc_onesDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,6] - nc_testDat[,,9,3,5])

nc_close(nc_rz)
nc_close(myNC)






#######################################################################################################################################
# 	TRANSIENT ON-SITE SUPPLY
#######################################################################################################################################

##########################################################
	# Transient Aridity Index - Interannual
nc_ppt = nc_open(paste0(ncpath, ncFileName_PPT_annual))
nc_ppt_inter_sd = nc_open(paste0(ncpath, ncFileName_PPT_interannualSD))
#nc_ppt_inter_q15 = nc_open(paste0(ncpath, ncFileName_PPT_interannualQ15))
#nc_ppt_seasn_sd = nc_open(paste0(ncpath, ncFileName_PPT_seasonalSD))
#nc_ppt_seasn_q15 = nc_open(paste0(ncpath, ncFileName_PPT_seasonalQ15))
nc_pet = nc_open(paste0(ncpath, ncFileName_PET_annual))
nc_lat = ncvar_get(nc_ppt, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(nc_ppt, 'lon')
nc_decade = ncvar_get(nc_ppt, 'decade')
nc_scen = ncvar_get(nc_ppt, 'rcpScen')
ppt = ncvar_get(nc_ppt, 'tcfdVariable')
pet = ncvar_get(nc_pet, 'tcfdVariable')
ppt_inter_sd = ncvar_get(nc_ppt_inter_sd, 'tcfdVariable')
#ppt_inter_q15 = ncvar_get(nc_ppt_inter_q15, 'tcfdVariable')
#ppt_seasn_sd = ncvar_get(nc_ppt_seasn_sd, 'tcfdVariable')
#ppt_seasn_q15 = ncvar_get(nc_ppt_seasn_q15, 'tcfdVariable')

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(nc_decade) * length(nc_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(nc_decade), length(nc_scen), length(valueType)))


for(i in 1:length(nc_scen))	{
	for(j in 1:length(nc_decade))	{
		dataOutArray[ , , j, i, 1] = (ppt[ , , j, i, 5] / pet[ , , j, i, 1]) / AI_humid
		dataOutArray[ , , j, i, 5] = (ppt[ , , j, i, 5] / pet[ , , j, i, 5]) / AI_humid
		dataOutArray[ , , j, i, 6] = (ppt[ , , j, i, 5] / pet[ , , j, i, 6]) / AI_humid
#		dataOutArray[ , , j, i, 1] = ((ppt[ , , j, i, 1] - ppt_inter_sd[ , , j, i, 1]) / pet[ , , j, i, 1]) / AI_humid
#		dataOutArray[ , , j, i, 5] = ((ppt[ , , j, i, 5] - ppt_inter_sd[ , , j, i, 5]) / pet[ , , j, i, 5]) / AI_humid
#		dataOutArray[ , , j, i, 6] = ((ppt[ , , j, i, 6] - ppt_inter_sd[ , , j, i, 6]) / pet[ , , j, i, 6]) / AI_humid
	}
}

dataOutArray[ , , , , 1][dataOutArray[ , , , , 1] < 0] = 0
dataOutArray[ , , , , 5][dataOutArray[ , , , , 5] < 0] = 0
dataOutArray[ , , , , 6][dataOutArray[ , , , , 6] < 0] = 0
	
	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 3, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 3, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(nc_decade))	{
	dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] <= histQuants[j]]
	for(j in 1:(length(histQuants)))	{
		dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 2, 2][dataOutArray[ , , i, 2, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 3, 2][dataOutArray[ , , i, 3, 1] <= histQuants[j]] = j
	}
	dataOutArray[ , , i, 1, 2][maskedLocs26] = NA
	dataOutArray[ , , i, 2, 2][maskedLocs60] = NA
	dataOutArray[ , , i, 3, 2][maskedLocs85] = NA
}


	# converting array to nc
tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Aridity Index - transient interannual avg [-]'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncpath,'aridityIndex_transientInterannual_ssp_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'aridityIndex_transientInterannual_ssp_v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
nc_onesDat = nc_testDat	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] < 1] = 0	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] >= 1] = 1
image(nc_lon, rev(nc_lat), nc_onesDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,6] - nc_testDat[,,9,3,5])

nc_close(nc_ppt)
nc_close(nc_pet)
nc_close(nc_ppt_inter_sd)
nc_close(nc_ppt_inter_q15)
nc_close(myNC)










##########################################################
	# Transient Aridity Index - Seasonal
nc_ppt = nc_open(paste0(ncpath, ncFileName_PPT_annual))
#nc_ppt_inter_sd = nc_open(paste0(ncpath, ncFileName_PPT_interannualSD))
#nc_ppt_inter_q15 = nc_open(paste0(ncpath, ncFileName_PPT_interannualQ15))
#nc_ppt_seasn_sd = nc_open(paste0(ncpath, ncFileName_PPT_seasonalSD))
nc_ppt_seasn_q15 = nc_open(paste0(ncpath, ncFileName_PPT_seasonalQ15))
nc_pet = nc_open(paste0(ncpath, ncFileName_PET_annual))
nc_lat = ncvar_get(nc_ppt, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(nc_ppt, 'lon')
nc_decade = ncvar_get(nc_ppt, 'decade')
nc_scen = ncvar_get(nc_ppt, 'rcpScen')
ppt = ncvar_get(nc_ppt, 'tcfdVariable')
pet = ncvar_get(nc_pet, 'tcfdVariable')
#ppt_inter_sd = ncvar_get(nc_ppt_inter_sd, 'tcfdVariable')
#ppt_inter_q15 = ncvar_get(nc_ppt_inter_q15, 'tcfdVariable')
#ppt_seasn_sd = ncvar_get(nc_ppt_seasn_sd, 'tcfdVariable')
ppt_seasn_q15 = ncvar_get(nc_ppt_seasn_q15, 'tcfdVariable')

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(nc_decade) * length(nc_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(nc_decade), length(nc_scen), length(valueType)))

####!!!!!!!!!!!!!!!!
## switching from SD of seasonal to quantiles
####!!!!!!!!!!!!!!!!

for(i in 1:length(nc_scen))	{
	for(j in 1:length(nc_decade))	{
#		dataOutArray[ , , j, i, 1] = (ppt[ , , j, i, 1] - ppt_seasn_sd[ , , j, i, 1]) / pet[ , , j, i, 1]
#		dataOutArray[ , , j, i, 5] = (ppt[ , , j, i, 5] - ppt_seasn_sd[ , , j, i, 5]) / pet[ , , j, i, 5]
#		dataOutArray[ , , j, i, 6] = (ppt[ , , j, i, 6] - ppt_seasn_sd[ , , j, i, 6]) / pet[ , , j, i, 6]
		dataOutArray[ , , j, i, 1] = (ppt_seasn_q15[ , , j, i, 1] / pet[ , , j, i, 1]) / AI_humid
		dataOutArray[ , , j, i, 5] = (ppt_seasn_q15[ , , j, i, 5] / pet[ , , j, i, 5]) / AI_humid
		dataOutArray[ , , j, i, 6] = (ppt_seasn_q15[ , , j, i, 6] / pet[ , , j, i, 6]) / AI_humid
	}
}

dataOutArray[ , , , , 1][dataOutArray[ , , , , 1] < 0] = 0
dataOutArray[ , , , , 5][dataOutArray[ , , , , 5] < 0] = 0
dataOutArray[ , , , , 6][dataOutArray[ , , , , 6] < 0] = 0

	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 2, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(nc_decade))	{
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

	# converting array to nc
tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Aridity Index - transient seasonal avg [-]'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncpath,'aridityIndex_transientSeasonal_ssp_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'aridityIndex_transientSeasonal_ssp_v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
nc_onesDat = nc_testDat	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] < 1] = 0	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] >= 1] = 1
image(nc_lon, rev(nc_lat), nc_onesDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,6] - nc_testDat[,,9,3,5])

nc_close(nc_ppt)
nc_close(nc_pet)
nc_close(nc_ppt_seasn_sd)
nc_close(nc_ppt_seasn_q15)
nc_close(myNC)











##########################################################
	# Transient RZsm Index - Interannual

#nc_ppt = nc_open(paste0(ncpath, ncFileName_PPT_annual))
#nc_ppt_inter_sd = nc_open(paste0(ncpath, ncFileName_PPT_interannualSD))
#nc_ppt_inter_q15 = nc_open(paste0(ncpath, ncFileName_PPT_interannualQ15))
#nc_ppt_seasn_sd = nc_open(paste0(ncpath, ncFileName_PPT_seasonalSD))
#nc_ppt_seasn_q15 = nc_open(paste0(ncpath, ncFileName_PPT_seasonalQ15))
#nc_pet = nc_open(paste0(ncpath, ncFileName_PET_annual))
nc_rz = nc_open(paste0(ncpath, ncFileName_RZ_annual))
nc_rz_inter_sd = nc_open(paste0(ncpath, ncFileName_RZ_interannualSD))
#nc_rz_seasn_sd = nc_open(paste0(ncpath, ncFileName_RZ_seasonalSD))
nc_lat = ncvar_get(nc_ppt, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(nc_ppt, 'lon')
nc_decade = ncvar_get(nc_ppt, 'decade')
nc_scen = ncvar_get(nc_ppt, 'rcpScen')
#ppt = ncvar_get(nc_ppt, 'tcfdVariable')
#pet = ncvar_get(nc_pet, 'tcfdVariable')
rzsm = ncvar_get(nc_rz, 'tcfdVariable')
#ppt_inter_sd = ncvar_get(nc_ppt_inter_sd, 'tcfdVariable')
#ppt_inter_q15 = ncvar_get(nc_ppt_inter_q15, 'tcfdVariable')
rzsm_inter_sd = ncvar_get(nc_rz_inter_sd, 'tcfdVariable')
#ppt_seasn_sd = ncvar_get(nc_ppt_seasn_sd, 'tcfdVariable')
#ppt_seasn_q15 = ncvar_get(nc_ppt_seasn_q15, 'tcfdVariable')
#rzsm_seasn_sd = ncvar_get(nc_rz_seasn_sd, 'tcfdVariable')


	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(nc_decade) * length(nc_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(nc_decade), length(nc_scen), length(valueType)))


for(i in 1:length(nc_scen))	{
	for(j in 1:length(nc_decade))	{
		dataOutArray[ , , j, i, 1] = (rzsm[ , , j, i, 1] - rzsm_inter_sd[ , , j, i, 1]) / pwp
		dataOutArray[ , , j, i, 5] = (rzsm[ , , j, i, 5] - rzsm_inter_sd[ , , j, i, 5]) / pwp
		dataOutArray[ , , j, i, 6] = (rzsm[ , , j, i, 6] - rzsm_inter_sd[ , , j, i, 6]) / pwp
	}
}

	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 3, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 3, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(nc_decade))	{
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

	# converting array to nc
tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'PWP Exceedance - transient interannual avg [-]'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncpath,'pwpExceedance_transientInterannual_ssp_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'pwpExceedance_transientInterannual_ssp_v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
nc_onesDat = nc_testDat	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] < 1] = 0	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] >= 1] = 1
image(nc_lon, rev(nc_lat), nc_onesDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,6] - nc_testDat[,,9,3,5])

nc_close(myNC)
nc_close(nc_rz)
nc_close(nc_rz_inter_sd)








##########################################################
	# Transient RZsm Index - Seasonal

#nc_ppt = nc_open(paste0(ncpath, ncFileName_PPT_annual))
#nc_ppt_inter_sd = nc_open(paste0(ncpath, ncFileName_PPT_interannualSD))
#nc_ppt_inter_q15 = nc_open(paste0(ncpath, ncFileName_PPT_interannualQ15))
#nc_ppt_seasn_sd = nc_open(paste0(ncpath, ncFileName_PPT_seasonalSD))
#nc_ppt_seasn_q15 = nc_open(paste0(ncpath, ncFileName_PPT_seasonalQ15))
#nc_pet = nc_open(paste0(ncpath, ncFileName_PET_annual))
nc_rz = nc_open(paste0(ncpath, ncFileName_RZ_annual))
#nc_rz_inter_sd = nc_open(paste0(ncpath, ncFileName_RZ_interannualSD))
nc_rz_seasn_sd = nc_open(paste0(ncpath, ncFileName_RZ_seasonalSD))
nc_lat = ncvar_get(nc_ppt, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(nc_ppt, 'lon')
nc_decade = ncvar_get(nc_ppt, 'decade')
nc_scen = ncvar_get(nc_ppt, 'rcpScen')
#ppt = ncvar_get(nc_ppt, 'tcfdVariable')
#pet = ncvar_get(nc_pet, 'tcfdVariable')
rzsm = ncvar_get(nc_rz, 'tcfdVariable')
#ppt_inter_sd = ncvar_get(nc_ppt_inter_sd, 'tcfdVariable')
#ppt_inter_q15 = ncvar_get(nc_ppt_inter_q15, 'tcfdVariable')
#rzsm_inter_sd = ncvar_get(nc_rz_inter_sd, 'tcfdVariable')
#ppt_seasn_sd = ncvar_get(nc_ppt_seasn_sd, 'tcfdVariable')
#ppt_seasn_q15 = ncvar_get(nc_ppt_seasn_q15, 'tcfdVariable')
rzsm_seasn_sd = ncvar_get(nc_rz_seasn_sd, 'tcfdVariable')


	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(nc_decade) * length(nc_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(nc_decade), length(nc_scen), length(valueType)))

pwp = 25
for(i in 1:length(nc_scen))	{
	for(j in 1:length(nc_decade))	{
		dataOutArray[ , , j, i, 1] = (rzsm[ , , j, i, 1] - rzsm_seasn_sd[ , , j, i, 1]) / pwp
		dataOutArray[ , , j, i, 5] = (rzsm[ , , j, i, 5] - rzsm_seasn_sd[ , , j, i, 5]) / pwp
		dataOutArray[ , , j, i, 6] = (rzsm[ , , j, i, 6] - rzsm_seasn_sd[ , , j, i, 6]) / pwp
	}
}

	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 3, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 3, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(nc_decade))	{
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

	# converting array to nc
tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'PWP Exceedance - transient seasonal avg [-]'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncpath,'pwpExceedance_transientSeasonal_ssp_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'pwpExceedance_transientSeasonal_ssp_v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
nc_onesDat = nc_testDat	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] < 1] = 0	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] >= 1] = 1
image(nc_lon, rev(nc_lat), nc_onesDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,6] - nc_testDat[,,9,3,5])

nc_close(myNC)
nc_close(nc_rz)
nc_close(nc_rz_inter_sd)













#######################################################################################################################################
# 	REGIONAL SUPPLY	
#######################################################################################################################################

##########################################################
	# Transient Aridity Index - Interannual
nc_ppt = nc_open(paste0(ncpath, ncFileName_PPT_annual))
nc_ppt_inter_sd = nc_open(paste0(ncpath, ncFileName_PPT_interannualSD))
#nc_ppt_inter_q15 = nc_open(paste0(ncpath, ncFileName_PPT_interannualQ15))
#nc_ppt_seasn_sd = nc_open(paste0(ncpath, ncFileName_PPT_seasonalSD))
#nc_ppt_seasn_q15 = nc_open(paste0(ncpath, ncFileName_PPT_seasonalQ15))
nc_pet = nc_open(paste0(ncpath, ncFileName_PET_annual))
nc_rech = nc_open(paste0(ncpath, ncFileName_Rech_annual))
nc_Q = nc_open(paste0(ncpath, ncFileName_Q_annual))
nc_lat = ncvar_get(nc_ppt, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(nc_ppt, 'lon')
nc_decade = ncvar_get(nc_ppt, 'decade')
nc_scen = ncvar_get(nc_ppt, 'rcpScen')
ppt = ncvar_get(nc_ppt, 'tcfdVariable')
pet = ncvar_get(nc_pet, 'tcfdVariable')
ppt_inter_sd = ncvar_get(nc_ppt_inter_sd, 'tcfdVariable')
#ppt_inter_q15 = ncvar_get(nc_ppt_inter_q15, 'tcfdVariable')
#ppt_seasn_sd = ncvar_get(nc_ppt_seasn_sd, 'tcfdVariable')
#ppt_seasn_q15 = ncvar_get(nc_ppt_seasn_q15, 'tcfdVariable')
rech = ncvar_get(nc_rech, 'tcfdVariable')
Q = ncvar_get(nc_Q, 'tcfdVariable')

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(nc_decade) * length(nc_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(nc_decade), length(nc_scen), length(valueType)))


for(i in 1:length(nc_scen))	{
	for(k in 1:length(nc_lat))	{
		thisLat = abs(nc_lat[k])
		gridArea = halfDegInKM * cos(thisLat * pi / 180) * halfDegInKM 
		
		for(j in 1:length(nc_decade))	{
			dataOutArray[ , k, j, i, 1] = ((ppt[ , k, j, i, 5] + rech[ , k, j, i, 1] + diverg * kmToMm * Q[ , k, j, i, 1] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
			dataOutArray[ , k, j, i, 5] = ((ppt[ , k, j, i, 5] + rech[ , k, j, i, 1] + diverg * kmToMm * Q[ , k, j, i, 5] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
			dataOutArray[ , k, j, i, 6] = ((ppt[ , k, j, i, 5] + rech[ , k, j, i, 1] + diverg * kmToMm * Q[ , k, j, i, 6] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
#			dataOutArray[ , k, j, i, 1] = ((ppt[ , k, j, i, 1] - ppt_inter_sd[ , k, j, i, 1] + rech[ , k, j, i, 1] + diverg * kmToMm * Q[ , k, j, i, 1] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
#			dataOutArray[ , k, j, i, 5] = ((ppt[ , k, j, i, 5] - ppt_inter_sd[ , k, j, i, 5] + rech[ , k, j, i, 5] + diverg * kmToMm * Q[ , k, j, i, 5] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
#			dataOutArray[ , k, j, i, 6] = ((ppt[ , k, j, i, 6] - ppt_inter_sd[ , k, j, i, 6] + rech[ , k, j, i, 6] + diverg * kmToMm * Q[ , k, j, i, 6] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
		}
	}
}

dataOutArray[ , , , , 1][dataOutArray[ , , , , 1] < 0] = 0
dataOutArray[ , , , , 5][dataOutArray[ , , , , 5] < 0] = 0
dataOutArray[ , , , , 6][dataOutArray[ , , , , 6] < 0] = 0
	
	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 3, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 3, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(nc_decade))	{
	dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] <= histQuants[j]]
	for(j in 1:(length(histQuants)))	{
		dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 2, 2][dataOutArray[ , , i, 2, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 3, 2][dataOutArray[ , , i, 3, 1] <= histQuants[j]] = j
	}
	dataOutArray[ , , i, 1, 2][maskedLocs26] = NA
	dataOutArray[ , , i, 2, 2][maskedLocs60] = NA
	dataOutArray[ , , i, 3, 2][maskedLocs85] = NA
}


	# converting array to nc
tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Aridity Index - transient interannual supplemented by regional avg [-]'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncpath,'aridityIndex_transientInterannualRegionalAvgSupplement_ssp_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'aridityIndex_transientInterannualRegionalAvgSupplement_ssp_v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
nc_onesDat = nc_testDat	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] < 1] = 0	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] >= 1] = 1
image(nc_lon, rev(nc_lat), nc_onesDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,6] - nc_testDat[,,9,3,5])

nc_close(nc_ppt)
nc_close(nc_pet)
nc_close(nc_Q)
nc_close(nc_rech)
nc_close(nc_ppt_inter_sd)
nc_close(nc_ppt_inter_q15)
nc_close(myNC)










##########################################################
	# Transient Aridity Index - Seasonal
nc_ppt = nc_open(paste0(ncpath, ncFileName_PPT_annual))
#nc_ppt_inter_sd = nc_open(paste0(ncpath, ncFileName_PPT_interannualSD))
nc_ppt_inter_q15 = nc_open(paste0(ncpath, ncFileName_PPT_interannualQ15))
#nc_ppt_seasn_sd = nc_open(paste0(ncpath, ncFileName_PPT_seasonalSD))
#nc_ppt_seasn_q15 = nc_open(paste0(ncpath, ncFileName_PPT_seasonalQ15))
nc_pet = nc_open(paste0(ncpath, ncFileName_PET_annual))
nc_rech = nc_open(paste0(ncpath, ncFileName_Rech_annual))
nc_Q = nc_open(paste0(ncpath, ncFileName_Q_annual))
nc_lat = ncvar_get(nc_ppt, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(nc_ppt, 'lon')
nc_decade = ncvar_get(nc_ppt, 'decade')
nc_scen = ncvar_get(nc_ppt, 'rcpScen')
ppt = ncvar_get(nc_ppt, 'tcfdVariable')
pet = ncvar_get(nc_pet, 'tcfdVariable')
#ppt_inter_sd = ncvar_get(nc_ppt_inter_sd, 'tcfdVariable')
#ppt_inter_q15 = ncvar_get(nc_ppt_inter_q15, 'tcfdVariable')
ppt_seasn_sd = ncvar_get(nc_ppt_seasn_sd, 'tcfdVariable')
ppt_seasn_q15 = ncvar_get(nc_ppt_seasn_q15, 'tcfdVariable')
rech = ncvar_get(nc_rech, 'tcfdVariable')
Q = ncvar_get(nc_Q, 'tcfdVariable')

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(nc_decade) * length(nc_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(nc_decade), length(nc_scen), length(valueType)))

####!!!!!!!!!!!!!!!!
## switching from SD of seasonal to quantiles
####!!!!!!!!!!!!!!!!

for(i in 1:length(nc_scen))	{
	for(k in 1:length(nc_lat))	{
		thisLat = abs(nc_lat[k])
		gridArea = halfDegInKM * cos(thisLat * pi / 180) * halfDegInKM 
		
		for(j in 1:length(nc_decade))	{
#			dataOutArray[ , k, j, i, 1] = ((ppt[ , k, j, i, 1] - ppt_inter_sd[ , k, j, i, 1] + rech[ , k, j, i, 1] + diverg * kmToMm * Q[ , k, j, i, 1] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
#			dataOutArray[ , k, j, i, 5] = ((ppt[ , k, j, i, 5] - ppt_inter_sd[ , k, j, i, 5] + rech[ , k, j, i, 5] + diverg * kmToMm * Q[ , k, j, i, 5] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
#			dataOutArray[ , k, j, i, 6] = ((ppt[ , k, j, i, 6] - ppt_inter_sd[ , k, j, i, 6] + rech[ , k, j, i, 6] + diverg * kmToMm * Q[ , k, j, i, 6] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
			dataOutArray[ , , j, i, 1] = (ppt_seasn_q15[ , , j, i, 1] + rech[ , k, j, i, 1] + diverg * kmToMm * Q[ , k, j, i, 1] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
			dataOutArray[ , , j, i, 5] = (ppt_seasn_q15[ , , j, i, 5] + rech[ , k, j, i, 5] + diverg * kmToMm * Q[ , k, j, i, 5] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
			dataOutArray[ , , j, i, 6] = (ppt_seasn_q15[ , , j, i, 6] + rech[ , k, j, i, 6] + diverg * kmToMm * Q[ , k, j, i, 6] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
		}
	}
}

dataOutArray[ , , , , 1][dataOutArray[ , , , , 1] < 0] = 0
dataOutArray[ , , , , 5][dataOutArray[ , , , , 5] < 0] = 0
dataOutArray[ , , , , 6][dataOutArray[ , , , , 6] < 0] = 0

	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 2, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(nc_decade))	{
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

	# converting array to nc
tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Aridity Index - transient seasonal supplemented by regional avg [-]'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncpath,'aridityIndex_transientSeasonalRegionalAvgSupplement_ssp_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'aridityIndex_transientSeasonalRegionalAvgSupplement_ssp_v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,6] - nc_testDat[,,9,3,5])

nc_close(nc_ppt)
nc_close(nc_pet)
nc_close(nc_Q)
nc_close(nc_rech)
nc_close(nc_ppt_seasn_sd)
nc_close(nc_ppt_seasn_q15)
nc_close(myNC)

















##########################################################
	# Transient Plant Water Need - Interannual
nc_ppt = nc_open(paste0(ncpath, ncFileName_PPT_annual))
nc_ppt_inter_sd = nc_open(paste0(ncpath, ncFileName_PPT_interannualSD))
#nc_ppt_inter_q15 = nc_open(paste0(ncpath, ncFileName_PPT_interannualQ15))
#nc_ppt_seasn_sd = nc_open(paste0(ncpath, ncFileName_PPT_seasonalSD))
#nc_ppt_seasn_q15 = nc_open(paste0(ncpath, ncFileName_PPT_seasonalQ15))
nc_pet = nc_open(paste0(ncpath, ncFileName_PET_annual))
nc_rech = nc_open(paste0(ncpath, ncFileName_Rech_annual))
nc_Q = nc_open(paste0(ncpath, ncFileName_Q_annual))
nc_lat = ncvar_get(nc_ppt, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(nc_ppt, 'lon')
nc_decade = ncvar_get(nc_ppt, 'decade')
nc_scen = ncvar_get(nc_ppt, 'rcpScen')
ppt = ncvar_get(nc_ppt, 'tcfdVariable')
pet = ncvar_get(nc_pet, 'tcfdVariable')
ppt_inter_sd = ncvar_get(nc_ppt_inter_sd, 'tcfdVariable')
#ppt_inter_q15 = ncvar_get(nc_ppt_inter_q15, 'tcfdVariable')
#ppt_seasn_sd = ncvar_get(nc_ppt_seasn_sd, 'tcfdVariable')
#ppt_seasn_q15 = ncvar_get(nc_ppt_seasn_q15, 'tcfdVariable')
rech = ncvar_get(nc_rech, 'tcfdVariable')
Q = ncvar_get(nc_Q, 'tcfdVariable')

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(nc_decade) * length(nc_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(nc_decade), length(nc_scen), length(valueType)))


for(i in 1:length(nc_scen))	{
	for(k in 1:length(nc_lat))	{
		thisLat = abs(nc_lat[k])
		gridArea = halfDegInKM * cos(thisLat * pi / 180) * halfDegInKM 
		
		for(j in 1:length(nc_decade))	{
			dataOutArray[ , k, j, i, 1] = ((1 - runoffRatio) * (ppt[ , k, j, i, 1] - ppt_inter_sd[ , k, j, i, 1]) + rech[ , k, j, i, 1] + (diverg * kmToMm * Q[ , k, j, i, 1] / gridArea)) /  wPlant
			dataOutArray[ , k, j, i, 5] = ((1 - runoffRatio) * (ppt[ , k, j, i, 5] - ppt_inter_sd[ , k, j, i, 5]) + rech[ , k, j, i, 5] + (diverg * kmToMm * Q[ , k, j, i, 1] / gridArea)) /  wPlant
			dataOutArray[ , k, j, i, 6] = ((1 - runoffRatio) * (ppt[ , k, j, i, 6] - ppt_inter_sd[ , k, j, i, 6]) + rech[ , k, j, i, 6] + (diverg * kmToMm * Q[ , k, j, i, 1] / gridArea)) /  wPlant
		}
	}
}

dataOutArray[ , , , , 1][dataOutArray[ , , , , 1] < 0] = 0
dataOutArray[ , , , , 5][dataOutArray[ , , , , 5] < 0] = 0
dataOutArray[ , , , , 6][dataOutArray[ , , , , 6] < 0] = 0
	
	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 3, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 3, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(nc_decade))	{
	dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] <= histQuants[j]]
	for(j in 1:(length(histQuants)))	{
		dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 2, 2][dataOutArray[ , , i, 2, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 3, 2][dataOutArray[ , , i, 3, 1] <= histQuants[j]] = j
	}
	dataOutArray[ , , i, 1, 2][maskedLocs26] = NA
	dataOutArray[ , , i, 2, 2][maskedLocs60] = NA
	dataOutArray[ , , i, 3, 2][maskedLocs85] = NA
}


	# converting array to nc
tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Plant Water Demand - transient interannual supplemented by regional avg [-]'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncpath,'plantWaterDemand_transientInterannualRegionalAvgSupplement_ssp_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'plantWaterDemand_transientInterannualRegionalAvgSupplement_ssp_v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
nc_onesDat = nc_testDat	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] < 1] = 0	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] >= 1] = 1
image(nc_lon, rev(nc_lat), nc_onesDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,6] - nc_testDat[,,9,3,5])

nc_close(nc_ppt)
nc_close(nc_pet)
nc_close(nc_Q)
nc_close(nc_rech)
nc_close(nc_ppt_inter_sd)
nc_close(nc_ppt_inter_q15)
nc_close(myNC)










##########################################################
	# Transient Plant Water Need - Seasonal
nc_ppt = nc_open(paste0(ncpath, ncFileName_PPT_annual))
#nc_ppt_inter_sd = nc_open(paste0(ncpath, ncFileName_PPT_interannualSD))
nc_ppt_inter_q15 = nc_open(paste0(ncpath, ncFileName_PPT_interannualQ15))
#nc_ppt_seasn_sd = nc_open(paste0(ncpath, ncFileName_PPT_seasonalSD))
#nc_ppt_seasn_q15 = nc_open(paste0(ncpath, ncFileName_PPT_seasonalQ15))
nc_pet = nc_open(paste0(ncpath, ncFileName_PET_annual))
nc_rech = nc_open(paste0(ncpath, ncFileName_Rech_annual))
nc_Q = nc_open(paste0(ncpath, ncFileName_Q_annual))
nc_lat = ncvar_get(nc_ppt, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(nc_ppt, 'lon')
nc_decade = ncvar_get(nc_ppt, 'decade')
nc_scen = ncvar_get(nc_ppt, 'rcpScen')
ppt = ncvar_get(nc_ppt, 'tcfdVariable')
pet = ncvar_get(nc_pet, 'tcfdVariable')
#ppt_inter_sd = ncvar_get(nc_ppt_inter_sd, 'tcfdVariable')
#ppt_inter_q15 = ncvar_get(nc_ppt_inter_q15, 'tcfdVariable')
ppt_seasn_sd = ncvar_get(nc_ppt_seasn_sd, 'tcfdVariable')
ppt_seasn_q15 = ncvar_get(nc_ppt_seasn_q15, 'tcfdVariable')
rech = ncvar_get(nc_rech, 'tcfdVariable')
Q = ncvar_get(nc_Q, 'tcfdVariable')

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(nc_decade) * length(nc_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(nc_decade), length(nc_scen), length(valueType)))

####!!!!!!!!!!!!!!!!
## switching from SD of seasonal to quantiles
####!!!!!!!!!!!!!!!!

for(i in 1:length(nc_scen))	{
	for(k in 1:length(nc_lat))	{
		thisLat = abs(nc_lat[k])
		gridArea = halfDegInKM * cos(thisLat * pi / 180) * halfDegInKM 
		
		for(j in 1:length(nc_decade))	{
#			dataOutArray[ , k, j, i, 1] = ((1 - runoffRatio) * (ppt[ , k, j, i, 1] - ppt_inter_sd[ , k, j, i, 1]) + rech[ , k, j, i, 1] + (diverg * kmToMm * Q[ , k, j, i, 1] / gridArea)) /  wPlant
#			dataOutArray[ , k, j, i, 5] = ((1 - runoffRatio) * (ppt[ , k, j, i, 5] - ppt_inter_sd[ , k, j, i, 5]) + rech[ , k, j, i, 5] + (diverg * kmToMm * Q[ , k, j, i, 1] / gridArea)) /  wPlant
#			dataOutArray[ , k, j, i, 6] = ((1 - runoffRatio) * (ppt[ , k, j, i, 6] - ppt_inter_sd[ , k, j, i, 6]) + rech[ , k, j, i, 6] + (diverg * kmToMm * Q[ , k, j, i, 1] / gridArea)) /  wPlant
			dataOutArray[ , k, j, i, 1] = ((1 - runoffRatio) * ppt_seasn_q15[ , , j, i, 1] + rech[ , k, j, i, 1] + (diverg * kmToMm * Q[ , k, j, i, 1] / gridArea)) /  wPlant
			dataOutArray[ , k, j, i, 5] = ((1 - runoffRatio) * ppt_seasn_q15[ , , j, i, 5] + rech[ , k, j, i, 5] + (diverg * kmToMm * Q[ , k, j, i, 1] / gridArea)) /  wPlant
			dataOutArray[ , k, j, i, 6] = ((1 - runoffRatio) * ppt_seasn_q15[ , , j, i, 6] + rech[ , k, j, i, 6] + (diverg * kmToMm * Q[ , k, j, i, 1] / gridArea)) /  wPlant
			}
	}
}

dataOutArray[ , , , , 1][dataOutArray[ , , , , 1] < 0] = 0
dataOutArray[ , , , , 5][dataOutArray[ , , , , 5] < 0] = 0
dataOutArray[ , , , , 6][dataOutArray[ , , , , 6] < 0] = 0

	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 2, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(nc_decade))	{
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

	# converting array to nc
tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Plant Water Demand - transient seasonal supplemented by regional avg [-]'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncpath,'plantWaterDemand_transientSeasonalRegionalAvgSupplement_ssp_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'plantWaterDemand_transientSeasonalRegionalAvgSupplement_ssp_v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,6] - nc_testDat[,,9,3,5])

nc_close(nc_ppt)
nc_close(nc_pet)
nc_close(nc_Q)
nc_close(nc_rech)
nc_close(nc_ppt_seasn_sd)
nc_close(nc_ppt_seasn_q15)
nc_close(myNC)












#######################################################################################################################################
#   TRANSIENT REGIONAL SUPPLY	
#######################################################################################################################################

##########################################################
	# Transient Aridity Index - Interannual
nc_ppt = nc_open(paste0(ncpath, ncFileName_PPT_annual))
nc_ppt_inter_sd = nc_open(paste0(ncpath, ncFileName_PPT_interannualSD))
#nc_ppt_inter_q15 = nc_open(paste0(ncpath, ncFileName_PPT_interannualQ15))
#nc_ppt_seasn_sd = nc_open(paste0(ncpath, ncFileName_PPT_seasonalSD))
#nc_ppt_seasn_q15 = nc_open(paste0(ncpath, ncFileName_PPT_seasonalQ15))
nc_pet = nc_open(paste0(ncpath, ncFileName_PET_annual))
nc_rech = nc_open(paste0(ncpath, ncFileName_Rech_annual))
nc_Q = nc_open(paste0(ncpath, ncFileName_Q_annual))
nc_Q_inter_sd = nc_open(paste0(ncpath, ncFileName_Q_interannualSD))
#nc_Q_seasn_sd = nc_open(paste0(ncpath, ncFileName_Q_seasonalSD))
nc_lat = ncvar_get(nc_ppt, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(nc_ppt, 'lon')
nc_decade = ncvar_get(nc_ppt, 'decade')
nc_scen = ncvar_get(nc_ppt, 'rcpScen')
ppt = ncvar_get(nc_ppt, 'tcfdVariable')
pet = ncvar_get(nc_pet, 'tcfdVariable')
ppt_inter_sd = ncvar_get(nc_ppt_inter_sd, 'tcfdVariable')
#ppt_inter_q15 = ncvar_get(nc_ppt_inter_q15, 'tcfdVariable')
#ppt_seasn_sd = ncvar_get(nc_ppt_seasn_sd, 'tcfdVariable')
#ppt_seasn_q15 = ncvar_get(nc_ppt_seasn_q15, 'tcfdVariable')
rech = ncvar_get(nc_rech, 'tcfdVariable')
Q = ncvar_get(nc_Q, 'tcfdVariable')
Q_inter_sd = ncvar_get(nc_Q_inter_sd, 'tcfdVariable')
#Q_seasn_sd = ncvar_get(nc_Q_season_sd, 'tcfdVariable')


	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(nc_decade) * length(nc_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(nc_decade), length(nc_scen), length(valueType)))


for(i in 1:length(nc_scen))	{
	for(k in 1:length(nc_lat))	{
		thisLat = abs(nc_lat[k])
		gridArea = halfDegInKM * cos(thisLat * pi / 180) * halfDegInKM 
		
		for(j in 1:length(nc_decade))	{
			dataOutArray[ , k, j, i, 1] = ((ppt[ , k, j, i, 1] - ppt_inter_sd[ , k, j, i, 1] + rech[ , k, j, i, 1] + diverg * kmToMm * (Q[ , k, j, i, 1] - Q_inter_sd[ , k, j, i, 1]) / gridArea) / pet[ , k, j, i, 1]) / AI_humid
			dataOutArray[ , k, j, i, 5] = ((ppt[ , k, j, i, 5] - ppt_inter_sd[ , k, j, i, 5] + rech[ , k, j, i, 5] + diverg * kmToMm * (Q[ , k, j, i, 5] - Q_inter_sd[ , k, j, i, 5]) / gridArea) / pet[ , k, j, i, 1]) / AI_humid
			dataOutArray[ , k, j, i, 6] = ((ppt[ , k, j, i, 6] - ppt_inter_sd[ , k, j, i, 6] + rech[ , k, j, i, 6] + diverg * kmToMm * (Q[ , k, j, i, 6] - Q_inter_sd[ , k, j, i, 6]) / gridArea) / pet[ , k, j, i, 1]) / AI_humid
		}
	}
}

dataOutArray[ , , , , 1][dataOutArray[ , , , , 1] < 0] = 0
dataOutArray[ , , , , 5][dataOutArray[ , , , , 5] < 0] = 0
dataOutArray[ , , , , 6][dataOutArray[ , , , , 6] < 0] = 0
	
	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 3, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 3, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(nc_decade))	{
	dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] <= histQuants[j]]
	for(j in 1:(length(histQuants)))	{
		dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 2, 2][dataOutArray[ , , i, 2, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 3, 2][dataOutArray[ , , i, 3, 1] <= histQuants[j]] = j
	}
	dataOutArray[ , , i, 1, 2][maskedLocs26] = NA
	dataOutArray[ , , i, 2, 2][maskedLocs60] = NA
	dataOutArray[ , , i, 3, 2][maskedLocs85] = NA
}


	# converting array to nc
tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Aridity Index - transient interannual supplemented by regional avg [-]'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncpath,'aridityIndex_transientInterannualRegionalTransientInterannualSupplement_ssp_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'aridityIndex_transientInterannualRegionalTransientInterannualSupplement_ssp_v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
nc_onesDat = nc_testDat	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] < 1] = 0	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] >= 1] = 1
image(nc_lon, rev(nc_lat), nc_onesDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,1] - nc_testDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,6] - nc_testDat[,,9,3,5])

nc_close(nc_ppt)
nc_close(nc_pet)
nc_close(nc_Q)
nc_close(nc_rech)
nc_close(nc_ppt_inter_sd)
nc_close(nc_ppt_inter_q15)
nc_close(nc_Q_inter_sd)
nc_close(myNC)










##########################################################
	# Transient Aridity Index - Seasonal
	# Transient Aridity Index - Interannual
nc_ppt = nc_open(paste0(ncpath, ncFileName_PPT_annual))
#nc_ppt_inter_sd = nc_open(paste0(ncpath, ncFileName_PPT_interannualSD))
#nc_ppt_inter_q15 = nc_open(paste0(ncpath, ncFileName_PPT_interannualQ15))
#nc_ppt_seasn_sd = nc_open(paste0(ncpath, ncFileName_PPT_seasonalSD))
nc_ppt_seasn_q15 = nc_open(paste0(ncpath, ncFileName_PPT_seasonalQ15))
nc_pet = nc_open(paste0(ncpath, ncFileName_PET_annual))
nc_rech = nc_open(paste0(ncpath, ncFileName_Rech_annual))
nc_Q = nc_open(paste0(ncpath, ncFileName_Q_annual))
#nc_Q_inter_sd = nc_open(paste0(ncpath, ncFileName_Q_interannualSD))
nc_Q_seasn_sd = nc_open(paste0(ncpath, ncFileName_Q_seasonalSD))
nc_lat = ncvar_get(nc_ppt, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(nc_ppt, 'lon')
nc_decade = ncvar_get(nc_ppt, 'decade')
nc_scen = ncvar_get(nc_ppt, 'rcpScen')
ppt = ncvar_get(nc_ppt, 'tcfdVariable')
pet = ncvar_get(nc_pet, 'tcfdVariable')
#ppt_inter_sd = ncvar_get(nc_ppt_inter_sd, 'tcfdVariable')
#ppt_inter_q15 = ncvar_get(nc_ppt_inter_q15, 'tcfdVariable')
#ppt_seasn_sd = ncvar_get(nc_ppt_seasn_sd, 'tcfdVariable')
ppt_seasn_q15 = ncvar_get(nc_ppt_seasn_q15, 'tcfdVariable')
rech = ncvar_get(nc_rech, 'tcfdVariable')
Q = ncvar_get(nc_Q, 'tcfdVariable')
#Q_inter_sd = ncvar_get(nc_Q_inter_sd, 'tcfdVariable')
Q_seasn_sd = ncvar_get(nc_Q_season_sd, 'tcfdVariable')

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(nc_decade) * length(nc_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(nc_decade), length(nc_scen), length(valueType)))

####!!!!!!!!!!!!!!!!
## switching from SD of seasonal to quantiles
####!!!!!!!!!!!!!!!!

for(i in 1:length(nc_scen))	{
	for(k in 1:length(nc_lat))	{
		thisLat = abs(nc_lat[k])
		gridArea = halfDegInKM * cos(thisLat * pi / 180) * halfDegInKM 
		
		for(j in 1:length(nc_decade))	{
#			dataOutArray[ , k, j, i, 1] = ((ppt[ , k, j, i, 1] - ppt_inter_sd[ , k, j, i, 1] + rech[ , k, j, i, 1] + diverg * kmToMm * (Q[ , k, j, i, 1] - Q_seasn_sd[ , k, j, i, 1]) / gridArea) / pet[ , k, j, i, 1]) / AI_humid
#			dataOutArray[ , k, j, i, 5] = ((ppt[ , k, j, i, 5] - ppt_inter_sd[ , k, j, i, 5] + rech[ , k, j, i, 5] + diverg * kmToMm * (Q[ , k, j, i, 5] - Q_seasn_sd[ , k, j, i, 1]) / gridArea) / pet[ , k, j, i, 1]) / AI_humid
#			dataOutArray[ , k, j, i, 6] = ((ppt[ , k, j, i, 6] - ppt_inter_sd[ , k, j, i, 6] + rech[ , k, j, i, 6] + diverg * kmToMm * (Q[ , k, j, i, 6] - Q_seasn_sd[ , k, j, i, 1]) / gridArea) / pet[ , k, j, i, 1]) / AI_humid
			dataOutArray[ , , j, i, 1] = (ppt_seasn_q15[ , k, j, i, 1] + rech[ , k, j, i, 1] + diverg * kmToMm * Q[ , k, j, i, 1] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
			dataOutArray[ , , j, i, 5] = (ppt_seasn_q15[ , k, j, i, 5] + rech[ , k, j, i, 5] + diverg * kmToMm * Q[ , k, j, i, 5] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
			dataOutArray[ , , j, i, 6] = (ppt_seasn_q15[ , k, j, i, 6] + rech[ , k, j, i, 6] + diverg * kmToMm * Q[ , k, j, i, 6] / gridArea) / pet[ , k, j, i, 1]) / AI_humid
		}
	}
}

dataOutArray[ , , , , 1][dataOutArray[ , , , , 1] < 0] = 0
dataOutArray[ , , , , 5][dataOutArray[ , , , , 5] < 0] = 0
dataOutArray[ , , , , 6][dataOutArray[ , , , , 6] < 0] = 0

	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 2, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(nc_decade))	{
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

	# converting array to nc
tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Aridity Index - transient seasonal supplemented by regional avg [-]'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncpath,'aridityIndex_transientSeasonalRegionalAvgSupplement_ssp_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'aridityIndex_transientSeasonalRegionalAvgSupplement_ssp_v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,6] - nc_testDat[,,9,3,5])

nc_close(nc_ppt)
nc_close(nc_pet)
nc_close(nc_Q)
nc_close(nc_rech)
nc_close(nc_ppt_seasn_sd)
nc_close(nc_ppt_seasn_q15)
nc_close(nc_Q_seasn_sd)
nc_close(myNC)
















##########################################################
	# Transient Plant Water Need - Interannual
	# Transient Aridity Index - Interannual
nc_ppt = nc_open(paste0(ncpath, ncFileName_PPT_annual))
nc_ppt_inter_sd = nc_open(paste0(ncpath, ncFileName_PPT_interannualSD))
#nc_ppt_inter_q15 = nc_open(paste0(ncpath, ncFileName_PPT_interannualQ15))
#nc_ppt_seasn_sd = nc_open(paste0(ncpath, ncFileName_PPT_seasonalSD))
#nc_ppt_seasn_q15 = nc_open(paste0(ncpath, ncFileName_PPT_seasonalQ15))
nc_pet = nc_open(paste0(ncpath, ncFileName_PET_annual))
nc_rech = nc_open(paste0(ncpath, ncFileName_Rech_annual))
nc_Q = nc_open(paste0(ncpath, ncFileName_Q_annual))
nc_Q_inter_sd = nc_open(paste0(ncpath, ncFileName_Q_interannualSD))
#nc_Q_seasn_sd = nc_open(paste0(ncpath, ncFileName_Q_seasonalSD))
nc_lat = ncvar_get(nc_ppt, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(nc_ppt, 'lon')
nc_decade = ncvar_get(nc_ppt, 'decade')
nc_scen = ncvar_get(nc_ppt, 'rcpScen')
ppt = ncvar_get(nc_ppt, 'tcfdVariable')
pet = ncvar_get(nc_pet, 'tcfdVariable')
ppt_inter_sd = ncvar_get(nc_ppt_inter_sd, 'tcfdVariable')
#ppt_inter_q15 = ncvar_get(nc_ppt_inter_q15, 'tcfdVariable')
#ppt_seasn_sd = ncvar_get(nc_ppt_seasn_sd, 'tcfdVariable')
#ppt_seasn_q15 = ncvar_get(nc_ppt_seasn_q15, 'tcfdVariable')
rech = ncvar_get(nc_rech, 'tcfdVariable')
Q = ncvar_get(nc_Q, 'tcfdVariable')
Q_inter_sd = ncvar_get(nc_Q_inter_sd, 'tcfdVariable')
#Q_seasn_sd = ncvar_get(nc_Q_season_sd, 'tcfdVariable')

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(nc_decade) * length(nc_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(nc_decade), length(nc_scen), length(valueType)))


for(i in 1:length(nc_scen))	{
	for(k in 1:length(nc_lat))	{
		thisLat = abs(nc_lat[k])
		gridArea = halfDegInKM * cos(thisLat * pi / 180) * halfDegInKM 
		
		for(j in 1:length(nc_decade))	{
			dataOutArray[ , k, j, i, 1] = ((1 - runoffRatio) * (ppt[ , k, j, i, 1] - ppt_inter_sd[ , k, j, i, 1]) + rech[ , k, j, i, 1] + (diverg * kmToMm * (Q[ , k, j, i, 1] - Q_inter_sd[ , k, j, i, 1]) / gridArea)) /  wPlant
			dataOutArray[ , k, j, i, 5] = ((1 - runoffRatio) * (ppt[ , k, j, i, 5] - ppt_inter_sd[ , k, j, i, 5]) + rech[ , k, j, i, 5] + (diverg * kmToMm * (Q[ , k, j, i, 5] - Q_inter_sd[ , k, j, i, 5]) / gridArea)) /  wPlant
			dataOutArray[ , k, j, i, 6] = ((1 - runoffRatio) * (ppt[ , k, j, i, 6] - ppt_inter_sd[ , k, j, i, 6]) + rech[ , k, j, i, 6] + (diverg * kmToMm * (Q[ , k, j, i, 6] - Q_inter_sd[ , k, j, i, 6]) / gridArea)) /  wPlant
		}
	}
}

dataOutArray[ , , , , 1][dataOutArray[ , , , , 1] < 0] = 0
dataOutArray[ , , , , 5][dataOutArray[ , , , , 5] < 0] = 0
dataOutArray[ , , , , 6][dataOutArray[ , , , , 6] < 0] = 0
	
	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 3, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 3, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(nc_decade))	{
	dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] <= histQuants[j]]
	for(j in 1:(length(histQuants)))	{
		dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 2, 2][dataOutArray[ , , i, 2, 1] <= histQuants[j]] = j
		dataOutArray[ , , i, 3, 2][dataOutArray[ , , i, 3, 1] <= histQuants[j]] = j
	}
	dataOutArray[ , , i, 1, 2][maskedLocs26] = NA
	dataOutArray[ , , i, 2, 2][maskedLocs60] = NA
	dataOutArray[ , , i, 3, 2][maskedLocs85] = NA
}


	# converting array to nc
tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Plant Water Demand - transient interannual supplemented by transient interannual regional avg [-]'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncpath,'plantWaterDemand_transientInterannualRegionalTransientInterannualSupplement_ssp_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'plantWaterDemand_transientInterannualRegionalTransientInterannualSupplement_ssp_v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
nc_onesDat = nc_testDat	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] < 1] = 0	;	nc_onesDat[,,,,1][nc_onesDat[,,,,1] >= 1] = 1
image(nc_lon, rev(nc_lat), nc_onesDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,6] - nc_testDat[,,9,3,5])

nc_close(nc_ppt)
nc_close(nc_pet)
nc_close(nc_Q)
nc_close(nc_rech)
nc_close(nc_ppt_inter_sd)
nc_close(nc_ppt_inter_q15)
nc_close(nc_Q_inter_sd)
nc_close(nc_Q_seasn_sd)
nc_close(myNC)










##########################################################
	# Transient Plant Water Need - Seasonal
nc_ppt = nc_open(paste0(ncpath, ncFileName_PPT_annual))
#nc_ppt_inter_sd = nc_open(paste0(ncpath, ncFileName_PPT_interannualSD))
#nc_ppt_inter_q15 = nc_open(paste0(ncpath, ncFileName_PPT_interannualQ15))
#nc_ppt_seasn_sd = nc_open(paste0(ncpath, ncFileName_PPT_seasonalSD))
nc_ppt_seasn_q15 = nc_open(paste0(ncpath, ncFileName_PPT_seasonalQ15))
nc_pet = nc_open(paste0(ncpath, ncFileName_PET_annual))
nc_rech = nc_open(paste0(ncpath, ncFileName_Rech_annual))
nc_Q = nc_open(paste0(ncpath, ncFileName_Q_annual))
#nc_Q_inter_sd = nc_open(paste0(ncpath, ncFileName_Q_interannualSD))
nc_Q_seasn_sd = nc_open(paste0(ncpath, ncFileName_Q_seasonalSD))
nc_lat = ncvar_get(nc_ppt, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(nc_ppt, 'lon')
nc_decade = ncvar_get(nc_ppt, 'decade')
nc_scen = ncvar_get(nc_ppt, 'rcpScen')
ppt = ncvar_get(nc_ppt, 'tcfdVariable')
pet = ncvar_get(nc_pet, 'tcfdVariable')
#ppt_inter_sd = ncvar_get(nc_ppt_inter_sd, 'tcfdVariable')
#ppt_inter_q15 = ncvar_get(nc_ppt_inter_q15, 'tcfdVariable')
#ppt_seasn_sd = ncvar_get(nc_ppt_seasn_sd, 'tcfdVariable')
ppt_seasn_q15 = ncvar_get(nc_ppt_seasn_q15, 'tcfdVariable')
rech = ncvar_get(nc_rech, 'tcfdVariable')
Q = ncvar_get(nc_Q, 'tcfdVariable')
#Q_inter_sd = ncvar_get(nc_Q_inter_sd, 'tcfdVariable')
Q_seasn_sd = ncvar_get(nc_Q_season_sd, 'tcfdVariable')

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(nc_decade) * length(nc_scen) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(nc_decade), length(nc_scen), length(valueType)))

####!!!!!!!!!!!!!!!!
## switching from SD of seasonal to quantiles
####!!!!!!!!!!!!!!!!

for(i in 1:length(nc_scen))	{
	for(k in 1:length(nc_lat))	{
		thisLat = abs(nc_lat[k])
		gridArea = halfDegInKM * cos(thisLat * pi / 180) * halfDegInKM 
		
		for(j in 1:length(nc_decade))	{
#			dataOutArray[ , k, j, i, 1] = ((1 - runoffRatio) * (ppt[ , k, j, i, 1] - ppt_seasn_sd[ , k, j, i, 1]) + rech[ , k, j, i, 1] + (diverg * kmToMm * (Q[ , k, j, i, 1] - Q_seasn_sd[ , k, j, i, 1]) / gridArea)) /  wPlant
#			dataOutArray[ , k, j, i, 5] = ((1 - runoffRatio) * (ppt[ , k, j, i, 5] - ppt_seasn_sd[ , k, j, i, 5]) + rech[ , k, j, i, 5] + (diverg * kmToMm * (Q[ , k, j, i, 1] - Q_seasn_sd[ , k, j, i, 1]) / gridArea)) /  wPlant
#			dataOutArray[ , k, j, i, 6] = ((1 - runoffRatio) * (ppt[ , k, j, i, 6] - ppt_seasn_sd[ , k, j, i, 6]) + rech[ , k, j, i, 6] + (diverg * kmToMm * (Q[ , k, j, i, 1] - Q_seasn_sd[ , k, j, i, 1]) / gridArea)) /  wPlant
			dataOutArray[ , k, j, i, 1] = ((1 - runoffRatio) * ppt_seasn_q15[ , , j, i, 1] + rech[ , k, j, i, 1] + (diverg * kmToMm * (Q[ , k, j, i, 1] - Q_seasn_sd[ , k, j, i, 1]) / gridArea)) /  wPlant
			dataOutArray[ , k, j, i, 5] = ((1 - runoffRatio) * ppt_seasn_q15[ , , j, i, 5] + rech[ , k, j, i, 5] + (diverg * kmToMm * (Q[ , k, j, i, 5] - Q_seasn_sd[ , k, j, i, 5]) / gridArea)) /  wPlant
			dataOutArray[ , k, j, i, 6] = ((1 - runoffRatio) * ppt_seasn_q15[ , , j, i, 6] + rech[ , k, j, i, 6] + (diverg * kmToMm * (Q[ , k, j, i, 6] - Q_seasn_sd[ , k, j, i, 6]) / gridArea)) /  wPlant
			}
	}
}

dataOutArray[ , , , , 1][dataOutArray[ , , , , 1] < 0] = 0
dataOutArray[ , , , , 5][dataOutArray[ , , , , 5] < 0] = 0
dataOutArray[ , , , , 6][dataOutArray[ , , , , 6] < 0] = 0

	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 2, 1][-maskedLocs85]
histQuants = rev(quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01)))
histQuants

for(i in 1:length(nc_decade))	{
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

	# converting array to nc
tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Plant Water Demand - transient seasonal supplemented by transient seasonal regional [-]'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncpath,'plantWaterDemand_transientSeasonalRegionalTransientSeasonalSupplement_ssp_v2_processed.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncpath, 'plantWaterDemand_transientSeasonalRegionalTransientSeasonalSupplement_ssp_v2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,1,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,6] - nc_testDat[,,9,3,5])

nc_close(nc_ppt)
nc_close(nc_pet)
nc_close(nc_Q)
nc_close(nc_rech)
nc_close(nc_ppt_inter_sd)
nc_close(nc_ppt_seasn_q15)
nc_close(nc_Q_inter_sd)
nc_close(nc_Q_seasn_sd)
nc_close(myNC)

