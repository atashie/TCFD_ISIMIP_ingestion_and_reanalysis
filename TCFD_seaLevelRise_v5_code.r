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
ncpath = "J:\\Cai_data\\TCFD\\SeaLevelRise\\AR6_Projections\\Regional\\low_confidence\\"
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
subFolder = 'ssp'
scenarios = c(126, 245, 585)
whichDecades = seq(10,90,10)
valueType = 1:6

ncname = paste0(ncpath, 'ssp', scenarios[1], '\\total_ssp', scenarios[1], '_low_confidence_values.nc')
ncin = nc_open(ncname)
lat = ncvar_get(ncin, 'lat')
lon = ncvar_get(ncin, 'lon')
glblGrd = 1032:66188
orderedLats = sort(unique(lat[glblGrd]))
orderedLons = sort(unique(lon[glblGrd]))
quantiles = ncvar_get(ncin, 'quantiles')
years = ncvar_get(ncin, 'years')
nc_close(ncin)

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(orderedLats) * length(orderedLons) * length(whichDecades) * length(scenarios) * length(valueType)), 
	dim = c(length(orderedLons), length(orderedLats), length(whichDecades), length(scenarios), length(valueType)))



for(thisScen in 1:length(scenarios))	{
	scenNumber = scenarios[thisScen]
	ncname = paste0(ncpath, 'ssp', scenarios[thisScen], '\\total_ssp', scenarios[thisScen], '_low_confidence_values.nc')
	ncin = nc_open(ncname)
	lat = ncvar_get(ncin, 'lat')[glblGrd]
	lon = ncvar_get(ncin, 'lon')[glblGrd]
	quantiles = ncvar_get(ncin, 'quantiles')
	years = ncvar_get(ncin, 'years')
	Q50 = ncvar_get(ncin, 'sea_level_change')[glblGrd,,which(quantiles == 0.5)]
	Q75 = ncvar_get(ncin, 'sea_level_change')[glblGrd,,which(quantiles == 0.75)]
	Q25 = ncvar_get(ncin, 'sea_level_change')[glblGrd,,which(quantiles == 0.25)]

	for(thisLoc in 1:length(lat))	{
		if(!is.na(Q50[thisLoc, 1]))	{
			thisQ25 = Q25[thisLoc, 1:8]
			thisQ50 = Q50[thisLoc, 1:8]
			thisQ75 = Q75[thisLoc, 1:8]
			
			slpQ25 = lm(thisQ25 ~ c(1:8))
			thisQ25 = c(thisQ25[1] - slpQ25$coef[2], thisQ25)
			slpQ50 = lm(thisQ50 ~ c(1:8))
			thisQ50 = c(thisQ50[1] - slpQ50$coef[2], thisQ50)
			slpQ75 = lm(thisQ75 ~ c(1:8))
			thisQ75 = c(thisQ75[1] - slpQ75$coef[2], thisQ75)

			thisLat = lat[thisLoc]
			thisLon = lon[thisLoc]
			
			i = which(orderedLats == thisLat)
			j = which(orderedLons == thisLon)
			
			dataOutArray[j, i, , thisScen, 1] = thisQ50	# med value
			dataOutArray[j, i, , thisScen, 5] = thisQ25	# Q25
			dataOutArray[j, i, , thisScen, 6] = thisQ75	# Q75

			dataOutArray[j, i, c(1:2), thisScen, 3] = NA # slopes
			dataOutArray[j, i, c(1:2), thisScen, 4] = NA # significance
			for(thisDecade in 3:length(whichDecades))	{
				thisModel = lm(thisQ25[1:thisDecade] ~ c(1:thisDecade))
				dataOutArray[j, i, thisDecade, thisScen, 3] = thisModel$coef[2]# slopes
				dataOutArray[j, i, thisDecade, thisScen, 4] = summary(thisModel)$coeff[2,4]# significance
			}	
		}
	print(thisLoc / 66190)
	}
	saveRDS(dataOutArray, file=paste0(ncpath, 'data_out.rds'))
	nc_close(ncin)
}	



dataOutArray = readRDS(file=paste0(ncpath, 'data_out.rds'))
dataOutArray[ , , 1, 1, 1] = dataOutArray[ , , 1, 2, 1]
dataOutArray[ , , 1, 3, 1] = dataOutArray[ , , 1, 2, 1]

	# defining quantiles 
maskedLocs26 = which(is.na(dataOutArray[ , , 1, 1, 1]))
histDatSubset26 =  dataOutArray[ , , 1, 1, 1][-maskedLocs26]
maskedLocs60 = which(is.na(dataOutArray[ , , 1, 2, 1]))
histDatSubset60 =  dataOutArray[ , , 1, 2, 1][-maskedLocs60]
maskedLocs85 = which(is.na(dataOutArray[ , , 1, 3, 1]))
histDatSubset85 =  dataOutArray[ , , 1, 3, 1][-maskedLocs85]
histQuants = quantile(c(histDatSubset26, histDatSubset60, histDatSubset85), seq(0.01, 1, 0.01))
histQuants

for(i in 1:length(whichDecades))	{
	dataOutArray[ , , i, , 2] = 1
	for(j in 1:(length(histQuants)))	{
		dataOutArray[ , , i, 1, 2][dataOutArray[ , , i, 1, 1] >= histQuants[j]] = j
		dataOutArray[ , , i, 2, 2][dataOutArray[ , , i, 2, 1] >= histQuants[j]] = j
		dataOutArray[ , , i, 3, 2][dataOutArray[ , , i, 3, 1] >= histQuants[j]] = j
	}
	dataOutArray[ , , i, 1, 2][maskedLocs26] = NA
	dataOutArray[ , , i, 2, 2][maskedLocs60] = NA
	dataOutArray[ , , i, 3, 2][maskedLocs60] = NA
}




tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Sea Level Rise - mm'))
attr(tcfdVariable, 'variables') = metadata
names(dim(tcfdVariable)) = c('lon', 'lat', 'decade','rcpScen', 'valueClass')

lon = orderedLons
dim(lon) = length(lon)
metadata = list(lon = list(units = 'degrees'))
attr(lon, 'variables') = metadata
names(dim(lon)) = 'lon'

lat = orderedLats
dim(lat) = length(lat)
metadata = list(lat = list(units = 'degrees'))
attr(lat, 'variables') = metadata
names(dim(lat)) = 'lat'

decade = whichDecades
dim(decade) = length(decade)
metadata = list(decade = list(units = 'decades_of_21st_C'))
attr(decade, 'variables') = metadata
names(dim(decade)) = 'decade'

rcpScen = scenarios
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
ncVarFileName = 'sea_level_change'
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncOutputPath, ncVarFileName, '_glblGrdv2_processed.nc'))
#ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncOutputPath, ncVarFileName, 'v2_processed.nc'))

	# testing output, squinty eye test
#myNC = nc_open(paste0(ncOutputPath, ncVarFileName, 'v2_processed.nc'))
myNC = nc_open(paste0(ncOutputPath, ncVarFileName, '_glblGrdv2_processed.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, nc_lat, nc_testDat[,,1,1,1])
image(nc_lon, nc_lat, nc_testDat[,,1,2,1])
image(nc_lon, nc_lat, nc_testDat[,,1,3,1])

image(nc_lon, nc_lat, nc_testDat[,,1,1,2])
image(nc_lon, nc_lat, nc_testDat[,,1,1,3])
image(nc_lon, nc_lat, nc_testDat[,,1,1,4])
image(nc_lon, nc_lat, nc_testDat[,,1,1,5])
image(nc_lon, nc_lat, nc_testDat[,,1,1,6])

image(nc_lon, nc_lat, nc_testDat[,,9,3,1] - nc_testDat[,,1,3,1])
image(nc_lon, nc_lat, nc_testDat[,,9,3,1] - nc_testDat[,,1,2,1])

image(nc_lon, nc_lat, nc_testDat[,,9,3,2] - nc_testDat[,,1,3,2])
image(nc_lon, nc_lat, nc_testDat[,,9,3,2] - nc_testDat[,,1,2,2])

