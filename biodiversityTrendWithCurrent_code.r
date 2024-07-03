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
ncpath = "J:\\Cai_data\\TCFD\\Biodiversity\\Future\\"
customerOutputPath = 'J:\\Cai_data\\TCFD\\locations\\McCain_May2024\\'
ncVarFileName = 'leh'
saveDate = '27MAR024'
rcpScenarios = c(26, 60)
customerTable = data.table::fread(paste0(customerOutputPath, "McCain_Output.csv"))[,-1]


	# reading in nc data
birdies = nc_open(paste0(ncpath, "birdsumprob.nc"))
mammals = nc_open(paste0(ncpath, "mammalsumprob.nc"))
amphibs = nc_open(paste0(ncpath, "amphibiansumprob.nc"))

birdVar = ncvar_get(birdies, "presence")
mammVar = ncvar_get(mammals, "presence")
amphVar = ncvar_get(amphibs, "presence")
	# sapply(birdies$var$presence$dim, function(x) x$name)
	# format [scenario, lon, lat, time, forcing, model]

nc_lons = ncvar_get(birdies, "lon")
nc_lats = ncvar_get(birdies, "lat")
nc_date = lubridate::year(ncvar_get(birdies, "time") + as.Date("1990-01-01"))



	# df for holding outputs
myMissingData = NA
customerTable$Bird_PresenceTrajectory = NA
customerTable$MammalPresenceTrajectory = NA
customerTable$Amphibian_PresenceTrajectory = NA
customerTable$Bird_RelativeCoverage = NA
customerTable$Mammal_RelativeCoverage = NA
customerTable$Amphibian_RelativeCoverage = NA

for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]
	birdAvg = quantile(birdVar[thisScen, , , 1:4, , ], seq(0,1,0.01), na.rm=TRUE)
	amphAvg = quantile(amphVar[thisScen, , , 1:4, , ], seq(0,1,0.01), na.rm=TRUE)
	mammAvg = quantile(mammVar[thisScen, , , 1:4, , ], seq(0,1,0.01), na.rm=TRUE)

	for(thisLocation in 1:nrow(customerTable))	{

		closeLon = which.min(abs(nc_lons - customerTable$Longitude[thisLocation]))
		closeLat = which.min(abs(nc_lats - customerTable$Latitude[thisLocation]))

		# checking to ensure we are not over a water tile
		if(all(is.na(mammVar[thisScen, closeLon, closeLat, 1, , ])))	{
			closeLat = closeLat + 1
			if(all(is.na(mammVar[thisScen, closeLon, closeLat, 1, , ])))	{
				closeLat = closeLat - 2
				if(all(is.na(mammVar[thisScen, closeLon, closeLat, 1, , ])))	{
					closeLat = closeLat + 1
					closeLon = closeLon + 1
					if(all(is.na(mammVar[thisScen, closeLon, closeLat, 1, , ])))	{
						closeLon = closeLon - 2
						if(all(is.na(mammVar[thisScen, closeLon, closeLat, 1, , ])))	{
							closeLon = closeLon + 1
						}
					}
				}
			}
		}
			
		birdData = as.data.frame(cbind(birdVar[thisScen, closeLon, closeLat, , , 1], birdVar[thisScen, closeLon, closeLat, , , 1]))
		birdData$Date = nc_date	
		birdMelt = reshape::melt(birdData, id="Date")

		mammData = as.data.frame(cbind(mammVar[thisScen, closeLon, closeLat, , , 1], mammVar[thisScen, closeLon, closeLat, , , 1]))
		mammData$Date = nc_date	
		mammMelt = reshape::melt(mammData, id="Date")

		amphData = as.data.frame(cbind(amphVar[thisScen, closeLon, closeLat, , , 1], amphVar[thisScen, closeLon, closeLat, , , 1]))
		amphData$Date = nc_date	
		amphMelt = reshape::melt(amphData, id="Date")

			# testing for NaNs, which unforunately do not coincide
		if(all(is.na(birdMelt$value))) {
			customerTable$Bird_PresenceTrajectory[thisLocation] = 0
			birdRel = 5
		} else {
			customerTable$Bird_PresenceTrajectory[thisLocation] = mblm(value ~ Date, birdMelt)$coefficients[2] * 10 * 100 / birdAvg[101]
			birdRel = which(birdAvg > mean(subset(birdMelt, Date < 2025)$value))[1]
		}

		if(all(is.na(mammMelt$value))) {
			customerTable$MammalPresenceTrajectory[thisLocation] = 0
			mammRel = 5
		} else {
			customerTable$MammalPresenceTrajectory[thisLocation] = mblm(value ~ Date, mammMelt)$coefficients[2] * 10 * 100 / mammAvg[101]
			mammRel = which(mammAvg > mean(subset(mammMelt, Date < 2025)$value))[1]
		}

		if(all(is.na(amphMelt$value))) {
			customerTable$Amphibian_PresenceTrajectory[thisLocation] = 5
			amphRel = 5
		} else {
			customerTable$Amphibian_PresenceTrajectory[thisLocation] = mblm(value ~ Date, amphMelt)$coefficients[2] * 10 * 100 / amphAvg[101]
			amphRel = which(amphAvg > mean(subset(amphMelt, Date < 2025)$value))[1]
		}
		
		customerTable$Bird_RelativeCoverage[thisLocation] = birdRel / (birdRel + mammRel + amphRel)
		customerTable$Mammal_RelativeCoverage[thisLocation] = mammRel / (birdRel + mammRel + amphRel)
		customerTable$Amphibian_RelativeCoverage[thisLocation] = amphRel / (birdRel + mammRel + amphRel)
		
		
	}
	customerTable$Biodiversity_Index_Score = 101 - customerTable$RankVal  
	data.table::fwrite(customerTable, paste0(customerOutputPath, "_", rcpScenNum, ".csv"))
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