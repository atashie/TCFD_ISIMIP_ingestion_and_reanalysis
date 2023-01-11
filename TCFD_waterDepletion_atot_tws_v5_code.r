########################################################
library(data.table)
library(lubridate)
library(ncdf4)
library(easyNCDF)	# for ArrayToNc()
#library(robslopes)	# for TheilSen()
#library(trend)		# for sens.slope()
library(mblm)		# for sens slope mlbm()
library(cmsafops)	# for cmsaf.div() and cmsaf.sub()



#########################################
# reading in climai netcdf data
ncpath1 = "J:\\Cai_data\\TCFD\\PotentialTotalWaterWithdrawal\\"
ncpath2 = "J:\\Cai_data\\TCFD\\ActualTotalWaterUse\\"
ncpath3 = "J:\\Cai_data\\TCFD\\TotalWaterStorage\\"
ncVarFileName1 = 'ptotww'
ncVarFileName2 = 'atotuse'
ncVarFileName3 = 'tws'
scenarios = c(26, 60, 85)

for(i in scenarios)	{
		# pre-processing all data
	secsPerMonth = 60*60*24*30.4375
		# first, fgdl
	ncname_gfdl1 = paste0('watergap2-2c_gfdl-esm2m_ewembi_rcp', i, '_2005soc_co2_', ncVarFileName1, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncname_gfdl2 = paste0('watergap2-2c_gfdl-esm2m_ewembi_rcp', i, '_2005soc_co2_', ncVarFileName2, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncname_gfdl3 = paste0('clm45_gfdl-esm2m_ewembi_rcp', i, '_2005soc_co2_', ncVarFileName3, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  

	cmsaf.mulc(var = ncVarFileName1, const = secsPerMonth, infile = paste0(ncpath1, ncname_gfdl1), outfile = file.path('J:\\Downloads\\secsToMonths_ptotww.nc'), overwrite = TRUE)
	cmsaf.mulc(var = ncVarFileName2, const = secsPerMonth, infile = paste0(ncpath2, ncname_gfdl2), outfile = file.path('J:\\Downloads\\secsToMonths_atotuse.nc'), overwrite = TRUE)
	cmsaf.sub(var1 = ncVarFileName3, var2 = ncVarFileName2, infile1 = paste0(ncpath3, ncname_gfdl3), infile2 = file.path('J:\\Downloads\\secsToMonths_atotuse.nc'),
		outfile = file.path('J:\\Downloads\\remaininWater.nc'), overwrite = TRUE)
	cmsaf.div(var1 = ncVarFileName1, var2 = ncVarFileName3, infile1 = file.path('J:\\Downloads\\secsToMonths_ptotww.nc'), infile2 = file.path('J:\\Downloads\\remaininWater.nc'),
		outfile = file.path(paste0('J:\\Cai_data\\TCFD\\WaterStress\\baselineWaterStress_gfdl', i, '.nc')), overwrite = TRUE)

	nc_gfdl_init = nc_open(file.path(paste0('J:\\Cai_data\\TCFD\\WaterStress\\baselineWaterStress_gfdl', i, '.nc')))
	nc_lat = ncvar_get(nc_gfdl_init, 'lat')	# lat is given from high to low
	nc_lon = ncvar_get(nc_gfdl_init, 'lon')
	nc_testDat = ncvar_get(nc_gfdl_init, ncVarFileName1)
	image(nc_lon, rev(nc_lat),  nc_testDat[,,2] - nc_testDat[,,1])
	nc_close(nc_gfdl_init)

		# next, hadgem
	ncname_hadgem1 = paste0('watergap2-2c_hadgem2-es_ewembi_rcp', i, '_2005soc_co2_', ncVarFileName1, '_global_monthly_2006_2099.nc4')#"clm45_hadgem-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncname_hadgem2 = paste0('watergap2-2c_hadgem2-es_ewembi_rcp', i, '_2005soc_co2_', ncVarFileName2, '_global_monthly_2006_2099.nc4')#"clm45_hadgem-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncname_hadgem3 = paste0('clm45_hadgem2-es_ewembi_rcp', i, '_2005soc_co2_', ncVarFileName3, '_global_monthly_2006_2099.nc4')#"clm45_hadgem-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  

	cmsaf.mulc(var = ncVarFileName1, const = secsPerMonth, infile = paste0(ncpath1, ncname_hadgem1), outfile = file.path('J:\\Downloads\\secsToMonths_ptotww.nc'), overwrite = TRUE)
	cmsaf.mulc(var = ncVarFileName2, const = secsPerMonth, infile = paste0(ncpath2, ncname_hadgem2), outfile = file.path('J:\\Downloads\\secsToMonths_atotuse.nc'), overwrite = TRUE)
	cmsaf.sub(var1 = ncVarFileName3, var2 = ncVarFileName2, infile1 = paste0(ncpath3, ncname_hadgem3), infile2 = file.path('J:\\Downloads\\secsToMonths_atotuse.nc'),
		outfile = file.path('J:\\Downloads\\remaininWater.nc'), overwrite = TRUE)
	cmsaf.div(var1 = ncVarFileName1, var2 = ncVarFileName3, infile1 = file.path('J:\\Downloads\\secsToMonths_ptotww.nc'), infile2 = file.path('J:\\Downloads\\remaininWater.nc'),
		outfile = file.path(paste0('J:\\Cai_data\\TCFD\\WaterStress\\baselineWaterStress_hadgem', i, '.nc')), overwrite = TRUE)

	nc_hadgem_init = nc_open(file.path(paste0('J:\\Cai_data\\TCFD\\WaterStress\\baselineWaterStress_hadgem', i, '.nc')))
	nc_lat = ncvar_get(nc_hadgem_init, 'lat')	# lat is given from high to low
	nc_lon = ncvar_get(nc_hadgem_init, 'lon')
	nc_testDat = ncvar_get(nc_hadgem_init, ncVarFileName1)
	image(nc_lon, rev(nc_lat), log(nc_testDat[,,9]))
	nc_close(nc_hadgem_init)


		# next, ipsl
	ncname_ipsl1 = paste0('watergap2-2c_ipsl-cm5a-lr_ewembi_rcp', i, '_2005soc_co2_', ncVarFileName1, '_global_monthly_2006_2099.nc4')#"clm45_ipsl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncname_ipsl2 = paste0('watergap2-2c_ipsl-cm5a-lr_ewembi_rcp', i, '_2005soc_co2_', ncVarFileName2, '_global_monthly_2006_2099.nc4')#"clm45_ipsl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncname_ipsl3 = paste0('clm45_ipsl-cm5a-lr_ewembi_rcp', i, '_2005soc_co2_', ncVarFileName3, '_global_monthly_2006_2099.nc4')#"clm45_ipsl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  

	cmsaf.mulc(var = ncVarFileName1, const = secsPerMonth, infile = paste0(ncpath1, ncname_ipsl1), outfile = file.path('J:\\Downloads\\secsToMonths_ptotww.nc'), overwrite = TRUE)
	cmsaf.mulc(var = ncVarFileName2, const = secsPerMonth, infile = paste0(ncpath2, ncname_ipsl2), outfile = file.path('J:\\Downloads\\secsToMonths_atotuse.nc'), overwrite = TRUE)
	cmsaf.sub(var1 = ncVarFileName3, var2 = ncVarFileName2, infile1 = paste0(ncpath3, ncname_ipsl3), infile2 = file.path('J:\\Downloads\\secsToMonths_atotuse.nc'),
		outfile = file.path('J:\\Downloads\\remaininWater.nc'), overwrite = TRUE)
	cmsaf.div(var1 = ncVarFileName1, var2 = ncVarFileName3, infile1 = file.path('J:\\Downloads\\secsToMonths_ptotww.nc'), infile2 = file.path('J:\\Downloads\\remaininWater.nc'),
		outfile = file.path(paste0('J:\\Cai_data\\TCFD\\WaterStress\\baselineWaterStress_ipsl', i, '.nc')), overwrite = TRUE)

	nc_ipsl_init = nc_open(file.path(paste0('J:\\Cai_data\\TCFD\\WaterStress\\baselineWaterStress_ipsl', i, '.nc')))
	nc_lat = ncvar_get(nc_ipsl_init, 'lat')	# lat is given from high to low
	nc_lon = ncvar_get(nc_ipsl_init, 'lon')
	nc_testDat = ncvar_get(nc_ipsl_init, ncVarFileName1)
	image(nc_lon, rev(nc_lat), log(nc_testDat[,,9]))
	nc_close(nc_ipsl_init)

		# next, miroc
	ncname_miroc1 = paste0('watergap2-2c_miroc5_ewembi_rcp', i, '_2005soc_co2_', ncVarFileName1, '_global_monthly_2006_2099.nc4')#"clm45_miroc-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncname_miroc2 = paste0('watergap2-2c_miroc5_ewembi_rcp', i, '_2005soc_co2_', ncVarFileName2, '_global_monthly_2006_2099.nc4')#"clm45_miroc-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncname_miroc3 = paste0('clm45_miroc5_ewembi_rcp', i, '_2005soc_co2_', ncVarFileName3, '_global_monthly_2006_2099.nc4')#"clm45_miroc-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  

	cmsaf.mulc(var = ncVarFileName1, const = secsPerMonth, infile = paste0(ncpath1, ncname_miroc1), outfile = file.path('J:\\Downloads\\secsToMonths_ptotww.nc'), overwrite = TRUE)
	cmsaf.mulc(var = ncVarFileName2, const = secsPerMonth, infile = paste0(ncpath2, ncname_miroc2), outfile = file.path('J:\\Downloads\\secsToMonths_atotuse.nc'), overwrite = TRUE)
	cmsaf.sub(var1 = ncVarFileName3, var2 = ncVarFileName2, infile1 = paste0(ncpath3, ncname_miroc3), infile2 = file.path('J:\\Downloads\\secsToMonths_atotuse.nc'),
		outfile = file.path('J:\\Downloads\\remaininWater.nc'), overwrite = TRUE)
	cmsaf.div(var1 = ncVarFileName1, var2 = ncVarFileName3, infile1 = file.path('J:\\Downloads\\secsToMonths_ptotww.nc'), infile2 = file.path('J:\\Downloads\\remaininWater.nc'),
		outfile = file.path(paste0('J:\\Cai_data\\TCFD\\WaterStress\\baselineWaterStress_miroc', i, '.nc')), overwrite = TRUE)

	nc_miroc_init = nc_open(file.path(paste0('J:\\Cai_data\\TCFD\\WaterStress\\baselineWaterStress_miroc', i, '.nc')))
	nc_lat = ncvar_get(nc_miroc_init, 'lat')	# lat is given from high to low
	nc_lon = ncvar_get(nc_miroc_init, 'lon')
	nc_testDat = ncvar_get(nc_miroc_init, ncVarFileName1)
	image(nc_lon, rev(nc_lat), log(nc_testDat[,,9]))
	nc_close(nc_miroc_init)
}


###########################################


ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
ncpath = 'J:\\Cai_data\\TCFD\\WaterStress\\'
saveDate = '05JAN2022'
rcpScenarios = c(26, 60, 85)
whichDecades = seq(10,90,10)
valueType = 1:6

	# initializing start decade
initDates = 1:168
ncin_gfdl = nc_open(paste0(ncpath, 'baselineWaterStress_gfdl26.nc'))
nc_gfdl_init = ncvar_get(ncin_gfdl, ncVarFileName1)[ , , initDates]	# lon, lat, time
	# identifying lat lons before closing data
nc_lat = ncvar_get(ncin_gfdl, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_gfdl, 'lon')
nc_close(ncin_gfdl)

ncin_hadgem = nc_open(paste0(ncpath, 'baselineWaterStress_hadgem26.nc'))
nc_hadgem_init = ncvar_get(ncin_hadgem, ncVarFileName1)[ , , initDates]	# lon, lat, time
nc_close(ncin_hadgem)

ncin_ipsl = nc_open(paste0(ncpath, 'baselineWaterStress_ipsl26.nc'))
nc_ipsl_init = ncvar_get(ncin_ipsl, ncVarFileName1)[ , , initDates]	# lon, lat, time
nc_close(ncin_ipsl)

ncin_miroc = nc_open(paste0(ncpath, 'baselineWaterStress_miroc26.nc'))
nc_miroc_init = ncvar_get(ncin_miroc, ncVarFileName1)[ , , initDates]	# lon, lat, time
nc_close(ncin_miroc)

	# array for holding outputs
myMissingData = NA
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))

	# rescaling for signif digits
scalar = 1


for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]

	ncname_gfdl = paste0('baselineWaterStress_gfdl',rcpScenNum,'.nc')
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName1)	# lon, lat, time

	ncname_hadgem = paste0('baselineWaterStress_hadgem',rcpScenNum,'.nc')
	ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
	nc_hadgem = ncvar_get(ncin_hadgem,ncVarFileName1)	# lon, lat, time
	
	ncname_ipsl = paste0('baselineWaterStress_ipsl',rcpScenNum,'.nc')
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
	nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName1)	# lon, lat, time
	
	ncname_miroc = paste0('baselineWaterStress_miroc',rcpScenNum,'.nc')
	ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
	nc_miroc = ncvar_get(ncin_miroc,ncVarFileName1)	# lon, lat, time

	nc_date = as.Date("1661-01-01") + ncvar_get(ncin_miroc, 'time') * 30.4375# time is months after 1661-1-1
	nc_years = unique(year(nc_date))
	numYears = length(nc_years)
	missing_data = 1.00000002004088e+20


	for(i in 1:length(nc_lat))	{
		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j,i, ] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				print(c(i, j))
				gfdl_all = na.approx(c(nc_gfdl_init[j,i, initDates], nc_gfdl[j,i, -initDates]), na.rm=FALSE) * scalar
				hadgem_all = na.approx(c(nc_hadgem_init[j,i, initDates], nc_hadgem[j,i, -initDates]), na.rm=FALSE) * scalar
				ipsl_all = na.approx(c(nc_ipsl_init[j,i, initDates], nc_ipsl[j,i, -initDates]), na.rm=FALSE) * scalar
				miroc_all = na.approx(c(nc_miroc_init[j,i, initDates], nc_miroc[j,i, -initDates]), na.rm=FALSE) * scalar

				gfdl_yrly = NULL
				hadgem_yrly = NULL
				ipsl_yrly = NULL
				miroc_yrly = NULL
				for(thisYear in nc_years)	{
					theseDates = which(year(nc_date) == thisYear)
					gfdl_yrly = c(gfdl_yrly, mean(gfdl_all[theseDates], na.rm=TRUE))
					hadgem_yrly = c(hadgem_yrly, mean(hadgem_all[theseDates], na.rm=TRUE))
					ipsl_yrly = c(ipsl_yrly, mean(ipsl_all[theseDates], na.rm=TRUE))
					miroc_yrly = c(miroc_yrly, mean(miroc_all[theseDates], na.rm=TRUE))
				}
				
				gfdl_smth = ksmooth(nc_years, gfdl_yrly, kernel = 'normal', bandwidth = 20, n.points = numYears)$y
				hadgem_smth = ksmooth(nc_years, hadgem_yrly, kernel = 'normal', bandwidth = 20, n.points = numYears)$y
				ipsl_smth = ksmooth(nc_years, ipsl_yrly, kernel = 'normal', bandwidth = 20, n.points = numYears)$y
				miroc_smth = ksmooth(nc_years, miroc_yrly, kernel = 'normal', bandwidth = 20, n.points = numYears)$y


				for(thisDecade in 1:9)	{
					theseYears = (thisDecade - 1) * 10 + 5:14
						# defining absolute values
					dataSmoothMed = median(c(gfdl_smth[theseYears], hadgem_smth[theseYears], ipsl_smth[theseYears], miroc_smth[theseYears]))
					dataOutArray[j, i, thisDecade, thisScen, 1] = dataSmoothMed
					dataQuantDiffs = diff(quantile(c(gfdl_yrly[theseYears], hadgem_yrly[theseYears], ipsl_yrly[theseYears], miroc_yrly[theseYears]), c(0.25, 0.5, 0.75)))
					dataOutArray[j, i, thisDecade, thisScen, 5] = dataSmoothMed - abs(dataQuantDiffs[1])
					dataOutArray[j, i, thisDecade, thisScen, 6] =  dataSmoothMed + abs(dataQuantDiffs[2])
						# calculating decadal trends (sens slope) and 	decadal significance (spearmans)	
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
histQuants

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
metadata = list(tcfdVariable = list(units = 'Water Stress [ - ]'))
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
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncOutputPath,'WRI_based_WaterStress_v2_processed.nc'))

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

image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,1] - nc_testDat[,,1,3,1])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,1] - nc_testDat[,,1,2,1])

image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,3,2])
image(nc_lon, rev(nc_lat), nc_testDat[,,9,3,2] - nc_testDat[,,1,2,2])

