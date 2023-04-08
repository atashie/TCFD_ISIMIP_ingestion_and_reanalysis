#######################################################
library(data.table)
library(lubridate)
library(ncdf4)
library(easyNCDF)	# for ArrayToNc()


# standard values and names
ncOutputPath = "J:\\Cai_data\\WaterIndex\\"
saveDate = '22MAR2023'
rcpScenarios = c(26, 60, 85)
sspScenarios = c(126, 370, 585)
whichDecades = seq(2010,2090,10)
valueType = 1:20 # 1-12 for months, 13 for annual mean,  14-20 for interannual 5, 15, 25, 50, 75, 85, 95th quantiles



#########################################
## Part 1: Precipitaiton
#########################################
ncpath = "J:\\Cai_data\\TCFD\\Precipitation\\"
ncVarFileName1 = 'rainf'
initDates = 1:168

# reading in climai netcdf data

	# initializing start decade
ncin_gfdl = nc_open(paste0(ncpath, 'totPrecip_gfdl_26.nc'))
nc_gfdl_init = ncvar_get(ncin_gfdl, ncVarFileName1)[ , , initDates]	# lon, lat, time
	# identifying lat lons before closing data
nc_lat = ncvar_get(ncin_gfdl, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_gfdl, 'lon')
nc_close(ncin_gfdl)

ncin_hadgem = nc_open(paste0(ncpath, 'totPrecip_hadgem_26.nc'))
nc_hadgem_init = ncvar_get(ncin_hadgem, ncVarFileName1)[ , , initDates]	# lon, lat, time
nc_close(ncin_hadgem)

ncin_ipsl = nc_open(paste0(ncpath, 'totPrecip_ipsl_26.nc'))
nc_ipsl_init = ncvar_get(ncin_ipsl, ncVarFileName1)[ , , initDates]	# lon, lat, time
nc_close(ncin_ipsl)

ncin_miroc = nc_open(paste0(ncpath, 'totPrecip_miroc_26.nc'))
nc_miroc_init = ncvar_get(ncin_miroc, ncVarFileName1)[ , , initDates]	# lon, lat, time
nc_close(ncin_miroc)

	# array for holding outputs
myMissingData = NA
#dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
#	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(valueType)))

	# rescaling for signif digits
scalar = 60*60*24*30.4375 # secs per month


for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]

	ncname_gfdl = paste0('totPrecip_gfdl_',rcpScenNum,'.nc')
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName1)	# lon, lat, time

	ncname_hadgem = paste0('totPrecip_hadgem_',rcpScenNum,'.nc')
	ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
	nc_hadgem = ncvar_get(ncin_hadgem,ncVarFileName1)	# lon, lat, time
	
	ncname_ipsl = paste0('totPrecip_ipsl_',rcpScenNum,'.nc')
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
	nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName1)	# lon, lat, time
	
	ncname_miroc = paste0('totPrecip_miroc_',rcpScenNum,'.nc')
	ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
	nc_miroc = ncvar_get(ncin_miroc,ncVarFileName1)	# lon, lat, time

	nc_date = as.Date("1661-01-01") + ncvar_get(ncin_miroc, 'time') * 30.4375# time is months after 1661-1-1
	nc_decades = (year(nc_date)%/% 10 ) *10
	nc_months = month(nc_date)
	nc_years_all = year(nc_date)
	nc_years_unique = unique(nc_years_all)
	missing_data = 1.00000002004088e+20
#	nonZeroGenerator = sample(seq(0.0001,0.001,length.out=1000), length(nc_date), replace = TRUE)
	monthsByDecade = seq(0,119,12)

	for(i in 1:length(nc_lat))	{
		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j,i, ] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				print(c(i, j))
				gfdl_all = c(nc_gfdl_init[j,i, initDates], nc_gfdl[j,i, -initDates]) * scalar
				hadgem_all = c(nc_hadgem_init[j,i, initDates], nc_hadgem[j,i, -initDates]) * scalar
				ipsl_all = c(nc_ipsl_init[j,i, initDates], nc_ipsl[j,i, -initDates]) * scalar
				miroc_all = c(nc_miroc_init[j,i, initDates], nc_miroc[j,i, -initDates]) * scalar

				gfdl_yrly = NULL
				hadgem_yrly = NULL
				ipsl_yrly = NULL
				miroc_yrly = NULL
				for(thisYear in nc_years_unique)	{
					theseDates = which(nc_years_all == thisYear)
					gfdl_yrly =   c(gfdl_yrly,   sum(gfdl_all[theseDates]))
					hadgem_yrly = c(hadgem_yrly, sum(hadgem_all[theseDates]))
					ipsl_yrly =   c(ipsl_yrly,   sum(ipsl_all[theseDates]))
					miroc_yrly =  c(miroc_yrly,  sum(miroc_all[theseDates]))
				}

				eachDecade = 0
				for(thisDecade in whichDecades)	{
					eachDecade = eachDecade + 1
					theseYears = which(nc_decades == thisDecade)
						# monthly averages
					for(thisMonth in 1:12)	{
						dataOutArray[j, i, eachDecade, thisMonth] =  mean(c(
							gfdl_all[theseYears][thisMonth + monthsByDecade], 
							hadgem_all[theseYears][thisMonth + monthsByDecade],
							ipsl_all[theseYears][thisMonth + monthsByDecade],
							miroc_all[theseYears][thisMonth + monthsByDecade]))
					}
						# annual averages
					subsetYears = which(nc_years_unique %in% seq(thisDecade, thisDecade+9, 1))
					dataOutArray[j, i, eachDecade, 13] =  mean(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]))
						# annual extremes
					dataOutArray[j, i, eachDecade, 14] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.05, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 15] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.15, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 16] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.25, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 17] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.5, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 18] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.75, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 19] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.85, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 20] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.95, na.rm=TRUE)
				}
			}
		}
	saveRDS(dataOutArray, file=paste0(ncpath, rcpScenarios[thisScen], 'data_out.rds'))
	}
	nc_close(ncin_gfdl)
	nc_close(ncin_hadgem)
	nc_close(ncin_ipsl)
	nc_close(ncin_miroc)
}
#dataOutArray = readRDS(file=paste0(ncpath, 'data_out.rds'))
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))
dataOutArray[ , , , 1, ] = readRDS(file=paste0(ncpath, rcpScenarios[1], 'data_out.rds'))
dataOutArray[ , , , 2, ] = readRDS(file=paste0(ncpath, rcpScenarios[2], 'data_out.rds'))
dataOutArray[ , , , 3, ] = readRDS(file=paste0(ncpath, rcpScenarios[3], 'data_out.rds'))

tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Precipitation (mm)'))
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

valueClass = 1:20#valueType
dim(valueClass) = length(valueClass)
metadata = list(valueClass = list(units = 'class'))
attr(valueClass, 'variables') = metadata
names(dim(valueClass)) = 'valueClass'

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncOutputPath,'Precipitation_decadalRawVals.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncOutputPath, 'Precipitation_decadalRawVals.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])






#########################################
#########################################
#########################################
## Part 2: PET
##################################################################

# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\Evapotranspiration\\"
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
ncVarFileName = 'potevap'
initDates = 1:60

	# initializing start decade
ncin_gfdl = nc_open(paste0(ncpath, paste0('h08_gfdl-esm4_w5e5_ssp126_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')))
nc_gfdl_init = ncvar_get(ncin_gfdl,ncVarFileName)[ , , initDates]	# lon, lat, time
	# identifying lat lons before closing data
nc_lat = ncvar_get(ncin_gfdl, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_gfdl, 'lon')
nc_close(ncin_gfdl)

ncin_mpi = nc_open(paste0(ncpath,  paste0('h08_mpi-esm1-2-hr_w5e5_ssp126_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')))
nc_mpi_init = ncvar_get(ncin_mpi,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_mpi)

ncin_ipsl = nc_open(paste0(ncpath, paste0('h08_ipsl-cm6a-lr_w5e5_ssp126_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')))
nc_ipsl_init = ncvar_get(ncin_ipsl,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_ipsl)

ncin_mri = nc_open(paste0(ncpath, paste0('h08_mri-esm2-0_w5e5_ssp126_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')))
nc_mri_init = ncvar_get(ncin_mri,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_mri)

ncin_ukesm = nc_open(paste0(ncpath, paste0('h08_ukesm1-0-ll_w5e5_ssp126_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')))
nc_ukesm_init = ncvar_get(ncin_ukesm,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_ukesm)


	# array for holding outputs
myMissingData = NA
#dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
#	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(valueType)))

scalar = 60*60*24*30.4375  #kg / m2 / s to mm / month

for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = sspScenarios[thisScen]
	rcpScen = paste0('ssp', rcpScenNum)

	ncname_gfdl = paste0('h08_gfdl-esm4_w5e5_', rcpScen, '_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))

	ncname_mpi = paste0('h08_mpi-esm1-2-hr_w5e5_', rcpScen, '_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')#"clm45_hadgem-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_mpi = nc_open(paste0(ncpath, ncname_mpi))

	ncname_ipsl = paste0('h08_ipsl-cm6a-lr_w5e5_', rcpScen, '_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')#"clm45_ipsl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))

	ncname_mri = paste0('h08_mri-esm2-0_w5e5_', rcpScen, '_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')#"clm45_miroc-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_mri = nc_open(paste0(ncpath, ncname_mri))

	ncname_ukesm = paste0('h08_ukesm1-0-ll_w5e5_', rcpScen, '_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')#"clm45_miroc-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_ukesm = nc_open(paste0(ncpath, ncname_ukesm))

	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName)	# lon, lat, time
	nc_mpi = ncvar_get(ncin_mpi,ncVarFileName)	# lon, lat, time
	nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName)	# lon, lat, time
	nc_mri = ncvar_get(ncin_mri,ncVarFileName)	# lon, lat, time
	nc_ukesm = ncvar_get(ncin_ukesm,ncVarFileName)	# lon, lat, time


	nc_date = as.Date("1601-01-15") + ncvar_get(ncin_ukesm, 'time')# time is months after 1601-1-1
	nc_decades = (year(nc_date) %/% 10 ) *10
	nc_months = month(nc_date)
	nc_years_all = year(nc_date)
	nc_years_unique = unique(nc_years_all)
	missing_data = 1.00000002004088e+20
#	nonZeroGenerator = sample(seq(0.0001,0.001,length.out=1000), length(nc_date), replace = TRUE)
	monthsByDecade = seq(0,119,12)

	for(i in 1:length(nc_lat))	{
		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j, i, ] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				print(c(i, j))
				gfdl_all = c(nc_gfdl_init[j, i, initDates], nc_gfdl[j, i, -initDates]) * scalar
				mpi_all = c(nc_mpi_init[j, i, initDates], nc_mpi[j, i, -initDates]) * scalar
				ipsl_all = c(nc_ipsl_init[j, i, initDates], nc_ipsl[j, i, -initDates]) * scalar
				mri_all = c(nc_mri_init[j, i, initDates], nc_mri[j, i, -initDates]) * scalar
				ukesm_all = c(nc_ukesm_init[j, i, initDates], nc_ukesm[j, i, -initDates]) * scalar

				gfdl_yrly = NULL
				mpi_yrly = NULL
				ipsl_yrly = NULL
				mri_yrly = NULL
				ukesm_yrly = NULL
				for(thisYear in nc_years_unique)	{
					theseDates = which(nc_years_all == thisYear)
					gfdl_yrly =   c(gfdl_yrly,   sum(gfdl_all[theseDates]))
					mpi_yrly = c(mpi_yrly, sum(mpi_all[theseDates]))
					ipsl_yrly =   c(ipsl_yrly,   sum(ipsl_all[theseDates]))
					mri_yrly =  c(mri_yrly,  sum(mri_all[theseDates]))
					ukesm_yrly =  c(ukesm_yrly,  sum(ukesm_all[theseDates]))
				}

				eachDecade = 0
				for(thisDecade in whichDecades)	{
					eachDecade = eachDecade + 1
					theseYears = which(nc_decades == thisDecade)
						# accounting for the 2010s only beginning in 2015
					if(eachDecade == 1) { theseYears = rep(theseYears, 2) }
						# monthly averages
					for(thisMonth in 1:12)	{
						dataOutArray[j, i, eachDecade, thisMonth] =  mean(c(
							gfdl_all[theseYears][thisMonth + monthsByDecade], 
							mpi_all[theseYears][thisMonth + monthsByDecade],
							ipsl_all[theseYears][thisMonth + monthsByDecade],
							mri_all[theseYears][thisMonth + monthsByDecade],
							ukesm_all[theseYears][thisMonth + monthsByDecade]))
					}
						# annual averages
					subsetYears = which(nc_years_unique %in% seq(thisDecade, thisDecade+9, 1))
					dataOutArray[j, i, eachDecade, 13] =  mean(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]))
						# annual extremes
					dataOutArray[j, i, eachDecade, 14] =  quantile(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]), 0.05, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 15] =  quantile(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]), 0.15, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 16] =  quantile(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]), 0.25, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 17] =  quantile(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]), 0.5, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 18] =  quantile(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]), 0.75, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 19] =  quantile(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]), 0.85, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 20] =  quantile(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]), 0.95, na.rm=TRUE)
				}
			}
		}
	saveRDS(dataOutArray, file=paste0(ncpath, rcpScenarios[thisScen], 'data_out.rds'))
	}
	nc_close(ncin_gfdl)
	nc_close(ncin_mpi)
	nc_close(ncin_ipsl)
	nc_close(ncin_mri)
	nc_close(ncin_ukesm)
}
#dataOutArray = readRDS(file=paste0(ncpath, 'data_out.rds'))
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))
dataOutArray[ , , , 1, ] = readRDS(file=paste0(ncpath, rcpScenarios[1], 'data_out.rds'))
dataOutArray[ , , , 2, ] = readRDS(file=paste0(ncpath, rcpScenarios[2], 'data_out.rds'))
dataOutArray[ , , , 3, ] = readRDS(file=paste0(ncpath, rcpScenarios[3], 'data_out.rds'))


tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Potential Evapotranspiration (mm)'))
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
metadata = list(rcpScen = list(units = 'SSP_scenario'))
attr(rcpScen, 'variables') = metadata
names(dim(rcpScen)) = 'rcpScen'

valueClass = 1:20#valueType
dim(valueClass) = length(valueClass)
metadata = list(valueClass = list(units = 'class'))
attr(valueClass, 'variables') = metadata
names(dim(valueClass)) = 'valueClass'

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncOutputPath,'PotentialEvapotrasnpiration_decadalRawVals.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncOutputPath, 'PotentialEvapotrasnpiration_decadalRawVals.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])






#########################################
#########################################
#########################################
## Part 3: Groundwater Recharge
ncpath = "J:\\Cai_data\\TCFD\\GWrecharge\\"
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
ncVarFileName = 'qr'
initDates = 1:168

ncin_gfdl = nc_open(paste0(ncpath, 'clm45_gfdl-esm2m_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4'))
nc_gfdl_init = ncvar_get(ncin_gfdl,ncVarFileName)[ , , initDates]	# lon, lat, time
	# identifying lat lons before closing data
nc_lat = ncvar_get(ncin_gfdl, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_gfdl, 'lon')
nc_close(ncin_gfdl)

ncin_hadgem = nc_open(paste0(ncpath, 'clm45_hadgem2-es_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4'))
nc_hadgem_init = ncvar_get(ncin_hadgem,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_hadgem)

ncin_ipsl = nc_open(paste0(ncpath, 'clm45_ipsl-cm5a-lr_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4'))
nc_ipsl_init = ncvar_get(ncin_ipsl,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_ipsl)

ncin_miroc = nc_open(paste0(ncpath, 'clm45_miroc5_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4'))
nc_miroc_init = ncvar_get(ncin_miroc,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_miroc)


scalar = 60*60*24*30.4375 # kg m2 / sec to mm per month

	# array for holding outputs
myMissingData = NA
#dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
#	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(valueType)))




for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]
	rcpScen = paste0('rcp', rcpScenNum)

	ncname_gfdl = paste0('clm45_gfdl-esm2m_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName)	# lon, lat, time

	ncname_hadgem = paste0('clm45_hadgem2-es_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
	nc_hadgem = ncvar_get(ncin_hadgem,ncVarFileName)	# lon, lat, time
	
	ncname_ipsl = paste0('clm45_ipsl-cm5a-lr_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
	nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName)	# lon, lat, time
	
	ncname_miroc = paste0('clm45_miroc5_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
	nc_miroc = ncvar_get(ncin_miroc,ncVarFileName)	# lon, lat, time


	nc_date = as.Date("1661-01-01") + ncvar_get(ncin_miroc, 'time') * 30.4375# time is months after 1661-1-1
	nc_decades = (year(nc_date)%/% 10 ) *10
	nc_months = month(nc_date)
	nc_years_all = year(nc_date)
	nc_years_unique = unique(nc_years_all)
	missing_data = 1.00000002004088e+20
#	nonZeroGenerator = sample(seq(0.0001,0.001,length.out=1000), length(nc_date), replace = TRUE)
	monthsByDecade = seq(0,119,12)

	for(i in 1:length(nc_lat))	{
		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j,i, ] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				print(c(i, j))
				gfdl_all = c(nc_gfdl_init[j,i, initDates], nc_gfdl[j,i, -initDates]) * scalar
				hadgem_all = c(nc_hadgem_init[j,i, initDates], nc_hadgem[j,i, -initDates]) * scalar
				ipsl_all = c(nc_ipsl_init[j,i, initDates], nc_ipsl[j,i, -initDates]) * scalar
				miroc_all = c(nc_miroc_init[j,i, initDates], nc_miroc[j,i, -initDates]) * scalar

				gfdl_yrly = NULL
				hadgem_yrly = NULL
				ipsl_yrly = NULL
				miroc_yrly = NULL
				for(thisYear in nc_years_unique)	{
					theseDates = which(nc_years_all == thisYear)
					gfdl_yrly =   c(gfdl_yrly,   sum(gfdl_all[theseDates]))
					hadgem_yrly = c(hadgem_yrly, sum(hadgem_all[theseDates]))
					ipsl_yrly =   c(ipsl_yrly,   sum(ipsl_all[theseDates]))
					miroc_yrly =  c(miroc_yrly,  sum(miroc_all[theseDates]))
				}

				eachDecade = 0
				for(thisDecade in whichDecades)	{
					eachDecade = eachDecade + 1
					theseYears = which(nc_decades == thisDecade)
						# monthly averages
					for(thisMonth in 1:12)	{
						dataOutArray[j, i, eachDecade, thisMonth] =  mean(c(
							gfdl_all[theseYears][thisMonth + monthsByDecade], 
							hadgem_all[theseYears][thisMonth + monthsByDecade],
							ipsl_all[theseYears][thisMonth + monthsByDecade],
							miroc_all[theseYears][thisMonth + monthsByDecade]))
					}
						# annual averages
					subsetYears = which(nc_years_unique %in% seq(thisDecade, thisDecade+9, 1))
					dataOutArray[j, i, eachDecade, 13] =  mean(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]))
						# annual extremes
					dataOutArray[j, i, eachDecade, 14] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.05, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 15] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.15, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 16] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.25, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 17] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.5, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 18] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.75, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 19] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.85, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 20] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.95, na.rm=TRUE)
				}
			}
		}
	saveRDS(dataOutArray, file=paste0(ncpath, rcpScenarios[thisScen], 'data_out.rds'))
	}
	nc_close(ncin_gfdl)
	nc_close(ncin_hadgem)
	nc_close(ncin_ipsl)
	nc_close(ncin_miroc)
}
#dataOutArray = readRDS(file=paste0(ncpath, 'data_out.rds'))
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))
dataOutArray[ , , , 1, ] = readRDS(file=paste0(ncpath, rcpScenarios[1], 'data_out.rds'))
dataOutArray[ , , , 2, ] = readRDS(file=paste0(ncpath, rcpScenarios[2], 'data_out.rds'))
dataOutArray[ , , , 3, ] = readRDS(file=paste0(ncpath, rcpScenarios[3], 'data_out.rds'))

tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Groundwater Recharge (mm)'))
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

valueClass = 1:20#valueType
dim(valueClass) = length(valueClass)
metadata = list(valueClass = list(units = 'class'))
attr(valueClass, 'variables') = metadata
names(dim(valueClass)) = 'valueClass'

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncOutputPath,'GroundwaterRecharge_decadalRawVals.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncOutputPath, 'GroundwaterRecharge_decadalRawVals.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])

################################################









#########################################
## Part 4: Streamflow
###################################################
# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\SurfaceWater_Streamflow\\"
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
ncVarFileName = 'dis'
initDates = 1:60

	# initializing start decade
ncin_gfdl = nc_open(paste0(ncpath, paste0('cwatm_gfdl-esm4_w5e5_ssp126_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')))
nc_gfdl_init = ncvar_get(ncin_gfdl,ncVarFileName)[ , , initDates]	# lon, lat, time
	# identifying lat lons before closing data
nc_lat = ncvar_get(ncin_gfdl, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_gfdl, 'lon')
nc_close(ncin_gfdl)

ncin_mpi = nc_open(paste0(ncpath,  paste0('cwatm_mpi-esm1-2-hr_w5e5_ssp126_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')))
nc_mpi_init = ncvar_get(ncin_mpi,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_mpi)

ncin_ipsl = nc_open(paste0(ncpath, paste0('cwatm_ipsl-cm6a-lr_w5e5_ssp126_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')))
nc_ipsl_init = ncvar_get(ncin_ipsl,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_ipsl)

ncin_mri = nc_open(paste0(ncpath, paste0('cwatm_mri-esm2-0_w5e5_ssp126_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')))
nc_mri_init = ncvar_get(ncin_mri,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_mri)

ncin_ukesm = nc_open(paste0(ncpath, paste0('cwatm_ukesm1-0-ll_w5e5_ssp126_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')))
nc_ukesm_init = ncvar_get(ncin_ukesm,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_ukesm)


	# array for holding outputs
myMissingData = NA
#dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
#	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(valueType)))

scalar = 60*60*24*30.4375 / (1000^3) #m^3 / s to km^3 / month


for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = sspScenarios[thisScen]
	rcpScen = paste0('ssp', rcpScenNum)

	ncname_gfdl = paste0('cwatm_gfdl-esm4_w5e5_', rcpScen, '_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))

	ncname_mpi = paste0('cwatm_mpi-esm1-2-hr_w5e5_', rcpScen, '_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc') 
	ncin_mpi = nc_open(paste0(ncpath, ncname_mpi))

	ncname_ipsl = paste0('cwatm_ipsl-cm6a-lr_w5e5_', rcpScen, '_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc') 
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))

	ncname_mri =paste0('cwatm_mri-esm2-0_w5e5_', rcpScen, '_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')
	ncin_mri = nc_open(paste0(ncpath, ncname_mri))

	ncname_ukesm =  paste0('cwatm_ukesm1-0-ll_w5e5_', rcpScen, '_2015soc_default_', ncVarFileName, '_global_monthly_2015_2100.nc')
	ncin_ukesm = nc_open(paste0(ncpath, ncname_ukesm))

	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName)	# lon, lat, time
	nc_mpi = ncvar_get(ncin_mpi,ncVarFileName)	# lon, lat, time
	nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName)	# lon, lat, time
	nc_mri = ncvar_get(ncin_mri,ncVarFileName)	# lon, lat, time
	nc_ukesm = ncvar_get(ncin_ukesm,ncVarFileName)	# lon, lat, time


	nc_date = as.Date("1601-01-15") + ncvar_get(ncin_ukesm, 'time')# time is months after 1601-1-1
	nc_decades = (year(nc_date) %/% 10 ) *10
	nc_months = month(nc_date)
	nc_years_all = year(nc_date)
	nc_years_unique = unique(nc_years_all)
	missing_data = 1.00000002004088e+20
#	nonZeroGenerator = sample(seq(0.0001,0.001,length.out=1000), length(nc_date), replace = TRUE)
	monthsByDecade = seq(0,119,12)

	for(i in 1:length(nc_lat))	{
		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j, i, ] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				print(c(i, j))
				gfdl_all = c(nc_gfdl_init[j, i, initDates], nc_gfdl[j, i, -initDates]) * scalar
				mpi_all = c(nc_mpi_init[j, i, initDates], nc_mpi[j, i, -initDates]) * scalar
				ipsl_all = c(nc_ipsl_init[j, i, initDates], nc_ipsl[j, i, -initDates]) * scalar
				mri_all = c(nc_mri_init[j, i, initDates], nc_mri[j, i, -initDates]) * scalar
				ukesm_all = c(nc_ukesm_init[j, i, initDates], nc_ukesm[j, i, -initDates]) * scalar

				gfdl_yrly = NULL
				mpi_yrly = NULL
				ipsl_yrly = NULL
				mri_yrly = NULL
				ukesm_yrly = NULL
				for(thisYear in nc_years_unique)	{
					theseDates = which(nc_years_all == thisYear)
					gfdl_yrly =   c(gfdl_yrly,   sum(gfdl_all[theseDates]))
					mpi_yrly = c(mpi_yrly, sum(mpi_all[theseDates]))
					ipsl_yrly =   c(ipsl_yrly,   sum(ipsl_all[theseDates]))
					mri_yrly =  c(mri_yrly,  sum(mri_all[theseDates]))
					ukesm_yrly =  c(ukesm_yrly,  sum(ukesm_all[theseDates]))
				}

				eachDecade = 0
				for(thisDecade in whichDecades)	{
					eachDecade = eachDecade + 1
					theseYears = which(nc_decades == thisDecade)
						# accounting for the 2010s only beginning in 2015
					if(eachDecade == 1) { theseYears = rep(theseYears, 2) }
						# monthly averages
					for(thisMonth in 1:12)	{
						dataOutArray[j, i, eachDecade, thisMonth] =  mean(c(
							gfdl_all[theseYears][thisMonth + monthsByDecade], 
							mpi_all[theseYears][thisMonth + monthsByDecade],
							ipsl_all[theseYears][thisMonth + monthsByDecade],
							mri_all[theseYears][thisMonth + monthsByDecade],
							ukesm_all[theseYears][thisMonth + monthsByDecade]))
					}
						# annual averages
					subsetYears = which(nc_years_unique %in% seq(thisDecade, thisDecade+9, 1))
					dataOutArray[j, i, eachDecade, 13] =  mean(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]))
						# annual extremes
					dataOutArray[j, i, eachDecade, 14] =  quantile(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]), 0.05, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 15] =  quantile(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]), 0.15, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 16] =  quantile(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]), 0.25, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 17] =  quantile(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]), 0.5, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 18] =  quantile(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]), 0.75, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 19] =  quantile(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]), 0.85, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 20] =  quantile(c(gfdl_yrly[subsetYears], mpi_yrly[subsetYears], ipsl_yrly[subsetYears], mri_yrly[subsetYears], ukesm_yrly[subsetYears]), 0.95, na.rm=TRUE)
				}
			}
		}
	saveRDS(dataOutArray, file=paste0(ncpath, rcpScenarios[thisScen], 'data_out.rds'))
	}
	nc_close(ncin_gfdl)
	nc_close(ncin_mpi)
	nc_close(ncin_ipsl)
	nc_close(ncin_mri)
	nc_close(ncin_ukesm)
}
#dataOutArray = readRDS(file=paste0(ncpath, 'data_out.rds'))
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))
dataOutArray[ , , , 1, ] = readRDS(file=paste0(ncpath, rcpScenarios[1], 'data_out.rds'))
dataOutArray[ , , , 2, ] = readRDS(file=paste0(ncpath, rcpScenarios[2], 'data_out.rds'))
dataOutArray[ , , , 3, ] = readRDS(file=paste0(ncpath, rcpScenarios[3], 'data_out.rds'))


tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Streamflow (km^3)'))
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
metadata = list(rcpScen = list(units = 'SSP_scenario'))
attr(rcpScen, 'variables') = metadata
names(dim(rcpScen)) = 'rcpScen'

valueClass = 1:20#valueType
dim(valueClass) = length(valueClass)
metadata = list(valueClass = list(units = 'class'))
attr(valueClass, 'variables') = metadata
names(dim(valueClass)) = 'valueClass'

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncOutputPath,'Streamflow_decadalRawVals.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncOutputPath, 'Streamflow_decadalRawVals.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])









#########################################
#########################################
#########################################
## Part 5: Root Zone Soil Moisture
ncpath = "J:\\Cai_data\\TCFD\\RootZoneSoilMoisture\\"
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
ncVarFileName = 'rootmoist'
initDates = 1:168


ncin_gfdl = nc_open(paste0(ncpath, 'clm45_gfdl-esm2m_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4'))
nc_gfdl_init = ncvar_get(ncin_gfdl,ncVarFileName)[ , , initDates]	# lon, lat, time
	# identifying lat lons before closing data
nc_lat = ncvar_get(ncin_gfdl, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_gfdl, 'lon')
nc_close(ncin_gfdl)

ncin_hadgem = nc_open(paste0(ncpath, 'clm45_hadgem2-es_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4'))
nc_hadgem_init = ncvar_get(ncin_hadgem,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_hadgem)

ncin_ipsl = nc_open(paste0(ncpath, 'clm45_ipsl-cm5a-lr_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4'))
nc_ipsl_init = ncvar_get(ncin_ipsl,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_ipsl)

ncin_miroc = nc_open(paste0(ncpath, 'clm45_miroc5_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4'))
nc_miroc_init = ncvar_get(ncin_miroc,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_miroc)


scalar = 1 # mm water / top 100mm of soil

	# array for holding outputs
myMissingData = NA
#dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
#	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(valueType)))




for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]
	rcpScen = paste0('rcp', rcpScenNum)

	ncname_gfdl = paste0('clm45_gfdl-esm2m_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName)	# lon, lat, time

	ncname_hadgem = paste0('clm45_hadgem2-es_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
	nc_hadgem = ncvar_get(ncin_hadgem,ncVarFileName)	# lon, lat, time
	
	ncname_ipsl = paste0('clm45_ipsl-cm5a-lr_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
	nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName)	# lon, lat, time
	
	ncname_miroc = paste0('clm45_miroc5_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
	nc_miroc = ncvar_get(ncin_miroc,ncVarFileName)	# lon, lat, time


	nc_date = as.Date("1661-01-01") + ncvar_get(ncin_miroc, 'time') * 30.4375# time is months after 1661-1-1
	nc_decades = (year(nc_date)%/% 10 ) *10
	nc_months = month(nc_date)
	nc_years_all = year(nc_date)
	nc_years_unique = unique(nc_years_all)
	missing_data = 1.00000002004088e+20
#	nonZeroGenerator = sample(seq(0.0001,0.001,length.out=1000), length(nc_date), replace = TRUE)
	monthsByDecade = seq(0,119,12)

	for(i in 1:length(nc_lat))	{
		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j,i, ] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				print(c(i, j))
				gfdl_all = c(nc_gfdl_init[j,i, initDates], nc_gfdl[j,i, -initDates]) * scalar
				hadgem_all = c(nc_hadgem_init[j,i, initDates], nc_hadgem[j,i, -initDates]) * scalar
				ipsl_all = c(nc_ipsl_init[j,i, initDates], nc_ipsl[j,i, -initDates]) * scalar
				miroc_all = c(nc_miroc_init[j,i, initDates], nc_miroc[j,i, -initDates]) * scalar

				gfdl_yrly = NULL
				hadgem_yrly = NULL
				ipsl_yrly = NULL
				miroc_yrly = NULL
				for(thisYear in nc_years_unique)	{
					theseDates = which(nc_years_all == thisYear)
					gfdl_yrly =   c(gfdl_yrly,   mean(gfdl_all[theseDates]))
					hadgem_yrly = c(hadgem_yrly, mean(hadgem_all[theseDates]))
					ipsl_yrly =   c(ipsl_yrly,   mean(ipsl_all[theseDates]))
					miroc_yrly =  c(miroc_yrly,  mean(miroc_all[theseDates]))
				}

				eachDecade = 0
				for(thisDecade in whichDecades)	{
					eachDecade = eachDecade + 1
					theseYears = which(nc_decades == thisDecade)
						# monthly averages
					for(thisMonth in 1:12)	{
						dataOutArray[j, i, eachDecade, thisMonth] =  mean(c(
							gfdl_all[theseYears][thisMonth + monthsByDecade], 
							hadgem_all[theseYears][thisMonth + monthsByDecade],
							ipsl_all[theseYears][thisMonth + monthsByDecade],
							miroc_all[theseYears][thisMonth + monthsByDecade]))
					}
						# annual averages
					subsetYears = which(nc_years_unique %in% seq(thisDecade, thisDecade+9, 1))
					dataOutArray[j, i, eachDecade, 13] =  mean(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]))
						# annual extremes
					dataOutArray[j, i, eachDecade, 14] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.05, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 15] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.15, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 16] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.25, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 17] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.5, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 18] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.75, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 19] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.85, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 20] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.95, na.rm=TRUE)
				}
			}
		}
	saveRDS(dataOutArray, file=paste0(ncpath, rcpScenarios[thisScen], 'data_out.rds'))
	}
	nc_close(ncin_gfdl)
	nc_close(ncin_hadgem)
	nc_close(ncin_ipsl)
	nc_close(ncin_miroc)
}
#dataOutArray = readRDS(file=paste0(ncpath, 'data_out.rds'))
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))
dataOutArray[ , , , 1, ] = readRDS(file=paste0(ncpath, rcpScenarios[1], 'data_out.rds'))
dataOutArray[ , , , 2, ] = readRDS(file=paste0(ncpath, rcpScenarios[2], 'data_out.rds'))
dataOutArray[ , , , 3, ] = readRDS(file=paste0(ncpath, rcpScenarios[3], 'data_out.rds'))

tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Root Zone Soil Moisture'))
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

valueClass = 1:20#valueType
dim(valueClass) = length(valueClass)
metadata = list(valueClass = list(units = 'class'))
attr(valueClass, 'variables') = metadata
names(dim(valueClass)) = 'valueClass'

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncOutputPath,'RootZoneSoilMoisture_decadalRawVals.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncOutputPath, 'RootZoneSoilMoisture_decadalRawVals.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])





#########################################
#########################################
#########################################
## Part 6: Total Water Storage
ncpath = "J:\\Cai_data\\TCFD\\TotalWaterStorage\\"
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
ncVarFileName = 'tws'
initDates = 1:168

	# initializing the 2010s
ncin_gfdl = nc_open(paste0(ncpath, 'clm45_gfdl-esm2m_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4'))
nc_gfdl_init = ncvar_get(ncin_gfdl,ncVarFileName)[ , , initDates]	# lon, lat, time
	# identifying lat lons before closing data
nc_lat = ncvar_get(ncin_gfdl, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_gfdl, 'lon')
nc_close(ncin_gfdl)

ncin_hadgem = nc_open(paste0(ncpath, 'clm45_hadgem2-es_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4'))
nc_hadgem_init = ncvar_get(ncin_hadgem,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_hadgem)

ncin_ipsl = nc_open(paste0(ncpath, 'clm45_ipsl-cm5a-lr_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4'))
nc_ipsl_init = ncvar_get(ncin_ipsl,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_ipsl)

ncin_miroc = nc_open(paste0(ncpath, 'clm45_miroc5_ewembi_rcp26_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4'))
nc_miroc_init = ncvar_get(ncin_miroc,ncVarFileName)[ , , initDates]	# lon, lat, time
nc_close(ncin_miroc)


scalar = 1 # kg / m2 = mm

	# array for holding outputs
myMissingData = NA
#dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
#	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(valueType)))



for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]
	rcpScen = paste0('rcp', rcpScenNum)

	ncname_gfdl = paste0('clm45_gfdl-esm2m_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
	nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName)	# lon, lat, time

	ncname_hadgem = paste0('clm45_hadgem2-es_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
	nc_hadgem = ncvar_get(ncin_hadgem,ncVarFileName)	# lon, lat, time
	
	ncname_ipsl = paste0('clm45_ipsl-cm5a-lr_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
	nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName)	# lon, lat, time
	
	ncname_miroc = paste0('clm45_miroc5_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
	ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
	nc_miroc = ncvar_get(ncin_miroc,ncVarFileName)	# lon, lat, time


	nc_date = as.Date("1661-01-01") + ncvar_get(ncin_miroc, 'time') * 30.4375# time is months after 1661-1-1
	nc_decades = (year(nc_date)%/% 10 ) *10
	nc_months = month(nc_date)
	nc_years_all = year(nc_date)
	nc_years_unique = unique(nc_years_all)
	missing_data = 1.00000002004088e+20
#	nonZeroGenerator = sample(seq(0.0001,0.001,length.out=1000), length(nc_date), replace = TRUE)
	monthsByDecade = seq(0,119,12)

	for(i in 1:length(nc_lat))	{
		for(j in 1:length(nc_lon))	{
			nc_dummy = nc_gfdl[j,i, ] # reading in one data set to test for nona
			if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
				print(c(i, j))
				gfdl_all = c(nc_gfdl_init[j,i, initDates], nc_gfdl[j,i, -initDates]) * scalar
				hadgem_all = c(nc_hadgem_init[j,i, initDates], nc_hadgem[j,i, -initDates]) * scalar
				ipsl_all = c(nc_ipsl_init[j,i, initDates], nc_ipsl[j,i, -initDates]) * scalar
				miroc_all = c(nc_miroc_init[j,i, initDates], nc_miroc[j,i, -initDates]) * scalar

				gfdl_yrly = NULL
				hadgem_yrly = NULL
				ipsl_yrly = NULL
				miroc_yrly = NULL
				for(thisYear in nc_years_unique)	{
					theseDates = which(nc_years_all == thisYear)
					gfdl_yrly =   c(gfdl_yrly,   mean(gfdl_all[theseDates]))
					hadgem_yrly = c(hadgem_yrly, mean(hadgem_all[theseDates]))
					ipsl_yrly =   c(ipsl_yrly,   mean(ipsl_all[theseDates]))
					miroc_yrly =  c(miroc_yrly,  mean(miroc_all[theseDates]))
				}

				eachDecade = 0
				for(thisDecade in whichDecades)	{
					eachDecade = eachDecade + 1
					theseYears = which(nc_decades == thisDecade)
						# monthly averages
					for(thisMonth in 1:12)	{
						dataOutArray[j, i, eachDecade, thisMonth] =  mean(c(
							gfdl_all[theseYears][thisMonth + monthsByDecade], 
							hadgem_all[theseYears][thisMonth + monthsByDecade],
							ipsl_all[theseYears][thisMonth + monthsByDecade],
							miroc_all[theseYears][thisMonth + monthsByDecade]))
					}
						# annual averages
					subsetYears = which(nc_years_unique %in% seq(thisDecade, thisDecade+9, 1))
					dataOutArray[j, i, eachDecade, 13] =  mean(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]))
						# annual extremes
					dataOutArray[j, i, eachDecade, 14] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.05, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 15] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.15, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 16] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.25, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 17] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.5, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 18] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.75, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 19] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.85, na.rm=TRUE)
					dataOutArray[j, i, eachDecade, 20] =  quantile(c(gfdl_yrly[subsetYears], hadgem_yrly[subsetYears], ipsl_yrly[subsetYears], miroc_yrly[subsetYears]), 0.95, na.rm=TRUE)
				}
			}
		}
	saveRDS(dataOutArray, file=paste0(ncpath, rcpScenarios[thisScen], 'data_out.rds'))
	}
	nc_close(ncin_gfdl)
	nc_close(ncin_hadgem)
	nc_close(ncin_ipsl)
	nc_close(ncin_miroc)
}
#dataOutArray = readRDS(file=paste0(ncpath, 'data_out.rds'))
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), length(rcpScenarios), length(valueType)))
dataOutArray[ , , , 1, ] = readRDS(file=paste0(ncpath, rcpScenarios[1], 'data_out.rds'))
dataOutArray[ , , , 2, ] = readRDS(file=paste0(ncpath, rcpScenarios[2], 'data_out.rds'))
dataOutArray[ , , , 3, ] = readRDS(file=paste0(ncpath, rcpScenarios[3], 'data_out.rds'))

tcfdVariable = dataOutArray
metadata = list(tcfdVariable = list(units = 'Total Water Storage (mm)'))
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

valueClass = 1:20#valueType
dim(valueClass) = length(valueClass)
metadata = list(valueClass = list(units = 'class'))
attr(valueClass, 'variables') = metadata
names(dim(valueClass)) = 'valueClass'

	# saving ncdf
ArrayToNc(list(tcfdVariable, lon, lat, decade, rcpScen, valueClass), file_path = paste0(ncOutputPath,'TotalWaterStorage_decadalRawVals.nc'))

	# testing output, squinty eye test
myNC = nc_open(paste0(ncOutputPath, 'TotalWaterStorage_decadalRawVals.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

image(nc_lon, rev(nc_lat), nc_testDat[,,1,1,1])




