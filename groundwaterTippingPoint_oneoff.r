	# gw specific functions
source('C:\\Users\\arik\\Documents\\GitHub\\seasonalForecasting_otherClimate_\\groundwaterModelingGRACEonly_functions.R')
source('C:\\Users\\arik\\Documents\\GitHub\\seasonalForecasting_otherClimate_\\groundwaterModelingGRACEtileSelection_functions.R')
source('C:\\Users\\arik\\Documents\\GitHub\\seasonalForecasting_otherClimate_\\longtermClimateReanalysis_functions.R')
	# basin delineation functions
source('C:\\Users\\arik\\Documents\\GitHub\\SurfaceWaterProjections\\surfaceWaterModeling_functions.r')


# file storage information
basinName = 'BespokeGW_nrSanAntonioTexas_Apr2024' #'BeefNW'
dataOut_location = paste0('J:\\Cai_data\\Rabo\\Locations\\', basinName, '\\')

# locations information
customerLocations = data.table::fread(paste0('J:\\Cai_data\\Rabo\\Locations\\', basinName, '\\', 'Customer Onboarding Information_', basinName, '.csv'), skip=1)
locationLon = customerLocations$Longitude[1]
locationLat = customerLocations$Latitude[1]
locationName = unlist(customerLocations[1, 'Location (name)'])
scenNames = c('ssp126', 'ssp245', 'ssp585')


era5DataNCDF =  paste0('J:\\Cai_data\\Rabo\\Locations\\', basinName, '\\', basinName, '-testing-recent-era.nc')
era5StartDate =  as.Date('1998-08-01') #as.Date('1980-01-01') # + ncvar_get(ncin_era5, 'time') for calculating actual dates 
cmip6DataFile =  paste0('J:\\Cai_data\\Rabo\\Locations\\', basinName, '\\cmip6ncs\\')
cmip6StartDate =  as.Date('1850-01-01') #as.Date('1980-01-01') # + ncvar_get(ncin_era5, 'time') for calculating actual dates 




#########################################################################################################
	# step 1
	# delineate a new basin
basinDelineation_f(
	gageLonLat = c(locationLon, locationLat),
	basinATLAS_locationAndFile = 'J:\\Cai_data\\BasinATLAS_Data_v10\\BasinATLAS_v10.gdb',
	simpleBasinAtlas = FALSE, # if we are not using layers
	dataOut_location = dataOut_location,
	basinName)


#########################################################################################################
	# step 2
	# import and convert historical climate data
climateInputConversion_f(
	basinName = basinName,
	climateDataNCDF = era5DataNCDF,
	tempConversionFactor = NA,
	pptConversionFactor = NA,
	avgTempGiven = FALSE, 
	startDate = era5StartDate, 	# when does the clock of the netcdf start?
	timeToDaysConversion = 1,	# convert time increments to days if necessary
	dataOut_location = dataOut_location,
	optionForPET = 1, 	# 1 = PET_fromTemp modified Pen-Mon, 
	variableOrderOption = 'era5', # # 'era5' = [longitude,latitude,time]; 'cfs' = [longitude, latitude, member, step]; 'seas5' = [longitude, latitude, member, lead_time] for tmax and tmin but [lead_time, longitude, latitude, member] for tp_sum]
	precipName = 'tp_sum')	# other options include: tp, tp_sum	
	


#########################################################################################################
	# step 3 
	# grace time series for location of interest
	# grace tile selection w/ smoothing
graceTS = graceTileSelection_f(
	graceFileLoc = 'J:\\Cai_data\\Rabo\\GRACE\\',
	graceFileName = 'GRCTellus.JPL.200204_202211.GLO.RL06.1M.MSCNv03CRI.nc',
	locationLat = locationLat,
	locationLon = locationLon,
	reweightingExponent = 2)
	

	# selection of neighboring grace tiles w/ limited smoothing
graceNeighborTS = graceNeighborTileSelection_f(
	graceFileLoc = 'J:\\Cai_data\\Rabo\\GRACE\\',
	graceFileName = 'GRCTellus.JPL.200204_202211.GLO.RL06.1M.MSCNv03CRI.nc',
	locationLat = locationLat,
	locationLon = locationLon,
	reweightingExponent = 0.25)
	




###### regional water availability

waterDemand = (populationScalar * populationTotal) * (waterPerPersonScalar * waterPerPersonTotal)
effectiveET = pet
effectiveRecharge

