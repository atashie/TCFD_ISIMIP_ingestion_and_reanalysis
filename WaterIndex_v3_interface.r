# load necessary functions
	# gw specific functions
source('C:\\Users\\arik\\Documents\\GitHub\\TCFD_ISIMIP_ingestion_and_reanalysis\\WaterIndex_v3_functions.R')



# customer data
ncFileLoc = 'J:\\Cai_data\\WaterIndex\\'		#C:\\Users\\18033\\Documents\\CaiData\\ncFiles\\
customerFolder = 'J:\\Cai_data\\Rabo\\Locations\\' # 'C:\\Users\\18033\\Documents\\CaiData\\temp_locationsForRabo\\Locations\\'
clientName = 'AutomatedWRI_test'#	'BeefNW' #'WestHillsFarms' #'AgricolaSanOsvaldo'#	'AgricolaPachacama' #' AgricolaSanTelmo	

customerTable = data.table::fread(paste0(customerFolder, clientName, '\\', 'Customer Onboarding Information_test.csv'),#'Customer Onboarding Information_BNW.csv'),#'Customer Onboarding Information_WestHillsFarms.csv'),#'Customer Onboarding Information_BNW.csv'), 
	skip = 1) #'Customer_Hazards_and_Locations-Rabobank_grid - Sheet1.csv'
locationHeader = 'Location (name)'




##########################################################################################################################################################################################
# running the data
climateDataSelection_f(	
#	climVars = c('Precipitation_decadalRawVals', 'PotentialEvapotranspiration_decadalRawVals', 'GroundwaterRecharge_decadalRawVals', 'Streamflow_decadalRawVals',	'RootZoneSoilMoisture_decadalRawVals', 'TotalWaterStorage_decadalRawVals'),
	climVars = c("waterIndexUnderlyingData_precip", "waterIndexUnderlyingData_potevap","waterIndexUnderlyingData_qr", "waterIndexUnderlyingData_dis",	"waterIndexUnderlyingData_rootmoist", "waterIndexUnderlyingData_tws"),
	ncFileLoc = ncFileLoc,
	customerTable_input = customerTable,
	clientName = clientName,
	locationHeader = locationHeader,
	historicData = FALSE
	)

climateDataPlotting_f(	
	climVars = c("waterIndexUnderlyingData_precip", "waterIndexUnderlyingData_potevap","waterIndexUnderlyingData_qr", "waterIndexUnderlyingData_dis",	"waterIndexUnderlyingData_rootmoist", "waterIndexUnderlyingData_tws"),
	ncFileLoc = ncFileLoc,
	climVarNames = c('Precip (mm)', 'PET (mm)', 'GW Rech (mm)', 'Streamflow (km^2)', 'RZ Soil Moisture (%)', 'Total Storage (mm)'),
	customerTable_input = customerTable,
	scenarioNames = c('LowEmissions', 'MedEmissions', 'HighEmissions'),
	rawDataColumnNames = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Mean', 'Q05', 'Q15', 'Q25', 'Q50', 'Q75', 'Q85', 'Q95'),
	clientName_input = clientName,
	customerFolder_input = customerFolder,
	locationHeader = locationHeader,
	historicData = FALSE
	)

waterIndexCalculations_f(	
	customerTable_input = customerTable,
	petGlobAvg = 2000,
	growSeason = 1:12,
	frcAreaUnderCult = 0.2,
	runoffRatio = 0.2,
	humidAI = 0.65,
	pwpSoil = 22, 
	wPlant = 900,# for alfalfa #1010  pistachio in ther san J; c(900, 1200) # citrus
	divertibleStrmfl = 0.25,
	strmflCaptureRatio =0.1,
	clientName_input = clientName,
	customerFolder_input = customerFolder,
	indexValues = c('Aridity Index - Avg', 'Soil Moisture Stress - Avg', 'Plant Water Demand - Avg', 'Aridity Index - Drought', 'Soil Moisture Stress - Drought', 'Plant Water Demand - Drought', 'Aridity Index w/ Irrigation - Avg', 'Plant Water Demand w/ Irrigation - Avg', 'Aridity Index w/ Irrigation - Drought', 'Plant Water Demand w/ Irrigation - Drought')
	)

waterIndexPlotter_f(	
	customerTable_input = customerTable,
	clientName_input = clientName,
	scenarioNames = c('LowEmissions', 'MedEmissions', 'HighEmissions'),
	customerFolder_input = customerFolder,
	locationHeader = locationHeader,
	climVars = c('Precipitation_decadalRawVals', 'PotentialEvapotranspiration_decadalRawVals', 'GroundwaterRecharge_decadalRawVals', 'Streamflow_decadalRawVals',	'RootZoneSoilMoisture_decadalRawVals', 'TotalWaterStorage_decadalRawVals'),
	ncFileLoc = ncFileLoc,
	indexValues = c('Aridity Index - Avg', 'Soil Moisture Stress - Avg', 'Plant Water Demand - Avg', 'Aridity Index - Drought', 'Soil Moisture Stress - Drought', 'Plant Water Demand - Drought', 'Aridity Index w/ Irrigation - Avg', 'Plant Water Demand w/ Irrigation - Avg', 'Aridity Index w/ Irrigation - Drought', 'Plant Water Demand w/ Irrigation - Drought')
	)

gracePlotter_f(
	customerTable_input = customerTable,
	customerFolder_input = customerFolder,
	clientName_input = clientName,
	graceDataLoc = 'J:\\Cai_data\\Rabo\\GRACE\\GRCTellus.JPL.200204_202211.GLO.RL06.1M.MSCNv03CRI.nc'
	)



