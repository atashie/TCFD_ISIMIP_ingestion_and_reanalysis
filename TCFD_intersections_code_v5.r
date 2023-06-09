########################################################################################################################
#reading in functions and libraries
library(data.table)
library(ncdf4)
source('C:\\Users\\arik\\Documents\\GitHub\\TCFD_ISIMIP_ingestion_and_reanalysis\\TCFD_intersections_code_functions_v5.r')
#source('~\\TCFD_ISIMIP_ingestion_and_reanalysis\\TCFD_intersections_code_functions_v5.r')
thisWD = 'J:\\Cai_data\\TCFD\\'	# '~//'

########################################################################################################################
# paths and values that are unlikely to change between runs
thisDate = Sys.Date()
hazardFolder = paste0(thisWD, 'ProcessedNCs\\')
ncpathRiverFloods = paste0(thisWD, 'Flash Floods\\')
floodMapTiffLoc = paste0(thisWD, 'CurrentFloodHazard')
waterMaskLoc = paste0(thisWD, 'CurrentFloodHazard\\LandMaskArray.rds')
ncpathDEM = paste0(thisWD, 'SeaLevelRise\\globalDEM\\srtm30plus_v11_land.nc')
ncpathSeaLevelRise = paste0(thisWD, 'SeaLevelRise\\sea_level_change')
locationFootprint = 3		# how big is the footprint of the location of interest? in number of 'boxes' to search to the left and right (so 0 is equal to 1 km^2, 1 is 3x3=9 km^2, 2 is 5x5=25 km^2, 3 is 7x7=49, 4 is 9x9=81, etc.
appendedHazardNames = c("River Flood (Local)", 'Coastal Flood', 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
aggScoreExceptions = c('Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
aggScoreExceptionsValues = c(0, 38, 30)
#startTime = proc.time() only used for testing run times

########################################################################################################################
# new customer data
# !!!!!!!!!!!!!!!!!!!!!!!!
# this is the only section that needs to be updated between runs
# !!!!!!!!!!!!!!!!!!!!!!!!

# 	# ASR
userName = 'ASR'	
customerFolder = 'J:\\Cai_data\\TCFD\\locations\\ASR_May2023\\'

customerTable = fread(paste0(customerFolder, 'Customer_Hazards_and_Locations-ASR-May2023 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
hazardTable = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Definitions.csv'))							# 
relHazScores = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Scores.csv'))				
hazardWeighting = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Weights.csv'))				
appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\ASR_May2023\\ASR_temp_precip_hazards_may_17.csv'
waterOnly = FALSE


# 	# Nuveen FL
userName = 'Nuveen'	
customerFolder = 'J:\\Cai_data\\TCFD\\locations\\Nuveen_May2023\\'

customerTable = fread(paste0(customerFolder, 'Customer_Hazards_and_Locations-Nuveen_May2023 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
hazardTable = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Definitions.csv'))							# 
relHazScores = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Scores.csv'))				
hazardWeighting = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Weights.csv'))				
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\ASR_May2023\\ASR_temp_precip_hazards_may_17.csv'
waterOnly = TRUE


########################################################################################################################
# functions to run analysis


f_mainExposureData(
	customerTable = customerTable,
	customerFolder = customerFolder,
	hazardFolder = hazardFolder,
	hazardTable = hazardTable,
	relHazScores = relHazScores,
	appendedHazardNames = appendedHazardNames,
	thisDate = thisDate)

f_riverFloods(
	customerTable = customerTable,
	customerFolder = customerFolder,
	userName = userName,
	locationFootprint = locationFootprint,
	ncpath = ncpathRiverFloods,
	fileLoc = floodMapTiffLoc,
	waterMaskLoc = waterMaskLoc,
	thisDate = thisDate)

f_seaLevelRise(
	customerTable = customerTable,
	customerFolder = customerFolder,
	userName = userName,
	ncpathDEM = ncpathDEM,
	ncpathSeaLevelRise = ncpathSeaLevelRise,
	thisDate = thisDate)

f_hazardAggregation(
	customerTable = customerTable,
	customerFolder = customerFolder,
	thisDate = thisDate,
	userName = userName,
	waterOnly = waterOnly,
	appendedHazardFileLoc = appendedHazardFileLoc,
	locationFootprint = locationFootprint,
	hazardWeighting = hazardWeighting)



###################################
# end of exposure; subsequent lines are not necessary to run new exposure data
###################################









################################
# old runs

	# corbion
#userName = 'Corbion'	
#customerFolder = 'J:\\Cai_data\\TCFD\\locations\\Corbion_Nov2022\\'

#customerTable = fread(paste0(customerFolder, 'Corbion_Locations_Nov2022 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
#hazardTable = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Definitions.csv'))							# 
#relHazScores = fread(paste0(customerFolder, 'Hazard_Tables_Corbion_Nov2022 - Hazard Scores.csv'))				
#appendedHazardNames = c("River Flood (Local)", 'Coastal Flood', 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\Corbion_Nov2022\\Corbion_temp_precip_hazards_mar_16.csv'
#aggScoreExceptions = c('Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#aggScoreExceptionsValues = c(0, 38, 30)

	# ITC
#userName = 'ITC'	
#customerFolder = 'J:\\Cai_data\\TCFD\\locations\\ITC_Dec2022\\'

#customerTable = fread(paste0(customerFolder, 'ITC_Locations_Dec2022 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
#hazardTable = fread(paste0(customerFolder, 'Hazard_Tables_ITC_Dec2022 - Hazard Definitions.csv'))							# 
#relHazScores = fread(paste0(customerFolder, 'Hazard_Tables_ITC_Dec2022 - Hazard Scores.csv'))				
#appendedHazardNames = c("River Flood (Local)", 'Coastal Flood', 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\ITC_Dec2022\\ITC_temp_precip_hazards_dec_6 - ITC_temp_precip_hazards_dec_6.csv'

	# ITC - Water
#userName = 'ITC - Water'	
#customerFolder = 'J:\\Cai_data\\TCFD\\locations\\ITC_Dec2022\\'

#customerTable = fread(paste0(customerFolder, 'ITC_Locations_Dec2022 - Water Exposure.csv')) #'HMClause_locations_allCucurbit.csv'
#hazardTable = fread(paste0(customerFolder, 'Hazard_Tables_ITC_Dec2022 - Hazard Definitions.csv'))							# 
#relHazScores = fread(paste0(customerFolder, 'Hazard_Tables_ITC_Dec2022 - Hazard Scores.csv'))				
#appendedHazardNames = c("River Flood (Local)", 'Coastal Flood', 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\ITC_Dec2022\\ITC_temp_precip_hazards_dec_6 - ITC_temp_precip_hazards_dec_6.csv'

	# Driscolls
#userName = 'Driscolls'	
#customerFolder = 'J:\\Cai_data\\TCFD\\locations\\Driscolls_Dec2022\\'

#customerTable = fread(paste0(customerFolder, 'Driscolls_Locations_Dec2022 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
#hazardTable = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Definitions.csv'))							# 
#relHazScores = fread(paste0(customerFolder, 'Hazard_Tables_Driscolls_Dec2022 - Hazard Scores.csv'))				
#appendedHazardNames = c("River Flood (Local)", 'Coastal Flood', 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\Driscolls_Dec2022\\Driscolls_temp_precip_hazards_dec_6 - Driscolls_temp_precip_hazards_dec_6.csv'
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\Driscolls_Dec2022\\Sales_davesData.csv'


	# HMClause
#userName = 'HMClause'	
#customerFolder = 'J:\\Cai_data\\TCFD\\locations\\HMClause_Dec2022\\'

#customerTable = fread(paste0(customerFolder, 'HMClause_Locations_Nov2022 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
#hazardTable = fread(paste0(customerFolder, 'Hazard_Tables_HMClause_Dec2022 - Hazard Definitions.csv'))							# 
#relHazScores = fread(paste0(customerFolder, 'Hazard_Tables_HMClause_Dec2022 - Hazard Scores.csv'))				
#appendedHazardNames = c("River Flood (Local)", 'Coastal Flood', 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\HMClause_Dec2022\\HMClause_temp_precip_hazards_dec_6 - HMClause_temp_precip_hazards_dec_6.csv'


	# Nuveen - Spain
#userName = 'Nuveen - Spain'	
#customerFolder = 'J:\\Cai_data\\TCFD\\locations\\Nuveen_Feb2023\\'

#customerTable = fread(paste0(customerFolder, 'Customer_Hazards_and_Locations-Nuveen_Jan2023 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
#hazardTable = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Definitions.csv'))							# 
#relHazScores = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Scores.csv'))				
#appendedHazardNames = c("River Flood (Local)", 'Coastal Flood', 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\HMClause_Dec2022\\HMClause_temp_precip_hazards_dec_6 - HMClause_temp_precip_hazards_dec_6.csv'


	# EQ Cap
#userName = 'Sales'	
#customerFolder = 'J:\\Cai_data\\TCFD\\locations\\EQCap_Feb2023\\'
#thisDate = Sys.Date()

#customerTable = fread(paste0(customerFolder, 'EQCap_Locations_Dec2022 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
#hazardTable = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Definitions.csv'))							# 
#relHazScores = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Scores.csv'))				
#appendedHazardNames = c("River Flood (Local)", 'Coastal Flood', 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\EQCap_Feb2023\\EQ_cap_temp_precip_hazards_feb_11.csv'


	# suntory - Japan Demo
#userName = 'Kurita'	
#customerFolder = 'J:\\Cai_data\\TCFD\\locations\\Sunotory_Feb2023\\'

#customerTable = fread(paste0(customerFolder, 'Suntory - Hops Supplier Locations - For_exposure.csv')) #'HMClause_locations_allCucurbit.csv'
#hazardTable = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Definitions.csv'))							# 
#relHazScores = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Scores.csv'))				
#appendedHazardNames = c("River Flood (Local)", 'Coastal Flood', 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\Sunotory_Feb2023\\Kurita_temp_precip_hazards_feb_18.csv'

	# Indigo - South Africa
#userName = 'Indigo'	
#customerFolder = 'J:\\Cai_data\\TCFD\\locations\\Indigo_Feb2023\\'

#customerTable = fread(paste0(customerFolder, 'SA - Exposure Locations - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
#hazardTable = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Definitions.csv'))							# 
#relHazScores = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Scores.csv'))				
#appendedHazardNames = c("River Flood (Local)", 'Coastal Flood', 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\Indigo_Feb2023\\Indigo_temp_precip_hazards_feb_22.csv'

	# UBS Demo
#userName = 'UBS'	
#customerFolder = 'J:\\Cai_data\\TCFD\\locations\\UBS_Feb2023\\'

#customerTable = fread(paste0(customerFolder, 'UBS Demo - Exposure Locations - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
#hazardTable = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Definitions.csv'))							# 
#relHazScores = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Scores.csv'))				
#appendedHazardNames = c("River Flood (Local)", 'Coastal Flood', 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\UBS_Feb2023\\UBS_temp_precip_hazards_feb_26.csv'

	# ICL Demo
#userName = 'ICL'	
#customerFolder = 'J:\\Cai_data\\TCFD\\locations\\ICL-Demo_Mar2023\\'

#customerTable = fread(paste0(customerFolder, 'Customer_Hazards_and_Locations-ICL_Demo - Sheet1.csv'))
#hazardTable = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Definitions.csv'))							# 
#relHazScores = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Scores.csv'))				
#appendedHazardNames = c("River Flood (Local)", 'Coastal Flood', 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\ICL-Demo_Mar2023\\ICL_temp_precip_hazards_mar_21.csv'
#aggScoreExceptions = c('Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#aggScoreExceptionsValues = c(0, 38, 30)

	# Nuveen - Georgia
userName = 'Nuveen - Georgia'	
customerFolder = 'J:\\Cai_data\\TCFD\\locations\\Nubeen_Apr2023\\'

customerTable = fread(paste0(customerFolder, 'Customer_Hazards_and_Locations-Nuveen_Apr2023 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
hazardTable = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Definitions.csv'))							# 
relHazScores = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Scores.csv'))				
appendedHazardNames = NA
appendedHazardFileLoc = NA
aggScoreExceptions = c('Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
aggScoreExceptionsValues = c(0, 38, 30)
waterOnly = TRUE

	# Nuveen - Romania
#userName = 'Nuveen - Romania'	
#customerFolder = 'J:\\Cai_data\\TCFD\\locations\\Nuveen_Apr2023b\\'

#customerTable = fread(paste0(customerFolder, 'Customer_Hazards_and_Locations-Nuveen_Apr2023 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
#hazardTable = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Definitions.csv'))							# 
#relHazScores = fread(paste0(customerFolder, 'Hazard_Tables - Hazard Scores.csv'))				
#appendedHazardNames = NA
#appendedHazardFileLoc = NA
#aggScoreExceptions = c('Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#aggScoreExceptionsValues = c(0, 38, 30)







################################ 
#### just checkin data
dataOutput = fread(paste0(customerFolder, 'processedOutputForAllHazards_', userName, '_', thisDate, '.csv'))

summary(subset(dataOutput, Hazard == 'Coastal Flood')$Raw_Hazard_Value - subset(dataOutput, Hazard == 'Coastal Flood')$Raw_Hazard_Value_25th)
summary(subset(dataOutput, Hazard == 'Coastal Flood')$Raw_Hazard_Value - subset(dataOutput, Hazard == 'Coastal Flood')$Raw_Hazard_Value_75th)
summary(subset(dataOutput, Hazard != 'Coastal Flood')$Raw_Hazard_Value - subset(dataOutput, Hazard != 'Coastal Flood')$Raw_Hazard_Value_25th)
summary(subset(dataOutput, Hazard != 'Coastal Flood')$Raw_Hazard_Value - subset(dataOutput, Hazard != 'Coastal Flood')$Raw_Hazard_Value_75th)
#dataOutput[which(dataOutput$Hazard == 'Coastal Flood'), ]$Raw_Hazard_Value_25th[which(subset(dataOutput, Hazard == 'Coastal Flood')$Raw_Hazard_Value > subset(dataOutput, Hazard == 'Coastal Flood')$Raw_Hazard_Value_25th)] = 
#	subset(dataOutput, Hazard == 'Coastal Flood')$Raw_Hazard_Value[which(subset(dataOutput, Hazard == 'Coastal Flood')$Raw_Hazard_Value > subset(dataOutput, Hazard == 'Coastal Flood')$Raw_Hazard_Value_25th)]
#dataOutput[which(dataOutput$Hazard == 'Coastal Flood'), ]$Raw_Hazard_Value_75th[which(subset(dataOutput, Hazard == 'Coastal Flood')$Raw_Hazard_Value < subset(dataOutput, Hazard == 'Coastal Flood')$Raw_Hazard_Value_75th)] = 
#	subset(dataOutput, Hazard == 'Coastal Flood')$Raw_Hazard_Value[which(subset(dataOutput, Hazard == 'Coastal Flood')$Raw_Hazard_Value < subset(dataOutput, Hazard == 'Coastal Flood')$Raw_Hazard_Value_75th)]


summary(subset(dataOutput, Decade == 2090 & Scenario == "Middle of the Road" & Hazard == 'Water Scarcity'))
summary(subset(dataOutput, Decade == 2090 & Scenario == "High Emissions" & Hazard == 'Aggregate Climate Score' & Hazard_Measure == 'Aggregate Score'))
summary(subset(dataOutput, Decade == 2090 & Scenario == "High Emissions" & Hazard == 'Aggregate Climate Score' & Hazard_Measure == 'Weighted Aggregate Score'))
summary(subset(dataOutput, Decade == 2090 & Scenario == "Middle of the Road" & Hazard == 'Aggregate Climate Score'))
summary(subset(dataOutput, Decade == 2090 & Scenario == "Low Emissions" & Hazard == 'Aggregate Climate Score'))
par(mfrow = c(3,3))
thisLoc = 5
thisScen = unique(dataOutput$Scenario)[2]
plot(subset(dataOutput, Scenario == thisScen & Hazard == 'Water Scarcity' & Location == customerTable$Location[thisLoc])$Percentile)	
plot(subset(dataOutput, Scenario == thisScen & Hazard == 'Hurricanes' & Location == customerTable$Location[thisLoc])$Percentile)	
plot(subset(dataOutput, Scenario == thisScen & Hazard == 'River Flood (Regional)' & Location == customerTable$Location[thisLoc])$Percentile)	
plot(subset(dataOutput, Scenario == thisScen & Hazard == 'River Flood (Local)' & Location == customerTable$Location[thisLoc])$Percentile)	
plot(subset(dataOutput, Scenario == thisScen & Hazard == 'Coastal Flood' & Location == customerTable$Location[thisLoc])$Percentile)	
plot(subset(dataOutput, Scenario == thisScen & Hazard == 'Water Availability' & Location == customerTable$Location[thisLoc])$Percentile)	
plot(subset(dataOutput, Scenario == thisScen & Hazard == 'Intense Precipitation' & Location == customerTable$Location[thisLoc])$Percentile)	
plot(subset(dataOutput, Scenario == thisScen & Hazard == 'Extreme Heat' & Location == customerTable$Location[thisLoc])$Percentile)	
plot(subset(dataOutput, Scenario == thisScen & Hazard == 'Extreme Cold' & Location == customerTable$Location[thisLoc])$Percentile)	

plot(subset(dataOutput, Scenario == thisScen & Hazard == 'Water Scarcity' & Location == customerTable$Location[thisLoc])$Raw_Hazard_Value)	
plot(subset(dataOutput, Scenario == thisScen & Hazard == 'Water Availability' & Location == customerTable$Location[thisLoc])$Raw_Hazard_Value)	
plot(subset(dataOutput, Scenario == 'Middle of the Road' & Hazard == 'Coastal Flood' & Location == customerTable$Location[1])$Raw_Hazard_Value)	
plot(subset(dataOutput, Scenario == thisScen & Hazard == 'Aggregate Climate Score' & Location %in% customerTable$Location)$Percentile)	
	
proc.time() - startTime




# identifying the top n worst / best performing
totalTrend = NULL
scarcityTrend = NULL
availabilityTrend = NULL
dataTrends = data.frame(User = NA, Location = NA, Hazard = NA, Hazard_Measure = NA, Raw_Hazard_Current_Value = NA, Raw_Hazard_Trend_to_2050s = NA, Percentile_Current_Value = NA, Percentile_Trend_to_2050s = NA)
for(thisLoc in unique(dataOutput$Location))	{
	for(thisHazard in unique(dataOutput$Hazard))	{
		dataSub = subset(dataOutput, Location == thisLoc & Scenario == "Middle of the Road" & Hazard == thisHazard)
	
		for(thisHazardMeasure in unique(dataSub$Hazard_Measure))	{
			dataTrends = rbind(dataTrends,
						data.frame(
							User = userName,
							Location = thisLoc,
							Hazard = thisHazard,
							Hazard_Measure = thisHazardMeasure,
							Raw_Hazard_Current_Value = mean(subset(dataSub, Decade %in% c('2010','2020') & Hazard_Measure == thisHazardMeasure)$Raw_Hazard_Value),
							Raw_Hazard_Trend_to_2050s = mean(subset(dataSub, Decade %in% c('2040','2050','2060') & Hazard_Measure == thisHazardMeasure)$Raw_Hazard_Value) - 
								mean(subset(dataSub, Decade %in% c('2010', '2020') & Hazard_Measure == thisHazardMeasure)$Raw_Hazard_Value), 
							Percentile_Current_Value = mean(subset(dataSub, Decade %in% c('2010','2020') & Hazard_Measure == thisHazardMeasure)$Percentile), 
							Percentile_Trend_to_2050s = mean(subset(dataSub, Decade %in% c('2040','2050','2060') & Hazard_Measure == thisHazardMeasure)$Percentile) - 
								mean(subset(dataSub, Decade %in% c('2010', '2020') & Hazard_Measure == thisHazardMeasure)$Percentile)
							)
						)
		}
	}

	
	
	totalTrend = c(totalTrend, mean(subset(dataOutput, Location == thisLoc & Scenario == "Middle of the Road" & Hazard == "Aggregate Climate Score" & Decade %in% c("2010", "2020"))$Percentile) - 
		mean(subset(dataOutput, Location == thisLoc & Scenario == "Middle of the Road" & Hazard == "Aggregate Climate Score" & Decade %in% c("2040", "2050", "2060"))$Percentile))
	scarcityTrend = c(scarcityTrend, mean(subset(dataOutput, Location == thisLoc & Scenario == "Middle of the Road" & Hazard == "Water Availability" & Hazard_Measure == "Aggregate Score" & Decade %in% c("2010", "2020"))$Percentile) - 
		mean(subset(dataOutput, Location == thisLoc & Scenario == "Middle of the Road" & Hazard == "Water Availability" & Hazard_Measure == "Aggregate Score" & Decade %in% c("2040", "2050", "2060"))$Percentile))
	availabilityTrend = c(availabilityTrend, mean(subset(dataOutput, Location == thisLoc & Scenario == "Middle of the Road" & Hazard == "Water Scarcity" & Hazard_Measure == "Aggregate Score" & Decade %in% c("2010", "2020"))$Percentile) - 
		mean(subset(dataOutput, Location == thisLoc & Scenario == "Middle of the Road" & Hazard == "Water Scarcity" & Hazard_Measure == "Aggregate Score" & Decade %in% c("2040", "2050", "2060"))$Percentile))
}	
fwrite(dataTrends, 'C:\\Users\\18033\\Downloads\\georgiaHighlights.csv')



## reformatting for a table view
dataOutputReformatted = data.table(Location = NA, Scenario = NA, Variable = NA, 
	D2010s= NA, D2020s= NA, D2030s= NA, D2040s= NA, D2050s= NA, D2060s= NA, D2070s= NA, D2080s= NA, D2090s= NA, Trend = NA)
for(thisLoc in unique(dataOutput$Location))	{
	for(thisScen in unique(dataOutput$Scenario))	{
		for(thisVariable in unique(dataOutput$Hazard_Measure))	{
			if(thisVariable != 'Aggregate Score')	{
				theseOutputs = subset(dataOutput, Location == thisLoc & Scenario == thisScen & Hazard_Measure == thisVariable)
				dataOutputReformatted = rbind(dataOutputReformatted, 
					data.table(
						Location = thisLoc,
						Scenario = thisScen,
						Variable = thisVariable,
						D2010s= subset(theseOutputs, Decade == 2010)$Raw_Hazard_Value, 
						D2020s= subset(theseOutputs, Decade == 2020)$Raw_Hazard_Value, 
						D2030s= subset(theseOutputs, Decade == 2030)$Raw_Hazard_Value, 
						D2040s= subset(theseOutputs, Decade == 2040)$Raw_Hazard_Value, 
						D2050s= subset(theseOutputs, Decade == 2050)$Raw_Hazard_Value, 
						D2060s= subset(theseOutputs, Decade == 2060)$Raw_Hazard_Value, 
						D2070s= subset(theseOutputs, Decade == 2070)$Raw_Hazard_Value, 
						D2080s= subset(theseOutputs, Decade == 2080)$Raw_Hazard_Value, 
						D2090s= subset(theseOutputs, Decade == 2090)$Raw_Hazard_Value,
						Trend = subset(theseOutputs, Decade == 2090)$Decadal_Trend_Strength)
					)
			}
		}
	}
}
fwrite(dataOutputReformatted, 'C:\\Users\\18033\\Downloads\\georgiaDataSimplified.csv')
				
dataOutputSuperSimple = data.table(Variable = NA, 
	D2010s= NA, D2020s= NA, D2030s= NA, D2040s= NA, D2050s= NA, D2060s= NA, D2070s= NA, D2080s= NA, D2090s= NA, Trend = NA)
thisScen = 'Middle of the Road'
for(thisVariable in unique(dataOutputReformatted$Variable))	{
	theseOutputs = subset(dataOutputReformatted, Scenario == thisScen & Variable == thisVariable)
	dataOutputSuperSimple = rbind(dataOutputSuperSimple, 
		data.table(
			Variable = thisVariable,
			D2010s= mean(theseOutputs$D2010), 
			D2020s= mean(theseOutputs$D2020), 
			D2030s= mean(theseOutputs$D2030), 
			D2040s= mean(theseOutputs$D2040), 
			D2050s= mean(theseOutputs$D2050), 
			D2060s= mean(theseOutputs$D2060), 
			D2070s= mean(theseOutputs$D2070), 
			D2080s= mean(theseOutputs$D2080), 
			D2090s= mean(theseOutputs$D2090),
			Trend = mean(theseOutputs$Trend))
	)
}
fwrite(dataOutputSuperSimple, 'C:\\Users\\18033\\Downloads\\georgiaDataSuperSimplified.csv')



theScenarios = c('RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5')
scenarioRename = c('Low Emissions', 'Middle of the Road', 'Middle of the Road', 'High Emissions')

for(thisLoc in unique(dataOutput$Location))	{
	locRow = which(customerTable$Location == thisLoc)
	avgOfAllHazards = NULL
	for(thisHazard in unique(dataOutput$Hazard))	{
		newHazard = subset(dataOutput, Decade == thisDecade & Scenario == theScenarios[thisScen] & Location == thisLoc & Hazard == thisHazard)
					# catching exceptions for when we do not have 4.5 or 8.5
				if(nrow(newHazard) == 0 | any(is.na(newHazard$Percentile_Score)))	{
					newHazard = subset(dataOutput, Decade == thisDecade & Scenario == theScenarios[thisScen - 1] & Location == thisLoc & Hazard == thisHazard)
				}
				dataOutput = rbind(dataOutput,
					data.frame(
						User = userName,
						Location = customerTable$Location[locRow],
						Region = customerTable$Region[locRow],
						Subregion = customerTable$Subregion[locRow],
						Lat = customerTable$Lat[locRow],
						Lon = customerTable$Lon[locRow],
						Hazard = thisHazard,
						Hazard_Measure = 'Aggregate Score',
						Decade = thisDecade,
						Scenario = scenarioRename[thisScen],
						Raw_Hazard_Value = NA,
						Percentile_Score = mean(newHazard$Percentile_Score, na.rm=TRUE),
						Relative_Hazard_Score = NA,
						Decadal_Trend_Strength = NA,
						Decadal_Trend_Significance = NA,
						Long_Term_Trend_Strength = NA,
						Long_Term_Trend_Significance = NA,
						Relative_Hazard_Score_Number = NA,
						Trend_Aggregated_For_Looker = NA,
						Advanced_Data_Measures = NA,
						Advanced_Data_Measures_Units = NA,
						Raw_Hazard_Value_25th = NA,
						Raw_Hazard_Value_75th = NA)

				)

				avgOfAllHazards = c(avgOfAllHazards, mean(newHazard$Percentile_Score, na.rm=TRUE))
			}

			dataOutput = rbind(dataOutput,
				data.frame(
					User = userName,
					Location = customerTable$Location[locRow],
					Region = customerTable$Region[locRow],
					Subregion = customerTable$Subregion[locRow],
					Lat = customerTable$Lat[locRow],
					Lon = customerTable$Lon[locRow],
					Hazard = "Aggregate Climate Score",
					Hazard_Measure = "Aggregate Score",
					Decade = thisDecade,
					Scenario = scenarioRename[thisScen],
					Raw_Hazard_Value = NA,
					Percentile_Score = mean(avgOfAllHazards, na.rm=TRUE),
					Relative_Hazard_Score = NA,
					Decadal_Trend_Strength = NA,
					Decadal_Trend_Significance = NA,
					Long_Term_Trend_Strength = NA,
					Long_Term_Trend_Significance = NA,
					Relative_Hazard_Score_Number = NA,
					Trend_Aggregated_For_Looker = NA,
					Advanced_Data_Measures = NA,
					Advanced_Data_Measures_Units = NA,
					Raw_Hazard_Value_25th = NA,
					Raw_Hazard_Value_75th = NA)
			)
		}
	}
}















unique(dataOutput$Region)
dataOutput$Region[which(dataOutput$Region == 'US')] = 'USA'
	
# sample analysis
	
	gg = data.frame(country = unique(dataOutput$Region))
for(j in unique(dataOutput$Decade))	{
	hh = NULL
	for(i in unique(dataOutput$Region)){
#		hh = c(hh, median(subset(dataOutput, Region == i & Hazard == 'Aggregate Climate Score' & Decade == j)$Percentile))
#		hh = c(hh, median(subset(dataOutput, Region == i & Hazard == 'Drought' & Decade == j)$Percentile))
#		hh = c(hh, median(subset(dataOutput, Region == i & Hazard == 'Water Availability' & Decade == j)$Percentile))
#		hh = c(hh, median(subset(dataOutput, Region == i & Hazard == 'Water Demand' & Decade == j)$Percentile))
		hh = c(hh, median(subset(dataOutput, Region == i & Hazard == 'Snow Pack' & Decade == j)$Percentile))
		print(i)
	}
	print(j)
	gg = cbind(gg, hh)
}


names(gg) = c('Country', 2010,2020,2030,2040,2050,2060,2070,2080,2090)
fwrite(gg, 'J://Downloads//medianSWEScore.csv')


dataSub = subset(dataOutput, Scenario == "Middle of the Road" & Hazard_Measure =='Groundwater Recharge (mm per yr)' & Region =="China")
#dataSub = subset(dataOutput, Scenario == "Middle of the Road" & Hazard_Measure =='Root Zone Soil Moisture (mm per yr)' & Region =="China")
plot(subset(chinaSM, Location == unique(chinaSM$Location)[1])$Trend_Agg)
for(i in unique(chinaSM$Location)){ lines(subset(chinaSM, Location == i)$Trend_Agg)}

trend_avg = matrix(NA, nrow=9, ncol=6)
trend_sum = matrix(NA, nrow=9, ncol=6)
trend_diff = matrix(NA, nrow=9, ncol=6)
trend_signif_diff = matrix(NA, nrow=9, ncol=6)
iter = 0
for(i in unique(dataSub$Decade))	{
	iter = iter + 1
	trend_avg[iter,] = summary(subset(dataSub, Decade == i)$Trend_Agg)
	trend_sum[iter,] = summary(subset(dataSub, Decade == i)$Trend_Agg) * iter
	trend_diff[iter,] = summary(subset(dataSub, Decade == i)$Raw_Hazard_Value - subset(dataSub, Decade == 2010)$Raw_Hazard_Value)
	trend_signif_diff[iter,] = summary(subset(dataSub, Decade == i & Long_Term_Trend_Significance < 0.05)$Raw_Hazard_Value - subset(dataSub, Decade == 2010 & Long_Term_Trend_Significance < 0.05)$Raw_Hazard_Value)
}
trend_avg=as.data.frame(trend_avg)
trend_sum=as.data.frame(trend_sum)
trend_diff=as.data.frame(trend_diff)
trend_signif_diff=as.data.frame(trend_signif_diff)
names(trend_avg) = c('Min','Q1','Med','Mean','Q3','Max')
names(trend_sum) = c('Min','Q1','Med','Mean','Q3','Max')
names(trend_diff) = c('Min','Q1','Med','Mean','Q3','Max')
names(trend_signif_diff) = c('Min','Q1','Med','Mean','Q3','Max')
row.names(trend_avg) = seq(2010,2090,10)
row.names(trend_sum) = seq(2010,2090,10)
row.names(trend_diff) = seq(2010,2090,10)
row.names(trend_signif_diff) = seq(2010,2090,10)
fwrite(trend_sum, 'total_rate_of_change_by_decade_GW.csv')
fwrite(trend_avg, 'avg_rate_of_change_by_decade_GW.csv')
fwrite(trend_diff, 'median_difference_by_decade_GW.csv')
fwrite(trend_signif_diff, 'median_difference_of_significant_differences_by_decade_GW.csv')



dataSummary = data.frame(Country = NA, Region = NA, Measure = NA, the_2010s = NA, the_2020s = NA, the_2030s = NA, the_2040s = NA, the_2050s = NA, the_2060s = NA, the_2070s = NA, the_2080s = NA, the_2090s = NA)
for(thisMeasure in c("Groundwater Recharge (mm per yr)", "Root Zone Soil Moisture (mm per yr)", "Surface Water (annual total in cubic km)")){
	measureOutput = subset(dataOutput, Hazard_Measure == thisMeasure)
	for(cc in unique(measureOutput$Region)){
		countryOutput = subset(measureOutput, Region == cc)
		dataSummary = rbind(dataSummary,
			data.frame(
				Country = cc,
				Region = "All",
				Measure = thisMeasure,
				the_2010s = mean(subset(countryOutput, Decade == 2010)$Trend_Aggregated_For_Looker),
				the_2020s = mean(subset(countryOutput, Decade == 2020)$Trend_Aggregated_For_Looker),
				the_2030s = mean(subset(countryOutput, Decade == 2030)$Trend_Aggregated_For_Looker),
				the_2040s = mean(subset(countryOutput, Decade == 2040)$Trend_Aggregated_For_Looker),
				the_2050s = mean(subset(countryOutput, Decade == 2050)$Trend_Aggregated_For_Looker),
				the_2060s = mean(subset(countryOutput, Decade == 2060)$Trend_Aggregated_For_Looker),
				the_2070s = mean(subset(countryOutput, Decade == 2070)$Trend_Aggregated_For_Looker),
				the_2080s = mean(subset(countryOutput, Decade == 2080)$Trend_Aggregated_For_Looker),
				the_2090s = mean(subset(countryOutput, Decade == 2090)$Trend_Aggregated_For_Looker)
		))

		for(ss in unique(countryOutput$Subregion))	{
			subregOutput = subset(countryOutput, Subregion == ss)	
			dataSummary = rbind(dataSummary,
				data.frame(
					Country = cc,
					Region = ss,
					Measure = thisMeasure,
					the_2010s = mean(subset(subregOutput, Decade == 2010)$Trend_Aggregated_For_Looker),
					the_2020s = mean(subset(subregOutput, Decade == 2020)$Trend_Aggregated_For_Looker),
					the_2030s = mean(subset(subregOutput, Decade == 2030)$Trend_Aggregated_For_Looker),
					the_2040s = mean(subset(subregOutput, Decade == 2040)$Trend_Aggregated_For_Looker),
					the_2050s = mean(subset(subregOutput, Decade == 2050)$Trend_Aggregated_For_Looker),
					the_2060s = mean(subset(subregOutput, Decade == 2060)$Trend_Aggregated_For_Looker),
					the_2070s = mean(subset(subregOutput, Decade == 2070)$Trend_Aggregated_For_Looker),
					the_2080s = mean(subset(subregOutput, Decade == 2080)$Trend_Aggregated_For_Looker),
					the_2090s = mean(subset(subregOutput, Decade == 2090)$Trend_Aggregated_For_Looker)
			))
		}
	}
}
fwrite(dataSummary, 'Trends.csv')



dataSummary = data.frame(Country = NA, Region = NA, Measure = NA, the_2010s = NA, the_2020s = NA, the_2030s = NA, the_2040s = NA, the_2050s = NA, the_2060s = NA, the_2070s = NA, the_2080s = NA, the_2090s = NA)
for(thisMeasure in c("Groundwater Recharge (mm per yr)", "Root Zone Soil Moisture (mm per yr)", "Surface Water (annual total in cubic km)")){
	measureOutput = subset(dataOutput, Hazard_Measure == thisMeasure & Long_Term_Trend_Significance < 0.05)
	for(cc in unique(measureOutput$Region)){
		countryOutput = subset(measureOutput, Region == cc)
		dataSummary = rbind(dataSummary,
			data.frame(
				Country = cc,
				Region = "All",
				Measure = thisMeasure,
				the_2010s = mean(subset(countryOutput, Decade == 2010)$Raw_Hazard_Value - subset(countryOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2020s = mean(subset(countryOutput, Decade == 2020)$Raw_Hazard_Value - subset(countryOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2030s = mean(subset(countryOutput, Decade == 2030)$Raw_Hazard_Value - subset(countryOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2040s = mean(subset(countryOutput, Decade == 2040)$Raw_Hazard_Value - subset(countryOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2050s = mean(subset(countryOutput, Decade == 2050)$Raw_Hazard_Value - subset(countryOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2060s = mean(subset(countryOutput, Decade == 2060)$Raw_Hazard_Value - subset(countryOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2070s = mean(subset(countryOutput, Decade == 2070)$Raw_Hazard_Value - subset(countryOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2080s = mean(subset(countryOutput, Decade == 2080)$Raw_Hazard_Value - subset(countryOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2090s = mean(subset(countryOutput, Decade == 2090)$Raw_Hazard_Value - subset(countryOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE)
		))

		for(ss in unique(countryOutput$Subregion))	{
			subregOutput = subset(countryOutput, Subregion == ss)	
			dataSummary = rbind(dataSummary,
				data.frame(
					Country = cc,
					Region = ss,
					Measure = thisMeasure,
				the_2010s = mean(subset(subregOutput, Decade == 2010)$Raw_Hazard_Value - subset(subregOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2020s = mean(subset(subregOutput, Decade == 2020)$Raw_Hazard_Value - subset(subregOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2030s = mean(subset(subregOutput, Decade == 2030)$Raw_Hazard_Value - subset(subregOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2040s = mean(subset(subregOutput, Decade == 2040)$Raw_Hazard_Value - subset(subregOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2050s = mean(subset(subregOutput, Decade == 2050)$Raw_Hazard_Value - subset(subregOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2060s = mean(subset(subregOutput, Decade == 2060)$Raw_Hazard_Value - subset(subregOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2070s = mean(subset(subregOutput, Decade == 2070)$Raw_Hazard_Value - subset(subregOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2080s = mean(subset(subregOutput, Decade == 2080)$Raw_Hazard_Value - subset(subregOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE),
				the_2090s = mean(subset(subregOutput, Decade == 2090)$Raw_Hazard_Value - subset(subregOutput, Decade == 2010)$Raw_Hazard_Value, na.rm=TRUE)
				))
		}
	}
}

fwrite(dataSummary, 'Differences.csv')
fwrite(dataSummary[which(abs(dataSummary[,-c(1:3)] , 'Differences.csv')










gg = fread('J:\\Cai_data\\TCFD\\locations\\ITC_Dec2022\\processedOutputForAllHazards_ITC - Water_2022-12-07.csv')
for(i in unique(gg$Decade))	{
	print(sum(subset(gg, Decade == i & Scenario == 'Low Emissions' & Hazard_Measure == 'Surface Water (annual total in cubic km)' & Region == 'India')$Trend_Aggregated_For_Looker))
}	

	
	
	
	
# some sanity checking
		
subset(dataOutput, Hazard != 'Aggregate Climate Score' & Hazard_Measure == 'Aggregate Score' & Scenario == 'Middle of the Road')[1:99,6:12]
subset(dataOutput, Hazard_Measure == 'Aggregate Score' & Scenario == 'Middle of the Road')[1:99,c(2,7:12)]
subset(dataOutput, Hazard_Measure == 'Aggregate Score' & Scenario == 'Low Emissions')[1:99,c(2,7:12)]
subset(dataOutput, Hazard_Measure == 'Aggregate Score' & Scenario == 'High Emissions')[1:99,c(2,7:12)]

summary(subset(dataOutput, Hazard != 'Aggregate Climate Score' & Hazard_Measure == 'Aggregate Score' & Scenario == 'Low Emissions')$Percentile)
summary(subset(dataOutput, Hazard == 'Aggregate Climate Score' & Hazard_Measure == 'Aggregate Score' & Scenario == 'Low Emissions')$Percentile)

summary(subset(dataOutput, Hazard != 'Aggregate Climate Score' & Hazard_Measure == 'Aggregate Score' & Scenario == 'Middle of the Road')$Percentile)
summary(subset(dataOutput, Hazard == 'Aggregate Climate Score' & Hazard_Measure == 'Aggregate Score' & Scenario == 'Middle of the Road')$Percentile)

summary(subset(dataOutput, Hazard != 'Aggregate Climate Score' & Hazard_Measure == 'Aggregate Score' & Scenario == 'High Emissions')$Percentile)
summary(subset(dataOutput, Hazard == 'Aggregate Climate Score' & Hazard_Measure == 'Aggregate Score' & Scenario == 'High Emissions')$Percentile)

for(thisScen in unique(dataOutput$Scenario))	{
	for(thisLoc in unique(dataOutput$Location))	{
		dataSub = subset(dataOutput, Scenario == thisScen & Location == thisLoc & Hazard == 'Aggregate Climate Score' & Hazard_Measure == 'Aggregate Score')
		png(filename=paste0('J:\\Downloads\\', substr(thisLoc, 1, 10), " - ", thisScen, '.png'))
		plot(dataSub$Decade, dataSub$Percentile, ylim = c(1,100), lwd = 3, type = 'l',
			ylab = 'Percentile', xlab = 'Decade', main = paste0(substr(thisLoc, 1, 10), " - ", thisScen))
		for(thisHazard in unique(dataOutput$Hazard))	{
			
			dataSub = subset(dataOutput, Scenario == thisScen & Location == thisLoc & Hazard == thisHazard & Hazard_Measure == 'Aggregate Score')
			lines(dataSub$Decade, dataSub$Percentile, col = 'grey30', lwd = 1.5)
		}
		dev.off()
	}
}

	
	
	
	
	
	
	#############################
	########## old ##############
	#############################
	
	
	
	
	
ncSurname = '_processed.nc'
burntarea_nc = paste0('burntarea', ncSurname)
dis_nc = paste0('dis', ncSurname)
groundwstor_nc = paste0('groundwstor', ncSurname)
riverflood_nc = paste0('ler', ncSurname)
tropcyclones_nc = paste0('let', ncSurname)
pft_mai_rainfed_nc = paste0('pft-mai-rained', ncSurname)
qr_nc = paste0('qr', ncSurname)
rootmoist_nc = paste0('rootmoist', ncSurname)
swe_nc = paste0('swe', ncSurname)
yield_soy_firr_nc = paste0('yield-soy-firr', ncSurname)
yield_soy_noirr_nc = paste0('yield-soi-noirr', ncSurname)







# reading in hazard maps
wfMaps = fread('J:\\Cai_data\\TCFD\\BurntArea\\burntarea_allData_30MAY2022.csv')
gwMaps = fread('J:\\Cai_data\\TCFD\\GWstorage\\groundwater_nonegs_allData_30MAY2022.csv')
smMaps = fread('J:\\Cai_data\\TCFD\\RootZoneSoilMoisture\\rzsm_allData_30MAY2022.csv')
wsMaps = fread('J:\\Cai_data\\TCFD\\WaterScarcity\\WaterScarcity_allData_03JUN2022.csv')
#tcMaps = fread('J:\\Cai_data\\TCFD\\TropicalCycloneArea\\tropicalCyclone_allData_30JUN2022.csv')
mzMaps = fread('J:\\Cai_data\\TCFD\\MaizeRainfed\\maizeProductivityUnirrigated_allData_30JUN2022.csv')
rfMaps = fread('J:\\Cai_data\\TCFD\\RiverFloodArea\\RiverFloodArea_allData_01JUL2022.csv')
sweMaps =fread('J:\\Cai_data\\TCFD\\SWE\\SWE_allData_30MAY2022.csv')

	

# reading in customer database
customerPath = 'J:\\Cai_data\\TCFD\\'
#customerCsv = 'CustomerLocData_Simplot.csv'
#customerTable = fread(paste0(customerPath, customerCsv))
#customerCsv = 'walmartstoreloaction.csv'
#customerTable = fread(paste0(customerPath, customerCsv))[seq(1,4655,10), c('name', 'state', 'city', 'latitude', 'longitude')]
#names(customerTable) = c('Name', 'Region', 'Subregion', 'Lat', 'Lon')
customerCsv = 'Richs-sample-locations.csv'
customerTable = fread(paste0(customerPath, customerCsv))

closeLats = NULL
closeLons = NULL
for(j in 1:nrow(customerTable))	{
	closeLons = c(closeLons, hazardMaps$Lon[which.min(abs(customerTable$Lon[j] - hazardMaps$Lon))])
	closeLats = c(closeLats, hazardMaps$Lat[which.min(abs(customerTable$Lat[j] - hazardMaps$Lat))])
}

closeLonLats = NULL
theseLabels = NULL
theseRegions = NULL
theseSubregions = NULL
for(k in 1:length(closeLons))	{
	newCloseLonLats = which(hazardMaps$Lat == closeLats[k] & hazardMaps$Lon == closeLons[k])
	closeLonLats = c(closeLonLats, newCloseLonLats)
	theseLabels = c(theseLabels, rep(customerTable$Name[k], length(newCloseLonLats)))
	theseRegions = c(theseRegions, rep(customerTable$Region[k], length(newCloseLonLats)))
	theseSubregions = c(theseSubregions, rep(customerTable$Subregion[k], length(newCloseLonLats)))
}


customerHazards = hazardMaps[closeLonLats,]
customerHazards$Regions = theseRegions
customerHazards$Subregions = theseSubregions
#customerHazards$Labels = unlist(strsplit(theseLabels, ','))[seq(1,(length(theseLabels)*2),2)]
customerHazards$Labels = theseLabels

			library(psych)
			customerAggHaz = cbind(as.numeric(subset(customerHazards, Hazard == "Fire (% Area Burned)")$Q_25_indx),
				as.numeric(subset(customerHazards, Hazard == "Groundwater (mm)")$Q_25_indx),
				as.numeric(subset(customerHazards, Hazard == "Water Scarcity (% Storage Remaining)")$Q_25_indx),
				as.numeric(subset(customerHazards, Hazard == 'Maize Productivity (unirrigated)')$Q_25_indx),
				as.numeric(subset(custoemrHazards, Hazard == 'Hundred Year Flood (% Area Impacted)')$Q_25_indx))
			customerAggHaz_df =	subset(customerHazards, Hazard == "Fire (% Area Burned)")
			customerAggHaz_df$Hazard =	'Aggregated Score'
			customerAggHaz_df[, c('Q_25_abs','Q_50_abs','Q_75_abs','Q_25_rel','Q_50_rel','Q_75_rel','Q_75_indx','Likelihood')] = NA
			customerAggHaz_df$Q_25_indx = apply(customerAggHaz, 1, geometric.mean)	

			customerHazards = rbind(customerHazards, customerAggHaz_df)

customerHazards$ID = 1:nrow(customerHazards)

customerHazards$Q_50_indx = 'Low'
customerHazards$Q_50_indx[which(customerHazards$Q_25_indx > 0.50)] = 'Medium'
customerHazards$Q_50_indx[which(customerHazards$Q_25_indx > 0.75)] = 'High'


thisdf = subset(customerHazards, Hazard == 'Maize Productivity (unirrigated)' & Decade == '2010s') 
plot(thisdf$Lon, thisdf$Lat, col=c('blue','orange','red')[as.numeric(thisdf$Q_50_indx)],lwd=3)

fwrite(customerHazards, paste0(customerPath, 'RichsSampleLocs_13JUL2022.csv'))
