##########################################################################################################################################################################################
##########################################################################################################################################################################################
library(ncdf4)
library(data.table)

# variable options
climVars = c('Precipitation_decadalRawVals', 'PotentialEvapotranspiration_decadalRawVals', 'GroundwaterRecharge_decadalRawVals', 'Streamflow_decadalRawVals',
	'RootZoneSoilMoisture_decadalRawVals', 'TotalWaterStorage_decadalRawVals')
ncFileLoc = 'J:\\Cai_data\\WaterIndex\\'		#C:\\Users\\18033\\Documents\\CaiData\\ncFiles\\
rawDataColumnNames = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Mean', 'Q05', 'Q15', 'Q25', 'Q50', 'Q75', 'Q85', 'Q95')

# reading in customer data
userName = 'Rabo'	
customerFolder = 'J:\\Cai_data\\Rabo\\Locations\\' # 'C:\\Users\\18033\\Documents\\CaiData\\temp_locationsForRabo\\Locations\\'
clientName = 'AgricolaSanOsvaldo'#	'AgricolaPachacama' #' AgricolaSanTelmo	BeefNW
thisDate = Sys.Date()

customerTable = data.table::fread(paste0(customerFolder, clientName, '\\', 'Customer Onboarding Information_agricolaSanOsvaldo.csv'),#'Customer Onboarding Information_BNW.csv'), 
	skip = 1) #'Customer_Hazards_and_Locations-Rabobank_grid - Sheet1.csv'
locationHeader = 'Location (name)'

# defining constants
	# !!!!!!!!!!!
	# todo:
	# runoffRatio = f(ppt, rzsm)
	# wPlant = f(plant, pet)
	# divertibleQ = f(Q)
	# !!!!!!!!!!!
	
runoffRatio = 0.2
humidAI = 0.65
pwpSoil = 22 # 
wPlant = 900 # for alfalfa #1010  pistachio in ther san J; c(900, 1200) # citrus
growSeason = 2:9
petGlobAvg = 2000 #1500 #
divertibleStrmfl = 0.25
strmflCaptureRatio = 0.1
frcAreaUnderCult = 0.15
hectToSqKm = 100
halfDegInKM = 111.1 / 2
kmToMm = 1000^2



##########################################################
# 3A- parsing, smoothing, and plotting the raw climate+hydro data

# filling arrays with required climate data
for(thisClimVar in 1:length(climVars))	{
	print(paste0(thisClimVar, ' out of ', length(climVars), ' climate variables'))
	ncName = climVars[thisClimVar]
	nc_file =  nc_open(paste0(ncFileLoc, ncName, '.nc'))
	nc_lat = ncvar_get(nc_file, 'lat')	# lat is given from high to low
	nc_lon = ncvar_get(nc_file, 'lon')
	nc_decade = ncvar_get(nc_file, 'decade')
	nc_scenario = ncvar_get(nc_file, 'rcpScen')
	nc_valueClass = ncvar_get(nc_file, 'valueClass')
	nc_values = ncvar_get(nc_file, 'tcfdVariable') # Lon, Lat, Decade (2010-2090, Scenarios (Low, Med, High), Value Type (1-12: months, 13: annual mean, 14:20: annual Q05, Q15, Q25, Q50, Q75, Q85, Q95)

	# defining array for holding climate data
	if(thisClimVar == 1)	{
		myMissingData = -10^5
		climateData = array(rep(myMissingData, nrow(customerTable) * length(nc_decade) * length(nc_valueClass) * length(nc_scenario) * length(climVars)), 
			dim = c(nrow(customerTable), length(nc_decade), length(nc_valueClass), length(nc_scenario), length(climVars)))
	}

	for(thisLoc in 1:nrow(customerTable))	{
		closestLat = which.min(abs(nc_lat - customerTable$Lat[thisLoc]))
		closestLon = which.min(abs(nc_lon - customerTable$Lon[thisLoc]))

			# smoothing / downscaling and reweighting based on distance
		closeishLats = rep(closestLat + c(-1,0,1), 3)
		closeishLons = rep(closestLon + c(-1,0,1), each = 3)
		closeishLatsVals = (nc_lat[closeishLats] - customerTable$Lat[thisLoc])^2
		closeishLonsVals = (nc_lon[closeishLons] - customerTable$Lon[thisLoc])^2
		thisExponent = ifelse(climVars[thisClimVar] == 'Streamflow_decadalRawVals', 2, .5) 		# streamflow inputs may require a wider search
		distanceBox = sqrt(closeishLatsVals + closeishLonsVals)
#		boxWeighting = max(distanceBox, na.rm=TRUE) - distanceBox
		boxWeighting = 1 - distanceBox # since max distance of a 2x2 .5 deg grid is 1 deg

			# un-weighting water / ocean tiles
		for(thisIter in 1:length(closeishLats))	{
			if(is.na(nc_values[closeishLons[thisIter], closeishLats[thisIter], 1, 1, 1]))	{
				boxWeighting[thisIter] = 0
				nc_values[closeishLons[thisIter], closeishLats[thisIter], , 1, ] = 0
			}
		}

			# normalizing weighting 
		boxWeighting = boxWeighting^thisExponent / sum(boxWeighting^thisExponent, na.rm=TRUE)

		for(thisScenario in 1:length(nc_values[1,1,1, , 1]))	{ # input nc is in format [lon, lat, decade, rcpScen, valueClass]
				# initializing array for holding 
			theseClimateValues = nc_values[closeishLons[1], closeishLats[1], , thisScenario, ] * boxWeighting[1]
				# accounting for ocean tiles
			if(any(is.na(theseClimateValues))) { theseClimateValues[is.na(theseClimateValues)] = 0 }
				
			for(thisWeight in 2:length(boxWeighting))	{
				nextClimateValues = nc_values[closeishLons[thisWeight], closeishLats[thisWeight], , thisScenario, ] 
				if(any(is.na(nextClimateValues))) { nextClimateValues[is.na(nextClimateValues)] = 0 }
				theseClimateValues = theseClimateValues + nextClimateValues * boxWeighting[thisWeight]# input nc is in format [lon, lat, decade, rcpScen, valueClass]
			}

			climateData[thisLoc, , , thisScenario, thisClimVar] = theseClimateValues

		}
	}
	nc_close(nc_file)
}
saveRDS(climateData, paste0(customerFolder, clientName, '\\',  clientName, '_rawValues.rds'))


	# standardized plots of data
	
climVars = c('Precipitation_decadalRawVals', 'PotentialEvapotranspiration_decadalRawVals', 'GroundwaterRecharge_decadalRawVals', 'Streamflow_decadalRawVals',
	'RootZoneSoilMoisture_decadalRawVals', 'TotalWaterStorage_decadalRawVals')
climVarNames = c('Precip (mm)', 'PET (mm)', 'GW Rech (mm)', 'Streamflow (km^2)', 'RZ Soil Moisture (%)', 'Total Storage (mm)')
rawDataColumnNames = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Mean', 'Q05', 'Q15', 'Q25', 'Q50', 'Q75', 'Q85', 'Q95')
scenarioNames = c('LowEmissions', 'MedEmissions', 'HighEmissions')

	# initializing data
climDataPlot = climateData
if(any(climDataPlot <= 0))	{climDataPlot[climDataPlot <= 0] = 1 ; print('check the data')}	# zero negative recharge, but no other values should be negative

for(thisLoc in 1:nrow(customerTable))	{
	for(thisScen in 1:length(scenarioNames))	{

		png(paste0(customerFolder, clientName, '\\',  customerTable[thisLoc, ..locationHeader], '_', scenarioNames[thisScen], "_rawValues.png"), width=900, height=900)
		par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(3,2), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
		windowsFonts(A = windowsFont("Roboto"))

		for(thisClimVar in 1:length(climVars))	{
				# climDataPlot is arranged as 	[location, decade, valueClass, scenario, climateVariable]
			currentAverage = mean(climDataPlot[thisLoc, 1:2, 17, 1:3, thisClimVar])
			ylabPctVals = c(seq(-5,5,0.1))
			ylabPctValLocs = currentAverage + currentAverage * ylabPctVals
		
			yMin = min(climDataPlot[thisLoc, , 14, , thisClimVar])*0.985
			yMax = max(climDataPlot[thisLoc, , 20, , thisClimVar])*1.025
			plot(nc_decade, climDataPlot[thisLoc, , 20, thisScen, thisClimVar],  ylim = c(yMin,yMax) ,
				type='l', lwd=1, col='white', xaxt = 'n', #log='y',
				main='', ylab='', xlab='',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A')
			abline(h=mean(climDataPlot[thisLoc, 1:2, 17, 1:3, thisClimVar]), lwd=2, lty =2, col='#1A232F')
			axis(1, at = nc_decade ,col.lab='#1A232F', col.axis='#666D74', 
				labels = nc_decade)
#			if(ylabPctValLocs[2] != 0)	{
			axis(4, at = ylabPctValLocs, col.lab='#1A232F', col.axis='#666D74', 
				labels = paste0(round(ylabPctVals * 100, 0), '%'))
#			}
		#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
			polygon(x=c(nc_decade, rev(nc_decade)), y=c(climDataPlot[thisLoc, , 14, thisScen, thisClimVar], rev(climDataPlot[thisLoc, , 20, thisScen, thisClimVar])),
				col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
			polygon(x=c(nc_decade, rev(nc_decade)), y=c(climDataPlot[thisLoc, , 15, thisScen, thisClimVar], rev(climDataPlot[thisLoc, , 19, thisScen, thisClimVar])),
				col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
			polygon(x=c(nc_decade, rev(nc_decade)), y=c(climDataPlot[thisLoc, , 16, thisScen, thisClimVar], rev(climDataPlot[thisLoc, , 18, thisScen, thisClimVar])),
				col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
			loessSmooth = loess(climDataPlot[thisLoc, , 17, thisScen, thisClimVar] ~ nc_decade)
#			lines(nc_decade, climDataPlot[thisLoc, , 17, thisScen, thisClimVar], 
#				col='#54575a', lwd=5)	#4cbfad
			lines(nc_decade, predict(loessSmooth),
				col='#EE6222', lwd=3)
			text(nc_decade[1], yMin, climVarNames[thisClimVar], adj = c(0,0), cex=2.75)
#			lines(nc_decade, nc_testDat[thisLon, thisLat, , 1, 1], 
#				col='#4cbfad', lwd=3) #015f6f
		}
		dev.off()
	}
}
	



###################################################################
# 3B- calculating and generating basic plots for the water indes

	# Water Index Calculations 
	# array for holding outputs
myMissingData = NA
indexValues = c('Aridity Index - Avg', 'Soil Moisture Stress - Avg', 'Plant Water Demand - Avg', 
	'Aridity Index - Drought', 'Soil Moisture Stress - Drought', 'Plant Water Demand - Drought', 
	'Aridity Index w/ Irrigation - Avg', 'Plant Water Demand w/ Irrigation - Avg', 
	'Aridity Index w/ Irrigation - Drought', 'Plant Water Demand w/ Irrigation - Drought')
#climVarNames = c('Precip (mm)', 'PET (mm)', 'GW Rech (mm)', 'Streamflow (km^2)', 'RZ Soil Moisture (%)', 'Total Storage (mm)')
#rawDataColumnNames = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Mean', 'Q05', 'Q15', 'Q25', 'Q50', 'Q75', 'Q85', 'Q95')

	# input array is in format  [location, decade, valueClass,      scenario, climateVariable]
	# output array is in format [location, decade, indexValueQuant, scenario, indexValue, indexValueClass]
indexValueClass = c('ratio', 'difference')
indexValueQuant = c('Q05', 'Q15', 'Q25', 'Q50', 'Q75', 'Q85', 'Q95')
indexValuesArray = array(rep(myMissingData, nrow(customerTable) * length(climateData[1, ,1,1,1]) * length(indexValueQuant) * length(climateData[1,1,1, ,1]) * length(indexValues) * length(indexValueClass)), 
								    dim = c(nrow(customerTable),  length(climateData[1, ,1,1,1]),  length(indexValueQuant),  length(climateData[1,1,1, ,1]),  length(indexValues), length(indexValueClass)))



for(thisRow in 1:nrow(customerTable))	{
		# initializing static values
	petGlobAvgForGrowSeason = petGlobAvg * (length(growSeason) / 12)
	thisFrcAreaUnderCult = ifelse(is.na(customerTable$Area_Irrigated[thisRow]), frcAreaUnderCult, customerTable$Area_Irrigated[thisRow])	# defining water needs by location
	thisWplant = ifelse(is.na(customerTable$Annual_Water_Needs[thisRow]), wPlant, customerTable$Annual_Water_Needs[thisRow])	# defining water needs by location
	thisDivertibleStrmfl = ifelse(is.na(customerTable$Divertible_Streamflow[thisRow]), divertibleStrmfl, customerTable$Divertible_Streamflow[thisRow]) # defining extractible water by location
	thisStrmflCaptureRatio = ifelse(is.na(customerTable$Streamflow_Capture[thisRow]), strmflCaptureRatio, customerTable$Streamflow_Capture[thisRow]) # defining extractible water by location

		# quantiles are combined in some eqs (and e.g. Q05*Q05 --> Q0025), so quantiles for each coincident var must be able to be handled separately
	#ppt = precipitation
	pptQntsNrml = 1 - ((climateData[thisRow, , rep(17,7), , 1] - climateData[thisRow, , 14:20, , 1]) /  climateData[thisRow, , rep(17,7), , 1])
	
	pptQntsAvg = sqrt(pptQntsNrml) * climateData[thisRow, , rep(17,7), , 1] # taking the sqrt since the var will be multiplied (and e.g. Q05*Q05 --> Q0025)
	pptQntsDrght = sqrt(pptQntsNrml) * climateData[thisRow, , rep(15,7), , 1]
	pptQntsDrghtShft = sqrt(pptQntsNrml) * (1 - ((climateData[thisRow, , rep(17,7), , 1] - climateData[thisRow, , rep(15,7), , 1]) / climateData[thisRow, , rep(17,7), , 1]))

	#pet = potential evapotranspiration
	petQntsNrml = 1 - ((climateData[thisRow, , rep(17,7), , 2] - climateData[thisRow, , 20:14, , 2]) /  climateData[thisRow, , rep(17,7), , 2])
	petQntsAvg = sqrt(petQntsNrml) * climateData[thisRow, , rep(17,7), , 2]
#	petQntsDrght = sqrt(petQntsNrml) * climateData[thisRow, , rep(19,7), , 2]

	#rzsm = root zone soil moisture
	rzsmQntsNrml = 1 - ((climateData[thisRow, , rep(17,7), , 5] - climateData[thisRow, , 14:20, , 5]) /  climateData[thisRow, , rep(17,7), , 5])
	rzsmQntsAvg = rzsmQntsNrml * climateData[thisRow, , rep(17,7), , 5] # no need to take the sqrt since this var is not multiplied
	rzsmQntsDrght = rzsmQntsNrml * climateData[thisRow, , rep(15,7), , 5]
	
	 #rech = net gw recharge
	rechAvg = climateData[thisRow, , rep(17,7), , 3]

	#strmfl = streamflow
	gridArea = (halfDegInKM * cos(customerTable$Lat[thisRow] * pi / 180))^2
	strmflQntsNrml = 1 - ((climateData[thisRow, , rep(17,7), , 4] - climateData[thisRow, , 14:20, , 4]) /  climateData[thisRow, , rep(17,7), , 4])
	strmflQntsAvg = sqrt(strmflQntsNrml) * climateData[thisRow, , rep(17,7), , 4]
	strmflQntsDrght = sqrt(strmflQntsNrml) * climateData[thisRow, , rep(15,7), , 4]
	strmflQntsDrghtShft = sqrt(strmflQntsNrml) * (1 - ((climateData[thisRow, , rep(17,7), , 4] - climateData[thisRow, , rep(15,7), , 4]) / climateData[thisRow, , rep(17,7), , 4]))
	streamflowScalar = ifelse(customerTable$Surface_Water[thisRow], thisDivertibleStrmfl, thisStrmflCaptureRatio)
	effectiveStrmfl = (streamflowScalar / thisFrcAreaUnderCult) * (strmflQntsAvg / gridArea) * kmToMm 
	effectiveStrmflDrght = (streamflowScalar / thisFrcAreaUnderCult) * (strmflQntsDrght / gridArea) * kmToMm 
	
	
		# Aridity Index - Avg
	indexValuesArray[thisRow, , , , 1, 1] = (pptQntsAvg / petQntsAvg) / humidAI	
	indexValuesArray[thisRow, , , , 1, 2] = (pptQntsAvg - petQntsAvg * humidAI)
		# Soil Moisture Stress - Avg
	indexValuesArray[thisRow, , , , 2, 1] = rzsmQntsAvg / pwpSoil
	indexValuesArray[thisRow, , , , 2, 2] = rzsmQntsAvg - pwpSoil
		# Plant Water Demand - Avg
	pptQntsNrml_local = pptQntsAvg
	if(customerTable$Soil_Moisture[thisRow])	{pptQntsNrml_local[,,] = 0}				# zeroing soil moisture is it cannot be used
	growSeasonPPTqntsAvg = sqrt(pptQntsNrml) * climateData[thisRow, , rep(growSeason[1], 7), , 1]	# taking the sqrt since the var will be multiplied (and e.g. Q05*Q05 --> Q0025)
	growSeasonPETqntsAvgRatio = sqrt(petQntsNrml) * climateData[thisRow, , rep(growSeason[1],7), , 2] / petGlobAvgForGrowSeason
	for(thisMonth in growSeason[-1])	{
		growSeasonPPTqntsAvg = growSeasonPPTqntsAvg + sqrt(pptQntsNrml) * climateData[thisRow, , rep(thisMonth, 7), , 1]
		growSeasonPETqntsAvgRatio = growSeasonPETqntsAvgRatio + sqrt(petQntsNrml) * climateData[thisRow, , rep(thisMonth, 7), , 2] / petGlobAvgForGrowSeason
	}
	effectivePPT = (1 - runoffRatio) * growSeasonPPTqntsAvg
	effectiveWPlant = sqrt(growSeasonPETqntsAvgRatio) * mean(thisWplant)
	indexValuesArray[thisRow, , , , 3, 1] = effectivePPT / effectiveWPlant
	indexValuesArray[thisRow, , , , 3, 2] = effectivePPT - effectiveWPlant

	# Aridity Index - Drought
	indexValuesArray[thisRow, , , , 4, 1] = (pptQntsDrght / petQntsAvg) / humidAI
	indexValuesArray[thisRow, , , , 4, 2] = pptQntsDrght - petQntsAvg * humidAI
		# Soil Moisture Stress - Drought
	indexValuesArray[thisRow, , , , 5, 1] = rzsmQntsDrght / pwpSoil
	indexValuesArray[thisRow, , , , 5, 2] = rzsmQntsDrght - pwpSoil
		# Plant Water Demand - Drought
	growSeasonPPTqntsdrought_local = sqrt(pptQntsDrghtShft) * climateData[thisRow, , rep(growSeason[1], 7), , 1]	
	if(customerTable$Soil_Moisture[thisRow])	{growSeasonPPTqntsdrought_local[,,] = 0}				# zeroing soil moisture is it cannot be used
	for(thisMonth in growSeason[-1])	{
		growSeasonPPTqntsdrought_local = growSeasonPPTqntsdrought_local + sqrt(pptQntsDrghtShft) * climateData[thisRow, , rep(thisMonth, 7), , 1]	
	}
	effectivePPTdrought = (1 - runoffRatio) * growSeasonPPTqntsdrought_local
	indexValuesArray[thisRow, , , , 6, 1] = effectivePPTdrought / effectiveWPlant
	indexValuesArray[thisRow, , , , 6, 2] = effectivePPTdrought - effectiveWPlant
	
		# Aridity Index w/ Irrigation - Avg
	indexValuesArray[thisRow, , , , 7, 1] = ((pptQntsDrght + rechAvg + effectiveStrmfl) / petQntsAvg) / humidAI
	indexValuesArray[thisRow, , , , 7, 2] = (pptQntsDrght + rechAvg + effectiveStrmfl) - petQntsAvg * humidAI
		# Plant Water Demand w/ Irrigation - Avg'
	rechAvg_local = rechAvg
	if(customerTable$Groundwater[thisRow])	{growSeasonPPTqntsdrought_local[,,] = 0}				# zeroing groundwater if it cannot be used
	indexValuesArray[thisRow, , , , 8, 1] = (effectivePPTdrought + rechAvg_local + effectiveStrmfl) / effectiveWPlant
	indexValuesArray[thisRow, , , , 8, 2] = (effectivePPTdrought + rechAvg_local + effectiveStrmfl) - effectiveWPlant

		# Aridity Index w/ Irrigation - Drought
	indexValuesArray[thisRow, , , , 9, 1] = ((pptQntsDrght + rechAvg_local + effectiveStrmflDrght) / petQntsAvg) / humidAI
	indexValuesArray[thisRow, , , , 9, 2] = (pptQntsDrght + rechAvg_local + effectiveStrmflDrght) - petQntsAvg * humidAI
		# Plant Water Demand w/ Irrigation - Drought
	indexValuesArray[thisRow, , , , 10, 1] = (effectivePPTdrought + rechAvg_local + effectiveStrmflDrght) / effectiveWPlant
	indexValuesArray[thisRow, , , , 10, 2] = (effectivePPTdrought + rechAvg_local + effectiveStrmflDrght) - effectiveWPlant
}
saveRDS(indexValuesArray, paste0(customerFolder, clientName, '\\',  clientName, "_waterIndex.rds"))




	# initializing data for plotting
waterIndexDataPlot = indexValuesArray
#if(any(waterIndexDataPlot < 0))	{waterIndexDataPlot[waterIndexDataPlot < 0] = 0}
	# identifying which indexValues to actually plot
indexValuesToPlot = c(1,3,4,6,7,8,9,10)

for(thisLoc in 1:nrow(customerTable))	{
	for(thisScen in 1:length(scenarioNames))	{

			# water index ratios
		png(paste0(customerFolder, clientName, '\\',  customerTable[thisLoc, ..locationHeader], '_', scenarioNames[thisScen], "_waterIndexRatio.png"), width=1100, height=1200)
		par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(4,2), font.lab=1.6, bty='l', cex.lab=2.0*1.8, cex.axis=2.0*1.4, cex.main=2.0*1.8, col='#1A232F')
		windowsFonts(A = windowsFont("Roboto"))
		for(thisIndexVal in indexValuesToPlot)	{
				# waterIndexDataPlot is in format [location, decade, indexValueQuant, scenario, indexValue, indexValueClass]
			plotRange = c(min(min(waterIndexDataPlot[thisLoc, , , , thisIndexVal, 1])*1.025, 0.9),  max(max(waterIndexDataPlot[thisLoc, , , , thisIndexVal, 1])*1.025, 1.1))
			plot(nc_decade, waterIndexDataPlot[thisLoc, , 4, thisScen, thisIndexVal, 1],  
				ylim = plotRange,
				type='l', lwd=1, col='white', xaxt = 'n', #log='y',
				main='', ylab='', xlab='',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A')
			abline(h=1, lwd=2, lty=1, col='#1A232F')
			abline(h=mean(waterIndexDataPlot[thisLoc, 1:2, 4, 1:3, thisIndexVal, 1]), lwd=2, lty =2, col='#1A232F')
			axis(1, at = nc_decade ,col.lab='#1A232F', col.axis='#666D74', 
				labels = nc_decade)
		#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
			polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 1, thisScen, thisIndexVal, 1], rev(waterIndexDataPlot[thisLoc, , 7, thisScen, thisIndexVal, 1])),
				col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
			polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 2, thisScen, thisIndexVal, 1], rev(waterIndexDataPlot[thisLoc, , 6, thisScen, thisIndexVal, 1])),
				col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
			polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 3, thisScen, thisIndexVal, 1], rev(waterIndexDataPlot[thisLoc, , 5, thisScen, thisIndexVal, 1])),
				col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
			loessSmooth = loess(waterIndexDataPlot[thisLoc, , 4, thisScen, thisIndexVal, 1] ~ nc_decade)
#			lines(nc_decade, waterIndexDataPlot[thisLoc, , 17, thisScen, thisIndexVal], 
#				col='#54575a', lwd=5)	#4cbfad
			lines(nc_decade, predict(loessSmooth),
				col='#EE6222', lwd=3)
			text(nc_decade[1], plotRange[1],  paste0(indexValues[thisIndexVal], ' (-)'), adj = c(0,0), cex=2.65)
#			lines(nc_decade, nc_testDat[thisLon, thisLat, , 1, 1], 
#				col='#4cbfad', lwd=3) #015f6f
		}
		dev.off()

			# water index deficits
		png(paste0(customerFolder, clientName, '\\', customerTable[thisLoc, ..locationHeader], '_', scenarioNames[thisScen], "_waterIndexDeficit.png"), width=1100, height=1200)
		par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(4,2), font.lab=1.6, bty='l', cex.lab=2.0*1.8, cex.axis=2.0*1.4, cex.main=2.0*1.8, col='#1A232F')
		windowsFonts(A = windowsFont("Roboto"))
		for(thisIndexVal in indexValuesToPlot)	{
				# waterIndexDataPlot is in format [location, decade, indexValueQuant, scenario, indexValue, indexValueClass]
			plotRange = c(min(min(waterIndexDataPlot[thisLoc, , , , thisIndexVal, 2]), 0), max(max(waterIndexDataPlot[thisLoc, , , , thisIndexVal, 2]), 0))
			plot(nc_decade, waterIndexDataPlot[thisLoc, , 4, thisScen, thisIndexVal, 2],  ylim = plotRange,
				type='l', lwd=1, col='white', xaxt = 'n', #log='y',
				main='', ylab='', xlab='',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A')
			abline(h=0, lwd=2, lty=1, col='#1A232F')
			abline(h=mean(waterIndexDataPlot[thisLoc, 1:2, 4, 1:3, thisIndexVal, 2]), lwd=2, lty =2, col='#1A232F')
			axis(1, at = nc_decade ,col.lab='#1A232F', col.axis='#666D74', 
				labels = nc_decade)
		#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
			polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 1, thisScen, thisIndexVal, 2], rev(waterIndexDataPlot[thisLoc, , 7, thisScen, thisIndexVal, 2])),
				col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
			polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 2, thisScen, thisIndexVal, 2], rev(waterIndexDataPlot[thisLoc, , 6, thisScen, thisIndexVal, 2])),
				col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
			polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 3, thisScen, thisIndexVal, 2], rev(waterIndexDataPlot[thisLoc, , 5, thisScen, thisIndexVal, 2])),
				col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
			loessSmooth = loess(waterIndexDataPlot[thisLoc, , 4, thisScen, thisIndexVal, 2] ~ nc_decade)
#			lines(nc_decade, waterIndexDataPlot[thisLoc, , 17, thisScen, thisIndexVal], 
#				col='#54575a', lwd=5)	#4cbfad
			lines(nc_decade, predict(loessSmooth),
				col='#EE6222', lwd=3)
			text(nc_decade[1], min(plotRange), paste0(indexValues[thisIndexVal], ' (mm)'), adj = c(0,0), cex=2.65)
#			lines(nc_decade, nc_testDat[thisLon, thisLat, , 1, 1], 
#				col='#4cbfad', lwd=3) #015f6f
		}
		dev.off()


	}
}
	
accenture, rayonier, enel, guidehouse

###################################################################
# 3C- incorporating 3rd party data for historicals

##### incorporating grace / gracefo
	# data resources
# https://podaac.jpl.nasa.gov/dataset/TELLUS_GRFO_L3_CSR_RL06.1_LND_v04?ids=&values=&search=GRFO&temporalSearch=2006-04-03T04:00:00.000ZTO*#
# https://github.com/podaac/data-subscriber
# https://www2.csr.utexas.edu/grace/science_links.html
# https://grace.jpl.nasa.gov/data/data-analysis-tool/
# https://podaac.jpl.nasa.gov/dataset/TELLUS_GRAC-GRFO_MASCON_GRID_RL06.1_V3?ids=&values=&search=GRFO&temporalSearch=2006-04-03T04:00:00.000ZTO*
# https://podaac.jpl.nasa.gov/dataset/TELLUS_GRAC-GRFO_MASCON_CRI_GRID_RL06.1_V3?ids=&values=&search=GRFO&temporalSearch=2006-04-03T04:00:00.000ZTO*#

#test = nc_open('J:\\Cai_data\\Rabo\\GRACE\\GRCTellus.JPL.200204_202211.GLO.RL06.1M.MSCNv03.nc')
test = nc_open('J:\\Cai_data\\Rabo\\GRACE\\GRCTellus.JPL.200204_202211.GLO.RL06.1M.MSCNv03CRI.nc')


nc_lat = ncvar_get(test, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(test, 'lon')	# given from 0.25 to 359.75, so need to convert to neg / pos
nc_lon[nc_lon > 180] = -(360 - nc_lon[nc_lon > 180])
nc_time = ncvar_get(test, 'time')
startDate = as.Date('2002-01-01')
graceDates = startDate + nc_time
nc_landMask = ncvar_get(test, 'land_mask')
nc_water = which(nc_landMask == 0)
#nc_scaleFactor = ncvar_get(test, 'scale_factor')
nc_lweThick = ncvar_get(test, 'lwe_thickness') * 10 # convert cm to mm
lweThickLand = nc_lweThick ; lweThickLand[nc_water] = NA
nc_uncertainty = ncvar_get(test, 'uncertainty') * 10 # convert cm to mm
uncertLand = nc_uncertainty ; uncertLand[nc_water] = NA

#image(nc_lon, nc_lat, nc_lweThick[,,1])
#image(nc_lon, nc_lat, lweThickLand[,,1])
#image(nc_lon, nc_lat, nc_uncertainty[,,150])
#image(nc_lon, nc_lat, uncertLand[,,1])

# defining array for holding climate data
graceDataTable = data.table(Location = NA, Lat = NA, Lon = NA, LWE_Depth_Median = NA, LWE_Depth_SD = NA, Date = as.Date(NA)) 

for(thisLoc in 1:nrow(customerTable))	{
	closestLat = which.min(abs(nc_lat - customerTable$Lat[thisLoc]))
	closestLon = which.min(abs(nc_lon - customerTable$Lon[thisLoc]))

		# smoothing / downscaling and reweighting based on distance
	closeishLats = rep(closestLat + c(-1,0,1), 3)
	closeishLons = rep(closestLon + c(-1,0,1), each = 3)
	closeishLatsVals = (nc_lat[closeishLats] - customerTable$Lat[thisLoc])^2
	closeishLonsVals = (nc_lon[closeishLons] - customerTable$Lon[thisLoc])^2
	thisExponent = 2 		# may want to revisit weighting, bu this should be standard
	distanceBox = sqrt(closeishLatsVals + closeishLonsVals)
	boxWeighting = max(distanceBox, na.rm=TRUE) - distanceBox
#	boxWeighting = 1 - distanceBox # since max distance of a 2x2 .5 deg grid is 1 deg

		# un-weighting water / ocean tiles
	for(thisIter in 1:length(closeishLats))	{
		if(is.na(lweThickLand[closeishLons[thisIter], closeishLats[thisIter], 1]))	{
			boxWeighting[thisIter] = 0
			lweThickLand[closeishLons[thisIter], closeishLats[thisIter], ] = 0
			uncertLand[closeishLons[thisIter], closeishLats[thisIter], ] = 0
		}
	}

		# normalizing weighting 
	boxWeighting = boxWeighting^thisExponent / sum(boxWeighting^thisExponent, na.rm=TRUE)

		# initializing array for holding 
	theseLweThickLand = lweThickLand[closeishLons[1], closeishLats[1], ] * boxWeighting[1]
	theseUncertLand = uncertLand[closeishLons[1], closeishLats[1], ] * boxWeighting[1]
		# accounting for ocean tiles
	if(any(is.na(theseLweThickLand))) { theseLweThickLand[is.na(theseLweThickLand)] = 0 ;  theseUncertLand[is.na(theseUncertLand)] = 0 }
	for(thisWeight in 2:length(boxWeighting))	{
		nextLweThickLand = lweThickLand[closeishLons[thisWeight], closeishLats[thisWeight], ] 
		nextUncertLand = uncertLand[closeishLons[thisWeight], closeishLats[thisWeight], ] 
		if(any(is.na(nextLweThickLand))) { nextLweThickLand[is.na(nextLweThickLand)] = 0 ; nextUncertLand[is.na(nextUncertLand)] = 0}
		theseLweThickLand = theseLweThickLand + nextLweThickLand * boxWeighting[thisWeight]# input nc is in format [lon, lat, decade, rcpScen, valueClass]
		theseUncertLand = theseUncertLand + nextUncertLand * boxWeighting[thisWeight]# input nc is in format [lon, lat, decade, rcpScen, valueClass]
	}


		# water index deficits
	png(paste0(customerFolder, clientName, '\\',  customerTable[thisLoc, ..locationHeader], '_', "_GRACE-historicalGW.png"), width=900, height=600)
	par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
	windowsFonts(A = windowsFont("Roboto"))

	plotRange = c(min(theseLweThickLand) - max(theseUncertLand), max(theseLweThickLand) + max(theseUncertLand))
	plot(graceDates, theseLweThickLand,  ylim = plotRange,
		type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
		main='', ylab='', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	abline(h=0, lwd=2, lty=1, col='#1A232F')
#	axis(1, at = nc_decade ,col.lab='#1A232F', col.axis='#666D74', 
#		labels = nc_decade)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(graceDates, rev(graceDates)), y=c(theseLweThickLand + theseUncertLand, rev(theseLweThickLand - theseUncertLand)),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	polygon(x=c(graceDates, rev(graceDates)), y=c(theseLweThickLand + 2*theseUncertLand, rev(theseLweThickLand - 2*theseUncertLand)),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	polygon(x=c(graceDates, rev(graceDates)), y=c(theseLweThickLand + 4*theseUncertLand, rev(theseLweThickLand - 4*theseUncertLand)),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
#	loessSmooth = loess(theseLweThickLand ~ nc_time)
	linearTrend = lm(theseLweThickLand ~ nc_time)
#	lines(nc_decade, waterIndexDataPlot[thisLoc, , 17, thisScen, thisIndexVal], 
#		col='#54575a', lwd=5)	#4cbfad
	lines(graceDates, theseLweThickLand,
		col='#0098B2', lwd=1.5)
	lines(graceDates, predict(linearTrend),
		col='#EE6222', lwd=3)
	text(last(graceDates), 0, '2004-2010 avg', adj = c(1,-0.2), cex=1.25, col='#666D74')
	text(first(graceDates), min(theseLweThickLand), paste0(round(linearTrend$coefficients[2]* 365, 0), ' mm per Year'), adj = c(0,0), cex=2.25, col ='#EE6222')
#	lines(nc_decade, nc_testDat[thisLon, thisLat, , 1, 1], 
#		col='#4cbfad', lwd=3) #015f6f
	dev.off()


	graceDataTable = rbind(graceDataTable, data.table(
		Location = unlist(customerTable[thisLoc, ..locationHeader]),
		Lat = customerTable$Lat[thisLoc],
		Lon = customerTable$Lon[thisLoc],
		LWE_Depth_Median = theseLweThickLand,
		LWE_Depth_SD = theseUncertLand,
		Date = graceDates))
}
fwrite(graceDataTable, paste0(customerFolder, clientName, '\\',  clientName, "_graceHistorical.csv"))

