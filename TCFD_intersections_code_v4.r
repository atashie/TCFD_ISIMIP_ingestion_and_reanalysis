library(data.table)
library(ncdf4)

	# corbion
userName = 'Corbion'	
hazardFolder = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
customerFolder = 'J:\\Cai_data\\TCFD\\locations\\Corbion_Nov2022\\'
thisDate = Sys.Date()

customerTable = fread(paste0(customerFolder, 'Corbion_Locations_Nov2022 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
hazardTable = fread(paste0(customerFolder, 'Hazard_Tables_Corbion_Nov2022 - Hazard Definitions.csv'))							# 
relHazScores = fread(paste0(customerFolder, 'Hazard_Tables_Corbion_Nov2022 - Hazard Scores.csv'))				
appendedHazardNames = c("River Flood (Local)", 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\Corbion_Nov2022\\Corbion_temp_precip_hazards_dec_6 - Corbion_temp_precip_hazards_dec_6.csv'

	# ITC
userName = 'ITC'	
hazardFolder = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
customerFolder = 'J:\\Cai_data\\TCFD\\locations\\ITC_Dec2022\\'
thisDate = Sys.Date()

customerTable = fread(paste0(customerFolder, 'ITC_Locations_Dec2022 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
hazardTable = fread(paste0(customerFolder, 'Hazard_Tables_ITC_Dec2022 - Hazard Definitions.csv'))							# 
relHazScores = fread(paste0(customerFolder, 'Hazard_Tables_ITC_Dec2022 - Hazard Scores.csv'))				
appendedHazardNames = c("River Flood (Local)", 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\ITC_Dec2022\\ITC_temp_precip_hazards_dec_6 - ITC_temp_precip_hazards_dec_6.csv'

	# ITC - Water
userName = 'ITC - Water'	
hazardFolder = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
customerFolder = 'J:\\Cai_data\\TCFD\\locations\\ITC_Dec2022\\'
thisDate = Sys.Date()

customerTable = fread(paste0(customerFolder, 'ITC_Locations_Dec2022 - Water Exposure.csv')) #'HMClause_locations_allCucurbit.csv'
hazardTable = fread(paste0(customerFolder, 'Hazard_Tables_ITC_Dec2022 - Hazard Definitions.csv'))							# 
relHazScores = fread(paste0(customerFolder, 'Hazard_Tables_ITC_Dec2022 - Hazard Scores.csv'))				
appendedHazardNames = c("River Flood (Local)", 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
#appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\ITC_Dec2022\\ITC_temp_precip_hazards_dec_6 - ITC_temp_precip_hazards_dec_6.csv'

	# Driscolls
userName = 'Driscolls'	
hazardFolder = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
customerFolder = 'J:\\Cai_data\\TCFD\\locations\\Driscolls_Dec2022\\'
thisDate = Sys.Date()

customerTable = fread(paste0(customerFolder, 'Driscolls_Locations_Dec2022 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
hazardTable = fread(paste0(customerFolder, 'Hazard_Tables_Driscolls_Dec2022 - Hazard Definitions.csv'))							# 
relHazScores = fread(paste0(customerFolder, 'Hazard_Tables_Driscolls_Dec2022 - Hazard Scores.csv'))				
appendedHazardNames = c("River Flood (Local)", 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\Driscolls_Dec2022\\Driscolls_temp_precip_hazards_dec_6 - Driscolls_temp_precip_hazards_dec_6.csv'


	# HMClause
userName = 'HMClause'	
hazardFolder = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
customerFolder = 'J:\\Cai_data\\TCFD\\locations\\HMClause_Dec2022\\'
thisDate = Sys.Date()

customerTable = fread(paste0(customerFolder, 'HMClause_Locations_Dec2022 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
hazardTable = fread(paste0(customerFolder, 'Hazard_Tables_HMClause_Dec2022 - Hazard Definitions.csv'))							# 
relHazScores = fread(paste0(customerFolder, 'Hazard_Tables_HMClause_Dec2022 - Hazard Scores.csv'))				
appendedHazardNames = c("River Flood (Local)", 'Extreme Cold', 'Extreme Heat', 'Intense Precipitation')
appendedHazardFileLoc = 'J:\\Cai_data\\TCFD\\locations\\HMClause_Dec2022\\HMClause_temp_precip_hazards_dec_6 - HMClause_temp_precip_hazards_dec_6.csv'

############################################################################################
## basic exposure data
dataOutput = data.frame(User = NA, Location = NA, Region = NA, Subregion = NA, Lat = NA, Lon = NA,
	Hazard = NA, Hazard_Measure = NA, Decade = NA, Scenario = NA,
	Raw_Hazard_Value = NA, Percentile_Score = NA, Relative_Hazard_Score = NA, Decadal_Trend_Strength = NA, Decadal_Trend_Significance = NA, Long_Term_Trend_Strength = NA, Long_Term_Trend_Significance = NA,
	Relative_Hazard_Score_Number = NA, Trend_Aggregated_For_Looker = NA, Advanced_Data_Measures = NA, Advanced_Data_Measures_Units = NA)

iter = 0
for(thisHazard in 6:ncol(customerTable))	{
	if(any(customerTable[, ..thisHazard]))	{
		if(names(customerTable)[thisHazard] %in% appendedHazardNames)	{	
			print(c('this hazard must be run independently and appended later:     ', names(customerTable)[thisHazard]))
		}	else	{
			hazardMeasures = subset(hazardTable, Hazard == names(customerTable)[thisHazard])
			for(thisHazardMeasure in 1:nrow(hazardMeasures))	{
				hazardMeasureNC = nc_open(paste0('J:\\Cai_data\\TCFD\\ProcessedNCs\\', hazardMeasures$Hazard_Measure[thisHazardMeasure], '_processed.nc'))
				nc_lats = ncvar_get(hazardMeasureNC, 'lat')
				nc_lons = ncvar_get(hazardMeasureNC, 'lon')
				nc_decades = 2000 + ncvar_get(hazardMeasureNC, 'decade')
							
				for(thisLocation in 1:nrow(customerTable))	{

					if(unlist(customerTable[thisLocation, ..thisHazard]))	{
						closeLon = which.min(abs(nc_lons - customerTable$Lon[thisLocation]))
						closeLat = which.min(abs(nc_lats - customerTable$Lat[thisLocation]))

							# ensuring we aren't drawing from a coastal water cell
						if(is.na(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, 1, 1, 2]))	{
							closeLat = closeLat + 1
							if(is.na(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, 1, 1, 2]))	{
								closeLat = closeLat - 2
								if(is.na(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, 1, 1, 2]))	{
									closeLat = closeLat + 1
									closeLon = closeLon + 1
									if(is.na(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, 1, 1, 2]))	{
										closeLon = closeLon - 2
									}
								}
							}
						}
		
						
						dataOutput = rbind(dataOutput,
							data.frame(
								User = userName,
								Location = customerTable$Location[thisLocation],
								Region = customerTable$Region[thisLocation],
								Subregion = customerTable$Subregion[thisLocation],
								Lat = customerTable$Lat[thisLocation],
								Lon = customerTable$Lon[thisLocation],
								Hazard = hazardMeasures$Hazard[thisHazardMeasure],
								Hazard_Measure = paste0(hazardMeasures$Hazard_Measure_Common_Name[thisHazardMeasure], ' ', hazardMeasures$Hazard_Measure_Units[thisHazardMeasure]),
								Decade = nc_decades,
								Scenario = rep(c('RCP 2.6', 'RCP 6.0', 'RCP 8.5'), each = 9),
								Raw_Hazard_Value = c(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 1, 1], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 2, 1], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 3, 1]),
								Percentile_Score = c(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 1, 2], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 2, 2], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 3, 2]),
								Relative_Hazard_Score = NA,
								Decadal_Trend_Strength = 	 	c(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 1, 3], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 2, 3], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 3, 3]),
								Decadal_Trend_Significance = 	c(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 1, 4], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 2, 4], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 3, 4]),
								Long_Term_Trend_Strength = 		c(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 1, 5], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 2, 5], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 3, 5]),
								Long_Term_Trend_Significance =	c(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 1, 6], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 2, 6], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 3, 6]),
								Relative_Hazard_Score_Number = NA,
								Trend_Aggregated_For_Looker = NA,
								Advanced_Data_Measures = NA,
								Advanced_Data_Measures_Units = NA)
						)
					}
					print(c(thisHazard, thisHazardMeasure, thisLocation))
				}
				nc_close(hazardMeasureNC)
				iter = iter + 1
				fwrite(dataOutput[-1,], paste0(customerFolder, 'temp_out_', iter, '.csv'))
				dataOutput = data.frame(User = NA, Location = NA, Region = NA, Subregion = NA, Lat = NA, Lon = NA,
					Hazard = NA, Hazard_Measure = NA, Decade = NA, Scenario = NA,
					Raw_Hazard_Value = NA, Percentile_Score = NA, Relative_Hazard_Score = NA, Decadal_Trend_Strength = NA, Decadal_Trend_Significance = NA, Long_Term_Trend_Strength = NA, Long_Term_Trend_Significance = NA,
					Relative_Hazard_Score_Number = NA, Trend_Aggregated_For_Looker = NA, Advanced_Data_Measures = NA, Advanced_Data_Measures_Units = NA)
			}
		}
	}
}
finalOutput = fread(paste0(customerFolder, 'temp_out_', 1, '.csv'))
for(thisIter in 2:iter)	{
	finalOutput = rbind(finalOutput, fread(paste0(customerFolder, 'temp_out_', thisIter, '.csv')))
}

fwrite(finalOutput, paste0(customerFolder, 'processedOutput_', thisDate, '.csv'))














###################################################################################################################################################################################################
#### specialized data, for now just localized flooding
# step 3 intersecting recurrence intervals with flood depth
library(raster)
locationFootprint = 3		# how big is the footprint of the location of interest? in number of 'boxes' to search to the left and right (so 0 is equal to 1 km^2, 1 is 3x3=9 km^2, 2 is 5x5=25 km^2, 3 is 7x7=49, 4 is 9x9=81, etc.
dataOutputLoc = 'J:\\Cai_data\\TCFD\\CustomerOutputs\\'
waterMaskLoc = 'J:\\Cai_data\\TCFD\\CurrentFloodHazard\\LandMaskArray.rds'


	# defining rcp scenarios, recurrence intervals, and decades of interest; this should not change for the foreseeable future
rcpScenarios = c('RCP 2.6', 'RCP 6.0', 'RCP 8.5')
recurIntrvls = c(10, 20, 50, 100, 200, 500)
whichDecades = seq(10,90,10)

	
	# read in flood recurrence data
ncpath = "J:\\Cai_data\\TCFD\\Flash Floods\\"
fldRcrIntNC = nc_open(paste0(ncpath, 'floodRecurIntervals_v4.nc'))
nc_lat = ncvar_get(fldRcrIntNC, 'lat')
nc_lon = ncvar_get(fldRcrIntNC, 'lon')
fldRcrVals = ncvar_get(fldRcrIntNC, 'fldRecurIntrvl')


	# initializing data output
dataOutput = data.frame(User = NA, Location = NA, Region = NA, Subregion = NA, Lat = NA, Lon = NA,
	Hazard = NA, Hazard_Measure = NA, Decade = NA, Scenario = NA,
	Raw_Hazard_Value = NA, Percentile_Score = NA, Relative_Hazard_Score = NA, Decadal_Trend_Strength = NA, Decadal_Trend_Significance = NA, Long_Term_Trend_Strength = NA, Long_Term_Trend_Significance = NA,
	Relative_Hazard_Score_Number = NA, Trend_Aggregated_For_Looker = NA, Advanced_Data_Measures = NA, Advanced_Data_Measures_Units = NA)
	
	# read in historic floods data
fileLoc = 'J:\\Cai_data\\TCFD\\CurrentFloodHazard'
fldDepthList = list()
fldDepthList[[1]] = raster(paste0(fileLoc, '\\floodMapGL_rp10y.tif'))
fldDepthList[[2]] = raster(paste0(fileLoc, '\\floodMapGL_rp20y.tif'))
fldDepthList[[3]] = raster(paste0(fileLoc, '\\floodMapGL_rp50y.tif'))
fldDepthList[[4]] = raster(paste0(fileLoc, '\\floodMapGL_rp100y.tif'))
fldDepthList[[5]] = raster(paste0(fileLoc, '\\floodMapGL_rp200y.tif'))
fldDepthList[[6]] = raster(paste0(fileLoc, '\\floodMapGL_rp500y.tif'))
myWaterMask = readRDS(waterMaskLoc)

	# identifying lat lon coordinates for tiffs
tif_lat = rev(seq(-54.00824,83.2251,length.out=16468))
tif_lon = seq(-166.8, 180, length.out=41616)

hazardDepthName = paste0("Avg Flood Depth (m)")
hazardLikliName = paste0('Flood Areal Extent (%)')


for(thisIntrvl in 1:length(recurIntrvls))	{
	for(j in 1:nrow(customerTable))	{
		closeTiffLons = which.min(abs(customerTable$Lon[j] - tif_lon))
		closeTiffLats = which.min(abs(customerTable$Lat[j] - tif_lat))
		
		theseLats = seq(closeTiffLats - locationFootprint, closeTiffLats + locationFootprint, 1)
		theseLons = seq(closeTiffLons - locationFootprint, closeTiffLons + locationFootprint, 1)
			#ensuring lats / lons don't go outside bounding box
		if(any(theseLats < 1))	{theseLats[theseLats < 1] = 1}
		if(any(theseLats > length(tif_lat)))	{theseLats[theseLats > length(tif_lat)] = length(tif_lat)}
		if(any(theseLons < 1))	{theseLons[theseLons < 1] = 1}
		if(any(theseLons > length(tif_lon)))	{theseLons[theseLons > length(tif_lon)] = length(tif_lon)}

			# defining the water mask
		thisWaterMask = myWaterMask[theseLons, theseLats]

		histFloodDepth = mean(fldDepthList[[6]][theseLats, theseLons], na.rm=TRUE)
		histFloodLikli = 100 * length(which(fldDepthList[[6]][theseLats, theseLons] > 0)) / length(theseLats)^2
		
			# check to see if even 500yr floods trigger historically; if not, the skip next analysis
		if(!is.na(histFloodDepth))	{
				# ensuring we are not drawing from a water tile, then searching box around point if so
			closeNCLon = which.min(abs(customerTable$Lon[j] - nc_lon))
			closeNCLat = which.min(abs(customerTable$Lat[j] - nc_lat))
			if(is.na(fldRcrVals[closeNCLon, closeNCLat, 1, thisIntrvl, 1, 1]))	{
					closeNCLon = closeNCLon + 1
					if(is.na(fldRcrVals[closeNCLon, closeNCLat, 1, thisIntrvl, 1, 1]))	{
						closeNCLon = closeNCLon - 2
						if(is.na(fldRcrVals[closeNCLon, closeNCLat, 1, thisIntrvl, 1, 1]))	{
							closeNCLon = closeNCLon + 1
							closeNCLat = closeNCLat + 1
							if(is.na(fldRcrVals[closeNCLon, closeNCLat, 1, thisIntrvl, 1, 1]))	{
								closeNCLat = closeNCLat - 2
			}}}}
				
			for(thisScenario in 1:length(rcpScenarios))	{
				fldRcrSignif_all = fldRcrVals[closeNCLon, closeNCLat, 9,  thisIntrvl, 2, thisScenario]
#				fldRcrDrctn_all = fldRcrVals[closeNCLon, closeNCLat, 9,  thisIntrvl, 3, thisScenario]
				
				for(thisDecade in 1:length(whichDecades))	{
					thisFldRcrVal = 		fldRcrVals[closeNCLon, closeNCLat, thisDecade, thisIntrvl, 1, thisScenario]
					thisFldRcrSignif = 		fldRcrVals[closeNCLon, closeNCLat, thisDecade, thisIntrvl, 2, thisScenario]
#					thisFldRcrDrctn = 		fldRcrVals[closeNCLon, closeNCLat, thisDecade, thisIntrvl, 3, thisScenario]
					
					if(thisFldRcrVal >= recurIntrvls[1])	{
						closestFldRcrIntrvl = 	last(which(thisFldRcrVal > recurIntrvls))
						
						theseFloodImpacts = fldDepthList[[closestFldRcrIntrvl]][theseLats, theseLons]
						theseFloodImpacts[is.na(theseFloodImpacts)] = 0
						theseFloodImpacts = theseFloodImpacts * thisWaterMask
	
						prevFloodImpacts = fldDepthList[[thisIntrvl]][theseLats, theseLons]
						prevFloodImpacts[is.na(prevFloodImpacts)] = 0
						prevFloodImpacts = prevFloodImpacts * thisWaterMask
	
						
						if(any(!is.na(theseFloodImpacts)))	{
							avgFloodDepth = mean(theseFloodImpacts, na.rm=TRUE)
							avgFloodDepthChng = avgFloodDepth - mean(prevFloodImpacts, na.rm=TRUE) / max(c(1, thisDecade - 1))

							avgFloodLikli = 100 * (length(which(theseFloodImpacts > 0)) / length(!is.na(theseFloodImpacts)))
							avgFloodLikliChng = avgFloodLikli - 100 * (length(which(prevFloodImpacts > 0)) / length(!is.na(prevFloodImpacts))) / max(c(1, thisDecade - 1))
						

							dataOutput = rbind(dataOutput,
								data.table(
									User = userName,
									Location = customerTable$Location[j],
									Region = customerTable$Region[j],
									Subregion = customerTable$Subregion[j],
									Lat = customerTable$Lat[j],
									Lon = customerTable$Lon[j],
									Hazard = "River Flood (Local)",
									Hazard_Measure = c(hazardDepthName, hazardLikliName),
									Decade = 2000 + whichDecades[thisDecade],
									Scenario = rcpScenarios[thisScenario],
									Raw_Hazard_Value = c(avgFloodDepth, avgFloodLikli),					# Raw_Hazard_Value
									Percentile_Score = NA,												# Percentile_Score
									Relative_Hazard_Score = NA,											# Relative_Hazard_Score
									Decadal_Trend_Strength = c(avgFloodDepthChng, avgFloodLikliChng),	# Decadal_Trend_Strength
									Decadal_Trend_Significance = thisFldRcrSignif,						# Decadal_Trend_Significance
									Long_Term_Trend_Strength = NA,										# Long_Term_Trend_Strength
									Long_Term_Trend_Significance = fldRcrSignif_all,					# Long_Term_Trend_Significance					
									Relative_Hazard_Score_Number = NA,
									Trend_Aggregated_For_Looker = NA,
									Advanced_Data_Measures =  recurIntrvls[thisIntrvl],
									Advanced_Data_Measures_Units = "Yr Flood"))

						} 	else	{ # if all theseFloodImpacts are NAs
							dataOutput = rbind(dataOutput,
								data.table(
									User = userName,
									Location = customerTable$Location[j],
									Region = customerTable$Region[j],
									Subregion = customerTable$Subregion[j],
									Lat = customerTable$Lat[j],
									Lon = customerTable$Lon[j],
									Hazard = "River Flood (Local)",
									Hazard_Measure = c(hazardDepthName, hazardLikliName),
									Decade = 2000 + whichDecades[thisDecade],
									Scenario = rcpScenarios[thisScenario],
									Raw_Hazard_Value = 0,								# Raw_Hazard_Value
									Percentile_Score = NA,								# Percentile_Score
									Relative_Hazard_Score = NA,							# Relative_Hazard_Score
									Decadal_Trend_Strength = 0,							# Decadal_Trend_Strength
									Decadal_Trend_Significance = thisFldRcrSignif,		# Decadal_Trend_Significance
									Long_Term_Trend_Strength = NA,						# Long_Term_Trend_Strength
									Long_Term_Trend_Significance = fldRcrSignif_all,	# Long_Term_Trend_Significance					
									Relative_Hazard_Score_Number = NA,
									Trend_Aggregated_For_Looker = NA,
									Advanced_Data_Measures =  recurIntrvls[thisIntrvl],
									Advanced_Data_Measures_Units = "Yr Flood"))
						}
					}	else	{ # if thisFldRcrVal < 10 yr recurrence
						dataOutput = rbind(dataOutput,
							data.table(
								User = userName,
								Location = customerTable$Location[j],
								Region = customerTable$Region[j],
								Subregion = customerTable$Subregion[j],
								Lat = customerTable$Lat[j],
								Lon = customerTable$Lon[j],
								Hazard = "River Flood (Local)",
								Hazard_Measure = c(hazardDepthName, hazardLikliName),
								Decade = 2000 + whichDecades[thisDecade],
								Scenario = rcpScenarios[thisScenario],
								Raw_Hazard_Value = 0,								# Raw_Hazard_Value
								Percentile_Score = NA,								# Percentile_Score
								Relative_Hazard_Score = NA,							# Relative_Hazard_Score
								Decadal_Trend_Strength = 0,							# Decadal_Trend_Strength
								Decadal_Trend_Significance = thisFldRcrSignif,		# Decadal_Trend_Significance
								Long_Term_Trend_Strength = NA,						# Long_Term_Trend_Strength
								Long_Term_Trend_Significance = fldRcrSignif_all,	# Long_Term_Trend_Significance
								Relative_Hazard_Score_Number = NA,
								Trend_Aggregated_For_Looker = NA,
								Advanced_Data_Measures =  recurIntrvls[thisIntrvl],
								Advanced_Data_Measures_Units = "Yr Flood"))
					}
				} # for each decade

			dataOutput[which(dataOutput$Location == customerTable$Location[j] & dataOutput$Scenario == rcpScenarios[thisScenario]), ]$Long_Term_Trend_Strength = last(dataOutput)$Decadal_Trend_Strength

			} # for each rcp scenario
		}	else	{	# if there are not historical floods out to 500 yrs
			dataOutput = rbind(rbind(rbind(dataOutput,
				data.table(
					User = userName,
					Location = customerTable$Location[j],
					Region = customerTable$Region[j],
					Subregion = customerTable$Subregion[j],
					Lat = customerTable$Lat[j],
					Lon = customerTable$Lon[j],
					Hazard = "River Flood (Local)",
					Hazard_Measure = c(hazardDepthName, hazardLikliName),
					Decade = 2000 + rep(whichDecades, each = 2),
					Scenario = rcpScenarios[1],
					Raw_Hazard_Value = 0,				# Raw_Hazard_Value
					Percentile_Score = NA,				# Percentile_Score
					Relative_Hazard_Score = NA,			# Relative_Hazard_Score
					Decadal_Trend_Strength = NA,			# Decadal_Trend_Strength
					Decadal_Trend_Significance = NA,		# Decadal_Trend_Significance
					Long_Term_Trend_Strength = NA,		# Long_Term_Trend_Strength
					Long_Term_Trend_Significance = NA,	# Long_Term_Trend_Significance
					Relative_Hazard_Score_Number = NA,
					Trend_Aggregated_For_Looker = NA,
					Advanced_Data_Measures =  recurIntrvls[thisIntrvl],
					Advanced_Data_Measures_Units = "Yr Flood")), 
				data.table(
					User = userName,
					Location = customerTable$Location[j],
					Region = customerTable$Region[j],
					Subregion = customerTable$Subregion[j],
					Lat = customerTable$Lat[j],
					Lon = customerTable$Lon[j],
					Hazard = "River Flood (Local)",
					Hazard_Measure = c(hazardDepthName, hazardLikliName),
					Decade = 2000 + rep(whichDecades, each = 2),
					Scenario = rcpScenarios[2],
					Raw_Hazard_Value = 0,				# Raw_Hazard_Value
					Percentile_Score = NA,				# Percentile_Score
					Relative_Hazard_Score = NA,			# Relative_Hazard_Score
					Decadal_Trend_Strength = NA,			# Decadal_Trend_Strength
					Decadal_Trend_Significance = NA,		# Decadal_Trend_Significance
					Long_Term_Trend_Strength = NA,		# Long_Term_Trend_Strength
					Long_Term_Trend_Significance = NA,	# Long_Term_Trend_Significance
					Relative_Hazard_Score_Number = NA,
					Trend_Aggregated_For_Looker = NA,
					Advanced_Data_Measures =  recurIntrvls[thisIntrvl],
					Advanced_Data_Measures_Units = "Yr Flood")), 
				data.table(
					User = userName,
					Location = customerTable$Location[j],
					Region = customerTable$Region[j],
					Subregion = customerTable$Subregion[j],
					Lat = customerTable$Lat[j],
					Lon = customerTable$Lon[j],
					Hazard = "River Flood (Local)",
					Hazard_Measure = c(hazardDepthName, hazardLikliName),
					Decade = 2000 + rep(whichDecades, each = 2),
					Scenario = rcpScenarios[3],
					Raw_Hazard_Value = 0,				# Raw_Hazard_Value
					Percentile_Score = NA,				# Percentile_Score
					Relative_Hazard_Score = NA,			# Relative_Hazard_Score
					Decadal_Trend_Strength = NA,			# Decadal_Trend_Strength
					Decadal_Trend_Significance = NA,		# Decadal_Trend_Significance
					Long_Term_Trend_Strength = NA,		# Long_Term_Trend_Strength
					Long_Term_Trend_Significance = NA,	# Long_Term_Trend_Significance
					Relative_Hazard_Score_Number = NA,
					Trend_Aggregated_For_Looker = NA,
					Advanced_Data_Measures =  recurIntrvls[thisIntrvl],
					Advanced_Data_Measures_Units = "Yr Flood"))
		}
		print(c(j, thisIntrvl))
	}
}	

dataOutput$Raw_Hazard_Value = as.numeric(dataOutput$Raw_Hazard_Value)
dataOutput = dataOutput[-1,]

hazardDepthNames =  c("Avg Flood Depth (m)")
hazardExtentNames = c("Flood Areal Extent (%)")
	# defining relative flood hazard
basSeqDepth = c(0,seq(0.01, 10, length.out=79))
basSeqLikli = c(0,seq(1, 100, length.out=79))
relFloodHazardDepth = (basSeqDepth^2 / max(basSeqDepth^2)) * 10 + min(basSeqDepth)
relFloodHazardLikli = basSeqLikli
#relFloodHazard = c(rep(0, 33), seq(0.01,10,length.out=(67)))
dataOutput$Percentile_Score = 1
depthRows = which(dataOutput$Hazard_Measure %in% hazardDepthNames)
likliRows = which(dataOutput$Hazard_Measure %in% hazardExtentNames)
for(ll in 1:length(relFloodHazardDepth))	{
	dataOutput$Percentile_Score[depthRows][which(dataOutput$Raw_Hazard_Value[depthRows] > relFloodHazardDepth[ll])] = ll + 20
	dataOutput$Percentile_Score[likliRows][which(dataOutput$Raw_Hazard_Value[likliRows] > relFloodHazardLikli[ll])] = ll + 20
}

fileName = paste0(userName, '_', thisDate, '_highRestFlood_', (locationFootprint*2+1), 'km')
fwrite(dataOutput, paste0(customerFolder, fileName, thisDate, '.csv'))





###################################################################################################################################################################################################
#### calculating aggregated score and relative hazard values
mainHazardOutputs = fread(paste0(customerFolder, 'processedOutput_', thisDate, '.csv'))
appendedOutputs = subset(fread(appendedHazardFileLoc), Scenario %in% c('RCP 2.6', 'RCP 4.5', 'RCP 8.5'))
appendedOutputs$Relative_Hazard_Score = NA	;	appendedOutputs$Relative_Hazard_Score_Number = NA	;	appendedOutputs$Trend_Aggregated_For_Looker = NA	
specializedOutputs = fread(paste0(customerFolder, fileName, thisDate, '.csv'))
dataOutput = merge(mainHazardOutputs,  specializedOutputs, all = TRUE)
dataOutput = merge(dataOutput, appendedOutputs, all = TRUE)
#dataOutput = merge(specializedOutputs, appendedOutputs, all = TRUE)
allHazards = unique(dataOutput$Hazard)

theScenarios = c('RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5')

for(thisDecade in unique(dataOutput$Decade))	{
	for(thisScen in c(1,3,4))	{
		for(thisLoc in unique(dataOutput$Location))	{
			avgOfAllHazards = NULL
			for(thisHazard in allHazards)	{
				newHazard = subset(dataOutput, Decade == thisDecade & Scenario == theScenarios[thisScen] & Location == thisLoc & Hazard == thisHazard)$Percentile_Score
					# catching exceptions for when we do not have 4.5 or 8.5
				if(any(is.na(newHazard)))	{
					newHazard[which(is.na(newHazard))] = subset(dataOutput, Decade == thisDecade & Scenario == theScenarios[thisScen - 1] & Location == thisLoc & Hazard == thisHazard)$Percentile_Score[which(is.na(newHazard))]
				}
				avgOfAllHazards = c(avgOfAllHazards, mean(newHazard, na.rm=TRUE))
			}

			locRow = which(customerTable$Location == thisLoc)
			dataOutput = rbind(dataOutput,
				data.frame(
					User = userName,
					Location = customerTable$Location[locRow],
					Region = customerTable$Region[locRow],
					Subregion = customerTable$Subregion[locRow],
					Lat = customerTable$Lat[locRow],
					Lon = customerTable$Lon[locRow],
					Hazard = "Aggregate Climate Score",
					Hazard_Measure = NA,
					Decade = thisDecade,
					Scenario = theScenarios[thisScen],
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
					Advanced_Data_Measures_Units = NA)
			)
		}
	}
}

	# quick fix to rename scenarios
scenariosRename = c('Low Emissions', 'Middle of the Road', 'Middle of the Road', 'High Emissions')
for(i in 1:length(scenariosRename))	{
	dataOutput$Scenario[dataOutput$Scenario == theScenarios[i]] = scenariosRename[i]
}
	
	
	# identifying relative hazard scores
for(thisRow in 1:nrow(relHazScores))	{
	dataOutput$Relative_Hazard_Score[which(dataOutput$Percentile_Score > relHazScores$Hazard_Percentile[thisRow])] = paste0(thisRow, '. ', relHazScores$Hazard_Common_Name[thisRow])
	dataOutput$Relative_Hazard_Score_Number[which(dataOutput$Percentile_Score > relHazScores$Hazard_Percentile[thisRow])] = thisRow
}		
			
	# aggregating trends for looker db
dataOutput$Trend_Aggregated_For_Looker = 0
signifiDecreaseRows = which(dataOutput$Decadal_Trend_Significance < 0.5 & dataOutput$Long_Term_Trend_Significance < 0.05 & dataOutput$Decadal_Trend_Strength < 0 & dataOutput$Long_Term_Trend_Strength < 0)
signifiIncreaseRows = which(dataOutput$Decadal_Trend_Significance < 0.5 & dataOutput$Long_Term_Trend_Significance < 0.05 & dataOutput$Decadal_Trend_Strength > 0 & dataOutput$Long_Term_Trend_Strength > 0)
dataOutput$Trend_Aggregated_For_Looker[signifiDecreaseRows] = dataOutput$Decadal_Trend_Strength[signifiDecreaseRows]
dataOutput$Trend_Aggregated_For_Looker[signifiIncreaseRows] = dataOutput$Decadal_Trend_Strength[signifiIncreaseRows]

fwrite(dataOutput, paste0(customerFolder, 'processedOutputForAllHazards_', userName, '_', thisDate, '.csv'))
	
summary(subset(dataOutput, Decade == 2090 & Scenario == "Middle of the Road" & Hazard == 'Water Demand'))
summary(subset(dataOutput, Decade == 2090 & Scenario == "High Emissions" & Hazard == 'Aggregate Climate Score'))
summary(subset(dataOutput, Decade == 2090 & Scenario == "Middle of the Road" & Hazard == 'Aggregate Climate Score'))
summary(subset(dataOutput, Decade == 2090 & Scenario == "Low Emissions" & Hazard == 'Aggregate Climate Score'))
plot(subset(dataOutput, Scenario == 'Middle of the Road' & Hazard == 'Hurricanes' & Location == customerTable$Location[1])$Raw_Hazard_Value)	
	
	
	
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
