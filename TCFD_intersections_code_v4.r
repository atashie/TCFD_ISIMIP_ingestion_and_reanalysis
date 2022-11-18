library(data.table)
library(ncdf4)

userName = 'Corbion'	#'HMClause'
hazardFolder = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
customerFolder = 'J:\\Cai_data\\TCFD\\locations\\Corbion_Nov2022\\'

customerTable = fread(paste0(customerFolder, 'Corbion_Locations_Nov2022 - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'
hazardTable = fread(paste0(customerFolder, 'Hazard_Tables_Corbion_Nov2022 - Hazard Definitions.csv'))							# 
relHazScores = fread(paste0(customerFolder, 'Hazard_Tables_Corbion_Nov2022 - Hazard Scores.csv'))				
appendedHazardNames = c("River Flood (Local)", 'poopies')
appendedHazardFileLoc = 'J:\\Downloads\\whatisinaname.csv'

############################################################################################
## basic exposure data
dataOutput = data.frame(User = NA, Location = NA, Region = NA, Subregion = NA, Lat = NA, Lon = NA,
	Hazard = NA, Hazard_Measure = NA, Decade = NA, Scenario = NA,
	Raw_Hazard_Value = NA, Percentile_Score = NA, Relative_Hazard_Score = NA, Decadal_Trend_Strength = NA, Decadal_Trend_Significance = NA, Long_Term_Trend_Strength = NA, Long_Term_Trend_Significance = NA)
	
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
								Scenario = rep(c('RCP 2.6', 'RCP 6.0'), each = 9),
								Raw_Hazard_Value = c(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 1, 1], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 2, 1]),
								Percentile_Score = c(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 1, 2], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 2, 2]),
								Relative_Hazard_Score = NA,
								Decadal_Trend_Strength = c(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 1, 3], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 2, 3]),
								Decadal_Trend_Significance = c(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 1, 4], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 2, 4]),
								Long_Term_Trend_Strength = c(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 1, 5], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 2, 5]),
								Long_Term_Trend_Significance = c(ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 1, 6], ncvar_get(hazardMeasureNC, 'tcfdVariable')[closeLon, closeLat, , 2, 6])
								))
					}
					print(c(thisHazard, thisHazardMeasure, thisLocation))
				}
				nc_close(hazardMeasureNC)
			}
		}
	}
}
dataOutput = dataOutput[-1,]
fwrite(dataOutput, paste0(customerFolder, 'processedOutput_', Sys.Date(), '.csv'))














###################################################################################################################################################################################################
#### specialized data, for now just localized flooding
# step 3 intersecting recurrence intervals with flood depth
library(raster)
locationFootprint = 3		# how big is the footprint of the location of interest? in number of 'boxes' to search to the left and right (so 0 is equal to 1 km^2, 1 is 3x3=9 km^2, 2 is 5x5=25 km^2, 3 is 7x7=49, 4 is 9x9=81, etc.
dataOutputLoc = 'J:\\Cai_data\\TCFD\\CustomerOutputs\\'
waterMaskLoc = 'J:\\Cai_data\\TCFD\\CurrentFloodHazard\\LandMaskArray.rds'


	# defining rcp scenarios, recurrence intervals, and decades of interest; this should not change for the foreseeable future
rcpScenarios = c('RCP 2.6', 'RCP 6.0')
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
	Raw_Hazard_Value = NA, Percentile_Score = NA, Relative_Hazard_Score = NA, Decadal_Trend_Strength = NA, Decadal_Trend_Significance = NA, Long_Term_Trend_Strength = NA, Long_Term_Trend_Significance = NA)

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


for(thisIntrvl in 1:length(recurIntrvls))	{
	hazardDepthName = paste0("Avg Flood Depth (m) ", recurIntrvls[thisIntrvl], 'yr Flood')
	hazardLikliName = paste0('Flood Areal Extent (%) ', recurIntrvls[thisIntrvl], 'yr Flood')
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
									Long_Term_Trend_Significance = fldRcrSignif_all))					# Long_Term_Trend_Significance					

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
									Long_Term_Trend_Significance = fldRcrSignif_all))	# Long_Term_Trend_Significance						}
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
								Long_Term_Trend_Significance = fldRcrSignif_all))	# Long_Term_Trend_Significance						}
					}
				} # for each decade

			dataOutput[which(dataOutput$Location == customerTable$Location[j] & dataOutput$Scenario == rcpScenarios[thisScenario]), ]$Long_Term_Trend_Strength = last(dataOutput)$Decadal_Trend_Strength

			} # for each rcp scenario
		}	else	{	# if there are not historical floods out to 500 yrs
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
					Decade = 2000 + rep(whichDecades, each = 2),
					Scenario = rcpScenarios[1],
					Raw_Hazard_Value = 0,				# Raw_Hazard_Value
					Percentile_Score = NA,				# Percentile_Score
					Relative_Hazard_Score = NA,			# Relative_Hazard_Score
					Decadal_Trend_Strength = 0,			# Decadal_Trend_Strength
					Decadal_Trend_Significance = 0,		# Decadal_Trend_Significance
					Long_Term_Trend_Strength = 0,		# Long_Term_Trend_Strength
					Long_Term_Trend_Significance = 0),	# Long_Term_Trend_Significance
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
					Decadal_Trend_Strength = 0,			# Decadal_Trend_Strength
					Decadal_Trend_Significance = 0,		# Decadal_Trend_Significance
					Long_Term_Trend_Strength = 0,		# Long_Term_Trend_Strength
					Long_Term_Trend_Significance = 0))	# Long_Term_Trend_Significance
		}
		print(c(j, thisIntrvl))
	}
}	

dataOutput$Raw_Hazard_Value = as.numeric(dataOutput$Raw_Hazard_Value)
dataOutput = dataOutput[-1,]

	# defining relative flood hazard
basSeqDepth = seq(0.01, 10, length.out=80)
basSeqLikli = seq(1, 100, length.out=80)
relFloodHazardDepth = (basSeqDepth^2 / max(basSeqDepth^2)) * 10 + min(basSeqDepth)
relFloodHazardLikli = basSeqLikli
#relFloodHazard = c(rep(0, 33), seq(0.01,10,length.out=(67)))
dataOutput$Percentile_Score = 1
depthRows = which(dataOutput$Hazard_Measure == hazardDepthName)
likliRows = which(dataOutput$Hazard_Measure == hazardLikliName)
for(ll in 1:length(relFloodHazardDepth))	{
	dataOutput$Percentile_Score[depthRows][which(dataOutput$Raw_Hazard_Value[depthRows] > relFloodHazardDepth[ll])] = ll + 20
	dataOutput$Percentile_Score[likliRows][which(dataOutput$Raw_Hazard_Value[likliRows] > relFloodHazardLikli[ll])] = ll + 20
}

fileName = paste0(userName, '_', Sys.Date(), '_highRestFlood_', (locationFootprint*2+1), 'km')
fwrite(dataOutput, paste0(customerFolder, fileName, Sys.Date(), '.csv'))





###################################################################################################################################################################################################
#### calculating aggregated score and relative hazard values
mainHazardOutputs = fread(paste0(customerFolder, 'processedOutput_', Sys.Date(), '.csv'))
#appendedOutputs = fread(appendedHazardFileLoc)
specializedOutputs = fread(paste0(customerFolder, fileName, Sys.Date(), '.csv'))
#dataOutput = rbind(mainHazardOutputs, appendedOutputs, specializedOutputs)
dataOutput = rbind(mainHazardOutputs,  specializedOutputs)

for(thisDecade in unique(dataOutput$Decade))	{
	for(thisScen in unique(dataOutput$Scenario))	{
		for(thisLoc in unique(dataOutput$Location))	{
			avgOfAllHazards = NULL
			for(thisHazard in unique(dataOutput$Hazard))	{
				avgOfAllHazards = c(avgOfAllHazards, mean(subset(dataOutput, Decade == thisDecade & Scenario == thisScen & Location == thisLoc & Hazard == thisHazard)$Percentile_Score, na.rm=TRUE))
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
					Scenario = thisScen,
					Raw_Hazard_Value = NA,
					Percentile_Score = mean(avgOfAllHazards, na.rm=TRUE),
					Relative_Hazard_Score = NA,
					Decadal_Trend_Strength = NA,
					Decadal_Trend_Significance = NA,
					Long_Term_Trend_Strength = NA,
					Long_Term_Trend_Significance = NA)
			)
		}
	}
}
			
	# identifying relative hazard scores
for(thisRow in 1:nrow(relHazScores))	{
	dataOutput$Relative_Hazard_Score[which(dataOutput$Percentile_Score > relHazScores$Hazard_Percentile[thisRow])] = relHazScores$Hazard_Common_Name[thisRow]
}		
			
	# aggregating trends for looker db
dataOutput$Trend_Aggregated_For_Looker = 0
signifiDecreaseRows = which(dataOutput$Decadal_Trend_Significance < 0.05 & dataOutput$Long_Term_Trend_Significance < 0.05 & dataOutput$Decadal_Trend_Strength < 0 & dataOutput$Long_Term_Trend_Strength < 0)
signifiIncreaseRows = which(dataOutput$Decadal_Trend_Significance < 0.05 & dataOutput$Long_Term_Trend_Significance < 0.05 & dataOutput$Decadal_Trend_Strength > 0 & dataOutput$Long_Term_Trend_Strength > 0)
dataOutput$Trend_Aggregated_For_Looker[signifiDecreaseRows] = dataOutput$Decadal_Trend_Strength[signifiDecreaseRows]
dataOutput$Trend_Aggregated_For_Looker[signifiIncreaseRows] = dataOutput$Decadal_Trend_Strength[signifiIncreaseRows]

fwrite(dataOutput, paste0(customerFolder, 'processedOutputForAllHazards_', Sys.Date(), '.csv'))
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
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
