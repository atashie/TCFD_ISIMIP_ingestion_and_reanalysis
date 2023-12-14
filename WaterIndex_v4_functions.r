##########################################################################################################################################################################################
##########################################################################################################################################################################################
library(ncdf4)
library(data.table)




##########################################################
# assessing historical trends using GRACE-font
	# data resources
# https://podaac.jpl.nasa.gov/dataset/TELLUS_GRFO_L3_CSR_RL06.1_LND_v04?ids=&values=&search=GRFO&temporalSearch=2006-04-03T04:00:00.000ZTO*#
# https://github.com/podaac/data-subscriber
# https://www2.csr.utexas.edu/grace/science_links.html
# https://grace.jpl.nasa.gov/data/data-analysis-tool/
# https://podaac.jpl.nasa.gov/dataset/TELLUS_GRAC-GRFO_MASCON_GRID_RL06.1_V3?ids=&values=&search=GRFO&temporalSearch=2006-04-03T04:00:00.000ZTO*
# https://podaac.jpl.nasa.gov/dataset/TELLUS_GRAC-GRFO_MASCON_CRI_GRID_RL06.1_V3?ids=&values=&search=GRFO&temporalSearch=2006-04-03T04:00:00.000ZTO*#


gracePlotter_f = function(
	customerTable_input = customerTable,
	customerFolder_input = customerFolder,
	clientName_input = clientName,
	graceDataLoc = 'J:\\Cai_data\\Rabo\\GRACE\\GRCTellus.JPL.200204_202211.GLO.RL06.1M.MSCNv03CRI.nc'
	)
	{
	#test = nc_open('J:\\Cai_data\\Rabo\\GRACE\\GRCTellus.JPL.200204_202211.GLO.RL06.1M.MSCNv03.nc')
	gracieNC = nc_open(graceDataLoc)


	nc_lat = ncvar_get(gracieNC, 'lat')	# lat is given from high to low
	nc_lon = ncvar_get(gracieNC, 'lon')	# given from 0.25 to 359.75, so need to convert to neg / pos
	nc_lon[nc_lon > 180] = -(360 - nc_lon[nc_lon > 180])
	nc_time = ncvar_get(gracieNC, 'time')
	startDate = as.Date('2002-01-01')
	graceDates = startDate + nc_time
	nc_landMask = ncvar_get(gracieNC, 'land_mask')
	nc_water = which(nc_landMask == 0)
	#nc_scaleFactor = ncvar_get(gracieNC, 'scale_factor')
	nc_lweThick = ncvar_get(gracieNC, 'lwe_thickness') * 10 # convert cm to mm
	lweThickLand = nc_lweThick ; lweThickLand[nc_water] = NA
	nc_uncertainty = ncvar_get(gracieNC, 'uncertainty') * 10 # convert cm to mm
	uncertLand = nc_uncertainty ; uncertLand[nc_water] = NA

	#image(nc_lon, nc_lat, nc_lweThick[,,1])
	#image(nc_lon, nc_lat, lweThickLand[,,1])
	#image(nc_lon, nc_lat, nc_uncertainty[,,150])
	#image(nc_lon, nc_lat, uncertLand[,,1])

	# defining array for holding climate data
	graceDataTable = data.table(Location = NA, Lat = NA, Lon = NA, LWE_Depth_Median = NA, LWE_Depth_SD = NA, Date = as.Date(NA)) 
	graceTrends = data.table(Location = NA, Lat = NA, Lon = NA, Slope = NA)

	for(thisLoc in 1:nrow(customerTable_input))	{
		closestLat = which.min(abs(nc_lat - customerTable_input$Lat[thisLoc]))
		closestLon = which.min(abs(nc_lon - customerTable_input$Lon[thisLoc]))

			# smoothing / downscaling and reweighting based on distance
		closeishLats = rep(closestLat + c(-2,-1,0,1,2), 5)
		closeishLons = rep(closestLon + c(-2,-1,0,1,2), each = 5)
		closeishLatsVals = (nc_lat[closeishLats] - customerTable_input$Lat[thisLoc])^2
		closeishLonsVals = (nc_lon[closeishLons] - customerTable_input$Lon[thisLoc])^2
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
		png(paste0(customerFolder_input, clientName_input, '\\',  customerTable_input[thisLoc, "Location_Name"], '_', "_GRACE-historicalGW.png"), width=900, height=600)
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
			Location = unlist(customerTable_input[thisLoc, "Location_Name"]),
			Lat = customerTable_input$Lat[thisLoc],
			Lon = customerTable_input$Lon[thisLoc],
			LWE_Depth_Median = theseLweThickLand,
			LWE_Depth_SD = theseUncertLand,
			Date = graceDates))
			
			# saving trends
		graceTrends = rbind(graceTrends, data.table(
			Location = unlist(customerTable_input[thisLoc, "Location_Name"]),
			Lat = customerTable_input$Lat[thisLoc],
			Lon = customerTable_input$Lon[thisLoc],
			Slope = robslopes::TheilSen(as.vector(nc_time), theseLweThickLand)$slope * 365.25))
	}
	fwrite(graceDataTable, paste0(customerFolder_input, clientName_input, '\\',  clientName_input, "_graceHistorical.csv"))
	fwrite(graceTrends[-1,], paste0(customerFolder_input, clientName_input, '\\',  clientName_input, "_graceTrends.csv"), quote = FALSE)
}





#### concatenating data
cropPhenologyTable = "J:\\Cai_data\\WaterIndex\\crop_info\\cropWaterNeedsLookupTable_v1.csv"
hydroBasins_crops_input = "J:\\Cai_data\\WaterIndex\\crop_info\\global_crops\\global.shp"

hydroBasins_crops = sf::st_read(hydroBasins_crops_input)
hydroBasins_crops = sf::st_drop_geometry(hydroBasins_crops)
hydroBasins_crops$REST = hydroBasins_crops$REST + 0.0001
hydroBasinsCropsNames = names(hydroBasins_crops[,-c(1:5)])
cropWaterUseTable = hydroBasins_crops[, -c(1:5)]
cropGrowDaysTable = hydroBasins_crops[, -c(1:5)]
hydroBasins_crops$totMapspam = apply(hydroBasins_crops[,-c(1:5)], 1, sum)

cropPhenologyTable = data.table::fread("J:\\Cai_data\\WaterIndex\\crop_info\\cropWaterNeedsLookupTable_v1.csv")
for(i in 1:length(hydroBasinsCropsNames))	{
	cropWaterUseTable[,i] = cropPhenologyTable$AvgWaterNeeds[which(cropPhenologyTable$Symbol == hydroBasinsCropsNames[i])]
	cropGrowDaysTable[,i] = cropPhenologyTable$AvgGrowSeason[which(cropPhenologyTable$Symbol == hydroBasinsCropsNames[i])]
}
cropWaterUseTotals_nonIrr = (cropWaterUseTable * hydroBasins_crops[,-c(1:5,48)]) / hydroBasins_crops[,48]
cropWaterUseTotals_irr = (cropWaterUseTable * hydroBasins_crops[,-c(1:5,48)]) / hydroBasins_crops[,48]
cropGrowDaysTotals = (cropGrowDaysTable * hydroBasins_crops[,-c(1:5,48)]) / hydroBasins_crops[,48]
hydroBasins_crops$waterUse_nonIrr = apply(cropWaterUseTotals_nonIrr, 1, sum)
hydroBasins_crops$waterUse_irr = apply(cropWaterUseTotals_irr, 1, sum)
hydroBasins_crops$growDaysAvg = apply(cropGrowDaysTotals, 1, sum)


test = sf::st_read("J:\\Cai_data\\BasinATLAS_Data_v10\\BasinATLAS_v10\\BasinATLAS_v10_lev12.shp")
sf::sf_use_s2(FALSE)
hydroBasins_orig = sf::st_centroid(test)
hydroBasins_orig = cbind(hydroBasins_orig, sf::st_coordinates(hydroBasins_orig))
hydroBasins_orig = sf::st_drop_geometry(hydroBasins_orig)	
rm(test)
#headdata.table::fwrite(hydroBasins_orig[,c(1, 7:8, 59:60, 73:74, 101:102, 257:258, 233:234, 237:238, 295:296)], 'J:\\Cai_data\\BasinATLAS_Data_v10\\basinAt12_df_centroid_lev12.csv')

hydroBasins_all = merge(hydroBasins_crops, hydroBasins_orig[,c(1, 7:8, 59:60, 73:74, 101:102, 257:258, 233:234, 237:238, 295:296)], all=TRUE, by = "HYBAS_ID")
rm(hydroBasins_crops)
rm(hydroBasins_orig)
data.table::fwrite(hydroBasins_all, "J:\\Cai_data\\BasinATLAS_Data_v10\\basinAt12_df_centroid_wCrop_simple.csv")



##########################################################
# 3A- parsing, smoothing, and plotting the raw climate+hydro data
climateDataSelection_f = function(	
#	climVars = c('Precipitation_decadalRawVals', 'PotentialEvapotranspiration_decadalRawVals', 'GroundwaterRecharge_decadalRawVals', 'Streamflow_decadalRawVals',	'RootZoneSoilMoisture_decadalRawVals', 'TotalWaterStorage_decadalRawVals'),
	climVars = c("waterIndexUnderlyingData_precip", "waterIndexUnderlyingData_potevap","waterIndexUnderlyingData_qr", "waterIndexUnderlyingData_dis",	"waterIndexUnderlyingData_rootmoist", "waterIndexUnderlyingData_tws"),
	climVarScalars = c(60*60*24*365.25,  60*60*24*365.25, 60*60*24*365.25, 60*60*24*365.25 / (1000^3), 1, 100),
	ncFileLoc = 'J:\\Cai_data\\WaterIndex\\',
	watershedInfoLoc = "J:\\Cai_data\\BasinATLAS_Data_v10\\basinAt12_df_centroid_wCrop_simple.csv",
	customerTable_input = customerTable,
	clientName_input = clientName,
	historicData = FALSE,
	customerFolder_input = customerFolder
	)
	{
	
	#reading in hydroBasins data 
	hydroBasins_df = data.table::fread(watershedInfoLoc)
	m3persec_to_km3peryr = (60*60*24*365.25) / (1000^3)
	hydroBasins_out_df = hydroBasins_df[1:nrow(customerTable_input)]
	
	# filling arrays with required climate data
	for(thisClimVar in 1:length(climVars))	{
		print(paste0(thisClimVar, ' out of ', length(climVars), ' climate variables'))
		ncName = climVars[thisClimVar]
		nc_file =  nc_open(paste0(ncFileLoc, ncName, '.nc'))
		nc_lat = ncvar_get(nc_file, 'lat')	# lat is given from high to low
		nc_lon = ncvar_get(nc_file, 'lon')
		nc_decade = ncvar_get(nc_file, 'decade')
		nc_scenario = ncvar_get(nc_file, 'scenario') #ncvar_get(nc_file, 'rcpScen')
		nc_valueClass = ncvar_get(nc_file, 'value_type') # ncvar_get(nc_file, 'valueClass')
#		nc_values = ncvar_get(nc_file, names(nc_file$var)) # Lon, Lat, Decade (2010-2090), Scenarios (Low, Med, High), Value Type (1-12: months, 13: annual mean, 14:20: annual Q05, Q15, Q25, Q50, Q75, Q85, Q95)
		nc_values = ncvar_get(nc_file, names(nc_file$var)) * climVarScalars[thisClimVar] # Decade (2010-2090), Value Type (1-12: months, 13: annual mean, 14:20: annual Q05, Q15, Q25, Q50, Q75, Q85, Q95), Scenarios (Low, 45, 60, High), Lon, Lat


			# !!!!!!!!! temp fix: pre 2010s data can be problematic for some variables (sharp demarcation between 1980-2006 and 2006-2099)
		if(!historicData)	{nc_values = nc_values[-c(1:3), , , , ] ;  nc_decade = nc_decade[-c(1:3)]}
			# !!!!!!!!! end temp fix: pre 2010s data can be problematic for some variables

			# !!!!!!!!! temp fix: different zarrs have different numbers of scenarios, so choosing the first middle and last
		if(length(nc_scenario) > 3)	{nc_values = nc_values[ , , c(1,3,length(nc_scenario)), , ]		;	nc_scenario = c(nc_scenario[1], nc_scenario[3], nc_scenario[length(nc_scenario)])}
			# !!!!!!!!! end temp fix: different zarrs have different numbers of scenarios, so choosing the first middle and last

			# !!!!!!!!! temp fix: monthly values / rates conversion
		if(thisClimVar %in% c(1:4))	{nc_values[ , 1:12, , , ] = nc_values[ , 1:12, , , ] / 12} # converting annual tot to monthly tot
			# !!!!!!!!! end temp fix: monthly soil moisture values


		# defining array for holding climate data
		if(thisClimVar == 1)	{
			myMissingData = -(10^5)
			climateData = array(rep(myMissingData, nrow(customerTable_input) * length(nc_decade) * length(nc_valueClass) * length(nc_scenario) * length(climVars)), 
				dim = c(nrow(customerTable_input), length(nc_decade), length(nc_valueClass), length(nc_scenario), length(climVars)))
		}

		for(thisLoc in 1:nrow(customerTable_input))	{
			closestLat = which.min(abs(nc_lat - customerTable_input$Lat[thisLoc]))
			closestLon = which.min(abs(nc_lon - customerTable_input$Lon[thisLoc]))

			closestBasin = which.min((hydroBasins_df$Y - customerTable_input$Lat[thisLoc])^2 + (hydroBasins_df$X - customerTable_input$Lon[thisLoc])^2)

				# smoothing / downscaling and reweighting based on distance
			closeishLats = rep(closestLat + c(-1,0,1), 3)
			closeishLons = rep(closestLon + c(-1,0,1), each = 3)
			closeishLatsVals = (nc_lat[closeishLats] - customerTable_input$Lat[thisLoc])^2
			closeishLonsVals = (nc_lon[closeishLons] - customerTable_input$Lon[thisLoc])^2
			thisExponent = ifelse(climVars[thisClimVar] != 'Streamflow_decadalRawVals', 2, .5) 		# streamflow inputs may require a wider search
			distanceBox = sqrt(closeishLatsVals + closeishLonsVals)
	#		boxWeighting = max(distanceBox, na.rm=TRUE) - distanceBox
			boxWeighting = 1 - distanceBox # since max distance of a 2x2 .5 deg grid is 1 deg

				# un-weighting water / ocean tiles
			for(thisIter in 1:length(closeishLats))	{
				if(is.na(nc_values[1, 1, 1, closeishLons[thisIter], closeishLats[thisIter]]))	{
					boxWeighting[thisIter] = 0
					nc_values[ , , , closeishLons[thisIter], closeishLats[thisIter]] = 0
				}
			}

				# normalizing weighting 
			boxWeighting = boxWeighting^thisExponent / sum(boxWeighting^thisExponent, na.rm=TRUE)


			for(thisScenario in 1:length(nc_scenario))	{ # input nc is in format [decade, valueClass, scenario, lon, lat]
					# initializing array for holding 
				theseClimateValues = nc_values[ , , thisScenario, closeishLons[1], closeishLats[1]] * boxWeighting[1]
					# accounting for ocean tiles
				if(any(is.na(theseClimateValues))) { theseClimateValues[is.na(theseClimateValues)] = 0 }
					
				for(thisWeight in 2:length(boxWeighting))	{
					nextClimateValues = nc_values[ , , thisScenario, closeishLons[thisWeight], closeishLats[thisWeight]] 
					if(any(is.na(nextClimateValues))) { nextClimateValues[is.na(nextClimateValues)] = 0 }
					theseClimateValues = theseClimateValues + nextClimateValues * boxWeighting[thisWeight]# input nc is in format [decade, valueClass, scenario, lon, lat]
				}
				
				
						# rescaling using user location data or hydrobasins data
				if(thisClimVar == 1)	{
						# also, storing basin level hydro data
					hydroBasins_out_df[thisLoc, ] = hydroBasins_df[closestBasin, ]
					if(!is.na(customerTable_input$Precipitation_annualAvg_mm[thisLoc]))	{
						theseClimateValues = theseClimateValues * (customerTable_input$Precipitation_annualAvg_mm[thisLoc] / mean(theseClimateValues[1:2, 13]))
					} else	{theseClimateValues = theseClimateValues * (hydroBasins_df$pre_mm_syr[closestBasin] / mean(theseClimateValues[1:2, 13]))}
				}
				if(thisClimVar == 2)	{
					if(!is.na(customerTable_input$PotentialEvapotranspiration_annualAvg_mm[thisLoc]))	{
						theseClimateValues = theseClimateValues * (customerTable_input$PotentialEvapotranspiration_annualAvg_mm[thisLoc] / mean(theseClimateValues[1:2, 13]))
					} else	{theseClimateValues = theseClimateValues * (hydroBasins_df$pet_mm_syr[closestBasin] / mean(theseClimateValues[1:2, 13]))}
				}
				if(thisClimVar == 3)	{
					if(!is.na(customerTable_input$Groundwater_annualAvgRecharge_mm[thisLoc]))	{
						theseClimateValues = theseClimateValues * (customerTable_input$Groundwater_annualAvgRecharge_mm[thisLoc] / mean(theseClimateValues[1:2, 13]))
					} else	{
						if(thisScenario == 1) {
							gwrechScalar = mean(theseClimateValues[1:2, 13])
						} else {
							theseClimateValues = theseClimateValues * (gwrechScalar / mean(theseClimateValues[1:2, 13]))
							print("no method for downscaling groundwater recharge")
						}
					}
				}
				if(thisClimVar == 4)	{
					if(!is.na(customerTable_input$SurfaceWater_annualAvgFlows_km3[thisLoc]))	{
						theseClimateValues = theseClimateValues * (customerTable_input$SurfaceWater_annualAvgFlows_km3[thisLoc] / mean(theseClimateValues[1:2, 13]))
					} else	{
						if(thisScenario == 1) {
							qScalar = mean(theseClimateValues[1:2, 13])
						} else {
							theseClimateValues = theseClimateValues * (qScalar / mean(theseClimateValues[1:2, 13]))
							print("no method for downscaling streamflow")
						}
					}
				}
				if(thisClimVar == 5)	{
					if(!is.na(customerTable_input$SoilMoisture_annualAvg_pct[thisLoc]))	{
						theseClimateValues = theseClimateValues * (customerTable_input$SoilMoisture_annualAvg_pct[thisLoc] / mean(theseClimateValues[1:2, 13]))
					} else	{theseClimateValues = theseClimateValues * (hydroBasins_df$swc_pc_syr[closestBasin] / mean(theseClimateValues[1:2, 13]))}
				}
				if(thisClimVar == 6)	{
					if(thisScenario == 1) {
						twsScalar = mean(theseClimateValues[1:2, 13])
					} else {
						theseClimateValues = theseClimateValues * (twsScalar / mean(theseClimateValues[1:2, 13]))
						print("no method for downscaling total watershed storage")
					}
				}
			
				climateData[thisLoc, , , thisScenario, thisClimVar] = theseClimateValues

			}
		}
		nc_close(nc_file)
	}

	theseDecades = 1:length(nc_decade)
	saveRDS(climateData[ , theseDecades, , , ], paste0(customerFolder_input, clientName_input, '\\',  clientName_input, '_rawValues.rds'))
	data.table::fwrite(hydroBasins_out_df, paste0(customerFolder_input, clientName_input, '\\',  clientName_input, '_hydroBasins.csv'))
}

# data assessment
#for(ggg in 1:20){
#print(summary(as.vector(nc_values[1, ggg, 3, , ]))*100000000)
#}
#nc_close(nc_file)





	
##########################################################
# 3B- # standardized plots of data
climateDataPlotting_f = function(	
#	climVars = c('Precipitation_decadalRawVals', 'PotentialEvapotranspiration_decadalRawVals', 'GroundwaterRecharge_decadalRawVals', 'Streamflow_decadalRawVals',	'RootZoneSoilMoisture_decadalRawVals', 'TotalWaterStorage_decadalRawVals'),
	climVars = c("waterIndexUnderlyingData_precip", "waterIndexUnderlyingData_potevap","waterIndexUnderlyingData_qr", "waterIndexUnderlyingData_dis",	"waterIndexUnderlyingData_rootmoist", "waterIndexUnderlyingData_tws"),
	ncFileLoc = 'J:\\Cai_data\\WaterIndex\\',
	climVarNames = c('Precip (mm)', 'PET (mm)', 'GW Rech (mm)', 'Streamflow (km^2)', 'RZ Soil Moisture (%)', 'Total Storage (mm)'),
	customerTable_input = customerTable,
	scenarioNames = c('LowEmissions', 'MedEmissions', 'HighEmissions'),
	rawDataColumnNames = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Mean', 'Q05', 'Q15', 'Q25', 'Q50', 'Q75', 'Q85', 'Q95'),
	clientName_input = clientName,
	customerFolder_input = customerFolder,
	historicData = FALSE
	)
	{
		# initializing data
	climDataPlot = readRDS(paste0(customerFolder_input, clientName_input, '\\',  clientName_input, '_rawValues.rds'))
	if(any(climDataPlot < 0))	{climDataPlot[which(climDataPlot < 0)] = 0.0001}

#	if(any(climDataAll[ , , , , ] < 0))	{climDataAll[climDataAll[ , , , , 3] < 0] = 0}	# zero negative recharge, but no other values should be negative
#	if(any(climDataAll <= 0))	{climDataAll[climDataAll <= 0] = 1 ; print('check the data')}	# zero negative recharge, but no other values should be negative

	ncName = climVars[1]
	nc_file =  nc_open(paste0(ncFileLoc, ncName, '.nc'))
	nc_decade = ncvar_get(nc_file, 'decade')
		# !!!!!!!!! temp fix: pre 2010s data can be problematic for some variables (sharp demarcation between 1980-2006 and 2006-2099)
	if(!historicData)	{nc_decade = nc_decade[-c(1:3)]}
		# !!!!!!!!! end temp fix: pre 2010s data can be problematic for some variables
	nc_close(nc_file)

	for(thisLoc in 1:nrow(customerTable_input))	{
		for(thisScen in 1:length(scenarioNames))	{

			png(paste0(customerFolder_input, clientName_input, '\\',  customerTable_input[thisLoc, "Location_Name"], '_', scenarioNames[thisScen], "_rawValues.png"), width=900, height=900)
			par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(3,2), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
			windowsFonts(A = windowsFont("Roboto"))

			for(thisClimVar in 1:length(climVarNames))	{
					# climDataPlot is arranged as 	[location, decade, valueClass, scenario, climateVariable]
				
				currentAverage = mean(climDataPlot[thisLoc, 1:2, 17, 1:3, thisClimVar])
				ylabPctVals = c(seq(-5,5,0.1))
				ylabPctValLocs = currentAverage + currentAverage * ylabPctVals
			
				yMin = max(min(climDataPlot[thisLoc, , 14, , thisClimVar])*1, 0)#0.985
				yMax = max(climDataPlot[thisLoc, , 20, , thisClimVar])*1.025
				plot(nc_decade, climDataPlot[thisLoc, , 14, thisScen, thisClimVar],  ylim = c(yMin,yMax) ,
					type='l', lwd=1, col='white', xaxt = 'n', #log='y',
					main='', ylab='', xlab='',
					col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
					family='A')
				abline(h=mean(climDataPlot[thisLoc, 1:2, 13, thisScen, thisClimVar]), lwd=2, lty =2, col='#1A232F')
				axis(1, at = nc_decade ,col.lab='#1A232F', col.axis='#666D74', 
					labels = nc_decade)
	#			if(ylabPctValLocs[2] != 0)	{
				axis(4, at = ylabPctValLocs, col.lab='#1A232F', col.axis='#666D74', 
					labels = paste0(round(ylabPctVals * 100, 0), '%'))
	#			}
			#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
				polygon(x=c(nc_decade, rev(nc_decade)), y=c(climDataPlot[thisLoc, , 14, thisScen, thisClimVar], rev(climDataPlot[thisLoc, , 20, thisScen, thisClimVar])),
					col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
				polygon(x=c(nc_decade, rev(nc_decade)), y=c(climDataPlot[thisLoc, , 19, thisScen, thisClimVar], rev(climDataPlot[thisLoc, , 15, thisScen, thisClimVar])),
					col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
				polygon(x=c(nc_decade, rev(nc_decade)), y=c(climDataPlot[thisLoc, , 18, thisScen, thisClimVar], rev(climDataPlot[thisLoc, , 16, thisScen, thisClimVar])),
					col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
				loessSmooth = loess(climDataPlot[thisLoc, , 13, thisScen, thisClimVar] ~ nc_decade)
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
}	







###################################################################
# 3C- calculating and the water index with generic data
waterIndexCalculations_f = function(	
	customerTable_input = customerTable,
	petGlobAvg = 2000,
#	runoffRatio = 0.08,
#	effectiveIrrigationRatio = 0.85,
	humidAI = 0.65,
	clientName_input = clientName,
	customerFolder_input = customerFolder,
	cropPhenologyTable = "J:\\Cai_data\\WaterIndex\\crop_info\\cropWaterNeedsLookupTable_v1.csv",
	indexValues = c('Aridity Index - Avg', 'Plant Water Demand - Avg', 'Aridity Index - Drought', 'Plant Water Demand - Drought', 'Aridity Index w/ Irrigation - Avg', 'Plant Water Demand w/ Irrigation - Avg', 'Aridity Index w/ Irrigation - Drought', 'Plant Water Demand w/ Irrigation - Drought')
	)
	{

#hectToSqKm = 100
	halfDegInKm = 111.1 / 2
	kmToMm = 1000^2

		# array for holding outputs
	myMissingData = NA
		# input array is in format  [location, decade, valueClass,      scenario, climateVariable]
		# output array is in format [location, decade, indexValueQuant, scenario, indexValue, indexValueClass]
	indexValueClass = c('ratio', 'difference')
	indexValueQuant = c('Q05', 'Q15', 'Q25', 'Q50', 'Q75', 'Q85', 'Q95', 'Mn')
	climateData = readRDS(paste0(customerFolder_input, clientName_input, '\\',  clientName_input, '_rawValues.rds'))
	numDecades = length(climateData[1, ,1,1,1])
	indexValuesArray = array(rep(myMissingData, nrow(customerTable) * numDecades * length(indexValueQuant) * length(climateData[1,1,1, ,1]) * length(indexValues) * length(indexValueClass)), 
										dim = c(nrow(customerTable),  numDecades,  length(indexValueQuant),  length(climateData[1,1,1, ,1]),  length(indexValues), length(indexValueClass)))
	calibOuts = data.table::data.table(location = NA, numRuns = NA, meanVal = NA, streamflowRechargeScalar = NA, thisFrcAreaUnderCult = NA, thisFrcCultAreaWthIrr = NA, thisWplant = NA,
		runoffRatio = NA, initialSoilMoisture = NA, effectiveIrrigationRatio = NA, recentHistoricSlope = NA, rescaledRecentHistoricSlope = NA, rechScalar = NA)

		# reading in crop phenology table
#	cropPhenology_df = data.table::fread(cropPhenologyTable)
		# reading in GRACE trends
	historicTrends = data.table::fread(paste0(customerFolder_input, clientName_input, '\\',  clientName_input, "_graceTrends.csv"))
		# reading hydroBasins data
	hydroBasins = data.table::fread(paste0(customerFolder_input, clientName_input, '\\',  clientName_input, '_hydroBasins.csv'))

		# adding new col names
	hydroBasins$Location = customerTable$Location_Name
	
	hydroBasins$currentPrecip_avg = NA		;	hydroBasins$currentPrecip_var = NA
	hydroBasins$trendLow_Precip_avg = NA	;  hydroBasins$trendMed_Precip_avg = NA	; hydroBasins$trendHigh_Precip_avg = NA
	hydroBasins$trendLow_Precip_var = NA	;  hydroBasins$trendMed_Precip_var = NA	; hydroBasins$trendHigh_Precip_var = NA

	hydroBasins$currentPET_avg = NA			;	hydroBasins$currentPET_var = NA	
	hydroBasins$trendLow_PET_avg = NA		;  hydroBasins$trendMed_PET_avg = NA		; hydroBasins$trendHigh_PET_avg = NA

	hydroBasins$currentStreamflow_avg = NA		;	hydroBasins$currentStreamflow_var = NA
	hydroBasins$trendLow_Streamflow_avg = NA	; 	hydroBasins$trendMed_Streamflow_avg = NA	; 	hydroBasins$trendHigh_Streamflow_avg = NA

	hydroBasins$currentAridityIndex_A = NA	;	hydroBasins$currentDeficit_A = NA	;	hydroBasins$currentRatio_A = NA
	hydroBasins$trendLow_Deficit_A = NA		;	hydroBasins$trendMed_Deficit_A = NA	;	hydroBasins$trendHigh_Deficit_A = NA
	hydroBasins$currentAridityIndex_B = NA	;	hydroBasins$currentDeficit_B = NA	;	hydroBasins$currentRatio_B = NA
	hydroBasins$trendLow_Deficit_B = NA		;	hydroBasins$trendMed_Deficit_B = NA	;	hydroBasins$trendHigh_Deficit_B = NA
	hydroBasins$currentDeficit_C = NA		;	hydroBasins$currentRatio_C = NA	
	hydroBasins$trendLow_Deficit_C = NA		;	hydroBasins$trendMed_Deficit_C = NA	;	hydroBasins$trendHigh_Deficit_C = NA
	hydroBasins$currentDeficit_D = NA		;	hydroBasins$currentRatio_D = NA
	hydroBasins$trendLow_Deficit_D = NA		;	hydroBasins$trendMed_Deficit_D = NA	;	hydroBasins$trendHigh_Deficit_D = NA
	
	hydroBasins$recentHistoricSlope = NA
	hydroBasins$rescaledRecentHistoricSlope = NA
	hydroBasins$streamflowRechargeScalar = NA
	hydroBasins$thisFrcAreaUnderCult = NA
	hydroBasins$thisFrcCultAreaWthIrr =NA
	hydroBasins$initialSoilMoisture = NA
	hydroBasins$thisWplant = NA
	hydroBasins$runoffRatio = NA
	hydroBasins$rechScalar = NA

	for(thisRow in 1:nrow(customerTable_input))	{
			# initializing static values
		# defining the growing season
		if(is.na(customerTable_input$GrowMonth_Start[thisRow]) | is.na(customerTable_input$GrowMonth_End[thisRow]))	{
			if(is.na(hydroBasins$growDaysAvg[thisRow]))	{growSeason = 1:12} else {growSeason = round(seq(1,12, length.out=min((25 + hydroBasins$growDaysAvg[thisRow]) / 30, 12)))}
		} else {
			if(customerTable_input$GrowMonth_Start[thisRow] < customerTable_input$GrowMonth_End[thisRow])	{
				growSeason = seq(customerTable_input$GrowMonth_Start[thisRow], customerTable_input$GrowMonth_End[thisRow], 1)
			}
			if(customerTable_input$GrowMonth_Start[thisRow] > customerTable_input$GrowMonth_End[thisRow])	{
				growSeason = unique(c(seq(customerTable_input$GrowMonth_Start[thisRow], 12, by=1), seq(1, customerTable_input$GrowMonth_End[thisRow], by=1)))
			}
			if(customerTable_input$GrowMonth_Start[thisRow] == customerTable_input$GrowMonth_End[thisRow])	{
				growSeason = customerTable_input$GrowMonth_Start[thisRow]
			}
		}

			# defining current surplus / deficit in water availability
		if(!is.na(customerTable_input$CurrentStorageTrend_mm[thisRow]))	{recentHistoricTrend = customerTable_input$CurrentStorageTrend_mm[thisRow]} else {recentHistoricTrend = historicTrends$Slope[thisRow]}
		
			# global average PET standardized by growing season
		petGlobAvgForGrowSeason = petGlobAvg * (length(growSeason) / 12)
	
			# water demand and cultivated area
		thisFrcAreaUnderCult = max(hydroBasins$no_irr_cro[thisRow] + hydroBasins$irr_cropla[thisRow], 0.01, na.rm=TRUE)	# defining water needs by location
		thisFrcCultAreaWthIrr = min(max(hydroBasins$irr_cropla[thisRow], 0.001, na.rm=TRUE), thisFrcAreaUnderCult)
		if(is.na(hydroBasins$waterUse_irr[thisRow])){thisWplant = 650} else {thisWplant = hydroBasins$waterUse_irr[thisRow]} 

			# defining runoff, w/ or w/o on-site storage 
		runoffRatio = 0.08
		if(customerTable_input$OnsiteReservoirs[thisRow]) {storageRescalar = 0} else {storageRescalar = 1}

			# rescalar of recharge, since recharge is poorly constrained
		rechScalar = 1

			# defining irrigation losses
		effectiveIrrigationRatio = 0.85
		if(customerTable$DripIrrigation[thisRow])	{effectiveIrrigationRatio = 0.98}

		thisDivertibleStrmfl = ifelse(is.na(customerTable_input$DivertibleStreamflow_Ratio[thisRow]), 0, customerTable_input$DivertibleStreamflow_Ratio[thisRow]) # defining extractible water by location

			# accounting for soils at field capacity at the start of the growing season
		if(length(growSeason) == 12){initialSoilMoisture = 0} else {initialSoilMoisture = 120}

			# quantiles are combined in some eqs (and e.g. Q05*Q05 --> Q0025), so quantiles for each coincident var must be able to be handled separately
		pptQntsNrml = climateData[thisRow, , c(14:20,13), , 1] /  climateData[thisRow, , rep(13,8), , 1]
		
		pptQntsAvg = sqrt(pptQntsNrml) * climateData[thisRow, , rep(13,8), , 1] # taking the sqrt since the var will be multiplied (and e.g. Q05*Q05 --> Q0025)
		pptQntsDrght = sqrt(pptQntsNrml) * climateData[thisRow, , rep(16,8), , 1]
		pptQntsDrghtShft = pptQntsNrml * mean(climateData[thisRow, , 16, , 1] /  climateData[thisRow, , 17, , 1])

		#pet = potential evapotranspiration
		petQntsNrml = climateData[thisRow, , c(20:14,13), , 2] /  climateData[thisRow, , rep(13,8), , 2]
		petQntsAvg = sqrt(petQntsNrml) * climateData[thisRow, , rep(13,8), , 2]
	#	petQntsDrght = sqrt(petQntsNrml) * climateData[thisRow, , rep(19,7), , 2]

		#strmfl = streamflow
		gridArea = halfDegInKm * (halfDegInKm * cos(customerTable$Lat[thisRow] * pi / 180))
		strmflQntsNrml = climateData[thisRow, , c(14:20,13), , 4] /  climateData[thisRow, , rep(13,8), , 4]
		strmflQntsAvg = sqrt(strmflQntsNrml) * climateData[thisRow, , rep(13,8), , 4]
		strmflQntsDrght = sqrt(strmflQntsNrml) * climateData[thisRow, , rep(16,8), , 4]
		strmflQntsDrghtShft = strmflQntsNrml * mean(climateData[thisRow, , 16, , 4] /  climateData[thisRow, , 17, , 4])

		
			# Aridity Index - Avg
#		aridityIndex_rel = (pptQntsAvg / petQntsAvg) / humidAI
#		aridityIndex_sclr = 1.5 - aridityIndex_rel; aridityIndex_sclr[which(aridityIndex_rel > 1.5)] = 0
		aridityIndex = pptQntsAvg / petQntsAvg
		indexValuesArray[thisRow, , , , 1, 1] =  aridityIndex / humidAI
		indexValuesArray[thisRow, , , , 1, 2] = (pptQntsAvg - petQntsAvg * humidAI)

			# using the Aridity Index to estimate streamflow losses to groundwater (i.e., regionally driven groundwater recharge)
		streamflowRechargeScalar = max(1 - aridityIndex[1:3, 4, ]^(1/20), 0)

			# customer inputs defining streamflow diversions
		streamflowImports_km2 = 0
		if(!is.na(customerTable$SurfaceWaterDiversion_averageInCubicKM[thisRow]))	{streamflowImports_km2 = customerTable$SurfaceWaterDiversion_averageInCubicKM[thisRow]}

		tooHigh = TRUE
		tooLow = TRUE
		numRuns = 0
		while(any(c(tooHigh, tooLow)))	{
			numRuns = numRuns + 1

				# customer inputs defining streamflow capture
			if(!is.na(customerTable$SurfaceWaterDiversion_Percent[thisRow]))	{streamflowRechargeScalar = customerTable$SurfaceWaterDiversion_Percent[thisRow]}
			streamflowImports_mm =  kmToMm * (streamflowImports_km2 / hydroBasins$SUB_AREA[thisRow]) / thisFrcCultAreaWthIrr
				

				# Plant Water Demand - Avg
			meanToMedianRatio_ppt = 1 # mean(climateData[thisRow, , 17, , 1] / climateData[thisRow, , 13, , 1])
			meanToMedianRatio_pet = 1 # mean(climateData[thisRow, , 17, , 2] / climateData[thisRow, , 13, , 2])
			growSeasonPPTqntsAvg = sqrt(pptQntsNrml) * climateData[thisRow, , rep(growSeason[1], 8), , 1] * meanToMedianRatio_ppt# taking the sqrt since the var will be multiplied (and e.g. Q05*Q05 --> Q0025)
			growSeasonPETqntsAvgRatio = sqrt(petQntsNrml) * climateData[thisRow, , rep(growSeason[1],8), , 2] * meanToMedianRatio_pet / petGlobAvgForGrowSeason
			for(thisMonth in growSeason[-1])	{
				growSeasonPPTqntsAvg = growSeasonPPTqntsAvg + sqrt(pptQntsNrml) * climateData[thisRow, , rep(thisMonth, 8), , 1] * meanToMedianRatio_ppt
				growSeasonPETqntsAvgRatio = growSeasonPETqntsAvgRatio + sqrt(petQntsNrml) * climateData[thisRow, , rep(thisMonth, 8), , 2]  * meanToMedianRatio_pet/ petGlobAvgForGrowSeason
			}
			petScalar = growSeasonPETqntsAvgRatio; petScalar[which(petScalar < 0.7)] = 0.7; petScalar[which(petScalar > 1.5)] = 1.5


				# estimating recharge ratios
			rechRaw = rechScalar * climateData[thisRow, , 13, , 3]
			if(any(rechRaw < 0))	{rechRaw[which(rechRaw < 0)] = 0}
			rechRatio = mean(rechRaw) / mean(climateData[thisRow, , 13, , 1])
			rechAvg = climateData[thisRow, , c(14:20,13), , 3]
			if(any(rechAvg < 0))	{rechAvg[which(rechAvg < 0)] = 0}
	

				# estimating effective precip and recharge
			effectivePPT = initialSoilMoisture * min(aridityIndex, 1) + (1 - runoffRatio * storageRescalar - rechRatio) * growSeasonPPTqntsAvg
			effectiveRech_local = effectiveIrrigationRatio * rechAvg * thisFrcAreaUnderCult
			effectiveRech_regional = 0.25 * effectiveIrrigationRatio * rechAvg / thisFrcAreaUnderCult
			effectiveRech = effectiveRech_local + effectiveRech_regional
			effectiveWPlant = petScalar * thisWplant
			indexValuesArray[thisRow, , , , 2, 1] = effectivePPT / effectiveWPlant
			indexValuesArray[thisRow, , , , 2, 2] = effectivePPT - effectiveWPlant




			# Aridity Index - Drought
			indexValuesArray[thisRow, , , , 3, 1] = (pptQntsDrght / petQntsAvg) / humidAI
			indexValuesArray[thisRow, , , , 3, 2] = pptQntsDrght - petQntsAvg * humidAI
			# Plant Water Demand - Drought
			growSeasonPPTqntsdrought_local = sqrt(pptQntsDrghtShft) * climateData[thisRow, , rep(growSeason[1], 8), , 1]	
	#		if(customerTable$Soil_Moisture[thisRow])	{growSeasonPPTqntsdrought_local[,,] = 0}				# zeroing soil moisture is it cannot be used
			for(thisMonth in growSeason[-1])	{
				growSeasonPPTqntsdrought_local = growSeasonPPTqntsdrought_local + sqrt(pptQntsDrghtShft) * climateData[thisRow, , rep(thisMonth, 8), , 1]	
			}
			effectivePPTdrought = 0.5 * initialSoilMoisture * min(aridityIndex, 1) + (1 - runoffRatio * storageRescalar - rechRatio) * growSeasonPPTqntsdrought_local 
			indexValuesArray[thisRow, , , , 4, 1] = effectivePPTdrought / effectiveWPlant
			indexValuesArray[thisRow, , , , 4, 2] = effectivePPTdrought - effectiveWPlant

			
			# Aridity Index w/ Irrigation - Avg
				# using the Aridity Index to estimate streamflow losses to groundwater (i.e., regionally driven groundwater recharge)
			effectiveStrmfl = 	effectiveIrrigationRatio * (streamflowImports_mm + (streamflowRechargeScalar / thisFrcCultAreaWthIrr) * (strmflQntsAvg / gridArea) * kmToMm)
			effectiveStrmflDrght = 	effectiveIrrigationRatio * (streamflowImports_mm * 0.5 + (streamflowRechargeScalar / thisFrcCultAreaWthIrr) * (strmflQntsDrght / gridArea) * kmToMm)
#			if(any(effectiveStrmfl > 1000))	{effectiveStrmfl[effectiveStrmfl > 1000] = 1000}
#			if(any(effectiveStrmflDrght > 1000))	{effectiveStrmflDrght[effectiveStrmflDrght > 1000] = 1000}

			indexValuesArray[thisRow, , , , 5, 1] = ((pptQntsDrght + effectiveRech + effectiveStrmfl) / petQntsAvg) / humidAI
			indexValuesArray[thisRow, , , , 5, 2] = (pptQntsDrght + effectiveRech + effectiveStrmfl) - petQntsAvg * humidAI
				# Plant Water Demand w/ Irrigation - Avg'
			rechAvg_local = effectiveRech
			if(customerTable$Groundwater_Access[thisRow])	{growSeasonPPTqntsdrought_local[,,] = 0}				# zeroing groundwater if it cannot be used
			indexValuesArray[thisRow, , , , 6, 1] = (effectivePPT + rechAvg_local + effectiveStrmfl) / effectiveWPlant
			indexValuesArray[thisRow, , , , 6, 2] = (effectivePPT + rechAvg_local + effectiveStrmfl) - effectiveWPlant

				# Aridity Index w/ Irrigation - Drought
			indexValuesArray[thisRow, , , , 7, 1] = ((pptQntsDrght + rechAvg_local + effectiveStrmflDrght) / petQntsAvg) / humidAI
			indexValuesArray[thisRow, , , , 7, 2] = (pptQntsDrght + rechAvg_local + effectiveStrmflDrght) - petQntsAvg * humidAI
				# Plant Water Demand w/ Irrigation - Drought
			indexValuesArray[thisRow, , , , 8, 1] = (effectivePPTdrought + rechAvg_local + effectiveStrmflDrght) / effectiveWPlant
			indexValuesArray[thisRow, , , , 8, 2] = (effectivePPTdrought + rechAvg_local + effectiveStrmflDrght) - effectiveWPlant

			calibOuts = rbind(calibOuts, 
				data.table::data.table(
					location = customerTable_input$Location_Name[thisRow],
					numRuns = numRuns,
					meanVal = mean(indexValuesArray[thisRow, 1:2, 8, , 6, 2]),
					streamflowRechargeScalar = streamflowRechargeScalar,
					thisFrcAreaUnderCult = thisFrcAreaUnderCult,
					thisFrcCultAreaWthIrr = thisFrcCultAreaWthIrr,
					thisWplant = thisWplant,
					runoffRatio = runoffRatio,
					initialSoilMoisture = initialSoilMoisture,
					effectiveIrrigationRatio = effectiveIrrigationRatio,
					recentHistoricSlope = recentHistoricTrend,
					rescaledRecentHistoricSlope = recentHistoricTrend / max(thisFrcCultAreaWthIrr, .0125),
					rechScalar = rechScalar)
			)
		
				# recalibrating if necessary
			if(( -5 + mean(indexValuesArray[thisRow, 1:2, 8, , 6, 2]) * max(thisFrcCultAreaWthIrr, .001)) > recentHistoricTrend |
			  (-500 + mean(indexValuesArray[thisRow, 1:2, 8, , 6, 2])) > (recentHistoricTrend / max(thisFrcCultAreaWthIrr, .001))) {
				streamflowRechargeScalar = max(streamflowRechargeScalar * 0.67 - 0.0005, 0)
				thisFrcAreaUnderCult = min(thisFrcAreaUnderCult * 1.005 + 0.01, 1)
				thisFrcCultAreaWthIrr = min(min(thisFrcCultAreaWthIrr * 1.005 + 0.01, 1), thisFrcAreaUnderCult)
				thisWplant = thisWplant * 1.02
				initialSoilMoisture = initialSoilMoisture * 0.95
				runoffRatio = min(runoffRatio * 1.05 + 0.01, 1)
				effectiveIrrigationRatio = effectiveIrrigationRatio * 0.99
				rechScalar = rechScalar * 0.9
			} else	{tooHigh = FALSE}
			if(( 5 + mean(indexValuesArray[thisRow, 1:2, 8, , 6, 2]) * max(thisFrcCultAreaWthIrr, .001)) < recentHistoricTrend  |
			  (500 + mean(indexValuesArray[thisRow, 1:2, 8, , 6, 2])) < (recentHistoricTrend / max(thisFrcCultAreaWthIrr, .001))) {
				streamflowRechargeScalar = min(streamflowRechargeScalar * 1.01 + 0.0001, 0.65)
				thisFrcAreaUnderCult = max(thisFrcAreaUnderCult * 0.995 - 0.005, 0.001)
				thisFrcCultAreaWthIrr = max(thisFrcCultAreaWthIrr * 0.995 - 0.005, 0.001)
				initialSoilMoisture = initialSoilMoisture * 1.01
				thisWplant = thisWplant * 0.98
				runoffRatio = runoffRatio * 0.98
				effectiveIrrigationRatio = max(effectiveIrrigationRatio * 1.005, 1)
				rechScalar = rechScalar * 1.1
			} else {tooLow = FALSE}
				
			if(numRuns == 10)	{ tooLow = FALSE; tooHigh = FALSE}

		}

		hydroBasins$currentPrecip_avg[thisRow] = mean(climateData[thisRow, 1:2, 13, , 1])
		hydroBasins$currentPrecip_var[thisRow] = mean((climateData[thisRow, 1:2, 19, , 1] - climateData[thisRow, 1:2, 15, , 1]) / 2)
		if(cor.test(1:numDecades, climateData[thisRow, , 13, 1, 1], method="spearman")$p.value < 0.1) {hydroBasins$trendLow_Precip_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 1, 1])$slope}
		if(cor.test(1:numDecades, climateData[thisRow, , 13, 2, 1], method="spearman")$p.value < 0.1) {hydroBasins$trendMed_Precip_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 2, 1])$slope}
		if(cor.test(1:numDecades, climateData[thisRow, , 13, 3, 1], method="spearman")$p.value < 0.1) {hydroBasins$trendHigh_Precip_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 3, 1])$slope}
		if(cor.test(1:numDecades, climateData[thisRow, , 19, 1, 1] - climateData[thisRow, , 15, 1, 1], method="spearman")$p.value < 0.1) {
			hydroBasins$trendLow_Precip_var[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 19, 1, 1] - climateData[thisRow, , 15, 1, 1])$slope
		}
		if(cor.test(1:numDecades, climateData[thisRow, , 19, 2, 1] - climateData[thisRow, , 15, 2, 1], method="spearman")$p.value < 0.1) {
			hydroBasins$trendMed_Precip_var[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 19, 2, 1] - climateData[thisRow, , 15, 2, 1])$slope
		}
		if(cor.test(1:numDecades, climateData[thisRow, , 19, 3, 1] - climateData[thisRow, , 15, 3, 1], method="spearman")$p.value < 0.1) {
			hydroBasins$trendMed_Precip_var[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 19, 3, 1] - climateData[thisRow, , 15, 3, 1])$slope
		}

		hydroBasins$currentPET_avg[thisRow] = mean(climateData[thisRow, 1:2, 13, , 2])
		hydroBasins$currentPET_var[thisRow] = mean((climateData[thisRow, 1:2, 19, , 2] - climateData[thisRow, 1:2, 15, , 2]) / 2)
		if(cor.test(1:numDecades, climateData[thisRow, , 13, 1, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendLow_PET_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 1, 2])$slope}
		if(cor.test(1:numDecades, climateData[thisRow, , 13, 2, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendMed_PET_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 2, 2])$slope}
		if(cor.test(1:numDecades, climateData[thisRow, , 13, 3, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendHigh_PET_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 3, 2])$slope}

		hydroBasins$currentStreamflow_avg[thisRow] = mean(climateData[thisRow, 1:2, 13, , 4])
		hydroBasins$currentStreamflow_var[thisRow] = mean((climateData[thisRow, 1:2, 19, , 4] - climateData[thisRow, 1:2, 15, , 4]) / 2)
		if(cor.test(1:numDecades, climateData[thisRow, , 13, 1, 4], method="spearman")$p.value < 0.1) {hydroBasins$trendLow_Streamflow_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 1, 4])$slope}
		if(cor.test(1:numDecades, climateData[thisRow, , 13, 2, 4], method="spearman")$p.value < 0.1) {hydroBasins$trendMed_Streamflow_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 2, 4])$slope}
		if(cor.test(1:numDecades, climateData[thisRow, , 13, 3, 4], method="spearman")$p.value < 0.1) {hydroBasins$trendHigh_Streamflow_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 3, 4])$slope}

		hydroBasins$currentAridityIndex_A[thisRow] = 	mean(indexValuesArray[thisRow, 1:2, 8, , 1, 1]) * humidAI
		hydroBasins$currentDeficit_A[thisRow] = 		mean(indexValuesArray[thisRow, 1:2, 8, , 2, 2])
		hydroBasins$currentRatio_A[thisRow] = 			mean(indexValuesArray[thisRow, 1:2, 8, , 2, 1])
		if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 1, 2, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendLow_Deficit_A[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 1, 2, 2])$slope}
		if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 2, 2, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendMed_Deficit_A[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 2, 2, 2])$slope}
		if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 3, 2, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendHigh_Deficit_A[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 3, 2, 2])$slope}
		hydroBasins$currentAridityIndex_B[thisRow] =	mean(indexValuesArray[thisRow, 1:2, 8, , 3, 1]) * humidAI
		hydroBasins$currentDeficit_B[thisRow] = 		mean(indexValuesArray[thisRow, 1:2, 8, , 4, 2])
		hydroBasins$currentRatio_B[thisRow] = 			mean(indexValuesArray[thisRow, 1:2, 8, , 4, 1])
		if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 1, 4, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendLow_Deficit_B[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 1, 4, 2])$slope}
		if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 2, 4, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendMed_Deficit_B[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 2, 4, 2])$slope}
		if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 3, 4, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendHigh_Deficit_B[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 3, 4, 2])$slope}
		hydroBasins$currentDeficit_C[thisRow] = 		mean(indexValuesArray[thisRow, 1:2, 8, , 6, 2])
		hydroBasins$currentRatio_C[thisRow] = 			mean(indexValuesArray[thisRow, 1:2, 8, , 6, 1])
		if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 1, 6, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendLow_Deficit_C[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 1, 6, 2])$slope}
		if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 2, 6, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendMed_Deficit_C[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 2, 6, 2])$slope}
		if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 3, 6, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendHigh_Deficit_C[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 3, 6, 2])$slope}
		hydroBasins$currentDeficit_D[thisRow] = 		mean(indexValuesArray[thisRow, 1:2, 8, , 8, 2])
		hydroBasins$currentRatio_D[thisRow] = 			mean(indexValuesArray[thisRow, 1:2, 8, , 8, 2])
		if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 1, 8, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendLow_Deficit_D[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 1, 8, 2])$slope}
		if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 2, 8, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendMed_Deficit_D[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 2, 8, 2])$slope}
		if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 3, 8, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendHigh_Deficit_D[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 3, 8, 2])$slope}
		hydroBasins$recentHistoricSlope[thisRow] = recentHistoricTrend
		hydroBasins$rescaledRecentHistoricSlope[thisRow] = recentHistoricTrend / max(thisFrcCultAreaWthIrr, .0125)
		hydroBasins$streamflowRechargeScalar[thisRow] = streamflowRechargeScalar
		hydroBasins$thisFrcAreaUnderCult[thisRow] = thisFrcAreaUnderCult
		hydroBasins$thisFrcCultAreaWthIrr[thisRow] =thisFrcCultAreaWthIrr
		hydroBasins$initialSoilMoisture[thisRow] = initialSoilMoisture
		hydroBasins$thisWplant[thisRow] = thisWplant
		hydroBasins$runoffRatio[thisRow] = runoffRatio
		hydroBasins$effectiveIrrigationRatio[thisRow] = effectiveIrrigationRatio
		hydroBasins$rechScalar[thisRow] = rechScalar

	}
	saveRDS(indexValuesArray, paste0(customerFolder_input, clientName_input, '\\',  clientName_input, "_waterIndex.rds"))
	data.table::fwrite(calibOuts, paste0(customerFolder_input, clientName_input, '\\',  clientName_input, "_calibrationRoutine_v2.csv"))
	data.table::fwrite(hydroBasins, paste0(customerFolder_input, clientName_input, '\\',  clientName_input, '_hydroBasins_wIndex.csv'))
}








	
##########################################################
# 3D- water index plots
waterIndexPlotter_f = function(	
	customerTable_input = customerTable,
	clientName_input = clientName,
	scenarioNames = c('LowEmissions', 'MedEmissions', 'HighEmissions'),
	customerFolder_input = customerFolder,
	historicData = FALSE,
#	locationHeader = 'Location (name)',
#	climVars = c('Precipitation_decadalRawVals', 'PotentialEvapotranspiration_decadalRawVals', 'GroundwaterRecharge_decadalRawVals', 'Streamflow_decadalRawVals',	'RootZoneSoilMoisture_decadalRawVals', 'TotalWaterStorage_decadalRawVals'),
	baseNm = "waterIndexUnderlyingData_",
	ncFileLoc = 'J:\\Cai_data\\WaterIndex\\',
	indexValues = c('Aridity Index - Avg', 'Plant Water Demand - Avg', 'Aridity Index - Drought', 'Plant Water Demand - Drought', 'Aridity Index w/ Irrigation - Avg', 'Plant Water Demand w/ Irrigation - Avg', 'Aridity Index w/ Irrigation - Drought', 'Plant Water Demand w/ Irrigation - Drought'),
	doPlot = FALSE
	)
	{

	# input array is in format [location, decade, indexValueQuant, scenario, indexValue, indexValueClass]
	climVars = c(paste0(baseNm, "precip"), paste0(baseNm, "potevap"), paste0(baseNm, "qr"), paste0(baseNm, "dis"),	paste0(baseNm, "rootmoist"), paste0(baseNm, "tws"))
	ncName = climVars[1]
	nc_file =  nc_open(paste0(ncFileLoc, ncName, '.nc'))
	nc_decade = ncvar_get(nc_file, 'decade')
		# !!!!!!!!! temp fix: pre 2010s data can be problematic for some variables (sharp demarcation between 1980-2006 and 2006-2099)
	if(!historicData)	{nc_decade = nc_decade[-c(1:3)]}
		# !!!!!!!!! end temp fix: pre 2010s data can be problematic for some variables
	nc_close(nc_file)
		# initializing data for plotting
	waterIndexDataPlot = readRDS(paste0(customerFolder, clientName_input, '\\',  clientName_input, "_waterIndex.rds"))

	#if(any(waterIndexDataPlot < 0))	{waterIndexDataPlot[waterIndexDataPlot < 0] = 0}
		# identifying which indexValues to actually plot
	#indexValuesToPlot = c(1,3,4,6,7,8,9,10)
	indexValuesToPlot = c(2,4,6,8)

	for(thisLoc in 1:nrow(customerTable_input))	{
		if(doPlot){
			for(thisScen in 1:length(scenarioNames))	{
				
					# water index ratios
		#		png(paste0(customerFolder_input, clientName_input, '\\',  customerTable_input[thisLoc, ..locationHeader], '_', scenarioNames[thisScen], "_waterIndexRatio.png"), width=1100, height=1200)
		#		par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(4,2), font.lab=1.6, bty='l', cex.lab=2.0*1.8, cex.axis=2.0*1.4, cex.main=2.0*1.8, col='#1A232F')
				png(paste0(customerFolder_input, clientName_input, '\\',  customerTable_input[thisLoc, "Location_Name"], '_', scenarioNames[thisScen], "_waterIndexRatio.png"), width=1100, height=800)
				par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(2,2), font.lab=1.6, bty='l', cex.lab=1.5*1.8, cex.axis=2.0*1.4, cex.main=1.5*1.8, col='#1A232F')
				windowsFonts(A = windowsFont("Roboto"))
				for(thisIndexVal in indexValuesToPlot)	{
						# waterIndexDataPlot is in format [location, decade, indexValueQuant, scenario, indexValue, indexValueClass]
					plotRange = c(min(min(waterIndexDataPlot[thisLoc, , , , thisIndexVal, 1])*1.025, 0.9),  max(max(waterIndexDataPlot[thisLoc, , , , thisIndexVal, 1])*1.025, 1.1))
					plot(nc_decade, waterIndexDataPlot[thisLoc, , 8, thisScen, thisIndexVal, 1],  
						ylim = plotRange, #log='y',
						type='l', lwd=1, col='white', xaxt = 'n', #log='y',
						main='', ylab='', xlab='',
						col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
						family='A')
					abline(h=1, lwd=2, lty=1, col='#1A232F')
					abline(h=mean(waterIndexDataPlot[thisLoc, 1:2, 8, 1:3, thisIndexVal, 1]), lwd=2, lty =2, col='#1A232F')
					axis(1, at = nc_decade ,col.lab='#1A232F', col.axis='#666D74', 
						labels = nc_decade)
				#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
					polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 1, thisScen, thisIndexVal, 1], rev(waterIndexDataPlot[thisLoc, , 7, thisScen, thisIndexVal, 1])),
						col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
					polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 2, thisScen, thisIndexVal, 1], rev(waterIndexDataPlot[thisLoc, , 6, thisScen, thisIndexVal, 1])),
						col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
					polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 3, thisScen, thisIndexVal, 1], rev(waterIndexDataPlot[thisLoc, , 5, thisScen, thisIndexVal, 1])),
						col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
					loessSmooth = loess(waterIndexDataPlot[thisLoc, , 8, thisScen, thisIndexVal, 1] ~ nc_decade)
		#			lines(nc_decade, waterIndexDataPlot[thisLoc, , 17, thisScen, thisIndexVal], 
		#				col='#54575a', lwd=5)	#4cbfad
					lines(nc_decade, predict(loessSmooth),
						col='#EE6222', lwd=3)
					text(nc_decade[1], plotRange[1],  paste0(indexValues[thisIndexVal], ' (-)'), adj = c(0,0), cex=2.05)
		#			lines(nc_decade, nc_testDat[thisLon, thisLat, , 1, 1], 
		#				col='#4cbfad', lwd=3) #015f6f
				}
				dev.off()

					# water index deficits
		#		png(paste0(customerFolder_input, clientName_input, '\\', customerTable_input[thisLoc, ..locationHeader], '_', scenarioNames[thisScen], "_waterIndexDeficit.png"), width=1100, height=1200)
		#		par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(4,2), font.lab=1.6, bty='l', cex.lab=2.0*1.8, cex.axis=2.0*1.4, cex.main=2.0*1.8, col='#1A232F')
				png(paste0(customerFolder_input, clientName_input, '\\',  customerTable_input[thisLoc, "Location_Name"], '_', scenarioNames[thisScen], "_waterIndexDeficit.png"), width=1100, height=800)
				par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(2,2), font.lab=1.6, bty='l', cex.lab=1.5*1.8, cex.axis=2.0*1.4, cex.main=1.5*1.8, col='#1A232F')
				windowsFonts(A = windowsFont("Roboto"))
				for(thisIndexVal in indexValuesToPlot)	{
						# waterIndexDataPlot is in format [location, decade, indexValueQuant, scenario, indexValue, indexValueClass]
					plotRange = c(min(min(waterIndexDataPlot[thisLoc, , , , thisIndexVal, 2]), 0), max(max(waterIndexDataPlot[thisLoc, , , , thisIndexVal, 2]), 0))
					plot(nc_decade, waterIndexDataPlot[thisLoc, , 8, thisScen, thisIndexVal, 2],
						ylim = plotRange, #log='y',
						type='l', lwd=1, col='white', xaxt = 'n',
						main='', ylab='', xlab='',
						col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
						family='A')
					abline(h=0, lwd=2, lty=1, col='#1A232F')
					abline(h=mean(waterIndexDataPlot[thisLoc, 1:2, 8, 1:3, thisIndexVal, 2]), lwd=2, lty =2, col='#1A232F')
					axis(1, at = nc_decade ,col.lab='#1A232F', col.axis='#666D74', 
						labels = nc_decade)
				#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
					polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 1, thisScen, thisIndexVal, 2], rev(waterIndexDataPlot[thisLoc, , 7, thisScen, thisIndexVal, 2])),
						col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
					polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 2, thisScen, thisIndexVal, 2], rev(waterIndexDataPlot[thisLoc, , 6, thisScen, thisIndexVal, 2])),
						col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
					polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 3, thisScen, thisIndexVal, 2], rev(waterIndexDataPlot[thisLoc, , 5, thisScen, thisIndexVal, 2])),
						col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
					loessSmooth = loess(waterIndexDataPlot[thisLoc, , 8, thisScen, thisIndexVal, 2] ~ nc_decade)
		#			lines(nc_decade, waterIndexDataPlot[thisLoc, , 17, thisScen, thisIndexVal], 
		#				col='#54575a', lwd=5)	#4cbfad
					lines(nc_decade, predict(loessSmooth),
						col='#EE6222', lwd=3)
					text(nc_decade[1], min(plotRange), paste0(indexValues[thisIndexVal], ' (mm)'), adj = c(0,0), cex=2.05)
		#			lines(nc_decade, nc_testDat[thisLon, thisLat, , 1, 1], 
		#				col='#4cbfad', lwd=3) #015f6f
				}
				dev.off()
			}

		}


		summaryRatioOutput = as.data.frame(waterIndexDataPlot[thisLoc, , 8, , 6, 1])
#		for(thisScen in 1:3)	{
#			summaryRatioOutput[,thisScen] = ksmooth(1:nrow(summaryRatioOutput), summaryRatioOutput[, thisScen], kernel = 'normal', bandwidth = 5, n.points = nrow(summaryRatioOutput))$y
#		}
		fwrite(summaryRatioOutput, paste0(customerFolder_input, clientName_input, '\\',  customerTable_input[thisLoc, "Location_Name"], '_', "_waterIndRatioSummary.csv"))
	}
}




	# input array is in format [location, decade, indexValueQuant, scenario, indexValue, indexValueClass]

startTime = Sys.time()
gracePlotter_f()
climateDataSelection_f()
climateDataPlotting_f()
waterIndexCalculations_f()
waterIndexPlotter_f(
	doPlot = TRUE
	)
endTime = Sys.time()
endTime - startTime

library(ggplot2)
library(viridis)
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")

hydroBasins = data.table::fread(paste0(customerFolder, clientName, '\\',  clientName, '_hydroBasins_wIndex.csv'))
test = sf::st_read("J:\\Cai_data\\BasinATLAS_Data_v10\\BasinATLAS_v10\\BasinATLAS_v10_lev12.shp")[,1]

myBasins = merge(test, hydroBasins, all.y=TRUE, by="HYBAS_ID")

myBasins_rejigger = myBasins	; myBasins_rejigger$currentRatio[myBasins$currentRatio > 2] = 2

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
lmt = 2
variableOfInt = "currentRatio"
myTitle ='water risk index sample'
latlonBox_local = c(38,42.5,-118,-124.5)
ggplot(data = myBasins) +
	geom_sf(data = myBasins_rejigger, size=6.4, shape=15, aes(fill=currentRatio, color=currentRatio), color ='grey25', linewidth = 0.0001) +
	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
             n.breaks=7, limits=c(0,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'rocket')+#, trans = scales::pseudo_log_trans(sigma = 0.9))	+#
#	scale_fill_gradient2(low = 'darkred', high = 'skyblue', mid = 'white', trans = scales::pseudo_log_trans(sigma = .5), limits = c(0,lmt))	+
#	scale_color_gradient2(low = 'darkred', high = 'skyblue', mid = 'white', trans = scales::pseudo_log_trans(sigma = .5), limits = c(0,lmt))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, variableOfInt, ".png"), width = 20, height = 8)

myBasins_rejigger$currentDeficit[myBasins$currentDeficit > 1000] = 1000

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
lmt = 1000
variableOfInt = "currentSurplus"
myTitle ='water risk index sample'
latlonBox_local = c(38,42.5,-118,-124.5)
ggplot(data = myBasins) +
	geom_sf(data = myBasins_rejigger, size=6.4, shape=15, aes(fill=currentDeficit, color=currentDeficit), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                             '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#			limits=c(-lmt,lmt),
#            n.breaks=7,  show.limits=T)  +
#	scale_fill_viridis(option = 'rocket')+#, trans = scales::pseudo_log_trans(sigma = 0.9))	+#
	scale_fill_gradient2(low = 'darkred', high = 'skyblue', mid = 'white', trans = scales::pseudo_log_trans(sigma = .5), limits = c(-lmt,lmt))	+
#	scale_color_gradient2(low = 'darkred', high = 'skyblue', mid = 'white', trans = scales::pseudo_log_trans(sigma = .5), limits = c(0,lmt))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder,  "_", variableOfInt, ".png"), width = 20, height = 8)






























































###################################################################
# 3C- calculating and generating basic plots for the water indes
waterIndexCalculations_f = function(	
	customerTable_input = customerTable,
	petGlobAvg = 2000,
	growSeason = 1:365, # for citrus, to be used as generic plant
	wPlant = 1050,# for citrus, to be used as generic plant
	runoffRatio = 0.08,
	humidAI = 0.65,
	pwpSoil = 22, 
	divertibleStrmfl = 0.25,
	strmflCaptureRatio =0.1,
	clientName_input = clientName,
	customerFolder_input = customerFolder,
	cropPhenologyTable = "J:\\Cai_data\\Rabo\\Locations\\AutomatedWRI_test\\cropWaterNeedsLookupTable.csv",
	indexValues = c('Aridity Index - Avg', 'Soil Moisture Stress - Avg', 'Plant Water Demand - Avg', 'Aridity Index - Drought', 'Soil Moisture Stress - Drought', 'Plant Water Demand - Drought', 'Aridity Index w/ Irrigation - Avg', 'Plant Water Demand w/ Irrigation - Avg', 'Aridity Index w/ Irrigation - Drought', 'Plant Water Demand w/ Irrigation - Drought')
	)
	{

#hectToSqKm = 100
	halfDegInKM = 111.1 / 2
	kmToMm = 1000^2

		# array for holding outputs
	myMissingData = NA
		# input array is in format  [location, decade, valueClass,      scenario, climateVariable]
		# output array is in format [location, decade, indexValueQuant, scenario, indexValue, indexValueClass]
	indexValueClass = c('ratio', 'difference')
	indexValueQuant = c('Q05', 'Q15', 'Q25', 'Q50', 'Q75', 'Q85', 'Q95')
	climateData = readRDS(paste0(customerFolder_input, clientName_input, '\\',  clientName_input, '_rawValues.rds'))
	indexValuesArray = array(rep(myMissingData, nrow(customerTable) * length(climateData[1, ,1,1,1]) * length(indexValueQuant) * length(climateData[1,1,1, ,1]) * length(indexValues) * length(indexValueClass)), 
										dim = c(nrow(customerTable),  length(climateData[1, ,1,1,1]),  length(indexValueQuant),  length(climateData[1,1,1, ,1]),  length(indexValues), length(indexValueClass)))

		# reading in crop phenology table
	cropPhenology_df = data.table::fread(cropPhenologyTable)
		# reading in GRACE trends
	historicTrends = data.table::fread(paste0(customerFolder_input, clientName_input, '\\',  clientName_input, "_graceTrends.csv"))
		# readining hydroBasins data
	hydroBasins = data.table::fread(paste0(customerFolder, clientName, '\\',  clientName, '_hydroBasins.csv'))



	for(thisRow in 1:nrow(customerTable_input))	{

			# pulling in crop phenology
		if(customerTable_input$PrimaryCrop[thisRow] %in% cropPhenology_df$Crop)	{
			thisCrop = which(cropPhenology_df$Crop == customerTable_input$PrimaryCrop[thisRow])
		} else {thisCrop = 1}

			# identifying growing months for crop of interest
		if(all(c(!is.na(customerTable_input$GrowMonth_Start) & !is.na(customerTable_input$GrowMonth_End))))	{
			if(customerTable_input$GrowMonth_Start < customerTable_input$GrowMonth_End)	{
				growMonths = customerTable_input$GrowMonth_Start:customerTable_input$GrowMonth_End
			} else { growMonths = c(customerTable_input$GrowMonth_Start:12, 1:customerTable_input$GrowMonth_End)	}
			# randominzing months for equally cool / hot wet / dry seasons btw global north and south
		} else { growMonths = c(12,6,1,7,2,8,3,9,4,10,5,11)[1:round(mean(c(cropPhenology_df$growingPeriod_min_days[thisCrop], cropPhenology_df$growingPeriod_max_days[thisCrop])) / 30.4, 0)]}
		
		
		
			# initializing static values
		petGlobAvgForGrowSeason = petGlobAvg * (length(growSeason) / 12)
		thisFrcAreaUnderCult = ifelse(is.na(customerTable_input$Area_Irrigated[thisRow]), frcAreaUnderCult, customerTable_input$Area_Irrigated[thisRow])	# defining water needs by location
		thisWplant = ifelse(is.na(customerTable_input$Annual_Water_Needs[thisRow]), wPlant, customerTable_input$Annual_Water_Needs[thisRow])	# defining water needs by location
		thisDivertibleStrmfl = ifelse(is.na(customerTable_input$Divertible_Streamflow[thisRow]), divertibleStrmfl, customerTable_input$Divertible_Streamflow[thisRow]) # defining extractible water by location
		thisStrmflCaptureRatio = ifelse(is.na(customerTable_input$Streamflow_Capture[thisRow]), strmflCaptureRatio, customerTable_input$Streamflow_Capture[thisRow]) # defining extractible water by location

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
		gridArea = halfDegInKm * (halfDegInKM * cos(customerTable$Lat[thisRow] * pi / 180))
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
}










	
##########################################################
# 3D- water index plots
waterIndexPlotter_f = function(	
	customerTable_input = customerTable,
	clientName_input = clientName,
	scenarioNames = c('LowEmissions', 'MedEmissions', 'HighEmissions'),
	customerFolder_input = customerFolder,
	locationHeader = 'Location_Name',
#	climVars = c('Precipitation_decadalRawVals', 'PotentialEvapotranspiration_decadalRawVals', 'GroundwaterRecharge_decadalRawVals', 'Streamflow_decadalRawVals',	'RootZoneSoilMoisture_decadalRawVals', 'TotalWaterStorage_decadalRawVals'),
	baseNm = "waterIndexUnderlyingData_",
	climVars = c(paste0(baseNm, "precip"), paste0(baseNm, "potevap"), paste0(baseNm, "qr"), paste0(baseNm, "dis"),	paste0(baseNm, "rootmoist"), paste0(baseNm, "tws")),
	ncFileLoc = 'J:\\Cai_data\\WaterIndex\\',
	indexValues = c('Aridity Index - Avg', 'Soil Moisture Stress - Avg', 'Plant Water Demand - Avg', 'Aridity Index - Drought', 'Soil Moisture Stress - Drought', 'Plant Water Demand - Drought', 'Aridity Index w/ Irrigation - Avg', 'Plant Water Demand w/ Irrigation - Avg', 'Aridity Index w/ Irrigation - Drought', 'Plant Water Demand w/ Irrigation - Drought')
	)
	{

	ncName = climVars[1]
	nc_file =  nc_open(paste0(ncFileLoc, ncName, '.nc'))
	nc_decade = ncvar_get(nc_file, 'decade')
	nc_close(nc_file)
		# initializing data for plotting
	waterIndexDataPlot = readRDS(paste0(customerFolder, clientName_input, '\\',  clientName_input, "_waterIndex.rds"))

	#if(any(waterIndexDataPlot < 0))	{waterIndexDataPlot[waterIndexDataPlot < 0] = 0}
		# identifying which indexValues to actually plot
	#indexValuesToPlot = c(1,3,4,6,7,8,9,10)
	indexValuesToPlot = c(3,6,8,10)

	for(thisLoc in 1:nrow(customerTable_input))	{
		for(thisScen in 1:length(scenarioNames))	{
			
				# water index ratios
	#		png(paste0(customerFolder_input, clientName_input, '\\',  customerTable_input[thisLoc, ..locationHeader], '_', scenarioNames[thisScen], "_waterIndexRatio.png"), width=1100, height=1200)
	#		par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(4,2), font.lab=1.6, bty='l', cex.lab=2.0*1.8, cex.axis=2.0*1.4, cex.main=2.0*1.8, col='#1A232F')
			png(paste0(customerFolder_input, clientName_input, '\\',  customerTable_input[thisLoc, ..locationHeader], '_', scenarioNames[thisScen], "_waterIndexRatio.png"), width=1100, height=800)
			par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(2,2), font.lab=1.6, bty='l', cex.lab=1.5*1.8, cex.axis=2.0*1.4, cex.main=1.5*1.8, col='#1A232F')
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
				text(nc_decade[1], plotRange[1],  paste0(indexValues[thisIndexVal], ' (-)'), adj = c(0,0), cex=2.05)
	#			lines(nc_decade, nc_testDat[thisLon, thisLat, , 1, 1], 
	#				col='#4cbfad', lwd=3) #015f6f
			}
			dev.off()

				# water index deficits
	#		png(paste0(customerFolder_input, clientName_input, '\\', customerTable_input[thisLoc, ..locationHeader], '_', scenarioNames[thisScen], "_waterIndexDeficit.png"), width=1100, height=1200)
	#		par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(4,2), font.lab=1.6, bty='l', cex.lab=2.0*1.8, cex.axis=2.0*1.4, cex.main=2.0*1.8, col='#1A232F')
			png(paste0(customerFolder_input, clientName_input, '\\',  customerTable_input[thisLoc, ..locationHeader], '_', scenarioNames[thisScen], "_waterIndexDeficit.png"), width=1100, height=800)
			par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(2,2), font.lab=1.6, bty='l', cex.lab=1.5*1.8, cex.axis=2.0*1.4, cex.main=1.5*1.8, col='#1A232F')
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
				text(nc_decade[1], min(plotRange), paste0(indexValues[thisIndexVal], ' (mm)'), adj = c(0,0), cex=2.05)
	#			lines(nc_decade, nc_testDat[thisLon, thisLat, , 1, 1], 
	#				col='#4cbfad', lwd=3) #015f6f
			}
			dev.off()


		}
		
		columnNames = c(
			paste0("AridityIndex_A_", scenarioNames),
			paste0("WaterIndexRatio_A_", scenarioNames),
			paste0("WaterIndexDeficit_A_", scenarioNames),
			paste0("AridityIndex_B_", scenarioNames),
			paste0("WaterIndexRatio_B_", scenarioNames),
			paste0("WaterIndexDeficit_B_", scenarioNames),
			paste0("WaterIndexRatio_C_", scenarioNames),
			paste0("WaterIndexDeficit_C_", scenarioNames),
			paste0("WaterIndexRatio_D_", scenarioNames),
			paste0("WaterIndexDeficit_D_", scenarioNames))
#		scenarioNames
		
		summaryRatioOutput = as.data.frame(waterIndexDataPlot[thisLoc, , 4, , 8, 1])
#		for(thisScen in 1:3)	{
#			summaryRatioOutput[,thisScen] = ksmooth(1:nrow(summaryRatioOutput), summaryRatioOutput[, thisScen], kernel = 'normal', bandwidth = 5, n.points = nrow(summaryRatioOutput))$y
#		}
		fwrite(summaryRatioOutput, paste0(customerFolder_input, clientName_input, '\\',  customerTable_input[thisLoc, ..locationHeader], '_', "_waterIndRatioSummary.csv"))
	}
}


