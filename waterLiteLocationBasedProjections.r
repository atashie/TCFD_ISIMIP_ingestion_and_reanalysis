########################################################
### library(hydroGOF)		# for nse calculations
library(data.table)
#library(sf)
#	sf::sf_use_s2(FALSE) # for problem with intersecting spherical w flat
library(lubridate)
library(ncdf4)
library(magrittr)
library(robslopes)


#########################################
# reading in climai netcdf data
allLocCsv = fread('J:\\Cai_data\\TCFD\\locations\\Driscolls_locations_onboarding_v2.csv')	# Driscolls_locations_onboarding,	Advanta_locations_Toowoomba
locCsv = allLocCsv[complete.cases(allLocCsv),]
userName = 'Driscolls_v2'																	# Driscolls,						Advanta
includeIndex = TRUE

	# these vars are unlikely to change between runs
projYears = 2010:2069	# projections for all models (so far) start in 2006
histYears = 1980:2005	# matsiro hist data goes to 2005, watergap goes to 2015
normYears = 1980:2019	# which years should we normalize on
trendYears = 2010:2069	# which uears to compute trend over
trendImprtncPct = 0.02 	# what fractional change is important?
trendImprtncVol = 500 * trendImprtncPct # what change in mm is important?
theseModels = c("gfdl","hadgem2","ipsl","miroc5")	# this cannot be changed for now
dataOutput = paste0('J:\\Cai_data\\TCFD\\', userName)

hydroVariables = data.frame(varLoc = 		c('GWstorage', 		'RootZoneSoilMoisture', 'GWrecharge',	'SurfaceWater_Streamflow',	'Evapotranspiration',	'ActualTotalWaterUse',		'PotentialTotalWaterWithdrawal',	'TotalWaterStorage'),
							varFileName = 	c('groundwstor',	'rootmoist', 			'qr',		 	'dis', 						'evap',					'atotuse',					'ptotww',							'tws'),
							model = 		c('watergap2-2c',	'clm45',				'watergap2-2c',	'matsiro',					'clm45',				'watergap2-2c',				'watergap2-2c',						'clm45'),
							commonName = 	c('Groundwater (mm)','Soil Moisture (mm)',	'Recharge (mm)','Streamflow (%)',			'ET Demand (mm)',		'Total Water Use (mm)', 	'Total Water Withdrawal (mm)',		'Total Storage (mm)'))	
							

 
 
for(jj in 1:nrow(hydroVariables))	{
	ncPath = paste0("J:\\Cai_data\\TCFD\\", hydroVariables$varLoc[jj], "\\")		# where are the relevant data stored?								GWstorage,        RootZoneSoilMoisture, GWrecharge   , SurfaceWater_Streamflow,	Evapotranspiration
	ncVarFileName = hydroVariables$varFileName[jj]									# what is the var name in the .nc? 									groundwstor,      rootmoist           , qr           , dis
	ncModel = hydroVariables$model[jj]											# what is the name of the model used to derive the var of interest?	    watergap2-2c,     clm45,              , watergap2-2c , matsiro,					clm45
	commonVarName = hydroVariables$commonName[jj]							# user friendly name (with units) of the var of interest					Groundwater (mm), Soil Moisture (mm)  , Recharge (mm), Streamflow (%),			ET (mm)
	convertToPct = ifelse(ncVarFileName == 'dis', TRUE, FALSE)	# do we convert to a % or keep in orig units
	print(jj)
	for(i in 1:nrow(locCsv))	{
		print(i)
		locName = locCsv$Location[i]
		locLat = locCsv$Lat[i]
		locLon = locCsv$Lon[i]
		for(rcpScen in c('rcp26', 'rcp60'))	{
			waterTrends_f(ncPath = ncPath,
				ncModel = ncModel,
				ncVarFileName = ncVarFileName,
				commonVarName = commonVarName,
				repScen = repScen,
				projYears = projYears,
				histYears = histYears,
				normYears = normYears,
				trendYears = trendYears, 
				theseModels = theseModels,
				locName = locName,
				locLat = locLat,
				locLon = locLon,
				userName = userName, 
				dataOutput = dataOutput)
		}
	}	
}


	# for combining all data for one user
combiningData_f(dataOutput = dataOutput)


#####################################################
## normalizing over 2006-2025
#####################################################

###################################################################
## defining functions
waterTrends_f = function(ncPath = ncPath,
	ncModel = ncModel,
	ncVarFileName = ncVarFileName,
	commonVarName = commonVarName,
	repScen = repScen,
	projYears = projYears,
	histYears = histYears,
	normYears = normYears,
	trendYears = trendYears, 
	theseModels = theseModels,
	locName = locName,
	locLat = locLat,
	locLon = locLon,
	userName = userName, 
	dataOutput = dataOutput)
{

	if(!file.exists(paste0(dataOutput)))	{
		dir.create(file.path(paste0(dataOutput)))
	}
	
	finalMonthlyUnits = ifelse(convertToPct, "pct per decade", "monthly mm per decade")
	finalAnnualUnits = ifelse(convertToPct, "pct per decade", "annual mm per decade")

	scalar = 1
	#recharge and evap are in in mean mm / s, so convert to mm / month
	if(ncVarFileName %in% c('qr', 'evap'))	{
		scalar = 60 * 60 * 24 * 30.4375
	}
	#streamflow is in mean m^3 / s, so convert to mm / month
	#if(ncVarFileName == 'dis')	{
	#	scalar = 60 * 60 * 24 * 30.4375 * 1000 / (1000 * 1000)
	#}

		# reading ssp126 climate data 
	ncname = paste0(ncModel, '_gfdl-esm2m_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_2005co2_thawdepth_global_annual_2006_2099.nc4"  
		# note that watergap treats gw as a relative value (depth below or above a relative datum) so the actual value can go negative (and theoretically can go to any real negative number)
	ncinProj = nc_open(paste0(ncPath, ncname))

		#creating a database for storing all initial data
	nc_lat = ncvar_get(ncinProj, 'lat')#rev(ncvar_get(ncin, 'lat'))	# lat is given from high to low
	nc_lon = ncvar_get(ncinProj, 'lon')
	this_lat = which.min(abs(nc_lat - locLat))
	this_lon = which.min(abs(nc_lon - locLon))
		# ensuring we aren't drawing from a coastal water cell
	if(is.na(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,1]))	{
		this_lat = this_lat + 1
		if(is.na(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,1]))	{
			this_lat = this_lat - 2
			if(is.na(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,1]))	{
				this_lat = this_lat + 1
				this_lon = this_lon + 1
				if(is.na(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,1]))	{
					this_lon = this_lon - 2
				}
			}
		}
	}
	
		# identifying the years to use for projected data
	nc_dateProj = as.Date("1661-01-01") + 10 + ncvar_get(ncinProj, 'time') * 30.4375# time is days after jan 1 1661
	nc_yearsProj = year(nc_dateProj)
	lastYearProj = last(which(nc_yearsProj == last(projYears)))
	firstYearProj = which(nc_yearsProj == projYears[1])[1]
	
	
		# initializing the data table
	nc_varOut = data.table(year = rep(projYears, each=12))


	###############################################
	# reading in data
	nc_varOut$gfdl = ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj]	* scalar# lon, lat, time
	nc_close(ncinProj)

	ncname = paste0(ncModel, '_hadgem2-es_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	
	ncinProj = nc_open(paste0(ncPath, ncname))
	nc_varOut$hadgem2 = ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj])	* scalar	# lon, lat, time
	nc_close(ncinProj)

	ncname = paste0(ncModel, '_ipsl-cm5a-lr_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	
	ncinProj = nc_open(paste0(ncPath, ncname))
	nc_varOut$ipsl = ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj])	* scalar	# lon, lat, time
	nc_close(ncinProj)

	ncname = paste0(ncModel, '_miroc5_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	
	ncinProj = nc_open(paste0(ncPath, ncname))
	nc_varOut$miroc5 = ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj])	* scalar # lon, lat, time
	nc_close(ncinProj)

	names(nc_varOut) = c('year',theseModels)

	
	varOutYr_df = data.frame(year = rep(c(histYears, projYears), length(theseModels)), 
		minVal = NA, maxVal = NA, meanVal = NA, sumVal = NA, rngVal = NA, sdVal = NA)

	iter = 0
	for(thisModel in theseModels)	{
		thisCol = which(names(nc_varOut) == thisModel)
		modSubset = nc_varOut[ , c(1, ..thisCol)]
		for(thisYear in c(histYears, projYears))	{
			iter = iter + 1
			thisSubset = subset(modSubset, year == thisYear)[ , -1]
			varOutYr_df[iter, -1] = c(
				min(thisSubset),
				max(thisSubset),
				mean(unlist(thisSubset)),
				sum(thisSubset),			
				diff(range(thisSubset)),
				sd(unlist(thisSubset)))
		}	
	}
	varOutYr_df$decade = varOutYr_df$year - varOutYr_df$year %% 10
	varOutYrAbs_df = varOutYr_df
	
		# cor.test can't take 0 vals, so adding a small random value 
	varOutYr_df[varOutYr_df == 0] =  runif(length(which(varOutYr_df == 0)), 0, 0.001)
	
	if(convertToPct)	{
			# normalizing to pct
		varOutYr_df$minVal = 100 * (varOutYr_df$minVal / median(varOutYr_df$minVal[varOutYr_df$year %in% normYears])) - 100
		varOutYr_df$maxVal = 100 * (varOutYr_df$maxVal / median(varOutYr_df$maxVal[varOutYr_df$year %in% normYears])) - 100
		varOutYr_df$meanVal = 100 * (varOutYr_df$meanVal / median(varOutYr_df$meanVal[varOutYr_df$year %in% normYears])) - 100
		varOutYr_df$sumVal = 100 * (varOutYr_df$sumVal / median(varOutYr_df$sumVal[varOutYr_df$year %in% normYears])) - 100
		varOutYr_df$rngVal = 100 * (varOutYr_df$rngVal / median(varOutYr_df$rngVal[varOutYr_df$year %in% normYears])) - 100
		varOutYr_df$sdVal = 100 * (varOutYr_df$sdVal / median(varOutYr_df$sdVal[varOutYr_df$year %in% normYears])) - 100
	} else	{
			# normalizing by median
		varOutYr_df$minVal = varOutYr_df$minVal - median(varOutYr_df$minVal[varOutYr_df$year %in% normYears])
		varOutYr_df$maxVal = varOutYr_df$maxVal - median(varOutYr_df$maxVal[varOutYr_df$year %in% normYears])
		varOutYr_df$meanVal = varOutYr_df$meanVal - median(varOutYr_df$meanVal[varOutYr_df$year %in% normYears])
		varOutYr_df$sumVal = varOutYr_df$sumVal - median(varOutYr_df$sumVal[varOutYr_df$year %in% normYears])
		varOutYr_df$rngVal = varOutYr_df$rngVal - median(varOutYr_df$rngVal[varOutYr_df$year %in% normYears])
		varOutYr_df$sdVal = varOutYr_df$sdVal - median(varOutYr_df$sdVal[varOutYr_df$year %in% normYears])
	}
	whichTrendYears = which(varOutYr_df$year %in% trendYears)

	cor.test(varOutYr_df$year[whichTrendYears], varOutYr_df$meanVal[whichTrendYears], method = 'kendall')$p.value
	cor.test(varOutYr_df$year[whichTrendYears], varOutYr_df$meanVal[whichTrendYears], method = 'kendall')$estimate

	varOutYr_df_sub = varOutYr_df[whichTrendYears, ]
	
	numBoxes = length(trendYears) / 10

	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_minVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df_sub$minVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(minVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Min Monthly ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
		labels = paste0(unique(varOutYr_df_sub$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", trendYears[1], ' to ', last(trendYears)), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		text(x=.08*numBoxes, y=maxLim * 0.9, paste0("Values Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$minVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$minVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$minVal)$slope), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else	{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()



	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_maxVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df_sub$maxVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(maxVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Max Monthly ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
		labels = paste0(unique(varOutYr_df_sub$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", trendYears[1], ' to ', last(trendYears)), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		text(x=.08*numBoxes, y=maxLim * 0.9, paste0("Values Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$maxVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$maxVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$maxVal)$slope), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else	{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()



	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_meanVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df_sub$meanVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(meanVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Avg Monthly ' , commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
		labels = paste0(unique(varOutYr_df_sub$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", trendYears[1], ' to ', last(trendYears)), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		text(x=.08*numBoxes, y=maxLim * 0.9, paste0("Values Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$meanVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$meanVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$meanVal)$slope), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()



	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_sumVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df_sub$sumVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(sumVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Total Annual (sum) ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
		labels = paste0(unique(varOutYr_df_sub$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", trendYears[1], ' to ', last(trendYears)), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		text(x=.08*numBoxes, y=maxLim * 0.9, paste0("Values Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sumVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sumVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$sumVal)$slope), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()

	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_rngVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df_sub$rngVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(rngVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual Range in ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
		labels = paste0(unique(varOutYr_df_sub$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", trendYears[1], ' to ', last(trendYears)), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		text(x=.08*numBoxes, y=maxLim * 0.9, paste0("Values Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$rngVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$rngVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$rngVal)$slope), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()




	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_sdVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df_sub$sdVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(sdVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual St. Dev. in ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
		labels = paste0(unique(varOutYr_df_sub$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", trendYears[1], ' to ', last(trendYears)), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		text(x=.08*numBoxes, y=maxLim * 0.9, paste0("Values Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sdVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sdVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$sdVal)$slope), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()

	lookerDF = data.frame(User = character(), Lat = numeric(), Lon = numeric(), Scenario = character(), Decade = numeric(), Variable = character(), Metric = character(),
		Significance = numeric(), Trend_Strength = numeric(), Trend_Units = character(),
		Q05 = numeric(), Q25 = numeric(), Q50 = numeric(), Q75 = numeric(), Q95 = numeric(),
		Location = character()) 
	
	theseDecades = seq(trendYears[1], by = 10, length.out = length(trendYears) / 10)
	for(thisDecade in theseDecades)	{
		subsetYrs = subset(varOutYr_df_sub, decade == thisDecade)
		subsetYrsAbs = subset(varOutYrAbs_df, decade == thisDecade)
		
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Minimum Monthly', 
			signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$minVal, method = 'kendall')$p.value, 3), signif(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$minVal)$slope, 2) * 10, finalMonthlyUnits,
			signif(quantile(subsetYrs$minVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$minVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			locName))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Maximum Monthly', 
			signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$maxVal, method = 'kendall')$p.value, 3), signif(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$maxVal)$slope, 2) * 10, finalMonthlyUnits,
			signif(quantile(subsetYrs$maxVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$maxVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			locName))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Average Monthly', 
			signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$meanVal, method = 'kendall')$p.value, 3), signif(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$meanVal)$slope, 2) * 10, finalMonthlyUnits,
			signif(quantile(subsetYrs$meanVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$meanVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			locName))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Total Annual', 
			signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sumVal, method = 'kendall')$p.value, 3), signif(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$sumVal)$slope, 2) * 10, finalAnnualUnits,
			signif(quantile(subsetYrs$sumVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$sumVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			locName))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Range', 
			signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$rngVal, method = 'kendall')$p.value, 3), signif(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$rngVal)$slope, 2) * 10, finalAnnualUnits,
			signif(quantile(subsetYrs$rngVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$rngVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			locName))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual St. Dev.', 
			signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sdVal, method = 'kendall')$p.value, 3), signif(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$sdVal)$slope, 2) * 10, finalAnnualUnits,
			signif(quantile(subsetYrs$sdVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$sdVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			locName))
	}
	names(lookerDF) = c('User', 'Lat', 'Lon', 'Scenario', 'Decade', 'Variable', 'Metric', 'Significance', 'Trend_Strength', 'Trend_Units',
		'Q05_rel', 'Q25_rel', 'Q50_rel', 'Q75_rel', 'Q95_rel',
		'Q05_abs', 'Q25_abs', 'Q50_abs', 'Q75_abs', 'Q95_abs',
		'Location')

	if(!file.exists(paste0(dataOutput, '\\outputsForLooker')))	{
		dir.create(file.path(paste0(dataOutput, '\\outputsForLooker')))
	}

	fwrite(lookerDF, paste0(dataOutput, '\\outputsForLooker\\', locName, '_', rcpScen, '_', ncVarFileName, '.csv'))
}



combiningData_f = function(dataOutput = dataOutput){
	allFiles = list.files(paste0(dataOutput, '\\outputsForLooker\\'))
	combFiles = fread(paste0(dataOutput, '\\outputsForLooker\\', allFiles[1]))
	for(thisFile in allFiles[-1])	{
		combFiles = rbind(combFiles, fread(paste0(dataOutput, '\\outputsForLooker\\', thisFile)))
	}
	fwrite(combFiles, paste0(dataOutput, '\\waterTrendsExposure_', userName, Sys.Date(), '.csv'))
}

































####################################################################
# normalizing with long-term historical data

###################################################################
## defining functions
waterTrends_f = function(ncPath = ncPath,
	ncModel = ncModel,
	ncVarFileName = ncVarFileName,
	commonVarName = commonVarName,
	repScen = repScen,
	projYears = projYears,
	histYears = histYears,
	normYears = normYears,
	trendYears = trendYears, 
	theseModels = theseModels,
	locName = locName,
	locLat = locLat,
	locLon = locLon,
	userName = userName, 
	dataOutput = dataOutput)
{

	if(!file.exists(paste0(dataOutput)))	{
		dir.create(file.path(paste0(dataOutput)))
	}
	
	finalMonthlyUnits = ifelse(convertToPct, "pct per decade", "monthly mm per decade")
	finalAnnualUnits = ifelse(convertToPct, "pct per decade", "annual mm per decade")

	scalar = 1
	#recharge and evap are in in mean mm / s, so convert to mm / month
	if(ncVarFileName %in% c('qr', 'evap'))	{
		scalar = 60 * 60 * 24 * 30.4375
	}
	#streamflow is in mean m^3 / s, so convert to mm / month
	#if(ncVarFileName == 'dis')	{
	#	scalar = 60 * 60 * 24 * 30.4375 * 1000 / (1000 * 1000)
	#}

		# reading ssp126 climate data 
	ncname = paste0(ncModel, '_gfdl-esm2m_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_2005co2_thawdepth_global_annual_2006_2099.nc4"  
		# note that watergap treats gw as a relative value (depth below or above a relative datum) so the actual value can go negative (and theoretically can go to any real negative number)
	ncinProj = nc_open(paste0(ncPath, ncname))

		#creating a database for storing all initial data
	nc_lat = ncvar_get(ncinProj, 'lat')#rev(ncvar_get(ncin, 'lat'))	# lat is given from high to low
	nc_lon = ncvar_get(ncinProj, 'lon')
	this_lat = which.min(abs(nc_lat - locLat))
	this_lon = which.min(abs(nc_lon - locLon))
		# ensuring we aren't drawing from a coastal water cell
	if(is.na(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,1]))	{
		this_lat = this_lat + 1
		if(is.na(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,1]))	{
			this_lat = this_lat - 2
			if(is.na(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,1]))	{
				this_lat = this_lat + 1
				this_lon = this_lon + 1
				if(is.na(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,1]))	{
					this_lon = this_lon - 2
				}
			}
		}
	}
	
		# identifying the years to use for projected data
	nc_dateProj = as.Date("1661-01-01") + 10 + ncvar_get(ncinProj, 'time') * 30.4375# time is days after jan 1 1661
	nc_yearsProj = year(nc_dateProj)
	lastYearProj = last(which(nc_yearsProj == last(projYears)))
	firstYearProj = which(nc_yearsProj == projYears[1])[1]
	
		# identifying the years to use for projected data
	if(ncModel == 'matsiro')	{
		ncname = paste0(ncModel, '_ipsl-cm5a-lr_ewembi_historical_histsoc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')
		ncinHist = nc_open(paste0(ncPath, ncname))
		nc_dateHist = as.Date("1661-01-01") + 10 + ncvar_get(ncinHist, 'time') * 30.4375	# time is days after jan 1 1661
	}
	if(ncModel == 'clm45')	{
		ncname = paste0(ncModel, '_ipsl-cm5a-lr_ewembi_historical_2005soc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')
		ncinHist = nc_open(paste0(ncPath, ncname))
		nc_dateHist = as.Date("1661-01-01") + 10 + ncvar_get(ncinHist, 'time') * 30.4375	# time is days after jan 1 1661
	}
	if(ncModel == 'watergap2-2c')	{
		ncname = paste0(ncModel, '_gswp3-w5e5_nobc_hist_varsoc_co2_', ncVarFileName, '_global_monthly_1901_2016.nc4')
		ncinHist = nc_open(paste0(ncPath, ncname))
		nc_dateHist = as.Date("1901-01-01") + 10 + ncvar_get(ncinHist, 'time') * 30.4375	# time is days after jan 1 1901
	}
	nc_yearsHist = year(nc_dateHist)
	lastYearHist = last(which(nc_yearsHist == last(histYears)))
	firstYearHist = which(nc_yearsHist == histYears[1])[1]
	
		# initializing the data table
	nc_varOut = data.table(year = rep(c(histYears, projYears), each=12))


	###############################################
	# reading in and concatenating data
		# concatenating hist and proj data
	if(ncVarFileName == 'groundwstor')	{
		initGW = ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist]
		projGWdiff = cumsum(diff(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj]))
		nc_varOut$gfdl = c(initGW, last(initGW), last(initGW) + projGWdiff)	* scalar# lon, lat, time
	} else {
		nc_varOut$gfdl = c(ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist] , ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj])	* scalar# lon, lat, time
	}
	nc_close(ncinProj)
	nc_close(ncinHist)

		# alternative hist data 
	if(ncModel == 'matsiro')	{
		ncname = paste0(ncModel, '_hadgem2-es_ewembi_historical_histsoc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4') 
	}
	if(ncModel == 'clm45')	{
		ncname = paste0(ncModel, '_hadgem2-es_ewembi_historical_2005soc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4') 
	}
	if(ncModel == 'watergap2-2c')	{
		ncname = paste0(ncModel, '_gswp3-w5e5_nobc_hist_pressoc_co2_', ncVarFileName, '_global_monthly_1901_2016.nc4')
	}
	ncinHist = nc_open(paste0(ncPath, ncname))
		# concatenating
	ncname = paste0(ncModel, '_hadgem2-es_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	
	ncinProj = nc_open(paste0(ncPath, ncname))
	if(ncVarFileName == 'groundwstor')	{
		initGW = ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist]
		projGWdiff = cumsum(diff(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj]))
		nc_varOut$hadgem2 = c(initGW, last(initGW), last(initGW) + projGWdiff)	* scalar# lon, lat, time
	} else {
		nc_varOut$hadgem2 = c(ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist] , ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj])	* scalar	# lon, lat, time
	}
	nc_close(ncinProj)
	nc_close(ncinHist)
		# alternative hist data 
	if(ncModel == 'matsiro')	{
		ncname = paste0(ncModel, '_gfdl-esm2m_ewembi_historical_histsoc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')  
	}
	if(ncModel == 'clm45')	{
		ncname = paste0(ncModel, '_gfdl-esm2m_ewembi_historical_2005soc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')  
	}
	if(ncModel == 'watergap2-2c')	{
		ncname = paste0(ncModel, '_gswp3-ewembi_nobc_hist_varsoc_co2_', ncVarFileName, '_global_monthly_1901_2016.nc4')
	}
	ncinHist = nc_open(paste0(ncPath, ncname))
		# concatenating 
	ncname = paste0(ncModel, '_ipsl-cm5a-lr_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	
	ncinProj = nc_open(paste0(ncPath, ncname))
	if(ncVarFileName == 'groundwstor')	{
		initGW = ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist]
		projGWdiff = cumsum(diff(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj]))
		nc_varOut$ipsl = c(initGW, last(initGW), last(initGW) + projGWdiff)	* scalar# lon, lat, time
	} else {
		nc_varOut$ipsl = c(ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist] , ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj])	* scalar	# lon, lat, time
	}
	nc_close(ncinProj)
	nc_close(ncinHist)
		# alternative hist data 
	if(ncModel == 'matsiro')	{
		ncname = paste0(ncModel, '_miroc5_ewembi_historical_histsoc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')
	}
	if(ncModel == 'clm45')	{
		ncname = paste0(ncModel, '_miroc5_ewembi_historical_2005soc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')
	}
	if(ncModel == 'watergap2-2c')	{
		ncname = paste0(ncModel, '_gswp3-ewembi_nobc_hist_pressoc_co2_', ncVarFileName, '_global_monthly_1901_2016.nc4') 
	}
	ncinHist = nc_open(paste0(ncPath, ncname))
		# concatenating
	ncname = paste0(ncModel, '_miroc5_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	
	ncinProj = nc_open(paste0(ncPath, ncname))
	if(ncVarFileName == 'groundwstor')	{
		initGW = ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist]
		projGWdiff = cumsum(diff(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj]))
		nc_varOut$miroc5 = c(initGW, last(initGW), last(initGW) + projGWdiff)	* scalar# lon, lat, time
	} else {
		nc_varOut$miroc5 = c(ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist] , ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj])	* scalar # lon, lat, time
	}
	nc_close(ncinProj)
	nc_close(ncinHist)

	names(nc_varOut) = c('year',theseModels)

	
	varOutYr_df = data.frame(year = rep(c(histYears, projYears), length(theseModels)), 
		minVal = NA, maxVal = NA, meanVal = NA, sumVal = NA, rngVal = NA, sdVal = NA)

	iter = 0
	for(thisModel in theseModels)	{
		thisCol = which(names(nc_varOut) == thisModel)
		modSubset = nc_varOut[ , c(1, ..thisCol)]
		for(thisYear in c(histYears, projYears))	{
			iter = iter + 1
			thisSubset = subset(modSubset, year == thisYear)[ , -1]
			varOutYr_df[iter, -1] = c(
				min(thisSubset),
				max(thisSubset),
				mean(unlist(thisSubset)),
				sum(thisSubset),			
				diff(range(thisSubset)),
				sd(unlist(thisSubset)))
		}	
	}
	varOutYr_df$decade = varOutYr_df$year - varOutYr_df$year %% 10
	varOutYrAbs_df = varOutYr_df
	
		# cor.test can't take 0 vals, so adding a small random value 
	varOutYr_df[varOutYr_df == 0] =  runif(length(which(varOutYr_df == 0)), 0, 0.001)
	
	if(convertToPct)	{
			# normalizing to pct
		varOutYr_df$minVal = 100 * (varOutYr_df$minVal / median(varOutYr_df$minVal[varOutYr_df$year %in% normYears])) - 100
		varOutYr_df$maxVal = 100 * (varOutYr_df$maxVal / median(varOutYr_df$maxVal[varOutYr_df$year %in% normYears])) - 100
		varOutYr_df$meanVal = 100 * (varOutYr_df$meanVal / median(varOutYr_df$meanVal[varOutYr_df$year %in% normYears])) - 100
		varOutYr_df$sumVal = 100 * (varOutYr_df$sumVal / median(varOutYr_df$sumVal[varOutYr_df$year %in% normYears])) - 100
		varOutYr_df$rngVal = 100 * (varOutYr_df$rngVal / median(varOutYr_df$rngVal[varOutYr_df$year %in% normYears])) - 100
		varOutYr_df$sdVal = 100 * (varOutYr_df$sdVal / median(varOutYr_df$sdVal[varOutYr_df$year %in% normYears])) - 100
	} else	{
			# normalizing by median
		varOutYr_df$minVal = varOutYr_df$minVal - median(varOutYr_df$minVal[varOutYr_df$year %in% normYears])
		varOutYr_df$maxVal = varOutYr_df$maxVal - median(varOutYr_df$maxVal[varOutYr_df$year %in% normYears])
		varOutYr_df$meanVal = varOutYr_df$meanVal - median(varOutYr_df$meanVal[varOutYr_df$year %in% normYears])
		varOutYr_df$sumVal = varOutYr_df$sumVal - median(varOutYr_df$sumVal[varOutYr_df$year %in% normYears])
		varOutYr_df$rngVal = varOutYr_df$rngVal - median(varOutYr_df$rngVal[varOutYr_df$year %in% normYears])
		varOutYr_df$sdVal = varOutYr_df$sdVal - median(varOutYr_df$sdVal[varOutYr_df$year %in% normYears])
	}
	whichTrendYears = which(varOutYr_df$year %in% trendYears)

	cor.test(varOutYr_df$year[whichTrendYears], varOutYr_df$meanVal[whichTrendYears], method = 'kendall')$p.value
	cor.test(varOutYr_df$year[whichTrendYears], varOutYr_df$meanVal[whichTrendYears], method = 'kendall')$estimate

	varOutYr_df_sub = varOutYr_df[whichTrendYears, ]
	
	numBoxes = length(trendYears) / 10

	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_minVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df_sub$minVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(minVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Min Monthly ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
		labels = paste0(unique(varOutYr_df_sub$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", trendYears[1], ' to ', last(trendYears)), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		text(x=.08*numBoxes, y=maxLim * 0.9, paste0("Values Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$minVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$minVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$minVal)$slope), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else	{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()



	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_maxVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df_sub$maxVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(maxVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Max Monthly ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
		labels = paste0(unique(varOutYr_df_sub$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", trendYears[1], ' to ', last(trendYears)), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		text(x=.08*numBoxes, y=maxLim * 0.9, paste0("Values Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$maxVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$maxVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$maxVal)$slope), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else	{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()



	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_meanVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df_sub$meanVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(meanVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Avg Monthly ' , commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
		labels = paste0(unique(varOutYr_df_sub$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", trendYears[1], ' to ', last(trendYears)), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		text(x=.08*numBoxes, y=maxLim * 0.9, paste0("Values Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$meanVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$meanVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$meanVal)$slope), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()



	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_sumVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df_sub$sumVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(sumVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Total Annual (sum) ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
		labels = paste0(unique(varOutYr_df_sub$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", trendYears[1], ' to ', last(trendYears)), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		text(x=.08*numBoxes, y=maxLim * 0.9, paste0("Values Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sumVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sumVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$sumVal)$slope), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()

	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_rngVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df_sub$rngVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(rngVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual Range in ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
		labels = paste0(unique(varOutYr_df_sub$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", trendYears[1], ' to ', last(trendYears)), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		text(x=.08*numBoxes, y=maxLim * 0.9, paste0("Values Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$rngVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$rngVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$rngVal)$slope), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()




	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_sdVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df_sub$sdVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(sdVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual St. Dev. in ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
		labels = paste0(unique(varOutYr_df_sub$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", trendYears[1], ' to ', last(trendYears)), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		text(x=.08*numBoxes, y=maxLim * 0.9, paste0("Values Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sdVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sdVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$sdVal)$slope), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()

	lookerDF = data.frame(User = character(), Lat = numeric(), Lon = numeric(), Scenario = character(), Decade = numeric(), Variable = character(), Metric = character(),
		Significance = numeric(), Trend_Strength = numeric(), Trend_Units = character(),
		Q05 = numeric(), Q25 = numeric(), Q50 = numeric(), Q75 = numeric(), Q95 = numeric(),
		Location = character()) 
	
	theseDecades = seq(trendYears[1], by = 10, length.out = length(trendYears) / 10)
	for(thisDecade in theseDecades)	{
		subsetYrs = subset(varOutYr_df_sub, decade == thisDecade)
		subsetYrsAbs = subset(varOutYrAbs_df, decade == thisDecade)
		
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Minimum Monthly', 
			signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$minVal, method = 'kendall')$p.value, 3), signif(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$minVal)$slope, 2) * 10, finalMonthlyUnits,
			signif(quantile(subsetYrs$minVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$minVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			locName))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Maximum Monthly', 
			signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$maxVal, method = 'kendall')$p.value, 3), signif(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$maxVal)$slope, 2) * 10, finalMonthlyUnits,
			signif(quantile(subsetYrs$maxVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$maxVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			locName))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Average Monthly', 
			signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$meanVal, method = 'kendall')$p.value, 3), signif(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$meanVal)$slope, 2) * 10, finalMonthlyUnits,
			signif(quantile(subsetYrs$meanVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$meanVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			locName))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Total Annual', 
			signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sumVal, method = 'kendall')$p.value, 3), signif(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$sumVal)$slope, 2) * 10, finalAnnualUnits,
			signif(quantile(subsetYrs$sumVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$sumVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			locName))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Range', 
			signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$rngVal, method = 'kendall')$p.value, 3), signif(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$rngVal)$slope, 2) * 10, finalAnnualUnits,
			signif(quantile(subsetYrs$rngVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$rngVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			locName))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual St. Dev.', 
			signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sdVal, method = 'kendall')$p.value, 3), signif(TheilSen(varOutYr_df_sub$year, varOutYr_df_sub$sdVal)$slope, 2) * 10, finalAnnualUnits,
			signif(quantile(subsetYrs$sdVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$sdVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			locName))
	}
	names(lookerDF) = c('User', 'Lat', 'Lon', 'Scenario', 'Decade', 'Variable', 'Metric', 'Significance', 'Trend_Strength', 'Trend_Units',
		'Q05_rel', 'Q25_rel', 'Q50_rel', 'Q75_rel', 'Q95_rel',
		'Q05_abs', 'Q25_abs', 'Q50_abs', 'Q75_abs', 'Q95_abs',
		'Location')

	if(!file.exists(paste0(dataOutput, '\\outputsForLooker')))	{
		dir.create(file.path(paste0(dataOutput, '\\outputsForLooker')))
	}

	fwrite(lookerDF, paste0(dataOutput, '\\outputsForLooker\\', locName, '_', rcpScen, '_', ncVarFileName, '.csv'))
}



combiningData_f = function(dataOutput = dataOutput){
	allFiles = list.files(paste0(dataOutput, '\\outputsForLooker\\'))
	combFiles = fread(paste0(dataOutput, '\\outputsForLooker\\', allFiles[1]))
	for(thisFile in allFiles[-1])	{
		combFiles = rbind(combFiles, fread(paste0(dataOutput, '\\outputsForLooker\\', thisFile)))
	}
	fwrite(combFiles, paste0(dataOutput, '\\waterTrendsExposure_', userName, Sys.Date(), '.csv'))
}

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
# just the recent period
 
 ###################################################################
## defining functions
waterTrends_f = function(ncPath = ncPath,
	ncModel = ncModel,
	ncVarFileName = ncVarFileName,
	commonVarName = commonVarName,
	repScen = repScen,
	projYears = projYears,
	histYears = histYears,
	normYears = normYears,
	theseModels = theseModels,
	locName = locName,
	locLat = locLat,
	locLon = locLon,
	userName = userName, 
	dataOutput = dataOutput)
{

	if(!file.exists(paste0(dataOutput)))	{
		dir.create(file.path(paste0(dataOutput)))
	}

	scalar = 1
	#recharge is in mean mm / s, so convert to mm / month
	if(ncVarFileName == 'qr')	{
		scalar = 60 * 60 * 24 * 30.4375
	}
	#streamflow is in mean m^3 / s, so convert to m^3 / month
	if(ncVarFileName == 'dis')	{
		scalar = 60 * 60 * 24 * 30.4375
	}

		# reading ssp126 climate data 
	ncname = paste0(ncModel, '_gfdl-esm2m_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_2005co2_thawdepth_global_annual_2006_2099.nc4"  
		# note that watergap treats gw as a relative value (depth below or above a relative datum) so the actual value can go negative (and theoretically can go to any real negative number)
	ncinProj = nc_open(paste0(ncPath, ncname))

		#creating a database for storing all initial data
	nc_lat = ncvar_get(ncinProj, 'lat')#rev(ncvar_get(ncin, 'lat'))	# lat is given from high to low
	nc_lon = ncvar_get(ncinProj, 'lon')
	this_lat = which.min(abs(nc_lat - locLat))
	this_lon = which.min(abs(nc_lon - locLon))
		# ensuring we aren't drawing from a coastal water cell
	if(is.na(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,1]))	{
		this_lat = this_lat + 1
		if(is.na(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,1]))	{
			this_lat = this_lat - 2
			if(is.na(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,1]))	{
				this_lat = this_lat + 1
				this_lon = this_lon + 1
				if(is.na(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,1]))	{
					this_lon = this_lon - 2
				}
			}
		}
	}
	
		# identifying the years to use for projected data
	nc_dateProj = as.Date("1661-01-01") + 10 + ncvar_get(ncinProj, 'time') * 30.4375# time is days after jan 1 1661
	nc_yearsProj = year(nc_dateProj)
	lastYearProj = last(which(nc_yearsProj == last(projYears)))
	firstYearProj = which(nc_yearsProj == projYears[1])[1]
	
	
		# initializing the data table
	nc_varOut = data.table(year = rep(projYears, each=12))


	###############################################
	# reading in data
		# concatenating hist and proj data
	nc_varOut$gfdl = ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj]	* scalar# lon, lat, time

	ncname = paste0(ncModel, '_hadgem2-es_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	
	ncinProj = nc_open(paste0(ncPath, ncname))
	nc_varOut$hadgem2 = ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj] * scalar# lon, lat, time
	
	ncname = paste0(ncModel, '_ipsl-cm5a-lr_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	
	ncinProj = nc_open(paste0(ncPath, ncname))
	nc_varOut$ipsl = ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj]	* scalar	# lon, lat, time

	ncname = paste0(ncModel, '_miroc5_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	
	ncinProj = nc_open(paste0(ncPath, ncname))
	nc_varOut$miroc5 = ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj]	* scalar # lon, lat, time

	names(nc_varOut) = c('year',theseModels)

	
	varOutYr_df = data.frame(year = rep(projYears, length(theseModels)), 
		minVal = NA, maxVal = NA, meanVal = NA, sumVal = NA, rngVal = NA, sdVal = NA)

	iter = 0
	for(thisModel in theseModels)	{
		thisCol = which(names(nc_varOut) == thisModel)
		modSubset = nc_varOut[ , c(1, ..thisCol)]
		for(thisYear in projYears)	{
			iter = iter + 1
			thisSubset = subset(nc_varOut, year == thisYear)[ , -1]
			varOutYr_df[iter, -1] = c(
				min(thisSubset),
				max(thisSubset),
				mean(unlist(thisSubset)),
				sum(thisSubset),
				diff(range(thisSubset)),
				sd(unlist(thisSubset)))
		}	
	}
	varOutYr_df$decade = varOutYr_df$year - varOutYr_df$year %% 10
	varOutYrAbs_df = varOutYr_df
	
		# cor.test can't take 0 vals, so adding a small random value 
	varOutYr_df$minVal = varOutYr_df$minVal  + runif(nrow(varOutYr_df), 0, 0.000001)
		# normalizing by median
	varOutYr_df$minVal = varOutYr_df$minVal - median(varOutYr_df$minVal[varOutYr_df$year %in% normYears])
	varOutYr_df$maxVal = varOutYr_df$maxVal - median(varOutYr_df$maxVal[varOutYr_df$year %in% normYears])
	varOutYr_df$meanVal = varOutYr_df$meanVal - median(varOutYr_df$meanVal[varOutYr_df$year %in% normYears])
	varOutYr_df$sumVal = varOutYr_df$sumVal - median(varOutYr_df$sumVal[varOutYr_df$year %in% normYears])
	varOutYr_df$rngVal = varOutYr_df$rngVal - median(varOutYr_df$rngVal[varOutYr_df$year %in% normYears])
	varOutYr_df$sdVal = varOutYr_df$sdVal - median(varOutYr_df$sdVal[varOutYr_df$year %in% normYears])

	cor.test(varOutYr_df$year, varOutYr_df$minVal, method = 'kendall')$p.value
	cor.test(varOutYr_df$year, varOutYr_df$minVal, method = 'kendall')$estimate


	numBoxes = length(projYears) / 10

	theUnits = 'mm'
	if(ncVarFileName == 'dis')	{
		theUnits = 'm^3'
	}
	
	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_minVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df$minVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(minVal ~ decade, data=varOutYr_df, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual Minimimum ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df$decade)),
		labels = paste0(unique(varOutYr_df$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Change Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df$year, varOutYr_df$minVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df$year, varOutYr_df$minVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(lm(minVal ~ year, varOutYr_df)$coef[2]), 2) * 10, theUnits, ' per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else	{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()



	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_maxVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df$maxVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(maxVal ~ decade, data=varOutYr_df, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual Maximum ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df$decade)),
		labels = paste0(unique(varOutYr_df$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Change Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df$year, varOutYr_df$maxVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df$year, varOutYr_df$maxVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(lm(maxVal ~ year, varOutYr_df)$coef[2]), 2) * 10, theUnits, ' per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else	{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()



	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_meanVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df$meanVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(meanVal ~ decade, data=varOutYr_df, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual Average ' , commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df$decade)),
		labels = paste0(unique(varOutYr_df$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Change Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df$year, varOutYr_df$meanVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df$year, varOutYr_df$meanVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(lm(meanVal ~ year, varOutYr_df)$coef[2]), 2) * 10, theUnits, ' per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()



	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_sumVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df$sumVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(sumVal ~ decade, data=varOutYr_df, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual Total ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df$decade)),
		labels = paste0(unique(varOutYr_df$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Change Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df$year, varOutYr_df$sumVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df$year, varOutYr_df$sumVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(lm(sumVal ~ year, varOutYr_df)$coef[2]), 2) * 10, theUnits, ' per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()

	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_rngVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df$rngVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(rngVal ~ decade, data=varOutYr_df, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual Range in ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df$decade)),
		labels = paste0(unique(varOutYr_df$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Change Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df$year, varOutYr_df$rngVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df$year, varOutYr_df$rngVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(lm(rngVal ~ year, varOutYr_df)$coef[2]), 2) * 10, theUnits, ' per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()




	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_sdVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	maxLim = max(abs(quantile(varOutYr_df$sdVal, c(0.05,0.95))))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(sdVal ~ decade, data=varOutYr_df, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual St. Dev. in ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(maxLim * -1.1, maxLim * 1.1))
		axis(1, at=1:length(unique(varOutYr_df$decade)),
		labels = paste0(unique(varOutYr_df$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=.08*numBoxes, y=maxLim * 1, paste0("Change Relative to ", normYears[1], ' to ', last(normYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df$year, varOutYr_df$sdVal, method = 'kendall')$p.value < 0.05)	{
			text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df$year, varOutYr_df$sdVal, method = 'kendall')$estimate < 0)	{
				text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(lm(sdVal ~ year, varOutYr_df)$coef[2]), 2) * 10, theUnits, ' per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()

	lookerDF = data.frame(User = character(), Lat = numeric(), Lon = numeric(), Scenario = character(), Decade = numeric(), Variable = character(), Metric = character(),
		Significance = numeric(), Trend_Strength = numeric(), Trend_Units = character(),
		Q05 = numeric(), Q25 = numeric(), Q50 = numeric(), Q75 = numeric(), Q95 = numeric()) 
	
	theseDecades = seq(projYears[1], by = 10, length.out = length(projYears) / 10)
	for(thisDecade in theseDecades)	{
		subsetYrs = subset(varOutYr_df, decade == thisDecade)
		subsetYrsAbs = subset(varOutYrAbs_df, decade == thisDecade)
		
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Minimum', 
			signif(cor.test(varOutYr_df$year, varOutYr_df$minVal, method = 'kendall')$p.value, 3), signif(lm(minVal ~ year, varOutYr_df)$coef[2], 2) * 10, 'mm per month',
			signif(quantile(subsetYrs$minVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$minVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Maximum', 
			signif(cor.test(varOutYr_df$year, varOutYr_df$maxVal, method = 'kendall')$p.value, 3), signif(lm(maxVal ~ year, varOutYr_df)$coef[2], 2) * 10, 'mm per month',
			signif(quantile(subsetYrs$maxVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$maxVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Average', 
			signif(cor.test(varOutYr_df$year, varOutYr_df$meanVal, method = 'kendall')$p.value, 3), signif(lm(meanVal ~ year, varOutYr_df)$coef[2], 2) * 10, 'mm per month',
			signif(quantile(subsetYrs$meanVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$meanVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Total (sum)', 
			signif(cor.test(varOutYr_df$year, varOutYr_df$sumVal, method = 'kendall')$p.value, 3), signif(lm(sumVal ~ year, varOutYr_df)$coef[2], 2) * 10, 'annual mm',
			signif(quantile(subsetYrs$sumVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$sumVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Range', 
			signif(cor.test(varOutYr_df$year, varOutYr_df$rngVal, method = 'kendall')$p.value, 3), signif(lm(rngVal ~ year, varOutYr_df)$coef[2], 2) * 10, 'mm per month',
			signif(quantile(subsetYrs$rngVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$rngVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual St. Dev.', 
			signif(cor.test(varOutYr_df$year, varOutYr_df$sdVal, method = 'kendall')$p.value, 3), signif(lm(sdVal ~ year, varOutYr_df)$coef[2], 2) * 10, 'mm per month',
			signif(quantile(subsetYrs$sdVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$sdVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
	}
	names(lookerDF) = c('User', 'Lat', 'Lon', 'Scenario', 'Decade', 'Variable', 'Metric', 'Significance', 'Trend_Strength', 'Trend_Units',
		'Q05_rel', 'Q25_rel', 'Q50_rel', 'Q75_rel', 'Q95_rel',
		'Q05_abs', 'Q25_abs', 'Q50_abs', 'Q75_abs', 'Q95_abs')

	if(!file.exists(paste0(dataOutput, '\\outputsForLooker')))	{
		dir.create(file.path(paste0(dataOutput, '\\outputsForLooker')))
	}

	fwrite(lookerDF, paste0(dataOutput, '\\outputsForLooker\\', locName, '_', rcpScen, '_', ncVarFileName, '.csv'))
}

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 							
for(jj in 1:nrow(hydroVariables))	{
	ncPath = paste0("J:\\Cai_data\\TCFD\\", hydroVariables$varLoc[jj], "\\")		# where are the relevant data stored?								GWstorage,        RootZoneSoilMoisture, GWrecharge   , SurfaceWater_Streamflow,	Evapotranspiration
	ncVarFileName = hydroVariables$varFileName[jj]									# what is the var name in the .nc? 									groundwstor,      rootmoist           , qr           , dis
	ncModel = hydroVariables$model[jj]											# what is the name of the model used to derive the var of interest?	    watergap2-2c,     clm45,              , watergap2-2c , matsiro,					clm45
	commonVarName = hydroVariables$commonName[jj]							# user friendly name (with units) of the var of interest					Groundwater (mm), Soil Moisture (mm)  , Recharge (mm), Streamflow (%),			ET (mm)
	convertToPct = ifelse(ncVarFileName == 'dis', TRUE, FALSE)	# do we convert to a % or keep in orig units
	print(jj)
	for(rcpScen in c('rcp26', 'rcp60'))	{
		waterTrends_f(ncPath = ncPath,
			ncModel = ncModel,
			ncVarFileName = ncVarFileName,
			commonVarName = commonVarName,
			repScen = repScen,
			projYears = projYears,
			histYears = histYears,
			normYears = normYears,
			trendYears = trendYears, 
			theseModels = theseModels,
			locName = locName,
			locLat = locLat,
			locLon = locLon,
			userName = userName, 
			dataOutput = dataOutput)
		
	}	
}


	# for combining all data for one user
combiningData_f(dataOutput = dataOutput)



###################################################################
## defining functions
waterTrends_f = function(ncPath = ncPath,
	ncModel = ncModel,
	ncVarFileName = ncVarFileName,
	commonVarName = commonVarName,
	repScen = repScen,
	projYears = projYears,
	histYears = histYears,
	normYears = normYears,
	trendYears = trendYears, 
	theseModels = theseModels,
	locName = locName,
	locLat = locLat,
	locLon = locLon,
	userName = userName, 
	dataOutput = dataOutput,
	locCsv = locCsv)
{

	if(!file.exists(paste0(dataOutput)))	{
		dir.create(file.path(paste0(dataOutput)))
	}
	
	finalMonthlyUnits = ifelse(convertToPct, "pct per decade", "monthly mm per decade")
	finalAnnualUnits = ifelse(convertToPct, "pct per decade", "annual mm per decade")

	scalar = 1
	#recharge and evap are in in mean mm / s, so convert to mm / month
	if(ncVarFileName %in% c('qr', 'evap'))	{
		scalar = 60 * 60 * 24 * 30.4375
	}
	#streamflow is in mean m^3 / s, so convert to mm / month
	#if(ncVarFileName == 'dis')	{
	#	scalar = 60 * 60 * 24 * 30.4375 * 1000 / (1000 * 1000)
	#}

	numLocs = nrow(locCsv)
	for(i in 1:numLocs)	{
		print(i)
		locName = locCsv$Location[i]
		locLat = locCsv$Lat[i]
		locLon = locCsv$Lon[i]
				

		if(i == 1)	{
			# reading ssp126 climate data 
			ncname = paste0(ncModel, '_gfdl-esm2m_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_2005co2_thawdepth_global_annual_2006_2099.nc4"  
				# note that watergap treats gw as a relative value (depth below or above a relative datum) so the actual value can go negative (and theoretically can go to any real negative number)
			ncinProj_gf = nc_open(paste0(ncPath, ncname))
			nc_lat = ncvar_get(ncinProj_gf, 'lat')#rev(ncvar_get(ncin, 'lat'))	# lat is given from high to low
			nc_lon = ncvar_get(ncinProj_gf, 'lon')
		}

				#creating a database for storing all initial data
		this_lat = which.min(abs(nc_lat - locLat))
		this_lon = which.min(abs(nc_lon - locLon))
			# ensuring we aren't drawing from a coastal water cell
		if(is.na(ncvar_get(ncinProj_gf, ncVarFileName)[this_lon,this_lat,1]))	{
			this_lat = this_lat + 1
			if(is.na(ncvar_get(ncinProj_gf, ncVarFileName)[this_lon,this_lat,1]))	{
				this_lat = this_lat - 2
				if(is.na(ncvar_get(ncinProj_gf, ncVarFileName)[this_lon,this_lat,1]))	{
					this_lat = this_lat + 1
					this_lon = this_lon + 1
					if(is.na(ncvar_get(ncinProj_gf, ncVarFileName)[this_lon,this_lat,1]))	{
						this_lon = this_lon - 2
					}
				}
			}
		}
		
		if(i == 1)	{
				# identifying the years to use for projected data
			nc_dateProj = as.Date("1661-01-01") + 10 + ncvar_get(ncinProj_gf, 'time') * 30.4375# time is days after jan 1 1661
			nc_yearsProj = year(nc_dateProj)
			lastYearProj = last(which(nc_yearsProj == last(projYears)))
			firstYearProj = which(nc_yearsProj == projYears[1])[1]
			
				# identifying the years to use for projected data
			if(ncModel == 'matsiro')	{
				ncname = paste0(ncModel, '_ipsl-cm5a-lr_ewembi_historical_histsoc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')
				ncinHist = nc_open(paste0(ncPath, ncname))
				nc_dateHist = as.Date("1661-01-01") + 10 + ncvar_get(ncinHist, 'time') * 30.4375	# time is days after jan 1 1661
			}
			if(ncModel == 'clm45')	{
				ncname = paste0(ncModel, '_ipsl-cm5a-lr_ewembi_historical_2005soc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')
				ncinHist = nc_open(paste0(ncPath, ncname))
				nc_dateHist = as.Date("1661-01-01") + 10 + ncvar_get(ncinHist, 'time') * 30.4375	# time is days after jan 1 1661
			}
			if(ncModel == 'watergap2-2c')	{
				ncname = paste0(ncModel, '_gswp3-w5e5_nobc_hist_varsoc_co2_', ncVarFileName, '_global_monthly_1901_2016.nc4')
				ncinHist = nc_open(paste0(ncPath, ncname))
				nc_dateHist = as.Date("1901-01-01") + 10 + ncvar_get(ncinHist, 'time') * 30.4375	# time is days after jan 1 1901
			}
			nc_yearsHist = year(nc_dateHist)
			lastYearHist = last(which(nc_yearsHist == last(histYears)))
			firstYearHist = which(nc_yearsHist == histYears[1])[1]
		}
			
			# initializing the data table
		nc_varOut = data.table(year = rep(c(histYears, projYears), each=12))


		###############################################
		# reading in and concatenating data
			# concatenating hist and proj data
		if(ncVarFileName == 'groundwstor')	{
			initGW = ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist]
			projGWdiff = cumsum(diff(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj]))
			nc_varOut$gfdl = c(initGW, last(initGW), last(initGW) + projGWdiff)	* scalar# lon, lat, time
		} else {
			nc_varOut$gfdl = c(ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist] , ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj])	* scalar# lon, lat, time
		}

		if(i == 1)	{
				# alternative hist data 
			if(ncModel == 'matsiro')	{
				ncname = paste0(ncModel, '_hadgem2-es_ewembi_historical_histsoc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4') 
			}
			if(ncModel == 'clm45')	{
				ncname = paste0(ncModel, '_hadgem2-es_ewembi_historical_2005soc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4') 
			}
			if(ncModel == 'watergap2-2c')	{
				ncname = paste0(ncModel, '_gswp3-w5e5_nobc_hist_pressoc_co2_', ncVarFileName, '_global_monthly_1901_2016.nc4')
			}
			ncinHist = nc_open(paste0(ncPath, ncname))
				# concatenating
			ncname = paste0(ncModel, '_hadgem2-es_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	
			ncinProj = nc_open(paste0(ncPath, ncname))
		}

		if(ncVarFileName == 'groundwstor')	{
			initGW = ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist]
			projGWdiff = cumsum(diff(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj]))
			nc_varOut$hadgem2 = c(initGW, last(initGW), last(initGW) + projGWdiff)	* scalar# lon, lat, time
		} else {
			nc_varOut$hadgem2 = c(ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist] , ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj])	* scalar	# lon, lat, time
		}

		if(i == 1)	{
				# alternative hist data 
			if(ncModel == 'matsiro')	{
				ncname = paste0(ncModel, '_gfdl-esm2m_ewembi_historical_histsoc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')  
			}
			if(ncModel == 'clm45')	{
				ncname = paste0(ncModel, '_gfdl-esm2m_ewembi_historical_2005soc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')  
			}
			if(ncModel == 'watergap2-2c')	{
				ncname = paste0(ncModel, '_gswp3-ewembi_nobc_hist_varsoc_co2_', ncVarFileName, '_global_monthly_1901_2016.nc4')
			}
			ncinHist = nc_open(paste0(ncPath, ncname))
				# concatenating 
			ncname = paste0(ncModel, '_ipsl-cm5a-lr_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	
			ncinProj = nc_open(paste0(ncPath, ncname))
		}
			if(ncVarFileName == 'groundwstor')	{
			initGW = ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist]
			projGWdiff = cumsum(diff(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj]))
			nc_varOut$ipsl = c(initGW, last(initGW), last(initGW) + projGWdiff)	* scalar# lon, lat, time
		} else {
			nc_varOut$ipsl = c(ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist] , ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj])	* scalar	# lon, lat, time
		}

		if(i == 1)	{
				# alternative hist data 
			if(ncModel == 'matsiro')	{
				ncname = paste0(ncModel, '_miroc5_ewembi_historical_histsoc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')
			}
			if(ncModel == 'clm45')	{
				ncname = paste0(ncModel, '_miroc5_ewembi_historical_2005soc_co2_', ncVarFileName, '_global_monthly_1861_2005.nc4')
			}
			if(ncModel == 'watergap2-2c')	{
				ncname = paste0(ncModel, '_gswp3-ewembi_nobc_hist_pressoc_co2_', ncVarFileName, '_global_monthly_1901_2016.nc4') 
			}
			ncinHist = nc_open(paste0(ncPath, ncname))
				# concatenating
			ncname = paste0(ncModel, '_miroc5_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	
			ncinProj = nc_open(paste0(ncPath, ncname))
		}
		if(ncVarFileName == 'groundwstor')	{
			initGW = ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist]
			projGWdiff = cumsum(diff(ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj]))
			nc_varOut$miroc5 = c(initGW, last(initGW), last(initGW) + projGWdiff)	* scalar# lon, lat, time
		} else {
			nc_varOut$miroc5 = c(ncvar_get(ncinHist, ncVarFileName)[this_lon,this_lat,firstYearHist:lastYearHist] , ncvar_get(ncinProj, ncVarFileName)[this_lon,this_lat,firstYearProj:lastYearProj])	* scalar # lon, lat, time
		}

		names(nc_varOut) = c('year',theseModels)

		
		varOutYr_df = data.frame(year = rep(c(histYears, projYears), length(theseModels)), 
			minVal = NA, maxVal = NA, meanVal = NA, sumVal = NA, rngVal = NA, sdVal = NA)

		iter = 0
		for(thisModel in theseModels)	{
			thisCol = which(names(nc_varOut) == thisModel)
			modSubset = nc_varOut[ , c(1, ..thisCol)]
			for(thisYear in c(histYears, projYears))	{
				iter = iter + 1
				thisSubset = subset(nc_varOut, year == thisYear)[ , -1]
				varOutYr_df[iter, -1] = c(
					min(thisSubset),
					max(thisSubset),
					mean(unlist(thisSubset)),
					sum(thisSubset),
					diff(range(thisSubset)),
					sd(unlist(thisSubset)))
			}	
		}
		varOutYr_df$decade = varOutYr_df$year - varOutYr_df$year %% 10
		varOutYrAbs_df = varOutYr_df
		
			# cor.test can't take 0 vals, so adding a small random value 
		varOutYr_df[varOutYr_df == 0] =  runif(length(which(varOutYr_df == 0)), 0, 0.001)
		
		if(convertToPct)	{
				# normalizing to pct
			varOutYr_df$minVal = 100 * (varOutYr_df$minVal / mean(varOutYr_df$minVal[varOutYr_df$year %in% normYears])) - 100
			varOutYr_df$maxVal = 100 * (varOutYr_df$maxVal / mean(varOutYr_df$maxVal[varOutYr_df$year %in% normYears])) - 100
			varOutYr_df$meanVal = 100 * (varOutYr_df$meanVal / mean(varOutYr_df$meanVal[varOutYr_df$year %in% normYears])) - 100
			varOutYr_df$sumVal = 100 * (varOutYr_df$sumVal / mean(varOutYr_df$sumVal[varOutYr_df$year %in% normYears])) - 100
			varOutYr_df$rngVal = 100 * (varOutYr_df$rngVal / mean(varOutYr_df$rngVal[varOutYr_df$year %in% normYears])) - 100
			varOutYr_df$sdVal = 100 * (varOutYr_df$sdVal / mean(varOutYr_df$sdVal[varOutYr_df$year %in% normYears])) - 100
		} else	{
				# normalizing by median
			varOutYr_df$minVal = varOutYr_df$minVal - median(varOutYr_df$minVal[varOutYr_df$year %in% normYears])
			varOutYr_df$maxVal = varOutYr_df$maxVal - median(varOutYr_df$maxVal[varOutYr_df$year %in% normYears])
			varOutYr_df$meanVal = varOutYr_df$meanVal - median(varOutYr_df$meanVal[varOutYr_df$year %in% normYears])
			varOutYr_df$sumVal = varOutYr_df$sumVal - median(varOutYr_df$sumVal[varOutYr_df$year %in% normYears])
			varOutYr_df$rngVal = varOutYr_df$rngVal - median(varOutYr_df$rngVal[varOutYr_df$year %in% normYears])
			varOutYr_df$sdVal = varOutYr_df$sdVal - median(varOutYr_df$sdVal[varOutYr_df$year %in% normYears])
		}
		whichTrendYears = which(varOutYr_df$year %in% trendYears)

		cor.test(varOutYr_df$year[whichTrendYears], varOutYr_df$meanVal[whichTrendYears], method = 'kendall')$p.value
		cor.test(varOutYr_df$year[whichTrendYears], varOutYr_df$meanVal[whichTrendYears], method = 'kendall')$estimate

		varOutYr_df_sub = varOutYr_df[whichTrendYears, ]
		
		numBoxes = trendYears / 10

		png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_minVal.png'), width = 720, height = 720)
		windowsFonts(A = windowsFont("Roboto"))
		maxLim = max(abs(quantile(varOutYr_df_sub$minVal, c(0.05,0.95))))
		par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
		boxplot(minVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
					main='', ylab=paste0('Min Monthly ', commonVarName), xlab='Decade', xaxt='n',
					col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
					family='A', 
					ylim = c(maxLim * -1.1, maxLim * 1.1))
			axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
			labels = paste0(unique(varOutYr_df_sub$decade), 's'),
					col.lab='#1A232F', col.axis='#666D74')
			abline(h=0, lwd=2, col='#1A232F', cex=2)
			text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", whichTrendYears[1], ' to ', last(whichTrendYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$minVal, method = 'kendall')$p.value < 0.05)	{
				text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
				if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$minVal, method = 'kendall')$estimate < 0)	{
					text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
				} else { 
					text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
				}
				text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(lm(minVal ~ year, varOutYr_df_sub)$coef[2]), 2) * 10, 'mm per decade'),
					adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			}	else	{
				text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			}	
		dev.off()



		png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_maxVal.png'), width = 720, height = 720)
		windowsFonts(A = windowsFont("Roboto"))
		maxLim = max(abs(quantile(varOutYr_df_sub$maxVal, c(0.05,0.95))))
		par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
		boxplot(maxVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
					main='', ylab=paste0('Max Monthly ', commonVarName), xlab='Decade', xaxt='n',
					col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
					family='A', 
					ylim = c(maxLim * -1.1, maxLim * 1.1))
			axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
			labels = paste0(unique(varOutYr_df_sub$decade), 's'),
					col.lab='#1A232F', col.axis='#666D74')
			abline(h=0, lwd=2, col='#1A232F', cex=2)
			text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", whichTrendYears[1], ' to ', last(whichTrendYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$maxVal, method = 'kendall')$p.value < 0.05)	{
				text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
				if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$maxVal, method = 'kendall')$estimate < 0)	{
					text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
				} else { 
					text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
				}
				text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(lm(maxVal ~ year, varOutYr_df_sub)$coef[2]), 2) * 10, 'mm per decade'),
					adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			}	else	{
				text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			}	
		dev.off()



		png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_meanVal.png'), width = 720, height = 720)
		windowsFonts(A = windowsFont("Roboto"))
		maxLim = max(abs(quantile(varOutYr_df_sub$meanVal, c(0.05,0.95))))
		par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
		boxplot(meanVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
					main='', ylab=paste0('Avg Monthly ' , commonVarName), xlab='Decade', xaxt='n',
					col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
					family='A', 
					ylim = c(maxLim * -1.1, maxLim * 1.1))
			axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
			labels = paste0(unique(varOutYr_df_sub$decade), 's'),
					col.lab='#1A232F', col.axis='#666D74')
			abline(h=0, lwd=2, col='#1A232F', cex=2)
			text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", whichTrendYears[1], ' to ', last(whichTrendYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$meanVal, method = 'kendall')$p.value < 0.05)	{
				text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
				if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$meanVal, method = 'kendall')$estimate < 0)	{
					text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
				} else { 
					text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
				}
				text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(lm(meanVal ~ year, varOutYr_df_sub)$coef[2]), 2) * 10, 'mm per decade'),
					adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			}	else{
				text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			}	
		dev.off()



		png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_sumVal.png'), width = 720, height = 720)
		windowsFonts(A = windowsFont("Roboto"))
		maxLim = max(abs(quantile(varOutYr_df_sub$sumVal, c(0.05,0.95))))
		par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
		boxplot(sumVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
					main='', ylab=paste0('Total Annual (sum) ', commonVarName), xlab='Decade', xaxt='n',
					col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
					family='A', 
					ylim = c(maxLim * -1.1, maxLim * 1.1))
			axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
			labels = paste0(unique(varOutYr_df_sub$decade), 's'),
					col.lab='#1A232F', col.axis='#666D74')
			abline(h=0, lwd=2, col='#1A232F', cex=2)
			text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", whichTrendYears[1], ' to ', last(whichTrendYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sumVal, method = 'kendall')$p.value < 0.05)	{
				text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
				if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sumVal, method = 'kendall')$estimate < 0)	{
					text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
				} else { 
					text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
				}
				text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(lm(sumVal ~ year, varOutYr_df_sub)$coef[2]), 2) * 10, 'mm per decade'),
					adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			}	else{
				text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			}	
		dev.off()

		png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_rngVal.png'), width = 720, height = 720)
		windowsFonts(A = windowsFont("Roboto"))
		maxLim = max(abs(quantile(varOutYr_df_sub$rngVal, c(0.05,0.95))))
		par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
		boxplot(rngVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
					main='', ylab=paste0('Annual Range in ', commonVarName), xlab='Decade', xaxt='n',
					col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
					family='A', 
					ylim = c(maxLim * -1.1, maxLim * 1.1))
			axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
			labels = paste0(unique(varOutYr_df_sub$decade), 's'),
					col.lab='#1A232F', col.axis='#666D74')
			abline(h=0, lwd=2, col='#1A232F', cex=2)
			text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", whichTrendYears[1], ' to ', last(whichTrendYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$rngVal, method = 'kendall')$p.value < 0.05)	{
				text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
				if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$rngVal, method = 'kendall')$estimate < 0)	{
					text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
				} else { 
					text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
				}
				text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(lm(rngVal ~ year, varOutYr_df_sub)$coef[2]), 2) * 10, 'mm per decade'),
					adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			}	else{
				text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			}	
		dev.off()




		png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_sdVal.png'), width = 720, height = 720)
		windowsFonts(A = windowsFont("Roboto"))
		maxLim = max(abs(quantile(varOutYr_df_sub$sdVal, c(0.05,0.95))))
		par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
		boxplot(sdVal ~ decade, data=varOutYr_df_sub, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
					main='', ylab=paste0('Annual St. Dev. in ', commonVarName), xlab='Decade', xaxt='n',
					col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
					family='A', 
					ylim = c(maxLim * -1.1, maxLim * 1.1))
			axis(1, at=1:length(unique(varOutYr_df_sub$decade)),
			labels = paste0(unique(varOutYr_df_sub$decade), 's'),
					col.lab='#1A232F', col.axis='#666D74')
			abline(h=0, lwd=2, col='#1A232F', cex=2)
			text(x=.08*numBoxes, y=maxLim * 1, paste0("Trend Over Period ", whichTrendYears[1], ' to ', last(whichTrendYears), ' Avg.'), adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sdVal, method = 'kendall')$p.value < 0.05)	{
				text(x=.08*numBoxes, y=-maxLim * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
				if(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sdVal, method = 'kendall')$estimate < 0)	{
					text(x=.654*numBoxes, y=-maxLim * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
				} else { 
					text(x=.654*numBoxes, y=-maxLim * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
				}
				text(x=.08*numBoxes, y=-maxLim * 1.1, paste0("of ", signif(abs(lm(sdVal ~ year, varOutYr_df_sub)$coef[2]), 2) * 10, 'mm per decade'),
					adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			}	else{
				text(x=.08*numBoxes, y=-maxLim * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			}	
		dev.off()

		lookerDF = data.frame(User = character(), Lat = numeric(), Lon = numeric(), Scenario = character(), Decade = numeric(), Variable = character(), Metric = character(),
			Significance = numeric(), Trend_Strength = numeric(), Trend_Units = character(),
			Q05 = numeric(), Q25 = numeric(), Q50 = numeric(), Q75 = numeric(), Q95 = numeric()) 
		
		theseDecades = seq(trendYears[1], by = 10, length.out = length(trendYears) / 10)
		for(thisDecade in theseDecades)	{
			subsetYrs = subset(varOutYr_df_sub, decade == thisDecade)
			subsetYrsAbs = subset(varOutYrAbs_df, decade == thisDecade)
			
			lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Minimum Monthly', 
				signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$minVal, method = 'kendall')$p.value, 3), signif(lm(minVal ~ year, varOutYr_df_sub)$coef[2], 2) * 10, finalMonthlyUnits,
				signif(quantile(subsetYrs$minVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
				signif(quantile(subsetYrsAbs$minVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
			lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Maximum Monthly', 
				signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$maxVal, method = 'kendall')$p.value, 3), signif(lm(maxVal ~ year, varOutYr_df_sub)$coef[2], 2) * 10, finalMonthlyUnits,
				signif(quantile(subsetYrs$maxVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
				signif(quantile(subsetYrsAbs$maxVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
			lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Average Monthly', 
				signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$meanVal, method = 'kendall')$p.value, 3), signif(lm(meanVal ~ year, varOutYr_df_sub)$coef[2], 2) * 10, finalMonthlyUnits,
				signif(quantile(subsetYrs$meanVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
				signif(quantile(subsetYrsAbs$meanVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
			lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Total Annual', 
				signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sumVal, method = 'kendall')$p.value, 3), signif(lm(sumVal ~ year, varOutYr_df_sub)$coef[2], 2) * 10, finalAnnualUnits,
				signif(quantile(subsetYrs$sumVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
				signif(quantile(subsetYrsAbs$sumVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
			lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Range', 
				signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$rngVal, method = 'kendall')$p.value, 3), signif(lm(rngVal ~ year, varOutYr_df_sub)$coef[2], 2) * 10, finalAnnualUnits,
				signif(quantile(subsetYrs$rngVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
				signif(quantile(subsetYrsAbs$rngVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
			lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual St. Dev.', 
				signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sdVal, method = 'kendall')$p.value, 3), signif(lm(sdVal ~ year, varOutYr_df_sub)$coef[2], 2) * 10, finalAnnualUnits,
				signif(quantile(subsetYrs$sdVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
				signif(quantile(subsetYrsAbs$sdVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
		}
		names(lookerDF) = c('User', 'Lat', 'Lon', 'Scenario', 'Decade', 'Variable', 'Metric', 'Significance', 'Trend_Strength', 'Trend_Units',
			'Q05_rel', 'Q25_rel', 'Q50_rel', 'Q75_rel', 'Q95_rel',
			'Q05_abs', 'Q25_abs', 'Q50_abs', 'Q75_abs', 'Q95_abs')
		lookerDF = data.frame(User = character(), Lat = numeric(), Lon = numeric(), Scenario = character(), Decade = numeric(), Variable = character(), Metric = character(),
			Significance = numeric(), Trend_Strength = numeric(), Trend_Units = character(),
			Q05 = numeric(), Q25 = numeric(), Q50 = numeric(), Q75 = numeric(), Q95 = numeric(),
			Location = character()) 
		
		theseDecades = seq(trendYears[1], by = 10, length.out = length(trendYears) / 10)
		for(thisDecade in theseDecades)	{
			subsetYrs = subset(varOutYr_df_sub, decade == thisDecade)
			subsetYrsAbs = subset(varOutYrAbs_df, decade == thisDecade)
			
			lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Minimum Monthly', 
				signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$minVal, method = 'kendall')$p.value, 3), signif(lm(minVal ~ year, varOutYr_df_sub)$coef[2], 2) * 10, finalMonthlyUnits,
				signif(quantile(subsetYrs$minVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
				signif(quantile(subsetYrsAbs$minVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
				locName))
			lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Maximum Monthly', 
				signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$maxVal, method = 'kendall')$p.value, 3), signif(lm(maxVal ~ year, varOutYr_df_sub)$coef[2], 2) * 10, finalMonthlyUnits,
				signif(quantile(subsetYrs$maxVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
				signif(quantile(subsetYrsAbs$maxVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)),
				locName)
			lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Average Monthly', 
				signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$meanVal, method = 'kendall')$p.value, 3), signif(lm(meanVal ~ year, varOutYr_df_sub)$coef[2], 2) * 10, finalMonthlyUnits,
				signif(quantile(subsetYrs$meanVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
				signif(quantile(subsetYrsAbs$meanVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)),
				locName)
			lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Total Annual', 
				signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sumVal, method = 'kendall')$p.value, 3), signif(lm(sumVal ~ year, varOutYr_df_sub)$coef[2], 2) * 10, finalAnnualUnits,
				signif(quantile(subsetYrs$sumVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
				signif(quantile(subsetYrsAbs$sumVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)),
				locName)
			lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Range', 
				signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$rngVal, method = 'kendall')$p.value, 3), signif(lm(rngVal ~ year, varOutYr_df_sub)$coef[2], 2) * 10, finalAnnualUnits,
				signif(quantile(subsetYrs$rngVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
				signif(quantile(subsetYrsAbs$rngVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)),
				locName)
			lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual St. Dev.', 
				signif(cor.test(varOutYr_df_sub$year, varOutYr_df_sub$sdVal, method = 'kendall')$p.value, 3), signif(lm(sdVal ~ year, varOutYr_df_sub)$coef[2], 2) * 10, finalAnnualUnits,
				signif(quantile(subsetYrs$sdVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
				signif(quantile(subsetYrsAbs$sdVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)),
				locName)
		}
		names(lookerDF) = c('User', 'Lat', 'Lon', 'Scenario', 'Decade', 'Variable', 'Metric', 'Significance', 'Trend_Strength', 'Trend_Units',
			'Q05_rel', 'Q25_rel', 'Q50_rel', 'Q75_rel', 'Q95_rel',
			'Q05_abs', 'Q25_abs', 'Q50_abs', 'Q75_abs', 'Q95_abs',
			'Location')

		if(!file.exists(paste0(dataOutput, '\\outputsForLooker')))	{
			dir.create(file.path(paste0(dataOutput, '\\outputsForLooker')))
		}

		fwrite(lookerDF, paste0(dataOutput, '\\outputsForLooker\\', locName, '_', rcpScen, '_', ncVarFileName, '.csv'))
	}
	nc_close(ncinHist)
	nc_close(ncinProj)
}



combiningData_f = function(dataOutput = dataOutput){
	allFiles = list.files(paste0(dataOutput, '\\outputsForLooker\\'))
	combFiles = fread(paste0(dataOutput, '\\outputsForLooker\\', allFiles[1]))
	for(thisFile in allFiles[-1])	{
		combFiles = rbind(combFiles, fread(paste0(dataOutput, '\\outputsForLooker\\', thisFile)))
	}
	fwrite(combFiles, paste0(dataOutput, '\\waterTrendsExposure_', userName, Sys.Date(), '.csv'))
}



 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ###############################
 # same but for entire period of record

########################################################
### library(hydroGOF)		# for nse calculations
library(dataRetrieval)	# for streamflow data (I think)
library(data.table)
library(sf)
	sf::sf_use_s2(FALSE) # for problem with intersecting spherical w flat
library(lubridate)
library(ddplyr) # for left_join() merging by date
library(ncdf4)
library(magrittr)
library(maptools)


#########################################
# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\GWstorage\\"

this_lat = which.min(abs(nc_lat - -24.900833013921915))
this_lon = which.min(abs(nc_lon - 152.41212621076588))

	# New South Wales
this_lat = which.min(abs(nc_lat - -35.7))
this_lon = which.min(abs(nc_lon - 146))

this_lat = which.min(abs(nc_lat - -13.7))
this_lon = which.min(abs(nc_lon - 142.7))


this_lat = which.min(abs(nc_lat - -34.5))
this_lon = which.min(abs(nc_lon - 138.7))
	
	# Perth
this_lat = which.min(abs(nc_lat - -32.3))
this_lon = which.min(abs(nc_lon - 115.8))

	# Canberra NSW
this_lat = which.min(abs(nc_lat - -35.5))
this_lon = which.min(abs(nc_lon - 149))

	# Mungo NSW
this_lat = which.min(abs(nc_lat - -34.0))
this_lon = which.min(abs(nc_lon - 143.2))

	# Enngonia NSW
this_lat = which.min(abs(nc_lat - -29.4))
this_lon = which.min(abs(nc_lon - 145.7))

	# Grafton NSW
this_lat = which.min(abs(nc_lat - -29.7))
this_lon = which.min(abs(nc_lon - 152.8))



	# reading ssp126 climate data 
ncname = 'watergap2-2c_gfdl-esm2m_ewembi_rcp60_2005soc_co2_groundwstor_global_monthly_2006_2099.nc4'#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_2005co2_thawdepth_global_annual_2006_2099.nc4"  
	# note that watergap treats gw as a relative value (depth below or above a relative datum) so the actual value can go negative (and theoretically can go to any real negative number)
ncfname = paste0(ncpath, ncname)
ncin = nc_open(ncfname)
nc_lat = ncvar_get(ncin, 'lat')#rev(ncvar_get(ncin, 'lat'))	# lat is given from high to low
nc_lon = ncvar_get(ncin, 'lon')
nc_date = as.Date("1661-01-01") + ncvar_get(ncin, 'time') * 30.4375# time is days after jan 1 2015
nc_years = unique(year(nc_date))

	#creating a database for storing all initial data
last_year = last(which(year(nc_date) == 2099))
first_year = which(year(nc_date) == 2010)[1]
nc_gwStor = data.frame(matrix(-999999, nrow=length(first_year:last_year), ncol=4))
names(nc_gwStor) = c("gfdl","hadgem2","ipsl","miroc5")

nc_gwStor$gfdl = ncvar_get(ncin,"groundwstor")[this_lon,this_lat,first_year:last_year]	# lon, lat, time

ncname = 'watergap2-2c_hadgem2-es_ewembi_rcp60_2005soc_co2_groundwstor_global_monthly_2006_2099.nc4'#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_2005co2_thawdepth_global_annual_2006_2099.nc4"  
	# note that watergap treats gw as a relative value (depth below or above a relative datum) so the actual value can go negative (and theoretically can go to any real negative number)
ncfname = paste0(ncpath, ncname)
ncin = nc_open(ncfname)
nc_lat = ncvar_get(ncin, 'lat')#rev(ncvar_get(ncin, 'lat'))	# lat is given from high to low
nc_lon = ncvar_get(ncin, 'lon')
nc_date = as.Date("1661-01-01") + ncvar_get(ncin, 'time') * 30.4375# time is days after jan 1 2015
nc_years = unique(year(nc_date))
nc_gwStor$hadgem2 = ncvar_get(ncin,"groundwstor")[this_lon,this_lat,first_year:last_year]	# lon, lat, time

ncname = 'watergap2-2c_ipsl-cm5a-lr_ewembi_rcp60_2005soc_co2_groundwstor_global_monthly_2006_2099.nc4'#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_2005co2_thawdepth_global_annual_2006_2099.nc4"  
	# note that watergap treats gw as a relative value (depth below or above a relative datum) so the actual value can go negative (and theoretically can go to any real negative number)
ncfname = paste0(ncpath, ncname)
ncin = nc_open(ncfname)
nc_lat = ncvar_get(ncin, 'lat')#rev(ncvar_get(ncin, 'lat'))	# lat is given from high to low
nc_lon = ncvar_get(ncin, 'lon')
nc_date = as.Date("1661-01-01") + ncvar_get(ncin, 'time') * 30.4375# time is days after jan 1 2015
nc_years = unique(year(nc_date))
nc_gwStor$ipsl = ncvar_get(ncin,"groundwstor")[this_lon,this_lat,first_year:last_year]	# lon, lat, time

ncname = 'watergap2-2c_miroc5_ewembi_rcp60_2005soc_co2_groundwstor_global_monthly_2006_2099.nc4'#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_2005co2_thawdepth_global_annual_2006_2099.nc4"  
	# note that watergap treats gw as a relative value (depth below or above a relative datum) so the actual value can go negative (and theoretically can go to any real negative number)
ncfname = paste0(ncpath, ncname)
ncin = nc_open(ncfname)
nc_lat = ncvar_get(ncin, 'lat')#rev(ncvar_get(ncin, 'lat'))	# lat is given from high to low
nc_lon = ncvar_get(ncin, 'lon')
nc_date = as.Date("1661-01-01") + ncvar_get(ncin, 'time') * 30.4375# time is days after jan 1 2015
nc_years = unique(year(nc_date))
nc_gwStor$miroc5 = ncvar_get(ncin,"groundwstor")[this_lon,this_lat,first_year:last_year] # lon, lat, time

nc_gwStor$year = rep(2010:2099, each=12)

gwStor_df = data.frame(matrix(NA, nrow=(length(2010:2099)*4), ncol=5))
names(gwStor_df) = c("year","minStor","maxStor","meanStor","model")
gwStor_df$year = rep(2010:2099, 4)
gwStor_df$decade = rep(rep(c("2010s","2020s",'2030s','2040s','2050s','2060s','2070s','2080s','2090s'), each=10), 4)#,'2050s'), each=10)
gwStor_df$model = rep(c("gfdl","hadgem2","ipsl","miroc5"), each=90)


iter = 0
for(this_model in unique(gwStor_df$model))	{
	this_gw = nc_gwStor[,which(names(nc_gwStor)==this_model)]
	
	for(kh in unique(gwStor_df$year))	{
		iter=iter+1
		this_year = which(nc_gwStor$year == kh)
		gwStor_df$minStor[iter] = min(this_gw[this_year])
		gwStor_df$maxStor[iter] = max(this_gw[this_year])
		gwStor_df$meanStor[iter] = mean(this_gw[this_year])
	}
}

p = ggplot(gwStor_df, aes(factor(decade),meanStor))
p + geom_boxplot()

plot(subset(gwStor_df,model=='hadgem2')$minStor)

#gwStor_df_data = c(100*(gwStor_df$minStor - median(subset(gwStor_df, decade == '2010s')$minStor)) /  median(subset(gwStor_df, decade == '2010s')$minStor),
#	100*(gwStor_df$meanStor - median(subset(gwStor_df, decade == '2010s')$meanStor)) / median(subset(gwStor_df, decade == '2010s')$meanStor),
#	100*(gwStor_df$maxStor - median(subset(gwStor_df, decade == '2010s')$maxStor)) / median(subset(gwStor_df, decade == '2010s')$maxStor))
gwStor_df_data = c(gwStor_df$minStor - median(subset(gwStor_df, decade == '2010s')$minStor),
	gwStor_df$meanStor - median(subset(gwStor_df, decade == '2010s')$meanStor),
	gwStor_df$maxStor - median(subset(gwStor_df, decade == '2010s')$maxStor))
box_plotter = data.frame(the_data = gwStor_df_data,
	the_decade = rep(gwStor_df$decade,3),
	the_range = rep(c("Annual Min","Annual Avg","Annual Max"), each=90*4))

ggplot(box_plotter, aes(the_range, the_data))				+
	geom_boxplot(aes(fill = the_decade), outlier.shape=NA)					+
	ylim(-30,20)												+
	scale_fill_viridis(discrete=TRUE, begin=0.3)			+
	geom_hline(yintercept = 0)								+
	labs(y="Change in Groundwater Availability (mm)",
		x="")												+													
	ggtitle(paste0("Data for: ",
		round(nc_lat[this_lat],1), ", ",
		round(nc_lon[this_lon],1))) +
	theme(plot.title = element_text(hjust = 0.5),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.background = element_blank())
 