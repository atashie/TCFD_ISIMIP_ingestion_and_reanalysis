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
ncPath = "J:\\Cai_data\\TCFD\\GWrecharge\\" 		# where are the relevant data stored?								GWstorage,        RootZoneSoilMoisture, GWrecharge   , SurfaceWater_Streamflow
ncVarFileName = 'qr'								# what is the var name in the .nc? 									groundwstor,      rootmoist           , qr           , dis
ncModel = 'clm45'									# what is the name of the model used to derive the var of interest?	watergap2-2c,     clm45,              , clm45        , matsiro
commonVarName = 'Recharge (mm)'						# user friendly name (with units) of the var of interest			GroundWater (mm), Soil Moisture (mm)  , Recharge (mm), Streamflow (mm)

locCsv = fread('J:\\Cai_data\\TCFD\\locations\\Advanta_locations_Toowoomba.csv')

locName = 'Perth'							# user defined location name
locLat = -32.3
locLon = 115.8
userName = 'Advanta'

	# these vars are unlikely to change between runs
theseYears = 2010:2059
theseModels = c("gfdl","hadgem2","ipsl","miroc5")
dataOutput = paste0('J:\\Cai_data\\TCFD\\', userName)

for(i in 1:nrow(locCsv))	{
	locName = locCsv$Location[i]
	locLat = locCsv$Lat[i]
	locLon = locCsv$Lon[i]
	for(rcpScen in c('rcp26', 'rcp60'))	{
		waterTrends_f(ncPath = ncPath,
			ncModel = ncModel,
			ncVarFileName = ncVarFileName,
			commonVarName = commonVarName,
			repScen = repScen,
			theseYears = theseYears,
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
	theseYears = theseYears,
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


		# reading ssp126 climate data 
	ncname = paste0(ncModel, '_gfdl-esm2m_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_2005co2_thawdepth_global_annual_2006_2099.nc4"  
		# note that watergap treats gw as a relative value (depth below or above a relative datum) so the actual value can go negative (and theoretically can go to any real negative number)
	ncin = nc_open(paste0(ncPath, ncname))

		#creating a database for storing all initial data
	nc_lat = ncvar_get(ncin, 'lat')#rev(ncvar_get(ncin, 'lat'))	# lat is given from high to low
	nc_lon = ncvar_get(ncin, 'lon')
	this_lat = which.min(abs(nc_lat - locLat))
	this_lon = which.min(abs(nc_lon - locLon))
	nc_date = as.Date("1661-01-01") + ncvar_get(ncin, 'time') * 30.4375# time is days after jan 1 2015
	nc_years = unique(year(nc_date))
	last_year = last(which(year(nc_date) == last(theseYears)))
	first_year = which(year(nc_date) == theseYears[1])[1]
	nc_varOut = data.table(year = rep(theseYears, each=12))

	scalar = 1
	#recharge is in mean mm / s, so convert to mm / month
	if(ncVarFileName == 'qr')	{
		scalar = 2628333
	}

	nc_varOut$gfdl = ncvar_get(ncin, ncVarFileName)[this_lon,this_lat,first_year:last_year]	* scalar# lon, lat, time

	ncname = paste0(ncModel, '_hadgem2-es_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	# note that watergap treats gw as a relative value (depth below or above a relative datum) so the actual value can go negative (and theoretically can go to any real negative number)
	ncin = nc_open(paste0(ncPath, ncname))
	nc_varOut$hadgem2 = ncvar_get(ncin,ncVarFileName)[this_lon,this_lat,first_year:last_year] * scalar	# lon, lat, time

	ncname = paste0(ncModel, '_ipsl-cm5a-lr_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	# note that watergap treats gw as a relative value (depth below or above a relative datum) so the actual value can go negative (and theoretically can go to any real negative number)
	ncin = nc_open(paste0(ncPath, ncname))
	nc_varOut$ipsl = ncvar_get(ncin,ncVarFileName)[this_lon,this_lat,first_year:last_year] * scalar	# lon, lat, time

	ncname = paste0(ncModel, '_miroc5_ewembi_', rcpScen, '_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')	# note that watergap treats gw as a relative value (depth below or above a relative datum) so the actual value can go negative (and theoretically can go to any real negative number)
	ncin = nc_open(paste0(ncPath, ncname))
	nc_varOut$miroc5 = ncvar_get(ncin,ncVarFileName)[this_lon,this_lat,first_year:last_year] * scalar # lon, lat, time
	names(nc_varOut) = c('year',theseModels)

	
	varOutYr_df = data.frame(year = rep(theseYears, length(theseModels)), 
		minVal = NA, maxVal = NA, meanVal = NA, sumVal = NA, rngVal = NA, varVal = NA)

	iter = 0
	for(thisModel in theseModels)	{
		thisCol = which(names(nc_varOut) == thisModel)
		modSubset = nc_varOut[ , c(1, ..thisCol)]
		for(thisYear in theseYears)	{
			iter = iter + 1
			thisSubset = subset(nc_varOut, year == thisYear)[ , -1]
			varOutYr_df[iter, -1] = c(
				min(thisSubset),
				max(thisSubset),
				mean(unlist(thisSubset)),
				sum(thisSubset),
				diff(range(thisSubset)),
				var(unlist(thisSubset)))
		}	
	}
	varOutYr_df$decade = varOutYr_df$year - varOutYr_df$year %% 10
	varOutYrAbs_df = varOutYr_df
	
	varOutYr_df$minVal = varOutYr_df$minVal - median(varOutYr_df$minVal[varOutYr_df$year %in% 2010:2019])
	varOutYr_df$maxVal = varOutYr_df$maxVal - median(varOutYr_df$maxVal[varOutYr_df$year %in% 2010:2019])
	varOutYr_df$meanVal = varOutYr_df$meanVal - median(varOutYr_df$meanVal[varOutYr_df$year %in% 2010:2019])
	varOutYr_df$sumVal = varOutYr_df$sumVal - median(varOutYr_df$sumVal[varOutYr_df$year %in% 2010:2019])
	varOutYr_df$rngVal = varOutYr_df$rngVal - median(varOutYr_df$rngVal[varOutYr_df$year %in% 2010:2019])
	varOutYr_df$varVal = varOutYr_df$varVal - median(varOutYr_df$varVal[varOutYr_df$year %in% 2010:2019])

	cor.test(varOutYr_df$year, varOutYr_df$minVal, method = 'kendall')$p.value
	cor.test(varOutYr_df$year, varOutYr_df$minVal, method = 'kendall')$estimate



	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_minVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(minVal ~ decade, data=varOutYr_df, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual Minimimum ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(max(abs(varOutYr_df$minVal)) * -1.1, max(abs(varOutYr_df$minVal)) * 1.1))
		axis(1, at=1:length(unique(varOutYr_df$decade)),
		labels = paste0(unique(varOutYr_df$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=0.6, y=max(abs(varOutYr_df$minVal)) * 1, "Change Relative to 2010s Average", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df$year, varOutYr_df$minVal, method = 'kendall')$p.value < 0.05)	{
			text(x=0.6, y=-max(abs(varOutYr_df$minVal)) * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df$year, varOutYr_df$minVal, method = 'kendall')$estimate < 0)	{
				text(x=3.22, y=-max(abs(varOutYr_df$minVal)) * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=3.22, y=-max(abs(varOutYr_df$minVal)) * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=0.6, y=-max(abs(varOutYr_df$minVal)) * 1.1, paste0("of ", signif(abs(lm(minVal ~ year, varOutYr_df)$coef[2]), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else	{
			text(x=0.6, y=-max(abs(varOutYr_df$minVal)) * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()



	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_maxVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(maxVal ~ decade, data=varOutYr_df, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual Maximum ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(max(abs(varOutYr_df$maxVal)) * -1.1, max(abs(varOutYr_df$maxVal)) * 1.1))
		axis(1, at=1:length(unique(varOutYr_df$decade)),
		labels = paste0(unique(varOutYr_df$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=0.6, y=max(abs(varOutYr_df$maxVal)) * 1, "Change Relative to 2010s Average", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df$year, varOutYr_df$maxVal, method = 'kendall')$p.value < 0.05)	{
			text(x=0.6, y=-max(abs(varOutYr_df$maxVal)) * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df$year, varOutYr_df$maxVal, method = 'kendall')$estimate < 0)	{
				text(x=3.22, y=-max(abs(varOutYr_df$maxVal)) * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=3.22, y=-max(abs(varOutYr_df$maxVal)) * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=0.6, y=-max(abs(varOutYr_df$maxVal)) * 1.1, paste0("of ", signif(abs(lm(maxVal ~ year, varOutYr_df)$coef[2]), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else	{
			text(x=0.6, y=-max(abs(varOutYr_df$maxVal)) * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()



	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_meanVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(meanVal ~ decade, data=varOutYr_df, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual Average ' , commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(max(abs(varOutYr_df$meanVal)) * -1.1, max(abs(varOutYr_df$meanVal)) * 1.1))
		axis(1, at=1:length(unique(varOutYr_df$decade)),
		labels = paste0(unique(varOutYr_df$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=0.6, y=max(abs(varOutYr_df$meanVal)) * 1, "Change Relative to 2010s Average", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df$year, varOutYr_df$meanVal, method = 'kendall')$p.value < 0.05)	{
			text(x=0.6, y=-max(abs(varOutYr_df$meanVal)) * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df$year, varOutYr_df$meanVal, method = 'kendall')$estimate < 0)	{
				text(x=3.22, y=-max(abs(varOutYr_df$meanVal)) * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=3.22, y=-max(abs(varOutYr_df$meanVal)) * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=0.6, y=-max(abs(varOutYr_df$meanVal)) * 1.1, paste0("of ", signif(abs(lm(meanVal ~ year, varOutYr_df)$coef[2]), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=0.6, y=-max(abs(varOutYr_df$meanVal)) * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()



	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_sumVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(sumVal ~ decade, data=varOutYr_df, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual Total ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(max(abs(varOutYr_df$sumVal)) * -1.1, max(abs(varOutYr_df$sumVal)) * 1.1))
		axis(1, at=1:length(unique(varOutYr_df$decade)),
		labels = paste0(unique(varOutYr_df$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=0.6, y=max(abs(varOutYr_df$sumVal)) * 1, "Change Relative to 2010s Average", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df$year, varOutYr_df$sumVal, method = 'kendall')$p.value < 0.05)	{
			text(x=0.6, y=-max(abs(varOutYr_df$sumVal)) * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df$year, varOutYr_df$sumVal, method = 'kendall')$estimate < 0)	{
				text(x=3.22, y=-max(abs(varOutYr_df$sumVal)) * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=3.22, y=-max(abs(varOutYr_df$sumVal)) * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=0.6, y=-max(abs(varOutYr_df$sumVal)) * 1.1, paste0("of ", signif(abs(lm(sumVal ~ year, varOutYr_df)$coef[2]), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=0.6, y=-max(abs(varOutYr_df$sumVal)) * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()

	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_rngVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(rngVal ~ decade, data=varOutYr_df, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual Range in ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(max(abs(varOutYr_df$rngVal)) * -1.1, max(abs(varOutYr_df$rngVal)) * 1.1))
		axis(1, at=1:length(unique(varOutYr_df$decade)),
		labels = paste0(unique(varOutYr_df$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=0.6, y=max(abs(varOutYr_df$rngVal)) * 1, "Change Relative to 2010s Average", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df$year, varOutYr_df$rngVal, method = 'kendall')$p.value < 0.05)	{
			text(x=0.6, y=-max(abs(varOutYr_df$rngVal)) * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df$year, varOutYr_df$rngVal, method = 'kendall')$estimate < 0)	{
				text(x=3.22, y=-max(abs(varOutYr_df$rngVal)) * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=3.22, y=-max(abs(varOutYr_df$rngVal)) * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=0.6, y=-max(abs(varOutYr_df$rngVal)) * 1.1, paste0("of ", signif(abs(lm(rngVal ~ year, varOutYr_df)$coef[2]), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=0.6, y=-max(abs(varOutYr_df$rngVal)) * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()




	png(paste0(dataOutput, '\\', locName, '_', rcpScen, '_', ncVarFileName, '_varVal.png'), width = 720, height = 720)
	windowsFonts(A = windowsFont("Roboto"))
	par(mar=1.6*c(5,5,2,2), mgp=1.5*c(3,1.3,0), font.lab=2, bty='l', cex.lab=1.4*1.8, cex.axis=1.4*1.4, cex.main=1.4*1.8, col='#1A232F')
	boxplot(varVal ~ decade, data=varOutYr_df, pch=1, lwd=1, col='#FDB600', border='#666D74', cex=1.5, 
				main='', ylab=paste0('Annual Variance in ', commonVarName), xlab='Decade', xaxt='n',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A', 
				ylim = c(max(abs(varOutYr_df$varVal)) * -1.1, max(abs(varOutYr_df$varVal)) * 1.1))
		axis(1, at=1:length(unique(varOutYr_df$decade)),
		labels = paste0(unique(varOutYr_df$decade), 's'),
				col.lab='#1A232F', col.axis='#666D74')
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=0.6, y=max(abs(varOutYr_df$varVal)) * 1, "Change Relative to 2010s Average", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		if(cor.test(varOutYr_df$year, varOutYr_df$varVal, method = 'kendall')$p.value < 0.05)	{
			text(x=0.6, y=-max(abs(varOutYr_df$varVal)) * 1, "Statistically significant ", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
			if(cor.test(varOutYr_df$year, varOutYr_df$varVal, method = 'kendall')$estimate < 0)	{
				text(x=3.22, y=-max(abs(varOutYr_df$varVal)) * 1, "decrease ", adj = c(0,0), font=2, col='#F06000', family='A', cex=1.4*1.4)
			} else { 
				text(x=3.22, y=-max(abs(varOutYr_df$varVal)) * 1, "increase ", adj = c(0,0), font=2, col='#0098B2', family='A', cex=1.4*1.4)
			}
			text(x=0.6, y=-max(abs(varOutYr_df$varVal)) * 1.1, paste0("of ", signif(abs(lm(varVal ~ year, varOutYr_df)$coef[2]), 2) * 10, 'mm per decade'),
				adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	else{
			text(x=0.6, y=-max(abs(varOutYr_df$varVal)) * 1, "No statistically significant trend", adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.4*1.4)
		}	
	dev.off()

	lookerDF = data.frame(User = character(), Lat = numeric, Lon = numeric(), Scenario = character(), Decade = numeric(), Variable = character(), Metric = character(),
		Significance = numeric, Trend_Strength = numeric, Trend_Units = character(),
		Q05 = numeric(), Q25 = numeric(), Q50 = numeric(), Q75 = numeric(), Q95 = numeric()) 
	
	theseDecades = seq(theseYears[1], by = 10, length.out = length(theseYears) / 10)
	for(thisDecade in theseDecades)	{
		subsetYrs = subset(varOutYr_df, decade == thisDecade)
		subsetYrsAbs = subset(varOutYrAbs_df, decade == thisDecade)
		
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Minimum', 
			signif(cor.test(varOutYr_df$year, varOutYr_df$minVal, method = 'kendall')$p.value, 3), signif(lm(minVal ~ year, varOutYr_df)$coef[2], 2) * 10, 'mm per decade',
			signif(quantile(subsetYrs$minVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$minVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Maximum', 
			signif(cor.test(varOutYr_df$year, varOutYr_df$maxVal, method = 'kendall')$p.value, 3), signif(lm(maxVal ~ year, varOutYr_df)$coef[2], 2) * 10, 'mm per decade',
			signif(quantile(subsetYrs$maxVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$maxVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Average', 
			signif(cor.test(varOutYr_df$year, varOutYr_df$meanVal, method = 'kendall')$p.value, 3), signif(lm(meanVal ~ year, varOutYr_df)$coef[2], 2) * 10, 'mm per decade',
			signif(quantile(subsetYrs$meanVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$meanVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Total (sum)', 
			signif(cor.test(varOutYr_df$year, varOutYr_df$sumVal, method = 'kendall')$p.value, 3), signif(lm(sumVal ~ year, varOutYr_df)$coef[2], 2) * 10, 'mm per decade',
			signif(quantile(subsetYrs$sumVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$sumVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Range', 
			signif(cor.test(varOutYr_df$year, varOutYr_df$rngVal, method = 'kendall')$p.value, 3), signif(lm(rngVal ~ year, varOutYr_df)$coef[2], 2) * 10, 'mm per decade',
			signif(quantile(subsetYrs$rngVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$rngVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
		lookerDF = rbind(lookerDF, c(userName, locLat, locLon, rcpScen, thisDecade, commonVarName, 'Annual Variance', 
			signif(cor.test(varOutYr_df$year, varOutYr_df$varVal, method = 'kendall')$p.value, 3), signif(lm(varVal ~ year, varOutYr_df)$coef[2], 2) * 10, 'mm per decade',
			signif(quantile(subsetYrs$varVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4),
			signif(quantile(subsetYrsAbs$varVal, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4)))
	}
	names(lookerDF) = c('User', 'Lat', 'Lon', 'Scenario', 'Decade', 'Variable', 'Metric', 'Significance', 'Trend_Strength', 'Trend_Units',
		'Q05_rel', 'Q25_rel', 'Q50_rel', 'Q75_rel', 'Q95_rel',
		'Q05_abs', 'Q25_abs', 'Q50_abs', 'Q75_abs', 'Q95_abs')

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

















cor.test(varOutYr_df$year, varOutYr_df$minVal, method = 'kendall')$estimate
		abline(h=0, lwd=2, col='#1A232F', cex=2)
		text(x=1, y=180,
			paste0(basinName),
			adj = c(0,0), font=2, col='#1A232F', family='A', cex=1.6*1.9)
		dev.off()














iter = 0
for(thisDecade in varOutDc_df$decade)	{
	iter = iter + 1
	thisSubset = subset(nc_varOut, year %in% thisDecade:(thisDecade+9))[ , -1]
	varOutDc_df[iter, -1] = c(
		min(thisSubset),
		max(thisSubset),
		mean(unlist(thisSubset)),
		sum(thisSubset),
		diff(range(thisSubset)),
		var(unlist(thisSubset)))
}	
	

lookerDF = data.frame(Variable = ncVarFileName, Metric = 'metric name', Q05 = numeric(), Q25 = numeric(), Q50 = numeric(), Q75 = numeric(), Q95 = numeric(), P_Value = numeric())

	
	
varOut_melt = melt(nc_varOut, id.vars = 'year', measure.vars=c("gfdl","hadgem2","ipsl","miroc5"))
varOut_melt$decade = varOut_melt$year - varOut_melt$year %% 10
	
	
p = ggplot(varOut_melt, aes(factor(decade),value))
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
	the_range = rep(c("Annual Min","Annual Avg","Annual Max"), each=50*4))

ggplot(box_plotter, aes(the_range, the_data))				+
	geom_boxplot(aes(fill = the_decade), outlier.shape=NA)					+
	ylim(-25,25)												+
	scale_fill_viridis(discrete=TRUE, begin=0.3)			+
	geom_hline(yintercept = 0)								+
	labs(y="Change in Groundwater (mm)",
		x="")												+													
	ggtitle(paste0("Data for: ",
		round(nc_lat[this_lat],1), ", ",
		round(nc_lon[this_lon],1))) +
	theme(plot.title = element_text(hjust = 0.5),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.background = element_blank())
 
 
 
 
 
 https://github.com/ClimateAI/DashboardAPIController/wiki/Database
 
 
 
 cloud_sql_proxy.exe -instances="[MyInstanceConnectionName]"=tcp:3306


C:/users/desktop>.\cloud_sql_proxy.exe -instances="[YOUR_INSTANCE_CONNECTION_NAME]"=tcp:3306
J:/SQL>.\cloud_sql_proxy.exe -instances="[YOUR_INSTANCE_CONNECTION_NAME]"=tcp:3306



Jp  9:19 AM
For points 1 & 2 you need to contact the eng team. Edgar or MartÃ­n
 
Chicago, Illinois
Columbus, Ohio
Indianapolis, Indiana
Detroit, Michigan
Milwaukee, Wisconsin


j:
cd j:\SQL
/SQL
## SET UP CREDENTIALS
set GOOGLE_APPLICATION_CREDENTIALS=/j:/SQL/climate-ai-cloud-sql.json

set DB_USER="dashboard-db-user"
set DB_PASS="Ng*Ldpj0E^bj2T%j*YoY" # this is the database password
set DB_NAME="looker_poc"

## START THE CLOUD SQL PROXY
cloud_sql_proxy -instances=climate-ai:us-central1:dashboard-db=tcp:5432
 
.\cloud_sql_proxy.exe -instances=climate-ai:us-central1:dashboard-db=tcp:3306
j:\SQL\.\cloud_sql_proxy.exe -instances=climate-ai:us-central1:dashboard-db=tcp:5432
 
 
export GOOGLE_APPLICATION_CREDENTIALS=/Users/jp/Documents/ClimateAi/Service_accounts/develop/climate-ai-cloud-sql.json
export DB_USER=âdashboard-db-userâ
export DB_PASS=âNg*Ldpj0E^bj2T%j*YoYâ
export DB_NAME=âlooker_pocâ
/Users/jp/Documents/ClimateAi/Service_accounts/develop/cloud_sql_proxy -instances=climate-ai:us-central1:dashboard-db=tcp:5432
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
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
 