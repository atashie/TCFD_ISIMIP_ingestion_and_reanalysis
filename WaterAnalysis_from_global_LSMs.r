###################################################
library(data.table)
library(ncdf4)
library(ggplot2)
library(viridis)


#########################################
# reading in climai netcdf data
ncOutputPath = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'

# reading in customer data
userName = 'ITC'	
hazardFolder = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
customerFolder = 'J:\\Cai_data\\Rabo\\Locations\\'
clientName = 'BeefNW'
thisDate = Sys.Date()

customerTable = fread(paste0(customerFolder, 'Customer_Hazards_and_Locations-Rabobank_grid - Sheet1.csv')) #'HMClause_locations_allCucurbit.csv'


##########################################################################################################################################################################################

	# part 1: geospatial figures

##########################################################################################################################################################################################
library(rnaturalearth)
#library(maps)
library(ggfx)
library(stars)	# for st_rasterize
library(sf)

ocean50 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_ocean\\ne_10m_ocean.shp'))
world <- ne_countries(scale = "medium", returnclass = "sf")
#countries10 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_admin_0_countries\\ne_10m_admin_0_countries.shp'))
provinces10 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_admin_1_states_provinces\\ne_10m_admin_1_states_provinces.shp'))

latlonBox = c(-55, 75, -180, 180) #(minlat, maxlat, minlon, maxlon)
latlonBox_local = c(-57,-16,-77,-64)# chile;  c(41,50,-128,-110)#pacnw

locOfInt = st_point(c(customerTable$Lat[1], customerTable$Lon[1]))


##########################################################################################################################################################################################
##########################################################################################################################################################################################
# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed
	# aridityIndex_ssp_v2_processed
	# pwpExceedance_ssp_v2_processed
	# aridityIndex_transientInterannual_ssp_v2_processed
		# aridityIndex_transientSeasonal_ssp_v2_processed
	# pwpExceedance_transientInterannual_ssp_v2_processed
		# pwpExceedance_transientSeasonal_ssp_v2_processed
	# aridityIndex_transientInterannualRegionalAvgSupplement_ssp_v2_processed
		# aridityIndex_transientSeasonalRegionalAvgSupplement_ssp_v2_processed
	# plantWaterDemand_transientInterannualRegionalAvgSupplement_ssp_v2_processed
		# plantWaterDemand_transientSeasonalRegionalAvgSupplement_ssp_v2_processed
	# aridityIndex_transientInterannualRegionalTransientInterannualSupplement_ssp_v2_processed
		# aridityIndex_transientSeasonalRegionalTransientSeasonalSupplement_ssp_v2_processed
	# plantWaterDemand_transientInterannualRegionalTransientInterannualSupplement_ssp_v2_processed
		# plantWaterDemand_transientSeasonalRegionalTransientSeasonalSupplement_ssp_v2_processed

# regional and global maps

myTitle = 'AI'
#mySubtitle = 'nonirrigated'
varName = 'aridityIndex_ssp_v2_processed'


# 1b: country or county avg
sf_use_s2(FALSE)

#thisCountry = subset(world, sovereignt == 'Chile')
#otherWorld = subset(world, sovereignt != 'Chile')

thisState = subset(provinces10, name %in% c("Idaho", "Washington", "Oregon"))
otherWorld = subset(provinces10, !(name %in% c("Idaho", "Washington", "Oregon")))


nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_proj = st_as_sf(geomData, coords = c('lon', 'lat'), crs = 4326)
#thisCRS = 4087#4326
#df_proj = st_transform(df_proj, crs = st_crs(thisCountry))
geomIntersects = st_intersection(df_proj, thisState)
#geomSub = subset(geomIntersects, !is.na(plotData))
#geomIntersects = st_intersects(df_proj, thisCountry)
rasterIntersects = as.data.frame(st_rasterize(geomIntersects))

#rasterIntersects$plotData[rasterIntersects$plotData > 1] = 1
rasterIntersects$plotData[rasterIntersects$plotData < 1] = NA
#rasterIntersects$plotData[rasterIntersects$initData < 1] = NA

ggplot(data = rasterIntersects) +
#	geom_sf(data = geomIntersects, size=6.4, shape=15, aes(fill=plotData, color=plotData),  linewidth = 0.0001) +
	with_blur(geom_raster(aes(y = y, x = x, fill = plotData), interpolate = FALSE), sigma = 9) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'inferno', trans = scales::pseudo_log_trans(sigma = 0.5), na.value = 'grey40')	+
	geom_sf(data = otherWorld, color = 'grey20', fill = 'grey40', linewidth =1) +
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey50", colour='grey20')+
	borders('world', xlim=latlonBox_local[3:4], ylim=latlonBox_local[1:2], 
            colour='gray50', size=.05)	+
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
	geom_sf(data = thisState, color = 'grey10', fill = NA, linewidth =1) +
#	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
#		subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='') +
	theme(legend.position = 'bottom')
ggsave(paste0(customerFolder, varName, ".png"), width = 12, height = 9)





geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 5] - nc_testDat[ , , 1, 2, 1]), initData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_proj = st_as_sf(geomData, coords = c('lon', 'lat'), crs = 4326)
#thisCRS = 4087#4326
#df_proj = st_transform(df_proj, crs = st_crs(thisCountry))
geomIntersects = st_intersection(df_proj, thisState)
#geomSub = subset(geomIntersects, !is.na(plotData))
#geomIntersects = st_intersects(df_proj, thisCountry)
rasterIntersects = as.data.frame(st_rasterize(geomIntersects))

#rasterIntersects$plotData[rasterIntersects$plotData > 1] = 1
#rasterIntersects$plotData[rasterIntersects$plotData < 1] = NA
rasterIntersects$plotData[rasterIntersects$initData < 1] = NA




trendsTitle = paste0(myTitle, ' trend')
lmt <- max(abs(range(rasterIntersects$plotData, na.rm=TRUE)))
ggplot(data = rasterIntersects) +
	with_blur(geom_raster(aes(y = y, x = x, fill = plotData), interpolate = FALSE), sigma = 9) +
	scale_fill_gradient2(low = 'darkred', high = 'darkgreen', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .01), limits = c(-lmt, lmt))	+
	geom_sf(data = otherWorld, color = 'grey20', fill = 'grey40', linewidth =1) +
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey50", colour='grey20')+
	borders('world', xlim=latlonBox_local[3:4], ylim=latlonBox_local[1:2], 
            colour='gray50', size=.05)	+
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
	geom_sf(data = thisState, color = 'grey10', fill = NA, linewidth =1) +
#	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
#		subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='') +
	theme(legend.position = 'bottom')
ggsave(paste0(customerFolder, varName, "_trends.png"), width = 12, height = 9)
















##########################################################################################################################################################################################

	# part 2: time series

##########################################################################################################################################################################################
# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed
	# aridityIndex_ssp_v2_processed
	# pwpExceedance_ssp_v2_processed
	# aridityIndex_transientInterannual_ssp_v2_processed
		# aridityIndex_transientSeasonal_ssp_v2_processed
	# pwpExceedance_transientInterannual_ssp_v2_processed
		# pwpExceedance_transientSeasonal_ssp_v2_processed
	# aridityIndex_transientInterannualRegionalAvgSupplement_ssp_v2_processed
		# aridityIndex_transientSeasonalRegionalAvgSupplement_ssp_v2_processed
	# plantWaterDemand_transientInterannualRegionalAvgSupplement_ssp_v2_processed
		# plantWaterDemand_transientSeasonalRegionalAvgSupplement_ssp_v2_processed
	# aridityIndex_transientInterannualRegionalTransientInterannualSupplement_ssp_v2_processed
		# aridityIndex_transientSeasonalRegionalTransientSeasonalSupplement_ssp_v2_processed
	# plantWaterDemand_transientInterannualRegionalTransientInterannualSupplement_ssp_v2_processed
		# plantWaterDemand_transientSeasonalRegionalTransientSeasonalSupplement_ssp_v2_processed

varName = 'aridityIndex_transientInterannualRegionalAvgSupplement_ssp_v2_processed'

nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
theYlab = 'AI [-]'



png(paste0(customerFolder, varName,"_gridsearchPlots.png"), width=900, height=900)
par(mar=2*c(1.2,3,1,1), mgp=2*c(1.5,.6,0), mfrow=c(3,3), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
for(thisLoc in 1:nrow(customerTable))	{
	thisLat = which.min(abs(nc_lat - customerTable$Lat[thisLoc]))
	thisLon = which.min(abs(nc_lon - customerTable$Lon[thisLoc]))
	#thisTestDat = nc_testDat[thisLon, thisLat, , , ]



	windowsFonts(A = windowsFont("Roboto"))
	plot(theDecades, nc_testDat[thisLon, thisLat, , 2, 1], ylim = c(0,  max(max(nc_testDat[thisLon, thisLat, , , 1])*1.025, 1)),
			type='l', lwd=1, col='white', xaxt = 'n', #log='y',
			main='', ylab=theYlab, xlab='',
			col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
			family='A')
		abline(h=1, lwd=2, lty =2, col='#1A232F')
		axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
			labels = theDecades)
	#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	#	polygon(x=c(theDecades, rev(theDecades)), y=c(nc_testDat[thisLon, thisLat, , 1, 3], rev(nc_testDat[thisLon, thisLat, , 1, 1])),
	#		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
		lines(theDecades, nc_testDat[thisLon, thisLat, , 2, 1], 
			col='#54575a', lwd=5)	#4cbfad
		lines(theDecades, nc_testDat[thisLon, thisLat, , 3, 1], 
			col='#EE6222', lwd=3)
		lines(theDecades, nc_testDat[thisLon, thisLat, , 1, 1], 
			col='#4cbfad', lwd=3) #015f6f

}
dev.off()












##########################################################################################################################################################################################
##########################################################################################################################################################################################

	# part 3: bespoke time series

##########################################################################################################################################################################################
##########################################################################################################################################################################################

# variable options
climVars = c('Precipitation_decadalRawVals', 'PotentialEvapotranspiration_decadalRawVals', 'GroundwaterRecharge_decadalRawVals', 'Streamflow_decadalRawVals',
	'RootZoneSoilMoisture_decadalRawVals', 'TotalWaterStorage_decadalRawVals')
ncFileLoc = 'J:\\Cai_data\\WaterIndex\\'
rawDataColumnNames = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Mean', 'Q05', 'Q15', 'Q25', 'Q50', 'Q75', 'Q85', 'Q95')

# reading in customer data
userName = 'Rabo'	
customerFolder = 'J:\\Cai_data\\Rabo\\Locations\\'
clientName = 'BeefNW'
thisDate = Sys.Date()

customerTable = data.table::fread(paste0(customerFolder, clientName, '\\', 'Customer Onboarding Information_BNW.csv'), 
	skip = 1) #'Customer_Hazards_and_Locations-Rabobank_grid - Sheet1.csv'
locationHeader = 'Location (name)'


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
divertibleStrmfl = 0.1
hectToSqKm = 100
frcAreaUnderCult = 0.4
halfDegInKM = 111.1 / 2
kmToMm = 1000^2


for(thisRow in 1:nrow(customerTable))	{
	petGlobAvgForGrowSeason = petGlobAvg * (length(growSeason) / 12)
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
	effectiveStrmfl = (divertibleStrmfl / frcAreaUnderCult) * (strmflQntsAvg / gridArea) * kmToMm 
	effectiveStrmflDrght = (divertibleStrmfl / frcAreaUnderCult) * (strmflQntsDrght / gridArea) * kmToMm 
		
		# Aridity Index - Avg
	indexValuesArray[thisRow, , , , 1, 1] = (pptQntsAvg / petQntsAvg) / humidAI	
	indexValuesArray[thisRow, , , , 1, 2] = (pptQntsAvg - petQntsAvg * humidAI)
		# Soil Moisture Stress - Avg
	indexValuesArray[thisRow, , , , 2, 1] = rzsmQntsAvg / pwpSoil
	indexValuesArray[thisRow, , , , 2, 2] = rzsmQntsAvg - pwpSoil
		# Plant Water Demand - Avg
	growSeasonPPTqntsAvg = sqrt(pptQntsNrml) * climateData[thisRow, , rep(growSeason[1], 7), , 1]	# taking the sqrt since the var will be multiplied (and e.g. Q05*Q05 --> Q0025)
	growSeasonPETqntsAvgRatio = sqrt(petQntsNrml) * climateData[thisRow, , rep(growSeason[1],7), , 2] / petGlobAvgForGrowSeason
	for(thisMonth in growSeason[-1])	{
		growSeasonPPTqntsAvg = growSeasonPPTqntsAvg + sqrt(pptQntsNrml) * climateData[thisRow, , rep(thisMonth, 7), , 1]
		growSeasonPETqntsAvgRatio = growSeasonPETqntsAvgRatio + sqrt(petQntsNrml) * climateData[thisRow, , rep(thisMonth, 7), , 2] / petGlobAvgForGrowSeason
	}
	effectivePPT = (1 - runoffRatio) * growSeasonPPTqntsAvg
	effectiveWPlant = sqrt(growSeasonPETqntsAvgRatio) * mean(wPlant)
	indexValuesArray[thisRow, , , , 3, 1] = effectivePPT / effectiveWPlant
	indexValuesArray[thisRow, , , , 3, 2] = effectivePPT - effectiveWPlant
		
		# Aridity Index - Drought
	indexValuesArray[thisRow, , , , 4, 1] = (pptQntsDrght / petQntsAvg) / humidAI
	indexValuesArray[thisRow, , , , 4, 2] = pptQntsDrght - petQntsAvg * humidAI
		# Soil Moisture Stress - Drought
	indexValuesArray[thisRow, , , , 5, 1] = rzsmQntsDrght / pwpSoil
	indexValuesArray[thisRow, , , , 5, 2] = rzsmQntsDrght- pwpSoil
		# Plant Water Demand - Drought
	growSeasonPPTqntsdrought = sqrt(pptQntsDrghtShft) * climateData[thisRow, , rep(growSeason[1], 7), , 1]	
	for(thisMonth in growSeason[-1])	{
		growSeasonPPTqntsdrought = growSeasonPPTqntsdrought + sqrt(pptQntsDrghtShft) * climateData[thisRow, , rep(thisMonth, 7), , 1]	
	}
	effectivePPTdrought = (1 - runoffRatio) * growSeasonPPTqntsdrought
	indexValuesArray[thisRow, , , , 6, 1] = effectivePPTdrought / effectiveWPlant
	indexValuesArray[thisRow, , , , 6, 2] = effectivePPTdrought - effectiveWPlant
	
		# Aridity Index w/ Irrigation - Avg
	indexValuesArray[thisRow, , , , 7, 1] = ((pptQntsDrght + rechAvg + effectiveStrmfl) / petQntsAvg) / humidAI
	indexValuesArray[thisRow, , , , 7, 2] = (pptQntsDrght + rechAvg + effectiveStrmfl) - petQntsAvg * humidAI
		# Plant Water Demand w/ Irrigation - Avg'
	indexValuesArray[thisRow, , , , 8, 1] = (effectivePPTdrought + rechAvg + effectiveStrmfl) / effectiveWPlant
	indexValuesArray[thisRow, , , , 8, 2] = (effectivePPTdrought + rechAvg + effectiveStrmfl) - effectiveWPlant

		# Aridity Index w/ Irrigation - Drought
	indexValuesArray[thisRow, , , , 9, 1] = ((pptQntsDrght + rechAvg + effectiveStrmflDrght) / petQntsAvg) / humidAI
	indexValuesArray[thisRow, , , , 9, 2] = (pptQntsDrght + rechAvg + effectiveStrmflDrght) - petQntsAvg * humidAI
		# Plant Water Demand w/ Irrigation - Drought
	indexValuesArray[thisRow, , , , 10, 1] = (effectivePPTdrought + rechAvg + effectiveStrmflDrght) / effectiveWPlant
	indexValuesArray[thisRow, , , , 10, 2] = (effectivePPTdrought + rechAvg + effectiveStrmflDrght) - effectiveWPlant
}



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





#########################################
#########################################
###### !!!!!!!!!todo todo todo
###### !!!!!!!!!todo todo todo
###### !!!!!!!!!todo todo todo
#########################################
#########################################

##### incorporating USGS data
library(dataRetrieval)	# is pulling from this webservice; need to revisit and hard code

# defining array for holding climate data
USGSDataTable = data.table(Location = NA, Lat = NA, Lon = NA, LWE_Depth_Median = NA, LWE_Depth_SD = NA, Date = as.Date(NA)) 

startDate = "1970-01-01"
endDate = "2023-04-01"
thisParameterCd = '72019' # gw;  '00060',# discharge; c("00010", "00060") # Temperature and discharge
statCd <- c("00001")#, "00003") # Mean and maximum

for(thisLoc in 1:nrow(customerTable))	{
	sites = whatNWISsites(
		bBox = c(round(customerTable$Lon[thisLoc] - 1, 3), round(customerTable$Lat[thisLoc] - 1, 3), round(customerTable$Lon[thisLoc] + 1, 3), round(customerTable$Lat[thisLoc] + 1, 3)),
		parameterCd = thisParameterCd, # gw level '00060',# discharge https://help.waterdata.usgs.gov/parameter_cd?group_cd=PHY for all'30210', #
		hasDataTypeCd = 'dv')

	for(thisSite in 1:nrow(sites))	{
		thisSiteNumber = sites$site_no[thisSite]
		parameterCd <- c("00010", "00060") # Temperature and discharge

		siteData = readNWISdv(siteNumber = thisSiteNumber, thisParameterCd,
			startDate, endDate , statCd = statCd
		)
		plot(siteData$Date, siteData[,4])
	}
}

















































#######!!!!!!!!!!!!!!!!!!!!!!!!!
	##### todo #######
#######!!!!!!!!!!!!!!!!!!!!!!!!!

# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
#df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
geomJoin = st_join(world, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2

myTitle = 'Soy Yield - 2020s'
mySubtitle = 'nonirrigated'
ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.5))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
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
	coord_sf(xlim = latlonBox[c(3,4)], ylim = latlonBox[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit_c.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 5, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
#df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
geomJoin = st_join(world, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(low = 'darkred', high = 'darkgreen', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .01), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox[c(3,4)], ylim = latlonBox[c(1,2)] + c(-1, -1), expand = FALSE) +
#	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit_c.png"), width = 20, height = 8)













# part 2: time series
#thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
#thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
#thisTestDat = nc_testDat[thisLon, thisLat, , , ]

# abs values
whichGeoUnit = 'Argentina'
geoUnitVals = matrix(nrow=9, ncol=4)
thisScen = 1
for(i in 1:9)	{
	for(ii in 1:4)	{
		j = c(1,3,5,6)[ii]
		geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , i, thisScen, j]))
		df_sub = subset(geomData, lat >= latlonBox[1] & lat <= latlonBox[2] & lon >= latlonBox[3] & lon <= latlonBox[4])
		geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

			# provinces
	#	geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
	#	geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
	#	geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
	#	missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
	#	geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
	#	geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

			# countries
		geomJoin = st_join(world, geomData_sf, join = st_intersects)
		geomSub = subset(geomJoin, sovereignt == whichGeoUnit)
		geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomSub)#, na.action = na.pass)
		geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
		missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
		geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
		geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)
		geoUnitVals[i,ii] = geomAvgMrg$plotData[1]
	}
}



theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = 'kg/ha/yr'

png(paste0(customerFolder, varName, theScenarios[thisScen], "_avgYield.png"), width=800, height=400)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, geoUnitVals[ , 1], ylim = c(0, max(geoUnitVals[ , 4])*1.025),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Yield (kg/ha/yr)', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(geoUnitVals[ , 3], rev(geoUnitVals[ , 4])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, geoUnitVals[ , 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(geoUnitVals[ , 4])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
#	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
#		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
#			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
#			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
#	} 	else {
#		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
#			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
#				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
#				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
#		}	else	{
#			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
#				paste0('Trend: No Significant Trend'),
#				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
#			}
#	}
	dev.off()





















































##########################################################################################################################################################################################
##########################################################################################################################################################################################
# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'WRI_based_WaterStress_v2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')



###############
# P1: maps
# 1a: gridded
myTitle = 'Water Stress'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.0005))	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .00001), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.0005))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .00001), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()

































##########################################################################################################################################################################################
##########################################################################################################################################################################################
# variable options:
	# WRI_based_WaterStress_v2_processed; WRI_based_WaterDepletion_v2_processed; qrv2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'WRI_based_WaterDepletion_v2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Water Depletion'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.0001))	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .000001), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.0001))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .000001), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()








































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed; dis_WRIbasedDroughtSeverityv2_processed

varName = 'dis_WRIbasedDroughtSeverityv2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Drought Intensity'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = .5))	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .01), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = .5))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.01) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .00001), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()













































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'WRI_basedStreamflowInterannualVariability_v2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Interannual Variability'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.2))	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .001), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.2) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.2))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.005) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .001), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()















































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'WRI_basedStreamflowSeasonalVariability_v2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Seasonal Variability'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.2))	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .001), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.2))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .001), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()








































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'disv2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Streamflow'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = .00001), direction = -1)	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .0001), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.00001), direction = -1)	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = .0001), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()
















































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'twsv2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Total Water Storage'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = .0001), direction = -1)	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 10), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 0.0001), direction = -1)	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 10), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()












































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'qrv2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Groundwater Rechage'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 1), direction = -1)	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 3.0), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 1), direction = -1)	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 3.0), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()










































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed

varName = 'rootmoistv2_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Root Zone Soil Moisture'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = df_sub) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = 2.5) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 2), direction = -1)	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = df_sub) +
	with_blur(geom_raster(data = df_sub, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = 1.75) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 10), limits = c(-lmt, lmt))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray10', size=.2)	+
	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 2), direction = -1)	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	scale_fill_gradient(low = 'skyblue', high = 'darkred', trans = scales::pseudo_log_trans(sigma = .00005))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
#geomJoin = st_join(world, geomData_sf, join = st_intersects)
#geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
#geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
#missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
#geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
#geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 10), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim = latlonBox_local[c(3,4)], ylim = latlonBox_local[c(1,2)] + c(-1, -1), expand = FALSE) +
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()























































# variable options:
	# WRI_based_WaterDepletion_v2_processed; qrv2_processed; WRI_based_WaterDepletion_v2_processed; cwood-evgndltrv2_processed; disv2_processed; WRI_basedStreamflowInterannualVariability_v2_processed; rootmoistv2_processed

varName = 'cwood-evgndltr_processed'
nc_close(myNC)
myNC = nc_open(paste0(ncOutputPath, varName, '.nc'))
nc_lat = ncvar_get(myNC, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(myNC, 'lon')
nc_testDat = ncvar_get(myNC, 'tcfdVariable')


###############
# P1: maps
# 1a: gridded
myTitle = 'Carbon Mass in Wood (evgndltr)'
mySubtitle = NA#'nonirrigated'
thisCRS = 4087#4326

#geomData = st_as_sf(data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1])), coords = c('lon', 'lat'))
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 1, 2, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])


df_proj = st_as_sf(geomData, coords = c('lon', 'lat'), crs = 4326)
#df_proj = st_as_sf(df_sub, coords = c('lon', 'lat'), crs = 4326)
df_proj = st_transform(df_proj, crs = thisCRS)



ggplot(data = geomData) +
	with_blur(geom_raster(aes(y = lat, x = lon, fill = plotData), interpolate = FALSE), sigma = .01) +
#	geom_sf(aes(colour = plotData)	+
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 2), direction = -1)	+
#	geom_raster(interpolate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_light()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_colour_distiller(palette='Spectral', 
#                         limits=c(quantile(df_sub, .7, na.rm=T), 
#                                  quantile(df_sub, .999, na.rm=T))) +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(geomData$lon), ylim=range(geomData$lat), 
            colour='gray90', size=.2)	+
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='skyblue', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
#	coord_sf(default_crs = sf::st_crs(thisCRS), xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) + 
#	coord_fixed(expand = FALSE)	+
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
#	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=myTitle,
		#subtitle=mySubtitle, 
         x="Longitude", y="Latitude", 
         fill='')
ggsave(paste0(customerFolder, varName, "_gridded.png"), width = 20, height = 8)


	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
#lmt <- ceiling(10*max(abs(range(df_sub$plotData, na.rm=TRUE))))/10000
lmt <- max(abs(range(df_sub$plotData, na.rm=TRUE)))
ggplot(data = geomData) +
	with_blur(geom_raster(data = geomData, aes(fill = plotData, y = lat, x = lon), interpolate = FALSE), sigma = .01) +
#	geom_raster(interploate = TRUE)	+
#	geom_sf(data = df_sub, aes(fill = plotData, color = plotData)) +
	theme_minimal()	+
	theme(panel.ontop=FALSE, panel.background=element_blank()) +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.000001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 10), limits = c(-lmt, lmt))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 10))	+
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
#                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
#            n.breaks=10, limits=c(-lmt,lmt), show.limits=T)  +
	geom_sf(data=ocean50, fill="grey20", colour='grey20')+
	borders('world', xlim=range(geomData$lon), ylim=range(geomData$lat), 
            colour='gray10', size=.2)	+
#	geom_point(data=customerTable, aes(y=Lat, x=Lon), fill='green4', col='green4', size=5, shape='+', stroke=1)	+
#	coord_quickmap(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2])) +
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
#	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(x = NULL, y = NULL, title=paste0('Trends in ', myTitle), 
		#subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        #fill=expression( ))
		fill = "")
ggsave(paste0(customerFolder, varName, "_trends_gridded.png"), width = 20, height = 8)
	



# 1b: country or county avg
sf_use_s2(FALSE)

st_as_sf(world)
st_as_sf(provinces10)


	# abs values
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 2, 3, 1]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])

geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
geomJoin = st_join(world, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/2000
#lmt = 0.2


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
	scale_fill_viridis(option = 'rocket', trans = scales::pseudo_log_trans(sigma = 2), direction = -1)	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
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
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
#	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(title=paste0(myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_polit.png"), width = 20, height = 8)






	# trends
geomData = data.frame(lat = rep(nc_lat, each=length(nc_lon)), lon = rep(nc_lon, length(nc_lat)), plotData = as.vector(nc_testDat[ , , 9, 3, 3]))
df_sub = subset(geomData, lat >= latlonBox_local[1] & lat <= latlonBox_local[2] & lon >= latlonBox_local[3] & lon <= latlonBox_local[4])
geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

geomData_sf = st_as_sf(df_sub, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')
geomData_sf = st_as_sf(geomData, coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')

	# provinces
geomJoin = st_join(provinces10, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ name, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, provinces10, by = 'name')
missingLocs = which(!(provinces10$name %in% geomAvgMrg$name))
geomNN = st_join(provinces10[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)

	# countries
geomJoin = st_join(world, geomData_sf, join = st_intersects)
geomAvg = aggregate(plotData ~ sovereignt, mean, data = geomJoin)#, na.action = na.pass)
geomAvgMrg = merge(geomAvg, world, by = 'sovereignt')
missingLocs = which(!(world$sovereignt %in% geomAvgMrg$sovereignt))
geomNN = st_join(world[missingLocs,], geomData_sf, join = st_nearest_feature)
geomAvgMrg = dplyr::bind_rows(geomAvgMrg, geomNN)


geomAvgMrg_sf = st_as_sf(geomAvgMrg)

#lmt <- ceiling(10*max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE))))/20
lmt <- max(abs(range(geomAvgMrg_sf$plotData, na.rm=TRUE)))


ggplot(data = geomAvgMrg_sf) +
	geom_sf(data = geomAvgMrg_sf, size=6.4, shape=15, aes(fill=plotData), color ='grey25', linewidth = 0.0001) +
#	scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
 #                            '#f7f7f7','#d1e5f0','#67a9cf','#2166ac'),
  #           n.breaks=7, limits=c(-lmt,lmt), show.limits=T)  +
#	scale_fill_viridis(option = 'turbo', trans = scales::pseudo_log_trans(sigma = 0.00001), limits =  c(-lmt, lmt))	+
#	scale_fill_gradient2(low = 'skyblue', high = 'darkred', mid = 'white', trans = scales::pseudo_log_trans(sigma = .00005))	+
	scale_fill_gradient2(high = 'darkblue', low = 'darkred', mid = 'grey90', trans = scales::pseudo_log_trans(sigma = 10), limits = c(-lmt, lmt))	+
#	geom_sf(data = provinces10, fill = NA, color = 'grey75')
	geom_sf(data = world, color = 'grey40', fill = NA, linewidth =1) +
#	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
#	geom_point(data=customerTable, aes(y=Lat, x=Lon),  col='green4', size=5, shape='+', stroke=1)	+
	theme(panel.grid = element_line(colour='grey20')) +
	theme(panel.background = element_rect(fill = 'grey20', colour='grey20')) +
	theme(legend.position = 'right',
		legend.background = element_rect(fill='white', colour='grey20')) +
	guides(guide_legend(title=myTitle)) +
	coord_sf(xlim=c(latlonBox[3], latlonBox[4]), ylim=c(latlonBox[1], latlonBox[2]), expand = FALSE) + 
#	coord_sf(xlim=c(latlonBox_local[3], latlonBox_local[4]), ylim=c(latlonBox_local[1], latlonBox_local[2]), expand = FALSE) + 
	labs(title=paste0('Trends in ', myTitle), #subtitle=mySubtitle, 
        x="Longitude", y="Latitude", 
        fill='')
ggsave(paste0(customerFolder, varName, "_trends_polit.png"), width = 20, height = 8)













# part 2: time series
thisLat = which.min(abs(nc_lat - customerTable$Lat[1]))
thisLon = which.min(abs(nc_lon - customerTable$Lon[1]))
thisTestDat = nc_testDat[thisLon, thisLat, , , ]

theDecades = seq(2010, 2090,10)
theScenarios = c("Low Emissions", "Middle of the Road", "High Emissions")
units = ' km^3 per year'
thisScen = 1

#png(paste0(dataOut_fileLoc, '\\', 'streamflowForecast.png'), width=1920, height=960)
windowsFonts(A = windowsFont("Roboto"))
#par(mar=2*c(5,5,2,2), mgp=2*c(3,1.3,0), font.lab=2, bty='l', cex.lab=2*1.8, cex.axis=2*1.4, cex.main=2*1.8, col='#1A232F')
plot(theDecades, thisTestDat[ , thisScen, 1], ylim = c(min(thisTestDat[ , 1:3, c(1,5,6)])*.95, max(thisTestDat[ , 1:3, c(1,5,6)])*1.05),
		type='l', lwd=1, col='white', xaxt = 'n', #log='y',
		main='', ylab='Streamflow [km^3 per year]', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	axis(1, at = theDecades,col.lab='#1A232F', col.axis='#666D74', 
		labels = theDecades)
#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
	polygon(x=c(theDecades, rev(theDecades)), y=c(thisTestDat[ , thisScen, 5], rev(thisTestDat[ , thisScen, 6])),
		col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
	lines(theDecades, thisTestDat[ , thisScen, 1], 
		col='#0098B2', lwd=5)
	text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*1.00,
		paste0(theScenarios[thisScen], ' Scenario'),
		adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] < 0)	{
		text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
			paste0('Trend: ', 'Decreasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
			adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
	} 	else {
		if((thisTestDat[9, thisScen, 4] < 0.05 | thisTestDat[5, thisScen, 4] < 0.05) & thisTestDat[9, thisScen, 3] > 0)	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: ', 'Increasing by ', signif((median(thisTestDat[6:9, thisScen, 1]) - median(thisTestDat[1:4, thisScen, 1])) / 50, 2), units),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
		}	else	{
			text(x=theDecades[1], y=max(thisTestDat[ , 1:3, c(1,5,6)])*0.87,
				paste0('Trend: No Significant Trend'),
				adj = c(0,0), font=2, col='#F06000', family='A', cex=1*1.0)
			}
	}
#	dev.off()



