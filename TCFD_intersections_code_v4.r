library(data.table)
library(ncdf4)

userName = 'HMClause'
hazardFolder = 'J:\\Cai_data\\TCFD\\ProcessedNCs\\'
customerFolder = 'J:\\Cai_data\\TCFD\\locations\\'

customerTable = fread(paste0(customerFolder, 'HMClause_locations_allCucurbit.csv'))
hazardTable = fread(paste0(hazardFolder, 'Hazard_Table.csv'))
relHazScores = fread(paste0(hazardFolder, 'Relative_Hazard_Table.csv'))

dataOutput = data.frame(User = NA, Location = NA, Region = NA, Subregion = NA, Lat = NA, Lon = NA,
	Hazard = NA, Hazard_Measure = NA, Decade = NA, Scenario = NA, Raw_Hazard_Value = NA, Percentile_Score = NA, Relative_Hazard_Score = NA)
	
for(thisHazard in 6:ncol(customerTable))	{
	if(any(customerTable[, ..thisHazard]))	{
		hazardMeasures = subset(hazardTable, Hazard == names(customerTable)[thisHazard])
		
		for(thisHazardMeasure in 1:nrow(hazardMeasures))	{
			hazardMeasureNC = nc_open(paste0('J:\\Cai_data\\TCFD\\ProcessedNCs\\', hazardMeasures$Hazard_Measure[thisHazardMeasure], '_processed.nc'))
			nc_lats = ncvar_get(hazardMeasureNC, 'lat')
			nc_lons = ncvar_get(hazardMeasureNC, 'lon')
			nc_decades = paste0('20', ncvar_get(hazardMeasureNC,'decade'), 's')
						
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
							Relative_Hazard_Score = NA))
				}
			}
		}
	}
}

for(thisRow in 1:nrow(relHazScores))	{
	dataOutput$Relative_Hazard_Score[which(dataOutput$Percentile_Score > relHazScores$Hazard_Percentile[thisRow])] = relHazScores$Hazard_Common_Name[thisRow]
}		
	
fwrite(dataOutput, paste0(customerFolder, 'processedOutput_', Sys.Date(), '.csv'))
	
	
	
	
	
	
	
	
	
	
	
	
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
