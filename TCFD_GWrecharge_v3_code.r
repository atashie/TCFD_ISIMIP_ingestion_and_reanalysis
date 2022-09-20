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
library(ggplot2)



#########################################
# reading in climai netcdf data
ncpath = "J:\\Cai_data\\TCFD\\GWrecharge\\"
ncVarFileName = 'qr'
saveDate = '15SEP2022'
rcpScen = 'rcp60'
	# reading ssp126 climate data 
ncname_gfdl = paste0('clm45_gfdl-esm2m_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_gfdl = nc_open(paste0(ncpath, ncname_gfdl))
nc_gfdl = ncvar_get(ncin_gfdl,ncVarFileName)	# lon, lat, time

ncname_hadgem = paste0('clm45_hadgem2-es_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_hadgem = nc_open(paste0(ncpath, ncname_hadgem))
nc_hadgem = ncvar_get(ncin_hadgem,ncVarFileName)	# lon, lat, time

ncname_ipsl = paste0('clm45_ipsl-cm5a-lr_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_ipsl = nc_open(paste0(ncpath, ncname_ipsl))
nc_ipsl = ncvar_get(ncin_ipsl,ncVarFileName)	# lon, lat, time

ncname_miroc = paste0('clm45_miroc5_ewembi_',rcpScen,'_2005soc_co2_', ncVarFileName, '_global_monthly_2006_2099.nc4')#"clm45_gfdl-esm2m_ewembi_rcp60_2005soc_co2_burntarea_global_monthly_2006_2099.nc4"  
ncin_miroc = nc_open(paste0(ncpath, ncname_miroc))
nc_miroc = ncvar_get(ncin_miroc,ncVarFileName)	# lon, lat, time

nc_lat = ncvar_get(ncin_miroc, 'lat')	# lat is given from high to low
nc_lon = ncvar_get(ncin_miroc, 'lon')
nc_date = as.Date("1661-01-01") + ncvar_get(ncin_miroc, 'time') * 30.4375# time is days after 1661-1-1
nc_years = unique(year(nc_date))
missing_data = 1.00000002004088e+20



dates1019 = which(year(nc_date) == 2010)[1]:which(year(nc_date) == 2019)[12]
dates2029 = which(year(nc_date) == 2020)[1]:which(year(nc_date) == 2029)[12]
dates3039 = which(year(nc_date) == 2030)[1]:which(year(nc_date) == 2039)[12]
dates4049 = which(year(nc_date) == 2040)[1]:which(year(nc_date) == 2049)[12]
dates5059 = which(year(nc_date) == 2050)[1]:which(year(nc_date) == 2059)[12]
dates6069 = which(year(nc_date) == 2060)[1]:which(year(nc_date) == 2069)[12]
dates7079 = which(year(nc_date) == 2070)[1]:which(year(nc_date) == 2079)[12]
dates8089 = which(year(nc_date) == 2080)[1]:which(year(nc_date) == 2089)[12]
dates9099 = which(year(nc_date) == 2090)[1]:which(year(nc_date) == 2099)[12]


summData = data.frame(Raw_Hazard_Value = NA, Percentile = NA, Relative_Hazard_Score = NA,
	Decade = NA, Scenario = NA, Lat = NA, Lon = NA)
	
iter = 1
save_iter = 0
for(i in 1:length(nc_lat))	{
	for(j in 1:length(nc_lon))	{
		nc_dummy = nc_gfdl[j,i,dates1019] # reading in one data set to test for nona
		if(any(!is.na(nc_dummy) & any(nc_dummy != missing_data)))	{
			nc_1019 = c(nc_gfdl[j,i,dates1019],nc_hadgem[j,i,dates1019],nc_ipsl[j,i,dates1019],nc_miroc[j,i,dates1019]) 
			nc_2029 = c(nc_gfdl[j,i,dates2029],nc_hadgem[j,i,dates2029],nc_ipsl[j,i,dates2029],nc_miroc[j,i,dates2029]) 
			nc_3039 = c(nc_gfdl[j,i,dates3039],nc_hadgem[j,i,dates3039],nc_ipsl[j,i,dates3039],nc_miroc[j,i,dates3039]) 
			nc_4049 = c(nc_gfdl[j,i,dates4049],nc_hadgem[j,i,dates4049],nc_ipsl[j,i,dates4049],nc_miroc[j,i,dates4049]) 
			nc_5059 = c(nc_gfdl[j,i,dates5059],nc_hadgem[j,i,dates5059],nc_ipsl[j,i,dates5059],nc_miroc[j,i,dates5059]) 
			nc_6069 = c(nc_gfdl[j,i,dates6069],nc_hadgem[j,i,dates6069],nc_ipsl[j,i,dates6069],nc_miroc[j,i,dates6069]) 			
			nc_7079 = c(nc_gfdl[j,i,dates7079],nc_hadgem[j,i,dates7079],nc_ipsl[j,i,dates7079],nc_miroc[j,i,dates7079]) 			
			nc_8089 = c(nc_gfdl[j,i,dates8089],nc_hadgem[j,i,dates8089],nc_ipsl[j,i,dates8089],nc_miroc[j,i,dates8089]) 		
			nc_9099 = c(nc_gfdl[j,i,dates9099],nc_hadgem[j,i,dates9099],nc_ipsl[j,i,dates9099],nc_miroc[j,i,dates9099]) 			

			data1019 = NULL
			for(kh in 1:(length(nc_1019)/12))	{
				data1019 = c(data1019,
					sum(nc_1019[(1:12)+(12*(kh-1))]))
			}
			data2029 = NULL
			for(kh in 1:(length(nc_2029)/12))	{
				data2029 = c(data2029,
					sum(nc_2029[(1:12)+(12*(kh-1))]))
			}
			data3039 = NULL
			for(kh in 1:(length(nc_3039)/12))	{
				data3039 = c(data3039,
					sum(nc_3039[(1:12)+(12*(kh-1))]))
			}
			data4049 = NULL
			for(kh in 1:(length(nc_4049)/12))	{
				data4049 = c(data4049,
					sum(nc_4049[(1:12)+(12*(kh-1))]))
			}
			data5059 = NULL
			for(kh in 1:(length(nc_5059)/12))	{
				data5059 = c(data5059,
					sum(nc_5059[(1:12)+(12*(kh-1))]))
			}
			data6069 = NULL
			for(kh in 1:(length(nc_6069)/12))	{
				data6069 = c(data6069,
					sum(nc_6069[(1:12)+(12*(kh-1))]))
			}
			data7079 = NULL
			for(kh in 1:(length(nc_7079)/12))	{
				data7079 = c(data7079,
					sum(nc_7079[(1:12)+(12*(kh-1))]))
			}
			data8089 = NULL
			for(kh in 1:(length(nc_8089)/12))	{
				data8089 = c(data8089,
					sum(nc_8089[(1:12)+(12*(kh-1))]))
			}
			data9099 = NULL
			for(kh in 1:(length(nc_9099)/12))	{
				data9099 = c(data9099,
					sum(nc_9099[(1:12)+(12*(kh-1))]))
			}

			
			the_lat = nc_lat[i]
			the_lon = nc_lon[j]

			summData[iter,] = c(median(nc_1019), NA, NA, 
				'2010s', rcpScen, the_lat, the_lon); iter = iter+1
			summData[iter,] = c(median(nc_2029), NA, NA, 
				'2020s', rcpScen, the_lat, the_lon); iter = iter+1
			summData[iter,] = c(median(nc_3039), NA, NA, 
				'2030s', rcpScen, the_lat, the_lon); iter = iter+1
			summData[iter,] = c(median(nc_4049), NA, NA, 
				'2040s', rcpScen, the_lat, the_lon); iter = iter+1
			summData[iter,] = c(median(nc_5059), NA, NA, 
				'2050s', rcpScen, the_lat, the_lon); iter = iter+1
			summData[iter,] = c(median(nc_6069), NA, NA, 
				'2060s', rcpScen, the_lat, the_lon); iter = iter+1
			summData[iter,] = c(median(nc_7079), NA, NA, 
				'2070s', rcpScen, the_lat, the_lon); iter = iter+1
			summData[iter,] = c(median(nc_8089), NA, NA, 
				'2080s', rcpScen, the_lat, the_lon); iter = iter+1
			summData[iter,] = c(median(nc_9099), NA, NA, 
				'2090s', rcpScen, the_lat, the_lon); iter = iter+1
			
			if(nrow(summData) > 20000)	{
				save_iter = save_iter + 1
				fwrite(summData, paste0(ncpath, "temp_out_", save_iter, ".csv"))
				summData = data.frame(Raw_Hazard_Value = NA, Percentile = NA, Relative_Hazard_Score = NA,
					Decade = NA, Scenario = NA, Lat = NA, Lon = NA)
				iter = 1
				print(c(i,j))
				print(c(the_lat, the_lon))
			} 
		}
	}
#	print(ftrFldIntrv_df)
}	
save_iter = save_iter + 1
fwrite(summData, paste0(ncpath, "temp_out_", save_iter, ".csv"))


all_data = fread(paste0(ncpath, "temp_out_", 1, ".csv"))
for(files in 2:save_iter)	{
	all_data = rbind(all_data, fread(paste0(ncpath, "temp_out_", files, ".csv")))
}
	# saving the scenario
fwrite(all_data, paste0(ncpath, ncVarFileName, "_", rcpScen, ".csv"))



##################################################################################################################################
	# combining the data
all_data = rbind(fread(paste0(ncpath, ncVarFileName, '_rcp60', ".csv")), 
				 fread(paste0(ncpath, ncVarFileName, "_rcp26", ".csv")))
				 
fwrite(all_data, paste0(ncpath, ncVarFileName, "_", "allData_", saveDate,  ".csv"))
	# reading back in to confirm data
all_data = fread(paste0(ncpath, ncVarFileName, "_", "allData_", saveDate, ".csv"), colClasses=c('numeric','numeric','character','character','character','numeric','numeric'))

	# quantizing, where more is bad
#histQuants = quantile(subset(all_data, Decade == '2010s')$Raw_Hazard_Value, seq(.01,1,0.01))
#for(i in 2:length(histQuants))	{
#	all_data$Percentile[which(all_data$Raw_Hazard_Value > histQuants[i])] = i
#}
#	# converting fraction to percent, and rounding for Looker
#all_data$Raw_Hazard_Value = round(all_data$Raw_Hazard_Value * 100, 6)
	# quantizing, where less is bad
all_data$Percentile = 100
histQuants = rev(quantile(subset(all_data, Decade == '2010s')$Raw_Hazard_Value, seq(.01,1,0.01)))
for(i in 1:length(histQuants))	{
	all_data$Percentile[which(all_data$Raw_Hazard_Value < histQuants[i])] = i
}
	# rescaling extreme values according to quantiles
all_data$Raw_Hazard_Value[all_data$Raw_Hazard_Value > max(histQuants)] = max(histQuants) * 1.1
all_data$Raw_Hazard_Value[all_data$Raw_Hazard_Value < min(histQuants)] = min(histQuants) * 1.1



	# identifying relative hazard values with numbers for plotting
all_data$Relative_Hazard_Score = 1
all_data$Relative_Hazard_Score[all_data$Percentile > 33] = 2
all_data$Relative_Hazard_Score[all_data$Percentile > 67] = 3


	# quick plotting for a sanity check
thisdecade = subset(all_data, Decade == '2060s' & Scenario == 'rcp60')# & Lon < -10 & Lon > -100)

ggplot(thisdecade, aes(x=Lon, y=Lat)) +
	geom_point(aes(color = as.numeric(Relative_Hazard_Score)))
ggplot(thisdecade, aes(x=Lon, y=Lat)) +
	geom_point(aes(color = Raw_Hazard_Value))
ggplot(thisdecade, aes(x=Lon, y=Lat)) +
	geom_point(aes(color = Percentile))


	# identifying relative hazard values as string for looker
class(all_data$Relative_Hazard_Score) = 'character'
all_data$Relative_Hazard_Score = '1. Low'
all_data$Relative_Hazard_Score[all_data$Percentile > 49] = '2. Medium'
all_data$Relative_Hazard_Score[all_data$Percentile > 74] = '3. High'

fwrite(all_data, paste0(ncpath, ncVarFileName, "_", "allData_", saveDate,  ".csv"))














