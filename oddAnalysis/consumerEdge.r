library(dplyr)
library(lubridate)
library(data.table)
savePath = 'J://Downloads//exampleSummaryOut_edgeCai//'

brandDetailPath = 'J://Downloads//brand_detail_2023_09_05-000000000000.gz'
brandDetail = data.table::fread(brandDetailPath)
thingToAgg = "subindustry_name"

mainFolder = 'J://Downloads//mainFolder//'
allFiles = list.files(mainFolder)
iter = 1; numSave = 1
for(thisFile in allFiles){
	if(iter == 1) {
		loadedFile = fread(paste0(mainFolder, thisFile))
		dailyData = merge(loadedFile, brandDetail[, c('brand_name', ..thingToAgg)], by.x="brand_name", by.y='brand_name', all.x = TRUE)
		colnames(dailyData) = c(colnames(dailyData)[1:7], "industry_name")

		rm(loadedFile)
		
		dailyDataMelt = dailyData %>%
		  group_by(industry_name, trans_date, state_abbr) %>%
		  summarize(
			total_sales_val = sum(spend_amount, na.rm = TRUE),
			total_sales_cnt = sum(trans_count, na.rm = TRUE)
		  )
		rm(dailyData)
	} else {
		loadedFile = fread(paste0(mainFolder, thisFile))
		dailyData = merge(loadedFile, brandDetail[, c('brand_name', ..thingToAgg)], by.x="brand_name", by.y='brand_name', all.x = TRUE)
		colnames(dailyData) = c(colnames(dailyData)[1:7], "industry_name")
		rm(loadedFile)
		
		dailyDataMelt_new = dailyData %>%
		  group_by(industry_name, trans_date, state_abbr) %>%
		  summarize(
			total_sales_val = sum(spend_amount, na.rm = TRUE),
			total_sales_cnt = sum(trans_count, na.rm = TRUE)
		  )
		rm(dailyData)
		
		dailyDataMelt = merge(dailyDataMelt, dailyDataMelt_new, all=TRUE)
		rm(dailyDataMelt_new)
	}
	print(c(iter, thisFile, nrow(dailyDataMelt)))
	if(iter == 5){
		data.table::fwrite(dailyDataMelt, paste0(savePath, "melt_ind_", thingToAgg, "_", numSave, ".gz"))
		rm(dailyDataMelt)
		iter = 0
		numSave = numSave + 1
	}
	iter = iter + 1
}
data.table::fwrite(dailyDataMelt, paste0(savePath, "melt_ind_", thingToAgg, "_", numSave, ".gz"))


mergedFiles = list.files(savePath)
iter = 1
for(thisFile in mergedFiles){
	if(iter == 1) {
		mergedData = fread(paste0(savePath, thisFile))
	} else {
		mergedData = merge(mergedData, fread(paste0(savePath, thisFile)), all=TRUE)
	}
	iter = iter+1
}
mergedDataMelt = mergedData %>%
  group_by(industry_name, trans_date, state_abbr) %>%
  summarize(
	total_sales_val = sum(total_sales_val, na.rm = TRUE),
	total_sales_cnt = sum(total_sales_cnt, na.rm = TRUE)
  )

data.table::fwrite(mergedDataMelt, paste0(savePath, "merged_data_all_", thingToAgg, ".gz"))
rm(mergedDataMelt)
###############################################################
# old code
year_week = paste0(year(dailyData$trans_date), "-W", sprintf("%02d", week(dailyData$trans_date) - 1), "-1")
date_object = as_date(year_week, format = "%Y-W%W-%u")
dailyData$yr_wk = date_object#paste0(lubridate::year(dailyData$trans_date), "_", lubridate::week(dailyData$trans_date))
dailyData$yr_mn = floor_date(dailyData$trans_date, unit = "month")#paste0(lubridate::year(dailyData$trans_date), "_", lubridate::month(dailyData$trans_date))
# /old code
###############################################################

  
library(ncdf4)

dailyData = data.table::fread(paste0(savePath, "merged_data_all.gz"))
dailyNb = subset(dailyData, state_abbr == "NE")
dateRange = range(dailyNb$trans_date)

climDat = nc_open(paste0(ncpath, "advNb_clim.nc"))


ncpath = "J:\\Cai_data\\Advanta\\firstFrost\\Nebraska\\"
latLon = c(41, -97.25)
# Get lat and lon arrays
lats = ncvar_get(climDat, "latitude")   # size 15
lons = ncvar_get(climDat, "longitude")  # size 36

# Function to find nearest index
find_nearest = function(array, value) {
    which.min(abs(array - value))
}

# Get indices for your target lat/lon
target_lat = latLon[1]
target_lon = latLon[2]

lat_index = find_nearest(lats, target_lat)
lon_index = find_nearest(lons, target_lon)

timeVar = ncvar_get(climDat, "time") + as.Date("1979-01-01")
theseTimes = which(timeVar > dateRange[1] & timeVar < dateRange[2])

chooseYourVar = 't2m_max'
time_lag = -1
myDataSlice = ncvar_get(climDat,
						varid = chooseYourVar,
						start = c(lon_index, lat_index, theseTimes[1] + time_lag),
						count = c(1,1, last(theseTimes) + 1- theseTimes[1]))

climTable = data.table(trans_date = timeVar[theseTimes], climVar = myDataSlice)

dataInspect = merge(dailyNb, climTable, by="trans_date")
for(thisIndustry in unique(dataInspect$industry))	{
	subData = subset(dataInspect, industry_name == thisIndustry)
	pval_val = cor(x=subData$climVar, y=subData$total_sales_val, method = 'spearman')
	pval_cnt = cor(x=subData$climVar, y=subData$total_sales_cnt, method = 'spearman')
	
	lm_val = lm(subData$total_sales_val ~ subData$climVar)
	lm_cnt = lm(subData$total_sales_cnt ~ subData$climVar)
	lm_val_r2 = summary(lm_val)$adj
	lm_cnt_r2 = summary(lm_cnt)$adj
	
	# genreate plot 
}



dataInspect = merge(dailyNb, climTable, by = "trans_date")


for (thisIndustry in unique(dataInspect$industry_name)) {
  subData = subset(dataInspect, industry_name == thisIndustry)
  if(chooseYourVar == "tp"){
	subData$climVar = log(subData$climVar+ 0.001)
	}
  
  # Perform Spearman correlation tests and extract p-values
  spearman_val = cor.test(subData$climVar, subData$total_sales_val, method = 'spearman')
  pval_val = spearman_val$p.value
  
  spearman_cnt = cor.test(subData$climVar, subData$total_sales_cnt, method = 'spearman')
  pval_cnt = spearman_cnt$p.value
  
  # Fit linear models
  lm_val = lm(total_sales_val ~ climVar, data = subData)
  lm_cnt = lm(total_sales_cnt ~ climVar, data = subData)
  
  # Extract adjusted R-squared values
  lm_val_r2 = summary(lm_val)$adj.r.squared
  lm_cnt_r2 = summary(lm_cnt)$adj.r.squared
  
  # Prepare to save plot
  # Ensure the industry name is safe for use in filenames
  safeIndustryName = gsub('[^A-Za-z0-9]+', '_', thisIndustry)
  plot_filename = paste0(savePath, "/", chooseYourVar, "_", safeIndustryName, "_timeLagOf_", time_lag, "days_plot.png")
  
  # Open a graphics device to save the plot
  png(filename = plot_filename, width = 800, height = 400)
  
  # Set up plotting area to display two plots side by side
  par(mfrow = c(1, 2))
  
  # Generate scatter plot for total_sales_val
  plot(subData$climVar, subData$total_sales_val,
       xlab = chooseYourVar, ylab = 'Total Sales Value',
       main = paste('Industry:', thisIndustry,
                    '\nSpearman p-value:', round(pval_val, 4),
                    'Adj R-squared:', round(lm_val_r2, 4)))
  
  # If the p-value is significant, add the regression line
  if (pval_val < 0.05) {
    abline(lm_val, col = 'red')
  }
  
  # Generate scatter plot for total_sales_cnt
  plot(subData$climVar, subData$total_sales_cnt,
       xlab = chooseYourVar, ylab = 'Total Sales Count',
       main = paste('Industry:', thisIndustry,
                    '\nSpearman p-value:', round(pval_cnt, 4),
                    'Adj R-squared:', round(lm_cnt_r2, 4)))
  
  # If the p-value is significant, add the regression line
  if (pval_cnt < 0.05) {
    abline(lm_cnt, col = 'blue')
  }
  
  # Close the graphics device to save the plot
  dev.off()
  
  # Reset plotting area
  par(mfrow = c(1, 1))
}








dataInspect = merge(dailyNb, climTable, by = "trans_date")


for (thisIndustry in unique(dataInspect$industry_name)) {
  subData = subset(dataInspect, industry_name == thisIndustry)
  if(chooseYourVar == "tp"){
	subData$climVar = log(subData$climVar+ 0.001)
	}
  
  # Perform Spearman correlation tests and extract p-values
  spearman_val = cor.test(subData$climVar, subData$total_sales_val, method = 'spearman')
  pval_val = spearman_val$p.value
  
  spearman_cnt = cor.test(subData$climVar, subData$total_sales_cnt, method = 'spearman')
  pval_cnt = spearman_cnt$p.value
  
  # Fit linear models
  lm_val = lm(total_sales_val ~ climVar, data = subData)
  lm_cnt = lm(total_sales_cnt ~ climVar, data = subData)
  
  # Extract adjusted R-squared values
  lm_val_r2 = summary(lm_val)$adj.r.squared
  lm_cnt_r2 = summary(lm_cnt)$adj.r.squared
  
  # Prepare to save plot
  # Ensure the industry name is safe for use in filenames
  safeIndustryName = gsub('[^A-Za-z0-9]+', '_', thisIndustry)
  plot_filename = paste0(savePath, "/", chooseYourVar, "_", safeIndustryName, "_plot.png")
  
  # Open a graphics device to save the plot
  png(filename = plot_filename, width = 800, height = 400)
  
  # Set up plotting area to display two plots side by side
  par(mfrow = c(1, 2))
  
  # Generate scatter plot for total_sales_val
  plot(subData$climVar, subData$total_sales_val,
       xlab = chooseYourVar, ylab = 'Total Sales Value',
       main = paste('Industry:', thisIndustry,
                    '\nSpearman p-value:', round(pval_val, 4),
                    'Adj R-squared:', round(lm_val_r2, 4)))
  
  # If the p-value is significant, add the regression line
  if (pval_val < 0.05) {
    abline(lm_val, col = 'red')
  }
  
  # Generate scatter plot for total_sales_cnt
  plot(subData$climVar, subData$total_sales_cnt,
       xlab = chooseYourVar, ylab = 'Total Sales Count',
       main = paste('Industry:', thisIndustry,
                    '\nSpearman p-value:', round(pval_cnt, 4),
                    'Adj R-squared:', round(lm_cnt_r2, 4)))
  
  # If the p-value is significant, add the regression line
  if (pval_cnt < 0.05) {
    abline(lm_cnt, col = 'blue')
  }
  
  # Close the graphics device to save the plot
  dev.off()
  
  # Reset plotting area
  par(mfrow = c(1, 1))
}






















  
library(ncdf4)
dateRange = range($trans_date)
climDat = nc_open(paste0(ncpath, "advNb_clim.nc"))


ncpath = "J:\\Cai_data\\Advanta\\firstFrost\\Nebraska\\"
latLon = c(41, -97.25)
# Get lat and lon arrays
lats = ncvar_get(climDat, "latitude")   # size 15
lons = ncvar_get(climDat, "longitude")  # size 36

# Function to find nearest index
find_nearest = function(array, value) {
    which.min(abs(array - value))
}

# Get indices for your target lat/lon
target_lat = latLon[1]
target_lon = latLon[2]

lat_index = find_nearest(lats, target_lat)
lon_index = find_nearest(lons, target_lon)

timeVar = ncvar_get(climDat, "time") + as.Date("1979-01-01")
theseTimes = which(timeVar > dateRange[1] & timeVar < dateRange[2])

time_lag = 1

chooseYourVar = 't2m_max'
maxTemp = ncvar_get(climDat, varid = chooseYourVar, start = c(lon_index, lat_index, theseTimes[1] + time_lag), count = c(1,1, last(theseTimes) + 1- theseTimes[1]))
chooseYourVar = 't2m_min'
minTemp = ncvar_get(climDat, varid = chooseYourVar, start = c(lon_index, lat_index, theseTimes[1] + time_lag), count = c(1,1, last(theseTimes) + 1- theseTimes[1]))
chooseYourVar = 'tp'
precip = ncvar_get(climDat, varid = chooseYourVar, start = c(lon_index, lat_index, theseTimes[1] + time_lag), count = c(1,1, last(theseTimes) + 1- theseTimes[1]))

climTable = data.table(date = as.Date(timeVar[theseTimes]), maxTemp = maxTemp, minTemp = minTemp, precip = precip)


library(fredr)
fredr_set_key('f910a50104a6cf522ad6c1c0060103c5')

ne_unemployment = fredr(series_id = "NEUR", observation_start = as.Date("2015-01-01"), frequency = "m")  # monthly data
ne_population = fredr(series_id = "NEPOP", observation_start = as.Date("2015-01-01"), frequency = "a")  # monthly data
ne_income = fredr(series_id = "MEHOINUSNEA672N", observation_start = as.Date("2015-01-01"), frequency = "a")  # monthly data
ne_rentalVac = fredr(series_id = "NERVAC", observation_start = as.Date("2015-01-01"), frequency = "a") 
ne_housingCost = fredr(series_id = "MEDLISPRIPERSQUFEENE", observation_start = as.Date("2015-01-01"), frequency = "a") # price per sq ft, median
addYearMonth = function(x){
	x$year = year(x$date)
	x$month = month(x$date)
	x$doy = yday(x$date)
	return(x)
}
allFred = ne_unemployment %>%
	rename(unemployment = value) %>%
	full_join(ne_income, by = "date") %>%
	rename(income = value) %>%
	full_join(ne_rentalVac, by="date") %>%
	rename(rentalVacs = value) %>%
	full_join(ne_housingCost, by="date") %>%
	rename(housingCost = value) %>%
	select(date, unemployment, income, rentalVacs, housingCost) %>%
	arrange(date)
	
	
nebData = filter(fread(paste0(savePath, "merged_data_all_", thingToAgg, ".gz")), state_abbr == 'NE')  
#   "Quick Service - Chicken"             "Quick Service - Hamburger"          
# [99] "Quick Service - Other"               "Quick Service - Pizza"              Grocers     Wholesale Building Materials        Home Improvement
#[101] "Quick Service - Snack/Beverage"      "Restaurant/Other Delivery Services" 
  
qsNebData = nebData %>%
	filter(industry_name  == 'Quick Service - Pizza') %>%
	rename(date = 'trans_date') %>%
	mutate(date = as.Date(date)) %>%
    complete(date = seq.Date(
        from = min(date, na.rm = TRUE),
        to = max(date, na.rm = TRUE),
        by = "day")) %>%
    mutate(
        total_sales_val = replace_na(total_sales_val, 0),
        total_sales_cnt = replace_na(total_sales_cnt, 0)
    ) %>%
	select(date, total_sales_val, total_sales_cnt) %>%
	left_join(allFred, by="date") %>%
	mutate(dayOfWeek = wday(date)) %>%
	right_join(climTable, by="date") %>%
	fill(unemployment, income, rentalVacs, housingCost, .direction = "downup") %>%
	addYearMonth() %>%
	arrange(date)

#plot(qsNebDataDetrend$trans_date, qsNebDataDetrend$total_sales_val)

#qsNebDataDetrend = qsNebData %>%
#	mutate(time_numeric = as.numeric(as.Date(trans_date)) - as.numeric(as.Date(min(trans_date)))) %>%
#	mutate(predicted_values = predict(lm(total_sales_val ~ time_numeric + 1, data = .))) %>%
#	mutate(detrended_sales = total_sales_val - predicted_values) 

#plot(qsNebDataDetrend$trans_date, qsNebDataDetrend$detrended_sales)
#plot(qsNebDataDetrend$trans_date, qsNebDataDetrend$total_sales_val)
  	

# Load required libraries
library(randomForest)
library(caret)
targetCol = 2

# Prepare the data
X = qsNebData[, c(4:11,13)]  # Features (columns 6-13)
X = qsNebData[, c(9:11)]  # Features (columns 6-13)
y = unlist(qsNebData[, targetCol])     # Target (column 2)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index = 1:(length(y)*.8)#createDataPartition(y, p = 0.8, list = FALSE)
X_train = X[train_index, ]
X_test = X[-train_index, ]
y_train = y[train_index]
y_test = y[-train_index]

# Train the random forest model
rf_model = randomForest(
    x = X_train,
    y = y_train,
    ntree = 1000,           # Number of trees
    mtry = ceiling(sqrt(ncol(X))),  # Number of variables to try at each split
    importance = TRUE      # Calculate variable importance
)

# Make predictions
predictions = predict(rf_model, X_test)
allVals = predict(rf_model, X)

# Evaluate the model
rmse = sqrt(mean((predictions - y_test)^2))
r2 = 1 - sum((y_test - predictions)^2) / sum((y_test - mean(y_test))^2)

# View variable importance
importance(rf_model)
varImpPlot(rf_model)
r2
 
plot(qsNebData$date, unlist(qsNebData[,targetCol]), type='l')
lines(qsNebData$date[-train_index], predictions, col='red')

plot(unlist(qsNebData[-train_index,targetCol]), predictions)
























myDataTable

myDataSlice

	# reading in nc data
birdies = nc_open(paste0(ncpath, "birdsumprob.nc"))
mammals = nc_open(paste0(ncpath, "mammalsumprob.nc"))
amphibs = nc_open(paste0(ncpath, "amphibiansumprob.nc"))

birdVar = ncvar_get(birdies, "presence")
mammVar = ncvar_get(mammals, "presence")
amphVar = ncvar_get(amphibs, "presence")
	# sapply(birdies$var$presence$dim, function(x) x$name)
	# format [scenario, lon, lat, time, forcing, model]

nc_lons = ncvar_get(birdies, "lon")
nc_lats = ncvar_get(birdies, "lat")
nc_date = lubridate::year(ncvar_get(birdies, "time") + as.Date("1990-01-01"))



	# df for holding outputs
myMissingData = NA
customerTable$Bird_PresenceTrajectory = NA
customerTable$MammalPresenceTrajectory = NA
customerTable$Amphibian_PresenceTrajectory = NA
customerTable$Bird_RelativeCoverage = NA
customerTable$Mammal_RelativeCoverage = NA
customerTable$Amphibian_RelativeCoverage = NA

for(thisScen in 1:length(rcpScenarios))	{
	rcpScenNum = rcpScenarios[thisScen]
	birdAvg = quantile(birdVar[thisScen, , , 1:4, , ], seq(0,1,0.01), na.rm=TRUE)
	amphAvg = quantile(amphVar[thisScen, , , 1:4, , ], seq(0,1,0.01), na.rm=TRUE)
	mammAvg = quantile(mammVar[thisScen, , , 1:4, , ], seq(0,1,0.01), na.rm=TRUE)

	for(thisLocation in 1:nrow(customerTable))	{

		closeLon = which.min(abs(nc_lons - customerTable$Longitude[thisLocation]))
		closeLat = which.min(abs(nc_lats - customerTable$Latitude[thisLocation]))

		# checking to ensure we are not over a water tile
		if(all(is.na(mammVar[thisScen, closeLon, closeLat, 1, , ])))	{
			closeLat = closeLat + 1
			if(all(is.na(mammVar[thisScen, closeLon, closeLat, 1, , ])))	{
				closeLat = closeLat - 2
				if(all(is.na(mammVar[thisScen, closeLon, closeLat, 1, , ])))	{
					closeLat = closeLat + 1
					closeLon = closeLon + 1
					if(all(is.na(mammVar[thisScen, closeLon, closeLat, 1, , ])))	{
						closeLon = closeLon - 2
						if(all(is.na(mammVar[thisScen, closeLon, closeLat, 1, , ])))	{
							closeLon = closeLon + 1
						}
					}
				}
			}
		}
			
		birdData = as.data.frame(cbind(birdVar[thisScen, closeLon, closeLat, , , 1], birdVar[thisScen, closeLon, closeLat, , , 1]))
		birdData$Date = nc_date	
		birdMelt = reshape::melt(birdData, id="Date")

		mammData = as.data.frame(cbind(mammVar[thisScen, closeLon, closeLat, , , 1], mammVar[thisScen, closeLon, closeLat, , , 1]))
		mammData$Date = nc_date	
		mammMelt = reshape::melt(mammData, id="Date")

		amphData = as.data.frame(cbind(amphVar[thisScen, closeLon, closeLat, , , 1], amphVar[thisScen, closeLon, closeLat, , , 1]))
		amphData$Date = nc_date	
		amphMelt = reshape::melt(amphData, id="Date")

			# testing for NaNs, which unforunately do not coincide
		if(all(is.na(birdMelt$value))) {
			customerTable$Bird_PresenceTrajectory[thisLocation] = 0
			birdRel = 5
		} else {
			customerTable$Bird_PresenceTrajectory[thisLocation] = mblm(value ~ Date, birdMelt)$coefficients[2] * 10 * 100 / birdAvg[101]
			birdRel = which(birdAvg > mean(subset(birdMelt, Date < 2025)$value))[1]
		}

		if(all(is.na(mammMelt$value))) {
			customerTable$MammalPresenceTrajectory[thisLocation] = 0
			mammRel = 5
		} else {
			customerTable$MammalPresenceTrajectory[thisLocation] = mblm(value ~ Date, mammMelt)$coefficients[2] * 10 * 100 / mammAvg[101]
			mammRel = which(mammAvg > mean(subset(mammMelt, Date < 2025)$value))[1]
		}

		if(all(is.na(amphMelt$value))) {
			customerTable$Amphibian_PresenceTrajectory[thisLocation] = 5
			amphRel = 5
		} else {
			customerTable$Amphibian_PresenceTrajectory[thisLocation] = mblm(value ~ Date, amphMelt)$coefficients[2] * 10 * 100 / amphAvg[101]
			amphRel = which(amphAvg > mean(subset(amphMelt, Date < 2025)$value))[1]
		}
		
		customerTable$Bird_RelativeCoverage[thisLocation] = birdRel / (birdRel + mammRel + amphRel)
		customerTable$Mammal_RelativeCoverage[thisLocation] = mammRel / (birdRel + mammRel + amphRel)
		customerTable$Amphibian_RelativeCoverage[thisLocation] = amphRel / (birdRel + mammRel + amphRel)
		
		
	}
	customerTable$Biodiversity_Index_Score = 101 - customerTable$RankVal  
	data.table::fwrite(customerTable, paste0(customerOutputPath, "_", rcpScenNum, ".csv"))
}

#dataOutArray = readRDS(file=paste0(ncpath, 'data_out.rds'))
#dataOutArray = readRDS(file=paste0(ncpath, 'data_out_bigSmoothAndSpatial.rds'))
#dataOutArray = readRDS(file=paste0(ncpath, 'data_out_bigSmoothAndSpatialMore.rds'))
dataOutArray = readRDS(file=paste0(ncpath, 'data_out_bigSmoothAndSpatialMorer.rds'))
##### temp fix for not having rcp 8.5
dataOutArray = array(rep(myMissingData, length(nc_lon) * length(nc_lat) * length(whichDecades) * length(rcpScenarios) * length(valueType)), 
	dim = c(length(nc_lon), length(nc_lat), length(whichDecades), 3, length(valueType)))
old_dataOutArray = readRDS(file=paste0(ncpath, 'data_out_bigSmoothAndSpatialMorer.rds'))
dataOutArray[ , , , 1:2, ] = old_dataOutArray[ , , , 1:2, ]
##### end temp fix

