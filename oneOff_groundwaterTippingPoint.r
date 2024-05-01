library(data.table)
library(lubridate)
library(zoo)
library(ggplot2)
library("ggthemes")
library(viridis)

gwHistoric = fread("J:\\Cai_data\\oneOffs\\localGroundwater.csv")

gwHistoric$Time = as.Date(strftime(gwHistoric$Time, format="%Y-%m-%d"))
startYear = year(first(gwHistoric$Time))
endYear = year(last(gwHistoric$Time))

gwBackfillDates = data.table(Time =seq(ymd(paste0(startYear,"0101")), ymd(paste0(endYear,"1231")), 1))
	
gwBackfill = merge(gwBackfillDates, gwHistoric, all.x=TRUE)
gwBackfill$gwLevel = na.approx(gwBackfill$Depth_to_Water_Below_Land_Surface_in_feet, rule=2)



gwJerjes = fread("J:\\Cai_data\\oneOffs\\final_predictions_2069.csv")
if(any(is.na(gwJerjes[1,]))) {
	theseCols = which(!is.na(gwJerjes[1,]))
	gwJerjes = gwJerjes[ , ..theseCols]
}
names(gwJerjes)[1] = "Time"
gwJerjes$Time = as.Date(gwJerjes$Time)
gwCombined = merge(gwJerjes, gwBackfill[,-2], all = TRUE, by = "Time")


gwCombined$Year = year(gwCombined$Time)



gwCounts = data.frame(Year = unique(year(gwCombined$Time)),
	Low_Risk = NA,
	Medium_Risk = NA,
	High_Risk = NA,
	Severe_Risk = NA)
gwAverages = data.frame(Year = unique(year(gwCombined$Time)),
	Avg_Water_Level = NA,
	Q95 = NA,
	Q90 = NA,
	Q75 = NA,
	Q50 = NA,
	Q25 = NA,
	Q10 = NA,
	Q05 = NA)
	
lowRisk = 600
medRisk = 800
highRisk = 1000
severeRisk = 1200
for(i in 1:length(gwCounts$Year)){
	thisYear = subset(gwCombined, Year == gwCounts$Year[i])
	dateCols = c(1, ncol(thisYear))
	justData = unlist(subset(thisYear[ , -..dateCols]))
	nonaData = justData[!is.na(justData)]
	lengthNonas = length(nonaData)
	gwCounts[i, -1] =
		c(365 * length(which(nonaData > lowRisk)) / lengthNonas,
			365 * length(which(nonaData > medRisk)) / lengthNonas,
			365 * length(which(nonaData > highRisk)) / lengthNonas,
			365 * length(which(nonaData > severeRisk)) / lengthNonas
		)
	gwAverages$Avg_Water_Level[i] = mean(nonaData)
	gwAverages[i, -c(1:2)] = quantile(nonaData, probs = c(0.95, 0.9, 0.75, 0.5, .25, 0.1, 0.05))
}


gwBarplotData = melt(gwCounts, id='Year')
names(gwBarplotData) = c("Year", "Risk_Category", "Days_per_Year")

ggplot(data=gwBarplotData, aes(x=Year, y=Days_per_Year, fill = Risk_Category)) +
	geom_bar(stat="identity", position = position_dodge(width = 0.2)) +
	theme_bw() +
	scale_fill_viridis(option="magma", discrete = TRUE)
	
	
	# adding lines
ylim.prim <- c(0, 366)   # in this example, counts
ylim.sec <- range(gwAverages$Avg_Water_Level)    # in this example, temperature
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]


ggplot() +
	geom_bar(data=gwBarplotData, aes(x=Year, y=Days_per_Year, fill = Risk_Category),
		stat="identity", position = position_dodge(width = 0.2)) +
	geom_line(data = gwAverages, aes(x=Year, y=a + Avg_Water_Level*b, group=1), color = 'red') +
	scale_y_continuous("Days of Risk per Year", sec.axis = sec_axis(~ (. - a)/b, name = "Depth to Groundwater"), trans="reverse") +
	theme_bw() +
	scale_fill_viridis(option="magma", discrete = TRUE)
	


































gwHistoric = fread("J:\\Cai_data\\oneOffs\\localGroundwater.csv")

gwHistoric$Time = as.Date(strftime(gwHistoric$Time, format="%Y-%m-%d"))
startYear = year(first(gwHistoric$Time))
endYear = year(last(gwHistoric$Time))

gwBackfillDates = data.table(Time =seq(ymd(paste0(startYear,"0101")), ymd(paste0(endYear,"1231")), 1))
	
gwBackfill = merge(gwBackfillDates, gwHistoric, all.x=TRUE)
gwBackfill$gwLevel = na.approx(gwBackfill$Depth_to_Water_Below_Land_Surface_in_feet, rule=2)



gwJerjes = fread("J:\\Cai_data\\oneOffs\\final_predictions_2069.csv")
if(any(is.na(gwJerjes[1,]))) {
	theseCols = which(!is.na(gwJerjes[1,]))
	gwJerjes = gwJerjes[ , ..theseCols]
}
names(gwJerjes)[1] = "Time"
gwJerjes$Time = as.Date(gwJerjes$Time)

gwJoseGe_month = fread("J:\\Cai_data\\oneOffs\\monthly_depth_to_water.csv")[,-1]
gwJoseGeDates = data.table(Time = seq(gwJoseGe_month$Month[1], last(gwJoseGe_month$Month), 1))
gwJoseGe = merge(gwJoseGeDates, gwJoseGe_month, by.x="Time", by.y="Month", all.x=TRUE)
for(i in 2:ncol(gwJoseGe)) {
	gwJoseGe[, i] = na.approx(gwJoseGe[, ..i], rule=2)
}

gwCombined = merge(gwJerjes,gwJoseGe, all.x = TRUE, by = "Time")
gwCombined = merge(gwCombined, gwBackfill[,-2], all = TRUE, by = "Time")


gwCombined$Year = year(gwCombined$Time)


gwCounts = data.frame(Year = unique(year(gwCombined$Time)),
	Shallow_Well = NA,
	Deep_Well = NA,
	Very_Deep_Well = NA)
	
gwAverages = data.frame(Year = unique(year(gwCombined$Time)),
	Avg_Water_Level = NA,
	Q95 = NA,
	Q90 = NA,
	Q75 = NA,
	Q50 = NA,
	Q25 = NA,
	Q10 = NA,
	Q05 = NA)
	
medRisk = 800
highRisk = 1000
severeRisk = 1300
for(i in 1:length(gwCounts$Year)){
	thisYear = subset(gwCombined, Year == gwCounts$Year[i])
	dateCols = c(1, ncol(thisYear))
	justData = unlist(subset(thisYear[ , -..dateCols]))
	nonaData = justData[!is.na(justData)]
	lengthNonas = length(nonaData)
	gwCounts[i, -1] =
		c(365 * length(which(nonaData > medRisk)) / lengthNonas,
			365 * length(which(nonaData > highRisk)) / lengthNonas,
			365 * length(which(nonaData > severeRisk)) / lengthNonas
		)
	gwAverages$Avg_Water_Level[i] = mean(nonaData)
	gwAverages[i, -c(1:2)] = quantile(nonaData, probs = c(0.95, 0.9, 0.75, 0.5, .25, 0.1, 0.05))
}


gwBarplotData = melt(gwCounts, id='Year')
names(gwBarplotData) = c("Year", "Risk_Category", "Days_per_Year")




ggplot() +
	geom_line(data = gwBarplotData, aes(x=Year, y=Days_per_Year, color = Risk_Category), size = 1.4) +
	scale_y_continuous("Dry Days per Year") +
	geom_vline(xintercept = last(subset(gwBarplotData, Risk_Category == "Shallow_Well" & Days_per_Year < 35)$Year)) +
	geom_vline(xintercept = subset(gwBarplotData, Risk_Category == "Deep_Well" & Days_per_Year > 18.25)$Year[1]) +
	geom_vline(xintercept = subset(gwBarplotData, Risk_Category == "Very_Deep_Well" & Days_per_Year > 18.25)$Year[1]) +
	theme_bw(base_size=12) +
#	scale_color_viridis(option="magma", discrete = TRUE) +
	scale_colour_wsj("colors6","") 
#	theme_wsj(base_size = 12)
#	theme_economist_white()
	theme(legend.position = c(0.13, 0.85),
		plot.title = element_text(size=18, face="bold"),
        legend.key = element_rect(colour = "transparent", fill = "transparent"))  
		




	# adding lines
costTable = data.table(relCost = c(25, 27.5, 30), Q95 = c(800, 1000, 1300))
costModel = lm(relCost ~ Q95, costTable)
gwAverages$relCost = predict(costModel, data.frame(Q95 = gwAverages$Q95))
gwAverages$Total_Cost = gwAverages$Q95 * gwAverages$relCost
for(i in 2:nrow(gwAverages))	{
	if(gwAverages$Total_Cost[i] < gwAverages$Total_Cost[i-1]){
		gwAverages$Total_Cost[i] = gwAverages$Total_Cost[i-1]
	}
}

ylim.prim <- range(gwAverages$Q95)   # in this example, counts
ylim.sec <- range(gwAverages$Total_Cost)    # in this example, temperature
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]


ggplot() +
	geom_line(data = gwAverages, aes(x=Year, y=Q95), size = 1.4) +
	geom_line(data = gwAverages, aes(x=Year, y=a + Total_Cost*b, group=1), color = 'red') +
	scale_y_continuous("Days of Risk per Year", sec.axis = sec_axis(~ (. - a)/b, name = "Depth to Groundwater"), trans="reverse") +
	theme_bw() +
	scale_fill_viridis(option="magma", discrete = TRUE)
	