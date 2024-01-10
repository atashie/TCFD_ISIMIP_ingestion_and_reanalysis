customerFolder_input = "J:\\Cai_data\\Rabo\\Locations\\NuveenAus_Dec2023"
clientName_input = "NuveenAus"
allTable = data.table::fread("J:\\Cai_data\\Rabo\\Locations\\NuveenAus_Dec2023\\nuveenLocsFromKmz_temp_precip_hazards_dec_20.csv")
precipTable = subset(allTable, Hazard == "Intense Precipitation")
scenarioNames = unique(precipTable$Scenario)
allDecades = unique(precipTable$Decade)
allLocs = unique(precipTable$Location)
precipAdvMsrs = unique(precipTable$Advanced_Data_Measures)

for(thisLoc in 1:length(allLocs))	{
	for(thisScen in 1:length(scenarioNames))	{
		png(paste0(customerFolder_input, '\\',  allLocs[thisLoc], '_', scenarioNames[thisScen], "_IntensePrecip.png"), width=500, height=800)
		par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(3,1), font.lab=1.6, bty='l', cex.lab=1.5*1.8, cex.axis=2.0*1.4, cex.main=1.5*1.8, col='#1A232F')
		windowsFonts(A = windowsFont("Roboto"))
		for(thisIndexVal in 1:length(precipAdvMsrs))	{
			thisData = subset(precipTable, Location == allLocs[thisLoc] & Scenario == scenarioNames[thisScen] & Advanced_Data_Measures == precipAdvMsrs[thisIndexVal])
			plotRange = c(0, max(thisData$Raw_Hazard_Value_75th))
			plot(allDecades, thisData$Raw_Hazard_Value,  
				ylim = plotRange, #log='y',
				type='l', lwd=1, col='white', xaxt = 'n', #log='y',
				main='', ylab='', xlab='',
				col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
				family='A')
			abline(h=mean(thisData$Raw_Hazard_Value[1:2]), lwd=2, lty =2, col='#1A232F')
			axis(1, at = allDecades ,col.lab='#1A232F', col.axis='#666D74', 
				labels = allDecades)
		#	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
			polygon(x=c(allDecades, rev(allDecades)), y=c(thisData$Raw_Hazard_Value_25th, rev(thisData$Raw_Hazard_Value_75th)),
				col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
			loessSmooth = loess(thisData$Raw_Hazard_Value ~ allDecades)
		#			lines(allDecades, waterIndexDataPlot[thisLoc, , 17, thisScen, thisIndexVal], 
		#				col='#54575a', lwd=5)	#4cbfad
			lines(allDecades, predict(loessSmooth),
				col='#EE6222', lwd=3)
			text(allDecades[1], plotRange[2] * .9,  paste0("Days per yr > ", precipAdvMsrs[thisIndexVal], ' mm precip'), adj = c(0,0), cex=2.05)
			text(allDecades[1], plotRange[2] * .8,  paste0("Change of ", round(thisData$Long_Term_Trend_Strength[1], 2), ' days per decade'), adj = c(0,0), cex=2.05)
		#			lines(allDecades, nc_testDat[thisLon, thisLat, , 1, 1], 
		#				col='#4cbfad', lwd=3) #015f6f
		}
		dev.off()
	}
}

