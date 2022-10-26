library(rnaturalearth)
library(raster)
library(data.table)
library(sf)
sf_use_s2(FALSE) # for error suppression of Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  :
land10 <- ne_download(scale = 10, type = 'land', category = 'physical') #scal50 scale110
sp::plot(land10)
land10Sf = st_as_sf(land10)
	# lons and lats to match the flood rasters

#floodRast = raster('J:\\Cai_data\\TCFD\\CurrentFloodHazard\\floodMapGL_rp100y.tif')
lats = rev(seq(-54.00824,83.2251,length.out=16468))
lons = seq(-166.8, 180, length.out=41616)
#spPoints = st_multipoint(rbind(c(lats[1], lons[1]), c(lats[100],lons[50])))
pointDf = data.frame('lon' = rep(lons, length(lats)),
	'lat' = rep(lats, each=length(lons)))
	# pointDf is too long to allocate to one SF object, so splitting to 4
theseRows = 1:5000000 #685332288
pointSf = st_as_sf(pointDf[theseRows,], coords = c('lon', 'lat'))
st_crs(pointSf) = st_crs(land10)
goodPoints = data.frame(row1 = rep(NA, nrow(pointSf)))
goodPoints[,1]  = st_intersects(pointSf, land10Sf[1,], sparse=FALSE)[,1]
goodPoints[,2] = st_intersects(pointSf, land10Sf[2,], sparse=FALSE)[,1]
goodPoints[,3] = st_intersects(pointSf, land10Sf[3,], sparse=FALSE)[,1]
goodPoints[,4] = st_intersects(pointSf, land10Sf[4,], sparse=FALSE)[,1]
goodPoints[,5] = st_intersects(pointSf, land10Sf[5,], sparse=FALSE)[,1]
goodPoints[,6] = st_intersects(pointSf, land10Sf[6,], sparse=FALSE)[,1]
goodPoints[,7] = st_intersects(pointSf, land10Sf[7,], sparse=FALSE)[,1]
goodPoints[,8] = st_intersects(pointSf, land10Sf[9,], sparse=FALSE)[,1]
goodPoints[,9] = st_intersects(pointSf, land10Sf[10,], sparse=FALSE)[,1]
goodPoints[,10] = st_intersects(pointSf, land10Sf[11,], sparse=FALSE)[,1]
goodPoints[,11] = apply(goodPoints, 1, any)
goodRows = theseRows[goodPoints[,11]]

iter = 1
write.csv(goodRows, paste0('goodRows', iter, '.csv'))
#sp::plot(goodPointsSf)
	# repeating for all news rows
theseRows = theseRows + 5000000 #685332288
maxRow = 685332288
while(theseRows[1] <= 685332288)	{
	if(last(theseRows) > maxRow)	{
		theseRows = seq(theseRows[1], maxRow, 1)
	}
	print(theseRows[1] / maxRow)
	pointSf = st_as_sf(pointDf[theseRows,], coords = c('lon', 'lat'))
	st_crs(pointSf) = st_crs(land10)
	goodPoints = data.frame(row1 = rep(NA, nrow(pointSf)))
	goodPoints[,1]  = st_intersects(pointSf, land10Sf[1,], sparse=FALSE)[,1]
	goodPoints[,2] = st_intersects(pointSf, land10Sf[2,], sparse=FALSE)[,1]
	goodPoints[,3] = st_intersects(pointSf, land10Sf[3,], sparse=FALSE)[,1]
	goodPoints[,4] = st_intersects(pointSf, land10Sf[4,], sparse=FALSE)[,1]
	goodPoints[,5] = st_intersects(pointSf, land10Sf[5,], sparse=FALSE)[,1]
	goodPoints[,6] = st_intersects(pointSf, land10Sf[6,], sparse=FALSE)[,1]
	goodPoints[,7] = st_intersects(pointSf, land10Sf[7,], sparse=FALSE)[,1]
	goodPoints[,8] = st_intersects(pointSf, land10Sf[9,], sparse=FALSE)[,1]
	goodPoints[,9] = st_intersects(pointSf, land10Sf[10,], sparse=FALSE)[,1]
	goodPoints[,10] = st_intersects(pointSf, land10Sf[11,], sparse=FALSE)[,1]
	goodPoints[,11] = apply(goodPoints, 1, any)
	goodRows = theseRows[goodPoints[,11]]
	#goodRows = c(goodRows, theseRows[goodPoints[,11]])
	#write.table(goodRows, 'goodRows2.txt')
	#saveRDS(goodRows, 'goodRows2.rds')
	#gg=readRDS('goodRows2.rds')
	iter = iter + 1
	write.csv(goodRows, paste0('goodRows', iter, '.csv'))

	theseRows = theseRows + 5000000 #685332288
	#sp::plot(goodPointsDf)
}




dataOutArray = array(rep(NA, length(lons) * length(lats)), 
	dim = c(length(lons), length(lats)))


for(i in 1:138)	{
	theseGoodRows = read.csv(paste0('goodRows', i, '.csv'))$x
	print(theseGoodRows[1])
	dataOutArray[theseGoodRows] = 1
}
	
saveRDS(dataOutArray, 'J:\\Cai_data\\TCFD\\CurrentFloodHazard\\LandMaskArray.rds')



# inspecting output
testArray = readRDS('J:\\Cai_data\\TCFD\\CurrentFloodHazard\\LandMaskArray.rds')[10:50,5:6]

image(testArray[4001:60000, 4001:6000])

#land
thisLat = 35.599993
thisLon = -76.010066
chsLat = which.min(abs(lats - thisLat))
chsLon = which.min(abs(lons - thisLon))
testArray[chsLon, chsLat]

#ocean
thisLat = 35.340518
thisLon = -75.897456
chsLat = which.min(abs(lats - thisLat))
chsLon = which.min(abs(lons - thisLon))
testArray[chsLon, chsLat]

#lake
thisLat = 35.498218
thisLon = -76.176858
chsLat = which.min(abs(lats - thisLat))
chsLon = which.min(abs(lons - thisLon))
testArray[chsLon, chsLat]

#barrier island
thisLat = 35.251559
thisLon = -75.554975
chsLat = which.min(abs(lats - thisLat))
chsLon = which.min(abs(lons - thisLon))
testArray[chsLon, chsLat]

#mouth of river
thisLat = 35.390320
thisLon = -76.693724
chsLat = which.min(abs(lats - thisLat))
chsLon = which.min(abs(lons - thisLon))
testArray[chsLon, chsLat]

#bahamas
thisLat = 24.691765
thisLon = -78.011630
chsLat = which.min(abs(lats - thisLat))
chsLon = which.min(abs(lons - thisLon))
testArray[chsLon, chsLat]

#ocean by bahamas
thisLat = 24.950897
thisLon = -77.702877
chsLat = which.min(abs(lats - thisLat))
chsLon = which.min(abs(lons - thisLon))
testArray[chsLon, chsLat]

#great lakes
thisLat = 42.951150
thisLon = -87.108683
chsLat = which.min(abs(lats - thisLat))
chsLon = which.min(abs(lons - thisLon))
testArray[chsLon, chsLat]

#chesapeake bay
thisLat = 39.501189
thisLon = -76.031681
chsLat = which.min(abs(lats - thisLat))
chsLon = which.min(abs(lons - thisLon))
testArray[chsLon, chsLat]


