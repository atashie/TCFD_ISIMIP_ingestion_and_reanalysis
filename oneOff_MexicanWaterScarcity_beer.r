library(sf)
library(ggplot2)
library(data.table)
library(rnaturalearth)
library(rnaturalearthdata)



sm_loc = "J:\\Cai_Data\\TCFD\\RootZoneSoilMoisture\\rzsm_allData_30MAY2022.csv"
ws_loc = "J:\\Cai_Data\\TCFD\\WaterScarcity\\WaterScarcity_allData_03JUN2022.csv"
gw_loc = "J:\\Cai_Data\\TCFD\\GWstorage\\groundwater_nonegs_allData_30MAY2022.csv"
smDat = st_as_sf(fread(sm_loc), coords=c('Lon','Lat'), remove=FALSE, crs=4326, agr='constant')
wsDat = st_as_sf(fread(ws_loc), coords=c('Lon','Lat'), remove=FALSE, crs=4326, agr='constant')
gwDat = st_as_sf(fread(gw_loc), coords=c('Lon','Lat'), remove=FALSE, crs=4326, agr='constant')



world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
#head(states)
oceans <- expand.grid(lon = c(-118,-85), lat = c(14, 33))


cityLocs_sf = st_as_sf(
	data.frame(
		company = c(rep('Femsa', 8), rep('Modelo', 7), 'Constelation'),
		city = c('Tecate','Navojoa','Chihuahua','Monterrey','Guadalajara','Toluca' ,'Orizaba' ,'Merida','Cd_Obregon','Torreon','Mazatlan','Zacatecas','Guadalajara','Mexico_City','Oaxaca','Nava'),
		lat = c(32.4099  ,27.1885  ,28.9719    ,25.685552  ,20.481622    ,19.28487 ,18.85389  ,20.9643 ,28.006      ,25.543641,23.286958 ,22.767550  ,20.9383      ,19.45        ,17.06948,28.8565),
		lon = c(-116.078 ,-109.662 ,-105.663   ,-100.323704,-103.346057  ,-99.65679,-97.095565,-89.6116,-110.233    ,-103.3974,-106.3532 ,-102.578834,-103.346057  ,-99.13816    ,-96.7309,-101.18059)
	),
	coords = c("lon", "lat"),
	remove = FALSE, crs = 4326, agr = "constant")


thisHaz = subset(wsDat, Lon < -85 & Lon > -118 & Lat < 33 & Lat > 14 & Decade =='2020s')
thisHaz$plotDat = as.numeric(thisHaz$Q_50_indx)
thisHaz$plotDat[thisHaz$plotDat > quantile(thisHaz$plotDat, 0.99)] = quantile(thisHaz$plotDat, 0.99)
thisHaz$plotDat[thisHaz$plotDat < quantile(thisHaz$plotDat, 0.01)] = quantile(thisHaz$plotDat, 0.05)*.5
thisHaz$plotDat = thisHaz$plotDat + abs(min(thisHaz$plotDat))
thisHaz$plotDat = (1 - (thisHaz$plotDat / max(thisHaz$plotDat)))^.07


ggplot() +
	geom_sf(data = subset(thisHaz, Decade == '2020s'), size=4.6, shape=15, aes(col=plotDat)) +
	 #   geom_sf(data = counties, fill = NA, color = gray(.5)) +
#	scale_colour_viridis_c(alpha = 1) +
	scale_colour_gradientn(colours=c('#F2F3F3', '#F2F3F3', '#FFF2F2', '#FFE5E5', 'orangered1', 'red1','red3','firebrick'),name='Water Scarcity') +
#	scale_colour_gradient(low='#F2F3F3', high='#B91863') +
	geom_sf(data = world, fill = NA, color = gray(0.75), size=1.4) +
	geom_sf(data = world, fill = NA, color = gray(0.05), size=0.8) +
	geom_sf(data = mask, fill='grey98', color=NA) +
	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
#	theme(panel.grid = element_line(colour='grey20')) +
	theme(legend.position = c(0.84, 0.74),
		legend.background = element_rect(fill='white', colour=NA)) +
	coord_sf(xlim = c(-118, -86.7), ylim = c(14.8, 32.63), expand = FALSE)
#	



borders = subset(world, name_sort=='Mexico')

mask <- borders %>%
st_bbox() %>%
st_as_sfc() %>%
st_buffer(5) %>%
st_difference(borders) %>%
st_as_sf()





basemap(data = oceans, bathymetry = FALSE) +
    geom_sf() +
geom_sf(data = subset(thisHaz, Decade == '2020s'), size=2.1, shape=15, aes(col=plotDat)) +
 #   geom_sf(data = counties, fill = NA, color = gray(.5)) +
scale_colour_viridis_c(alpha = 1) +
geom_sf(data = cityLocs_sf, size=4, shape=24, fill='darkred') +
basemap(data = oceans, bathymetry =FALSE)

geom_sf(data = world, fill = NA, color = gray(0.75), size=2) +
theme(panel.background = element_rect(fill='grey90'),
panel.grid = element_line(colour=NA)) +
    coord_sf(xlim = c(-118, -85), ylim = c(14, 33), expand = FALSE)














thisHaz = subset(gwDat, Decade=='2020s' & Lon < -85 & Lon > -120 & Lat < 33 & Lat > 14)
thisHaz$plotDat = as.numeric(thisHaz$Q_50_indx)
thisHaz$plotDat = thisHaz$plotDat + abs(min(thisHaz$plotDat))
thisHaz$plotDat = 1 - thisHaz$plotDat / max(thisHaz$plotDat)

ggplot(data = world) +
    geom_sf() +
geom_sf(data = cityLocs_sf, size=4, shape=24, fill='darkred') +
geom_sf(data = subset(thisHaz, Decade == '2020s'), size=2.00, shape=15, aes(col=plotDat)) +
 #   geom_sf(data = counties, fill = NA, color = gray(.5)) +
scale_colour_viridis_c(trans='sqrt',alpha = 1) +
geom_sf(data = world, fill = NA, color = gray(0.75)) +
    coord_sf(xlim = c(-120, -85), ylim = c(14, 33), expand = FALSE)


	
	
	
