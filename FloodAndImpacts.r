

# read in necessary libraries
library(raster)


fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\hist_damAgr'
str_name = 'damageAgr_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\hist_damCom'
str_name = 'damageCom_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)


fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\hist_damInd'
str_name = 'damageInd_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster


plot(imported_raster^0.001)


fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\hist_damInf'
str_name = 'damageInf_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\hist_damRes'
str_name = 'damageRes_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)












fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\hist_damAgr'
str_name = 'damageAgr_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\hist_damCom'
str_name = 'damageCom_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\hist_damInd'
str_name = 'damageInd_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster


plot(imported_raster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\hist_damInf'
str_name = 'damageInf_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\hist_damRes'
str_name = 'damageRes_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)















fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\rcp8p5_damAgr'
str_name = 'damageAgr_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)


fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\rcp8p5_damCom'
str_name = 'damageCom_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\rcp8p5_damInd'
str_name = 'damageInd_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster


plot(imported_raster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\rcp8p5_damInf'
str_name = 'damageInf_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_GFDL_ESM2M\\rcp8p5_damRes'
str_name = 'damageRes_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)























# read in necessary libraries
library(raster)


fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_HadGEM2_ES\\hist_damAgr'
str_name = 'damageAgr_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_HadGEM2_ES\\hist_damCom'
str_name = 'damageCom_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)


fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_HadGEM2_ES\\hist_damInd'
str_name = 'damageInd_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster


plot(imported_raster^0.001)


fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_HadGEM2_ES\\hist_damInf'
str_name = 'damageInf_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_HadGEM2_ES\\hist_damRes'
str_name = 'damageRes_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)











fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_HadGEM2_ES\\rcp8p5_damAgr'
str_name = 'damageAgr_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_HadGEM2_ES\\rcp8p5_damCom'
str_name = 'damageCom_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)




fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_HadGEM2_ES\\rcp8p5_damInd'
str_name = 'damageInd_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster


plot(imported_raster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_HadGEM2_ES\\rcp8p5_damInf'
str_name = 'damageInf_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\DBH_HadGEM2_ES\\rcp8p5_damRes'
str_name = 'damageRes_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)








































# read in necessary libraries
library(raster)


fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\hist_damAgr'
str_name = 'damageAgr_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\hist_damCom'
str_name = 'damageCom_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)


fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\hist_damInd'
str_name = 'damageInd_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster


plot(imported_raster^0.001)


fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\hist_damInf'
str_name = 'damageInf_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\hist_damRes'
str_name = 'damageRes_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)











fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\rcp8p5_damAgr'
str_name = 'damageAgr_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\rcp8p5_damCom'
str_name = 'damageCom_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)




fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\rcp8p5_damInd'
str_name = 'damageInd_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster


plot(imported_raster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\rcp8p5_damInf'
str_name = 'damageInf_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\rcp8p5_damRes'
str_name = 'damageRes_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)































# read in necessary libraries
library(raster)


fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\hist_damAgr'
str_name = 'damageAgr_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\hist_damCom'
str_name = 'damageCom_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)


fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\hist_damInd'
str_name = 'damageInd_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster


plot(imported_raster^0.001)


fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\hist_damInf'
str_name = 'damageInf_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\H08_NorESM1_M\\hist_damRes'
str_name = 'damageRes_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)











fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\JULES_GFDL_ESM2M\\rcp8p5_damAgr'
str_name = 'damageAgr_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\JULES_GFDL_ESM2M\\rcp8p5_damCom'
str_name = 'damageCom_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)




fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\JULES_GFDL_ESM2M\\rcp8p5_damInd'
str_name = 'damageInd_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster


plot(imported_raster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\JULES_GFDL_ESM2M\\rcp8p5_damInf'
str_name = 'damageInf_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\JULES_GFDL_ESM2M\\rcp8p5_damRes'
str_name = 'damageRes_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)










fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\JULES_GFDL_ESM2M\\rcp8p5_damAgr'
str_name = 'damageAgr_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\JULES_GFDL_ESM2M\\rcp8p5_damCom'
str_name = 'damageCom_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)




fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\JULES_GFDL_ESM2M\\rcp8p5_damInd'
str_name = 'damageInd_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster


plot(imported_raster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\JULES_GFDL_ESM2M\\rcp8p5_damInf'
str_name = 'damageInf_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\JULES_GFDL_ESM2M\\rcp8p5_damRes'
str_name = 'damageRes_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)


















fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damAgr'
str_name = 'damageAgr_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damCom'
str_name = 'damageCom_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)




fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damInd'
str_name = 'damageInd_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster


plot(imported_raster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damInf'
str_name = 'damageInf_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damRes'
str_name = 'damageRes_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)










fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damAgr'
str_name = 'damageAgr_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damCom'
str_name = 'damageCom_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)




fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damInd'
str_name = 'damageInd_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster


plot(imported_raster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damInf'
str_name = 'damageInf_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damRes'
str_name = 'damageRes_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)





























fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\VIC_IPSL_CM5A_LR\\hist_damAgr'
str_name = 'damageAgr_global_y_1.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))
plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)
	
sumRaster = imported_raster
for(i in 1:35)	{

	fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\VIC_IPSL_CM5A_LR\\hist_damAgr'
	str_name = paste0('damageAgr_global_y_', i, '.tif')
	imported_raster=raster(paste0(fileLoc, '\\', str_name))

	plot(imported_raster^0.001)

	sumRaster = sumRaster + imported_raster
	plot(sumRaster^0.001)
}
naRaster = sumRaster
naRaster[naRaster==0] = NA
plot(naRaster, col=rainbow(10))

quantRast = quantile(sumRaster, seq(0.01,1,length.out=100))
indRast = sumRaster	; indRast[] = NA
for(i in 1:100)	{
	indRast[sumRaster > quantRast[i]] = i / 100
}

plot(indRast, col=rainbow(3))
	
sumRaster = imported_raster



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\hist_damCom'
str_name = 'damageCom_global_y_10.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)




fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\hist_damInd'
str_name = 'damageInd_global_y_10.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster


plot(imported_raster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\hist_damInf'
str_name = 'damageInf_global_y_10.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\hist_damRes'
str_name = 'damageRes_global_y_10.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)










fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damAgr'
str_name = 'damageAgr_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damCom'
str_name = 'damageCom_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)




fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damInd'
str_name = 'damageInd_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster


plot(imported_raster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damInf'
str_name = 'damageInf_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



fileLoc = 'J:\\Cai_data\\TCFD\\FloodAndImpacts\\output_global_damage\\output_global_damage\\LPJmL_NorESM1_M\\rcp8p5_damRes'
str_name = 'damageRes_global_y_30.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name)) + imported_raster

plot(imported_raster^0.001)
naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster^0.001)



































fileLoc = 'J:\\Cai_data\\TCFD\\CurrentFloodHazard'
str_name = 'floodMapGL_rp100y.tif' 
str_name = 'floodMapGL_rp500y.tif' 
imported_raster=raster(paste0(fileLoc, '\\', str_name))
plot(imported_raster, col='red')

naRaster = imported_raster
naRaster[naRaster==0] = NA
plot(naRaster)
plot(naRaster, col='red')










