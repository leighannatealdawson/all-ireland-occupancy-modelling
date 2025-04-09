###########LOAD ALL THE PACKAGES YOU CAN THINK OF #######################

#if you don't have these packages install them with install.package('nameofpackage')

library(raster)
library(landscapemetrics)
library(rgdal)
library(rgdal)
library(maptools)
library(sp)
library(rgeos)
library(plyr)
library(purrr)
library(tidyr)
library(dplyr)

#set working directory - where you keep your stuff
setwd("C:/Users/jpt93/Documents/R/Nepal")

###########LOAD IN DATA #########################
#read in Land cover data
NepalLCM <- raster('C:/Users/jpt93/Documents/ArcGISPro/Projects/MyProject/Nepal/lcm_2019/LandCover_NP_2019.tif')
#source: http://rds.icimod.org/Home/DataDetail?metadataId=1972729

#read in digital elevation model data
NepalDEM <- raster('C:/Users/jpt93/Documents/ArcGISPro/Projects/MyProject/Nepal/srtm_cgiar_nepal_boundary.img')
#source: https://data.humdata.org/dataset/nepal-digital-model-elevation-dem?

#read in human population density data
Nepalhumanpop <- raster('C:/Users/jpt93/Documents/ArcGISPro/Projects/MyProject/Nepal/npl_pd_2020_1km.tif')
#source: https://hub.worldpop.org/geodata/summary?id=42712 

#read in road density data
Nepalroaddensity <- raster('C:/Users/jpt93/Documents/ArcGISPro/Projects/MyProject/Nepal/YT_mapping/all_roads_1km_densit_1.tif')

#load site data
cams <- read.csv('C:/Users/jpt93/Documents/ArcGISPro/Projects/MyProject/Nepal/sitecoords_Lambert_Conformal_Conic_Survey_Nepal.csv')
#sent in email 

########### DO SOME FORMATTING AND ORGANIZATION ################
#give sites ID numbers
cams$plot_id <- 1:70

#copy DF to project coordinates both in Lambert Conformal Conic Survey, and in WGS84 (could transform them but I am lazy)
cams.sp <- cams
cams.dem <- cams
coordinates(cams.sp)<- ~X+Y
coordinates(cams.dem) <- ~LONG+LAT

plot(cams.sp)
#plot landcover and XY of sites (make sure sites are being plotted where you expect)
plot(NepalLCM)
plot(cams.sp, add=TRUE)

#plot DEM and lat-long (WGS84) of sites (make sure sites are being plotted where you expect)
plot(NepalDEM)
plot(cams.dem, add=TRUE)


#plot human pop den map and lat-long (WSG84) of sites (make sure sites are being plotted where you expect)
plot(Nepalhumanpop)
plot(cams.dem, add=TRUE)

#plot road density and lat-long (WSG84) of sites (make sure sites are being plotted where you expect)
plot(Nepalhumanpop)
plot(cams.dem, add=TRUE)


#############LAND COVER CLASS CALCS##########################################
#calculate landcover patch areas per 5km2 buffer around each site using sample_lsm

LCM_5kmbuffer_allsites <- sample_lsm(NepalLCM, cams.sp, size = 1261.57, level = "patch", metric = "area")


df <- LCM_5kmbuffer_allsites %>%
  group_by(plot_id, class) %>%              # group by point (ID) and lc class               # count the number of occurences of each class
  summarise(area = sum(value))  %>% 
  spread(class, area)


#rename number classes with names
# colnames(df)[colnames(df) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")] c("water", "snow", "bare_soil", "forest", "riverbed", "urban", "croplands", "glaciers", "bare_rock", "grassland", "scrub")

colnames(df)[colnames(df) %in% c("1", "4", "5", "6", "7", "10", "11")] <-c("water", "forest", "riverbed", "urban", "croplands", "grassland", "scrub")


#create total sum column
df$total <- rowSums(df[,c("water", "forest", "riverbed", "urban", "croplands", "grassland", "scrub")], na.rm = TRUE)

#calculate proportions of each class 
df[is.na(df)] <- 0
df[,"water"]<-df[,"water"]/df[,"total"]
df[,"forest"]<-df[,"forest"]/df[,"total"]
df[,"riverbed"]<-df[,"riverbed"]/df[,"total"]
df[,"urban"]<-df[,"urban"]/df[,"total"]
df[,"croplands"]<-df[,"croplands"]/df[,"total"]
df[,"grassland"]<-df[,"grassland"]/df[,"total"]
df[,"scrub"]<-df[,"scrub"]/df[,"total"]

#merge with cam data
lcm_df <- merge(df, cams, by = "plot_id")

#save out
write.csv(lcm_df, "Nepal_Midlands_5km2sitebuffer_LCM.csv")

#check a few ranges
range(df$urban) # 0.00000000 0.01890756

#plot it
hist(df$urban)

#check forest
range(df$forest) # 0.1778711 0.8911765

#plot it
hist(df$forest)

####################### CALCULATE FOREST EDGE ########################################
# calculate forest edge in 5km buffers around sites
edge_5km_sites <- sample_lsm(NepalLCM, cams.sp, size = 1261.57,, level = "class", metric = "ed")
#subset out forest edge (class 4)
edge_5km_sites_forestedge <- subset(edge_5km_sites, class == 4)

#merge with site data
edge_df <- merge(edge_5km_sites_forestedge, cams, by = "plot_id", all.y =TRUE)

#check range of forest edge values
range(edge_df$value)

#plot it
hist(edge_df$value)


write.csv(check, "Nepal_Midlands_5km2sitebuffer_forestedgedensity.csv")

#######################HUMAN POPULATION DENSITY TIME #########################
#calculate mean population density per 5km2 buffer around each site

humanden <- raster::extract(Nepalhumanpop,             # raster layer
                            cams.dem,   # SPDF with centroids for buffer
                            buffer = 1261.57,     # buffer size, units depend on CRS
                            fun=mean,         # what to value to extract
                            df=TRUE)         # return a dataframe? 

#add same site IDs
humanden$plot_id <- 1:70


#merge with original sites data
humanden_df <- merge(humanden, cams, by = "plot_id")

#check out range of values 
range(humanden_df$npl_pd_2020_1km) # 63.18069 1053.50833

#plot it
hist(humanden_df$npl_pd_2020_1km)

#save out
write.csv(humanden_df, "Nepal_Midlands_5km2buffersites_humanpopulationden.csv")


####################### ROAD DENSITY LETS GO #########################
#calculate mean road density per 5km2 buffer around each site

roadden <- raster::extract(Nepalroaddensity,             # raster layer
                            cams.dem,   # SPDF with centroids for buffer
                            buffer = 1261.57,     # buffer size, units depend on CRS
                            fun=mean,         # what to value to extract
                            df=TRUE)         # return a dataframe? 

#add same site IDs
roadden$plot_id <- 1:70


#merge with original sites data
roadden_df <- merge(roadden, cams, by = "plot_id")

#check out range of values 
range(roadden_df$all_roads_1km_densit_1) # 166.7848 401.5698

#plot it
hist(roadden_df$all_roads_1km_densit_1)

#save out
write.csv(roadden_df, "Nepal_Midlands_5km2buffersites_roadden.csv")


################ ELEVATION #######################
##calculate mean elevation per 5km2 buffer around each site
#note we want a 5km2 circle, for this we need to calculate and input the radius (1.26157 km)
elevation <- raster::extract(NepalDEM,             # raster layer
                             cams.dem,   # SPDF with centroids for buffer
                             buffer = 1261.57,     # buffer size, units depend on CRS
                             fun=mean,         # what to value to extract
                             df=TRUE)         # return a dataframe? 

#add same site IDs 
elevation$plot_id <- 1:70


#merge with original sites data
elevation_df <- merge(elevation, cams, by = "plot_id")

#check out range of values 
range(elevation$srtm_cgiar_nepal_boundary) #[1]  469.0571, 1331.5748

#save out
write.csv(elevation_df, "Nepal_Midlands_5km2buffersites_elevation.csv")


##### extractions for spatially explicit predictions into state space ####

fivekmgrid <- readOGR(dsn = ".", layer = "nepal_midhills_5kmgrid_15kmbuffer_statespace")
fivekmgrid$plot_id <- c(1:635)

#transform the grid vector into same CRS as LCM data
fivekmgridreprojected <- spTransform(fivekmgrid, crs(NepalLCM))

#plot landcover and XY of sites (make sure sites are being plotted where you expect)
plot(NepalLCM)
plot(fivekmgridreprojected, add=TRUE)

#plot DEM and lat-long (WGS84) of sites (make sure sites are being plotted where you expect)
plot(NepalDEM)
plot(fivekmgrid, add=TRUE)


#plot human pop den map and lat-long (WSG84) of sites (make sure sites are being plotted where you expect)
plot(Nepalhumanpop)
plot(fivekmgrid, add=TRUE)



###### LANDCOVER CLASS EXTRACTIONS AT 5km2 grid scale ##############


LCM_5km2_grid <- sample_lsm(NepalLCM, fivekmgridreprojected, level = "patch", metric = "area")


df <- LCM_5km2_grid %>%
  group_by(plot_id, class) %>%              # group by point (ID) and lc class               # count the number of occurences of each class
  summarise(area = sum(value))  %>% 
  spread(class, area)

unique(df$class)
#rename number classes with names
# colnames(df)[colnames(df) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")] c("water", "snow", "bare_soil", "forest", "riverbed", "urban", "croplands", "glaciers", "bare_rock", "grassland", "scrub")

colnames(df)[colnames(df) %in% c("1", "4", "5", "6", "7", "10", "11")] <-c("water", "forest", "riverbed", "urban", "croplands", "grassland", "scrub")


#create total sum column
df$total <- rowSums(df[,c("water", "forest", "riverbed", "urban", "croplands", "grassland", "scrub")], na.rm = TRUE)

#calculate proportions of each class 
df[is.na(df)] <- 0
df[,"water"]<-df[,"water"]/df[,"total"]
df[,"forest"]<-df[,"forest"]/df[,"total"]
df[,"riverbed"]<-df[,"riverbed"]/df[,"total"]
df[,"urban"]<-df[,"urban"]/df[,"total"]
df[,"croplands"]<-df[,"croplands"]/df[,"total"]
df[,"grassland"]<-df[,"grassland"]/df[,"total"]
df[,"scrub"]<-df[,"scrub"]/df[,"total"]

#merge with cam data
lcm_grid_df <- merge(df, fivekmgrid, by = "plot_id")

#save out
write.csv(lcm_grid_df, "Nepal_Midhills_5km2grid_LCM.csv")


###### FOREST EDGE EXTRACTIONS AT 5km2 grid scale ################


edge_5km_grid <- sample_lsm(NepalLCM, fivekmgridreprojected, level = "class", metric = "ed")

edge_5km_grid_forest <- subset(edge_5km_grid, class == 4)

#merge with site data
edge_grid_df <- merge(edge_5km_grid_forest, fivekmgrid, by = "plot_id", all.y =TRUE)

#check range of forest edge values
range(edge_grid_df$value, na.rm=TRUE)
range(edge_)

#save it
write.csv(edge_grid_df, "Nepal_midhills_5km2_grid_forestedge.csv")



######### Human population density at 5km2 grid scale #################

#calculate mean population density per 5km2 buffer around each site

humanden_5km_grid <- raster::extract(Nepalhumanpop,             # raster layer
                                     fivekmgrid,   # SPDF with centroids for buffer
                                     fun=mean,         # what to value to extract
                            df=TRUE)         # return a dataframe? 

#add same site IDs
humanden_5km_grid$plot_id <- 1:635


#merge with original sites data
humanden_grid_df <- merge(humanden_5km_grid, fivekmgrid, by = "plot_id")

#check out range of values 
range(humanden_grid_df$npl_pd_2020_1km) # 63.18069 1053.50833

#plot it
hist(humanden_grid_df$npl_pd_2020_1km)

#save out
write.csv(humanden_grid_df, "Nepal_Midhills_5km2grid_humanpopulationden.csv")




###### LANDCOVER CLASS EXTRACTIONS AT 0.5km2 grid scale ##############


zeropointfivekmgrid <- readOGR(dsn = ".", layer = "nepal_midhills_0point5kmgrid_15kmbuffer_statespace")
zeropointfivekmgrid$plot_id <- c(1:5986)

#transform the grid vector into same CRS as LCM data
zeropointfivekmgridreprojected <- spTransform(zeropointfivekmgrid, crs(NepalLCM))

#plot landcover and XY of sites (make sure sites are being plotted where you expect)
plot(NepalLCM)
plot(zeropointfivekmgridreprojected, add=TRUE)

#plot human pop den map and lat-long (WSG84) of sites (make sure sites are being plotted where you expect)
plot(Nepalhumanpop)
plot(zeropointfivekmgrid, add=TRUE)


LCM_0.5km2_grid <- sample_lsm(NepalLCM, zeropointfivekmgridreprojected, level = "patch", metric = "area")


df <- LCM_0.5km2_grid %>%
  group_by(plot_id, class) %>%              # group by point (ID) and lc class               # count the number of occurences of each class
  summarise(area = sum(value))  %>% 
  spread(class, area)

unique(df$class)
#rename number classes with names
# colnames(df)[colnames(df) %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")] c("water", "snow", "bare_soil", "forest", "riverbed", "urban", "croplands", "glaciers", "bare_rock", "grassland", "scrub")

colnames(df)[colnames(df) %in% c("1", "4", "5", "6", "7", "10", "11")] <-c("water", "forest", "riverbed", "urban", "croplands", "grassland", "scrub")


#create total sum column
df$total <- rowSums(df[,c("water", "forest", "riverbed", "urban", "croplands", "grassland", "scrub")], na.rm = TRUE)

#calculate proportions of each class 
df[is.na(df)] <- 0
df[,"water"]<-df[,"water"]/df[,"total"]
df[,"forest"]<-df[,"forest"]/df[,"total"]
df[,"riverbed"]<-df[,"riverbed"]/df[,"total"]
df[,"urban"]<-df[,"urban"]/df[,"total"]
df[,"croplands"]<-df[,"croplands"]/df[,"total"]
df[,"grassland"]<-df[,"grassland"]/df[,"total"]
df[,"scrub"]<-df[,"scrub"]/df[,"total"]

#merge with cam data
lcm_grid_df <- merge(df, zeropointfivekmgrid, by = "plot_id")

#save out
write.csv(lcm_grid_df, "Nepal_Midhills_0.5km2grid_LCM.csv")


###### FOREST EDGE EXTRACTIONS AT 5km2 grid scale ################


edge_0.5km_grid <- sample_lsm(NepalLCM, zeropointfivekmgrid, level = "class", metric = "ed")

edge_0.5km_grid_forest <- subset(edge_0.5km_grid, class == 4)

#merge with site data
edge_grid_df <- merge(edge_0.5km_grid_forest, zeropointfivekmgrid, by = "plot_id", all.y =TRUE)

#check range of forest edge values
range(edge_grid_df$value, na.rm=TRUE)
range(edge_)

#save it
write.csv(edge_grid_df, "Nepal_midhills_0.5km2_grid_forestedge.csv")



######### Human population density at 5km2 grid scale #################

#calculate mean population density per 5km2 buffer around each site

humanden_0.5km_grid <- raster::extract(Nepalhumanpop,             # raster layer
                                       zeropointfivekmgrid,   # SPDF with centroids for buffer
                                     fun=mean,         # what to value to extract
                            df=TRUE)         # return a dataframe? 

#add same site IDs
humanden_0.5km_grid$plot_id <- 1:5986


#merge with original sites data
humanden_grid_df <- merge(humanden_0.5km_grid, zeropointfivekmgrid, by = "plot_id")

#check out range of values 
range(humanden_grid_df$npl_pd_2020_1km) # 63.18069 1053.50833

#plot it
hist(humanden_grid_df$npl_pd_2020_1km)

#save out
write.csv(humanden_grid_df, "Nepal_Midhills_0.5km2grid_humanpopulationden.csv")

############################# CHECKING OUT SOME STUFF#######################

#Check out distributions of different landcover classes
# hist(df$forest)
# hist(df$scrub)
# hist(df$water)
# hist(df$riverbed)
# hist(df$urban)
# hist(df$croplands)
# 
# par(mfrow=c(3,2))
# 
# hist(df$forest, xlim=c(0,1), breaks = 20,  main = "Proportion of forest cover represented in sample",
#      xlab = "Proportion of forest cover represented in sample",
#      ylab = "Count")
# range(df$forest, na.rm =TRUE)
# mtext("range = 0.2-0.87")
# 
# hist(df$croplands, xlim=c(0,1), breaks = 20,  main = "Proportion of croplands cover represented in sample",
#      xlab = "Proportion of croplands cover represented in sample",
#      ylab = "Count")
# range(df$croplands, na.rm =TRUE)
# mtext("range = 0.08-0.68")
# 
# hist(df$scrub, xlim=c(0,1), breaks = 20,  main = "Proportion of scrub cover represented in sample",
#      xlab = "Proportion of scrub cover represented in sample",
#      ylab = "Count")
# range(df$scrub, na.rm =TRUE)
# mtext("range = 0-0.11")
# 
# hist(df$urban, xlim=c(0,1), breaks = 20,  main = "Built-up lands across landscape in Midlands of Nepal",
#      xlab = "Proportion of built-up lands represented in sample",
#      ylab = "Count")
# range(df$urban, na.rm =TRUE)
# mtext("range = 0-0.02")
# 
# 
# hist(df$riverbed, xlim=c(0,1), breaks = 20,  main = "Riverbeds across landscape in Midlands of Nepal",
#      xlab = "Proportion of riverbeds cover represented in sample",
#      ylab = "Count")
# range(df$riverbeds, na.rm =TRUE)
# mtext("range = 0-0.02")
# 
# hist(df$grassland, xlim=c(0,1), breaks = 20,  main = "Grassland across landscape in Midlands of Nepal",
#      xlab = "Proportion of grassland cover represented in sample",
#      ylab = "Count")
# range(df$grassland, na.rm =TRUE)
# mtext("range = 0-0.02")

