# a script for pulling land cover covariate data from CORINE raster data for Ireland at various spatial scales
# load relevant packages
library(raster)
library(landscapemetrics)
library(sf)
library(dplyr)
library(tidyr)

setwd("C:/Users/twininjo/Documents/R/Leighanna_Dawson_marten_ireland")

#read in Corine Ireland and UK raster
ireland <- raster("C:/Users/twininjo/Downloads/Corine_data_Ireland/Results/U2018_CLC2018_V2020_20u1/U2018_CLC2018_V2020_20u1/U2018_CLC2018_V2020_20u1.tif")

# plot it
plot(ireland)

# check classes 
# patches <- get_patches(ireland)

# check out CRS
crs(ireland)

# read in all the cam locations
cams <- read.csv('habitat info 2015 2018 2020 sites only.csv')


# pull out coordinates of cams
cams.sp <- cams
coordinates(cams.sp)<- ~long+lat
st_crs(cams.sp)

#convert foreign object into sf object 
cams.yo <- st_as_sf(cams.sp)

# set that CRS again
st_crs(cams.yo) = 4326

# transform to same CRS at Ireland raster (see crs(Ireland) above)
cams.yo <- st_transform(cams.yo, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

# check they now match up
plot(ireland)
points(cams.yo)

#area of each class for 10km2 buffer 
allsites_10km_buffer <- sample_lsm(ireland, y = cams.yo, what = "lsm_p_area", size = 1784.12)


# check it out
str(allsites_10km_buffer)

#sum the different classes
df <- allsites_10km_buffer %>%
  group_by(plot_id, class) %>%              # group by point (ID) and lc class               # count the number of occurences of each class
  summarise(area = sum(value))%>% 
  spread(class, area)

table(allsites_10km_buffer$plot_id)

unique(allsites_10km_buffer$class)
# bloody hell theres alot of classes

#rename number classes with actual class names
colnames(df)[colnames(df) %in% c("1", "2",  "3", "4", "5",  "6",  "7",  "8",  "9", "10",  "11", "12",
                                 "16", 
                                 "18",
                                 "20","21", 
                                 "23", "24", "25", "26", "27",
                                 "29", 
                                 "31", "32", 
                                 "35",  "36", 
                                 "39",  "40", "41", 
                                 "43", "44")] <-c("Continuous urban fabric", "Discontinuous urban fabric", "Industrial or commercial units", "Road and railway", "Port areas", "Airports", "Mineral extraction sites",
                                                  "Dump sites", "Construction sites", "Green urban area", "Sport and leisure facilities", "Non-irrigated arable land",
                                                  "Fruit trees and berry plantations",
                                                  "Pastures",
                                                  "Complex cultivation patterns","Agriculture with natural vegetation",
                                                  "Broadleaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands", "Moors and heathland",
                                                  "Transitional woodland-shrub",
                                                  "Bare rocks", "Sparesely vegetated areas",
                                                  "Inland marshes", "Peat bogs", 
                                                  "Intertidal flats", "Water courses", "Water bodies",
                                                  "Estuaries", "Sea and Ocean")
                                                  
                                                  
# coerce to dataframe
df <- as.data.frame(df)                                                

# check its behaving
str(df)                          

# set NAs to 0
df[is.na(df)] <- 0

# create total column and sum all columns                                                                                                 
df$total <- rowSums(df[,c("Continuous urban fabric", "Discontinuous urban fabric", "Industrial or commercial units", "Road and railway", "Port areas", "Airports", "Mineral extraction sites",
                          "Dump sites", "Construction sites", "Green urban area", "Sport and leisure facilities", "Non-irrigated arable land",
                          "Fruit trees and berry plantations",
                          "Pastures",
                          "Complex cultivation patterns","Agriculture with natural vegetation",
                          "Broadleaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands", "Moors and heathland",
                          "Transitional woodland-shrub",
                          "Bare rocks", "Sparesely vegetated areas",
                          "Inland marshes", "Peat bogs", 
                          "Intertidal flats", "Water courses", "Water bodies",
                          "Estuaries", "Sea and Ocean")])

#calculate proportions of each class in each buffer
for (i in 2:ncol(df)){
  df[,i]<-df[,i]/df[,"total"]
  
}

# how do they come out
range(df$`Broadleaved forest`)
range(df$`Coniferous forest`)
range(df$Pastures)
range(df$`Mixed forest`)
range(df$`Moors and heathland`)

# combine the proportions with the cams info
test <- cbind(cams, df)

# save out
write.csv(test,"2015-2020_allsites_corinedata_10km2_martenandsquirrelsampling_NI.csv" )



###################### #area of each class for 5km buffer ####################################

# sample lsm again but this time with 5km2 buffer radius (1.261 km2)
allsites_5km_buffer <- sample_lsm(ireland, y = cams.yo, what = "lsm_p_area", size = 1261.57)


#sum the different classes
df <- allsites_5km_buffer %>%
  group_by(plot_id, class) %>%              # group by point (ID) and lc class               # count the number of occurences of each class
  summarise(area = sum(value))%>% 
  spread(class, area)

#check out the sites
table(allsites_5km_buffer$plot_id)

# check out what classes were in the landscape
unique(allsites_5km_buffer$class)

#rename number classes with names 
colnames(df)[colnames(df) %in% c("1", "2",  "3", "4", "5",  "6",  "7",  "8",  "9", "10",  "11", "12",
                                 "16", 
                                 "18",
                                 "20","21", 
                                 "23", "24", "25", "26", "27",
                                 "29", 
                                 "31", "32", 
                                 "35",  "36", 
                                 "39",  "40", "41", 
                                 "43", "44")] <-c("Continuous urban fabric", "Discontinuous urban fabric", "Industrial or commercial units", "Road and railway", "Port areas", "Airports", "Mineral extraction sites",
                                                  "Dump sites", "Construction sites", "Green urban area", "Sport and leisure facilities", "Non-irrigated arable land",
                                                  "Fruit trees and berry plantations",
                                                  "Pastures",
                                                  "Complex cultivation patterns","Agriculture with natural vegetation",
                                                  "Broadleaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands", "Moors and heathland",
                                                  "Transitional woodland-shrub",
                                                  "Bare rocks", "Sparesely vegetated areas",
                                                  "Inland marshes", "Peat bogs", 
                                                  "Intertidal flats", "Water courses", "Water bodies",
                                                  "Estuaries", "Sea and Ocean")



df <- as.data.frame(df)                                                

str(df)                          

df$total <- rowSums(df[,c("Continuous urban fabric", "Discontinuous urban fabric", "Industrial or commercial units", "Road and railway", "Port areas", "Airports", "Mineral extraction sites",
                          "Dump sites", "Construction sites", "Green urban area", "Sport and leisure facilities", "Non-irrigated arable land",
                          "Fruit trees and berry plantations",
                          "Pastures",
                          "Complex cultivation patterns","Agriculture with natural vegetation",
                          "Broadleaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands", "Moors and heathland",
                          "Transitional woodland-shrub",
                          "Bare rocks", "Sparesely vegetated areas",
                          "Inland marshes", "Peat bogs", 
                          "Intertidal flats", "Water courses", "Water bodies",
                          "Estuaries", "Sea and Ocean")])

#calculate proportions of each class 
df[is.na(df)] <- 0

for (i in 2:ncol(df)){
  df[,i]<-df[,i]/df[,"total"]
  
}

# how do they come out
range(df$`Broadleaved forest`)
range(df$`Coniferous forest`)
range(df$Pastures)
range(df$`Mixed forest`)
range(df$`Moors and heathland`)

test <- cbind(cams, df)

write.csv(test,"2015-2020_allsites_corinedata_5km2_martenandsquirrelsampling_NI.csv" )




########## extract covariates across all ireland using 10km2 grid #########################

setwd('C:/Users/twininjo/Documents/ArcGIS/Projects/Ireland_marten')
# read in 10km2 grid shapefile
tenkm_grid<- st_read(dsn = ".", layer = "all_ireland_10km2_grid")

# plot it to see if the slipper fits
plot(ireland)
plot(tenkm_grid, add=TRUE)

# add plot ids to each grid
nrow(tenkm_grid)
tenkm_grid$plot_id <- c(1:9219)


#CORINE 10km2 grid extractions
corine_10km_grid_ireland <- sample_lsm(ireland, tenkm_grid, level = "patch", metric = "area")

write.csv(corine_10km_grid_ireland, 'corine_10km_grid_ireland_rawunformatted.csv')

#sum the different classes
df <- corine_10km_grid_ireland %>%
  group_by(plot_id, class) %>%              # group by point (ID) and lc class               # count the number of occurences of each class
  summarise(area = sum(value))%>% 
  spread(class, area)

table(allsites_10km_buffer$plot_id)

unique(allsites_10km_buffer$class)

#rename number classes with names 
colnames(df)[colnames(df) %in% c("1", "2",  "3", "4", "5",  "6",  "7",  "8",  "9", "10",  "11", "12",
                                 "16", 
                                 "18",
                                 "20","21", 
                                 "23", "24", "25", "26", "27",
                                 "29", 
                                 "31", "32", 
                                 "35",  "36", 
                                 "39",  "40", "41", 
                                 "43", "44")] <-c("Continuous urban fabric", "Discontinuous urban fabric", "Industrial or commercial units", "Road and railway", "Port areas", "Airports", "Mineral extraction sites",
                                                  "Dump sites", "Construction sites", "Green urban area", "Sport and leisure facilities", "Non-irrigated arable land",
                                                  "Fruit trees and berry plantations",
                                                  "Pastures",
                                                  "Complex cultivation patterns","Agriculture with natural vegetation",
                                                  "Broadleaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands", "Moors and heathland",
                                                  "Transitional woodland-shrub",
                                                  "Bare rocks", "Sparesely vegetated areas",
                                                  "Inland marshes", "Peat bogs", 
                                                  "Intertidal flats", "Water courses", "Water bodies",
                                                  "Estuaries", "Sea and Ocean")


# make dataframe and set NAs to 0
df <- as.data.frame(df)                                                
df[is.na(df)] <- 0

str(df)                          

# total column
df$total <- rowSums(df[,c("Continuous urban fabric", "Discontinuous urban fabric", "Industrial or commercial units", "Road and railway", "Port areas", "Airports", "Mineral extraction sites",
                          "Dump sites", "Construction sites", "Green urban area", "Sport and leisure facilities", "Non-irrigated arable land",
                          "Fruit trees and berry plantations",
                          "Pastures",
                          "Complex cultivation patterns","Agriculture with natural vegetation",
                          "Broadleaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands", "Moors and heathland",
                          "Transitional woodland-shrub",
                          "Bare rocks", "Sparesely vegetated areas",
                          "Inland marshes", "Peat bogs", 
                          "Intertidal flats", "Water courses", "Water bodies",
                          "Estuaries", "Sea and Ocean")])


#calculate proportions of each class 
for (i in 2:ncol(df)){
  df[,i]<-df[,i]/df[,"total"]
  
}

# how do they come out
range(df$`Continuous urban fabric`, na.rm=TRUE)
range(df$`Coniferous forest`, na.rm=TRUE)
range(df$`Broadleaved forest`, na.rm=TRUE)

range(df$Pastures)
range(df$`Mixed forest`)
range(df$`Moors and heathland`)

# make it a df
tenkm_grid_df<- data.frame(tenkm_grid)

# dont do this
# test <- cbind(tenkm_grid, df)

# remove the geometry column which is buggering up the csv formatting
check <- merge(df, tenkm_grid, by = "plot_id", all.x = TRUE, all.y =TRUE)

# remove the geometry column which is buggering up the csv formatting
check <- as.data.frame(check[,1:41])

str(check)
str(test) 


write.csv(check,"allireland_corinedata_10km2grid.csv" )
