#compiling the data variables 
#ending up with 1 df with all the variables needed for the analysis (corrine, populations etc)

# Load necessary libraries
library(sf)

# import data 




############################################################################################
#'# Corine Data
#' Import corine data
corine1km <- read.csv("1.data/1.3.processedinarc/1kmwidecorine.csv")
corine10km <- read.table("1.data/1.3.processedinarc/10kmwidecorine.csv", sep = ",", header = TRUE)
# View data 
View(corine1km)
View(corine10km)

#convert percentages to proportions for every column in the corine1km data set
corine1km[, grep("^(X|t)", colnames(corine1km))] <- corine1km[
    , grep("^(X|t)", colnames(corine1km))] / 100

corine10km[, grep("^(X|t)", colnames(corine10km))] <- corine10km[
    , grep("^(X|t)", colnames(corine10km))] / 100 ### >>> g=big issue with 10km corrine data, totals not adding to 100% or 1. 

############################################################################################
# Summarise the corine data by large categories 

#create Artificial surfaces column (sum of all artificial surfaces CLC doe start with 1) 
corine1km$artificial_surfaces <- rowSums(corine1km[, grep("^X1", names(corine1km))], na.rm = TRUE)
corine10km$artificial_surfaces <- rowSums(corine10km[, grep("^X1", names(corine10km))], na.rm = TRUE)

#create agricultural surfaces column (sum of all agricultural surfaces CLC does start with 2)
corine1km$agricultural_surfaces <- rowSums(corine1km[, grep("^X2", names(corine1km))], na.rm = TRUE)
corine10km$agricultural_surfaces <- rowSums(corine10km[, grep("^X2", names(corine10km))], na.rm = TRUE)

#create forested areas column (sum of all forested areas CLC does start with 3)
corine1km$forested_and_semi_natural  <- rowSums(corine1km[, grep("^X3", names(corine1km))], na.rm = TRUE)
corine10km$forested_and_semi_natural <- rowSums(corine10km[, grep("^X3", names(corine10km))], na.rm = TRUE)

#create wetlands column (sum of all wetlands CLC does start with 4)
corine1km$wetlands <- rowSums(corine1km[, grep("^X4", names(corine1km))], na.rm = TRUE)
corine10km$wetlands <- rowSums(corine10km[, grep("^X4", names(corine10km))], na.rm = TRUE)

#create water bodies column (sum of all water bodies CLC does start with 5)
corine1km$water_bodies <- rowSums(corine1km[, grep("^X5", names(corine1km))], na.rm = TRUE)
corine10km$water_bodies <- rowSums(corine10km[, grep("^X5", names(corine10km))], na.rm = TRUE)

View(corine10km)
# Summarise the corine data by smaller categories

corine1km$urban <- rowSums(corine1km[, grep("^X11", names(corine1km))], na.rm = TRUE)
corine10km$urban <- rowSums(corine10km[, grep("^X11", names(corine10km))], na.rm = TRUE)

corine1km$forest <- rowSums(corine1km[, grep("^X31", names(corine1km))], na.rm = TRUE)
corine10km$forest <- rowSums(corine10km[, grep("^X31", names(corine10km))], na.rm = TRUE)

corine1km$scrub <- rowSums(corine1km[, grep("^X32", names(corine1km))], na.rm = TRUE)
corine10km$scrub <- rowSums(corine10km[, grep("^X32", names(corine10km))], na.rm = TRUE)

# Remeber this data will overlap with indivuidualso always check before using 

############################################################################################
#'# Irish Grid Data
#import irish grid data with x and y cooridnated in DD 
irishgrid1kmwithDD <- read.csv("1.data/1.3.processedinarc/irishgrid1kmwithDD.csv")
irishgrid10kmwithDD <- read.csv("1.data/1.3.processedinarc/irishgrid10kmwithDD.csv")
keep only relevant columns 
irishgrid1kmwithDD <- irishgrid1kmwithDD[,c("gridid", "y", "x")]
irishgrid10kmwithDD <- irishgrid10kmwithDD[,c("gridid", "y", "x")]
# View to check 
View(irishgrid1kmwithDD)
View(irishgrid10kmwithDD)

############################################################################################
#'# Merge all data sets by gridid 
#merge all data in 1km cells by grid id 
merge1km <- merge(corine1km, irishgrid1kmwithDD, by = "gridid")
merge10km <- merge(corine10km, irishgrid10kmwithDD, by = "gridid")
View(merge10km)
############################################################################################
# save data as compile covariate data 
write.csv(merge1km, "1.data/1.3.processedinarc/compiled1km.csv", row.names = FALSE)
write.csv(merge10km, "1.data/1.3.processedinarc/compiled10km.csv", row.names = FALSE)

##############################################################################################
# merge PA Pm data with merged data 
View(merge1km)
# Import PA data
PA_data_NI_1km <- read.csv("1.data/1.2.processed/PA_data_NI_1km.csv")
View(PA_data_NI_1km)
PA_data_NI_1km$gridid <- PA_data_NI_1km$X1km_square
# Merge PA data with the 1km merged data, keeping all rows in PA_data_NI_1km
final_merge1km <- merge(merge1km, PA_data_NI_1km, by = "gridid", all.x = TRUE)

# View the final merged data
View(final_merge1km)  # note this final df should have 87 thousand rows 

# Save the final merged data
