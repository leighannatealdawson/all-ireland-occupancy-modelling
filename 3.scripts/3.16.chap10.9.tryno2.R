#   Applied hierarchical modeling in ecology
#   Modeling distribution, abundance and species richness using R and BUGS
#   Volume 1: Prelude and Static models
#   Marc K�ry & J. Andy Royle
#
# Chapter 10. Modeling static occurrence and species distributions using
#             site-occupancy models
# =========================================================================
# Save the final merged data

library(unmarked)
library(tidyverse)
version


# ~~~~ impact of changes in R 4.0 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The default for options("stringsAsFactors"), used in 'data.frame', changed from
# TRUE to FALSE. This affects older versions of 'unmarked' and 'AICcmodavg', so
# reset the old default as a temporary measure.
if(packageVersion("unmarked") <= '1.0.0' || packageVersion("AICcmodavg") <= '2.2.2')
  options(stringsAsFactors = TRUE)

# This will not work from 4.1.0 as 'data.frame' ignores options("stringsAsFactors").
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# adapted using my own df of pine marten data 

# reformat data for use in this package 
# merge PA Pm data with merged data 
merge1km <- read.csv("1.data/1.3.processedinarc/compiled1km.csv")
PA_data_NI_1km <- read.csv("1.data/1.2.processed/input_data_test1km.csv")

#rename columns to match 
PA_data_NI_1km$gridid <- PA_data_NI_1km$X1km_square 
#keep only cols needed to avoid confusion
PA_data_NI_1km <- PA_data_NI_1km %>% select(gridid, route, 
                 detection_2015, detection_2018, detection_2020,
                 date_2015, date_2018, date_2020, 
                 duration_2015, duration_2018, duration_2020)
View(PA_data_NI_1km)

#add new col to PA_data_NI_1km called samplelled where 1 is added to this df for later 
PA_data_NI_1km$sampled <- 1

# Merge PA data with the 1km merged data, keeping all rows in PA_data_NI_1km
data_full <- merge(merge1km, PA_data_NI_1km, by = "gridid", all.x = TRUE)
colnames(PA_data_NI_1km)
#create new df of only PA Pm data with covariates needed for analysis (ie remove rows with na in detection columns)
PA_data_NI_1km_with_cov <- data_full %>%
  filter(sampled == 1) %>%  # Keep rows where sampled == 1
View(PA_data_NI_1km_with_cov)
# Filter out rows where 'sampled' is 1
PA_data_NI_1km_with_cov <- data_full %>%
  filter(sampled == 1)  # Keep rows where sampled == 1

# View the resulting dataframe to check
View(PA_data_NI_1km_with_cov)

########################################################################################
ensure round data long and lat are rounded to same dp 
View(merge1km$)
merge1km <- round(merge1km$x, digits = 3)
merge1km <- round(merge1km$y, digits = 3)
str(merge1km)

# 10.9 Distribution modeling and mapping of Swiss red squirrels
# =============================================================
## Code modified to use my PA PM data 
# data being used 
data <- PA_data_NI_1km_with_cov

# create detection matrix with my data 
y <- as.matrix(data[, c("detection_2015", "detection_2018", "detection_2020")]) 

# get covariates urban and forest 
urban.orig <- data[,"urban"]      # Unstandardised, original values of covariates
forest.orig <- data[,"forest"]

# create time matrix 
nrow_data <-nrow(data)
time <- matrix(as.character(1:3), nrow=nrow_data, ncol = 3, byrow = TRUE)
# create date and duration matrix 
date.orig <- as.matrix(data[, c("date_2020", "date_2018", "date_2015")])
dur.orig <- as.matrix(data[, c("duration_2020", "duration_2018", "duration_2015")])


# Overview of covariates
covs <- cbind(urban.orig, forest.orig, date.orig, dur.orig)
op <- par(mfrow = c(3,3))
for(i in 1:8){
  hist(covs[,i], breaks = 50, col = "grey", main = colnames(covs)[i])
}
pairs(cbind(urban.orig, forest.orig, date.orig, dur.orig))
par(op)

# Standardize covariates and mean-impute date and duration
# Compute means and standard deviations
(means <- c(apply(cbind(urban.orig, forest.orig), 2, mean),
    date.orig = mean(c(date.orig), na.rm = TRUE), dur.orig=mean(c(dur.orig),
    na.rm = TRUE)))
(sds <- c(apply(cbind(urban.orig, forest.orig), 2, sd),
    date.orig = sd(c(date.orig), na.rm = TRUE), dur.orig=sd(c(dur.orig),
    na.rm = TRUE)))

# Scale covariates
urban <- (urban.orig - means[1]) / sds[1]
forest <- (forest.orig - means[2]) / sds[2]
date <- (date.orig - means[3]) / sds[3]
date[is.na(date)] <- 0
dur <- (dur.orig - means[4]) / sds[4]
dur[is.na(dur)] <- 0

# Load unmarked, format data and summarize
library(unmarked)
umf <- unmarkedFrameOccu(y = y, siteCovs = data.frame(urban = urban, forest = forest),
    obsCovs = list(time = time, date = date, dur = dur))
summary(umf)

# Fit a series of models for detection first and do model selection
summary(fm1 <- occu(~1 ~1, data=umf))
summary(fm2 <- occu(~date ~1, data=umf))
summary(fm3 <- occu(~date+I(date^2) ~1, data=umf))
summary(fm4 <- occu(~date+I(date^2)+I(date^3) ~1, data=umf))
summary(fm5 <- occu(~dur ~1, data=umf))
summary(fm6 <- occu(~date+dur ~1, data=umf))
summary(fm7 <- occu(~date+I(date^2)+dur ~1, data=umf))
summary(fm8 <- occu(~date+I(date^2)+I(date^3)+dur ~1, data=umf))
summary(fm9 <- occu(~dur+I(dur^2) ~1, data=umf))
summary(fm10 <- occu(~date+dur+I(dur^2) ~1, data=umf))
summary(fm11 <- occu(~date+I(date^2)+dur+I(dur^2) ~1, data=umf))
summary(fm12 <- occu(~date+I(date^2)+I(date^3)+dur+I(dur^2) ~1, data=umf))

# Put the fitted models in a "fitList" and rank them by AIC
fms <- fitList("p(.)psi(.)"                     = fm1,
           "p(date)psi(.)"                      = fm2,
           "p(date+date2)psi(.)"                = fm3,
           "p(date+date2+date3)psi(.)"          = fm4,
           "p(dur)psi(.)"                       = fm5,
           "p(date+dur)psi(.)"                  = fm6,
           "p(date+date2+dur)psi(.)"            = fm7,
           "p(date+date2+date3+dur)psi(.)"      = fm8,
           "p(dur+dur2)psi(.)"                  = fm9,
           "p(date+dur+dur2)psi(.)"             = fm10,
           "p(date+date2+dur+dur2)psi(.)"       = fm11,
           "p(date+date2+date3+dur+dur2)psi(.)" = fm12)

(ms <- modSel(fms))

# Continue with model fitting for occupancy, guided by AIC as we go
# Check effects of urban
summary(fm13 <- occu(~date+dur+I(dur^2) ~urban, data=umf))
summary(fm14 <- occu(~date+dur+I(dur^2) ~urban+I(urban^2), data=umf))
summary(fm15 <- occu(~date+dur+I(dur^2) ~urban+I(urban^2)+ I(urban^3), data=umf))
cbind(fm13@AIC, fm14@AIC, fm15@AIC) # model 15 is best

# Check effects of forest and interactions
summary(fm16 <- occu(~date+dur+I(dur^2) ~urban+I(urban^2)+forest, data=umf))
summary(fm17 <- occu(~date+dur+I(dur^2) ~urban+I(urban^2)+forest+I(forest^2),
    data=umf))
summary(fm18 <- occu(~date+dur+I(dur^2) ~urban+I(urban^2)+forest+I(forest^2)+
    urban:forest, data=umf))
summary(fm19 <- occu(~date+dur+I(dur^2) ~urban+I(urban^2)+forest+I(forest^2)+
    urban:forest+urban:I(forest^2), data=umf))
summary(fm20 <- occu(~date+dur+I(dur^2) ~urban+I(urban^2)+forest+I(forest^2)+
    urban:forest+urban:I(forest^2)+I(urban^2):forest, data=umf))
summary(fm21 <- occu(~date+dur+I(dur^2) ~urban+I(urban^2)+forest+I(forest^2)+
    urban:forest+urban:I(forest^2)+I(urban^2):forest+ I(urban^2):I(forest^2), data=umf))
cbind(fm16@AIC, fm17@AIC, fm18@AIC, fm19@AIC, fm20@AIC) # model 16 is best 

# Check for some additional effects in detection
summary(fm22 <- occu(~date+dur+I(dur^2)+urban ~urban+I(urban^2)+
    forest+I(forest^2)+urban:forest+urban:I(forest^2)+I(urban^2):forest, data=umf))
summary(fm23 <- occu(~dur+I(dur^2)+date*(urban+I(urban^2)) ~urban+I(urban^2)+
    forest+I(forest^2)+urban:forest+urban:I(forest^2)+I(urban^2):forest, data=umf))
summary(fm24 <- occu(~dur+I(dur^2)+date*(urban+I(urban^2))+forest ~urban+I(urban^2)+
    forest+I(forest^2)+urban:forest+urban:I(forest^2)+I(urban^2):forest, data=umf))
cbind(fm22@AIC, fm23@AIC, fm24@AIC)

# OPTIONAL: Store results in a dataframe for easy viewing
aic_results <- data.frame(Model = c("fm13", "fm14", "fm15", "fm16", "fm17", "fm18",
                         "fm19", "fm20", "fm22", "fm23", "fm24"),
                          AIC = c(fm13@AIC, fm14@AIC, fm15@AIC, fm16@AIC, fm17@AIC, 
                          fm18@AIC, fm19@AIC, fm20@AIC, fm22@AIC, fm23@AIC, fm24@AIC))
aic_results <- aic_results[order(aic_results$AIC), ]  # Sort by AIC (ascending)
print(aic_results)  # Best model is at the top which is fm23 
#########################################################################################
library(AICcmodavg)

# system.time(gof.boot <- mb.gof.test(fm20, nsim = 1000, parallel=FALSE)) >>>
gof.boot <- mb.gof.test(fm23, nsim = 10, parallel=FALSE)  # ~~~ for testing
gof.boot
print("Done")
R.version.string



# Create new covariates for prediction ('prediction covs')

summary(orig.urban)
data.frame(
  Variable = c("Urban", "Forest", "Date", "Duration"),
  Min = c(min(urban.orig), min(forest.orig), min(date.orig), min(dur.orig)),
  Max = c(max(urban.orig), max(forest.orig), max(date.orig), max(dur.orig))
)

# note layout is seq(min, max ,, interval)
orig.urban <- seq(0, 1, length.out =100)    # New covs for prediction
orig.forest <- seq(0, 1, length.out =100)
orig.date <- seq(1, 3, length.out =100)
orig.duration <- seq(0, 14, length.out =100)



View(orig.urban)
nrow(orig.date)
ep <- (orig.urban - means[1]) / sds[1] # Standardise them like actual covs
fp <- (orig.forest - means[2]) / sds[2]
dp <- (orig.date - means[3]) / sds[3]
durp <- (orig.duration - means[4]) / sds[4]
View(newData)

# Obtain predictions using model 23 >>>
newData <- data.frame(urban=ep, forest=0)
pred.occ.urban <- predict(fm23, type="state", newdata=newData, appendData=TRUE)
newData <- data.frame(urban=0, forest=fp)
pred.occ.forest <- predict(fm23, type="state", newdata=newData, appendData=TRUE)
newData <- data.frame(date=dp, dur=0)
pred.det.date <- predict(fm23, type="det", newdata=newData, appendData=TRUE)
newData <- data.frame(date=0, dur=durp)
pred.det.dur <- predict(fm23, type="det", newdata=newData, appendData=TRUE)



# Plot predictions against unstandardized 'prediction covs'
op <- par(mfrow = c(2,2), mar = c(5,5,2,3), cex.lab = 1.2)
plot(pred.occ.urban[[1]] ~ orig.urban, type = "l", lwd = 3, col = "blue",
    ylim = c(0,1), las = 1, ylab = "Pred. occupancy prob.",
    xlab = "urban (m)", frame = FALSE)
matlines(orig.urban, pred.occ.urban[,3:4], lty = 1, lwd = 1, col = "grey")
plot(pred.occ.forest[[1]] ~ orig.forest, type = "l", lwd = 3, col = "blue",
    ylim = c(0,1), las = 1, ylab = "Pred. occupancy prob.",
    xlab = "Forest cover (%)", frame = FALSE)
matlines(orig.forest, pred.occ.forest[,3:4], lty = 1, lwd = 1, col = "grey")
plot(pred.det.date[[1]] ~ orig.date, type = "l", lwd = 3, col = "blue",
    ylim = c(0,1), las = 1, ylab = "Pred. detection prob.",
    xlab = "Date (1 = 1 April)", frame = FALSE)
matlines(orig.date, pred.det.date[,3:4], lty = 1, lwd = 1, col = "grey")
plot(pred.det.dur[[1]] ~ orig.duration, type = "l", lwd = 3, col = "blue",
    ylim = c(0,1), las = 1, ylab = "Pred. detection prob.",
    xlab = "Survey duration (min)", frame = FALSE)
matlines(orig.duration, pred.det.dur[,3:4], lty = 1, lwd = 1, col = "grey")
par(op)

# Predict abundance and detection jointly along two separate covariate gradients
# abundance ~ (forest, urban) and detection ~ (survey duration, date)
pred.matrix1 <- pred.matrix2 <- array(NA, dim = c(100, 100)) # Define arrays
for(i in 1:100){
  for(j in 1:100){
    newData1 <- data.frame(urban=ep[i], forest=fp[j])       # For abundance
    pred <- predict(fm20, type="state", newdata=newData1)
    pred.matrix1[i, j] <- pred$Predicted
    newData2 <- data.frame(dur=durp[i], date=dp[j])        # For detection
    pred <- predict(fm20, type="det", newdata=newData2)
    pred.matrix2[i, j] <- pred$Predicted
  }
}

op <- par(mfrow = c(1,2), cex.lab = 1.2)
mapPalette <- colorRampPalette(c("grey", "yellow", "orange", "red"))
image(x=orig.urban, y=orig.forest, z=pred.matrix1, col = mapPalette(100),
    axes = FALSE, xlab = "urban [m]", ylab = "Forest cover [%]")
contour(x=orig.urban, y=orig.forest, z=pred.matrix1, add = TRUE, lwd = 1.5,
    col = "blue", labcex = 1.3)
axis(1, at = seq(min(orig.urban), max(orig.urban), by = 250))
axis(2, at = seq(0, 100, by = 10))
box()
title(main = "Expected squirrel occurrence prob.", font.main = 1)
points(data$urban, data$forest, pch="+", cex=1)

image(x=orig.duration, y=orig.date, z=pred.matrix2, col = mapPalette(100),
    axes = FALSE, xlab = "Survey duration [min]", ylab = "Date (1 = April 1)")
contour(x=orig.duration, y=orig.date, z=pred.matrix2, add = TRUE, lwd = 1.5,
    col = "blue", labcex = 1.3)
axis(1, at = seq(min(orig.duration), max(orig.duration), by = 50))
axis(2, at = seq(0, 100, by = 10))
box()
title(main = "Expected squirrel detection prob.", font.main = 1)
matpoints(as.matrix(data[, 13:15]), as.matrix(data[, 10:12]), pch="+", cex=1)
par(op)

# Load the Swiss landscape data from unmarked
data(Switzerland)   
View(Switzerland)  
View(PredCH)

turn all x coodinates into positive number to test?? 


View(merge1km)
# Load Northern Ireland data 
CH <- merge1km
# Note my data is in WGS 1984 but model input requires EPSG:2056
#use my Northern ireland data 

# Get predictions of occupancy prob for each 1km2 quadrat of Switzerland
newData <- data.frame(urban = (CH$urban - means[1])/sds[1],
    forest = (CH$forest - means[2])/sds[2])
predCH <- predict(fm23, type="state", newdata=newData)
View(PARAM)
# Prepare Swiss coordinates and produce map
library(raster)
#library(rgdal)  # ~~~~ not necessary ~~~~

# Define new data frame with coordinates and outcome to be plotted
PARAM <- data.frame(x = CH$x, y = CH$y, z = predCH$Predicted)
r1 <- rasterFromXYZ(PARAM)     # convert into raster object

# Mask quadrats with urban greater than 2250
urban <- rasterFromXYZ(cbind(CH$x, CH$y, CH$urban))
urban[urban > 1] <- NA
r1 <- mask(r1, urban)

# Plot species distribution map (Fig. 10-14 left)
op <- par(mfrow = c(1,2), mar = c(1,2,2,5))
mapPalette <- colorRampPalette(c("grey", "yellow", "orange", "red"))
plot(r1, col = mapPalette(100), axes = FALSE, box = FALSE,
    main = "Red squirrel distribution in 2007")
# ~~~~~ shape files not available ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lakes <- readOGR(".", "lakes")
# rivers <- readOGR(".", "rivers")
# border <- readOGR(".", "border")
# plot(rivers, col = "dodgerblue", add = TRUE)
# plot(border, col = "transparent", lwd = 1.5, add = TRUE)
# plot(lakes, col = "skyblue", border = "royalblue", add = TRUE)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plot SE of the species distrbution map (Fig. 10-14 right)
r2 <- rasterFromXYZ(data.frame(x = CH$x, y = CH$y, z = predCH$SE))
r2 <- mask(r2, urban)
plot(r2, col = mapPalette(100), axes = FALSE, box = FALSE, main = "Uncertainty map 2007")
# ~~~~~ shape files not available ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot(rivers, col = "dodgerblue", add = TRUE)
# plot(border, col = "transparent", lwd = 1.5, add = TRUE)
# plot(lakes, col = "skyblue", border = "royalblue", add = TRUE)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
points(data$coordx, data$coordy, pch = "+", cex = 0.8)
par(op)

# Get extent of squirrel occurrence in 2007
sum(predCH$Predicted)                      # All quadrats
sum(predCH$Predicted[CH$urban < 2250]) # Only at urbans < 2250 m


# Standardise prediction covariate identical to those in analysis
purban <- (CH$urban - means[1]) / sds[1]
pforest <- (CH$forest - means[2]) / sds[2]

# Define function that predicts occupancy under model 20
Eocc <- function(fm) {
   betavec <- coef(fm)[1:8]       # Extract coefficients in psi
   DM <- cbind(rep(1,length(purban)), purban, purban^2, pforest, pforest^2,
      purban*pforest, purban*pforest^2, purban^2*pforest) # design matrix
   pred <- plogis(DM%*%(betavec)) # Prediction = DM * param. vector
   Eocc <- sum(pred)              # Sum over all Swiss quadrats (no mask)
   Eocc
}

(estimate.of.occurrence <- Eocc(fm20))    # Same as before, without mask
# ~~~~~ changes to unmarked::parboot ~~~~~~~~~~~~~~~~~~~~~~~
# This now has a parallel' argument with default TRUE, but it does not work with Eocc
# system.time(Eocc.boot <- parboot(fm20, Eocc, nsim=1000, report=10)) # 100 sec
system.time(Eocc.boot <- parboot(fm20, Eocc, nsim=1000, report=10,
    parallel=FALSE)) # 100 sec
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(Eocc.boot)         # Plot bootstrap distribution of extent of occurrence
quantile(Eocc.boot@t.star, c(0.025, 0.975))


