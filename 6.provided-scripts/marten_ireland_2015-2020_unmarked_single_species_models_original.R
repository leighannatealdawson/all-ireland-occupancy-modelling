setwd('C:/Users/twininjo/Documents/R/Leighanna_Dawson_marten_ireland')

# load packages
library('unmarked')
library('MuMIn')
library('AICcmodavg')


# read in detection/non-detection data
y <- read.csv("pinemarten201520182020noname.csv")
y <- as.matrix(y)

str(y)

siteCovs <- read.csv('2015-2020_allsites_corinedata_10km2_martenandsquirrelsampling_NI.csv')
str(siteCovs)

# note a stack data fromat is being used here, so each row (each site) is a unique site and year combination
#  therefore we MUST include year in each site covariate as detection and occupancy probabilities

# create some detection covariates, for example: occasion number covariate
occ <- y
for (i in 1:nrow(y)) {
  tmp <- y[i, ]
  n.samp <- sum(!is.na(tmp))
  tmp[!is.na(tmp)] <- 1:n.samp
  occ[i, ] <- tmp
}


# format bait as dummy cov
siteCovs$bait[siteCovs$bait == "sunflower"] <- 0
siteCovs$bait[siteCovs$bait == "sunflower and peanuts"] <- 1

# create observation covariates list
obvsCov <-
  list(
    occ = occ )

# check formatting is correct - observation covariates should mirror the observation data (y)
str(y)
str(occ)

# check out distribution of covariates sampled
hist(siteCovs$Coniferous.forest)
hist(siteCovs$Broadleaved.forest)
hist(siteCovs$Continuous.urban.fabric)
hist(siteCovs$Discontinuous.urban.fabric)
hist(siteCovs$Agriculture.with.natural.vegetation)
hist(siteCovs$Transitional.woodland.shrub)
hist(siteCovs$Moors.and.heathland)
hist(scale(siteCovs$Coniferous.forest))

#create combined class for covariates, for example, here I create broadleaf and mixed forest covariate 
siteCovs$Broadleaf_and_mixed <- siteCovs$Broadleaved.forest + siteCovs$Mixed.forest
hist(siteCovs$Broadleaf_and_mixed)


# create unmarked dataframe 
umf2 = unmarkedFrameOccu(y = y, siteCovs = siteCovs, obsCovs = obvsCov)

str(umf2)
head(siteCovs)


# wait here and put some thought into the models you want to build - what covaraites do you have hypotheses for - write these down. 

# create first model as an example

mod1 <- occu(~ scale(occ) + scale(year) + bait
             ~ scale(Broadleaf_and_mixed) + scale(Coniferous.forest) + scale(Agriculture.with.natural.vegetation) +scale(Moors.and.heathland) + scale(Transitional.woodland.shrub) + scale(year), umf2)

coef(mod1)

confint(mod1, type = "state", level = 0.95)
str(siteCovs$year)

# stay here until you have a model you are happy with. You can either build and interpret a single global model, or you can conduct two-stage AIC-based model selection (see end of script).

# create a dataframe to make predictions out to
# this will explore marginal relationship between marten psi and coniferous forest
df <- data.frame(cbind(Coniferous.forest = seq(min(siteCovs$Coniferous.forest), max(siteCovs$Coniferous.forest), length.out = 200),
                       Broadleaf_and_mixed = mean(siteCovs$Broadleaf_and_mixed),
                       Discontinuous.urban.fabric = mean(siteCovs$Discontinuous.urban.fabric),
                       Agriculture.with.natural.vegetation = mean(siteCovs$Agriculture.with.natural.vegetation),
                       Transitional.woodland.shrub = mean(siteCovs$Transitional.woodland.shrub),
                       year = mean(siteCovs$year)))


# create predictions from mod1
preds <- predict(mod1, newdata = df, type = "state")

# set type to state for occupancy probability, set type to detection for detection probability

# combine with the dataframe
preds_df <- cbind(df, preds)

# plot it.
ggplot(preds_df,
         aes(
           x = Coniferous.forest,
           y = Predicted,
         )) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.15,
              linetype = "dashed",) +
  geom_path(size = 1) +
  labs(x = "Covariates (standardized)", y = "Occupancy probability") +
  theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

# what about predictions across ireland?

# read in the data
landscape <- read.csv('allireland_corinedata_10km2grid.csv')
landscape$year <- mean(siteCovs$year)
landscape$Broadleaf_and_mixed <- landscape$Broadleaved.forest + landscape$Coniferous.forest

# make predictions of marten psi across landscape
preds_allireland <- predict(mod1, landscape, type = "state")

# combine preds with landscape
preds_df <- cbind(landscape, preds_allireland)

write.csv(preds_df, "all_ireland_marten_psi_preds_example.csv")

# if there is time we can talk about .....
# two step model selection.....

# build your global model
mod1 <- occu(~ scale(occ) + scale(year) + bait
             ~ scale(Broadleaf_and_mixed) + scale(Coniferous.forest) + scale(Agriculture.with.natural.vegetation) +scale(Moors.and.heathland) + scale(Transitional.woodland.shrub) + scale(year), umf2)

#goodness of fit
occ_gof1 <- mb.gof.test(mod1, plot.hist = TRUE) 

# two step model selection
# chat = 1.77  
# p-value = 0.2 

# we will use AIC model selection when chat = 1 or near 1
# we will use QAIC model selection what chat > 1

# Part 1 of model selection - the detection submodel

# first build global detection model (every cov you interest in on p)
mod1 <- occu(~ occ + bait + year + scale(Broadleaf_and_mixed)
             ~ 1, umf2)

# then conduct model selection
modelList <- dredge(mod1, rank = "QAIC", fixed = "p(year)", chat = 1.77)

model_list_df <- as.data.frame(modelList.fullp.nullpsi)

# top detection model has bait, year, and bread.leaf and mixed

# whatever comes out as top model - use in part of model selection

# Part 2 of model selection - the occupancy submodel
mod2 <- occu(~ bait + year + scale(Broadleaf_and_mixed)
             ~ scale(Broadleaf_and_mixed) + scale(Coniferous.forest) + scale(Agriculture.with.natural.vegetation) +scale(Moors.and.heathland) + scale(Transitional.woodland.shrub) + scale(year), umf2)

# then conduct model selection
modelList <- dredge(mod2, rank = "QAIC", fixed = c("p(year)","p(bait)", "p(scale(Broadleaf_and_mixed))"), chat = 1.77)


# what ever is the top model in this model selection, is your top model and you should use it to make predictions as we did above. 

