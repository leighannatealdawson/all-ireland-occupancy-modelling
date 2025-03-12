# set working directory
setwd('C:/R')

# load packages
library('unmarked')
library('ggplot')

# read in detection/non-detection data
pm <- read.csv("pinemarten201520182020noname.csv")
pm <- as.matrix(pm)
g <- read.csv("greysquirrel201520182020noname.csv")
g <- as.matrix(g)
r <- read.csv("redsquirrel201520182020noname.csv")
r <- as.matrix(r)

# create list with three species detection/non-detection data
y <- list(pm, r, g)

# add species names to list
names(y) <- c('pine marten', 'red squirrel', 'grey squirrel')

# create occasion number covariate based on detection/non-detection data
occ <- pm
for (i in 1:nrow(pm)) {
  tmp <- pm[i, ]
  n.samp <- sum(!is.na(tmp))
  tmp[!is.na(tmp)] <- 1:n.samp
  occ[i, ] <- tmp
}

# Previous capture (behavioral effect) marten covariate
J <- dim(pm)[1]
K <- dim(pm)[2]
prevpm <- matrix(0, nrow = J, ncol = K)
for (i in 1:nrow(pm)) {
  tmp <- pm[i, ]
  if (sum(pm[i, ], na.rm = T) > 0) {
    first <- min(which(pm[i, ] > 0))
    if (first < K) {
      prevpm[i, (first + 1):K] <- 1
    }
  }
}

prevpm[is.na(pm)] <- NA

# Previous capture (behavioral effect) red covariate
J <- dim(r)[1]
K <- dim(r)[2]
prevr <- matrix(0, nrow = J, ncol = K)
for (i in 1:nrow(r)) {
  tmp <- r[i, ]
  if (sum(r[i, ], na.rm = T) > 0) {
    first <- min(which(r[i, ] > 0))
    if (first < K) {
      prevr[i, (first + 1):K] <- 1
    }
  }
}

prevr[is.na(r)] <- NA
prevr <- as.matrix(prevr)

# Previous capture (behavioral effect) grey covariate
J <- dim(g)[1]
K <- dim(g)[2]
prevg <- matrix(0, nrow = J, ncol = K)
for (i in 1:nrow(g)) {
  tmp <- g[i, ]
  if (sum(g[i, ], na.rm = T) > 0) {
    first <- min(which(g[i, ] > 0))
    if (first < K) {
      prevg[i, (first + 1):K] <- 1
    }
  }
}

prevg[is.na(g)] <- NA
prevg <- as.matrix(prevg)

# read in habitat covariate data
siteCovs <- read.csv("habitat info 2015 2018 2020 sites only.csv")

# format bait as dummy cov
siteCovs$bait[siteCovs$bait == "sunflower"] <- 0
siteCovs$bait[siteCovs$bait == "sunflower and peanuts"] <- 1

# create observation covariates list
obvsCov <-
  list(
    occ = occ,
    prevpm = prevpm,
    prevr = prevr,
    prevg = prevg
  )

# create unmarked dataframe
data2 = unmarkedFrameOccuMulti(y = y,
                               siteCovs = siteCovs,
                               obsCovs = obvsCov)

# create top model (m21 - second order interactions = habitat - constant - constant)
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~scale(conifer)+scale(broadleaf)',
    '~1',
    '~1',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

topmod <-  occuMulti(detFormulas, occFormulas, data2)


####### marginal estimates of occupancy #######

#broadleaf predictions
broadleaf_newdata <-
  data.frame(
    broadleaf = seq(
      min(data2@siteCovs$broadleaf),
      max(data2@siteCovs$broadleaf),
      by = 0.02
    ),
    lat = mean(data2@siteCovs$lat),
    long = mean(data2@siteCovs$long),
    conifer = mean(data2@siteCovs$conifer),
    built = mean(data2@siteCovs$built),
    year = mean(data2@siteCovs$year)
  )


# Marginal psi predictions and CIs for broadleaf
# marten
pm_occu_broadleaf_pred <- predict(
  topmod,
  type = "state",
  # psi = occupancy
  newdata = broadleaf_newdata,
  species = "pine marten",
  nsims = 1000
)
# red squirrel
rs_occu_broadleaf_pred <- predict(
  topmod,
  type = "state",
  # psi = occupancy
  newdata = broadleaf_newdata,
  species = "red squirrel",
  nsims = 1000
)
# grey squirrel
gs_occu_broadleaf_pred <- predict(
  topmod,
  type = "state",
  # psi = occupancy
  newdata = broadleaf_newdata,
  species = "grey squirrel",
  nsims = 1000
)

# Put prediction, confidence interval, and covariate values together in a data frame and name em
pm_occu_broadleaf_pred_df <-
  data.frame(
    Predicted = pm_occu_broadleaf_pred$Predicted,
    lower = pm_occu_broadleaf_pred$lower,
    upper = pm_occu_broadleaf_pred$upper,
    broadleaf_newdata
  )

rs_occu_broadleaf_pred_df <-
  data.frame(
    Predicted = rs_occu_broadleaf_pred$Predicted,
    lower = rs_occu_broadleaf_pred$lower,
    upper = rs_occu_broadleaf_pred$upper,
    broadleaf_newdata
  )

gs_occu_broadleaf_pred_df <-
  data.frame(
    Predicted = gs_occu_broadleaf_pred$Predicted,
    lower = gs_occu_broadleaf_pred$lower,
    upper = gs_occu_broadleaf_pred$upper,
    broadleaf_newdata
  )


#built predictions

built_newdata <- data.frame(
  built = seq(min(data2@siteCovs$built),
              max(data2@siteCovs$built), by = 0.02),
  lat = mean(data2@siteCovs$lat),
  long = mean(data2@siteCovs$long),
  broadleaf = mean(data2@siteCovs$broadleaf),
  conifer = mean(data2@siteCovs$conifer),
  year = mean(data2@siteCovs$year)
)


# Marginal psi predictions and CIs for built
# marten
pm_occu_built_pred <- predict(
  topmod,
  type = "state",
  # psi = occupancy
  newdata = built_newdata,
  species = "pine marten",
  nsims = 1000
)
# red squirrel
rs_occu_built_pred <- predict(
  topmod,
  type = "state",
  # psi = occupancy
  newdata = built_newdata,
  species = "red squirrel",
  nsims = 1000
)
# grey squirrel
gs_occu_built_pred <- predict(
  topmod,
  type = "state",
  # psi = occupancy
  newdata = built_newdata,
  species = "grey squirrel",
  nsims = 1000
)

# Put prediction, confidence interval, and covariate values together in a data frame
pm_occu_built_pred_df <-
  data.frame(
    Predicted = pm_occu_built_pred$Predicted,
    lower = pm_occu_built_pred$lower,
    upper = pm_occu_built_pred$upper,
    built_newdata
  )


rs_occu_built_pred_df <-
  data.frame(
    Predicted = rs_occu_built_pred$Predicted,
    lower = rs_occu_built_pred$lower,
    upper = rs_occu_built_pred$upper,
    built_newdata
  )


gs_occu_built_pred_df <-
  data.frame(
    Predicted = gs_occu_built_pred$Predicted,
    upper = gs_occu_built_pred$upper,
    lower = gs_occu_built_pred$lower,
    built_newdata
  )


# marginal psi preds for conifer
conifer_newdata <-
  data.frame(
    conifer = seq(
      min(data2@siteCovs$conifer),
      max(data2@siteCovs$conifer),
      by = 0.02
    ),
    lat = mean(data2@siteCovs$lat),
    long = mean(data2@siteCovs$long),
    broadleaf = mean(data2@siteCovs$broadleaf),
    built = mean(data2@siteCovs$built),
    year = mean(data2@siteCovs$year)
  )



# Marginal psi predictions and CIs for conifer
# marten preds
pm_occu_conifer_pred <- predict(
  topmod,
  type = "state",
  # psi = occupancy
  newdata = conifer_newdata,
  species = "pine marten",
  nsims = 1000
)

# red squirrel preds
rs_occu_conifer_pred <- predict(
  topmod,
  type = "state",
  # psi = occupancy
  newdata = conifer_newdata,
  species = "red squirrel",
  nsims = 1000
)

# grey squirrel preds
gs_occu_conifer_pred <- predict(
  topmod,
  type = "state",
  # psi = occupancy
  newdata = conifer_newdata,
  species = "grey squirrel",
  nsims = 1000
)

# Put prediction, confidence interval, and covariate values together in a data frame
# marten
pm_occu_conifer_pred_df <-
  data.frame(
    Predicted = pm_occu_conifer_pred$Predicted,
    lower = pm_occu_conifer_pred$lower,
    upper = pm_occu_conifer_pred$upper,
    conifer_newdata
  )

# red squirrel
rs_occu_conifer_pred_df <-
  data.frame(
    Predicted = rs_occu_conifer_pred$Predicted,
    lower = rs_occu_conifer_pred$lower,
    upper = rs_occu_conifer_pred$upper,
    conifer_newdata
  )

# grey squirrel
gs_occu_conifer_pred_df <-
  data.frame(
    Predicted = gs_occu_conifer_pred$Predicted,
    lower = gs_occu_conifer_pred$lower,
    upper = gs_occu_conifer_pred$upper,
    conifer_newdata
  )


# set colum for varying covariate x
x <- 4

# stuff them all into super df
all_occu_preds_df <-
  rbind(
    cbind(
      rs_occu_broadleaf_pred_df,
      Species = "red squirrel",
      Covariate = "broadleaf",
      Value = rs_occu_broadleaf_pred_df[, x]
    ),
    cbind(
      gs_occu_broadleaf_pred_df,
      Species = "grey squirrel",
      Covariate = "broadleaf",
      Value = gs_occu_broadleaf_pred_df[, x]
    ),
    cbind(
      pm_occu_broadleaf_pred_df,
      Species = "pine marten",
      Covariate = "broadleaf",
      Value = pm_occu_broadleaf_pred_df[, x]
    ),
    cbind(
      rs_occu_conifer_pred_df,
      Species = "red squirrel",
      Covariate = "conifer",
      Value = rs_occu_conifer_pred_df[, x]
    ),
    cbind(
      gs_occu_conifer_pred_df,
      Species = "grey squirrel",
      Covariate = "conifer",
      Value = gs_occu_conifer_pred_df[, x]
    ),
    cbind(
      pm_occu_conifer_pred_df,
      Species = "pine marten",
      Covariate = "conifer",
      Value = pm_occu_conifer_pred_df[, x]
    ),
    cbind(
      rs_occu_built_pred_df,
      Species = "red squirrel",
      Covariate = "urban",
      Value = rs_occu_built_pred_df[, x]
    ),
    cbind(
      gs_occu_built_pred_df,
      Species = "grey squirrel",
      Covariate = "urban",
      Value = gs_occu_built_pred_df[, x]
    ),
    cbind(
      pm_occu_built_pred_df,
      Species = "pine marten",
      Covariate = "urban",
      Value = pm_occu_built_pred_df[, x]
    )
  )

# plot it! #nice.
all_occu_pred_plot <-
  ggplot(all_occu_preds_df,
         aes(
           x = Value,
           y = Predicted,
           fill = Species,
           color = Species
         )) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2,
              color = NA) +
  geom_path(size = 1) +
  labs(x = "Covariates (standardized)", y = "Occupancy probability") +
  facet_grid(Species ~ Covariate) +
  theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

###### conditional estimates of occupancy ######

#red squirrel conditional on marten +/- in broadleaf
broadredmarten <-
  predict(
    topmod,
    'state',
    newdata = broadleaf_newdata,
    species = "red squirrel",
    cond = "pine marten",
    nsims = 1000
  )
broadrednomarten <-
  predict(
    topmod,
    'state',
    newdata = broadleaf_newdata,
    species = "red squirrel",
    cond = "-pine marten",
    nsims = 1000
  )
broadredmarten["Marten"] <- "present"
broadrednomarten["Marten"] <- "absent"

# bind them
broadredcond <- rbind(broadredmarten, broadrednomarten)

# put them together in df
broadredcond_pred_df <-
  data.frame(
    Predicted = broadredcond$Predicted,
    lower = broadredcond$lower,
    upper = broadredcond$upper,
    marten = broadredcond$Marten,
    broadleaf_newdata
  )


#red squirrel conditional on marten +/- in conifer
coniferredmarten <-
  predict(
    topmod,
    'state',
    newdata = conifer_newdata,
    species = "red squirrel",
    cond = "pine marten",
    nsims = 1000
  )
coniferrednomarten <-
  predict(
    topmod,
    'state',
    newdata = conifer_newdata,
    species = "red squirrel",
    cond = "-pine marten",
    nsims = 1000
  )
coniferredmarten["Marten"] <- "present"
coniferrednomarten["Marten"] <- "absent"

# bind them
coniferredcond <- rbind(coniferredmarten, coniferrednomarten)

# put them together in df
coniferredcond_pred_df <-
  data.frame(
    Predicted = coniferredcond$Predicted,
    lower = coniferredcond$lower,
    upper = coniferredcond$upper,
    marten = coniferredcond$Marten,
    conifer_newdata
  )


#red squirrel conditional on marten +/- in built
builtredmarten <-
  predict(topmod,
          'state',
          newdata = built_newdata,
          species = "red squirrel",
          cond = "pine marten")
builtrednomarten <-
  predict(topmod,
          'state',
          newdata = built_newdata,
          species = "red squirrel",
          cond = "-pine marten")
builtredmarten["Marten"] <- "present"
builtrednomarten["Marten"] <- "absent"

# bind them
builtredcond <- rbind(builtredmarten, builtrednomarten)

# put them together in df
builtredcond_pred_df <-
  data.frame(
    Predicted = builtredcond$Predicted,
    lower = builtredcond$lower,
    upper = builtredcond$upper,
    marten = builtredcond$Marten,
    built_newdata
  )


#grey squirrel conditional on marten +/- in broadleaf
broadgreymarten <-
  predict(
    topmod,
    'state',
    newdata = broadleaf_newdata,
    species = "grey squirrel",
    cond = "pine marten",
    nsims = 1000
  )
broadgreynomarten <-
  predict(
    topmod,
    'state',
    newdata = broadleaf_newdata,
    species = "grey squirrel",
    cond = "-pine marten",
    nsims = 1000
  )
broadgreymarten["Marten"] <- "present"
broadgreynomarten["Marten"] <- "absent"

# bind them
broadgreycond <- rbind(broadgreymarten, broadgreynomarten)

# put them together in df
broadgreycond_pred_df <-
  data.frame(
    Predicted = broadgreycond$Predicted,
    lower = broadgreycond$lower,
    upper = broadgreycond$upper,
    marten = broadgreycond$Marten,
    broadleaf_newdata
  )

#grey squirrel conditional on marten +/- in conifer
conifergreymarten <-
  predict(
    topmod,
    'state',
    newdata = conifer_newdata,
    species = "grey squirrel",
    cond = "pine marten",
    nsims = 1000
  )
conifergreynomarten <-
  predict(
    topmod,
    'state',
    newdata = conifer_newdata,
    species = "grey squirrel",
    cond = "-pine marten",
    nsims = 1000
  )
conifergreymarten["Marten"] <- "present"
conifergreynomarten["Marten"] <- "absent"

# bind them
conifergreycond <- rbind(conifergreymarten, conifergreynomarten)

# put them together in df
conifergreycond_pred_df <-
  data.frame(
    Predicted = conifergreycond$Predicted,
    lower = conifergreycond$lower,
    upper = conifergreycond$upper,
    marten = conifergreycond$Marten,
    conifer_newdata
  )

#grey squirrel conditional on marten +/- in built
builtgreymarten <-
  predict(topmod,
          'state',
          newdata = built_newdata,
          species = "grey squirrel",
          cond = "pine marten")
builtgreynomarten <-
  predict(topmod,
          'state',
          newdata = built_newdata,
          species = "grey squirrel",
          cond = "-pine marten")
builtgreymarten["Marten"] <- "present"
builtgreynomarten["Marten"] <- "absent"

# bind them
builtgreycond <- rbind(builtgreymarten, builtgreynomarten)

# put them together in df
builtgreycond_pred_df <-
  data.frame(
    Predicted = builtgreycond$Predicted,
    lower = builtgreycond$lower,
    upper = builtgreycond$upper,
    marten = builtgreycond$Marten,
    built_newdata
  )

#red squirrel conditional on grey squirrel +/- in broadleaf

broadredgrey <-
  predict(
    topmod,
    'state',
    newdata = broadleaf_newdata,
    species = "red squirrel",
    cond = "grey squirrel",
    nsims = 1000
  )
broadrednogrey <-
  predict(
    topmod,
    'state',
    newdata = broadleaf_newdata,
    species = "red squirrel",
    cond = "-grey squirrel",
    nsims = 1000
  )
broadredgrey["grey"] <- "present"
broadrednogrey["grey"] <- "absent"

# bind them
broadredgreycond <- rbind(broadredgrey, broadrednogrey)

# put them together in df
broadredgreycond_pred_df <-
  data.frame(
    Predicted = broadredgreycond$Predicted,
    lower = broadredgreycond$lower,
    upper = broadredgreycond$upper,
    grey = broadredgreycond$grey,
    broadleaf_newdata
  )

#red squirrel conditional on grey squirrel +/- in conifer
coniferredgrey <-
  predict(
    topmod,
    'state',
    newdata = conifer_newdata,
    species = "red squirrel",
    cond = "grey squirrel",
    nsims = 1000
  )
coniferrednogrey <-
  predict(
    topmod,
    'state',
    newdata = conifer_newdata,
    species = "red squirrel",
    cond = "-grey squirrel",
    nsims = 1000
  )
coniferredgrey["grey"] <- "present"
coniferrednogrey["grey"] <- "absent"

# bind them
coniferredgreycond <- rbind(coniferredgrey, coniferrednogrey)

# put them together in df
coniferredgreycond_pred_df <-
  data.frame(
    Predicted = coniferredgreycond$Predicted,
    lower = coniferredgreycond$lower,
    upper = coniferredgreycond$upper,
    grey = coniferredgreycond$grey,
    conifer_newdata
  )

#red squirrel conditional on grey squirrel +/- in built
builtredgrey <-
  predict(topmod,
          'state',
          newdata = built_newdata,
          species = "red squirrel",
          cond = "grey squirrel")
builtrednogrey <-
  predict(topmod,
          'state',
          newdata = built_newdata,
          species = "red squirrel",
          cond = "-grey squirrel")
builtredgrey["grey"] <- "present"
builtrednogrey["grey"] <- "absent"

# bind them
builtredgreycond <- rbind(builtredgrey, builtrednogrey)

# put them together in df
builtredgreycond_pred_df <-
  data.frame(
    Predicted = builtredgreycond$Predicted,
    lower = builtredgreycond$lower,
    upper = builtredgreycond$upper,
    grey = builtredgreycond$grey,
    built_newdata
  )

# set column to pull covariate values from on the bind
x <- 5

# merge into super df for red and grey psi conditional on marten +/-
all_cond_preds_df <-
  rbind(
    cbind(
      broadredcond_pred_df,
      Species = "red squirrel",
      Covariate = "broadleaf",
      Value = broadredcond_pred_df[, x]
    ),
    cbind(
      coniferredcond_pred_df,
      Species = "red squirrel",
      Covariate = "conifer",
      Value = coniferredcond_pred_df[, x]
    ),
    cbind(
      broadgreycond_pred_df,
      Species = "grey squirrel",
      Covariate = "broadleaf",
      Value = broadgreycond_pred_df[, x]
    ),
    cbind(
      conifergreycond_pred_df,
      Species = "grey squirrel",
      Covariate = "conifer",
      Value = conifergreycond_pred_df[, x]
    )
  )

# merge into super df for red psi conditional on grey squirrel +/-

grey_cond_preds_df  <-
  rbind(
    cbind(
      broadredgreycond_pred_df,
      Species = "red squirrel",
      Covariate = "broadleaf",
      grey = broadredgreycond_pred_df[, y],
      Value = broadredgreycond_pred_df[, x]
    ),
    cbind(
      coniferredgreycond_pred_df,
      Species = "red squirrel",
      Covariate = "conifer",
      grey = coniferredgreycond_pred_df[, y],
      Value = coniferredgreycond_pred_df[, x]
    )
  )


# red squirrel conditional occupancy plot
ggplot(all_cond_preds_df,
       aes(
         x = Value,
         y = Predicted,
         fill = marten,
         color = marten
       )) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2,
              color = NA) +
  geom_path(size = 1) +
  labs(x = "Covariates (standardized)", y = "Occupancy probability") +
  facet_grid(Species ~ Covariate) +
  theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

# grey conditional occupancy plot
ggplot(grey_cond_preds_df,
       aes(
         x = Value,
         y = Predicted,
         fill = grey,
         color = grey
       )) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2,
              color = NA) +
  geom_path(size = 1) + scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Covariates (standardized)", y = "Occupancy probability") +
  facet_grid(Species ~ Covariate) +
  theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
