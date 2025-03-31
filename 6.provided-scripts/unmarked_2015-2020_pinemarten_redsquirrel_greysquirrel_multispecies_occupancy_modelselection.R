

# load packages
library('unmarked')
library('AICcmodavg')

# read in detection/non-detection data
pm <- read.csv("6.provided-scripts/pinemarten201520182020noname.csv")
pm <- as.matrix(pm)
g <- read.csv('6.provided-scripts/greysquirrel201520182020noname.csv')
g <- as.matrix(g)
r <- read.csv("6.provided-scripts/redsquirrel201520182020noname.csv")
r <- as.matrix(r)

# create list with three species detection/non-detection data
y <- list(pm, r, g)
names(y) <- c('pine marten', 'red squirrel', 'grey squirrel')

# create occasion number covariate
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

data2 = unmarkedFrameOccuMulti(y = y,
                               siteCovs = siteCovs,
                               obsCovs = obvsCov)

#M1 Model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year) +scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~0',
    '~0',
    '~0',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m1 <- occuMulti(detFormulas, occFormulas, data2)


#M2 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) +  scale(scale(year))+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~1',
    '~0',
    '~0',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m2 <- occuMulti(detFormulas, occFormulas, data2)

#M3 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~0',
    '~1',
    '~0',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m3 <- occuMulti(detFormulas, occFormulas, data2)

#M4 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~0',
    '~0',
    '~1',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m4 <- occuMulti(detFormulas, occFormulas, data2)

#M5 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~scale(conifer)+scale(broadleaf)',
    '~0',
    '~0',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m5 <- occuMulti(detFormulas, occFormulas, data2)

#M6 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~0',
    '~scale(conifer)+scale(broadleaf)',
    '~0',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m6 <- occuMulti(detFormulas, occFormulas, data2)

#M7 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~0',
    '~0',
    '~scale(conifer)+scale(broadleaf)',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m7 <- occuMulti(detFormulas, occFormulas, data2)

#M8 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~1',
    '~1',
    '~0',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m8 <-  occuMulti(detFormulas, occFormulas, data2)

#M9 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~scale(conifer)+scale(broadleaf)',
    '~1',
    '~0',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m9 <-  occuMulti(detFormulas, occFormulas, data2)

#M10 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~1',
    '~scale(conifer)+scale(broadleaf)',
    '~0',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m10 <-  occuMulti(detFormulas, occFormulas, data2)

#M11 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~scale(conifer)+scale(broadleaf)',
    '~scale(conifer)+scale(broadleaf)',
    '~0',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m11 <-  occuMulti(detFormulas, occFormulas, data2)

#M12 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~1',
    '~0',
    '~1',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m12 <-  occuMulti(detFormulas, occFormulas, data2)

#M13 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~1',
    '~0',
    '~scale(conifer)+scale(broadleaf)',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m13 <-  occuMulti(detFormulas, occFormulas, data2)

#M14 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~scale(conifer)+scale(broadleaf)',
    '~0',
    '~1',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m14 <-  occuMulti(detFormulas, occFormulas, data2)

#M15 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~scale(conifer)+scale(broadleaf)',
    '~0',
    '~scale(conifer)+scale(broadleaf)',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m15 <-  occuMulti(detFormulas, occFormulas, data2)

#M16 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~0',
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

m16 <-  occuMulti(detFormulas, occFormulas, data2)


#M17 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~0',
    '~scale(conifer)+scale(broadleaf)',
    '~1',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m17 <-  occuMulti(detFormulas, occFormulas, data2)

#M18 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~0',
    '~1',
    '~scale(conifer)+scale(broadleaf)',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m18 <-  occuMulti(detFormulas, occFormulas, data2)

#M19 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~0',
    '~scale(conifer)+scale(broadleaf)',
    '~scale(conifer)+scale(broadleaf)',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m19 <-  occuMulti(detFormulas, occFormulas, data2)

#M20 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~1',
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

m20 <-  occuMulti(detFormulas, occFormulas, data2)

#M21 model
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

m21 <-  occuMulti(detFormulas, occFormulas, data2)

#M22 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~1',
    '~scale(conifer)+scale(broadleaf)',
    '~1',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m22 <-  occuMulti(detFormulas, occFormulas, data2)


#M23 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~1',
    '~1',
    '~scale(conifer)+scale(broadleaf)',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m23 <-  occuMulti(detFormulas, occFormulas, data2)


#M24 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~scale(conifer)+scale(broadleaf)',
    '~scale(conifer)+scale(broadleaf)',
    '~1',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m24 <-  occuMulti(detFormulas, occFormulas, data2)

#M25 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~scale(conifer)+scale(broadleaf)',
    '~1',
    '~scale(conifer)+scale(broadleaf)',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m25 <-  occuMulti(detFormulas, occFormulas, data2)

#M26 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~scale(conifer)+scale(broadleaf)',
    '~scale(conifer)+scale(broadleaf)',
    '~scale(conifer)+scale(broadleaf)',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m26 <-  occuMulti(detFormulas, occFormulas, data2)


#M27 model
occFormulas <-
  c(
    '~ scale(broadleaf) + scale(conifer) +scale(built) + scale(year)+scale(lat)',
    '~ scale(built) + scale(conifer) +  scale(year)+scale(lat)',
    '~scale(built) + scale(conifer) + scale(broadleaf) + scale(year)+scale(lat)+scale(long)',
    '~1',
    '~scale(conifer)+scale(broadleaf)',
    '~scale(conifer)+scale(broadleaf)',
    '~0'
  )
detFormulas <-
  c(
    '~bait+scale(occ) +scale(year) +prevpm +scale(conifer)',
    '~scale(year) + prevr + scale(broadleaf)+scale(people_km2)',
    '~bait + prevg +  scale(year) +scale(broadleaf) +scale(built)+scale(river)'
  )

m27 <-  occuMulti(detFormulas, occFormulas, data2)


#model selection
Cand.models <-
  list(
    m1,
    m2,
    m3,
    m4,
    m5,
    m6,
    m7,
    m8,
    m9,
    m10,
    m11,
    m12,
    m13,
    m14,
    m15,
    m16,
    m17,
    m18,
    m19,
    m20,
    m21,
    m22,
    m23,
    m24,
    m25,
    m26,
    m27
  )

# AIC-based model selection on 27 candidate models
modselection <- aictab(
  Cand.models,
  modnames = NULL,
  second.ord = FALSE,
  nobs = NULL,
  sort = TRUE,
  c.hat = 1
)

# top model = m21 = habitat - constant - constant