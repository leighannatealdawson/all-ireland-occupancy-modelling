# Chosing varibles to go into the model
library(ggplot2)

rm(list = ls())
#############################################################################################
# use landscape df of  all ireland in 1km grid
landscape <- read.csv("1.data/1.3.processedinarc/1kmwidecorine.csv")
View(landscape)
nrow(landscape)
# rename columns to match siteCovs
colnames(landscape) <- gsub("^X(\\d{3})$", "CLC_\\1", colnames(landscape))

# import rivers and roads data for all Ireland 1km grid
rivers_roads <- read.csv("1.data/1.3.processedinarc/1km_all_ireland_grid_rivers_and_roads.csv")

# select only the columns with SUM_ in the name
rivers_roads <- rivers_roads[, c("gridid", grep("^SUM_", names(rivers_roads), value = TRUE))]
# rename to gridid, roads, rivers
colnames(rivers_roads) <- c("gridid", "roads", "rivers")
# merge landscape and rivers_roads by gridid
landscape_RR <- merge(landscape, rivers_roads, by = "gridid", all.x = TRUE)
# change nas to 0
landscape_RR[is.na(landscape_RR)] <- 0

landscape <- landscape_RR
# show summry of each col

summary(landscape)

############################ groupings ############################################################
# create new df for input data
landscape_datainput <- data.frame(gridid = landscape$gridid)
head(landscape_datainput)
# agriculture (pasture and arable), non agriculture open land , conifer,
# mixedwood, open non agriculture, urban, Roads, Rivers

# create new columns for each category
############################# agriculture #######################################################
landscape$agrinonpas <- rowSums(landscape[, c("CLC_211", "CLC_242", "CLC_243")], na.rm = TRUE)
landscape$pasture <- landscape$CLC_231
landscape$agri <- rowSums(landscape[, c("CLC_211", "CLC_242", "CLC_243", "CLC_231")], na.rm = TRUE)

# check correlatioon between agrinonpas and pasture
cor(landscape$agrinonpas, landscape$pasture, use = "complete.obs")

# plot this
plot(landscape$agrinonpas, landscape$pasture)

# check corrlation between CLC_211, CLC_242 and CLC_243
cor(landscape[, c("CLC_211", "CLC_222", "CLC_231", "CLC_242", "CLC_243")], use = "complete.obs")


summary(landscape[, c("CLC_211", "CLC_222", "CLC_231", "CLC_242", "CLC_243")])

# plot this
plot(landscape[, c("CLC_211", "CLC_222", "CLC_231", "CLC_242", "CLC_243")])

# add agri_all to landscape_datainput
landscape_datainput$agri <- landscape$agri

############################# urban #########################################################
# urban
landscape$urban <- rowSums(landscape[, c(
    "CLC_111", "CLC_112", "CLC_121", "CLC_123",
    "CLC_124", "CLC_131", "CLC_132", "CLC_133", "CLC_142"
)], na.rm = TRUE)
# landscape$roads <- landscape$roads

# check corrlation between urban and road length
cor(landscape$urban, landscape$roads, use = "complete.obs")
# high correlation therefor we will only use roads
# plot this with ggplot2
ggplot(landscape, aes(x = urban, y = roads)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "darkred") +
    labs(
        title = "Relationship between Urban Areas and Road Length",
        x = "Urban Area Proportion",
        y = "Road Length (m)"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
    )

# add roads to landscape_datainput
landscape_datainput$roads <- landscape$roads

landscape_datainput$urban <- landscape$urban

############################# woodlands #####################################################
landscape$broadleaf <- landscape$CLC_311
landscape$conifer <- landscape$CLC_312
landscape$mixed <- landscape$CLC_313
landscape$transition <- landscape$CLC_324

# check cor between conifer and mixedwood

cor(landscape[, c("conifer", "broadleaf", "mixed", "transition")], use = "complete.obs")

# high cor between conifer and transitional woodland therefore we will gorup them
# small cor between broadleaf and mixedwood but we will group them as much of irelands broadleaf is mixedwood
# where as most of the conifer is monoculture plantations and clear cut areas in a cicle of around 30 years.
# calculate total area for each woodland type in new table
woodland_summary <- data.frame(
    woodland_type = c("conifer", "broadleaf", "mixed", "transition"),
    total_area = c(
        sum(landscape$conifer, na.rm = TRUE),
        sum(landscape$broadleaf, na.rm = TRUE),
        sum(landscape$mixed, na.rm = TRUE),
        sum(landscape$transition, na.rm = TRUE)
    )
)
View(woodland_summary)

# check correlation between new confier and mixwood gorupings
landscape$conifer <- rowSums(landscape[, c("CLC_312", "CLC_324")], na.rm = TRUE)
landscape$mixedwood <- rowSums(landscape[, c("CLC_311", "CLC_313")], na.rm = TRUE)
cor(landscape$conifer, landscape$mixedwood, use = "complete.obs")

# add conifer and mixwood groupings to landscape_datainput
landscape_datainput$conifer <- rowSums(landscape[, c("CLC_312", "CLC_324")], na.rm = TRUE)
landscape_datainput$mixedwood <- rowSums(landscape[, c("CLC_311", "CLC_313")], na.rm = TRUE)


############################# open non agriculture ###########################################
landscape$peat <- landscape$CLC_412
landscape$moor <- landscape$CLC_322
landscape$grass <- landscape$CLC_321
landscape$opennoneagri <- rowSums(landscape[, c("CLC_321", "CLC_412", "CLC_322")], na.rm = TRUE)

cor(landscape[, c("peat", "moor", "grass")], use = "complete.obs")
# not highly correlated but these open areas not used for agriculture are likely used in the same way by martens

# add peat, moor and grass goruping called opennonagri to landscape_datainput
landscape_datainput$opennoneagri <- rowSums(landscape[, c("CLC_321", "CLC_412", "CLC_322")], na.rm = TRUE)

############################# river #########################################################
landscape_datainput$rivers <- landscape$rivers

#############################################################################################
head(landscape_datainput)
# check the groupings to make sure out variables dont corrlated
cor(landscape_datainput[, c("conifer", "mixedwood", "agri", "roads", "rivers", "opennoneagri")], use = "complete.obs")

write.csv(landscape_datainput, "1.data/1.4.final_data_gropupings/landscape_datainput_1km_grid_groupings.csv", row.names = FALSE)

#############################################################################################


#############################################################################################
# groupings for siteCovs for the buffer to match landscape_datainput
buffer <- read.csv("1.data/1.3.processedinarc/roads_and_rivers_plot_id1km_buffer.csv")


View(buffer)
# create new df of all grouping found in landscape_datainput
# conifer, mixedwood, agri_all, opennoneagri, urban, river
siteCovs_dataiunput <- data.frame(gridid = buffer$X1km_square)

# add long and lat
siteCovs_dataiunput$long <- buffer$long
siteCovs_dataiunput$lat <- buffer$lat
siteCovs_dataiunput$plot_id <- buffer$plot_id
# Create the same groupings as in landscape_datainput
# Agriculture
siteCovs_dataiunput$agri <- rowSums(buffer[, c("CLC_211", "CLC_242", "CLC_243", "CLC_231")], na.rm = TRUE)

# Woodlands
siteCovs_dataiunput$conifer <- rowSums(buffer[, c("CLC_312", "CLC_324")], na.rm = TRUE)
siteCovs_dataiunput$mixedwood <- rowSums(buffer[, c("CLC_311", "CLC_313")], na.rm = TRUE)

# Open non-agriculture
siteCovs_dataiunput$opennoneagri <- rowSums(buffer[, c("CLC_321", "CLC_412", "CLC_322")], na.rm = TRUE)

# Roads
siteCovs_dataiunput$roads <- buffer$road_length

# Rivers
siteCovs_dataiunput$rivers <- buffer$river_length

siteCovs_dataiunput$year <- buffer$year

siteCovs_dataiunput$bait <- buffer$bait

head(siteCovs_dataiunput)

# check which col na mes match in landscape_datainput
colnames(landscape_datainput)
colnames(siteCovs_dataiunput)

# save this df
write.csv(siteCovs_dataiunput, "1.data/1.4.final_data_gropupings/buffer_datainput_1km_grid_groupings.csv", row.names = FALSE)

#############################################################################################
