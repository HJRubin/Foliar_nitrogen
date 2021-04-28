### March 2021, Hannah Rubin, UTK

library(ggplot2)
library(dplyr)
library(randomForest)
library(lubridate)
library(caret)
library(pROC)
library(hrbrthemes)
library(gtable)
library(grid)

# read file (output from GEE script)
setwd('~/R 2021')
ntn = read.csv("NADP__with_no3.csv")

#Add column for DVI, depending on Landsat mission
l8 = ntn[ntn$sensor == "LANDSAT_8",]
drops8 <- c("B1_median","B7_median")
l8_keep = l8[ , !(names(l8) %in% drops8)]
colnames(l8_keep)[which(names(l8_keep) == "B2_median")] <- "B1_median"
colnames(l8_keep)[which(names(l8_keep) == "B3_median")] <- "B2_median"
colnames(l8_keep)[which(names(l8_keep) == "B4_median")] <- "B3_median"
colnames(l8_keep)[which(names(l8_keep) == "B5_median")] <- "B4_median"
colnames(l8_keep)[which(names(l8_keep) == "B6_median")] <- "B5_median"

l7 = ntn[ntn$sensor == "LANDSAT_7",]
drops <- c("B6_median","B7_median")
l7_keep = l7[ , !(names(l7) %in% drops)]

l5 = ntn[ntn$sensor == "LANDSAT_5",]
l5_keep = l5[ , !(names(l5) %in% drops)]

ntn_dvi = rbind(l5_keep,l7_keep,l8_keep)
ntn_dvi = ntn_dvi[ntn_dvi$B1_median >= 0, ]
ntn_dvi = ntn_dvi[ntn_dvi$B2_median >= 0, ]
ntn_dvi = ntn_dvi[ntn_dvi$B3_median >= 0, ]
ntn_dvi = ntn_dvi[ntn_dvi$B4_median >= 0, ]
ntn_dvi = ntn_dvi[ntn_dvi$B5_median >= 0, ]
ntn_dvi$dvi = ntn_dvi$B4_median - ntn_dvi$B3_median

ntn_dvi$Year = year(as.Date(ntn_dvi$date))
ntn_dvi$Month = month(as.Date(ntn_dvi$date))
ntn_dvi$elev = as.numeric(ntn_dvi$elev)
ntn_dvi$day = yday(as.Date(ntn_dvi$date))


#More filtering - get rid of 20000 values
ntn_dvi = ntn_dvi[ntn_dvi$B1_median <= 4000, ]
ntn_dvi = ntn_dvi[ntn_dvi$B3_median <= 8000, ]
ntn_dvi = ntn_dvi[ntn_dvi$B5_median <= 8000, ]
ntn_dvi = ntn_dvi[ntn_dvi$NO3 > -9, ]
ntn_dvi = ntn_dvi[ntn_dvi$img_qual != 3, ] #remove clouds
ntn_dvi = ntn_dvi[ntn_dvi$img_qual != 5, ] #remove snow
ntn_dvi = ntn_dvi[ntn_dvi$img_qual != 6, ] #remove cirrus
ntn_dvi = ntn_dvi[!is.na(ntn_dvi$NO3), ] #remove missing

#Elevation
ggplot(ntn_dvi, aes(elev, NO3)) + 
  geom_point() +
  geom_smooth(method = 'lm')

#Anomalies
ntn_dvi$dvi_mean = ntn_dvi$dvi - 1981.234
ntn_dvi$NO3_mean = ntn_dvi$NO3 - 1.447056
ntn_dvi$ppt_mean = ntn_dvi$ppt- 26.74551
ggplot(ntn_dvi, aes(dvi_mean, NO3_mean)) + 
  geom_point() +
  geom_smooth(method = 'lm')


#Year
count_year <- count(ntn_dvi, dvi_mean, Year)
agg_year = aggregate(dvi_mean ~ Year, data = count_year, mean)
count_precip <- count(ntn_dvi, ppt_mean, Year)
agg_precip = aggregate(ppt_mean ~ Year, data = count_precip, mean)
PrecipColor <- '#6999b3'
NO3Color <- '#b3a569'
DVIColor <- '#bf3483'

p1 <- ggplot(data = agg_year, aes(Year, dvi_mean))+
  geom_line(color = DVIColor, size = 2) + 
  ylab('DVI (mg/L)')+ 
  xlab('Year') + 
  theme_bw()
p2 <- ggplot(data = agg_precip, aes(Year, ppt_mean)) + 
  geom_line(color = PrecipColor, size = 2) + 
  ylab('precipitation (mm)')+ 
  xlab('Year') + 
  theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)

#Day of Year
count_day <- count(ntn_dvi, NO3, day)
agg_day = aggregate(NO3 ~ day, data = count_day, mean)
count_precip_d <- count(ntn_dvi, ppt, day)
agg_precip_d = aggregate(ppt ~ day, data = count_precip_d, mean)

p3 <- ggplot(data = agg_day, aes(day, NO3))+
  geom_line(color = NO3Color, size = 1) + 
  ylab('NO3 (mg/L)')+ 
  xlab('Day') + 
  theme_bw()
p4 <- ggplot(data = agg_precip_d, aes(day, ppt)) + 
  geom_line(color = PrecipColor, size = 1) + 
  ylab('precipitation (mm)')+ 
  xlab('Day') + 
  theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g3 <- ggplot_gtable(ggplot_build(p3))
g4 <- ggplot_gtable(ggplot_build(p4))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g3$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g3, g4$grobs[[which(g4$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g4$layout$name == "axis-l")
ga <- g4$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g4$widths[g4$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)


#Anomalies by year
count_year <- count(ntn_dvi, NO3_mean, Year)
agg_year = aggregate(NO3_mean ~ Year, data = count_year, mean)
count_DVI <- count(ntn_dvi, dvi_mean, Year)
agg_DVI = aggregate(dvi_mean ~ Year, data = count_DVI, mean)

p1 <- ggplot(data = agg_year, aes(Year, NO3_mean))+
  geom_line(color = NO3Color, size = 1) + 
  ylab('NO3 anomaly (mg/L)')+ 
  xlab('Year') + 
  theme_bw()
p2 <- ggplot(data = agg_dvi, aes(Year, dvi_mean)) + 
  geom_line(color = DVIColor, size = 1) + 
  ylab('DVI anomaly (mg/L)')+ 
  xlab('Year') + 
  theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)


#Train a model
ind <- sample(c(TRUE, FALSE), nrow(ntn_dvi), replace=TRUE, prob=c(0.75, 0.25))
training <- ntn_dvi[ind, ] #training
test <- ntn_dvi[!ind, ]  #test
RF.obj = randomForest(NO3 ~ B1_median + B2_median + B3_median + B4_median + B5_median + ppt + elev + lat + lon,
                       data = training)
RF.obj1 = randomForest(NO3 ~ B1_median + B2_median + B3_median + B4_median + B5_median,
                      data = training)
RF.pred = predict(RF.obj, test)
postResample(test$NO3, RF.pred)
plot(varImp(RF.obj))
test$results = RF.pred
ggplot(test, aes(results, NO3))+
  geom_point()+
  xlab('Predicted Values')+
  ylab('Actual Values')+
  geom_smooth(method = 'lm')


linearmod <- lm(NO3 ~ dvi, data=training)  # build linear regression model on full data
summary(linearmod)
L_Pred <- predict(linearmod, test)
postResample(test$NO3, L_Pred)
test$results = L_Pred
ggplot(test, aes(results, NO3))+
  geom_point()+
  xlab('Predicted Values')+
  ylab('Actual Values')+
  geom_smooth(method = 'lm')

#Extrapolate to new locations
newLocs = read.csv('Hansen_5.csv')

#Add column for DVI, depending on Landsat mission
nl8 = newLocs[newLocs$sensor == "LANDSAT_8",]
ndrops8 <- c("B1_median","B7_median")
nl8_keep = nl8[ , !(names(nl8) %in% ndrops8)]
colnames(nl8_keep)[which(names(nl8_keep) == "B2_median")] <- "B1_median"
colnames(nl8_keep)[which(names(nl8_keep) == "B3_median")] <- "B2_median"
colnames(nl8_keep)[which(names(nl8_keep) == "B4_median")] <- "B3_median"
colnames(nl8_keep)[which(names(nl8_keep) == "B5_median")] <- "B4_median"
colnames(nl8_keep)[which(names(nl8_keep) == "B6_median")] <- "B5_median"

nl7 = newLocs[newLocs$sensor == "LANDSAT_7",]
ndrops <- c("B6_median","B7_median")
nl7_keep = nl7[ , !(names(nl7) %in% ndrops)]

nl5 = newLocs[newLocs$sensor == "LANDSAT_5",]
nl5_keep = nl5[ , !(names(nl5) %in% ndrops)]

newLocs_dvi = rbind(nl5_keep,nl7_keep,nl8_keep)
newLocs_dvi = newLocs_dvi[newLocs_dvi$B1_median >= 0, ]
newLocs_dvi = newLocs_dvi[newLocs_dvi$B2_median >= 0, ]
newLocs_dvi = newLocs_dvi[newLocs_dvi$B3_median >= 0, ]
newLocs_dvi = newLocs_dvi[newLocs_dvi$B4_median >= 0, ]
newLocs_dvi = newLocs_dvi[newLocs_dvi$B5_median >= 0, ]
newLocs_dvi$dvi = newLocs_dvi$B4_median - newLocs_dvi$B3_median
newLocs_dvi = newLocs_dvi[newLocs_dvi$lon > -135,]

## Map
states <- map_data("state")
m <- ggplot() + geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "black", fill = "gray") + 
  coord_fixed(1.3)
m <- p +geom_count(data = newLocs_dvi, aes(lon, lat), col="darkgreen", show.legend=T) +
  labs(data = all, y="Latitude", x="Longitude", title="Random Locations with Landsat Matches")
m

## Analyze
RF.pred_new = predict(RF.obj1, newLocs_dvi)
newLocs_dvi$prediction = RF.pred_new
write.csv(newLocs_dvi, 'RF_preds_no3_bands.csv')

newLocs_dvi$date1 = as.Date(newLocs_dvi$date, "%d-%m-%y")
newLocs_dvi$year = year(newLocs_dvi$date1)
new_1980s = newLocs_dvi[newLocs_dvi$year < 1990, ] #3203
new_2010s = newLocs_dvi[newLocs_dvi$year >= 2010,] #14388

# par(c(2,2,2,2))
plot(density(ntn_dvi$B1_median))
lines(density(ntn_dvi$B2_median))
lines(density(ntn_dvi$B3_median))
lines(density(ntn_dvi$B4_median))
lines(density(ntn_dvi$B5_median))
boxplot(ntn_dvi$B1_median)

# precip = read.csv('random_precip.csv')
