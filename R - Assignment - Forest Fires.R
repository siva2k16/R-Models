#Dataset - http://archive.ics.uci.edu/ml/datasets/Forest+Fires
#Step 1 - Load Data and check summary details
setwd("E:/RNotes/RData/")
forestfires = NULL
forestfires <- read.csv(file = "forestfires.csv")
head(forestfires)
summary(forestfires)

#Step 2 - Normalize columns where min and max values have large differences
nrow(forestfires)
forestfires = subset(forestfires, forestfires$area>0)
forestfires$area <- log(forestfires$area)
forestfires$X = (forestfires$X-min(forestfires$X))/(max(forestfires$X)-min(forestfires$X))
forestfires$Y = (forestfires$Y-min(forestfires$Y))/(max(forestfires$Y)-min(forestfires$Y))
forestfires$XY = forestfires$X*forestfires$Y
forestfires$XXYY = forestfires$X*forestfires$X*forestfires$Y*forestfires$Y
forestfires$XY = (forestfires$XY-min(forestfires$XY))/(max(forestfires$XY)-min(forestfires$XY))
forestfires$DMCDC <- forestfires$DMC*forestfires$DC
forestfires$tempwind <- forestfires$temp*forestfires$wind
forestfires$FFMCDMCDC <- forestfires$FFMC*forestfires$DMC*forestfires$DC
forestfires$DMCDC <- log(forestfires$DMCDC)
forestfires$tempwind <- log(forestfires$tempwind)
forestfires$FFMCDMCDC <- log(forestfires$FFMCDMCDC)

sample(forestfires)

#Step 3 - Replace month and data text to numeric values as below
forestfires$month1[forestfires$month=='jan'] = 1
forestfires$month1[forestfires$month=='feb'] = 2
forestfires$month1[forestfires$month=='mar'] = 3
forestfires$month1[forestfires$month=='apr'] = 4
forestfires$month1[forestfires$month=='may'] = 5
forestfires$month1[forestfires$month=='jun'] = 6
forestfires$month1[forestfires$month=='jul'] = 7
forestfires$month1[forestfires$month=='aug'] = 8
forestfires$month1[forestfires$month=='sep'] = 9
forestfires$month1[forestfires$month=='oct'] = 10
forestfires$month1[forestfires$month=='nov'] = 11
forestfires$month1[forestfires$month=='dec'] = 12
forestfires$day1[forestfires$day=='sun'] = 0
forestfires$day1[forestfires$day=='mon'] = 1
forestfires$day1[forestfires$day=='tue'] = 2
forestfires$day1[forestfires$day=='wed'] = 3
forestfires$day1[forestfires$day=='thu'] = 4
forestfires$day1[forestfires$day=='fri'] = 5
forestfires$day1[forestfires$day=='sat'] = 6
forestfires$day <- NULL
forestfires$month <- NULL

#Step 4 - Run K means
library(cluster)
library(fpc)
library(NbClust)
set.seed(1)
numberofclusters <- NbClust(forestfires,min.nc=2,max.nc=15,method="kmeans")
table(numberofclusters$Best.n[1,])
grpForest <- kmeans( forestfires, centers=2,nstart=10)
grpForest$cluster
grpForest$centers
grpForest$withinss
grpForest$size
clusterresults = c(grpForest$cluster)
length(clusterresults)
nrow(forestfires)
plotcluster(forestfires,grpForest$cluster)

#Step 5 - Prepare Data for clusters
forestfirescluster1 = NULL
forestfirescluster2 = NULL

for(i in 1:nrow(forestfires))
{ 
  if(clusterresults[i]==1)
  {
    print('Cluster 1')
    deldata = forestfires[i,]
    forestfirescluster1 = rbind(forestfirescluster1,deldata)
  }
  if(clusterresults[i]==2)
  {
    print('Cluster 2')
    deldata = forestfires[i,]
    forestfirescluster2 = rbind(forestfirescluster2,deldata)
  }
}

nrow(forestfirescluster1)
nrow(forestfirescluster2)

#Step 6 - Perform Linear Regression on Each Cluster
model1 = lm(forestfirescluster1$area~forestfirescluster1$DMCDC+forestfirescluster1$tempwind +forestfirescluster1$FFMCDMCDC+forestfirescluster1$DMCDC+forestfirescluster1$XY+forestfirescluster1$XXYY+forestfirescluster1$X+forestfirescluster1$Y+forestfirescluster1$FFMC+forestfirescluster1$DMC+forestfirescluster1$DC+forestfirescluster1$ISI+forestfirescluster1$temp+forestfirescluster1$RH+forestfirescluster1$wind+forestfirescluster1$rain+factor(forestfirescluster1$month1)+factor(forestfirescluster1$day1),na.action="na.exclude")
model1
summary(model1)
plot(model1$fitted.values, model1$residual.values)
hist(model1$residuals)

#Step 7- Perform Linear Regression on Each Cluster
model2 = lm(forestfirescluster2$area~forestfirescluster2$DMCDC+forestfirescluster2$tempwind +forestfirescluster2$FFMCDMCDC+forestfirescluster2$DMCDC+forestfirescluster2$XY+forestfirescluster2$XXYY+forestfirescluster2$X+forestfirescluster2$Y+forestfirescluster2$FFMC+forestfirescluster2$DMC+forestfirescluster2$DC+forestfirescluster2$ISI+forestfirescluster2$temp+forestfirescluster2$RH+forestfirescluster2$wind+forestfirescluster2$rain+factor(forestfirescluster2$month1)+factor(forestfirescluster2$day1),na.action="na.exclude")
model2
summary(model2)
plot(model2$fitted.values, model2$residual.values)
hist(model2$residuals)
