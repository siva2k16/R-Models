#Dataset - http://archive.ics.uci.edu/ml/datasets/ILPD+%28Indian+Liver+Patient+Dataset%29
liverpatientdata = read.csv("E:\\RNotes\\RData\\Exams\\ILPD\\ILPD.csv")

colnames(liverpatientdata)[1] <- "Age"
colnames(liverpatientdata)[2] <- "Gender"
colnames(liverpatientdata)[3] <- "TB"
colnames(liverpatientdata)[4] <- "DB"
colnames(liverpatientdata)[5] <- "Alkphos"
colnames(liverpatientdata)[6] <- "Sgpt"
colnames(liverpatientdata)[7] <- "Sgot"
colnames(liverpatientdata)[8] <- "TP"
colnames(liverpatientdata)[9] <- "ALB"
colnames(liverpatientdata)[10] <- "AGRatio"
colnames(liverpatientdata)[11] <- "Selector"

head(liverpatientdata)

summary(liverpatientdata)
attach(liverpatientdata)

liverpatientdata$sex[liverpatientdata$Gender=='Male'] = 0
liverpatientdata$sex[liverpatientdata$Gender=='Female'] = 1

liverpatientdata$Selector[liverpatientdata$Selector=='1'] = 0
liverpatientdata$Selector[liverpatientdata$Selector=='2'] = 1

head(liverpatientdata)
unique(liverpatientdata$Selector)

cleaneddata = liverpatientdata[complete.cases(liverpatientdata),]
cleaneddata

noRow <- nrow(cleaneddata)
noRow
trainIndex <- sample(c(TRUE, FALSE), size=noRow, replace = TRUE, prob = c(0.7, 0.3))
trainData <- cleaneddata[trainIndex, ]
testData <- cleaneddata[!trainIndex, ]
nrow(trainData)
nrow(testData)

model1 <- glm(Selector~Age+Gender+TB+DB+Alkphos+Sgpt+Sgot+TP+ALB+AGRatio+
                Age*Gender+Age*TB+Age*DB+Age*Alkphos+Age*Sgpt+Age*Sgot+Age*TP+Age*ALB+Age*AGRatio+
                TB*DB+TB*Alkphos+TB*Sgpt+TB*Sgot+TB*TP+TB*ALB+TB*AGRatio+
                DB*Alkphos+DB*Sgpt+DB*Sgot+DB*TP+DB*ALB+DB*AGRatio+
                Alkphos*Sgpt+Alkphos*Sgot+Alkphos*TP+Alkphos*ALB+Alkphos*AGRatio+
                Sgpt*Sgot+Sgpt*TP+Sgpt*ALB+Sgpt*AGRatio+
                Sgot*TP+Sgot*ALB+Sgot*AGRatio+
                TP*ALB+TP*AGRatio+
                ALB*AGRatio
              ,family=binomial,data=trainData)
summary(model1)
pchisq(484.86 - 345.41,  396 - 349,  lower.tail = FALSE)
#4.274548e-11

# Prediction for training data
predictionTrain <- predict(model1, type="response")
predictionTrain <- floor(predictionTrain + 0.5)
confusionMatrix <- table(trainData[, "Selector"], predictionTrain)
confusionMatrix
accuracyPercentage <- (confusionMatrix[1, 1] + confusionMatrix[2, 2]) / nrow(trainData)
accuracyPercentage

# Predict for the test data
predictionTest <- predict(model1, newdata=testData, type="response")
predictionTest <- floor(predictionTest + 0.5)
confusionMatrix <- table(testData[, "Selector"], predictionTest)
confusionMatrix
accuracyPercentageTest <- (confusionMatrix[1, 1] + confusionMatrix[2, 2]) / nrow(testData)
accuracyPercentageTest

