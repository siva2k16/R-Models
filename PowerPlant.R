	#DataSet - Powerplant.csv
	#DataSet - http://archive.ics.uci.edu/ml/datasets/Combined+Cycle+Power+Plant
	#Linear Regression
	Powerplantdata = read.csv("Powerplant.csv")

	head(Powerplantdata)
	attach(Powerplantdata)
	summary(Powerplantdata)

	colnames(Powerplantdata)[1] <- "AmbientTemperature"
	colnames(Powerplantdata)[2] <- "ExhaustVacuum"
	colnames(Powerplantdata)[3] <- "AmbientPressure"
	colnames(Powerplantdata)[4] <- "RelativeHumidity"
	colnames(Powerplantdata)[5] <- "Nethourlyelectricalenergyoutput"

	head(Powerplantdata)
	
	Powerplantdata$PressureHumidity = Powerplantdata$AmbientPressure*Powerplantdata$RelativeHumidity
	Powerplantdata$TemperaturePressure = Powerplantdata$AmbientPressure*Powerplantdata$AmbientTemperature
	Powerplantdata$TemperaturePressureHumidity = Powerplantdata$AmbientPressure*Powerplantdata$AmbientTemperature*Powerplantdata$RelativeHumidity

	attach(Powerplantdata)
	
	#using leaps and identify variables for linear regression
	library(leaps)
	leaps=regsubsets(Nethourlyelectricalenergyoutput~RelativeHumidity+AmbientPressure+ExhaustVacuum+AmbientTemperature+PressureHumidity+TemperaturePressure+TemperaturePressureHumidity,data=Powerplantdata, nbest=1)
	a = summary(leaps)
	a$which
	
	#Pick five variables suggested
	#Linear Model with five variables
	model1 = lm(Nethourlyelectricalenergyoutput~RelativeHumidity+AmbientPressure+ExhaustVacuum+AmbientTemperature+TemperaturePressureHumidity)
	model1
	summary(model1)
	#Adj R square value high - 0.9323 

	#Validation methods
	#Validating the model
	plot(model1$fitted.values, model1$residual.values)
	hist(model1$residuals)
	