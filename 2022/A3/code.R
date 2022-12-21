#******
#0 - Imports, Lectura del fitxer i analisi descriptiva 
#******

#Imports

library(knitr)
if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('VIM')) install.packages('VIM'); library('VIM')
if (!require('fitdistrplus')) install.packages('fitdistrplus'); library('fitdistrplus')
require(gridExtra)


#Load file
df <- read.csv("datSat_Air.csv", sep=",")

##Anàlisi descriptiva
##DF descriptive analysis
#Dimensions
dim(df)
#df structure
str(df)
#Head data sample
head(df)
#Summary
summary(df)
#Class per each var
sapply(df,class)


#******
# 1- Regressió lineal
#******

##MODELS DE REGRESSIO LINEAL - VARIABLES QUANTITATIVES

#1 Arrival delay - Distance
#scatter plot
plot.scatter.distanceArrivaldelay <- plot(Distance~Arrival_Delay, data = df, main="Diagrama de disperció Distance - Arrival delay", xlab="Arrival Delay", ylab="Distance")

#src: https://www.cyclismo.org/tutorial/R/linearLeastSquares.html
#Arrival_Delay - Distance Model
model.linearRegression.arrivaldelayDistance<- lm(Arrival_Delay~Distance, data=df)
summary(model.linearRegression.arrivaldelayDistance)


#Linear Regression model plot over scatter plot Distance-Arrival delay
plot.scatter.lm.distanceArrivaldelay <- ggplot(df, aes(x = Distance, y = Arrival_Delay)) + 
  ggtitle("Model de Regressió Lineal Distance - Arrival delay") + xlab("Distance") + ylab("Arrival Delay") +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
plot.scatter.lm.distanceArrivaldelay

#2 Arrival delay - Distance - Departure delay

#Arrival_Delay - Distance - Departure_Delay Model
model.linearRegression.arrivaldelayDistanceDeparturedelay<- lm(Arrival_Delay~Distance+Departure_Delay, data=df)
summary(model.linearRegression.arrivaldelayDistanceDeparturedelay)


## MODELS DE REGRESSIO LINEAL - VARIABLES QUANTITATIVES I QUALITATIVES