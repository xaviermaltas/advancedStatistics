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
# df <- read.csv("datSat_Air.csv", sep="," , stringsAsFactors=TRUE)

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

plot.scatter.lm.departuredelayArrivaldelay <- ggplot(df, aes(x = Departure_Delay, y = Arrival_Delay)) + 
  ggtitle("Model de Regressió Lineal Departure delay - Arrival delay") + xlab("Departure Delay") + ylab("Arrival Delay") +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
plot.scatter.lm.departuredelayArrivaldelay

#-------

## MODELS DE REGRESSIO LINEAL - VARIABLES QUANTITATIVES I QUALITATIVES

#model
model.a <- lm(Arrival_Delay~Service+Food_drink, data=df)
summary(model.a)


satisfactionTointeger <- function(input) {
  input [input == "neutral or dissatisfied"] <- 0
  input [input == "satisfied"] <- 1
  input <- as.integer(input)
}

castToFactor <- function(input){
  input <- as.factor(input)
}

#value remap
df$numericSatisfaction <- satisfactionTointeger(df$satisfaction)
df$satisfaction <- castToFactor(df$satisfaction)
df$Gender <- castToFactor(df$Gender)
df$Customer_Type <- castToFactor(df$Customer_Type)
df$Class <- castToFactor(df$Class)
df$Type_Travel <- castToFactor(df$Type_Travel)

#model
model.b <- lm(numericSatisfaction~Arrival_Delay, data=df)
summary(model.b)

#plot
ggplot(df, aes(x = numericSatisfaction, y = Arrival_Delay)) + 
  ggtitle("Model de Regressió Lineal Numeric satisfaction - Arrival delay") + xlab("Numeric atisfaction") + ylab("Arrival Delay") +
  geom_point() +
  stat_smooth(method = "lm", col = "red")



#model
model.c <- lm(Arrival_Delay~Customer_Type,data=df)
summary(model.c)
#OR
exp(coefficients(model.c))



model.linearRegression.F <- lm(formula=numericSatisfaction~Arrival_Delay+Distance+Departure_Delay+Service+Food_drink+Customer_Type, data=df)
summary(model.linearRegression.F)
exp(coefficients(model.linearRegression.F))


model.linearRegression.F <- lm(formula=numericSatisfaction~Service+Food_drink+Customer_Type, data=df)
summary(model.linearRegression.F)





#******
# 2 - Regressió logística
#******

## GENERACIÓ DELS CONJUNTS D'ENTRENAMENT I DE TEST

#Creating a seed
set.seed(101)
#Getting dataset dimension
df.nrows = dim(df)[1]
trainRows = df.nrows * 0.8
#Creating training indexes
train_indexes <- sample(df.nrows, trainRows)
#Create training sample 
df.train <- df[train_indexes, ]
#Creating test sample
df.test  <- df[-train_indexes, ]
