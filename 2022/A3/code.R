#******
#0 - Imports, Lectura del fitxer i analisi descriptiva 
#******

#Imports

library(knitr)
if (!require('knitr')) install.packages('knitr'); library('knitr')
if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('gridExtra')) install.packages('gridExtra'); library('gridExtra')
if (!require('VIM')) install.packages('VIM'); library('VIM')
if (!require('reprex')) install.packages('reprex'); library('reprex')
if (!require('fitdistrplus')) install.packages('fitdistrplus'); library('fitdistrplus')
### https://cran.r-project.org/web/packages/mappings/mappings.pdf
if(!require('mappings'))
  install.packages('remotes')
library('remotes')
remotes::install_github("benjaminrich/mappings")
library(mappings)
if (!require('car')) install.packages('car'); library('car')
if (!require('pROC')) install.packages('pROC'); library('pROC')

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


#1 
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



# model.linearRegression.F <- lm(formula=numericSatisfaction~Arrival_Delay+Distance+Departure_Delay+Service+Food_drink+Customer_Type, data=df)

model.linearRegression.F <- lm(formula=Arrival_Delay~Distance+Departure_Delay+Service+Food_drink+Customer_Type+numericSatisfaction, data=df)
summary(model.linearRegression.F)

model.linearRegression.F <- lm(formula=Arrival_Delay~Distance+Departure_Delay+Service+numericSatisfaction, data=df)
summary(model.linearRegression.F)



# exp(coefficients(model.linearRegression.F))

#2

# model.arrival <- lm(numericSatisfaction~Arrival_Delay,data=df)
# summary(model.arrival)
# model.distance <- lm(numericSatisfaction~Distance,data=df)
# model.departure <- lm(numericSatisfaction~Departure_Delay,data=df)
# summary(model.departure)
# model.service <- lm(numericSatisfaction~Service,data=df)
# model.food <- lm(numericSatisfaction~Food_drink,data=df)
# model.customerType <- lm(numericSatisfaction~Customer_Type,data=df)
# model.3 <- lm(numericSatisfaction~Arrival_Delay+Departure_Delay, data=df)
# summary(model.3)

#correlation DepartureDelay-ArrivalDelay
cor(x = df$Arrival_Delay, y = df$Departure_Delay, method = "pearson", use="pairwise.complete.obs")


#correlation numericSatisfaction-Service
cor(x = df$numericSatisfaction, y = df$Service, method = "pearson", use="pairwise.complete.obs")

#src: https://fhernanb.github.io/libro_regresion/multicoli.html
#VIF
library(car)
vif(model.linearRegression.F)
1/(1-summary(model.linearRegression.F)$r.squared)




## DIAGNOSI DEL MODEL

# model.linearRegression.F <- lm(formula=numericSatisfaction~Distance+Departure_Delay+Service+Food_drink+Customer_Type, data=df)
# summary(model.linearRegression.F)
# vif(model.linearRegression.F)


#valors residus
residus <- rstandard(model.linearRegression.F)
#valors ajustats
fitterValues <- fitted(model.linearRegression.F)

#plot fitterValues and qqnorm
par(mfrow=c(1,2))
plot(fitterValues, residus)
qqnorm(residus)

#https://towardsdatascience.com/q-q-plots-explained-5aa8495426c0



## PREDICCIO DEL MODEL

#Model totes les variables
model.linearRegression.withFoodCustomerType <- lm(formula=Arrival_Delay~Distance+Departure_Delay+Service+Food_drink+Customer_Type+numericSatisfaction, data=df)
summary(model.linearRegression.withFoodCustomerType)

#Model sense Food_drink i Customer_type
model.linearRegression.F <- lm(formula=Arrival_Delay~Distance+Departure_Delay+Service+numericSatisfaction, data=df)
summary(model.linearRegression.F)

#Model totes les variables, Max i min Food_drink. Customer_type loyal i no loyal
predictData = data.frame(Distance=2500, Departure_Delay=30, Service=3, numericSatisfaction=1, Food_drink=0, Customer_Type="Loyal Customer")
predict(model.linearRegression.withFoodCustomerType, predictData)
predictData = data.frame(Distance=2500, Departure_Delay=30, Service=3, numericSatisfaction=1, Food_drink=5, Customer_Type="Loyal Customer")
predict(model.linearRegression.withFoodCustomerType, predictData)
predictData = data.frame(Distance=2500, Departure_Delay=30, Service=3, numericSatisfaction=1, Food_drink=0, Customer_Type="disloyal Customer")
predict(model.linearRegression.withFoodCustomerType, predictData)
predictData = data.frame(Distance=2500, Departure_Delay=30, Service=3, numericSatisfaction=1, Food_drink=5, Customer_Type="disloyal Customer")
predict(model.linearRegression.withFoodCustomerType, predictData)

#Model sense Food_drink i Customer_type
predictData = data.frame(Distance=2500, Departure_Delay=30, Service=3, numericSatisfaction=1)
predict(model.linearRegression.F, predictData)

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


## ESTIMACIÓ DEL MODEL AMB EL CONJUNT D'ENTRENAMENT I INTERPRETACIÓ

#ModlgF
formula = numericSatisfaction~Gender+Customer_Type+Age+Type_Travel + Class + Distance + Seat_comfort + Food_drink + Gate + Wifi + Ent + Ease_booking + Service + Baggage_handling + Checkin_service + Cleanliness + Online_boarding + Departure_Delay + Arrival_Delay
model.logisticRegression.gF <- glm(formula=formula,family=binomial(link=logit), data=df.train)
summary(model.logisticRegression.gF)


#Col·linealitat ModlgF
#correlation DepartureDelay-ArrivalDelay
cor(x = df$Arrival_Delay, y = df$Departure_Delay, method = "pearson", use="pairwise.complete.obs")
#correlation OnlineBoarding-Wifi
cor(x = df$Wifi, y = df$Online_boarding, method = "pearson", use="pairwise.complete.obs")
#correlation BaggageHandling-Cleanliness
cor(x = df$Baggage_handling, y = df$Cleanliness, method = "pearson", use="pairwise.complete.obs")
#correlation OnlineBoarding-EaseBooking
cor(x = df$Online_boarding, y = df$Ease_booking, method = "pearson", use="pairwise.complete.obs")
#correlation SeatComfort-FoodDrink
cor(x = df$Seat_comfort, y = df$Food_drink, method = "pearson", use="pairwise.complete.obs")

#VIF
library(car)
vif(model.logisticRegression.gF)

#ModlgF
formula = numericSatisfaction~Gender+Customer_Type+Age+Type_Travel + Class + Distance + Seat_comfort + Food_drink + Gate + Wifi + Ent + Ease_booking + Service + Baggage_handling + Checkin_service + Cleanliness + Online_boarding + Arrival_Delay
model.logisticRegression.gF <- glm(formula=formula,family=binomial(link=logit), data=df.train)
summary(model.logisticRegression.gF)


model.gF.stepAIC <- stepAIC(model.logisticRegression.gF, trace = FALSE)
model.gF.stepAIC$anova


## CALCUL DE LES ODDS RATIO

## MATRIU DE CONFUSIO

## PREDICCIO
library(pROC)
#Obtenim la row numero 3 del dataset de test
df.test[3,]
sapply(predictData.glm,class)
#Obtenim la predicció
glm.prediction <- predict (model.logisticRegression.gF, data.frame(Gender="Female",  Customer_Type="disloyal Customer", Age=14, Type_Travel="Business travel", Class="Eco", Distance=2750, Seat_comfort=4, Food_drink=4, Gate=5, Wifi=1, Ent=4, Ease_booking=1, Service=5, Baggage_handling=4, Checkin_service=3, Cleanliness=4, Online_boarding=1, Arrival_Delay=0), type="response")
#predicció
glm.prediction


## BONDAT DE L'AJUST

## CORBA ROC

library(pROC)
prob=predict(model.logisticRegression.gF, df, type="response")
r=roc(df$numericSatisfaction,prob, data=df)
par(mfrow=c(1,2))
plot(r)
auc(r)


