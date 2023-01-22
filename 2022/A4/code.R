#Imports

library(knitr)
if (!require('knitr')) install.packages('knitr'); library('knitr')
if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('fitdistrplus')) install.packages('fitdistrplus'); library('fitdistrplus')


#******
# Preprocessament
#******

#Load file
df <- read.csv("Fumadores.csv", sep=";")

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
## NA values
colSums(is.na(df))

#Transform without units, comma -> dot and numeric datatype
cleanAndExtractNum <- function(row){
  num <- gsub("[a-zA-Z ]", "", row) #Elimiem caracters
  num <- as.numeric(gsub(",", ".",num)) #Comma to dot, to numeric
}
#AE cleaning
df$AE <- cleanAndExtractNum(df$AE)

#Tipo unique values and standarization
unique(df$Tipo)
df$Tipo[df$Tipo == "  FM  " ] <- "FM"
df$Tipo[df$Tipo == "FM  "] <- "FM"
df$Tipo[df$Tipo == "fm" ] <- "FM"
df$Tipo[df$Tipo == "fi" ] <- "FI"

#Numeric Tipo value
smokerTypeToInteger <- function(input) {
  input [input == "NF"] <- 0
  input [input == "FP"] <- 1
  input [input == "NI"] <- 2
  input [input == "FL"] <- 3
  input [input == "FM"] <- 4
  input [input == "FI"] <- 5
  input <- as.integer(input)
}
df$TipoNumeric <- smokerTypeToInteger(df$Tipo)


#Gender numeric value
unique(df$genero)
genderTointeger <- function(input) {
  input [input == "M"] <- 0
  input [input == "F"] <- 1
  input <- as.integer(input)
}
df$generoNumeric <- genderTointeger(df$genero)

#NA check
colSums(is.na(df))

#******
# Anàlisi descriptiva de la mostra
#******

## Capacitat pulmonar i gènere

#Box plot
ggplot(df, aes(x=AE, y=genero, fill=genero)) + geom_boxplot() + guides() + ggtitle("AE - Gender Box plot")

# Density plots
ggplot(df, aes(x=AE, fill=genero)) + geom_density(alpha=.3) + ggtitle("AE - Gender Density plot ")


## Capacitat pulmonar i edat

#Scatterplot AE-Age
ggplot(df, aes(edad, AE)) + geom_point() + ggtitle("AE - Age Scatter plot ")

#Linear Regression model plot over scatter plot AE-Age
ggplot(df, aes(edad, AE))+ geom_point() + ggtitle("Lineal Regression Model AE-Age") + xlab("Age") + ylab("AE") +  stat_smooth(method = "lm", col = "red")

#Scatterplot AE-Age color by gender
ggplot(df, aes(edad, AE)) + geom_point(aes(color=genero)) + ggtitle("AE - Age Scatter plot by Gender")

#Linear Regression model plot over scatter plot AE-Age color by gender
ggplot(df, aes(edad, AE))+ geom_point(aes(color=genero)) + ggtitle("Lineal Regression Model AE-Age") + xlab("Age") + ylab("AE") +  stat_smooth(method = "lm", col = "red")

## Tipus de fumadors i capacitat pulmonar

df %>% count(Tipo,mean(AE~Tipo), sort=TRUE)
df 

df.NF <- df %>% filter(Tipo == "NF")
df.FP <- df %>% filter(Tipo == "FP")
df.NI <- df %>% filter(Tipo == "NI")
df.FL <- df %>% filter(Tipo == "FL")
df.FM <- df %>% filter(Tipo == "FM")
df.FI <- df %>% filter(Tipo == "FI")


#src :
#https://www.statology.org/r-mean-by-group/
#https://sparkbyexamples.com/r-programming/sort-data-frame-in-r/

#DF smoker type, n elements and AE mean
library(dplyr)
df.byTipoMeanAE <- df %>% group_by(Tipo) %>% summarise(total_count=n(), mean_AE=mean(AE), .groups = 'drop') %>% arrange(desc(mean_AE)) %>% as.data.frame() 

#Barplot decreasing meanAE value
df.byTipoMeanAE[order(df.byTipoMeanAE$mean_AE,decreasing = TRUE),]
barplot(df.byTipoMeanAE$mean_AE,names.arg = df.byTipoMeanAE$Tipo, main="Barplot mean AE by smoke type",ylim = c(0, 2.5))#, horiz=T , las=1)


#Box plot AE by Tipo
ggplot(df, aes(x=AE, y=Tipo, fill=Tipo)) + geom_boxplot() + guides() + ggtitle("AE - Smoker type Box plot")


new_order <- with(df, reorder(Tipo,AE, FUN=mean, na.rm=T))
boxplot(AE~Tipo*new_order, data=df)

#******
# Interval de confiança de la capacitat pulmonar
#******

# Men and female new data frames
df.men <- df %>% filter(genero == "M")
df.female <- df %>% filter(genero == "F")
summary(df.men)
summary(df.female)

#IC function
IC <- function(x, NC) {
  n <- length(x)
  mean <- mean(x)
  sd <- sd(x)
  z <- abs(qnorm(((1-NC)/2)))
  errorst <- sd/sqrt(n)
  lim_inf <- mean - (z*errorst)
  lim_sup <- mean + (z*errorst)
  output <- data.frame(NC, n, mean, sd, z, errorst, lim_inf, lim_sup)
  return(output)
}

#Intervals de confinaça
ic95.AE.men <- IC(df.men$AE, 0.95)
ic95.AE.men
ic95.AE.female <- IC(df.female$AE, 0.95)
ic95.AE.female

#******
# Diferències en capacitat pulmonar entre dones i homes
#******

## Contrast

#Female
fit.norm.female.AE <- fitdist(df.female$AE, "norm")
denscomp(fit.norm.female.AE, main= "Histogram and theoretical densities - Female")

#Male
fit.norm.male.AE <- fitdist(df.female$AE, "norm")
denscomp(fit.norm.male.AE, main= "Histogram and theoretical densities - Male")

## Càlculs

#tTest function
tTest <- function( x1, x2, halternativa="twosided", C=95 ){
  
  mean1 <- mean(x1)
  mean2 <- mean(x2)
  n1 <- length(x1)
  n2 <- length(x2)
  sd1 <- sd(x1)
  sd2 <- sd(x2)
  alfa <- (1-C/100)
  
  #src: https://www.investopedia.com/terms/t/t-test.asp#:~:text=two%20sample%20sets.-,T%2DScore,validity%20of%20the%20null%20hypothesis.
  degreefreedom <- ( (sd1**2/n1 + sd2**2/n2)**2 ) / ( (sd1**2/n1)**2/(n1-1) + (sd2**2/n2)**2/(n2-1))
  t <- (mean1-mean2) / (sqrt( sd1**2/n1 + sd2**2/n2 ))
  
  if (halternativa=="twosided"){
    tcritical <- qt( alfa/2, degreefreedom, lower.tail=FALSE ) 
    pvalue <- pt( abs(t), degreefreedom, lower.tail=FALSE )*2 
  }
  else if (halternativa=="<"){
    tcritical <- qt( alfa, degreefreedom, lower.tail=TRUE )
    pvalue <- pt( t, degreefreedom, lower.tail=TRUE )
  }
  else{ #">"
    tcritical <- qt( alfa, degreefreedom, lower.tail=FALSE )
    pvalue <- pt( t, degreefreedom, lower.tail=FALSE )
  }
  #Resultat en un named vector
  output <- c(alfa, mean1, mean2, t, tcritical, pvalue)
  names(output) <- c("alfa", "mean1", "mean2", "t", "tcritical", "pvalue")
  return (output)
}

#tTest compute
ttest.menFemale.AE.95<-tTest(df.men$AE, df.female$AE, ">", 95); ttest.menFemale.AE.95

#t.test checking
t.test(df.men$AE, df.female$AE, alternative="greater", conf.level=0.95)
