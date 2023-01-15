#Imports

library(knitr)
if (!require('knitr')) install.packages('knitr'); library('knitr')
if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')


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
