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
