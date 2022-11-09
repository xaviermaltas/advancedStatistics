#0 Imports

if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('VIM')) install.packages('VIM'); library('VIM')


#******
#1 - Lectura del fitxer
#******

#Load file
df.clean <- read.csv("gpa_clean.csv", sep=",")
#Summary input file/df
dim(df.clean)
str(df.clean)
head(df.clean)
summary(df.clean)
sapply(df.clean,class)